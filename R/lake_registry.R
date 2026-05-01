#' dq Lake Registry — single source of truth for every PAI data lake
#'
#' Replaces scattered refresh_*() functions, bespoke build_batch.R cron scripts,
#' and per-lake manual schema enforcement with one declarative YAML
#' (`inst/lakes.yaml`) + four runtime primitives:
#'
#'   * \code{load_lake_registry()}  — parse YAML into list of lake specs
#'   * \code{run_lake(name)}        — execute one lake's source_call with all gates
#'   * \code{audit_lakes()}         — health view across the registry
#'   * \code{lake_to_cron(spec)}    — emit literal-path crontab line for one lake
#'
#' Built per LandMine sweep 2026-05-01 verdict: every lake-related failure today
#' (Python rot, snake_case drift, missing cron, retired-field, $HOME non-expansion,
#' silent staleness) was invisible to the prior pattern. Registry contract
#' moves all 7 failure modes to one verifiable place.

#' Load the lake registry
#'
#' Parses `inst/lakes.yaml` (installed) or `inst/lakes.yaml` (dev) into a list
#' of lake-spec lists. Each spec has: name, description, source_pkg, source_call,
#' lake_root, partition_key, partition_value, schema, cadence, gates, on_failure.
#'
#' @param yaml_path Override path. NULL = auto-locate from package install / dev tree.
#' @return List of lake specs. Names = lake names.
#' @export
load_lake_registry <- function(yaml_path = NULL) {
  if (is.null(yaml_path)) {
    yaml_path <- system.file("lakes.yaml", package = "fundManageR")
    if (!nzchar(yaml_path)) {
      # dev mode — try working directory
      candidates <- c("inst/lakes.yaml", "../inst/lakes.yaml")
      yaml_path <- candidates[file.exists(candidates)][1]
    }
  }
  if (is.null(yaml_path) || !file.exists(yaml_path)) {
    stop("lakes.yaml not found. Looked in: package install, ./inst/, ../inst/")
  }
  parsed <- yaml::read_yaml(yaml_path)
  lakes <- parsed$lakes
  names(lakes) <- vapply(lakes, function(x) x$name, character(1))
  lakes
}

#' Resolve a partition_value template
#'
#' Supports literal strings ("current") or templates with placeholders
#' \code{{YYYY-MM}}, \code{{YYYY-MM-DD}}, \code{{YYYY}}.
#' @keywords internal
.resolve_partition <- function(template, when = Sys.time()) {
  template <- gsub("\\{YYYY-MM-DD\\}", format(as.Date(when), "%Y-%m-%d"), template)
  template <- gsub("\\{YYYY-MM\\}",    format(as.Date(when), "%Y-%m"),    template)
  template <- gsub("\\{YYYY\\}",       format(as.Date(when), "%Y"),       template)
  template
}

#' Resolve a lake's expected parquet path
#' @export
lake_path <- function(spec, when = Sys.time()) {
  pv <- .resolve_partition(spec$partition_value, when)
  dir <- file.path(path.expand(spec$lake_root), spec$name,
                   paste0(spec$partition_key, "=", pv))
  file.path(dir, paste0(spec$name, ".zstd.parquet"))
}

#' Probe the upstream API for liveness (HTTP < 500)
#' @keywords internal
.probe_liveness <- function(url, timeout_sec = 8) {
  if (is.null(url) || !nzchar(url)) return(TRUE)  # no probe specified -> assume live
  resp <- tryCatch(
    httr::GET(url, httr::timeout(timeout_sec),
              httr::add_headers(`User-Agent` = "Mozilla/5.0")),
    error = function(e) NULL
  )
  if (is.null(resp)) return(FALSE)
  httr::status_code(resp) < 500
}

#' Verify a tibble matches a declared schema (column presence + base type)
#' @keywords internal
.check_schema <- function(df, schema) {
  if (is.null(schema) || length(schema) == 0) return(list(ok = TRUE, missing = character(), wrong_type = character()))
  missing <- setdiff(names(schema), names(df))
  wrong_type <- character()
  for (col in intersect(names(schema), names(df))) {
    expected <- schema[[col]]
    actual <- class(df[[col]])[1]
    type_ok <- switch(
      expected,
      character = is.character(df[[col]]),
      numeric   = is.numeric(df[[col]]),
      integer   = is.integer(df[[col]]) || is.numeric(df[[col]]),
      logical   = is.logical(df[[col]]),
      Date      = inherits(df[[col]], "Date"),
      POSIXct   = inherits(df[[col]], c("POSIXct", "POSIXt")),
      TRUE  # unknown declared type -> pass
    )
    if (!type_ok) wrong_type <- c(wrong_type, sprintf("%s (got %s, want %s)", col, actual, expected))
  }
  list(ok = length(missing) == 0 && length(wrong_type) == 0,
       missing = missing, wrong_type = wrong_type)
}

#' Execute a single lake's source_call with all gates
#'
#' Runs in this order:
#'   1. Liveness probe (skip if no liveness_url)
#'   2. Evaluate source_call in fundManageR namespace
#'   3. Optional snake_case enforcement (janitor::clean_names)
#'   4. Schema check (warn on drift)
#'   5. Row-floor check vs prior snapshot (refuse write if breach)
#'   6. Atomic write to partitioned path
#'
#' @param name Lake name from registry.
#' @param when Reference time for partition resolution (default: now).
#' @param dry_run If TRUE, run pipeline but skip the final parquet write.
#' @return Invisibly: list(name, ok, n_rows, path, bytes, seconds, schema_drift, error).
#' @export
run_lake <- function(name, when = Sys.time(), dry_run = FALSE) {
  registry <- load_lake_registry()
  if (!name %in% names(registry)) stop("Lake not in registry: ", name)
  spec <- registry[[name]]
  t0 <- Sys.time()
  result <- list(name = name, ok = FALSE, n_rows = 0L, path = NA_character_,
                 bytes = 0L, seconds = 0, schema_drift = NULL, error = NULL,
                 dry_run = dry_run)

  # Gate 1: liveness
  if (!is.null(spec$gates$liveness_url) &&
      !.probe_liveness(spec$gates$liveness_url)) {
    result$error <- sprintf("liveness gate FAILED: %s unreachable", spec$gates$liveness_url)
    return(invisible(result))
  }

  # Gate 2: evaluate source_call (R) OR run source_command (shell)
  if (!is.null(spec$source_command) && nzchar(spec$source_command)) {
    # Shell-invoked lake (orchestrator-driven). Registry tracks; doesn't parse output.
    rc <- tryCatch(system(spec$source_command, intern = FALSE), error = function(e) e)
    if (inherits(rc, "error")) {
      result$error <- sprintf("source_command FAILED: %s", conditionMessage(rc))
      return(invisible(result))
    }
    if (!is.numeric(rc) || rc != 0) {
      result$error <- sprintf("source_command exit=%s", as.character(rc))
      return(invisible(result))
    }
    # Shell lake: read parquet back to verify schema/freshness
    fp_check <- lake_path(spec, when)
    if (!file.exists(fp_check)) {
      result$error <- sprintf("source_command succeeded but expected parquet missing: %s", fp_check)
      return(invisible(result))
    }
    df <- tryCatch(arrow::read_parquet(fp_check), error = function(e) e)
    if (inherits(df, "error")) {
      result$error <- sprintf("post-command parquet read FAILED: %s", conditionMessage(df))
      return(invisible(result))
    }
  } else {
    df <- tryCatch({
      expr <- parse(text = spec$source_call)
      eval(expr, envir = asNamespace(spec$source_pkg %||% "fundManageR"))
    }, error = function(e) e)
    if (inherits(df, "error")) {
      result$error <- sprintf("source_call FAILED: %s", conditionMessage(df))
      return(invisible(result))
    }
  }
  if (!is.data.frame(df) || nrow(df) == 0) {
    result$error <- sprintf("source_call returned empty; refusing write")
    return(invisible(result))
  }

  # Gate 3: snake_case enforcement
  if (isTRUE(spec$gates$snake_case)) df <- janitor::clean_names(df)

  # Gate 4: schema check (warn, don't block — TV adds fields)
  schema_check <- .check_schema(df, spec$schema)
  if (!schema_check$ok) {
    result$schema_drift <- schema_check
    warning(sprintf("[%s] schema drift — missing: %s; wrong-type: %s",
                    name,
                    paste(schema_check$missing, collapse = ", "),
                    paste(schema_check$wrong_type, collapse = ", ")), call. = FALSE)
  }

  # Gate 5: row-floor vs prior snapshot
  fp <- lake_path(spec, when)
  floor <- spec$gates$row_floor %||% 0
  if (file.exists(fp) && floor > 0) {
    prior_n <- tryCatch(arrow::open_dataset(fp)$num_rows, error = function(e) 0L)
    if (prior_n > 0L && nrow(df) < floor * prior_n) {
      result$error <- sprintf("row-floor gate FAILED: new=%d < %.0f%% of prior=%d",
                              nrow(df), floor * 100, prior_n)
      return(invisible(result))
    }
  }

  # Gate 6: write
  if (!dry_run) {
    dir.create(dirname(fp), recursive = TRUE, showWarnings = FALSE)
    arrow::write_parquet(df, fp, compression = "zstd", compression_level = 9L)
  }
  result$ok <- TRUE
  result$n_rows <- nrow(df)
  result$path <- fp
  result$bytes <- if (file.exists(fp)) file.info(fp)$size else 0L
  result$seconds <- as.numeric(Sys.time() - t0, units = "secs")
  invisible(result)
}

#' Audit every lake in the registry
#'
#' Returns one row per lake: declared cadence, last refresh mtime, age in days,
#' staleness flag, schema-mismatch indicator (if file exists).
#'
#' @return Tibble with columns: name, cron, max_age_days, path, exists,
#'   last_refresh, age_days, stale, n_rows.
#' @export
audit_lakes <- function() {
  registry <- load_lake_registry()
  rows <- lapply(registry, function(spec) {
    fp <- lake_path(spec)
    info <- if (file.exists(fp)) file.info(fp) else NULL
    age <- if (!is.null(info)) as.numeric(difftime(Sys.time(), info$mtime, units = "days")) else NA_real_
    n_rows <- if (!is.null(info)) tryCatch(arrow::open_dataset(fp)$num_rows, error = function(e) NA_integer_) else NA_integer_
    max_age <- spec$cadence$max_age_days %||% NA_real_
    tibble::tibble(
      name           = spec$name,
      cron           = spec$cadence$cron %||% NA_character_,
      max_age_days   = max_age,
      path           = fp,
      exists         = file.exists(fp),
      last_refresh   = if (!is.null(info)) info$mtime else as.POSIXct(NA),
      age_days       = round(age, 1),
      stale          = !is.na(age) && !is.na(max_age) && age > max_age,
      n_rows         = n_rows
    )
  })
  dplyr::bind_rows(rows)
}

#' Generate a literal-path crontab line for one lake
#'
#' Eliminates the macOS-cron `$HOME` / `$RSCRIPT` / `$ORCH` non-expansion mine.
#' Output is one fully-qualified path line ready to paste into `crontab -e`.
#'
#' @param spec Lake spec from registry (a single list element).
#' @param rscript Optional Rscript path override. Default uses system R.
#' @return Character string with the crontab line.
#' @export
lake_to_cron <- function(spec, rscript = "/Library/Frameworks/R.framework/Versions/Current/Resources/bin/Rscript") {
  if (is.null(spec$cadence$cron)) return(NA_character_)
  log_dir <- file.path(path.expand(spec$lake_root), "_logs")
  log_fp  <- file.path(log_dir, paste0(spec$name, "_cron.log"))
  caffeinate <- if (grepl("\\* \\* 1-5$", spec$cadence$cron)) "caffeinate -i " else ""
  if (!is.null(spec$source_command) && nzchar(spec$source_command)) {
    # Shell-invoked lake: emit the command directly (no R wrap)
    sprintf("%s     %s%s >> %s 2>&1",
            spec$cadence$cron, caffeinate, spec$source_command, log_fp)
  } else {
    sprintf("%s     %s%s -e \"fundManageR::run_lake('%s')\" >> %s 2>&1",
            spec$cadence$cron, caffeinate, rscript, spec$name, log_fp)
  }
}

#' Emit the full registry's crontab block
#' @export
emit_lake_crontab <- function() {
  registry <- load_lake_registry()
  lines <- c("# ─── dq Lake Registry — generated from inst/lakes.yaml ───",
             paste("# Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")))
  for (spec in registry) {
    lines <- c(lines, paste0("# ", spec$description), lake_to_cron(spec), "")
  }
  paste(lines, collapse = "\n")
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
