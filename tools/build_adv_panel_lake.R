#!/usr/bin/env Rscript
# build_adv_panel_lake.R — historic + monthly SEC ADV panel lake builder
#
# Layout:
#   ~/Desktop/data/sec_adv_panel/period_data=YYYY-MM/is_exempt={false,true}/part-0000.zstd.parquet
#   ~/Desktop/data/sec_adv_panel/_manifests/manifest.zstd.parquet  (one row per partition)
#
# Modes:
#   Rscript tools/build_adv_panel_lake.R                  # latest period only (idempotent — skips if exists)
#   Rscript tools/build_adv_panel_lake.R --month 2024-06  # single period
#   Rscript tools/build_adv_panel_lake.R --all            # full historic backfill (~383 partitions)
#   Rscript tools/build_adv_panel_lake.R --force          # rebuild even if partition exists
#
# All column names are snake_case (via fundManageR::tidy_snake_case).
# Compression: zstd level 9.

suppressPackageStartupMessages({
  library(fundManageR)
  library(dplyr)
  library(arrow)
  library(stringr)
  library(purrr)
  library(tibble)
  library(lubridate)
})

options(scipen = 999)
Sys.setenv(FUND_MANAGER_UA = Sys.getenv("FUND_MANAGER_UA",
                                        unset = "fundManageR alex@pwcommunications.com"))

LAKE_ROOT <- Sys.getenv("SEC_ADV_PANEL_LAKE",
                        unset = path.expand("~/Desktop/data/sec_adv_panel"))

args <- commandArgs(trailingOnly = TRUE)
mode_all   <- "--all"   %in% args
mode_force <- "--force" %in% args
mode_month <- {
  i <- which(args == "--month")
  if (length(i)) args[i + 1] else NULL
}

ensure_dirs <- function() {
  for (sub in c("_manifests", "_logs")) {
    d <- file.path(LAKE_ROOT, sub)
    if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }
}

partition_path <- function(period_data, is_exempt) {
  file.path(LAKE_ROOT,
            paste0("period_data=", period_data),
            paste0("is_exempt=", tolower(as.character(is_exempt))),
            "part-0000.zstd.parquet")
}

# Strip formattable classes — Arrow can't cast S4-ish formattable currency to parquet types.
strip_formattable <- function(df) {
  for (nm in names(df)) {
    cls <- class(df[[nm]])
    if (any(c("formattable", "currency", "comma", "percent") %in% cls)) {
      df[[nm]] <- as.numeric(unclass(df[[nm]]))
    }
  }
  df
}

# Force list-cols (dataADV nested, etc.) out before parquet write.
strip_listcols <- function(df) {
  list_cols <- names(df)[vapply(df, is.list, logical(1))]
  if (length(list_cols)) df <- df[, setdiff(names(df), list_cols), drop = FALSE]
  df
}

# SEC source CSVs are mixed Latin-1 + UTF-8 (foreign firm names like
# "CANDRIAM, SOCIÉTÉ EN COMMANDITE PAR ACTIONS"). DuckDB enforces UTF-8 on
# parquet reads — coerce all character cols before writing.
coerce_utf8 <- function(df) {
  chr_cols <- names(df)[vapply(df, is.character, logical(1))]
  for (nm in chr_cols) {
    x <- df[[nm]]
    bad <- !is.na(x) & !validUTF8(x)
    if (any(bad)) {
      x[bad] <- iconv(x[bad], from = "latin1", to = "UTF-8", sub = "")
    }
    # Belt-and-suspenders: any remaining invalid bytes get scrubbed.
    still_bad <- !is.na(x) & !validUTF8(x)
    if (any(still_bad)) {
      x[still_bad] <- iconv(x[still_bad], from = "", to = "UTF-8", sub = "")
    }
    df[[nm]] <- x
  }
  df
}

write_period <- function(period_data, is_exempt, force = FALSE) {
  fp <- partition_path(period_data, is_exempt)
  if (file.exists(fp) && !force) {
    cat(sprintf("[skip] %s exempt=%s — partition exists\n", period_data, is_exempt))
    return(invisible(list(period_data = period_data, is_exempt = is_exempt,
                          status = "skip", rows = NA_integer_, cols = NA_integer_,
                          path = fp, bytes = file.info(fp)$size)))
  }
  t0 <- Sys.time()
  df <- tryCatch(
    adv_managers_periods_summaries(periods = period_data,
                                   include_exempt = is_exempt,
                                   return_message = FALSE,
                                   snake_case = TRUE),
    error = function(e) { message(sprintf("ERR %s exempt=%s: %s", period_data, is_exempt, conditionMessage(e))); NULL }
  )
  if (is.null(df) || nrow(df) == 0) {
    return(invisible(list(period_data = period_data, is_exempt = is_exempt,
                          status = "empty", rows = 0L, cols = 0L, path = fp, bytes = 0)))
  }
  # When include_exempt=TRUE, function returns BOTH RIA and ERA — split here.
  if ("is_exempt" %in% names(df) && is_exempt) {
    df <- df[isTRUE(is_exempt) == TRUE & df$is_exempt == TRUE, , drop = FALSE]
  }
  if (nrow(df) == 0) {
    return(invisible(list(period_data = period_data, is_exempt = is_exempt,
                          status = "empty_after_split", rows = 0L, cols = 0L,
                          path = fp, bytes = 0)))
  }
  df <- df %>% strip_formattable() %>% strip_listcols() %>% coerce_utf8()

  dir.create(dirname(fp), recursive = TRUE, showWarnings = FALSE)
  # Stage write to avoid partial files on crash.
  staging <- paste0(fp, ".staging")
  arrow::write_parquet(df, staging, compression = "zstd", compression_level = 9L)
  file.rename(staging, fp)
  bytes <- file.info(fp)$size
  dt <- round(as.numeric(Sys.time() - t0, units = "secs"), 1)
  cat(sprintf("[ok]   %s exempt=%s rows=%s cols=%d bytes=%s elapsed=%ss\n",
              period_data, is_exempt, format(nrow(df), big.mark = ","),
              ncol(df), format(bytes, big.mark = ","), dt))
  invisible(list(period_data = period_data, is_exempt = is_exempt,
                 status = "ok", rows = nrow(df), cols = ncol(df),
                 path = fp, bytes = bytes, elapsed_s = dt))
}

# Cleaner that splits the RIA+ERA combined call (faster: single fetch, two writes).
write_period_pair <- function(period_data, force = FALSE) {
  fp_ria <- partition_path(period_data, FALSE)
  fp_era <- partition_path(period_data, TRUE)
  ria_done <- file.exists(fp_ria) && !force
  era_done <- file.exists(fp_era) && !force
  if (ria_done && era_done) {
    cat(sprintf("[skip] %s — both partitions exist\n", period_data))
    return(invisible(NULL))
  }
  t0 <- Sys.time()
  df <- tryCatch(
    adv_managers_periods_summaries(periods = period_data,
                                   include_exempt = TRUE,
                                   return_message = FALSE,
                                   snake_case = TRUE),
    error = function(e) { message(sprintf("ERR %s: %s", period_data, conditionMessage(e))); NULL }
  )
  if (is.null(df) || nrow(df) == 0) {
    cat(sprintf("[empty] %s\n", period_data))
    return(invisible(NULL))
  }
  df <- df %>% strip_formattable() %>% strip_listcols() %>% coerce_utf8()
  out <- list()
  for (flag in c(FALSE, TRUE)) {
    fp <- partition_path(period_data, flag)
    if (file.exists(fp) && !force) next
    sub <- df[df$is_exempt == flag, , drop = FALSE]
    if (nrow(sub) == 0) { cat(sprintf("[empty] %s exempt=%s\n", period_data, flag)); next }
    dir.create(dirname(fp), recursive = TRUE, showWarnings = FALSE)
    staging <- paste0(fp, ".staging")
    arrow::write_parquet(sub, staging, compression = "zstd", compression_level = 9L)
    file.rename(staging, fp)
    out[[paste0("exempt_", flag)]] <- list(rows = nrow(sub), cols = ncol(sub),
                                           bytes = file.info(fp)$size, path = fp)
  }
  dt <- round(as.numeric(Sys.time() - t0, units = "secs"), 1)
  for (k in names(out)) {
    cat(sprintf("[ok]   %s %s rows=%s cols=%d bytes=%s elapsed=%ss\n",
                period_data, k, format(out[[k]]$rows, big.mark=","),
                out[[k]]$cols, format(out[[k]]$bytes, big.mark=","), dt))
  }
  invisible(out)
}

build_manifest <- function() {
  parts <- Sys.glob(file.path(LAKE_ROOT, "period_data=*", "is_exempt=*", "*.parquet"))
  if (!length(parts)) { cat("[manifest] no partitions yet\n"); return(invisible(NULL)) }
  rows <- purrr::map_dfr(parts, function(fp) {
    info <- file.info(fp)
    m <- regmatches(fp, regexec("period_data=([0-9-]+)/is_exempt=(true|false)/", fp))[[1]]
    md <- arrow::open_dataset(fp)
    tibble::tibble(
      path        = fp,
      period_data = m[2],
      is_exempt   = m[3] == "true",
      rows        = md$num_rows,
      cols        = length(md$schema$names),
      bytes       = info$size,
      mtime       = info$mtime
    )
  })
  manifest_fp <- file.path(LAKE_ROOT, "_manifests", "manifest.zstd.parquet")
  arrow::write_parquet(rows, manifest_fp, compression = "zstd", compression_level = 9L)
  cat(sprintf("[manifest] %d partitions, %s rows total, %.1f MB on disk\n",
              nrow(rows), format(sum(rows$rows), big.mark=","),
              sum(rows$bytes) / 1024^2))
  invisible(rows)
}

main <- function() {
  ensure_dirs()
  log_fp <- file.path(LAKE_ROOT, "_logs",
                      sprintf("build-%s.log", format(Sys.time(), "%Y%m%d-%H%M%S")))
  cat(sprintf("Lake root: %s\n", LAKE_ROOT))
  cat(sprintf("Log:       %s\n", log_fp))

  # Get url manifest (camelCase = FALSE for backward-compat with old code)
  urls <- adv_period_urls(snake_case = TRUE)
  cat(sprintf("URL manifest: %d files, latest = %s\n", nrow(urls), max(urls$date_data)))

  if (mode_all) {
    periods <- sort(unique(urls$period_data))
    cat(sprintf("Mode: --all (%d periods)\n", length(periods)))
  } else if (!is.null(mode_month)) {
    periods <- mode_month
    cat(sprintf("Mode: --month %s\n", mode_month))
  } else {
    periods <- max(urls$period_data)
    cat(sprintf("Mode: latest (%s)\n", periods))
  }

  for (p in periods) {
    res <- tryCatch(write_period_pair(p, force = mode_force),
                    error = function(e) { message("FAIL ", p, ": ", conditionMessage(e)); NULL })
  }
  build_manifest()
  cat("[done]\n")
}

main()
