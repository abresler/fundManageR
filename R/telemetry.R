#' Wrapper-call telemetry decorator
#'
#' Decorates any data-fetching function so each call appends one JSONL row to
#' a session telemetry log. Records: timestamp, function name, payload row count,
#' duration in milliseconds, success flag, error message (if any). Zero-overhead
#' when telemetry is disabled (default-on; opt out via `options(fundManageR.telemetry = FALSE)`).
#'
#' Schema per JSONL line:
#'   {ts, fn, n_rows, duration_ms, success, error, args_hash}
#'
#' Default log path: `~/.fundmanager-telemetry.jsonl`. Override via
#' `options(fundManageR.telemetry_path = "/custom/path.jsonl")`.
#'
#' @param fn Function to decorate. Must return a data.frame / tibble (uses
#'   `nrow()` for the row-count metric); non-tibble returns are still timed
#'   and logged with `n_rows = NA`.
#' @param fn_name Optional name override; defaults to `deparse(substitute(fn))`.
#'
#' @return Decorated function with identical signature.
#' @export
#' @examples
#' \dontrun{
#'   tbl_iapd_funds_logged <- with_telemetry(tbl_iapd_funds)
#'   tbl_iapd_funds_logged()
#'   read_telemetry()  # tibble of all calls this session
#' }
with_telemetry <- function(fn, fn_name = NULL) {
  fn_name <- fn_name %||% deparse(substitute(fn))
  force(fn_name)
  function(...) {
    if (!isTRUE(getOption("fundManageR.telemetry", TRUE))) {
      return(fn(...))
    }
    t0 <- Sys.time()
    args_hash <- substr(rlang::hash(list(...)), 1, 8)
    result <- tryCatch(fn(...), error = function(e) e)
    duration_ms <- as.numeric(Sys.time() - t0, units = "secs") * 1000
    is_err <- inherits(result, "error")
    n_rows <- if (!is_err && is.data.frame(result)) nrow(result) else NA_integer_
    .telemetry_emit(
      fn = fn_name,
      n_rows = n_rows,
      duration_ms = round(duration_ms, 1),
      success = !is_err,
      error = if (is_err) conditionMessage(result) else NA_character_,
      args_hash = args_hash
    )
    if (is_err) stop(result)
    result
  }
}

#' @keywords internal
.telemetry_emit <- function(fn, n_rows, duration_ms, success, error, args_hash) {
  path <- getOption("fundManageR.telemetry_path",
                    file.path(Sys.getenv("HOME"), ".fundmanager-telemetry.jsonl"))
  row <- list(
    ts = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC"),
    fn = fn,
    n_rows = n_rows,
    duration_ms = duration_ms,
    success = success,
    error = if (is.na(error)) NULL else error,
    args_hash = args_hash
  )
  line <- jsonlite::toJSON(row, auto_unbox = TRUE, na = "null")
  tryCatch(
    cat(line, "\n", sep = "", file = path, append = TRUE),
    error = function(e) invisible(NULL)
  )
}

#' Read the telemetry log
#'
#' Returns the JSONL log as a tibble. Honors the same path option as
#' `with_telemetry()`.
#'
#' @param last_n Optional integer to tail the most recent N rows. NULL = all.
#'
#' @return Tibble with columns: `ts`, `fn`, `n_rows`, `duration_ms`, `success`, `error`, `args_hash`.
#' @export
read_telemetry <- function(last_n = NULL) {
  path <- getOption("fundManageR.telemetry_path",
                    file.path(Sys.getenv("HOME"), ".fundmanager-telemetry.jsonl"))
  if (!file.exists(path)) return(tibble::tibble(
    ts = character(), fn = character(), n_rows = integer(),
    duration_ms = numeric(), success = logical(), error = character(),
    args_hash = character()
  ))
  lines <- readLines(path, warn = FALSE)
  if (!is.null(last_n) && length(lines) > last_n) lines <- tail(lines, last_n)
  rows <- lapply(lines, function(l) {
    p <- tryCatch(jsonlite::fromJSON(l, simplifyVector = TRUE), error = function(e) NULL)
    if (is.null(p)) return(NULL)
    # Normalize size-0 fields (from explicit JSON null) to scalar NA so
    # tibble::as_tibble_row doesn't choke. Each field is single-row scalar.
    expected <- c("ts", "fn", "n_rows", "duration_ms", "success", "error", "args_hash")
    for (k in expected) {
      v <- p[[k]]
      if (is.null(v) || length(v) == 0) {
        p[[k]] <- switch(k,
          ts = NA_character_, fn = NA_character_, args_hash = NA_character_, error = NA_character_,
          n_rows = NA_integer_,
          duration_ms = NA_real_,
          success = NA
        )
      }
    }
    p
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) return(tibble::tibble())
  dplyr::bind_rows(lapply(rows, tibble::as_tibble_row))
}

#' Summarize telemetry by function
#'
#' Aggregates the telemetry log by function name. Useful for IRR audits of
#' which wrappers are getting hammered, which are slow, and which are erroring.
#'
#' @param last_n Optional integer to tail the most recent N rows before summarizing.
#'
#' @return Tibble with one row per function: `fn`, `n_calls`, `pct_success`,
#'   `total_rows`, `median_duration_ms`, `p95_duration_ms`, `last_called`.
#' @export
summarize_telemetry <- function(last_n = NULL) {
  tel <- read_telemetry(last_n)
  if (nrow(tel) == 0) return(tel)
  tel %>%
    dplyr::group_by(.data$fn) %>%
    dplyr::summarise(
      n_calls = dplyr::n(),
      pct_success = round(100 * mean(.data$success, na.rm = TRUE), 1),
      total_rows = sum(.data$n_rows, na.rm = TRUE),
      median_duration_ms = round(stats::median(.data$duration_ms, na.rm = TRUE), 1),
      p95_duration_ms = round(stats::quantile(.data$duration_ms, 0.95, na.rm = TRUE), 1),
      last_called = max(.data$ts),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(.data$n_calls))
}

`%||%` <- function(a, b) if (is.null(a)) b else a
