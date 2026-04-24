#' Refresh the unified securities arrow lake
#'
#' Pulls a fresh snapshot via \code{\link{get_unified_securities}} and
#' writes a single zstd-9 parquet file in overwrite mode to the
#' \code{securities/market_unified/snapshot=current/} hive partition.
#' Intended for 2x/day weekday cron or ad-hoc invocation.
#'
#' Includes a row-count floor guard: if the new snapshot is <80\% of the
#' prior row count, the write is refused and an error is raised. Override
#' with \code{enforce_floor = FALSE}.
#'
#' @param asset_classes passed to \code{get_unified_securities}.
#' @param max_rows_per_class passed to \code{get_unified_securities}.
#' @param lake_root base directory (default \code{~/Desktop/data/securities}).
#' @param enforce_floor if \code{TRUE} (default), abort when new count < 80\% prior.
#' @param return_message pass-through message flag.
#'
#' @return invisibly, a list with fields \code{n_rows}, \code{path},
#'   \code{bytes}, \code{datetime_written}, \code{per_class} (tibble).
#' @export
#' @import arrow dplyr tibble glue
refresh_market_unified <- function(asset_classes = c("equity","bond","futures","crypto",
                                                     "coin","forex","cfd"),
                                   max_rows_per_class = 500000L,
                                   lake_root = "~/Desktop/data/securities",
                                   enforce_floor = TRUE,
                                   return_message = TRUE) {
  lake_root <- path.expand(lake_root)
  out_dir   <- file.path(lake_root, "market_unified", "snapshot=current")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  fp        <- file.path(out_dir, "market_unified.zstd.parquet")

  t0 <- Sys.time()
  df <- get_unified_securities(asset_classes      = asset_classes,
                               max_rows_per_class = max_rows_per_class,
                               return_message     = return_message)

  if (!nrow(df)) stop("refresh_market_unified: empty result; refusing to write.")

  prior <- tryCatch(arrow::open_dataset(out_dir)$num_rows,
                    error = function(e) 0L)
  if (enforce_floor && prior > 0L && nrow(df) < 0.8 * prior) {
    stop(sprintf("row-count floor breach: new=%d < 80%% of prior=%d", nrow(df), prior))
  }

  df <- df %>% dplyr::mutate(snapshot = "current")
  arrow::write_parquet(df, fp, compression = "zstd", compression_level = 9L)

  per_class <- df %>% dplyr::count(type_asset, name = "n_rows")
  info <- list(
    n_rows           = nrow(df),
    path             = fp,
    bytes            = file.info(fp)$size,
    datetime_written = Sys.time(),
    seconds_elapsed  = as.numeric(Sys.time() - t0, units = "secs"),
    per_class        = per_class
  )
  if (return_message) {
    message(sprintf("refresh_market_unified: %d rows | %.2f MB | %.1fs -> %s",
                    info$n_rows, info$bytes/1024/1024, info$seconds_elapsed, fp))
  }
  invisible(info)
}
