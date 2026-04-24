#' Pull a TradingView scanner metainfo dictionary for one region
#'
#' Returns one row per field exposed by the scanner for the given region,
#' with normalized name + type metadata. Captures the schema dictionary
#' so consumers can interpret \code{_fq}, \code{_fy}, \code{_ttm},
#' \code{_fh}, \code{_h}, \code{_current}, \code{_next_fq} suffix families.
#'
#' @param region scanner region slug (e.g. \code{"america"}, \code{"uk"},
#'   \code{"japan"}, \code{"global"}).
#' @return a \code{tibble} with columns \code{name_field}, \code{type_field},
#'   \code{slug_suffix_family}, \code{name_region}, \code{date_snapshot},
#'   \code{datetime_snapshot}.
#' @export
#' @import dplyr tibble stringr httr jsonlite
tv_metainfo <- function(region = "america") {
  url <- glue::glue("https://scanner.tradingview.com/{region}/metainfo") %>% as.character()
  resp <- httr::GET(url, httr::add_headers(`User-Agent` = "Mozilla/5.0"))
  if (httr::status_code(resp) != 200L) {
    stop(sprintf("tv_metainfo: HTTP %d for region %s", httr::status_code(resp), region))
  }
  parsed <- jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"),
                               simplifyVector = FALSE)
  fields <- parsed$fields %||% list()

  suffix_fam <- function(n) {
    dplyr::case_when(
      stringr::str_ends(n, "_ttm")     ~ "ttm",
      stringr::str_ends(n, "_fq")      ~ "fq",
      stringr::str_ends(n, "_fy")      ~ "fy",
      stringr::str_ends(n, "_fh")      ~ "fh",
      stringr::str_ends(n, "_ytd")     ~ "ytd",
      stringr::str_ends(n, "_current") ~ "current",
      stringr::str_ends(n, "_recent")  ~ "recent",
      stringr::str_ends(n, "_next_fq") ~ "next_fq",
      stringr::str_ends(n, "_next_fy") ~ "next_fy",
      stringr::str_ends(n, "_forecast")~ "forecast",
      stringr::str_ends(n, "_h")       ~ "history_array",
      TRUE                             ~ "instant"
    )
  }

  snapshot_time <- Sys.time()
  attr(snapshot_time, "tzone") <- "America/New_York"

  tibble::tibble(
    name_field         = vapply(fields, function(f) f$n %||% NA_character_, character(1)),
    type_field         = vapply(fields, function(f) f$t %||% NA_character_, character(1)),
    name_region        = region,
    date_snapshot      = as.Date(snapshot_time, tz = "America/New_York"),
    datetime_snapshot  = snapshot_time
  ) %>% dplyr::mutate(slug_suffix_family = suffix_fam(name_field))
}

#' Refresh the metainfo sibling lake
#'
#' Writes \code{securities/market_metainfo/snapshot=current/market_metainfo.zstd.parquet}
#' containing one row per (region, field) tuple across the supplied regions.
#'
#' @param regions character vector of region slugs.
#' @param lake_root base lake directory.
#' @return invisibly, info list with n_rows, path, bytes, seconds_elapsed.
#' @export
#' @import arrow dplyr purrr
refresh_market_metainfo <- function(regions = c("america","global","uk","japan","germany","canada"),
                                    lake_root = "~/Desktop/data/securities") {
  lake_root <- path.expand(lake_root)
  out_dir   <- file.path(lake_root, "market_metainfo", "snapshot=current")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  fp        <- file.path(out_dir, "market_metainfo.zstd.parquet")

  t0 <- Sys.time()
  df <- purrr::map_dfr(regions, purrr::possibly(tv_metainfo, tibble::tibble()))
  if (!nrow(df)) stop("refresh_market_metainfo: empty result.")
  df <- df %>% dplyr::mutate(snapshot = "current")
  arrow::write_parquet(df, fp, compression = "zstd", compression_level = 9L)
  invisible(list(
    n_rows          = nrow(df),
    path            = fp,
    bytes           = file.info(fp)$size,
    seconds_elapsed = as.numeric(Sys.time() - t0, units = "secs")
  ))
}
