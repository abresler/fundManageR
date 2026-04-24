#' Enrich the unified securities lake with CIK, FX, and primary-listing flag
#'
#' Reads \code{securities/market_unified/snapshot=current/} plus
#' \code{securities/tickers_exchange/batch=<latest>/} and writes an enriched
#' sibling parquet at \code{securities/market_unified_enriched/snapshot=current/}.
#'
#' Adds:
#' \itemize{
#'   \item \code{id_cik}: CIK from SEC company_tickers_exchange.json (NYSE/NASDAQ/OTC/CBOE only)
#'   \item \code{is_primary_listing}: TRUE when (name_exchange, id_ticker) matches SEC authoritative mapping
#'   \item \code{amount_market_cap_usd}: local market cap converted to USD via forex-class cross rates
#' }
#' Uses DuckDB for the join and FX compute — no per-ticker API calls.
#'
#' @param lake_root base lake directory.
#' @return invisibly, info list with n_rows, path, bytes, match_rate_cik.
#' @export
#' @import dplyr
enrich_market_unified <- function(lake_root = "~/Desktop/data/securities") {
  lake_root <- path.expand(lake_root)
  src_fp    <- file.path(lake_root, "market_unified", "snapshot=current",
                         "market_unified.zstd.parquet")
  if (!file.exists(src_fp)) stop("market_unified snapshot missing at ", src_fp)

  # Find latest SEC tickers_exchange batch
  te_glob <- Sys.glob(file.path(lake_root, "tickers_exchange", "batch=*",
                                "tickers_exchange.zstd.parquet"))
  if (!length(te_glob)) stop("tickers_exchange lake missing — run securities_lake build_batch.R first")
  te_fp <- sort(te_glob, decreasing = TRUE)[1]

  out_dir <- file.path(lake_root, "market_unified_enriched", "snapshot=current")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out_fp  <- file.path(out_dir, "market_unified_enriched.zstd.parquet")

  # Build enrichment via DuckDB in-memory (fast + columnar)
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  DBI::dbExecute(con, sprintf(
    "CREATE VIEW mu  AS SELECT * FROM read_parquet('%s')", src_fp))
  DBI::dbExecute(con, sprintf(
    "CREATE VIEW te  AS SELECT * FROM read_parquet('%s')", te_fp))

  # FX from our own forex class: pairs are "XXXYYY" where XXX is base, YYY is quote.
  # Build USD cross-rate map: for each 3-letter currency, find rate to USD.
  DBI::dbExecute(con, "
    CREATE TABLE fx_usd AS
    WITH raw AS (
      SELECT
        UPPER(SUBSTR(id_ticker, 1, 3)) AS base_cur,
        UPPER(SUBSTR(id_ticker, 4, 3)) AS quote_cur,
        price_close
      FROM mu
      WHERE type_asset = 'forex'
        AND LENGTH(id_ticker) = 6
        AND price_close IS NOT NULL
        AND price_close > 0
    ),
    -- USD direct quotes: XYZ/USD tells us XYZ->USD rate
    direct AS (
      SELECT base_cur AS cur, AVG(price_close) AS rate_to_usd
      FROM raw WHERE quote_cur = 'USD'
      GROUP BY 1
    ),
    -- USD as base: USD/XYZ tells us 1/price XYZ->USD rate
    inverse AS (
      SELECT quote_cur AS cur, AVG(1.0/price_close) AS rate_to_usd
      FROM raw WHERE base_cur = 'USD'
      GROUP BY 1
    )
    SELECT cur, rate_to_usd FROM direct
    UNION ALL SELECT cur, rate_to_usd FROM inverse WHERE cur NOT IN (SELECT cur FROM direct)
    UNION ALL SELECT 'USD' AS cur, 1.0 AS rate_to_usd
  ")

  DBI::dbExecute(con, sprintf("
    COPY (
      SELECT
        mu.*,
        te.id_cik,
        (te.id_cik IS NOT NULL) AS is_primary_listing,
        CASE
          WHEN mu.amount_market_cap IS NULL THEN NULL
          WHEN mu.name_currency = 'USD' THEN mu.amount_market_cap
          ELSE mu.amount_market_cap * fx.rate_to_usd
        END AS amount_market_cap_usd,
        fx.rate_to_usd AS fx_rate_to_usd
      FROM mu
      LEFT JOIN te
        ON  mu.id_ticker     = te.id_ticker
        AND mu.name_exchange = UPPER(te.name_exchange)
      LEFT JOIN fx_usd fx
        ON  mu.name_currency = fx.cur
    ) TO '%s' (FORMAT PARQUET, COMPRESSION ZSTD, COMPRESSION_LEVEL 9)",
    out_fp))

  n_rows <- DBI::dbGetQuery(con, sprintf(
    "SELECT COUNT(*) n FROM read_parquet('%s')", out_fp))$n
  cik_rate <- DBI::dbGetQuery(con, sprintf(
    "SELECT AVG(CAST(is_primary_listing AS DOUBLE)) r FROM read_parquet('%s') WHERE type_asset='equity'",
    out_fp))$r

  invisible(list(
    n_rows           = n_rows,
    path             = out_fp,
    bytes            = file.info(out_fp)$size,
    match_rate_cik   = cik_rate
  ))
}
