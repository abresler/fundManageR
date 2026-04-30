#!/usr/bin/env Rscript
# build_adv_firm_lifecycle.R — derived per-CRD lifecycle table
#
# Output: ~/Desktop/data/sec_adv_panel/_products/firm_lifecycle.zstd.parquet
#
# One row per id_crd × is_exempt-stream with:
#   first_seen, last_seen, n_months_present, n_distinct_periods,
#   first_aum, last_aum, max_aum, min_aum_positive,
#   peak_to_last_drawdown_pct, total_growth_multiple,
#   first_state, last_state, first_country, last_country,
#   moved_state (bool), changed_country (bool),
#   exited_flag (last_seen < global_max - 1 month),
#   transition_path (RIA→ERA→exit, etc),
#   final_aum_band (mega/large/mid/small/sub_b)
#
# Run:  Rscript tools/build_adv_firm_lifecycle.R
# Idempotent — overwrites the parquet on each run.

suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(duckdb)
  library(DBI)
})

LAKE   <- path.expand("~/Desktop/data/sec_adv_panel")
PROD   <- file.path(LAKE, "_products")
OUTPUT <- file.path(PROD, "firm_lifecycle.zstd.parquet")
if (!dir.exists(PROD)) dir.create(PROD, recursive = TRUE, showWarnings = FALSE)

con <- dbConnect(duckdb::duckdb(), ":memory:")
dbExecute(con, "SET memory_limit='4GB';")

cat("[1/3] reading panel + computing per-firm lifecycle...\n")
t0 <- Sys.time()

dbExecute(con, sprintf("
  CREATE OR REPLACE TEMP VIEW panel AS
  SELECT
    id_crd,
    name_entity_manager_legal,
    is_exempt,
    period_data,
    date_data,
    amount_aum_total,
    state_office_primary,
    country_office_primary,
    count_employees_total,
    count_accounts_total,
    status_sec
  FROM read_parquet('%s/period_data=*/is_exempt=*/*.parquet',
                    hive_partitioning = true,
                    union_by_name     = true)
  WHERE id_crd IS NOT NULL
", LAKE))

dbExecute(con, "
  CREATE OR REPLACE TEMP VIEW global_period AS
  SELECT MAX(period_data) AS global_max FROM panel;
")

# Use first_value / last_value via window functions so we capture name and state
# at ENTRY and at EXIT (per stream).
sql <- "
  WITH ordered AS (
    SELECT
      id_crd,
      is_exempt,
      period_data,
      date_data,
      amount_aum_total,
      state_office_primary,
      country_office_primary,
      count_employees_total,
      count_accounts_total,
      name_entity_manager_legal,
      status_sec,
      ROW_NUMBER() OVER (PARTITION BY id_crd, is_exempt ORDER BY period_data ASC ) AS rn_first,
      ROW_NUMBER() OVER (PARTITION BY id_crd, is_exempt ORDER BY period_data DESC) AS rn_last
    FROM panel
  ),
  agg AS (
    SELECT
      id_crd,
      is_exempt,
      MIN(period_data)  AS first_seen,
      MAX(period_data)  AS last_seen,
      COUNT(*)          AS n_months_present,
      COUNT(DISTINCT period_data) AS n_distinct_periods,
      MAX(amount_aum_total) AS max_aum,
      MIN(amount_aum_total) FILTER (WHERE amount_aum_total > 0) AS min_aum_positive,
      MAX(count_employees_total) AS max_employees,
      MAX(count_accounts_total)  AS max_accounts,
      ANY_VALUE(name_entity_manager_legal) AS any_name
    FROM ordered
    GROUP BY 1, 2
  ),
  firsts AS (
    SELECT id_crd, is_exempt,
      name_entity_manager_legal AS name_first,
      amount_aum_total          AS first_aum,
      state_office_primary      AS first_state,
      country_office_primary    AS first_country,
      status_sec                AS first_status
    FROM ordered WHERE rn_first = 1
  ),
  lasts AS (
    SELECT id_crd, is_exempt,
      name_entity_manager_legal AS name_last,
      amount_aum_total          AS last_aum,
      state_office_primary      AS last_state,
      country_office_primary    AS last_country,
      status_sec                AS last_status
    FROM ordered WHERE rn_last = 1
  ),
  ria_era_streams AS (
    -- Did the firm appear under both is_exempt=true and is_exempt=false at any point?
    SELECT id_crd,
      BOOL_OR(is_exempt = true)  AS ever_era,
      BOOL_OR(is_exempt = false) AS ever_ria
    FROM panel GROUP BY 1
  )
  SELECT
    a.id_crd,
    a.is_exempt,
    COALESCE(l.name_last, f.name_first, a.any_name) AS name_entity_manager_legal,
    a.first_seen, a.last_seen,
    a.n_months_present, a.n_distinct_periods,
    f.first_aum, l.last_aum, a.max_aum, a.min_aum_positive,
    -- growth multiple, NULL-safe
    CASE WHEN f.first_aum > 0 THEN l.last_aum / f.first_aum ELSE NULL END AS growth_multiple,
    -- peak to last drawdown
    CASE WHEN a.max_aum > 0
         THEN 100.0 * (a.max_aum - l.last_aum) / a.max_aum
         ELSE NULL END AS peak_to_last_drawdown_pct,
    f.first_state, l.last_state,
    f.first_country, l.last_country,
    (f.first_state IS DISTINCT FROM l.last_state)     AS moved_state,
    (f.first_country IS DISTINCT FROM l.last_country) AS changed_country,
    f.first_status, l.last_status,
    a.max_employees, a.max_accounts,
    -- exited if last_seen is more than ~45 days before the latest panel snapshot
    (a.last_seen < (SELECT global_max FROM global_period)) AS exited_from_stream,
    -- AUM band on last appearance
    CASE
      WHEN l.last_aum >= 1e12 THEN 'mega_T+'
      WHEN l.last_aum >= 1e11 THEN 'large_100B+'
      WHEN l.last_aum >= 1e10 THEN 'mid_10B+'
      WHEN l.last_aum >= 1e9  THEN 'small_1B+'
      WHEN l.last_aum >= 1e8  THEN 'micro_100M+'
      WHEN l.last_aum >  0    THEN 'sub_100M'
      ELSE 'no_aum'
    END AS final_aum_band,
    s.ever_ria, s.ever_era
  FROM agg a
  JOIN firsts f USING (id_crd, is_exempt)
  JOIN lasts  l USING (id_crd, is_exempt)
  LEFT JOIN ria_era_streams s USING (id_crd)
  ORDER BY a.id_crd, a.is_exempt
"

df <- dbGetQuery(con, sql)
elapsed <- round(as.numeric(Sys.time() - t0, units = "secs"), 1)
cat(sprintf("    rows: %s · cols: %d · elapsed: %ss\n",
            format(nrow(df), big.mark=","), ncol(df), elapsed))

cat("[2/3] writing parquet (zstd-9)...\n")
arrow::write_parquet(df, OUTPUT, compression = "zstd", compression_level = 9L)
bytes <- file.info(OUTPUT)$size
cat(sprintf("    %s · %.1f MB\n", OUTPUT, bytes / 1024^2))

cat("[3/3] sanity checks...\n")
cat(sprintf("    distinct CRDs: %s\n",
            format(length(unique(df$id_crd)), big.mark=",")))
cat(sprintf("    streams (CRD+is_exempt): %s\n",
            format(nrow(df), big.mark=",")))
cat(sprintf("    exited streams: %s (%.1f%%)\n",
            format(sum(df$exited_from_stream, na.rm=TRUE), big.mark=","),
            100 * mean(df$exited_from_stream, na.rm=TRUE)))
cat(sprintf("    moved_state: %s · changed_country: %s\n",
            format(sum(df$moved_state, na.rm=TRUE), big.mark=","),
            format(sum(df$changed_country, na.rm=TRUE), big.mark=",")))
cat("    final_aum_band distribution:\n")
band_summary <- table(df$final_aum_band, useNA = "ifany")
for (nm in names(band_summary)) {
  cat(sprintf("      %-15s %s\n", nm, format(band_summary[[nm]], big.mark=",")))
}

dbDisconnect(con, shutdown = TRUE)
cat("[done]\n")
