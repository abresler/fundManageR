#!/usr/bin/env Rscript
# Phase 2 LP Lake: 5-iteration sequential build, single process, Heartbeat-monitored
# IRR-ordered: Cayman fuzzy first (highest yield), then ingesters, then cleanup.

options(scipen = 999, stringsAsFactors = FALSE)
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(stringr)
  library(glue); library(tibble); library(readr); library(arrow); library(curl)
  library(httr); library(jsonlite)
})

setwd("~/Desktop/r_packages/fundManageR")
devtools::load_all(".", quiet = TRUE)

PROG <- "~/Desktop/data/_raw/dol_5500/phase2_progress.jsonl"
DATA <- path.expand("~/Desktop/data")
ts_now <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
log_evt <- function(...) {
  cat(jsonlite::toJSON(list(ts = ts_now(), ...), auto_unbox = TRUE), "\n",
      sep = "", file = path.expand(PROG), append = TRUE)
}

# Cap-check
cap_ok <- function() {
  sz <- as.numeric(system(
    "du -sb ~/Desktop/data/lake_lp_* 2>/dev/null | awk '{s+=$1} END {print s}'",
    intern = TRUE))
  log_evt(event = "cap_check", lake_bytes = sz, cap_bytes = 1073741824L,
          ok = sz < 1073741824L)
  sz < 1073741824L
}

# ============ ITER 1: Cayman fuzzy matcher ============
log_evt(event = "iter_start", iter = 1, name = "cayman_fuzzy")
tryCatch({
  con <- DBI::dbConnect(duckdb::duckdb())
  DBI::dbExecute(con, "INSTALL parquet; LOAD parquet;")

  # Pull ADV Cayman funds + manager
  adv_cayman <- DBI::dbGetQuery(con, sprintf("
    SELECT DISTINCT id_private_fund, name_fund_clean, name_entity_manager,
                    id_crd, type_fund
    FROM read_parquet('%s/sec_adv/section=section_7_b_private_fund_reporting/**/*.parquet', union_by_name=true)
    WHERE id_private_fund IS NOT NULL AND name_fund_clean IS NOT NULL
      AND UPPER(location_fund_incorporation) = 'CAYMAN ISLANDS'", DATA))

  # Pull SoA holdings + filer plan name
  soa <- DBI::dbGetQuery(con, sprintf("
    SELECT DISTINCT name_fund AS pdf_name, name_manager AS pdf_manager,
                    participating_plan_name, amount_commitment, year_filing
    FROM read_parquet('%s/lake_lp_fund_commitments/subdomain=erisa_schedule_of_assets/**/*.parquet', union_by_name=true)", DATA))

  DBI::dbDisconnect(con, shutdown = TRUE)

  # Fuzzy match: token-set overlap + manager substring containment
  norm_loose <- function(x) {
    str_to_lower(x) %>%
      str_replace_all("[[:punct:]]", " ") %>%
      str_replace_all("\\b(lp|llc|inc|corp|ltd|fund|trust|partners|partnership|the|of|and|na|company|co|series|class|master|feeder|cayman|delaware|bvi|offshore|onshore|international|intl)\\b", " ") %>%
      str_squish()
  }
  adv_cayman$nfm <- norm_loose(adv_cayman$name_fund_clean)
  adv_cayman$nmm <- norm_loose(adv_cayman$name_entity_manager)
  soa$nfm <- norm_loose(soa$pdf_name)

  # Fast token-set Jaccard
  toks <- function(s) strsplit(s, "\\s+")
  jaccard <- function(a, b) {
    A <- toks(a)[[1]]; B <- toks(b)[[1]]
    A <- A[nchar(A) >= 3]; B <- B[nchar(B) >= 3]
    if (!length(A) || !length(B)) return(0)
    length(intersect(A, B)) / length(union(A, B))
  }

  # For speed, only consider candidate pairs that share ≥1 substantial token
  edges <- list()
  adv_idx <- adv_cayman %>% mutate(i = row_number())
  for (i in seq_len(nrow(soa))) {
    if (i %% 500 == 0) log_evt(event = "iter_progress", iter = 1, soa_row = i,
                                so_far = length(edges))
    s <- soa$nfm[i]
    if (is.na(s) || nchar(s) < 5) next
    s_toks <- toks(s)[[1]]
    s_toks <- s_toks[nchar(s_toks) >= 4]
    if (!length(s_toks)) next
    cand <- adv_idx %>%
      filter(stringr::str_detect(.data$nfm,
                                   paste0("\\b(", paste(s_toks, collapse = "|"), ")\\b")))
    if (!nrow(cand)) next
    cand$jc <- vapply(cand$nfm, jaccard, 0, b = s, USE.NAMES = FALSE)
    cand <- cand %>% filter(.data$jc >= 0.5) %>% arrange(desc(.data$jc))
    if (nrow(cand)) {
      best <- cand[1, ]
      edges[[length(edges) + 1L]] <- tibble(
        id_private_fund = best$id_private_fund,
        id_crd          = best$id_crd,
        name_fund_adv   = best$name_fund_clean,
        name_manager_adv = best$name_entity_manager,
        type_fund       = best$type_fund,
        pdf_name        = soa$pdf_name[i],
        participating_plan_name = soa$participating_plan_name[i],
        amount_commitment = soa$amount_commitment[i],
        year_filing     = soa$year_filing[i],
        jaccard_score   = best$jc,
        source          = "cayman_fuzzy_match",
        confidence_tier = if (best$jc >= 0.8) 1L else 2L
      )
    }
  }
  fuzzy <- if (length(edges)) bind_rows(edges) else tibble()

  if (nrow(fuzzy) > 0) {
    out_dir <- file.path(DATA, "lake_lp_fund_commitments/subdomain=cayman_fuzzy_match",
                          glue("year={max(fuzzy$year_filing, na.rm=TRUE)}"))
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    arrow::write_parquet(fuzzy,
                          file.path(out_dir, glue("cayman_fuzzy_{format(Sys.time(),'%Y%m%d_%H%M%S')}.zstd.parquet")),
                          compression = "zstd")
  }
  log_evt(event = "iter_done", iter = 1, name = "cayman_fuzzy",
          rows = nrow(fuzzy),
          distinct_cayman_funds = n_distinct(fuzzy$id_private_fund))
}, error = function(e) {
  log_evt(event = "iter_failed", iter = 1, error = conditionMessage(e))
})

# ============ ITER 2: NJ Pension Explorer Socrata ============
log_evt(event = "iter_start", iter = 2, name = "nj_socrata")
tryCatch({
  # Probe Socrata API
  url <- "https://data.nj.gov/resource/asti-vewj.json?$limit=5000"
  resp <- httr::GET(url, httr::user_agent("fundManageR/0.1"),
                     httr::timeout(60))
  if (httr::status_code(resp) == 200) {
    rows <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"))
    log_evt(event = "iter_done", iter = 2, name = "nj_socrata",
            rows = if (is.data.frame(rows)) nrow(rows) else 0L,
            note = "API responsive but dataset asti-vewj may not be the right one")
  } else {
    log_evt(event = "iter_skipped", iter = 2, status = httr::status_code(resp),
            reason = "Socrata dataset id needs manual discovery")
  }
}, error = function(e) {
  log_evt(event = "iter_failed", iter = 2, error = conditionMessage(e))
})

# ============ ITER 3: NY CRF ACFR PDF ============
log_evt(event = "iter_start", iter = 3, name = "ny_crf_acfr")
tryCatch({
  acfr_url <- "https://www.osc.ny.gov/files/common-retirement-fund/pdf/cafr-2024.pdf"
  dest <- tempfile(fileext = ".pdf")
  ok <- try(curl::curl_download(acfr_url, dest, quiet = TRUE), silent = TRUE)
  if (inherits(ok, "try-error") || !file.exists(dest) || file.info(dest)$size < 100000) {
    log_evt(event = "iter_skipped", iter = 3, reason = "ACFR URL not found, manual discovery needed")
  } else {
    holdings <- parse_5500_pdf_holdings(dest)
    funds <- filter_to_fund_holdings(holdings)
    if (nrow(funds) > 0) {
      out_dir <- file.path(DATA, "lake_lp_fund_commitments/subdomain=us_state_pension_acfr/year=2024")
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
      arrow::write_parquet(
        funds %>% mutate(
          id_lp_canonical = "us-pension-ny-crf",
          name_lp = "NEW YORK STATE COMMON RETIREMENT FUND",
          source = "ny_crf_acfr",
          year_filing = 2024L,
          confidence_tier = 1L
        ),
        file.path(out_dir, glue("ny_crf_acfr_{format(Sys.time(),'%Y%m%d_%H%M%S')}.zstd.parquet")),
        compression = "zstd")
    }
    file.remove(dest)
    log_evt(event = "iter_done", iter = 3, name = "ny_crf_acfr",
            rows = nrow(funds))
  }
}, error = function(e) {
  log_evt(event = "iter_failed", iter = 3, error = conditionMessage(e))
})

# ============ ITER 4: CourtListener RECAP ERISA fee-suit watcher ============
log_evt(event = "iter_start", iter = 4, name = "courtlistener_erisa")
tryCatch({
  url <- "https://www.courtlistener.com/api/rest/v3/search/?type=r&q=ERISA+excessive+fees+401k&order_by=dateFiled+desc"
  resp <- httr::GET(url, httr::user_agent("fundManageR/0.1"),
                     httr::timeout(60))
  if (httr::status_code(resp) == 200) {
    data <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"))
    n <- if (!is.null(data$results)) nrow(data$results) else 0L
    log_evt(event = "iter_done", iter = 4, name = "courtlistener_erisa",
            results_returned = n,
            total_count = data$count %||% 0,
            note = "needs API token + per-docket exhibit text fetch for fund-name extraction; deferring to backlog")
  } else {
    log_evt(event = "iter_skipped", iter = 4, status = httr::status_code(resp))
  }
}, error = function(e) {
  log_evt(event = "iter_failed", iter = 4, error = conditionMessage(e))
})

# ============ ITER 5: Cleanup + final stats ============
log_evt(event = "iter_start", iter = 5, name = "final_stats")
tryCatch({
  cap_ok()
  con <- DBI::dbConnect(duckdb::duckdb())
  stats <- DBI::dbGetQuery(con, sprintf("
    SELECT source, COUNT(*) AS rows,
           COUNT(DISTINCT id_lp_canonical) AS distinct_lps,
           COUNT(DISTINCT name_fund_norm) AS distinct_funds
    FROM read_parquet('%s/lake_lp_fund_commitments/**/*.parquet', union_by_name=true)
    GROUP BY 1 ORDER BY rows DESC", DATA))
  DBI::dbDisconnect(con, shutdown = TRUE)

  for (i in seq_len(nrow(stats))) {
    log_evt(event = "stat", source = stats$source[i],
             rows = stats$rows[i],
             distinct_lps = stats$distinct_lps[i],
             distinct_funds = stats$distinct_funds[i])
  }
  log_evt(event = "iter_done", iter = 5, name = "final_stats",
          n_sources = nrow(stats))
}, error = function(e) {
  log_evt(event = "iter_failed", iter = 5, error = conditionMessage(e))
})

log_evt(event = "all_done")

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
