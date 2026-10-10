#!/usr/bin/env Rscript
# 7-year Schedule of Assets backfill: reuse existing pipeline across 2017-2023
# Then re-run cayman fuzzy matcher across all years
options(scipen = 999)
suppressPackageStartupMessages({
  library(dplyr); library(stringr); library(tibble); library(purrr); library(glue)
  library(readr); library(arrow); library(curl); library(httr); library(jsonlite); library(pdftools)
})
setwd("~/Desktop/r_packages/fundManageR")
devtools::load_all(".", quiet = TRUE)

PROG <- path.expand("~/Desktop/data/_raw/dol_5500/soa_7year_progress.jsonl")
ts_now <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
log_evt <- function(...) cat(jsonlite::toJSON(list(ts = ts_now(), ...), auto_unbox = TRUE), "\n",
                              sep = "", file = PROG, append = TRUE)

PE_HEAVY <- c("BOEING", "IBM", "GENERAL ELECTRIC", "GENERAL MOTORS", "FORD MOTOR",
              "AT&T", "VERIZON", "EXXON", "CHEVRON", "JPMORGAN", "BANK OF AMERICA",
              "UNITED PARCEL", "RAYTHEON", "LOCKHEED", "NORTHROP", "HONEYWELL",
              "3M COMPANY", "PROCTER", "PEPSICO", "COCA-COLA", "CATERPILLAR",
              "DEERE", "TARGET", "WALMART", "DUPONT", "PFIZER", "MERCK", "ELI LILLY",
              "DOW CHEMICAL", "BANK OF NEW YORK", "DELTA AIR", "AMERICAN AIRLINES",
              "UNITED AIRLINES", "GOLDMAN SACHS", "MORGAN STANLEY")

YEARS <- 2017:2023

log_evt(event = "start", years = paste(YEARS, collapse = ","), sponsors = length(PE_HEAVY))

for (y in YEARS) {
  log_evt(event = "year_start", year = y)
  hits_list <- list()
  for (sp in PE_HEAVY) {
    q <- glue('plansponsor:"{sp}" AND planname:"MASTER" AND planyear:"{y}"')
    res <- try(search_efast2(q, max_records = 5), silent = TRUE)
    if (!inherits(res, "try-error") && nrow(res) > 0) hits_list[[sp]] <- res
    Sys.sleep(0.05)
  }
  hits <- if (length(hits_list)) bind_rows(hits_list) else tibble()
  if (!nrow(hits)) { log_evt(event = "year_no_hits", year = y); next }
  hits <- hits %>% filter(!is.na(pdfpath)) %>% distinct(id_ack, .keep_all = TRUE)
  log_evt(event = "year_hits", year = y, n_filings = nrow(hits))
  res <- try(write_schedule_of_assets_to_lake(hits, max_filings = 100), silent = TRUE)
  log_evt(event = "year_done", year = y,
          n_rows = if (inherits(res, "try-error") || is.null(res)) 0 else nrow(res))
}

log_evt(event = "all_done")
