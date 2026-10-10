#!/usr/bin/env Rscript
# Phase 1 LP-identification lake — historical backfill
# Years: 2017-2023 (7 years × ~1.6M LP edges/year ≈ 11M+ edges)
# Source: DoL EFAST2 bulk archives

options(scipen = 999, stringsAsFactors = FALSE)
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(stringr)
  library(glue); library(tibble); library(readr); library(arrow); library(curl)
})

setwd("~/Desktop/r_packages/fundManageR")
devtools::load_all(".", quiet = TRUE)

YEARS <- 2017:2023
LOG   <- "~/Desktop/data/_raw/dol_5500/build_lp_lake_backfill.log"
TGT   <- "~/Desktop/data/_raw/dol_5500/backfill_progress.jsonl"

ts_now <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%S")

cat(sprintf("{\"ts\":\"%s\",\"event\":\"start\",\"years\":\"%d-%d\"}\n",
             ts_now(), min(YEARS), max(YEARS)),
    file = TGT, append = TRUE)

# Step 1: download all schedules across all years
schedules <- c("F_5500", "F_SCH_C_PART1_ITEM1", "F_SCH_D_PART1", "F_SCH_D_PART2")
dl <- get_dol_form_5500_bulk(years = YEARS, schedules = schedules, overwrite = FALSE)

cat(sprintf("{\"ts\":\"%s\",\"event\":\"download_done\",\"n_files\":%d}\n",
             ts_now(), nrow(dl)),
    file = TGT, append = TRUE)

# Step 2: parse + write each year
for (y in YEARS) {
  cat(sprintf("{\"ts\":\"%s\",\"event\":\"year_start\",\"year\":%d}\n",
               ts_now(), y),
      file = TGT, append = TRUE)
  res <- try(write_5500_year_to_lake(y), silent = TRUE)
  if (inherits(res, "try-error")) {
    cat(sprintf("{\"ts\":\"%s\",\"event\":\"year_failed\",\"year\":%d,\"error\":%s}\n",
                 ts_now(), y, jsonlite::toJSON(as.character(res))),
        file = TGT, append = TRUE)
  } else {
    cat(sprintf("{\"ts\":\"%s\",\"event\":\"year_done\",\"year\":%d,\"entities\":%d,\"edges\":%d,\"signals\":%d}\n",
                 ts_now(), y, res$n_entities, res$n_edges, res$n_signals),
        file = TGT, append = TRUE)
  }
}

cat(sprintf("{\"ts\":\"%s\",\"event\":\"all_done\"}\n", ts_now()),
    file = TGT, append = TRUE)
