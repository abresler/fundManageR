#!/usr/bin/env Rscript
# Phase 1 LP-identification lake build
#
# Loads fundManageR (devtools::load_all), parses DoL Form 5500 latest year,
# writes parquet partitions to ~/Desktop/data/lake_lp_*.

options(scipen = 999, stringsAsFactors = FALSE)
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(glue)
  library(tibble)
  library(readr)
  library(arrow)
  library(curl)
})

setwd("~/Desktop/r_packages/fundManageR")
devtools::load_all(".", quiet = TRUE)

YEAR <- 2023
LOG  <- "~/Desktop/data/_raw/dol_5500/build_lp_lake_phase1.log"

cat(sprintf("[%s] PHASE 1 BUILD START year=%d\n", format(Sys.time()), YEAR), file = LOG, append = TRUE)

# Step 1: ensure files present (already downloaded, this just validates)
get_dol_form_5500_bulk(
  years = YEAR,
  schedules = c("F_5500", "F_SCH_C_PART1_ITEM1", "F_SCH_D_PART1", "F_SCH_D_PART2"),
  overwrite = FALSE
)

# Step 2: write to lake
res <- write_5500_year_to_lake(YEAR)

cat(sprintf("[%s] PHASE 1 BUILD DONE entities=%d edges=%d signals=%d\n",
             format(Sys.time()),
             res$n_entities, res$n_edges, res$n_signals),
    file = LOG, append = TRUE)

# Step 3: register lake summary
ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
summary_df <- tibble::tibble(
  ts             = ts,
  year_filing    = YEAR,
  n_entities     = res$n_entities,
  n_edges        = res$n_edges,
  n_signals      = res$n_signals,
  source_module  = "fundManageR::write_5500_year_to_lake"
)
arrow::write_parquet(
  summary_df,
  sprintf("~/Desktop/data/_raw/dol_5500/build_summary_%s.parquet", ts)
)

cat("\n=== SUMMARY ===\n")
print(summary_df)
