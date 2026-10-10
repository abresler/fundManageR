#!/usr/bin/env Rscript
# Process a batch of ERISA Master Trust filings:
# 1) CloudSearch for candidate filings
# 2) Download + parse PDFs
# 3) Extract fund-shaped holdings + cross-walk to ADV 7B (incl. foreign domiciles)
# 4) Write parquet partitioned by year
# 5) Summary stats

options(scipen = 999, stringsAsFactors = FALSE)
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(stringr)
  library(glue); library(tibble); library(readr); library(arrow); library(curl)
  library(httr); library(jsonlite); library(pdftools)
})

setwd("~/Desktop/r_packages/fundManageR")
devtools::load_all(".", quiet = TRUE)

YEAR <- as.integer(Sys.getenv("YEAR", 2022))
MAX_FILINGS <- as.integer(Sys.getenv("MAX_FILINGS", 25))

# Target large plans: known PE-heavy sponsors + master trusts
# CloudSearch supports lucene; participantsboy stored as string so range is awkward
# Use known PE-heavy sponsor list as seed
PE_HEAVY_SPONSORS <- c(
  "BOEING", "IBM", "GENERAL ELECTRIC", "GENERAL MOTORS", "FORD MOTOR",
  "AT&T", "VERIZON", "EXXON", "CHEVRON", "JPMORGAN", "BANK OF AMERICA",
  "UNITED PARCEL", "RAYTHEON", "LOCKHEED", "NORTHROP", "HONEYWELL",
  "3M COMPANY", "PROCTER", "JOHNSON", "PEPSICO", "COCA-COLA",
  "CATERPILLAR", "DEERE", "TARGET", "WALMART", "DUPONT",
  "UNITED TECHNOLOGIES", "PFIZER", "MERCK", "ELI LILLY"
)

cat(sprintf("[%s] Searching %d known PE-heavy sponsors for year %d\n",
             format(Sys.time()), length(PE_HEAVY_SPONSORS), YEAR))

hits_list <- list()
for (sp in PE_HEAVY_SPONSORS) {
  q <- glue('plansponsor:"{sp}" AND planname:"MASTER" AND planyear:"{YEAR}"')
  res <- try(search_efast2(q, max_records = 5), silent = TRUE)
  if (!inherits(res, "try-error") && nrow(res) > 0) {
    hits_list[[sp]] <- res
  }
  Sys.sleep(0.1)
}
hits <- bind_rows(hits_list)
cat(sprintf("Found %d candidate large master trust filings\n", nrow(hits)))

hits <- hits %>%
  filter(!is.na(pdfpath)) %>%
  distinct(id_ack, .keep_all = TRUE) %>%
  arrange(id_ack) %>%
  head(MAX_FILINGS)

write_schedule_of_assets_to_lake(hits, max_filings = MAX_FILINGS)

cat(sprintf("[%s] Batch done.\n", format(Sys.time())))
