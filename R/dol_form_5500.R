#' DoL Form 5500 EFAST2 Bulk Data Functions
#'
#' Functions for downloading and parsing the DoL EFAST2 Form 5500 bulk-data
#' archives. Form 5500 is the annual ERISA pension-plan filing. For
#' LP-identification purposes, Schedule D (Direct Filing Entity participation)
#' is the killer source: each row is a `(participating plan, pooled investment
#' vehicle, end-of-year value)` edge.
#'
#' Source: https://www.askebsa.dol.gov/FOIA%20Files/{year}/Latest/{filename}.zip
#'
#' Schedule files of interest:
#' \itemize{
#'   \item `F_5500_YYYY_Latest.zip` -- main filing (plan sponsor + EIN)
#'   \item `F_SCH_C_PART1_ITEM1_YYYY_Latest.zip` -- service providers (managers)
#'   \item `F_SCH_D_PART1_YYYY_Latest.zip` -- DFE -> participating plans
#'   \item `F_SCH_D_PART2_YYYY_Latest.zip` -- plan -> master trusts / 103-12 IEs
#'   \item `F_SCH_H_YYYY_Latest.zip` -- large-plan financial statements
#' }

EFAST2_BASE_URL <- "https://www.askebsa.dol.gov/FOIA%20Files"

EFAST2_SCHEDULES <- c(
  "F_5500",
  "F_SCH_C",
  "F_SCH_C_PART1_ITEM1",
  "F_SCH_C_PART1_ITEM2",
  "F_SCH_D",
  "F_SCH_D_PART1",
  "F_SCH_D_PART2",
  "F_SCH_H"
)

#' Download DoL EFAST2 Form 5500 Bulk Archives
#'
#' @param years integer vector of years to download (e.g. 2020:2023)
#' @param schedules character vector of schedule prefixes (default: full set)
#' @param dir staging directory (default: ~/Desktop/data/_raw/dol_5500)
#' @param overwrite re-download if file exists (default FALSE)
#' @return tibble of (year, schedule, path, bytes, status)
#' @export
get_dol_form_5500_bulk <- function(years,
                                    schedules = EFAST2_SCHEDULES,
                                    dir = "~/Desktop/data/_raw/dol_5500",
                                    overwrite = FALSE) {
  dir <- path.expand(dir)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)

  jobs <- tidyr::expand_grid(year = years, schedule = schedules)

  results <- purrr::pmap_dfr(jobs, function(year, schedule) {
    fname <- glue::glue("{schedule}_{year}_Latest.zip")
    url <- glue::glue("{EFAST2_BASE_URL}/{year}/Latest/{fname}")
    year_dir <- file.path(dir, as.character(year))
    dir.create(year_dir, recursive = TRUE, showWarnings = FALSE)
    dest <- file.path(year_dir, fname)

    if (!overwrite && file.exists(dest) && file.info(dest)$size > 1500) {
      .fm_info(glue::glue("Cached {fname} ({format(file.info(dest)$size, big.mark=',')} bytes)"))
      return(tibble::tibble(
        year = year, schedule = schedule, path = dest,
        bytes = file.info(dest)$size, status = "cached"
      ))
    }

    .fm_parsing(url)
    res <- try(curl::curl_download(url, dest, quiet = TRUE), silent = TRUE)
    if (inherits(res, "try-error") || !file.exists(dest)) {
      return(tibble::tibble(
        year = year, schedule = schedule, path = dest,
        bytes = NA_real_, status = "failed"
      ))
    }
    sz <- file.info(dest)$size
    if (sz < 1500) {
      file.remove(dest)
      return(tibble::tibble(
        year = year, schedule = schedule, path = dest,
        bytes = sz, status = "not_published"
      ))
    }

    extract_dir <- sub("\\.zip$", "", dest)
    utils::unzip(dest, exdir = extract_dir, overwrite = TRUE)

    tibble::tibble(
      year = year, schedule = schedule, path = dest,
      bytes = sz, status = "downloaded"
    )
  })

  .fm_data_acquired(
    n_rows = sum(results$status %in% c("downloaded", "cached")),
    source = "DoL EFAST2",
    extra = glue::glue("years {min(years)}-{max(years)}, {length(schedules)} schedule types")
  )

  results
}


#' Parse Form 5500 Main Filing CSV
#'
#' Returns plan-sponsor-level facts: id_ack, ein, plan_name, sponsor_name,
#' plan_year, business_code, plan_type. This is the spine for `lake_lp_entities`
#' (each unique sponsor EIN = one ERISA plan-sponsor LP).
#'
#' @param year filing year
#' @param dir staging dir
#' @return tibble with prefix `name_*`, `id_*`, `date_*`, `is_*`
#' @export
parse_5500_main <- function(year, dir = "~/Desktop/data/_raw/dol_5500") {
  csv <- file.path(
    path.expand(dir), as.character(year),
    glue::glue("F_5500_{year}_Latest"),
    glue::glue("f_5500_{year}_latest.csv")
  )
  if (!file.exists(csv)) stop("Not found: ", csv)
  raw <- readr::read_csv(csv, show_col_types = FALSE,
                          col_types = readr::cols(.default = "c"))
  out <- tibble::tibble(
    id_ack             = raw$ACK_ID,
    id_sponsor_ein     = raw$SPONS_DFE_EIN,
    id_plan_pn         = raw$SPONS_DFE_PN,
    name_plan          = raw$PLAN_NAME,
    name_sponsor       = raw$SPONSOR_DFE_NAME,
    name_sponsor_dba   = raw$SPONS_DFE_DBA_NAME,
    code_business      = raw$BUSINESS_CODE,
    code_plan_type     = raw$TYPE_PLAN_ENTITY_CD,
    code_dfe_type      = raw$TYPE_DFE_PLAN_ENTITY_CD,
    state_sponsor      = raw$SPONS_DFE_MAIL_US_STATE,
    country_sponsor    = ifelse(
      is.na(raw$SPONS_DFE_MAIL_US_STATE),
      raw$SPONS_DFE_MAIL_FOREIGN_CNTRY, "US"
    ),
    # HARD RULE: dates as DATE, never VARCHAR. Source emits ISO YYYY-MM-DD.
    date_plan_year_begin = suppressWarnings(as.Date(raw$FORM_PLAN_YEAR_BEGIN_DATE)),
    date_tax_period_end  = suppressWarnings(as.Date(raw$FORM_TAX_PRD)),
    is_initial_filing  = raw$INITIAL_FILING_IND == "1",
    is_amended         = raw$AMENDED_IND == "1",
    is_final_filing    = raw$FINAL_FILING_IND == "1",
    year_filing        = year
  )
  out
}


#' Parse Schedule D Part 1 (DFE -> Participating Plans)
#'
#' Each row is filed BY the Direct Filing Entity (CCT, PSA, MTIA, 103-12 IE,
#' GIA) and lists ONE participating ERISA plan. This is the LP -> fund edge.
#'
#' DFE_P1_ENTITY_NAME = the pooled investment vehicle's name (the "fund")
#' DFE_P1_PLAN_EIN    = the participating ERISA plan's EIN (the "LP")
#' DFE_P1_PLAN_INT_EOY_AMT = end-of-year $ value of plan's interest in the DFE
#' DFE_P1_ENTITY_CODE = C(CCT) / P(PSA) / M(MTIA) / E(103-12 IE) / G(GIA)
#'
#' @param year filing year
#' @return tibble of LP -> fund edges with $ values
#' @export
parse_5500_schedule_d_part1 <- function(year, dir = "~/Desktop/data/_raw/dol_5500") {
  csv <- file.path(
    path.expand(dir), as.character(year),
    glue::glue("F_SCH_D_PART1_{year}_Latest"),
    glue::glue("F_SCH_D_PART1_{year}_latest.csv")
  )
  if (!file.exists(csv)) stop("Not found: ", csv)
  raw <- readr::read_csv(csv, show_col_types = FALSE,
                          col_types = readr::cols(
                            .default = "c",
                            DFE_P1_PLAN_INT_EOY_AMT = "d",
                            ROW_ORDER = "i"
                          ))

  entity_decode <- c(
    "C" = "common_collective_trust",
    "P" = "pooled_separate_account",
    "M" = "master_trust_investment_account",
    "E" = "investment_entity_103_12",
    "G" = "group_insurance_arrangement"
  )

  tibble::tibble(
    id_ack_dfe              = raw$ACK_ID,
    row_order               = raw$ROW_ORDER,
    name_dfe_entity         = raw$DFE_P1_ENTITY_NAME,
    name_dfe_sponsor        = raw$DFE_P1_SPONS_NAME,
    id_lp_plan_ein          = raw$DFE_P1_PLAN_EIN,
    id_lp_plan_pn           = raw$DFE_P1_PLAN_PN,
    code_dfe_entity         = raw$DFE_P1_ENTITY_CODE,
    type_dfe                = unname(entity_decode[raw$DFE_P1_ENTITY_CODE]),
    amount_plan_interest_eoy = raw$DFE_P1_PLAN_INT_EOY_AMT,
    year_filing             = year
  )
}


#' Parse Schedule D Part 2 (Plan -> Master Trust / 103-12 IE)
#'
#' Each row is filed BY a plan and lists ONE pooled vehicle it participates in.
#' The plan-side perspective of LP -> fund.
#'
#' @param year filing year
#' @return tibble of plan -> vehicle edges
#' @export
parse_5500_schedule_d_part2 <- function(year, dir = "~/Desktop/data/_raw/dol_5500") {
  csv <- file.path(
    path.expand(dir), as.character(year),
    glue::glue("F_SCH_D_PART2_{year}_Latest"),
    glue::glue("F_SCH_D_PART2_{year}_latest.csv")
  )
  if (!file.exists(csv)) stop("Not found: ", csv)
  raw <- readr::read_csv(csv, show_col_types = FALSE,
                          col_types = readr::cols(.default = "c", ROW_ORDER = "i"))

  tibble::tibble(
    id_ack_plan      = raw$ACK_ID,
    row_order        = raw$ROW_ORDER,
    name_dfe_entity  = raw$DFE_P2_PLAN_NAME,
    name_dfe_sponsor = raw$DFE_P2_PLAN_SPONS_NAME,
    id_dfe_ein       = raw$DFE_P2_PLAN_EIN,
    id_dfe_pn        = raw$DFE_P2_PLAN_PN,
    year_filing      = year
  )
}


#' Parse Schedule C Part 1 Item 1 (Service Providers)
#'
#' Plan -> service provider (incl. investment managers, RIAs, custodians,
#' fund-of-fund advisors). Provides plan -> fund-manager edges where the
#' "fund manager" is a separate signal from Schedule D's "fund vehicle".
#'
#' @param year filing year
#' @return tibble of plan -> service provider edges
#' @export
parse_5500_schedule_c <- function(year, dir = "~/Desktop/data/_raw/dol_5500") {
  csv <- file.path(
    path.expand(dir), as.character(year),
    glue::glue("F_SCH_C_PART1_ITEM1_{year}_Latest"),
    glue::glue("F_SCH_C_PART1_ITEM1_{year}_latest.csv")
  )
  if (!file.exists(csv)) stop("Not found: ", csv)
  raw <- readr::read_csv(csv, show_col_types = FALSE,
                          col_types = readr::cols(.default = "c", ROW_ORDER = "i"))

  tibble::tibble(
    id_ack_plan       = raw$ACK_ID,
    row_order         = raw$ROW_ORDER,
    name_provider     = raw$PROVIDER_ELIGIBLE_NAME,
    id_provider_ein   = raw$PROVIDER_ELIGIBLE_EIN,
    address_provider  = raw$PROVIDER_ELIGIBLE_US_ADDRESS1,
    city_provider     = raw$PROVIDER_ELIGIBLE_US_CITY,
    state_provider    = raw$PROVIDER_ELIGIBLE_US_STATE,
    zip_provider      = raw$PROVIDER_ELIGIBLE_US_ZIP,
    country_provider  = ifelse(
      is.na(raw$PROVIDER_ELIGIBLE_US_STATE),
      raw$PROV_ELIGIBLE_FOREIGN_CNTRY, "US"
    ),
    year_filing = year
  )
}


#' Normalize Entity Name for Joining (LP / Fund / Manager)
#'
#' Mirrors the dq `name_norm()` macro used across govtrackR / fundManageR /
#' sheldon. Applies: lowercase, strip punctuation, collapse whitespace, drop
#' generic suffixes (LP, LLC, INC, FUND, TRUST, etc.).
#'
#' @param x character vector of names
#' @return character vector of normalized names
#' @export
name_norm_entity <- function(x) {
  x %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all("[[:punct:]]", " ") %>%
    stringr::str_replace_all(
      "\\b(lp|llc|llp|inc|incorporated|corp|corporation|company|co|trust|fund|fd|partners|partnership|partner|advisors|advisers|management|mgmt|capital|cap|holdings|holding|group|grp|usa|us|international|intl|the|of|and|na|n a)\\b",
      ""
    ) %>%
    stringr::str_squish()
}


#' Persist Form 5500 Year to LP Lake (Parquet, Partitioned)
#'
#' Writes 4 outputs partitioned by `year=`:
#'   - lake_lp_entities/source=erisa_plan_sponsor/year=YYYY/...
#'   - lake_lp_fund_commitments/source=erisa_5500_d_part1/year=YYYY/...
#'   - lake_lp_fund_commitments/source=erisa_5500_d_part2/year=YYYY/...
#'   - lake_lp_signals/source=erisa_5500_c_provider/year=YYYY/...
#'
#' @param year filing year
#' @param data_root output root (default ~/Desktop/data)
#' @return invisible list of write paths
#' @export
write_5500_year_to_lake <- function(year, data_root = "~/Desktop/data") {
  data_root <- path.expand(data_root)

  .fm_headline(glue::glue("ERISA panel: {year}"))

  main <- parse_5500_main(year)
  d1   <- parse_5500_schedule_d_part1(year)
  d2   <- parse_5500_schedule_d_part2(year)
  c1   <- parse_5500_schedule_c(year)

  .fm_info(glue::glue("Main: {format(nrow(main), big.mark=',')} plans"))
  .fm_info(glue::glue("Sch-D Part1: {format(nrow(d1), big.mark=',')} LP edges"))
  .fm_info(glue::glue("Sch-D Part2: {format(nrow(d2), big.mark=',')} plan edges"))
  .fm_info(glue::glue("Sch-C: {format(nrow(c1), big.mark=',')} provider rows"))

  entities <- main %>%
    dplyr::filter(!is.na(.data$id_sponsor_ein),
                   !is.na(.data$name_sponsor)) %>%
    dplyr::distinct(.data$id_sponsor_ein, .data$name_sponsor,
                     .keep_all = TRUE) %>%
    dplyr::transmute(
      id_lp_canonical = paste0("erisa-", .data$id_sponsor_ein),
      id_sponsor_ein  = .data$id_sponsor_ein,
      name_lp         = .data$name_sponsor,
      name_lp_norm    = name_norm_entity(.data$name_sponsor),
      type_lp         = "erisa_plan_sponsor",
      jurisdiction    = .data$country_sponsor,
      state           = .data$state_sponsor,
      code_business   = .data$code_business,
      year_filing     = year
    )

  edges_d1 <- d1 %>%
    dplyr::filter(!is.na(.data$id_lp_plan_ein),
                   !is.na(.data$name_dfe_entity)) %>%
    dplyr::transmute(
      id_lp_canonical    = paste0("erisa-", .data$id_lp_plan_ein),
      name_fund          = .data$name_dfe_entity,
      name_fund_norm     = name_norm_entity(.data$name_dfe_entity),
      name_manager       = .data$name_dfe_sponsor,
      name_manager_norm  = name_norm_entity(.data$name_dfe_sponsor),
      type_vehicle       = .data$type_dfe,
      amount_commitment  = .data$amount_plan_interest_eoy,
      currency           = "USD",
      date_disclosed     = paste0(year, "-12-31"),
      source             = "erisa_5500_d_part1",
      confidence_tier    = 1L,
      url_source         = glue::glue("{EFAST2_BASE_URL}/{year}/Latest/F_SCH_D_PART1_{year}_Latest.zip"),
      year_filing        = year
    )

  # Schedule D Part 2 is filed BY the master trust / 103-12 IE listing
  # participating plans. So ACK_ID = the FUND filer; row = a participating LP.
  # The fund's name comes from joining ACK_ID back to main 5500 for filer plan
  # name (which is the master trust's "plan name" because DFEs file as plans).
  fund_name_lookup <- main %>%
    dplyr::select(.data$id_ack, .data$name_plan, .data$name_sponsor) %>%
    dplyr::distinct(.data$id_ack, .keep_all = TRUE)

  edges_d2 <- d2 %>%
    dplyr::filter(!is.na(.data$id_dfe_ein),
                   !is.na(.data$name_dfe_entity)) %>%
    dplyr::left_join(fund_name_lookup, by = c("id_ack_plan" = "id_ack")) %>%
    dplyr::transmute(
      id_lp_canonical    = paste0("erisa-", .data$id_dfe_ein),
      name_fund          = dplyr::coalesce(.data$name_plan, paste0("(filer-ack-", .data$id_ack_plan, ")")),
      name_fund_norm     = name_norm_entity(dplyr::coalesce(.data$name_plan, "")),
      name_manager       = dplyr::coalesce(.data$name_sponsor, .data$name_dfe_sponsor),
      name_manager_norm  = name_norm_entity(dplyr::coalesce(.data$name_sponsor, .data$name_dfe_sponsor)),
      participating_plan_name = .data$name_dfe_entity,
      type_vehicle       = "master_trust_or_103_12_ie",
      amount_commitment  = NA_real_,
      currency           = "USD",
      date_disclosed     = paste0(year, "-12-31"),
      source             = "erisa_5500_d_part2",
      confidence_tier    = 2L,
      url_source         = glue::glue("{EFAST2_BASE_URL}/{year}/Latest/F_SCH_D_PART2_{year}_Latest.zip"),
      year_filing        = year
    )

  signals_c <- c1 %>%
    dplyr::filter(!is.na(.data$name_provider)) %>%
    dplyr::transmute(
      id_ack_plan       = .data$id_ack_plan,
      name_provider     = .data$name_provider,
      name_provider_norm = name_norm_entity(.data$name_provider),
      id_provider_ein   = .data$id_provider_ein,
      city_provider     = .data$city_provider,
      state_provider    = .data$state_provider,
      country_provider  = .data$country_provider,
      signal_type       = "erisa_service_provider",
      source            = "erisa_5500_c_part1_item1",
      confidence_tier   = 3L,
      year_filing       = year
    )

  ent_dir   <- file.path(data_root, "lake_lp_entities/subdomain=erisa_plan_sponsor",
                          glue::glue("year={year}"))
  ed1_dir   <- file.path(data_root, "lake_lp_fund_commitments/subdomain=erisa_5500_d_part1",
                          glue::glue("year={year}"))
  ed2_dir   <- file.path(data_root, "lake_lp_fund_commitments/subdomain=erisa_5500_d_part2",
                          glue::glue("year={year}"))
  sig_dir   <- file.path(data_root, "lake_lp_signals/subdomain=erisa_5500_c_provider",
                          glue::glue("year={year}"))

  for (d in c(ent_dir, ed1_dir, ed2_dir, sig_dir)) {
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }

  arrow::write_parquet(entities,  file.path(ent_dir,
                                              glue::glue("erisa_plan_sponsor_{year}.zstd.parquet")),
                        compression = "zstd")
  arrow::write_parquet(edges_d1,  file.path(ed1_dir,
                                              glue::glue("erisa_5500_d_part1_{year}.zstd.parquet")),
                        compression = "zstd")
  arrow::write_parquet(edges_d2,  file.path(ed2_dir,
                                              glue::glue("erisa_5500_d_part2_{year}.zstd.parquet")),
                        compression = "zstd")
  arrow::write_parquet(signals_c, file.path(sig_dir,
                                              glue::glue("erisa_5500_c_provider_{year}.zstd.parquet")),
                        compression = "zstd")

  .fm_success(glue::glue("Written: {format(nrow(entities), big.mark=',')} LPs, ",
                          "{format(nrow(edges_d1) + nrow(edges_d2), big.mark=',')} edges, ",
                          "{format(nrow(signals_c), big.mark=',')} signals"))

  invisible(list(
    entities = ent_dir,
    edges_d1 = ed1_dir,
    edges_d2 = ed2_dir,
    signals  = sig_dir,
    n_entities = nrow(entities),
    n_edges = nrow(edges_d1) + nrow(edges_d2),
    n_signals = nrow(signals_c)
  ))
}
