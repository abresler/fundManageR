# Jurisdiction risk scoring for private fund incorporation.
#
# Sourced from SHELDON Knowledge Bases/OffshoreFundJurisdictions/
# FATF 2026-02-13 · EU 2026-02-17 · TJN FSI 2025 rolling.
# Bayes log-likelihood composition documented at
# OffshoreFundJurisdictions/wiki/bayes_risk_scoring.md.

.jurisdiction_aliases <- c(
  "UK" = "UNITED KINGDOM",
  "USA" = "UNITED STATES",
  "U.S." = "UNITED STATES",
  "U.S.A." = "UNITED STATES",
  "BVI" = "BRITISH VIRGIN ISLANDS",
  "DPRK" = "NORTH KOREA"
)

#' Jurisdiction risk dictionary
#'
#' Returns the Bayesian risk scoring table for fund-incorporation jurisdictions,
#' sourced from the SHELDON Knowledge Base
#' (\code{OffshoreFundJurisdictions/data/jurisdiction_risk.csv}) and shipped with
#' the package at \code{inst/extdata/jurisdiction_risk.csv}.
#'
#' Raw flags come from FATF (Call for Action / Increased Monitoring),
#' EU Council (Annex I / Annex II), Tax Justice Network Financial Secrecy Index,
#' and an OFAC cross-check. The composed \code{rating_jurisdiction_risk}
#' (0-100) and \code{flag_jurisdiction_risk} (CLEAR/WATCH/CAUTION/HIGH/CRITICAL)
#' are computed from those flags via log-likelihood Bayes composition --
#' see \code{?flag_fund_jurisdictions} and the KB article.
#'
#' @return tibble with one row per jurisdiction
#' @export
#' @family jurisdiction risk
#' @examples
#' \dontrun{
#' library(dplyr)
#' dictionary_jurisdiction_risk() %>%
#'   filter(flag_jurisdiction_risk %in% c("HIGH", "CRITICAL"))
#' }
dictionary_jurisdiction_risk <- function() {
  fp <- system.file("extdata", "jurisdiction_risk.csv", package = "fundManageR")
  if (!nzchar(fp)) {
    stop("jurisdiction_risk.csv not found in installed package extdata/")
  }
  raw <- readr::read_csv(fp, show_col_types = FALSE, progress = FALSE)
  .score_jurisdiction_table(raw)
}

# Bayes log-likelihood composition, applied row-wise on the raw flag tibble.
.score_jurisdiction_table <- function(df) {
  prior_logit <- log(0.10 / 0.90)  # -2.197
  df <- df %>%
    dplyr::mutate(
      logit_posterior =
        prior_logit +
        dplyr::if_else(is_fatf_blacklist,         5.0, 0) +
        dplyr::if_else(is_fatf_greylist,          3.0, 0) +
        dplyr::if_else(is_eu_blacklist_annex_1,   3.0, 0) +
        dplyr::if_else(is_eu_blacklist_annex_2,   1.5, 0) +
        dplyr::if_else(is_fsi_top_10,             1.0, 0) +
        dplyr::if_else(is_fsi_top_20 & !is_fsi_top_10, 0.5, 0) +
        dplyr::if_else(is_traditional_tax_haven,  1.5, 0) +
        dplyr::if_else(is_us_secrecy_state,       0.5, 0) +
        dplyr::if_else(has_recent_fatf_scrutiny & !is_fatf_greylist & !is_fatf_blacklist, 0.5, 0),
      rating_jurisdiction_risk = round(100 * plogis(logit_posterior)),
      flag_jurisdiction_risk = dplyr::case_when(
        rating_jurisdiction_risk >= 90 ~ "CRITICAL",
        rating_jurisdiction_risk >= 70 ~ "HIGH",
        rating_jurisdiction_risk >= 45 ~ "CAUTION",
        rating_jurisdiction_risk >= 25 ~ "WATCH",
        TRUE                          ~ "CLEAR"
      ),
      source_jurisdiction_risk = purrr::pmap_chr(
        list(is_fatf_blacklist, is_fatf_greylist, is_eu_blacklist_annex_1,
             is_eu_blacklist_annex_2, is_fsi_top_10, is_fsi_top_20,
             is_traditional_tax_haven, is_us_secrecy_state, has_recent_fatf_scrutiny),
        function(fb, fg, e1, e2, f10, f20, th, ss, rs) {
          bits <- c(
            if (fb)  "fatf-blacklist",
            if (fg)  "fatf-greylist",
            if (e1)  "eu-annex-i",
            if (e2)  "eu-annex-ii",
            if (f10) "fsi-top-10",
            if (f20 && !f10) "fsi-top-20",
            if (th)  "traditional-haven",
            if (ss)  "us-secrecy-state",
            if (rs && !fg && !fb) "recent-fatf-scrutiny"
          )
          if (length(bits) == 0) NA_character_ else paste(bits, collapse = "|")
        }
      )
    ) %>%
    dplyr::select(-logit_posterior)
  tibble::as_tibble(df)
}

# String-normalize an input jurisdiction label.
.norm_jurisdiction <- function(x) {
  if (is.null(x) || length(x) == 0) return(character(0))
  y <- toupper(trimws(as.character(x)))
  y <- gsub("\\s+", " ", y)
  # "CAYMAN ISLANDS" vs "DELAWARE, UNITED STATES" -- split on comma, take leading segment
  # for fund-incorp strings the leading segment IS the jurisdiction.
  leading <- vapply(strsplit(y, ","), function(parts) trimws(parts[1]), character(1))
  # Apply aliases
  ifelse(leading %in% names(.jurisdiction_aliases),
         .jurisdiction_aliases[leading],
         leading)
}

# Detect US state embedded in e.g. "DELAWARE, UNITED STATES"
.detect_us_state <- function(x) {
  y <- toupper(trimws(as.character(x)))
  is_us <- grepl("UNITED STATES", y)
  state <- vapply(strsplit(y, ","), function(parts) trimws(parts[1]), character(1))
  secrecy_states <- c("DELAWARE", "NEVADA", "WYOMING", "SOUTH DAKOTA")
  ifelse(is_us & state %in% secrecy_states, state, NA_character_)
}

#' Flag fund jurisdictions
#'
#' Left-joins jurisdiction risk columns onto any tibble that contains a
#' jurisdiction-like location column. Matches the text of
#' \code{location_fund_incorporation} (or a column you name via
#' \code{location_col}) against the package's embedded risk dictionary
#' and appends columns described in the SHELDON KB
#' (\code{OffshoreFundJurisdictions/wiki/fundmanager_integration.md}).
#'
#' Normalization rules:
#' \itemize{
#'   \item Uppercased and whitespace-collapsed.
#'   \item Comma-split -- leading segment taken as the jurisdiction
#'         (so \code{"DELAWARE, UNITED STATES"} yields \code{DELAWARE}).
#'   \item Common aliases expanded (BVI, DPRK, UK, USA).
#'   \item When leading segment is a US state in the secrecy-state set
#'         (DE/NV/WY/SD), flagged as \code{is_us_secrecy_state = TRUE}.
#' }
#'
#' Unmatched jurisdictions get \code{flag_jurisdiction_risk = "CLEAR"}
#' and \code{rating_jurisdiction_risk = 10} (the prior), so the output
#' is never \code{NA} for downstream filtering.
#'
#' @param data a \code{data.frame} / \code{tibble}
#' @param location_col optional character; column name holding the
#'   jurisdiction string. If \code{NULL}, auto-detected from the first
#'   match in \code{c("location_fund_incorporation",
#'   "locationFundIncorporation", "name_jurisdiction",
#'   "country_office_primary", "countryOfficePrimary")}.
#' @param snake_case logical; if \code{TRUE} (default) output column
#'   names are snake_case; set \code{FALSE} for camelCase
#'   passthrough to match legacy callers.
#' @return tibble with appended risk columns
#' @export
#' @family jurisdiction risk
#' @examples
#' \dontrun{
#' library(dplyr)
#' funds <- adv_managers_filings(crd_ids = 156663, all_sections = FALSE,
#'                               section_names = "Private Fund Reporting",
#'                               assign_to_environment = TRUE, parallel = FALSE)
#' flagged <- flag_fund_jurisdictions(section7BPrivateFundReporting)
#' flagged %>% count(flag_jurisdiction_risk)
#' }
flag_fund_jurisdictions <- function(data, location_col = NULL, snake_case = TRUE) {
  if (is.null(data) || !is.data.frame(data) || nrow(data) == 0) return(data)
  candidate_cols <- c("location_fund_incorporation",
                      "locationFundIncorporation",
                      "name_jurisdiction",
                      "country_office_primary",
                      "countryOfficePrimary",
                      "locationFundIncorporation1")
  if (is.null(location_col)) {
    location_col <- intersect(candidate_cols, names(data))[1]
  }
  if (is.na(location_col) || !length(location_col) || !location_col %in% names(data)) {
    warning("flag_fund_jurisdictions(): no jurisdiction-like column found. Pass `location_col`.")
    return(data)
  }

  raw_values <- data[[location_col]]
  normalized <- .norm_jurisdiction(raw_values)
  us_state   <- .detect_us_state(raw_values)

  # Score the dictionary
  dict <- dictionary_jurisdiction_risk()

  # Build per-row lookup. For rows whose leading segment is a US secrecy state,
  # prefer the state row (DELAWARE) over UNITED STATES.
  lookup_key <- ifelse(!is.na(us_state), us_state, normalized)

  joined <- tibble::tibble(
    .__row__ = seq_along(lookup_key),
    .__key__ = lookup_key
  ) %>%
    dplyr::left_join(dict, by = c(".__key__" = "name_jurisdiction")) %>%
    dplyr::arrange(.data$.__row__) %>%
    dplyr::select(-`.__row__`, -`.__key__`)

  # Fill unmatched rows with CLEAR + prior rating.
  joined <- joined %>%
    dplyr::mutate(
      is_offshore = is_traditional_tax_haven | is_fatf_blacklist | is_fatf_greylist |
                    is_eu_blacklist_annex_1 | is_eu_blacklist_annex_2,
      rating_jurisdiction_risk = dplyr::coalesce(rating_jurisdiction_risk, 10L),
      flag_jurisdiction_risk   = dplyr::coalesce(flag_jurisdiction_risk, "CLEAR"),
      source_jurisdiction_risk = dplyr::coalesce(source_jurisdiction_risk, "unmatched-or-onshore"),
      is_traditional_tax_haven = dplyr::coalesce(is_traditional_tax_haven, FALSE),
      is_fatf_blacklist        = dplyr::coalesce(is_fatf_blacklist, FALSE),
      is_fatf_greylist         = dplyr::coalesce(is_fatf_greylist, FALSE),
      is_eu_blacklist_annex_1  = dplyr::coalesce(is_eu_blacklist_annex_1, FALSE),
      is_eu_blacklist_annex_2  = dplyr::coalesce(is_eu_blacklist_annex_2, FALSE),
      is_fsi_top_10            = dplyr::coalesce(is_fsi_top_10, FALSE),
      is_fsi_top_20            = dplyr::coalesce(is_fsi_top_20, FALSE),
      is_us_secrecy_state      = dplyr::coalesce(is_us_secrecy_state, FALSE),
      has_recent_fatf_scrutiny = dplyr::coalesce(has_recent_fatf_scrutiny, FALSE),
      is_offshore              = dplyr::coalesce(is_offshore, FALSE)
    )

  out <- dplyr::bind_cols(data, joined)

  if (isFALSE(snake_case)) {
    # restore camelCase for legacy callers
    rn <- c(
      name_jurisdiction_iso      = "nameJurisdictionISO",
      is_traditional_tax_haven   = "isTraditionalTaxHaven",
      is_offshore                = "isOffshore",
      is_fatf_blacklist          = "isFATFBlacklist",
      is_fatf_greylist           = "isFATFGreylist",
      is_eu_blacklist_annex_1    = "isEUBlacklistAnnexI",
      is_eu_blacklist_annex_2    = "isEUBlacklistAnnexII",
      is_fsi_top_10              = "isFSITop10",
      is_fsi_top_20              = "isFSITop20",
      is_us_secrecy_state        = "isUSSecrecyState",
      has_recent_fatf_scrutiny   = "hasRecentFATFScrutiny",
      rating_jurisdiction_risk   = "ratingJurisdictionRisk",
      flag_jurisdiction_risk     = "flagJurisdictionRisk",
      source_jurisdiction_risk   = "sourceJurisdictionRisk",
      note_jurisdiction          = "noteJurisdiction",
      date_source_updated        = "dateSourceUpdated"
    )
    for (k in names(rn)) {
      if (k %in% names(out)) names(out)[names(out) == k] <- rn[[k]]
    }
  }
  tibble::as_tibble(out)
}
