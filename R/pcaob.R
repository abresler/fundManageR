#
# https://rasr.pcaobus.org/Search/Search.aspx

#' Firms Denying PCAOB Audits
#'
#' @return a \code{tibble} with firms that have denied PCAOB audit access
#' @export
#'
#' @examples
#' \dontrun{
#' pcaob_denied_firms()
#' }
pcaob_denied_firms <-
  memoise::memoise(function() {
    page <- read_html("https://pcaobus.org/International/Inspections/Pages/IssuerClientsWithoutAccess.aspx")
    tables <- page %>% html_table(fill = T)
    data <- tables[[length(tables)]] %>% as_tibble()
    data <- data %>% set_names(c("name_issuer","name_audit_firm", "country_audit_firm")) %>%
      mutate(is_pcaob_denied = T)

    data %>%
      munge_tbl()
  })


#' PCAOB Audit Relationships
#'
#' @param include_denied_firms adds PCAOB denied firm feature
#'
#' @return a \code{tibble} with PCAOB auditor relationships and firm filings
#' @export
#'
#' @examples
#' \dontrun{
#' pcaob_auditors(include_denied_firms = TRUE)
#' }
pcaob_auditors <-
  function(include_denied_firms = T) {
    data <-
      "https://pcaobus.org/RUSDocuments/FirmFilings.zip" %>% .import_url_curl()
    data <- data %>% janitor::clean_names() %>% as_tibble()

    data <- data %>%
      mutate(across(where(is.character), ~ case_when(.x == "" ~ NA_character_,
                  TRUE ~ .x)))

    # Use any_of() for columns that may or may not exist in current data
    rename_map <- c(
      id_firm = "firm_id",
      id_firm_filing = "form_filing_id",
      datetime_audit = "audit_report_date",
      datetime_fiscal_period_end = "fiscal_period_end_date",
      is_dual_dated = "dual_dated",
      datestime_dual_audit = "audit_dual_date",
      datetime_signed = "signed_date",
      datetime_filing = "filing_date",
      id_ticker = "issuer_ticker",
      is_ticker_not_available = "issuer_ticker_not_available",
      id_cik = "issuer_cik",
      is_cik_none = "issuer_cik_none",
      name_issuer = "issuer_name",
      name_audit_firm = "firm_name",
      name_audit_form_other = "firm_other_name",
      country_audit_firm = "firm_country",
      country_issuer = "firm_issuing_country",
      state_issuer = "firm_issuing_state",
      city_issuer = "firm_issuing_city",
      id_issuer = "issuer_id",
      type_audit = "audit_report_type",
      telephone_signatory = "signed_phone_number"
    )

    # Only rename columns that exist
    existing_cols <- rename_map[rename_map %in% names(data)]
    data <- data %>% rename(any_of(existing_cols))

    data <- data %>%
      mutate(across(where(is.character), ~ case_when(is.na(.x) ~ "",
                            TRUE ~ .x))) %>%
      mutate(
        name_issuer_signatory = glue("{signed_first_name} {signed_last_name}") %>% str_squish(),
        name_engagment_partner_primary = glue(
          "{engagement_partner_first_name} {engagement_partner_last_name}"
        ) %>% str_squish(),
        name_engagment_partner_secondary = glue(
          "{secondary_engagement_partner_first_name} {secondary_engagement_partner_last_name}"
        ) %>% str_squish(),
        location_issuer = glue("{city_issuer} {state_issuer}, {country_issuer}") %>% str_squish()
      )

    data <- data %>%
      mutate(across(where(is.character), ~ case_when(.x == "" ~ NA_character_,
                  TRUE ~ .x)))

    # Handle datestime_dual_audit if it exists
    if ("datestime_dual_audit" %in% names(data)) {
      data <- data %>%
        mutate(datestime_dual_audit = datestime_dual_audit %>% str_replace_all("#^#", " | "))
    }

    dates <- data %>% select(matches('datetime')) %>% names()

    if (length(dates) > 0) {
      data <-
        data %>%
        mutate(across(all_of(dates), ~lubridate::mdy_hms(., quiet = TRUE)))
    }

    data <-
      data %>%
      munge_tbl(
        data = data,
        snake_names = T,
        unformat = T,
        include_address = F,
        convert_case = T
      )

    if (include_denied_firms) {
      tbl_denied <- tryCatch(pcaob_denied_firms(), error = function(e) tibble())
      if (nrow(tbl_denied) > 0 && "name_issuer" %in% names(data)) {
        data <- data %>%
          left_join(
            tbl_denied %>% select(name_issuer, is_pcaob_denied), by = "name_issuer"
          )
      }
    }

    data
  }
