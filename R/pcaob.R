#
# https://rasr.pcaobus.org/Search/Search.aspx

#' Firms Denying PCAOB Audits
#'
#' @return
#' @export
#'
#' @examples
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
#' @return
#' @export
#'
#' @examples
pcaob_auditors <-
  function(include_denied_firms = T) {
    data <-
      "https://pcaobus.org/RUSDocuments/FirmFilings.zip" %>% rio::import()
    data <- data %>% janitor::clean_names() %>% as_tibble()

    data <- data %>%
      mutate_if(is.character, list(function(x) {
        case_when(x == "" ~ NA_character_,
                  TRUE ~ x)
      }))

    data <- data %>%
      rename(
        id_firm = firm_id,
        id_firm_filing = form_filing_id,
        datetime_audit = audit_report_date,
        datetime_fiscal_period_end = fiscal_period_end_date,
        is_dual_dated = dual_dated,
        datestime_dual_audit = audit_dual_date,
        datetime_signed = signed_date,
        datetime_filing = filing_date,
        id_ticker = issuer_ticker,
        id_cik = issuer_cik,
        name_issuer = issuer_name,
        name_audit_firm = firm_name,
        name_audit_form_other = firm_other_name,
        country_audit_country = firm_country,
        country_issuer = firm_issuing_country,
        state_issuer = firm_issuing_state,
        city_issuer = firm_issuing_city,
        id_issuer = issuer_id,
        type_audit = audit_report_type,
        telephone_signartory = signed_phone_number
      ) %>%
      mutate_if(is.character,
                list(function(x) {
                  case_when(is.na(x) ~ "",
                            TRUE ~ x)
                })) %>%
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
      mutate_if(is.character, list(function(x) {
        case_when(x == "" ~ NA_character_,
                  TRUE ~ x)
      }))

    data <- data %>%
      mutate(datestime_dual_audit = datestime_dual_audit %>% str_replace_all("#^#", " | "))

    dates <- data %>% select(matches('datetime')) %>% names()

    data <-
      data %>%
      mutate_at(dates, lubridate::mdy_hms)

    data <-
      data %>%
      munge_tbl(
        data = data,
        snake_names = T,
        unformat = T,
        include_address = F,
        convert_case = T
      )

    if (include_denied_firms ) {
      tbl_denied <- pcaob_denied_firms()
      data %>%
        left_join(
          tbl_denied %>% select(name_issuer, is_pcaob_denied), by = "name_issuer"
        )
    }

    data
  }
