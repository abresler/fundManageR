# parsing -----------------------------------------------------------------
#
# SEC IAPD API CHANGES (2024-2025)
# ================================
# The SEC completely rebuilt the Investment Adviser Public Disclosure (IAPD)
# website from ASP.NET to an Angular Single Page Application. As a result:
#
# 1. HTML scraping no longer works (old pages removed)
# 2. New REST API available at: https://api.adviserinfo.sec.gov/
# 3. The API returns JSON with limited data fields compared to the old HTML pages
#
# AVAILABLE SECTIONS (via new API):
# - Registration (basic firm info, SEC number, filing dates)
# - Identifying Information (address data)
# - SEC Reporting (registration status by jurisdiction)
# - Notice Filings (state notice filing status)
# - Brochures (Part 2 brochure details)
# - Accountant Surprise Exams
# - Other Names (DBA names)
# - Client Relationship Summary (CRS)
# - Organization Scope (SEC/State registration flags)
# - Relying Advisors (umbrella registrations)
# - Compilation Data (report metadata)
# - Exempt Reporting Advisers
#
# NO LONGER AVAILABLE (from old HTML scraping):
# - Detailed Form ADV Item responses (Items 1-12)
# - Schedule A (direct owners)
# - Schedule B (indirect owners)
# - Schedule D (additional details)
# - Private Fund Reporting details
# - Direct/Indirect Manager Owners
# - Manager Signatories
#
# For detailed Form ADV data, users should:
# - Download the full ADV PDF from: adviserinfo.sec.gov/reports/ADV/{CRD}/PDF/{CRD}.pdf
# - Use SEC EDGAR bulk data files
# - Access the IAPD website directly
#

# Modernization helpers ---------------------------------------------------------

#' Parallel map helper with user control
#' @param .x Input to iterate over
#' @param .f Function to apply
#' @param ... Additional arguments passed to .f
#' @param parallel Logical, if TRUE uses furrr::future_map_dfr, else purrr::map_dfr
#' @noRd
.parallel_map_dfr <- function(.x, .f, ..., parallel = TRUE) {
  if (parallel) {
    furrr::future_map_dfr(.x, .f, ...)
  } else {
    purrr::map_dfr(.x, .f, ...)
  }
}

#' Convert ADV column types based on naming conventions
#' @param data A data frame with ADV data
#' @return Data frame with properly typed columns
#' @noRd
.convert_adv_column_types <- function(data) {
  # Handle date columns
  date_cols <- names(data)[str_detect(names(data), "^date")]
  if (length(date_cols) > 0) {
    data <- data %>%
      mutate(across(all_of(date_cols), ~lubridate::ymd(.)))
  }

  # Handle count and amount columns (numeric with formatting)
  count_cols <- names(data)[str_detect(names(data), "^count[A-Z]|^amount[A-Z]") &
                              !str_detect(names(data), "country")]
  if (length(count_cols) > 0) {
    data <- data %>%
      mutate(across(all_of(count_cols), ~as.character(.) %>%
                      parse_number() %>%
                      formattable::comma(digits = 0)))
  }

  # Handle logical columns
  logical_cols <- names(data)[str_detect(names(data), "^is[A-Z]|^has[A-Z]")]
  if (length(logical_cols) > 0) {
    data <- data %>%
      mutate(across(all_of(logical_cols), as.logical))
  }

  # Handle amount columns (currency formatting)
  amount_cols <- names(data)[str_detect(names(data), "^amount[A-Z]")]
  if (length(amount_cols) > 0) {
    data <- data %>%
      mutate(across(all_of(amount_cols), ~as.numeric(.) %>%
                      formattable::currency(digits = 0)))
  }

  # Handle percentage columns
  pct_cols <- names(data)[str_detect(names(data), "^pct[A-Z]|^percent")]
  if (length(pct_cols) > 0) {
    data <- data %>%
      mutate(across(all_of(pct_cols), ~as.numeric(.) %>%
                      formattable::percent(digits = 3)))
  }

  # Handle ID columns
  id_cols <- names(data)[str_detect(names(data), "^idCRD|idCIK")]
  if (length(id_cols) > 0) {
    data <- data %>%
      mutate(across(all_of(id_cols), as.numeric))
  }

  # Handle text columns (uppercase)
  text_cols <- names(data)[str_detect(names(data), "^name|^city|^state|^country|^address") &
                             sapply(data, is.character)]
  if (length(text_cols) > 0) {
    data <- data %>%
      mutate(across(all_of(text_cols), str_to_upper))
  }

  return(data)
}

# New SEC IAPD API function (SEC rebuilt their website in 2024-2025)
.get_iapd_api_data <-
  function(id_crd = 105378) {
    url <- glue::glue("https://api.adviserinfo.sec.gov/search/firm/{id_crd}?hl=true&nrows=12&r=25&sort=score+desc&wt=json")

    response <- tryCatch({
      httr::GET(
        url,
        httr::add_headers(
          "Accept" = "application/json",
          "Origin" = "https://adviserinfo.sec.gov",
          "Referer" = "https://adviserinfo.sec.gov/",
          "User-Agent" = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36"
        )
      )
    }, error = function(e) {
      return(NULL)
    })

    if (is.null(response) || httr::status_code(response) != 200) {
      return(NULL)
    }

    json_data <- httr::content(response, as = "text", encoding = "UTF-8") %>%
      jsonlite::fromJSON(flatten = FALSE)

    hits_data <- json_data$hits$hits

    # Check for empty results
    if (is.null(hits_data)) {
      return(NULL)
    }

    # Handle both data.frame and list structures from jsonlite
    if (is.data.frame(hits_data)) {
      if (nrow(hits_data) == 0) {
        return(NULL)
      }
      # For data frame, access _source column from first row
      source_data <- hits_data$`_source`[1, , drop = FALSE]
      # If _source is a nested data frame, convert to list
      if (is.data.frame(source_data)) {
        source_data <- as.list(source_data)
      }
      # Get iacontent from the nested structure
      iacontent_raw <- hits_data$`_source`$iacontent[1]
    } else if (is.list(hits_data) && length(hits_data) > 0) {
      source_data <- hits_data[[1]]$`_source`
      iacontent_raw <- source_data$iacontent
    } else {
      return(NULL)
    }

    # Parse the nested iacontent JSON string
    if (!is.null(iacontent_raw) && nchar(iacontent_raw) > 0) {
      iacontent <- tryCatch({
        jsonlite::fromJSON(iacontent_raw, flatten = FALSE)
      }, error = function(e) NULL)
      source_data$iacontent_parsed <- iacontent
    }

    return(source_data)
  }

# Get basic manager info from new API
.get_manager_info_from_api <-
  function(id_crd) {
    api_data <- .get_iapd_api_data(id_crd)

    if (is.null(api_data)) {
      return(tibble())
    }

    basic_info <- api_data$iacontent_parsed$basicInformation

    tibble(
      idCRD = as.numeric(basic_info$firmId %||% id_crd),
      nameEntityManager = basic_info$firmName %||% NA_character_,
      idSEC = basic_info$iaSECNumber %||% NA_character_,
      typeIDSEC = basic_info$iaSECNumberType %||% NA_character_,
      statusIA = basic_info$iaScope %||% NA_character_,
      dateADVFiling = basic_info$advFilingDate %||% NA_character_,
      hasPDF = basic_info$hasPdf == "Y"
    )
  }

# Parse all available sections from the new SEC IAPD API
.parse_api_sections_data <-
  function(id_crd, api_data = NULL) {
    if (is.null(api_data)) {
      api_data <- .get_iapd_api_data(id_crd)
    }

    if (is.null(api_data) || is.null(api_data$iacontent_parsed)) {
      return(list())
    }

    parsed <- api_data$iacontent_parsed
    basic_info <- parsed$basicInformation
    nameEntityManager <- basic_info$firmName %||% NA_character_

    results <- list()

    # Registration / Basic Info
    results$dataRegistration <- tibble(
      idCRD = as.numeric(basic_info$firmId %||% id_crd),
      nameEntityManager = nameEntityManager,
      idSEC = basic_info$iaSECNumber %||% NA_character_,
      idSECFull = paste0(basic_info$iaSECNumberType %||% "", "-", basic_info$iaSECNumber %||% "") %>%
        str_remove("^-$"),
      statusIA = basic_info$iaScope %||% NA_character_,
      dateADVFiling = basic_info$advFilingDate %||% NA_character_,
      hasPDF = (basic_info$hasPdf %||% "N") == "Y",
      urlPDF = glue::glue("https://adviserinfo.sec.gov/reports/ADV/{id_crd}/PDF/{id_crd}.pdf") %>% as.character()
    )

    # Other Names
    if (!is.null(basic_info$otherNames) && length(basic_info$otherNames) > 0) {
      results$dataOtherNames <- tibble(
        idCRD = as.numeric(id_crd),
        nameEntityManager = nameEntityManager,
        nameOther = basic_info$otherNames
      )
    }

    # Address Information
    if (!is.null(parsed$iaFirmAddressDetails)) {
      addr <- parsed$iaFirmAddressDetails$officeAddress
      if (!is.null(addr)) {
        results$dataAddress <- tibble(
          idCRD = as.numeric(id_crd),
          nameEntityManager = nameEntityManager,
          addressStreet1 = addr$street1 %||% NA_character_,
          addressStreet2 = addr$street2 %||% NA_character_,
          addressCity = addr$city %||% NA_character_,
          addressState = addr$state %||% NA_character_,
          addressCountry = addr$country %||% NA_character_,
          addressPostalCode = addr$postalCode %||% NA_character_
        )
      }
    }

    # Registration Status
    if (!is.null(parsed$registrationStatus) && length(parsed$registrationStatus) > 0) {
      reg_status <- parsed$registrationStatus
      if (is.data.frame(reg_status)) {
        results$dataRegistrationStatus <- reg_status %>%
          as_tibble() %>%
          mutate(
            idCRD = as.numeric(id_crd),
            nameEntityManager = nameEntityManager
          ) %>%
          rename(
            nameJurisdiction = secJurisdiction,
            statusRegistration = status,
            dateEffective = effectiveDate
          ) %>%
          dplyr::select(idCRD, nameEntityManager, everything())
      }
    }

    # Notice Filings
    if (!is.null(parsed$noticeFilings) && length(parsed$noticeFilings) > 0) {
      notice <- parsed$noticeFilings
      if (is.data.frame(notice)) {
        results$dataNoticeFilings <- notice %>%
          as_tibble() %>%
          mutate(
            idCRD = as.numeric(id_crd),
            nameEntityManager = nameEntityManager
          ) %>%
          rename(
            nameJurisdiction = jurisdiction,
            statusNotice = status,
            dateEffective = effectiveDate
          ) %>%
          dplyr::select(idCRD, nameEntityManager, everything())
      }
    }

    # Organization Scope Status Flags
    if (!is.null(parsed$orgScopeStatusFlags)) {
      flags <- parsed$orgScopeStatusFlags
      results$dataOrgScopeFlags <- tibble(
        idCRD = as.numeric(id_crd),
        nameEntityManager = nameEntityManager,
        isSECRegistered = (flags$isSECRegistered %||% "N") == "Y",
        isStateRegistered = (flags$isStateRegistered %||% "N") == "Y",
        isERARegistered = (flags$isERARegistered %||% "N") == "Y",
        isSECERARegistered = (flags$isSECERARegistered %||% "N") == "Y",
        isStateERARegistered = (flags$isStateERARegistered %||% "N") == "Y"
      )
    }

    # Accountant Surprise Exams
    if (!is.null(parsed$accountantSurpriseExams) && length(parsed$accountantSurpriseExams) > 0) {
      exams <- parsed$accountantSurpriseExams
      if (is.data.frame(exams)) {
        results$dataAccountantExams <- exams %>%
          as_tibble() %>%
          mutate(
            idCRD = as.numeric(id_crd),
            nameEntityManager = nameEntityManager
          ) %>%
          rename(
            nameAccountantFirm = accountantFirmName,
            dateFilingSurpriseExam = filingDate,
            statusFile = fileStatus
          ) %>%
          dplyr::select(idCRD, nameEntityManager, everything())
      }
    }

    # Brochures
    if (!is.null(parsed$brochures)) {
      brochures <- parsed$brochures
      results$dataBrochures <- tibble(
        idCRD = as.numeric(id_crd),
        nameEntityManager = nameEntityManager,
        isExemptPart2 = (brochures$part2ExemptFlag %||% "N") == "Y"
      )

      if (!is.null(brochures$brochuredetails) && length(brochures$brochuredetails) > 0) {
        broch_details <- brochures$brochuredetails
        if (is.data.frame(broch_details)) {
          results$dataBrochureDetails <- broch_details %>%
            as_tibble() %>%
            mutate(
              idCRD = as.numeric(id_crd),
              nameEntityManager = nameEntityManager
            ) %>%
            rename(
              idBrochureVersion = brochureVersionID,
              nameBrochure = brochureName,
              dateSubmitted = dateSubmitted
            ) %>%
            dplyr::select(idCRD, nameEntityManager, everything())
        }
      }
    }

    # CRS Info
    if (!is.null(basic_info$crs)) {
      crs <- basic_info$crs
      results$dataCRS <- tibble(
        idCRD = as.numeric(id_crd),
        nameEntityManager = nameEntityManager,
        typeCRS = crs$crsType %||% NA_character_,
        idFileCRS = crs$fileId %||% NA_character_,
        urlCRS = glue::glue("https://adviserinfo.sec.gov/crs/crs_{id_crd}.pdf") %>% as.character()
      )
    }

    # Relying Advisors (umbrella registration)
    if (!is.null(parsed$relyingAdvisors) && length(parsed$relyingAdvisors) > 0) {
      relying <- parsed$relyingAdvisors
      if (is.data.frame(relying)) {
        results$dataRelyingAdvisors <- relying %>%
          as_tibble() %>%
          mutate(
            idCRD = as.numeric(id_crd),
            nameEntityManager = nameEntityManager
          ) %>%
          rename_with(~ case_when(
            . == "firmId" ~ "idCRDRelying",
            . == "name" ~ "nameRelyingAdvisor",
            . == "status" ~ "statusRelyingAdvisor",
            . == "nameStatus" ~ "statusRelyingAdvisorName",
            TRUE ~ .
          )) %>%
          dplyr::select(idCRD, nameEntityManager, everything())
      }
    }

    # Compilation Data (report generation info)
    if (!is.null(parsed$compilationData) && length(parsed$compilationData) > 0) {
      comp <- parsed$compilationData
      if (is.data.frame(comp)) {
        results$dataCompilation <- comp %>%
          as_tibble() %>%
          mutate(
            idCRD = as.numeric(id_crd),
            nameEntityManager = nameEntityManager
          ) %>%
          rename_with(~ case_when(
            . == "editionID" ~ "idEdition",
            . == "type" ~ "typeCompilation",
            . == "generatedOn" ~ "dateGenerated",
            TRUE ~ .
          )) %>%
          dplyr::select(idCRD, nameEntityManager, everything())
      }
    }

    # Exempt Reporting Advisers
    if (!is.null(parsed$exemptReportingAdvisers) && length(parsed$exemptReportingAdvisers) > 0) {
      era <- parsed$exemptReportingAdvisers
      if (is.data.frame(era) && nrow(era) > 0) {
        results$dataExemptReportingAdvisers <- era %>%
          as_tibble() %>%
          mutate(
            idCRD = as.numeric(id_crd),
            nameEntityManager = nameEntityManager
          ) %>%
          dplyr::select(idCRD, nameEntityManager, everything())
      }
    }

    return(results)
  }

# Wrapper function for API-based section retrieval
.get_api_section_data <-
  function(url, id_crd = NULL) {
    # Extract CRD from URL if not provided
    if (is.null(id_crd)) {
      id_crd <- url %>%
        str_extract("[0-9]+") %>%
        as.numeric()
    }

    if (is.na(id_crd)) {
      return(tibble())
    }

    # Get all API data
    api_data <- .get_iapd_api_data(id_crd)

    if (is.null(api_data)) {
      return(tibble())
    }

    # Parse all sections
    sections <- .parse_api_sections_data(id_crd, api_data)

    # Return registration data as default (most commonly needed)
    if (!is.null(sections$dataRegistration)) {
      return(sections$dataRegistration)
    }

    return(tibble())
  }

.get_html_page <-
  function(url = 'https://files.adviserinfo.sec.gov/IAPD/IAPDFirmSummary.aspx?ORG_PK=160080') {
    page <-
      url %>% curl::curl() %>% xml2::read_html()

    page
  }


.check_html_node <-
  function(page, node_css = '#ctl00_cphMain_landing_p2BrochureLink') {
    if (page %>% html_nodes(css = node_css) %>% length() > 0) {
      node_exists <-
        T
    } else {
      node_exists <-
        F
    }
    node_exists
  }


.get_html_node_text <-
  function(page, node_css = '#ctl00_cphMain_landing_lblActiveOrgName') {
    parse_html_for_text <-
      function(page, node_css = '#ctl00_cphMain_landing_lblActiveOrgName') {
        node_text <-
          page %>%
          html_nodes(css = node_css) %>%
          html_text()

        return(node_text)
      }

    node_exists <-
      page %>%
      .check_html_node(node_css = node_css)

    if (node_exists) {
      node_text <-
        page %>% parse_html_for_text(node_css = node_css)
    } else {
      node_text <-
        NA
    }
    node_text
  }


.get_entity_manager_name <-
  function(page) {
    manager_name <-
      page %>%
      .get_html_node_text(node_css = '#ctl00_ctl00_cphMainContent_ucADVHeader_lblPrimaryBusinessName')

    if (manager_name %>% is.na()) {
      manager_name <-
        page %>%
        html_nodes('.summary-displayname') %>%
        html_text()
    }

    return(manager_name)
  }

.get_html_node_attributes <-
  function(page,
           node_css = '#ctl00_cphMain_landing_p2BrochureLink',
           html_attribute = 'href',
           base_url = NULL) {
    parse_for_html_attribute <-
      function(page,
               node_css = '#ctl00_cphMain_landing_p2BrochureLink',
               html_attribute = 'href',
               base_url = NULL) {
        node_attribute <-
          page %>%
          html_nodes(css = node_css) %>%
          html_attr(name = html_attribute)

        if (!base_url %>% purrr::is_null()) {
          node_attribute <-
            base_url %>%
            paste0(node_attribute)
        }
        return(node_attribute)

      }

    node_exists <-
      page %>%
      .check_html_node(node_css = node_css)

    if (node_exists) {
      node_attribute <-
        page %>%
        parse_for_html_attribute(
          node_css = node_css,
          html_attribute = html_attribute,
          base_url = base_url
        )
    } else {
      node_attribute <-
        NA
    }

    return(node_attribute)
  }


.parse_node_table_to_text <-
  function(page, css_node = '#ctl00_ctl00_cphMainContent_cphAdvFormContent_ClientCompensation_ctl00_trIAPDHeader + tr + tr') {
    node_text <-
      page %>%
      .get_html_node_text(node_css = css_node) %>%
      str_replace_all('\r|\n|\t', '') %>%
      stri_replace_all_charclass("\\p{WHITE_SPACE}", " ") %>%
      stri_trim_both() %>%
      gsub("^\\s+|\\s+$", "", .) %>%
      str_split("   +") %>%
      flatten_chr()

    node_text <-
      node_text[!node_text == '']
    return(node_text)
  }


.parse_finra_c_url <-
  function(finra_c_url = "curl 'http://brokercheck.finra.org/Search/GenericSearch' -H 'Pragma: no-cache' -H 'Origin: http://brokercheck.finra.org' -H 'Accept-Encoding: gzip, deflate' -H 'Accept-Language: en-US,en;q=0.8' -H 'Upgrade-Insecure-Requests: 1' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/53.0.2785.80 Safari/537.36' -H 'Content-Type: application/x-www-form-urlencoded' -H 'Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8' -H 'Cache-Control: no-cache' -H 'Referer: http://brokercheck.finra.org/Search/GenericSearch' -H 'Cookie: __RequestVerificationToken=rSB02up7WU1YiLtAmLvoqbrC4heA7LXKidX0GkSiEjprktiE0qFDfkGKbQkTQVd94ShQynDI5BsmpIJeB4idW39G2QE6hRtgA9M_ejnQoHc1; ASP.NET_SessionId=kjzpjdpm1p0ltmm1xcuafmoz' -H 'Connection: keep-alive' -H 'DNT: 1' --data '__RequestVerificationToken=1jsNCQ-1KyYFIfKZuaJc_-CFqVaBiSi1MQ_WyvUrMKk9bEh1ux4s_te11rbeAEHH23ncmfbN8Ndf9gIQvYb0gGEuSabJ1PKIhKi0_AJac9Q1&GenericSearch.StartRow=1&GenericSearch.PageSize=15&GenericSearch.System=BC&GenericSearch.SearchType=2&GenericSearch.IndividualSearchText=&GenericSearch.EmploymingFirmSearchText=&GenericSearch.FirmSearchText=EJF&GenericSearch.Within=25&GenericSearch.ZipCode=' --compressed") {
    clean_url <-
      finra_c_url %>%
      curlconverter::straighten() %>%
      suppressMessages()

    res <-
      clean_url %>%
      curlconverter::make_req()

    html_page <-
      res[[1]]() %>%
      content(as = "parsed")

    return(html_page)
  }

.find_text_node <-
  function(node_text,
           hit_words = "Total Number of Clients",
           off_set = 4,
           is_numeric_node = T) {
    if (hit_words %>% length() > 1) {
      hit_words <-
        hit_words %>% str_c(collapse = '|')
    }

    node_exists <-
      (node_text %>% grep(hit_words, .)) %>% length() > 0

    if (!node_exists) {
      stop("Sorry " %>%
             paste0(hit_words, ' is not found in the text nodes'))
    }
    node_location <-
      (node_text %>% grep(hit_words, .)) %>% +off_set

    text_node <-
      node_text[node_location] %>%
      str_trim()

    if (is_numeric_node) {
      text_node <-
        text_node %>%
        str_replace_all('\\$', '') %>% str_trim() %>%
        as.character() %>%
        readr::parse_number()

      text_node <-
        text_node[!text_node %>% is.na]
    }
    return(text_node)
  }

# munging -----------------------------------------------------------------

.munge_fund_names <-
  function(data) {
    funds <-
      data %>%
      select(nameFund) %>%
      mutate(across(where(is.character), ~str_trim(.) %>% stringr::str_to_upper())) %>%
      suppressWarnings()

    funds <-
      funds %>%
      mutate(nameFundClean = nameFund) %>%
      separate(nameFundClean,
               sep = "\\(",
               into = c("nameFundClean", "detailFund")) %>%
      mutate(
        detailFund = detailFund %>% str_replace_all("\\)", ""),
        nameFundClean = nameFundClean %>% str_replace_all("\\ L.P.|\\ L.P", " LP") %>% str_replace_all("\\--", " ") %>%
          str_replace_all("\\G.P.|G.P", "GP") %>% str_replace_all("\\L.L.C.|L.L.C", "LLC"),
        typeFundEntity = case_when(
          nameFundClean %>% str_detect("LLC") ~ "LLC",
          nameFundClean %>% str_detect("GP") ~ "GP",
          nameFundClean %>% str_detect("\\LP") ~ "LP",
          TRUE                      ~  NA_character_
        ),
        isCoInvestmentVehicle = nameFundClean %>% str_detect("COINVESTMENT|CO-INVESTMENT|SIDECAR|SIDE-CAR|\\COINVEST"),
        isParalellFund = nameFundClean %>% str_detect("PARALLEL"),
        nameFundClean =
          nameFundClean %>% str_replace_all(
            "\\ LLP|\\ LLC|\\ LP|\\ GP|\\,|CO-INVESTMENT|COINVESTMENT|\\SIDECAR|\\SIDE-CAR|PARALLEL",
            ""
          )
      ) %>%
      mutate(nameFundClean = nameFundClean %>% str_trim() %>% str_replace_all("  ", " ")) %>%
      select(
        typeFundEntity,
        nameFund,
        nameFundClean,
        detailFund,
        isCoInvestmentVehicle,
        isParalellFund
      ) %>%
      suppressWarnings()

    has_dash <-
      funds$nameFundClean %>% str_detect('\\-[A-H]') %>% sum(na.rm = TRUE) >
      0

    if (has_dash) {
      funds$nameFundClean[funds$nameFundClean %>% str_detect('\\-[A-H]')] <-
        funds$nameFundClean[funds$nameFundClean %>% str_detect('\\-[A-H]')] %>% str_replace_all('\\-', " SERIES ")
    }

    count_series <-
      funds$nameFundClean %>% str_count('\\SERIES') %>% sum(na.rm = T)

    if (count_series > 0) {
      funds <-
        funds %>%
        separate(nameFundClean,
                 into = c("nameFundClean", "idSeries"),
                 sep = "\\ SERIES ") %>%
        suppressWarnings()
    }
    new_names <-
      names(funds)[!names(funds) %>% str_detect("nameFund")]
    data %>%
      left_join(funds) %>%
      select(idCRD:nameFund,
             nameFundClean,
             any_of(new_names),
             everything()) %>%
      suppressMessages()

  }

.select_start_vars <-
  function(data) {
    data <-
      data %>%
      dplyr::select(idCRD, nameEntityManager, everything())
    return(data)
  }


.mutate_adv_data <-
  function(data) {
    # Handle date columns
    if (any(str_detect(names(data), "^date"))) {
      data <- data %>%
        mutate(across(matches("^date"), ~lubridate::ymd(.)))
    }

    # Handle count and amount columns
    count_regex <- "^count[A-Z]|^amount[A-Z]|idCRD"
    count_cols <- names(data)[str_detect(names(data), count_regex) &
                                !str_detect(names(data), "country")]
    if (length(count_cols) > 0) {
      data <- data %>%
        mutate(across(all_of(count_cols),
                      ~as.character(.) %>% parse_number() %>% formattable::comma(digits = 0)))
    }

    # Handle logical columns
    if (any(str_detect(names(data), "^is[A-Z]|^has[A-Z]"))) {
      data <- data %>%
        mutate(across(matches("^is[A-Z]|^has[A-Z]"), as.logical))
    }

    # Handle amount columns (currency)
    if (any(str_detect(names(data), "^amount[A-Z]"))) {
      data <- data %>%
        mutate(across(matches("^amount"),
                      ~as.character(.) %>% parse_number() %>% formattable::currency(digits = 0)))
    }

    # Handle percentage columns
    if (any(str_detect(names(data), "^pct[A-Z]"))) {
      data <- data %>%
        mutate(across(matches("^pct"),
                      ~as.numeric(.) %>% formattable::percent(digits = 3)))
    }

    return(data)
  }


.widen_adv_data <-
  function(data) {
    data <-
      data %>%
      mutate(across(matches("^date"), as.character)) %>%
      pivot_longer(cols = -c(countItem, nameEntityManager),
                   names_to = "nameItem",
                   values_to = "value") %>%
      suppressWarnings()

    data <-
      data %>%
      mutate(
        countItem = countItem - 1,
        countItem = countItem %>% as.character(),
        countItem = ifelse(countItem == "0", '', countItem)
      ) %>%
      unite(item, nameItem, countItem, sep = '') %>%
      distinct() %>%
      suppressWarnings()

    col_order <-
      c('nameEntityManager', data$item)

    data <-
      data %>%
      pivot_wider(names_from = item, values_from = value) %>%
      dplyr::select(any_of(col_order))

    data <-
      data %>%
      .mutate_adv_data() %>%
      suppressWarnings()

    return(data)
  }


.get_item_name_yes_no_df <-
  function(item_name = 'hasCustodyClientCash') {
    item_name_df <-
      seq_along(item_name) %>%
      future_map_dfr(function(x) {
        tibble(nameItem = rep(item_name[x], 2),
                   valueItem = c(T, F)) %>%
          unite(fullnameItem,
                nameItem,
                valueItem,
                sep = '.',
                remove = F)
      })
    return(item_name_df)
  }

.has_item_check_name <-
  function(item_name = 'hasQuarterlyStatemnt') {
    item_name_df <-
      seq_along(item_name) %>%
      future_map_dfr(function(x) {
        tibble(nameItem = rep(item_name[x], 1),
                   valueItem = T) %>%
          mutate(fullnameItem = nameItem)
      })
    return(item_name_df)
  }




# api ---------------------------------------------------------------------

.parse_crd_json <- function(url = "https://api.adviserinfo.sec.gov/search/firm/138854") {
  json_data <-
    url |>
    jsonlite::fromJSON(
      flatten = TRUE,
      simplifyVector = TRUE,
      simplifyDataFrame = TRUE
    )

  data <- json_data$hits$hits$`_source.iacontent` |> as.character() |> jsonlite::fromJSON(simplifyDataFrame = T, flatten = T)

  names(data)

  data

}

# sec_adv_data ------------------------------------------------------------

.parse_sec_manager_pdf_url <-
  function(url = 'https://files.adviserinfo.sec.gov/IAPD/IAPDFirmSummary.aspx?ORG_PK=156663') {
    # Extract CRD from URL
    idCRD <- url %>%
      str_extract("ORG_PK=([0-9]+)") %>%
      str_remove("ORG_PK=") %>%
      as.numeric()

    # If URL is in old format (adviserinfo.sec.gov/firm/brochure/CRD), convert it
    if (str_detect(url, "adviserinfo.sec.gov/firm/brochure")) {
      idCRD <- url %>%
        str_extract("[0-9]+$") %>%
        as.numeric()
      url <- glue::glue("https://files.adviserinfo.sec.gov/IAPD/IAPDFirmSummary.aspx?ORG_PK={idCRD}")
    }

    page <-
      url %>%
      .get_html_page()

    # Extract entity name from firm summary page
    nameEntityBrochure <-
      page %>%
      .get_html_node_text(node_css = '#ctl00_cphMain_landing_lblActiveOrgName') %>%
      str_trim()

    # Extract brochure PDF URL
    urlPDFManagerADVBrochure <-
      page %>%
      .get_html_node_attributes(node_css = '#ctl00_cphMain_landing_p2BrochureLink',
                               html_attribute = 'href',
                               base_url = 'https://files.adviserinfo.sec.gov')

    # Dates are not available on the firm summary page
    # They would require accessing additional pages or the API
    dateBrochureSubmitted <- NA
    dateLastConfirmed <- NA

    pdf_data <-
      tibble(
        idCRD,
        nameEntityBrochure,
        urlPDFManagerADVBrochure,
        dateBrochureSubmitted,
        dateLastConfirmed
      )

    if (pdf_data %>% nrow > 1) {
      pdf_data <-
        pdf_data %>%
        pivot_longer(cols = -c(idCRD, dateBrochureSubmitted, dateLastConfirmed),
                     names_to = "item",
                     values_to = "value") %>%
        group_by(item) %>%
        mutate(idItem = (1:n() - 1),
               nameItem = if_else(idItem > 0, item %>% paste0(idItem), item)) %>%
        ungroup() %>%
        dplyr::select(-c(item, idItem)) %>%
        pivot_wider(names_from = nameItem, values_from = value)
    }

    return(pdf_data)

  }


.get_url_crd <-
  function(url = 'https://files.adviserinfo.sec.gov/IAPD/IAPDFirmSummary.aspx?ORG_PK=135952') {
    idCRD <-
      url %>%
      str_split('\\?ORG_PK=') %>%
      flatten_chr() %>%
      .[2] %>%
      as.numeric()
    return(idCRD)
  }


.get_manager_sec_page <-
  function(url = 'https://files.adviserinfo.sec.gov/IAPD/IAPDFirmSummary.aspx?ORG_PK=156663') {
    httr::set_config(httr::config(ssl_verifypeer = 0L))

    # Convert new URL format (adviserinfo.sec.gov/firm/summary/CRD) to old format
    # (files.adviserinfo.sec.gov/IAPD/IAPDFirmSummary.aspx?ORG_PK=CRD)
    if (str_detect(url, "adviserinfo\\.sec\\.gov/firm/summary/")) {
      crd_id <- url %>%
        str_extract("summary/([0-9]+)") %>%
        str_replace("summary/", "")
      url <- paste0("https://files.adviserinfo.sec.gov/IAPD/IAPDFirmSummary.aspx?ORG_PK=", crd_id)
    }

    page_status <-
      url %>%
      GET()

    if (page_status$url == 'https://files.adviserinfo.sec.gov/IAPD/SearchNoResult.aspx') {
      idCRD <-
        url %>%
        .get_url_crd()

      manager_df <-
        tibble(idCRD,
                   urlManagerSummaryADV = NA)
      "No data" %>% message
    }
    else {
      page <-
        url %>%
        .get_html_page()

      name_entity_manager <-
        page %>%
        .get_html_node_text(node_css = '#ctl00_cphMain_landing_lblActiveOrgName')

      idCRDSEC <-
        page %>%
        .get_html_node_text(node_css = '#ctl00_cphMain_landing_lblActiveOrgCrd') %>%
        str_replace_all('\\(|\\)|CRD# |SEC#', '')

      get_all_status_data <-
        function(page) {
          parse_era_table <-
            function(page) {
              nodes_exist <-
                page %>%
                html_nodes('#tbERAStatus td') %>% length() > 0

              if (nodes_exist) {
                statusJurisdictionERA <-
                  page %>%
                  html_nodes('#tbERAStatus td:nth-child(1)') %>%
                  html_text()

                statusReportingERA <-
                  page %>%
                  html_nodes('#tbERAStatus td:nth-child(2)') %>%
                  html_text()

                dateEffectiveERA <-
                  page %>%
                  html_nodes('#tbERAStatus td:nth-child(3)') %>%
                  html_text() %>%
                  lubridate::mdy() %>%
                  as.character()


                table_df <-
                  tibble(statusJurisdictionERA,
                             statusReportingERA,
                             dateEffectiveERA) %>%
                  mutate(nameEntityManager = name_entity_manager,
                         countItem = 1:n()) %>%
                  dplyr::select(nameEntityManager, everything()) %>%
                  .widen_adv_data() %>%
                  .mutate_adv_data()

              } else {
                table_df <-
                  tibble(nameEntityManager = name_entity_manager, isExempt = F)
              }
              return(table_df)
            }

          parse_state_table <-
            function(page) {
              nodes_exist <-
                page %>%
                html_nodes('#tbNtcStatus td') %>% length() > 0

              if (nodes_exist) {
                stateRegistration <-
                  page %>%
                  html_nodes('#tbNtcStatus td:nth-child(1)') %>%
                  html_text()

                dateEffectiveState <-
                  page %>%
                  html_nodes('#tbNtcStatus td:nth-child(2)') %>%
                  html_text() %>%
                  lubridate::mdy() %>%
                  as.character()

                table_df <-
                  tibble(stateRegistration, dateEffectiveState) %>%
                  mutate(nameEntityManager = name_entity_manager) %>%
                  dplyr::select(nameEntityManager, everything()) %>%
                  mutate(countItem = 1:n()) %>%
                  arrange(countItem) %>%
                  .widen_adv_data() %>%
                  .mutate_adv_data()

              } else {
                table_df <-
                  tibble(nameEntityManager = name_entity_manager)
              }
              return(table_df)
            }

          parse_reg_status <-
            function(page) {
              nodes_exist <-
                page %>%
                html_nodes('#tbRegStatus td') %>% length() > 0

              if (nodes_exist) {
                statusJurisdiction <-
                  page %>%
                  html_nodes('#tbRegStatus td:nth-child(1)') %>%
                  html_text()

                statusReporting <-
                  page %>%
                  html_nodes('#tbRegStatus td:nth-child(2)') %>%
                  html_text()

                dateEffective <-
                  page %>%
                  html_nodes('#tbRegStatus td:nth-child(3)') %>%
                  html_text() %>%
                  lubridate::mdy() %>%
                  as.character()

                table_df <-
                  tibble(statusJurisdiction, statusReporting, dateEffective) %>%
                  mutate(nameEntityManager = name_entity_manager,
                         countItem = 1:n()) %>%
                  dplyr::select(nameEntityManager, everything()) %>%
                  .widen_adv_data() %>%
                  .mutate_adv_data()

              } else {
                table_df <-
                  tibble(nameEntityManager = name_entity_manager, isExempt = F)
              }
              return(table_df)
            }

          era_df <-
            page %>%
            parse_era_table()

          reg_df <-
            parse_reg_status(page)

          state_df <-
            parse_state_table(page)

          all_df <-
            era_df %>%
            left_join(reg_df) %>%
            left_join(state_df) %>%
            suppressMessages()
          return(all_df)
        }

      manager_df <-
        page %>%
        get_all_status_data()

      urlManagerADV <-
        page %>%
        .get_html_node_attributes(
          node_css = '#aspnetForm > div.container-fluid > div > nav > ul > li:nth-child(7) > a',
          html_attribute = 'href',
          base_url = 'https://files.adviserinfo.sec.gov'
        )

      manager_df <-
        manager_df %>%
        mutate(
          nameEntityManager = name_entity_manager,
          idCRDSEC,
          urlManagerSummaryADV = url,
          urlManagerADV
        ) %>%
        separate(idCRDSEC,
                 sep = ' / ',
                 into = c('idCRD', 'idSEC')) %>%
        mutate(idCRD = idCRD %>% as.numeric()) %>%
        dplyr::select(nameEntityManager, idCRD, idSEC, everything())

      brochure_exists <-
        page %>%
        .check_html_node(node_css = '#ctl00_cphMain_landing_p2BrochureLink')

      if (brochure_exists) {
        urlPDFManager <-
          page %>%
          .get_html_node_attributes(
            node_css = '#aspnetForm > div.container-fluid > div > nav > ul > li:nth-child(8) > a',
            html_attribute = 'href',
            base_url = 'https://files.adviserinfo.sec.gov'
          )

        if (!'nameEntityManager' %in% names(manager_df)) {
          nameEntityManager <-
            page %>%
            .get_entity_manager_name()
          manager_df <-
            manager_df %>%
            mutate(nameEntityManager)

        }

        manager_df <-
          manager_df %>%
          left_join(urlPDFManager %>%
                      .parse_sec_manager_pdf_url()) %>%
          suppressMessages()
      }
      manager_df <-
        manager_df %>%
        dplyr::select(idCRD, nameEntityManager, everything())
    }

    return(manager_df)
  }


#' IAPD registered meta data
#'
#' This function returns meta data
#' on any Investment Adviser Public Disclosure [IAPD]
#' filing manager.  This function can be used to discover
#' managers to power the
#' \code{\link{adv_managers_filings}} function.
#'
#' @param entity_names vector of entities to search
#' @param crd_ids numeric vector of CRDs to search
#' @param score_threshold matching score threshold for the search name.
#'   If \code{NULL} there is no threshold.
#' @param return_message Logical. If \code{TRUE}, returns progress messages.
#' @param parallel Logical. If \code{TRUE} (default), uses parallel processing via furrr.
#'
#' @import dplyr purrr curl jsonlite lubridate tidyr rvest httr
#' @return a data frame
#' @export
#' @family IAPD
#' @family FINRA
#' @family fund data
#' @family entity search
#' @examples
#' \dontrun{
#' adv_managers_metadata(entity_names = c('Divco', 'EJF'), score_threshold = .2)
#' adv_managers_metadata(entity_names = "Blackstone", score_threshold = NULL)
#' adv_managers_metadata(crd_ids = 173787)
#' }
adv_managers_metadata <-
  function(entity_names =  NULL,
           crd_ids = NULL,
           score_threshold = .2,
           return_message = TRUE,
           parallel = TRUE) {
    if (entity_names %>% purrr::is_null() & crd_ids %>% purrr::is_null()) {
      stop("Please enter a name or CRD to search")
    }

    if (!crd_ids %>% purrr::is_null()) {
      crd_urls <-
        'https://files.adviserinfo.sec.gov/IAPD/IAPDFirmSummary.aspx?ORG_PK=' %>%
        paste0(crd_ids)
    }

    if (!entity_names %>% purrr::is_null()) {
      finra_data <-
        entity_names %>%
        finra_entities(
          ocr_pdf = FALSE,
          return_message = return_message
        ) %>%
        suppressMessages() %>%
        suppressWarnings()

      if (finra_data %>% nrow == 0) {
        return(invisible())
      }

      adv_urls <-
        finra_data$urlManagerSummaryADV
    }

    if ((!crd_ids %>% purrr::is_null()) & (!entity_names %>% purrr::is_null())) {
      adv_urls <-
        c(adv_urls, crd_urls)
    }

    if ((!crd_ids %>% purrr::is_null()) & (entity_names %>% purrr::is_null())) {
      adv_urls <-
        c(crd_urls)
    }

    .get_manager_sec_page_safe <-
      possibly(.get_manager_sec_page, NULL)

    sec_summary_data <-
      adv_urls %>%
      map_df(function(url){
        .get_manager_sec_page(url = url)
      }) %>%
      suppressWarnings() %>%
      filter(!idCRD %>% is.na()) %>%
      mutate(across(where(is.character), str_trim))

    return(sec_summary_data)
  }

.get_manager_sec_adv_actual_url <-
  function(url = 'https://files.adviserinfo.sec.gov/IAPD/crd_iapd_AdvVersionSelector.aspx?ORG_PK=146629') {
    page <-
      url %>%
      GET()

    actual_url <-
      page$url
    return(actual_url)
  }


.get_url_primary_key <-
  function(url = 'https://files.adviserinfo.sec.gov/IAPD/content/viewform/adv/sections/iapd_AdvIdentifyingInfoSection.aspx?ORG_PK=146629&FLNG_PK=01285F220008018901086F5100319405056C8CC0') {
    url_primary_key <-
      url %>%
      str_split(pattern = 'FLNG_PK=') %>%
      flatten_chr() %>%
      .[2]
    return(url_primary_key)
  }


.get_sec_sitemap_df <-
  function() {
    tibble(
        nameSection = c(
          'Registration',
          "Part 1A Item 11 Disclosure Information",
          "DRPs",
          "Item 1 Identifying Information",
          "Item 2 SEC Registration/Reporting",
          "Item 3 Form Of Organization",
          "Item 4 Successions",
          "Item 5 Information About Your Advisory Business",
          "Item 6 Other Business Activities",
          "Item 7.A Financial Industry Affiliations",
          "Item 7.B Private Fund Reporting",
          "Item 8 Participation or Interest in Client Transactions",
          "Item 9 Custody",
          "Item 10 Control Persons",
          "Item 11 Disclosure Information",
          "Item 12 Small Businesses",
          "Schedule A",
          "Schedule B",
          "Schedule D",
          "Signature Page"
        ),
        idSection =
          c(
            'sectionRegistration',
            "sectionDisclosures",
            "sectionDRP",
            "section1IdentifyingInfo",
            "section2SECRegistration",
            "section3OrganizationForm",
            "section4Successions",
            "section5AdvisoryBusinessInformation",
            "section6OtherBusinessInformation",
            "section7AFinanceAffiliations",
            "section7BPrivateFundReporting",
            "section8ClientConflicts",
            "section9Custody",
            "section10ControlPersons",
            "section11Disclosures",
            "section12SmallBusiness",
            "sectionScheduleAOwners",
            "sectionScheduleBIndirectOwners",
            "sectionScheduleD",
            "sectionSignaturePage"
          ),
        nameData =
          c(
            "data_registration",
            "data_disclosures",
            "data_drp",
            "data_1identifyinginfo",
            "data_2secregistration",
            "data_3organizationform",
            "data_4successions",
            "data_5advisorybusinessinformation",
            "data_6otherbusinessinformation",
            "data_7afinanceaffiliations",
            "data_7bprivatefundreporting",
            "data_8clientconflicts",
            "data_9custody",
            "data_10controlpersons",
            "data_11disclosures",
            "data_12smallbusiness",
            "data_schedulea",
            "data_scheduleb",
            "data_scheduled",
            "data_signaturepage"
          ),
        nameFunction = c(
          ".get_manager_sec_page_safe",
          NA,
          ".get_section_drp_safe",
          ".get_section_1_data_safe",
          ".get_section_2_data_safe",
          ".get_section_3_data_safe",
          ".get_section_4_data_safe",
          ".get_section_5_data_safe",
          ".get_section_6_data_safe",
          ".get_section_7a_data_safe",
          ".get_section_7b_data_safe",
          ".get_section_8_data_safe",
          ".get_section_9_data_safe",
          ".get_section_10_data_safe",
          ".get_section_11_data_safe",
          ".get_section_12_data_safe",
          ".get_schedule_a_data_safe",
          ".get_schedule_b_data_safe",
          ".get_schedule_d_data_safe",
          ".get_manager_signatory_data_safe"
        ),
        nameSectionActual = c(
          "Registration",
          "Part 1A Item 11 Disclosure Information",
          "DRPs",
          "Identifying Information",
          "SEC Reporting",
          "Organization",
          "Successions",
          "Advisory Business Information",
          "Other Business Activities",
          "Financial Industry Affiliations",
          "Private Fund Reporting",
          "Participation or Interest in Client Transactions",
          "Custody",
          "Control Persons",
          "Disclosure Information",
          "Small Businesses",
          "Direct Manager Owners",
          "Indirect Manager Owners",
          "Other Manager Information",
          "Manager Signatories"
        )

      )
  }


#' Returns data frame of SEC ADV sitemap
#'
#' @return A tibble containing SEC ADV section identifiers and their corresponding names
#' @export
#' @import dplyr
#' @examples
#' \dontrun{
#' sec_adv_manager_sitemap()
#' }
sec_adv_manager_sitemap <-
  function() {
    data <-
      tibble(
        idSection =
          c(
            'sectionRegistration',
            "sectionDisclosures",
            "sectionDRP",
            "section1IdentifyingInfo",
            "section2SECRegistration",
            "section3OrganizationForm",
            "section4Successions",
            "section5AdvisoryBusinessInformation",
            "section6OtherBusinessInformation",
            "section7AFinanceAffiliations",
            "section7BPrivateFundReporting",
            "section8ClientConflicts",
            "section9Custody",
            "section10ControlPersons",
            "section11Disclosures",
            "section12SmallBusiness",
            "sectionScheduleA",
            "sectionScheduleB",
            "sectionScheduleD",
            "sectionSignaturePage"
          ),
        nameSection = c(
          'Registration',
          "Part 1A Item 11 Disclosure Information",
          "DRPs",
          "Item 1 Identifying Information",
          "Item 2 SEC Registration/Reporting",
          "Item 3 Form Of Organization",
          "Item 4 Successions",
          "Item 5 Information About Your Advisory Business",
          "Item 6 Other Business Activities",
          "Item 7.A Financial Industry Affiliations",
          "Item 7.B Private Fund Reporting",
          "Item 8 Participation or Interest in Client Transactions",
          "Item 9 Custody",
          "Item 10 Control Persons",
          "Item 11 Disclosure Information",
          "Item 12 Small Businesses",
          "Schedule A",
          "Schedule B",
          "Schedule D",
          "Signature Page"
        ),
        nameSectionActual = c(
          "Registration",
          "Part 1A Item 11 Disclosure Information",
          "DRPs",
          "Identifying Information",
          "SEC Reporting",
          "Organization",
          "Successions",
          "Advisory Business Information",
          "Other Business Activities",
          "Financial Industry Affiliations",
          "Private Fund Reporting",
          "Participation or Interest in Client Transactions",
          "Custody",
          "Control Persons",
          "Disclosure Information",
          "Small Businesses",
          "Direct Manager Owners",
          "Indirect Manager Owners",
          "Other Manager Information",
          "Manager Signatories"
        )

      )
    return(data)
  }


.get_location_name_df <-
  function() {
    location_name_df <-
      tibble(
        itemNode = c(
          "Number and Street 1",
          'Number and Street 2',
          "City",
          "State",
          "Country",
          "ZIP+4/Postal Code"
        ),
        nameNode = c(
          "addressStreet1",
          'addressStreet2',
          "city",
          "state",
          "country",
          "zip"
        )
      )
    return(location_name_df)
  }

.get_sitemap_urls <-
  function(data, section_name = 'section7BPrivateFundReporting') {
    urls_exist <-
      data %>%
      dplyr::filter(idSection %in% section_name) %>%
      .$urlADVSection %>% length() > 0
    if (urls_exist) {
      urls <-
        data %>%
        dplyr::filter(idSection %in% section_name) %>%
        .$urlADVSection
      return(urls)
    }
  }


.parse_adv_manager_sitemap_df <-
  function(url = 'https://files.adviserinfo.sec.gov/IAPD/crd_iapd_AdvVersionSelector.aspx?ORG_PK=135952',
           manager = NULL,
           return_wide = F) {
    idCRD <-
      url %>%
      .get_url_crd()

    if (idCRD %>% is.na()) {
      idCRD <-
        url %>%
        str_split('\\?ORG_PK=') %>%
        flatten_chr() %>%
        .[2] %>%
        str_split('\\&') %>%
        flatten_chr() %>%
        .[[1]] %>%
        as.numeric() %>%
        suppressWarnings()
    }

    # Ensure URL uses files.adviserinfo.sec.gov domain (old ASP.NET pages still work there)
    # Only replace if not already using files. subdomain
    if (!str_detect(url, "files\\.adviserinfo\\.sec\\.gov")) {
      url <- url %>%
        str_replace("www\\.adviserinfo\\.sec\\.gov", "files.adviserinfo.sec.gov") %>%
        str_replace("(?<!files\\.)adviserinfo\\.sec\\.gov", "files.adviserinfo.sec.gov")
    }
    url <- url %>% str_replace("^http://", "https://")

    # Try HTML scraping first from files.adviserinfo.sec.gov (detailed Form ADV data)
    actual_url <- tryCatch({
      url %>% .get_manager_sec_adv_actual_url()
    }, error = function(e) url)

    url_primary_key <- tryCatch({
      actual_url %>% .get_url_primary_key()
    }, error = function(e) NA)

    page <- tryCatch({
      actual_url %>% .get_html_page()
    }, error = function(e) NULL)

    if (is.null(page)) {
      # Return empty tibble with correct structure if page fetch fails
      return(tibble(
        idCRD = integer(),
        nameEntityManager = character(),
        nameSectionActual = character(),
        idSection = character(),
        nameData = character(),
        nameFunction = character(),
        urlADVSection = character()
      ))
    }
    base_url <-
      actual_url %>%
      str_split('sections|Sections') %>%
      flatten_chr() %>%
      .[[1]]

    name_entity_manager <-
      page %>%
      .get_entity_manager_name()

    # Fallback selector for new website
    if (length(name_entity_manager) == 0 || is.na(name_entity_manager[1])) {
      name_entity_manager <-
        page %>%
        html_nodes('.summary-displayname') %>%
        html_text()
    }

    # If still empty, use API data
    if (length(name_entity_manager) == 0) {
      api_info <- tryCatch(.get_manager_info_from_api(idCRD), error = function(e) tibble())
      if (nrow(api_info) > 0) {
        name_entity_manager <- api_info$nameEntityManager[1]
      } else {
        name_entity_manager <- NA_character_
      }
    }

    if (length(manager) > 0) {
      name_entity_manager <- manager
    }

    items <-
      page %>%
      .get_html_node_text('.sidebar a[href^=".."]') %>%
      str_trim()

    values <-
      page %>%
      .get_html_node_attributes(node_css = '.sidebar a[href^=".."]',
                                'href') %>%
      str_trim() %>%
      str_replace('../', '') %>%
      paste0(base_url,
             .) %>%
      unique()

    # If no items found via HTML, fall back to API-based sitemap
    if (length(items) == 0 || all(is.na(items))) {
      sitemap_df <- .get_sec_sitemap_df()
      adv_data <- sitemap_df %>%
        mutate(
          idCRD = idCRD,
          nameEntityManager = if (length(name_entity_manager) > 0) name_entity_manager[1] else NA_character_,
          urlADVSection = glue::glue("https://api.adviserinfo.sec.gov/search/firm/{idCRD}#section={idSection}") %>% as.character()
        ) %>%
        dplyr::filter(!nameFunction %>% is.na()) %>%
        dplyr::select(idCRD, nameEntityManager, nameSectionActual, idSection, nameData, nameFunction, urlADVSection)

      if (return_wide) {
        adv_data <- adv_data %>% pivot_wider(names_from = idSection, values_from = urlADVSection)
      }
      return(adv_data)
    }

    adv_data <-
      tibble(
        idCRD,
        nameSection = 'Registration',
        urlADVSection = glue::glue('https://files.adviserinfo.sec.gov/IAPD/IAPDFirmSummary.aspx?ORG_PK={idCRD}') %>% as.character()
      ) %>%
      bind_rows(tibble(idCRD,
                       nameSection = items,
                       urlADVSection = values)) %>%
      left_join(.get_sec_sitemap_df()) %>%
      distinct() %>%
      dplyr::filter(!nameFunction %>% is.na()) %>%
      dplyr::select(-nameSection) %>%
      mutate(nameEntityManager = if (length(name_entity_manager) > 0) name_entity_manager[1] else NA_character_,
             idRow = 1:n()) %>%
      group_by(idSection) %>%
      dplyr::filter(idRow == min(idRow)) %>%
      ungroup() %>%
      dplyr::select(-idRow) %>%
      dplyr::select(idCRD, nameEntityManager, everything()) %>%
      suppressMessages()

    if (return_wide) {
      adv_data <-
        adv_data %>%
        pivot_wider(names_from = idSection, values_from = urlADVSection)

    }
    adv_data
  }

.get_managers_adv_sitemap_adv <-
  function(idCRDs = c(162351),
           score_threshold = .2,
           entity_names =  NULL) {
    adv_managers_metadata_safe <-
      possibly(adv_managers_metadata, NULL)
    .parse_adv_manager_sitemap_df_safe <-
      possibly(.parse_adv_manager_sitemap_df, NULL)

    if (!purrr::is_null(idCRDs)) {
      urls <-
        glue::glue("https://files.adviserinfo.sec.gov/IAPD/crd_iapd_AdvVersionSelector.aspx?ORG_PK={idCRDs}") %>% as.character()
    }

    if (!purrr::is_null(entity_names)) {
      manager_data <-
        entity_names %>%
        future_map_dfr(function(x) {
          adv_managers_metadata(
            crd_ids = idCRDs,
            score_threshold = score_threshold,
            entity_names = x,
            return_message = T
          )
        })

      manager_data <-
        manager_data %>%
        dplyr::filter(!urlManagerADV %>% is.na())
    }

    if ('urls' %>% exists() & 'manager_data' %>% exists()) {
      urls <-
        c(urls, manager_data$urlManagerADV %>% unique()) %>% unique()
    }

    if (idCRDs %>% purrr::is_null() & 'manager_data' %>% exists()) {
      urls <-
        manager_data$urlManagerADV %>% unique()
    }

    if ('urls' %>% exists() & entity_names %>% purrr::is_null()) {
      manager_data <-
        adv_managers_metadata(crd_ids = idCRDs, entity_names = NULL)

      name_entity_manager <-
        manager_data$nameEntityManager
    }
    # Use tryCatch directly instead of possibly() to avoid namespace issues with furrr
    data <-
      urls %>%
      unique() %>%
      future_map_dfr(function(x) {
        tryCatch({
          .parse_adv_manager_sitemap_df(url = x, return_wide = F)
        }, error = function(e) {
          tibble()
        })
      })
    if (manager_data %>% nrow() > 0) {
    data <-
      data %>%
      dplyr::filter(!nameEntityManager %>% is.na()) %>%
      left_join(manager_data %>% dplyr::select(idCRD, nameEntityManager)) %>%
      dplyr::select(
        idCRD,
        nameSectionActual,
        nameEntityManager,
        idSection,
        nameData,
        nameFunction,
        urlADVSection
      ) %>%
      suppressMessages()
    }

    return(data)
  }


.get_type_manager_entity_owner_df <-
  function() {
    type_df <-
      tibble(
        idTypeEntityManagerOwner = c("I", "DE", "FE"),
        typeEntityManagerOwner = c('Individual', 'Domestic Entity', 'Foreign Entity'),
        isEntityOwnerManagerEntity = c(F, T, T)
      )
    return(type_df)
  }

.get_range_entity_owner_df <-
  function() {
    range_df <-
      tibble(
        idRangeManagerEntityOwnership = c(NA, LETTERS[1:6]),
        rangeManagerEntityOwnership = c(
          "< 5%",
          ">= 5%, < 10%",
          ">= 10%, < 25%",
          ">= 25%, < 50%",
          ">= 50%, < 75%",
          ">= 75%",
          "Other (general partner, trustee, or elected manager)"
        )
      )
    return(range_df)
  }

.parse_manager_owner_name <-
  function(x = "GRAY, ROBERT, LAWRENCE") {
    count_commas <-
      x %>% str_count('\\, ')

    full_name <-
      x %>% str_split('\\, ') %>% flatten_chr()

    if (count_commas == 1) {
      name_first <-
        full_name[2]
      name_middle <-
        NA
      name_last <-
        full_name[1]

      name_full <-
        name_first %>% paste(name_last)

      name_common <-
        name_full
    }

    if (count_commas == 2) {
      name_first <-
        full_name[2]

      name_middle <-
        full_name[3]

      name_last <-
        full_name[1]

      name_full <-
        name_first %>% paste(name_middle, name_last)

      name_common <-
        name_first %>% paste(name_last)
    }

    if (count_commas == 3) {
      name_first <-
        full_name[3]

      name_middle <-
        full_name[4]

      name_last <-
        full_name[1] %>%
        paste(full_name[2], collapse = '')

      name_full <-
        name_first %>% paste(name_middle, name_last)

      name_common <-
        name_first %>% paste(name_last)
    }

    name_df <-
      tibble(
        nameEntityManagerOwner = x,
        nameCommonEntityOwnerManager = name_common,
        nameFullEntityOwnerManager = name_full,
        nameFirstEntityManagerOwner = name_first,
        nameMiddleEntityManagerOwner = name_middle,
        nameLastEntityManagerOwner = name_last
      ) %>%
      mutate(across(everything(), str_to_upper))

    return(name_df)
  }

.get_pk_url_crd <-
  function(url = 'https://files.adviserinfo.sec.gov/IAPD/content/viewform/adv/Sections/iapd_AdvPrivateFundReportingSection.aspx?ORG_PK=162351&FLNG_PK=052DAAB400080184043CE66005E35E29056C8CC0') {
    idCRD <-
      url %>%
      str_split('=') %>%
      flatten_chr() %>%
      .[2] %>% str_replace_all('&FLNG_PK', '') %>%
      as.numeric()
    return(idCRD)
  }


.get_sitemap_filter_url <-
  function(data, filter_name = 'sectionScheduleA') {
    urls <-
      data %>%
      dplyr::filter(idSection == filter_name) %>%
      .$urlADVSection
    return(urls)
  }

# private fund data -------------------------------------------------------
.get_check_box_value_df <-
  function() {
    node_check_df <-
      tibble(
        nodeName = c(
          "Checkbox checked, changed",
          "Checkbox not checked",
          "Radio button not selected",
          "Radio button selected, changed"
        ),
        isNodeChecked = c(TRUE, FALSE, FALSE,
                          TRUE)
      )
    return(node_check_df)
  }

.get_form_check_box_df <-
  function(modify = T) {
    form_value_df <-
      tibble(
        idRow = 1:52,
        nameItem = c(
          'hasICA1940S31Exclusion.TRUE',
          'hasICA1940S37Exclusion.TRUE',
          'isMasterFund.TRUE',
          'isMasterFund.FALSE',
          'isFeederFund.TRUE',
          'isFeederFund.FALSE',
          'isFundOfFund.TRUE',
          'isFundOfFund.FALSE',
          'isFundOfFundRelatedInvestment.TRUE',
          'isFundOfFundRelatedInvestment.FALSE',
          'isICA1940PrivateSecuritiesInvestor.TRUE',
          'isICA1940PrivateSecuritiesInvestor.FALSE',
          'isHedgeFund.TRUE',
          'isLiquidityFund.TRUE',
          'isPrivateEquityFund.TRUE',
          'isRealEstateFund.TRUE',
          'isSecuritizedAssetFund.TRUE',
          'isVentureFund.TRUE',
          'isOtherFund.TRUE',
          'hasPrivateFundSubAdviser.TRUE',
          'hasPrivateFundSubAdviser.FALSE',
          'hasOtherPrivateFundAdviser.TRUE',
          'hasOtherPrivateFundAdviser.FALSE',
          'hasSolicitedClientsForFund.TRUE',
          'hasSolicitedClientsForFund.FALSE',
          'hasREGDExemption.TRUE',
          'hasREGDExemption.FALSE',
          'isFundAuditedAnnually.TRUE',
          'isFundAuditedAnnually.FALSE',
          'isFundAuditGAAPCompliant.TRUE',
          'isFundAuditGAAPCompliant.FALSE',
          'isFundAuditorIndependent.TRUE',
          'isFundAuditorIndependent.FALSE',
          'isFundAuditorPCAOBRegistered.TRUE',
          'isFundAuditorPCAOBRegistered.FALSE',
          'isFundAuditorPCAOBCompliant.TRUE',
          'isFundAuditorPCAOBCompliant.FALSE',
          'hasInvestorAnnualFinancialStatement.TRUE',
          'hasInvestorAnnualFinancialStatement.FALSE',
          'hasAuditorUnqualifiedOpinion.TRUE',
          'hasAuditorUnqualifiedOpinion.FALSE',
          'hasAuditorUnqualifiedOpinion.NA',
          'hasFundPrimeBroker.TRUE',
          'hasFundPrimeBroker.FALSE',
          'hasFundCustodian.TRUE',
          'hasFundCustodian.FALSE',
          'isCustodianRelatedPerson.TRUE',
          'isCustodianRelatedPerson.FALSE',
          'hasFundAdministrator.TRUE',
          'hasFundAdministrator.FALSE',
          'hasFundPlacementAgent.TRUE',
          'hasFundPlacementAgent.FALSE'
        )
      )
    if (modify) {
      form_value_df <-
        form_value_df <-
        tibble(
          nameItem = c(
            'isMasterFund.TRUE',
            'isMasterFund.FALSE',
            'isFeederFund.TRUE',
            'isFeederFund.FALSE',
            'isFundOfFund.TRUE',
            'isFundOfFund.FALSE',
            'isFundOfFundRelatedInvestment.TRUE',
            'isFundOfFundRelatedInvestment.FALSE',
            'isFundICASecuritiesInvestor.TRUE',
            'isFundICASecuritiesInvestor.FALSE',
            'isHedgeFund.TRUE',
            'isLiquidityFund.TRUE',
            'isPrivateEquityFund.TRUE',
            'isRealEstateFund.TRUE',
            'isSecuritizedAssetFund.TRUE',
            'isVentureFund.TRUE',
            'isOtherFund.TRUE',
            'hasPrivateFundSubAdviser.TRUE',
            'hasPrivateFundSubAdviser.FALSE',
            'hasOtherPrivateFundAdviser.TRUE',
            'hasOtherPrivateFundAdviser.FALSE',
            'hasSolicitedClientsForFund.TRUE',
            'hasSolicitedClientsForFund.FALSE',
            'hasREGDExemption.TRUE',
            'hasREGDExemption.FALSE',
            'isFundAuditedAnnually.TRUE',
            'isFundAuditedAnnually.FALSE',
            'isFundAuditGAAPCompliant.TRUE',
            'isFundAuditGAAPCompliant.FALSE',
            'isFundAuditorIndependent.TRUE',
            'isFundAuditorIndependent.FALSE',
            'isFundAuditorPCAOBRegistered.TRUE',
            'isFundAuditorPCAOBRegistered.FALSE',
            'isFundAuditorPCAOBCompliant.TRUE',
            'isFundAuditorPCAOBCompliant.FALSE',
            'hasInvestorAnnualFinancialStatement.TRUE',
            'hasInvestorAnnualFinancialStatement.FALSE',
            'hasAuditorUnqualifiedOpinion.TRUE',
            'hasAuditorUnqualifiedOpinion.FALSE',
            'hasAuditorUnqualifiedOpinion.NA',
            'hasFundPrimeBroker.TRUE',
            'hasFundPrimeBroker.FALSE'
          )
        ) %>%
        mutate(idRow = 1:n()) %>%
        dplyr::select(idRow, nameItem)
    }
    return(form_value_df)
  }



.get_fund_table_html <-
  function(page = page,
           table_number = 1,
           table_css_node_df = table_css_node_df) {
    node_css <-
      table_css_node_df %>%
      dplyr::filter(numberTable == table_number) %>%
      .$tableCSSNode

    table_html <-
      page %>%
      html_nodes(css = node_css)

    return(table_html)
  }

.get_table_check_box_df <-
  function(page = page,
           table_number = 1,
           table_css_node_df,
           only_radio = T) {
    node_table <-
      .get_fund_table_html(
        page = page,
        table_number = table_number,
        table_css_node_df = table_css_node_df
      )

    table_checked_nodes <-
      node_table %>%
      html_nodes('img') %>%
      html_attr('alt') %>%
      str_trim()

    if (only_radio) {
      table_checked_nodes <-
        table_checked_nodes[table_checked_nodes %>%
                              str_detect("Radio")]
    }

    check_box_df <-
      tibble(nodeName = table_checked_nodes) %>%
      mutate(idTable = table_number,
             idImageNode = 1:n()) %>%
      left_join(.get_check_box_value_df()) %>%
      suppressMessages() %>%
      dplyr::select(idTable, idImageNode, nodeName, isNodeChecked)
    return(check_box_df)
  }

.get_tables_check_boxes_df <-
  function(page = page,
           table_number = 1,
           table_css_node_df = table_css_node_df) {
    tables_check_boxes_df <-
      table_css_node_df$numberTable %>%
      future_map_dfr(function(x) {
        .get_table_check_box_df(
          page = page,
          table_number = x,
          table_css_node_df = table_css_node_df
        )
      })
    return(tables_check_boxes_df)
  }

.get_table_check_box_data <-
  function(page = page,
           table_number = 1,
           table_css_node_df,
           only_radio = T) {
    form_value_df <-
      .get_form_check_box_df(modify = T)

    check_box_df <-
      .get_table_check_box_df(
        page = page,
        table_number = table_number,
        table_css_node_df = table_css_node_df,
        only_radio = T
      )

    form_value_df <-
      form_value_df %>%
      separate(nameItem,
               into = c('nameItem', 'logicalResponse'),
               sep = '\\.') %>%
      suppressWarnings() %>%
      mutate(logicalResponse = logicalResponse %>% as.logical)

    response_df <-
      check_box_df %>%
      slice(1:nrow(form_value_df)) %>%
      left_join(form_value_df %>% dplyr::rename(idImageNode = idRow)) %>%
      dplyr::filter(isNodeChecked == T) %>%
      dplyr::filter(logicalResponse == T) %>%
      dplyr::select(nameItem, logicalResponse) %>%
      suppressWarnings() %>%
      suppressMessages()

    x <-
      response_df$nameItem

    fund_type <-
      case_when(
        x == 'isHedgeFund' ~ "Hedge Fund",
        x == 'isLiquidityFund' ~ "Liquidity",
        x == 'isPrivateEquityFund' ~ "Private Equity",
        x == 'isRealEstateFund' ~ "Real Estate",
        x == 'isSecuritizedAssetFund' ~ "Securitized Asset",
        x == 'isVentureFund' ~ "Venture",
        x == 'isOtherFund' ~ "Other",
        FALSE ~ as.character(x)
      )

    fund_type <-
      fund_type[!fund_type %>% is.na()]

    response_df <-
      response_df %>%
      dplyr::filter(
        !nameItem %in% c(
          'isHedgeFund',
          'isLiquidityFund',
          'isPrivateEquityFund',
          'isRealEstateFund',
          'isSecuritizedAssetFund',
          'isVentureFund',
          'isOtherFund'
        )
      )

    column_order <-
      response_df$nameItem

    response_df <-
      response_df %>%
      pivot_wider(names_from = nameItem, values_from = logicalResponse) %>%
      dplyr::select(any_of(column_order)) %>%
      mutate(numberFund = table_number,
             typeFund = fund_type) %>%
      dplyr::select(numberFund, typeFund, everything())
    return(response_df)
  }

.parse_table_node_df <-
  function(page = page,
           table_number = 1,
           table_css_node_df = table_css_node_df) {
    base_table_html <-
      .get_fund_table_html(
        page = page,
        table_number = table_number,
        table_css_node_df = table_css_node_df
      ) %>%
      html_nodes('.PaperFormTableData tr td')

    table_data_nodes <-
      base_table_html %>%
      html_text(trim = T) %>%
      str_replace_all('\r|\n|\t', '') %>%
      stri_replace_all_charclass("\\p{WHITE_SPACE}", " ") %>%
      stri_trim_both() %>%
      gsub("^\\s+|\\s+$", "", .)

    table_data_nodes <-
      table_data_nodes[!table_data_nodes == '']

    table_node_df <-
      seq_along(table_data_nodes) %>%
      future_map_dfr(function(x) {
        data_nodes <-
          table_data_nodes[[x]] %>%
          str_split('  ') %>%
          flatten_chr()

        if (data_nodes[!data_nodes == ''] %>% length() > 0) {
          data_nodes <-
            data_nodes[!data_nodes == '']

          nodeText <-
            data_nodes %>% paste0(collapse = ' ')

          nodeText <-
            nodeText %>%
            str_replace_all(
              'Name of the marketer: |Legal name of custodian: |Primary business name of custodian: |CRD Number (if any):|Name of the prime broker: ',
              ''
            )

          isLetter <-
            (nodeText %>% str_detect("^\\([a-z]")) &
            (nodeText %>% nchar() < 10)

          isNumberSection <-
            (nodeText %>% str_detect("^[1-9].")) &
            (nodeText %>% nchar() < 4) &
            (nodeText %>% str_detect('\\.'))

          if (isNumberSection) {
            numberSection <-
              nodeText %>% str_extract_all("^[1-9].") %>% flatten_chr() %>% as.numeric()
          } else {
            numberSection <-
              NA
          }

          if (isLetter) {
            letterSection <-
              nodeText %>% str_extract_all("^\\([a-z]") %>% flatten_chr() %>%
              str_replace_all('\\(', '')
          } else {
            letterSection <-
              NA
          }

          remove_node <-
            nodeText %>% str_detect('Include only those assets where')

          if (remove_node) {
            letterSection <-
              "g"
            isLetter <-
              T
          }

          if (nodeText == 'B. SERVICE PROVIDERS') {
            letterSection <-
              "c"
            isLetter <-
              T
          }
          if (nodeText == 'Your Advisory Services') {
            letterSection <-
              "q"
            isLetter <-
              T
          }

          if (nodeText %>% str_detect('\\CRD Number')) {
            nodeText <-
              nodeText %>%
              gsub('\\CRD Number', '', .) %>%
              gsub('\\(if any):', '', .) %>%
              gsub('and', '', .) %>%
              str_trim()

            letterSection <-
              "cc"

          }

          if (nodeText == 'Private Offering') {
            letterSection <-
              "c"
            isLetter <-
              T
          }

          if (nodeText == 'Administrator') {
            letterSection <-
              "g"
            isLetter <-
              T
          }

          if (nodeText == 'No Information Filed') {
            nodeText <-
              NA
          }

          if (isLetter | isNumberSection) {
            nodeText <-
              NA
          }
          tibble(idNode = x, numberSection, letterSection, nodeText)
        }
      }) %>%
      fill(numberSection, letterSection) %>%
      dplyr::filter(!nodeText %>% is.na()) %>%
      dplyr::filter(!numberSection %>% is.na()) %>%
      dplyr::filter(!nodeText %>% str_detect("^NOTE:|Ownership")) %>%
      unite(
        numberLetterSection,
        numberSection,
        letterSection,
        sep = '',
        remove = F
      ) %>%
      mutate(numberTable = table_number) %>%
      dplyr::select(numberTable, everything())

    return(table_node_df)
  }

.extract_node_data <-
  function(table_node_df,
           variable_name = 'pctFundNonUSCitizens',
           section_letter = "16b",
           method = "max") {
    letter_section_location <-
      c('7c', '23c', '24d', '25d', '26c', '28e')

    letter_section_other <-
      c(
        '7a',
        '7b',
        '7d',
        '23b',
        '24a',
        '24b',
        '24c',
        '24cc',
        '25a',
        '25b',
        '25c',
        '25f',
        '25cc',
        '26a',
        '26b',
        '28c',
        '28d',
        '28cc',
        '28g'
      )

    is_location <-
      variable_name %>% str_detect("location")
    if (is_location) {
      method <-
        'location'
    } else {
      method <-
        'max'
    }
    if (!method %>% str_to_lower() %in% c('max', 'location')) {
      stop('Method can only be ')
    }

    if (section_letter %in% table_node_df$numberLetterSection) {
      node_df <-
        table_node_df %>%
        dplyr::filter(numberLetterSection == section_letter)

      if ((method == 'max') &
          (!section_letter %in% letter_section_other)) {
        node_value <-
          node_df %>%
          dplyr::filter(idNode == max(idNode)) %>%
          .$nodeText

        if (variable_name %in% c('nameFundGPManagerTrusteeDirector') &
            node_df %>% nrow > 2)  {
          node_value <-
            node_df %>% slice(-c(1:2)) %>%
            .$nodeText

          if (node_value %>% length() > 1) {
            node_value <-
              node_value %>% paste0(collapse = ' - ')
          }
        }

        if (variable_name %in% c('idSECCRDPrimeBroker', 'idSECCRDCustodian'))  {
          node_value <-
            node_df %>%
            dplyr::filter(!nodeText %>% str_detect('If')) %>%
            .$nodeText

          if (node_value %>% length() > 1) {
            node_value <-
              node_value %>% paste0(collapse = ' & ')
          }
        }
        has_currency <-
          node_value %>% str_detect('\\$') %>% unique()
        if (has_currency) {
          node_value <-
            node_value %>% str_replace('\\$', '') %>%
            str_trim() %>%
            as.character() %>%
            readr::parse_number()
        }
        if (variable_name %>% str_detect("pct")) {
          node_value <-
            node_value %>% str_replace('\\%', '') %>% str_trim() %>% as.numeric() %>% suppressWarnings()
        }

        if (node_value %>% str_detect('\\$') &!is.na(node_value)) {
          node_value <-
            node_value %>% str_replace('\\$|\\', '')
        }
        if ((variable_name == 'idFundFormD') &
            (node_value %>% str_detect("-") == F)) {
          node_value <-
            NA
        }
      }
      if ((method == 'location') &
          (!section_letter %in% c('7c', '23c', '24d', '25d', '26c'))) {
        node_value <-
          node_df %>%
          dplyr::filter(nodeText %>% str_detect('City:|State:|Country')) %>%
          .$nodeText %>%
          str_replace_all('City:|State:|Country:', '')

        node_value <-
          node_value[!node_value == ''] %>% str_trim()
        if (node_value %>% length() > 1) {
          node_value <-
            node_value %>%
            paste0(collapse = ', ') %>%
            str_trim() %>% str_to_upper()
        } else {
          node_value <-
            node_value %>% str_trim() %>% str_to_upper()
        }
      }
      if (section_letter %in% letter_section_other) {
        hitwords <-
          'Name|Private fund|If you are filing |Additional Feeder Fund|If the|Yes|No|The location|Additional|If the marketer is registered|Website Address|If the answer to 28(f)|Click the button|If you or another adviser'
        has_node_length <-
          node_df %>%
          dplyr::filter(!nodeText %>% str_detect(hitwords)) %>%
          .$nodeText %>% length() > 0
        if (has_node_length) {
          node_value <-
            node_df %>%
            dplyr::filter(!nodeText %>% str_detect(hitwords)) %>%
            .$nodeText

          if ((node_value %>% length()) > 1) {
            vars_names <-
              c(variable_name,
                1:(length(node_value) - 1) %>% paste0(variable_name, .))
          }
        } else {
          node_value <-
            NA
        }
      }
      if (section_letter %in% letter_section_location) {
        hit_words <-
          c("The",
            "Under",
            'Location of prime',
            'The location of',
            'Location of') %>%
          paste0(collapse = '|')

        locations_df <-
          node_df %>%
          dplyr::select(idNode, nodeText) %>%
          dplyr::filter(!nodeText %>% str_detect('Yes|No')) %>%
          mutate(idLocationNode = ifelse(nodeText %>% str_detect(hit_words), idNode, NA))

        count_df <-
          locations_df %>%
          dplyr::filter(nodeText %>% str_detect(hit_words)) %>%
          mutate(countLocation = 1:n()) %>%
          dplyr::select(countLocation, idLocationNode, nodeText)

        locations_df <-
          locations_df %>% left_join(count_df) %>%
          fill(countLocation) %>%
          dplyr::filter(!nodeText %>% str_detect("The")) %>%
          dplyr::select(countLocation, nodeText) %>%
          suppressMessages()

        count_locations <-
          locations_df$countLocation %>% unique()

        node_value <-
          count_locations %>%
          map_chr(function(x) {
            node_vals <-
              locations_df %>%
              dplyr::filter(countLocation == x) %>%
              dplyr::filter(nodeText %>% str_detect('City:|State:|Country')) %>%
              .$nodeText %>%
              str_replace_all('City:|State:|Country:', '')
            node_vals <-
              node_vals[!node_vals == ''] %>% str_trim()
            if (node_vals %>% length() > 1) {
              node_vals <-
                node_vals %>%
                paste0(collapse = ', ') %>%
                str_trim()
            } else {
              node_vals <-
                node_vals %>% str_trim()
            }
            return(node_vals %>% str_to_upper)
          })

        if ((node_value %>% length() > 1)) {
          vars_names <-
            c(variable_name,
              1:(length(node_value) - 1) %>% paste0(variable_name, .))
        }
      }

    } else {
      node_value <-
        NA
    }

    if ('vars_names' %>% exists()) {
      variable_name <-
        vars_names
      rm(vars_names) %>% suppressWarnings()
    }

    node_df <-
      tibble(nameVariable = variable_name,
                 valueNode = node_value) %>%
      mutate(sectionLetter = section_letter) %>%
      dplyr::select(sectionLetter, everything())

    return(node_df)
  }

.get_section_matrix_df <-
  function() {
    section_matrix_df <-
      tibble(
        sectionLetter = c(
          "1a",
          "1b",
          "2b",
          "3b",
          "7a",
          "7b",
          "7c",
          "7d",
          "11b",
          "12b",
          "13b",
          "14b",
          "15b",
          "16b",
          "20b",
          "22c",
          "23b",
          "23c",
          "24b",
          "24c",
          '24cc',
          "24d",
          "25b",
          "25c",
          "25d",
          "25f",
          '25cc',
          "26b",
          "26c",
          "27f",
          '28c',
          '28d',
          '28cc',
          '28e',
          '28g'
        ),
        variableName =  c(
          "nameFund",
          "idPrivateFund",
          "locationFundIncorporation",
          "nameFundGPManagerTrusteeDirector",
          "nameFeederFund",
          "idPrivateFundFeeder",
          "locationFeederFund",
          "nameFundFeederGPManagerTrusteeDirector",
          "amountFundGrossAUM",
          "amountFundMinimumInvestment",
          "countFundOwners",
          "pctFundManagerOwned",
          "pctFundFundOfFunds",
          "pctFundNonUSCitizens",
          "pctClientsFundInvestor",
          "idFundFormD",
          "nameAuditorFund",
          "locationAuditorFund",
          "namePrimeBroker",
          "idSECPrimeBroker",
          'idCRDPrimeBroker',
          "locationPrimeBroker",
          "nameCustodianLegal",
          "nameCustodianBusiness",
          "locationCustodian",
          "idSECCustodian",
          'idCRDCustodian',
          "nameAdministratorFund",
          "locationAdministratorFund",
          "pctFund3rdPartyValued",
          'nameFundMarketer',
          'idSECFundMarketer',
          'idCRDFundMarketer',
          'locationFundMarketer',
          'urlFundMarketer'

        )
      )
    return(section_matrix_df)
  }

.parse_funds_tables <-
  function(page,
           table_css_node_df = table_css_node_df,
           section_matrix_df = section_matrix_df,
           return_message = T)  {
    parse_fund_table <-
      function(page,
               table_number = 1,
               table_css_node_df = table_css_node_df,
               section_matrix_df = section_matrix_df) {
        table_node_df <-
          .parse_table_node_df(
            page = page,
            table_number = table_number,
            table_css_node_df = table_css_node_df
          )

      response_df <-
          .get_table_check_box_data(
            page = page,
            table_number = table_number,
            table_css_node_df = table_css_node_df,
            only_radio = T
          )

      .extract_node_data_safe <-
        purrr::possibly(.extract_node_data, tibble())
      fund_table_data <-
        1:nrow(section_matrix_df) %>%
        future_map_dfr(function(x) {
          .extract_node_data(
            table_node_df = table_node_df,
            variable_name = section_matrix_df$variableName[x],
            section_letter = section_matrix_df$sectionLetter[x],
            method = 'max'
          ) %>%
            mutate(valueNode = valueNode %>% as.character())
        }) %>%
        suppressWarnings() %>%
        mutate(valueNode = ifelse(valueNode %in% c('', '-'), NA, valueNode)) %>%
        dplyr::filter(!valueNode %>% is.na())

        col_order <-
          fund_table_data$nameVariable

        fund_table_data <-
          fund_table_data %>%
          dplyr::select(-sectionLetter) %>%
          pivot_wider(names_from = nameVariable, values_from = valueNode) %>%
          dplyr::select(any_of(col_order)) %>%
          mutate(numberFund = table_number) %>%
          dplyr::select(numberFund, everything())

        has_amount <-
          fund_table_data %>% dplyr::select(dplyr::matches("^count|amount")) %>% names() %>% length() > 0

        if (has_amount) {
          fund_table_data <-
            fund_table_data %>%
            mutate(across(matches("^count|amount"), as.numeric))
        }
        has_percent <-
          fund_table_data %>% dplyr::select(dplyr::matches("^pct")) %>% names() %>% length() > 0
        if (has_percent) {
          fund_table_data <-
            fund_table_data %>%
            mutate(across(matches("^pct"), ~as.numeric(.) / 100))
        }
        fund_table_data <-
          fund_table_data %>%
          left_join(response_df) %>%
          dplyr::select(numberFund:nameFund, typeFund, everything()) %>%
          suppressMessages()

        return(fund_table_data)
      }

    parse_fund_table_safe <-
      possibly(parse_fund_table, NULL)

    table_numbers <-
      table_css_node_df$numberTable

    all_table_data <-
      table_numbers %>%
      future_map_dfr(function(x) {
        parse_fund_table_safe(
          page = page,
          table_number = x,
          table_css_node_df = table_css_node_df,
          section_matrix_df = section_matrix_df
        )
      })

    return(all_table_data)
  }


# form_sections -----------------------------------------------------------
.get_section_1_data <-
  function(url = 'https://files.adviserinfo.sec.gov/IAPD/content/viewform/adv102017/Sections/iapd_AdvIdentifyingInfoSection.aspx?ORG_PK=138854&FLNG_PK=0324B906000801AC01C4BA71024984CD056C8CC0') {
    idCRD <-
      url %>%
      .get_pk_url_crd()

    get_node_item_df <- function() {
      items <- c("isUmbrellaADVFiling",
                 'hasChangedLegalName',
                 'hasChangedBusinessName',
                 'isPrivateResidence',
                 'isOfficeOpenMondayFriday',
                 'isOfficeOpenOther',
                 'isPrivateResidence2',
                 "hasCompanySocialMediaAccounts",
                 "hasNoCompanySocialMediaAccounts",
                 "hasSection204Accounting",
                 "hasNoSection204Accounting",
                 "hasForeignRegulatoryRegistration",
                 "hasNoForeignRegulatoryRegistration",
                 "is1934Filer",
                 "isNo1934Filer",
                 "hasAUMOver1B",
                 "hasNoAUMOver1B",
                 "rangeAUM1Bto10B",
                 "rangeAUM10Bto50B",
                 "rangeAUMOver50B")

      data <-
        tibble(nameItem = items, valueItem = T) %>%
        mutate(idRow = 1:n())

      data
    }

    page <-
      url %>%
      .get_html_page()

    name_entity_manager <-
      page %>%
      .get_entity_manager_name()

    .find_text_node_safe <-
      possibly(.find_text_node, NULL)

    .parse_node_df <-
      function(page) {
        check_nodes <-
          page %>%
          html_nodes('.main td img') %>%
          html_attr('alt') %>%
          str_trim()

        node_df <-
          tibble(nodeName = check_nodes) %>%
          left_join(.get_check_box_value_df(), by = "nodeName") %>%
          mutate(idRow = 1:n()) %>%
          left_join(get_node_item_df(), by = "idRow") %>%
          mutate(valueItem = T) %>%
          dplyr::filter(isNodeChecked) %>%
          dplyr::select(nameItem, valueItem) %>%
          suppressMessages()

        column_order <-
          node_df$nameItem

        node_df <-
          node_df %>%
          pivot_wider(names_from = nameItem, values_from = valueItem) %>%
          dplyr::select(any_of(column_order))
        return(node_df)
      }

    .parse_value_nodes <-
      function(page) {
        node_text <-
          page %>%
          .parse_node_table_to_text(css_node = '#ctl00_ctl00_cphMainContent_cphAdvFormContent_IdentInfoPHSection_ctl00_trIAPDHeader + tr +tr td td')

        business_name_df <-
          tibble(
            nameItem = c(
              'nameEntityManagerLegal',
              'nameEntityManagerBusiness',
              'idSEC',
              'addressStreet1OfficePrimary',
              'addressStreet2OfficePrimary',
              'cityOfficePrimary',
              'stateOfficePrimary',
              'countryOfficePrimary',
              'zipOfficePrimary',
              'hoursOffice',
              'phoneOfficePrimary',
              'idLEI'
            ),
            hit_words = c(
              'Your full legal name',
              'Name under which you primarily conduct your advisory business, if different from',
              'egistered with the SEC as an investment adviser',
              'Number and Street 1:',
              'Number and Street 2:',
              'City:',
              'State:',
              'Country:',
              'Postal Code:',
              'Normal business hours at this location:',
              'Telephone number at this location:',
              'Provide your Legal Entity Identifier'
            ),
            off_set = c(1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1),
            is_numeric_node = rep(F, 12)
          )

        business_data_df <-
          1:nrow(business_name_df) %>%
          future_map_dfr(function(x) {
            tibble(
              nameItem =
                business_name_df$nameItem[[x]],
              value =
                .find_text_node_safe(
                  node_text = node_text,
                  hit_words = business_name_df$hit_words[[x]],
                  off_set = business_name_df$off_set[[x]],
                  is_numeric_node = business_name_df$is_numeric_node[[x]]
                )
            )
          }) %>%
          mutate(value = value %>% sub('\\:', '>', .)) %>%
          separate(value, into = c('remove', 'value'), sep = "\\>") %>%
          suppressWarnings()

        business_data_df <-
          business_data_df %>%
          mutate(value = if_else(value %>% is.na, remove, value),
                 value = value %>% str_trim()) %>%
          dplyr::select(nameItem, value) %>%
          dplyr::filter(!value == '') %>%
          mutate(idRow = 1:n()) %>%
          group_by(nameItem) %>%
          dplyr::filter(idRow == min(idRow)) %>%
          ungroup() %>%
          dplyr::select(-idRow) %>%
          distinct()

        column_order <-
          business_data_df$nameItem

        business_data_df <-
          business_data_df %>%
          pivot_wider(names_from = nameItem, values_from = value) %>%
          dplyr::select(any_of(column_order))
        has_secondary_office <-
          'addressStreet2OfficePrimary' %in% names(business_data_df)
        if (has_secondary_office) {
          business_data_df <-
            business_data_df %>%
            unite(
              addressStreet1OfficePrimary,
              addressStreet1OfficePrimary,
              addressStreet2OfficePrimary,
              sep = ' '
            )
        }

        has_office_location <-
          names(business_data_df) %>% str_count('^address|^city|^country|^state') %>% sum() >= 4
        if (has_office_location) {
          business_data_df <-
            business_data_df %>%
            mutate(
              addressOfficePrimary = addressStreet1OfficePrimary %>% paste0(
                ' ',
                cityOfficePrimary,
                ', ',
                stateOfficePrimary,
                ' ',
                countryOfficePrimary,
                ' ',
                zipOfficePrimary
              ) %>% str_to_upper()
            )
        }
        # Check if idLEI column exists and contains placeholder text
        if ('idLEI' %in% names(business_data_df)) {
          if (nrow(business_data_df) > 0 && !is.na(business_data_df$idLEI[1]) &&
              business_data_df$idLEI[1] == 'A legal entity identifier') {
            business_data_df <-
              business_data_df %>%
              dplyr::select(-idLEI)
          }
        }
        return(business_data_df)
      }

    section_data <-
      page %>%
      .parse_value_nodes() %>%
      mutate(idCRD, nameEntityManager = name_entity_manager) %>%
      left_join(page %>%
                  .parse_node_df() %>% mutate(idCRD, nameEntityManager = name_entity_manager)) %>%
      .select_start_vars() %>%
      suppressMessages()

    section_data <-
      section_data %>%
      mutate(across(matches("^address|^city|^state|^country"), str_to_upper)) %>%
      mutate(across(where(is.character), ~str_remove_all(., "Item 1.A.") %>% str_trim()))

    return(section_data)
  }

.get_section_2_data <-
  function(url = 'https://files.adviserinfo.sec.gov/IAPD/content/viewform/adv/Sections/iapd_AdvSecRegistrationSection.aspx?ORG_PK=158207&FLNG_PK=056E2F260008019605946011010394F5056C8CC0') {
    idCRD <-
      url %>%
      .get_pk_url_crd()


    get_node_item_df <- function(page) {
      node_item_df <-
        c(
          "hasAUMGreater100M",
          "hasAUMUnder100MOver25m",
          "hasPrincipalOfficeWY",
          "hasPrincipalOfficeForeign",
          "isAdviser1940InvestmentActCompany",
          "isAdviserBusinessDevelopmentCompany25MInCapital",
          "isAdviserPensionCapitalGreater200M",
          "isAdviserRelated203A",
          "isAdviserNew203A",
          "isAdviserMultiState203A",
          "isAdviserInternet",
          "hasSECOrderProhibitingRegistration",
          "isAdviserSECIneligible"
        ) %>%
        .has_item_check_name()

      state_exists <-
        page %>%
        html_nodes('.main') %>%
        html_nodes(
          '#ctl00_ctl00_cphMainContent_cphAdvFormContent_SECRegisteredPHSection_ctl00_FourthColumn'
        ) %>%
        length() > 0

      if (state_exists) {
        states <-
          c(
            "AL",
            "AK",
            "AZ",
            "AR",
            "CA",
            "CO",
            "CT",
            "DE",
            "DC",
            "FL",
            "GA",
            "GU",
            "HI",
            "ID",
            "IL",
            "IN",
            "IA",
            "KS",
            "KY",
            "LA",
            "ME",
            "MD",
            "MA",
            "MI",
            "MN",
            "MS",
            "MO",
            "MT",
            "NE",
            "NV",
            "NH",
            "NJ",
            "NM",
            "NY",
            "NC",
            "ND",
            "OH",
            "OK",
            "OR",
            "PA",
            "PR",
            "RI",
            "SC",
            "SD",
            "TN",
            "TX",
            "UT",
            "VT",
            "VI",
            "VA",
            "WA",
            "WV",
            "WI"
          )
        item_name <-
          'stateRegistered'
        state_df <-
          seq_along(states) %>%
          future_map_dfr(function(x) {
            tibble(nameItem = item_name,
                       valueItem = states[x]) %>%
              unite(fullnameItem,
                    nameItem,
                    valueItem,
                    sep = '.',
                    remove = F)
          })

        node_item_df <-
          node_item_df %>% mutate(valueItem = valueItem %>% as.character()) %>%
          bind_rows(state_df)
      }

      return(node_item_df)
    }

    page <-
      url %>%
      .get_html_page()

    name_entity_manager <-
      page %>%
      .get_entity_manager_name()

    parse_node_df <-
      function(page) {
        check_nodes <-
          page %>%
          html_nodes('.main td img') %>%
          html_attr('alt') %>%
          str_trim()

        check_nodes <-
          check_nodes[!check_nodes == '']

        if (check_nodes %>% length() == 3) {
          node_item_df <-
            tibble(
              nameItem = c(
                'hasExemptionAsSolelyVentureAdviser',
                'hasExemptionAsPrivateFundManagerUnder150MAUM',
                'hasExemptionSoleyPrivateFundManagerAUMOver150M'
              ),
              valueItem = T
            )
          node_df <-
            tibble(nodeName = check_nodes) %>%
            left_join(.get_check_box_value_df()) %>%
            mutate(isNodeChecked = if_else(nodeName %>% str_detect('Checkbox not checked'), F, T)) %>%
            bind_cols(node_item_df) %>%
            mutate(valueItem = T) %>%
            dplyr::filter(isNodeChecked == T) %>%
            dplyr::select(nameItem, valueItem) %>%
            suppressMessages()
        }

        if (!check_nodes %>% length() == 3) {
          node_item_df <-
            get_node_item_df(page = page)

          if (check_nodes %>% length() == 56) {
            node_item_df <-
              node_item_df %>%
              dplyr::filter(nameItem %>% str_detect("^state")) %>%
              dplyr::select(nameItem, valueItem)

            node_item_df <-
              tibble(
                nameItem = c(
                  'hasExemptionAsSolelyVentureAdviser',
                  'hasExemptionAsPrivateFundManagerUnder150MAUM',
                  'hasExemptionSoleyPrivateFundManagerAUMOver150M'
                ),
                valueItem = T %>% as.character()
              ) %>% bind_rows(node_item_df)

          }

          if (check_nodes %>% length() == 66) {
            node_item_df <-
              node_item_df %>%
              dplyr::filter(nameItem %>% str_detect("^state")) %>%
              dplyr::select(nameItem, valueItem)

            node_item_df <-
              tibble(
                nameItem = c(
                  "hasAUMGreater100M",
                  "hasAUMUnder100MOver25m",
                  "hasPrincipalOfficeWY",
                  "hasPrincipalOfficeForeign",
                  "isAdviser1940InvestmentActCompany",
                  "isAdviserBusinessDevelopmentCompany25MInCapital",
                  "isAdviserPensionCapitalGreater200M",
                  "isAdviserRelated203A",
                  "isAdviserNew203A",
                  "isAdviserMultiState203A",
                  "isAdviserInternet",
                  "hasSECOrderProhibitingRegistration",
                  "isAdviserSECIneligible"
                ),
                valueItem = T %>% as.character()
              ) %>% bind_rows(node_item_df)

          }

          node_df <-
            tibble(nodeName = check_nodes) %>%
            left_join(.get_check_box_value_df()) %>%
            tidyr::replace_na(list(isNodeChecked = F)) %>%
            mutate(isNodeChecked = if_else(nodeName %>% str_detect('Checkbox not checked'), F, T)) %>%
            bind_cols(node_item_df) %>%
            dplyr::filter(isNodeChecked == T) %>%
            dplyr::select(nameItem, valueItem) %>%
            suppressMessages()
        }

        node_df <-
          node_df %>%
          group_by(nameItem) %>%
          mutate(countItem = 1:n()) %>%
          ungroup() %>%
          mutate(nameEntityManager = name_entity_manager) %>%
          mutate(
            countItem = countItem - 1,
            countItem = countItem %>% as.character(),
            countItem = ifelse(countItem == "0", '', countItem)
          ) %>%
          unite(item, nameItem, countItem, sep = '') %>%
          distinct() %>%
          suppressWarnings()

        col_order <-
          c('nameEntityManager', node_df$item)

        node_df <-
          node_df %>%
          pivot_wider(names_from = item, values_from = valueItem) %>%
          dplyr::select(any_of(col_order)) %>%
          .mutate_adv_data()
        return(node_df)
      }

    section_data <-
      page %>% parse_node_df() %>%
      mutate(idCRD, nameEntityManager = name_entity_manager) %>%
      .select_start_vars()

    return(section_data)
  }

.get_section_3_data <-
  function(url = 'https://files.adviserinfo.sec.gov/IAPD/content/viewform/adv/Sections/iapd_AdvFormOfOrgSection.aspx?ORG_PK=158207&FLNG_PK=056E2F260008019605946011010394F5056C8CC0') {
    idCRD <-
      url %>%
      .get_pk_url_crd()

    page <-
      url %>%
      .get_html_page()

    name_entity_manager <-
      page %>%
      .get_entity_manager_name()

    parse_organizational_form_data <-
      function(page) {
        boxes <-
          page %>%
          html_nodes('img') %>%
          html_attr('alt') %>%
          str_trim()

        boxes <-
          boxes[boxes %>%
                  str_detect("Radio")]

        typeEntityManager <-
          tibble(
            typeEntityManager = c(
              'Corporation',
              'Sole Proprietorship',
              'LLP',
              'Partnership',
              'LLC',
              'LP',
              'Other'
            ),
            nodeName = boxes
          ) %>%
          left_join(.get_check_box_value_df()) %>%
          dplyr::filter(isNodeChecked == T) %>%
          suppressMessages() %>%
          .$typeEntityManager

        nodes <-
          page %>%
          html_nodes('.PrintHistRed') %>%
          html_text()

        monthFiscalYearEnd <-
          nodes[1]

        if (nodes %>% length() == 3) {
          locationEntityOrganized <-
            nodes[2:3] %>% str_to_upper() %>% paste0(collapse = ', ')
        } else {
          locationEntityOrganized <-
            NA
        }
        org_data <-
          tibble(typeEntityManager,
                     monthFiscalYearEnd,
                     locationEntityOrganized)
        return(org_data)
      }

    section_data <-
      page %>%
      parse_organizational_form_data() %>%
      mutate(idCRD, nameEntityManager = name_entity_manager) %>%
      .select_start_vars()

    return(section_data)

  }


.get_section_4_data <-
  function(url = 'https://files.adviserinfo.sec.gov/IAPD/content/viewform/adv/Sections/iapd_AdvSuccessionsSection.aspx?ORG_PK=158207&FLNG_PK=056E2F260008019605946011010394F5056C8CC0',
           return_wide = T) {
    idCRD <-
      url %>%
      .get_pk_url_crd()

    get_node_item_df <-
      function() {
        node_item_df <-
          'isSucceedingRIABusiness' %>%
          .get_item_name_yes_no_df()
        return(node_item_df)
      }

    page <-
      url %>%
      .get_html_page()

    name_entity_manager <-
      page %>%
      .get_entity_manager_name()

    parse_node_df <-
      function(page) {
        check_nodes <-
          page %>%
          html_nodes('.main td img') %>%
          html_attr('alt') %>%
          str_trim()

        node_df <-
          tibble(nodeName = check_nodes) %>%
          left_join(.get_check_box_value_df()) %>%
          bind_cols(get_node_item_df()) %>%
          dplyr::filter(isNodeChecked == T) %>%
          dplyr::select(nameItem, valueItem) %>%
          suppressMessages()

        column_order <-
          node_df$nameItem

        node_df <-
          node_df %>%
          pivot_wider(names_from = nameItem, values_from = valueItem) %>%
          dplyr::select(any_of(column_order))

        has_ria_node <-
          'isSucceedingRIABusiness' %in% names(node_df)

        if (has_ria_node) {
          if (node_df$isSucceedingRIABusiness == T) {
            node_text <-
              page %>%
              .parse_node_table_to_text(
                '#ctl00_ctl00_cphMainContent_cphAdvFormContent_SuccessionPHSection_ctl00_trIAPDHeader+ tr td'
              )

            dateSuccession <-
              node_text %>%
              .find_text_node(
                hit_words = "Date of Succession",
                off_set = 0,
                is_numeric_node = F
              ) %>%
              lubridate::mdy()
            node_df <-
              node_df %>%
              mutate(dateSuccession)
          }
        }
        return(node_df)
      }

    name_entity_manager <-
      page %>%
      .get_entity_manager_name()

    section_data <-
      page %>% parse_node_df() %>%
      mutate(idCRD, nameEntityManager = name_entity_manager) %>%
      .select_start_vars()

    if (!return_wide) {
      section_data <-
        section_data %>%
        pivot_longer(cols = -c(idCRD, nameEntityManager),
                     names_to = "itemName",
                     values_to = "value")
    }

    return(section_data)
  }

.get_section_5_data <-
  function(url = "https://files.adviserinfo.sec.gov/IAPD/content/viewform/adv/Sections/iapd_AdvAdvisoryBusinessSection.aspx?ORG_PK=158207&FLNG_PK=056E2F260008019605946011010394F5056C8CC0") {
    section_name <-
      'section5AdvisoryBusinessInformation'
    idCRD <-
      url %>%
      .get_pk_url_crd()

    page <-
      url %>%
      .get_html_page()

    name_entity_manager <-
      page %>%
      .get_entity_manager_name()

    if (!'sitefuture_map_dfr' %>% exists()) {
      url <-
        glue::glue('https://files.adviserinfo.sec.gov/IAPD/crd_iapd_AdvVersionSelector.aspx?ORG_PK={idCRD}')

      sitefuture_map_dfr <-
        .parse_adv_manager_sitemap_df(url =url, return_wide = F, manager = name_entity_manager)
    }

    is_new_iapd <- page %>% html_nodes('.QueryHeaderLabel') %>% html_text() %>% str_detect(" Amount of Regulatory Assets") %>% sum(na.rm = T) > 0

    section_exists <-
      section_name %in% sitefuture_map_dfr$idSection

    if (!section_exists) {
      stop(section_name %>% paste0(" does not exists for ", idCRD))
    }

    .get_section_5_node_name_df <- function() {
      base <- "hasLessThan5Clients"
      terms <- c("IndividualNonHighNetWorth", "IndividualHighNetWorth", "BankThrift", "PensionPlan",
                 "ClientsCharitable","StateMunicipalGovernment","InvestmentAdviserOther","InsuranceCompany","SoverignWealthFund","CorporationOther","OtherAll")

      ranges <- glue::glue("{base}{terms}") %>% as.character()
      fees <-
        c("hasFeeAUM",
          "hasFeeHourlyCharge",
          "hasFeeSubscription",
          "hasFeeFixed",
          "hasFeeCommission",
          "hasFeePerformance",
          "hasFeeOther",
          "hasSecuritiesPortfolioManagement.TRUE",
          "hasSecuritiesPortfolioManagement.FALSE",
          "hasFinancialPlanning",
          "hasPortfolioManagementIndividualSmallBusiness",
          "hasPortfolioManagementInvestmentCompanies",
          "hasPortfolioManagementPooledInvestmentVehicles",
          "hasPortfolioManagementInstitutionalClients",
          "hasServicePensionConsulting",
          "hasServiceInvestmentAdviserSelection",
          "hasServicePeriodicalPublication",
          "hasServiceSecurityRating",
          "hasServiceMarketTiming",
          "hasServiceEducationSeminars",
          "hasServiceOther",
          "rangeClientsFinancialPlanningZero",
          "rangeClientsFinancialPlanning1to10",
          "rangeClientsFinancialPlanning11to25",
          "rangeClientsFinancialPlanning26to50",
          "rangeClientsFinancialPlanning51to100",
          "rangeClientsFinancialPlanning101to250",
          "rangeClientsFinancialPlanning251to500",
          "rangeClientsFinancialPlanningOver500",
          "hasFeeWrapSponsor.TRUE",
          "hasFeeWrapSponsor.FALSE",
          "hasInvestmentAdviceLimited.TRUE",
          "hasInvestmentAdviceLimited.FALSE",
          "hasDifferentClientReportMethod.TRUE",
          "hasDifferentClientReportMethod.FALSE",
          "hasAUMOther5D.TRUE",
          "hasAUMOther5D.FALSE",
          "hasSeperateAccountMargin.TRUE",
          "hasSeperateAccountMargin.FALSE",
          "hasSeperateAccountDervatives.TRUE",
          "hasSeperateAccountDervatives.FALSE",
          "hasCustodian10PCT.TRUE",
          "hasCustodian10PCT.FALSE"
        )

      tibble(nameItem = c(ranges,fees)) %>%
        separate(nameItem, into = c("nameItem", "valueItem"), sep = "\\.") %>%
        mutate(valueItem = ifelse(is.na(valueItem), T, valueItem) %>% as.logical()) %>%
        suppressWarnings() %>%
        mutate(idRow = 1:n())
    }



    section_exists <-
      'section5AdvisoryBusinessInformation' %in% sitefuture_map_dfr$idSection

    if (!section_exists) {
      stop("Company structure data does not exists for " %>% paste0(idCRD))
    }

    parse_company_structure_page <-
      function(page)  {
        parse_section_5_check_nodes <-
          function(page) {
            check_nodes <-
              page %>%
              html_nodes('.main td img') %>%
              html_attr('alt') %>%
              str_trim()

            client_summary_image_df <-
              tibble(nodeName = check_nodes) %>%
              left_join(.get_check_box_value_df()) %>%
              suppressMessages()

            client_summary_image_df <-
              client_summary_image_df %>%
              mutate(idRow = 1:n()) %>%
              left_join(
                .get_section_5_node_name_df(),
                by = "idRow"
                ) %>%
              select(-idRow) %>%
              suppressMessages() %>%
              dplyr::filter(isNodeChecked) %>%
              dplyr::filter(!is.na(nameItem)) %>%
              dplyr::select(nameItem, valueItem) %>%
              distinct()

            column_order <-
              client_summary_image_df$nameItem %>% unique()

            client_summary_df <-
              client_summary_image_df %>%
              pivot_wider(names_from = nameItem, values_from = valueItem) %>%
              dplyr::select(any_of(column_order))

            client_summary_df <-
              client_summary_df %>%
              mutate(across(matches("^is|^has"), as.logical))

            client_summary_df
          }

        .find_text_node_safe <-
          possibly(.find_text_node, NULL)

        name_entity_manager <-
          page %>%
          .get_entity_manager_name()

        ##
        node_text <-
          page %>%
          .parse_node_table_to_text(css_node = '#ctl00_ctl00_cphMainContent_cphAdvFormContent_ClientCompensation_ctl00_trIAPDHeader + tr + tr')

        employee_node_df <-
          tibble(
            nameItem = c(
              "countEmployeesTotal",
              "countEmployeesInvestmentAdvisory",
              "countEmployeesBrokerDealer",
              "countEmployeesStateRegisteredInvestmentAdviser",
              "countEmployeesStateRegisteredInvestmentAdviserMultipleEntities",
              "countEmployeesLicensedInsuranceAgents",
              "countEmployeesSolicitAdvisoryClients",
              "pctClientsNonUS"
            ),
            hit_words = c(
              'but do not include any clerical workers.',
              ' perform investment advisory',
              'registered representatives of a broker-dealer',
              'investment adviser representatives',
              'for an investment adviser other than you',
              'licensed agents of an insurance company or agency',
              'Approximately how many firms or other',
              'are non-United States persons'
            ),
            off_set = c(1, 1, 1, 1, 1, 1, 5, 1),
            is_numeric_node = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, T)
          )

        employee_count_df <-
          1:nrow(employee_node_df) %>%
          future_map_dfr(function(x) {
            tibble(
              nameItem =
                employee_node_df$nameItem[[x]],
              value =
                .find_text_node_safe(
                  node_text = node_text,
                  hit_words = employee_node_df$hit_words[[x]],
                  off_set = employee_node_df$off_set[[x]],
                  is_numeric_node = employee_node_df$is_numeric_node[[x]]
                )
            )
          }) %>%
          pivot_wider(names_from = nameItem, values_from = value) %>%
          dplyr::select(any_of(employee_node_df$nameItem)) %>%
          suppressWarnings()

        # Convert pctClientsNonUS to decimal if column exists
        if ('pctClientsNonUS' %in% names(employee_count_df)) {
          employee_count_df <- employee_count_df %>%
            mutate(pctClientsNonUS = pctClientsNonUS / 100)
        }

        node_text <-
          page %>%
          .parse_node_table_to_text(
            '#ctl00_ctl00_cphMainContent_cphAdvFormContent_AssetsUnderMgnmt_ctl00_trIAPDHeader + tr table td tr td'
          )

        aum_value_name_df <-
          tibble(
            nameItem = c(
              "amountAUMDiscretionary",
              "amountAUMNonDiscretionary",
              "amountAUMTotal",
              "countAccountsDiscretionary",
              "countAccountsNonDiscretionary",
              "countAccountsTotal"
            ),
            hit_words = c(
              c(
                'Discretionary',
                'Non-Discretionary',
                'Total:',
                'Discretionary',
                'Non-Discretionary',
                'Total:'
              )
            ),
            off_set = c(2, 2, 2, 4, 4, 4),
            is_numeric_node = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
          )

        has_aum_df <-
          node_text %>% str_count(aum_value_name_df$hit_words %>% paste0(collapse = "|")) %>% sum() > 0

        if (has_aum_df) {
          aum_value_df <-
            1:nrow(aum_value_name_df) %>%
            future_map_dfr(function(x) {
              has_value <-
                .find_text_node_safe(
                  node_text = node_text,
                  hit_words = aum_value_name_df$hit_words[[x]],
                  off_set = aum_value_name_df$off_set[[x]],
                  is_numeric_node = aum_value_name_df$is_numeric_node[[x]]
                ) %>% length() > 0

              if (has_value) {
                val <-
                  .find_text_node_safe(
                    node_text = node_text,
                    hit_words = aum_value_name_df$hit_words[[x]],
                    off_set = aum_value_name_df$off_set[[x]],
                    is_numeric_node = aum_value_name_df$is_numeric_node[[x]]
                  )
              } else {
                val <-
                  NA
              }

              tibble(nameItem =
                           aum_value_name_df$nameItem[[x]],
                         value =
                           val)
            }) %>%
            distinct() %>%
            group_by(nameItem) %>%
            dplyr::filter(value == max(value)) %>%
            ungroup() %>%
            pivot_wider(names_from = nameItem, values_from = value) %>%
            suppressWarnings()
          aum_value_df <-
            aum_value_df %>%
            dplyr::select(dplyr::matches(aum_value_name_df$nameItem %>% paste0(collapse = '|'))) %>%
            mutate(nameEntityManager = name_entity_manager)

        } else {
          aum_value_df <-
            tibble(nameEntityManager = name_entity_manager)
        }

        check_node_df <-
          page %>% parse_section_5_check_nodes()

        section_5_data <-
          tibble(idCRD,
                     nameEntityManager = name_entity_manager) %>%
          bind_cols(list(employee_count_df, check_node_df)) %>%
          left_join(aum_value_df) %>%
          suppressMessages()
        node_text <-
          page %>%
          .parse_node_table_to_text(css_node = '#ctl00_ctl00_cphMainContent_cphAdvFormContent_ClientCompensation_ctl00_trIAPDHeader + tr + tr')
        if (node_text[grepl("[[:upper:]]+$", node_text)] %>% unique() %>%
            length() == 1) {
          other_value <-
            node_text[grepl("[[:upper:]]+$", node_text)] %>% unique()
          section_5_data <-
            section_5_data %>%
            mutate(typeOther = other_value) %>%
            dplyr::select(idCRD,
                          nameEntityManager,
                          dplyr::matches("typeOther"),
                          everything())
        }

        if (node_text[grepl("[[:upper:]]+$", node_text)] %>% unique() %>%
            length() > 1) {
          other_value <-
            node_text[grepl("[[:upper:]]+$", node_text)] %>% unique()

          type_df <-
            tibble(nameItem = rep('typeOther', length(other_value)),
                       value = other_value) %>%
            mutate(
              countItem = (1:n()) - 1,
              fullnameItem = if_else(
                countItem > 0,
                nameItem %>% paste0(".", countItem),
                nameItem
              )
            ) %>%
            dplyr::select(fullnameItem, value) %>%
            pivot_wider(names_from = fullnameItem, values_from = value) %>%
            mutate(nameEntityManager = name_entity_manager)
          section_5_data <-
            section_5_data %>%
            left_join(type_df) %>%
            suppressMessages() %>%
            dplyr::select(idCRD,
                          nameEntityManager,
                          dplyr::matches("typeOther"),
                          everything())
        }
        return(section_5_data)

      }

    section_5_data <-
      page %>%
      parse_company_structure_page() %>%
      mutate(nameEntityManager = name_entity_manager) %>%
      .select_start_vars()

    if ('countAccountsNonDiscretionary' %in% names(section_5_data)) {
      section_5_data <-
        section_5_data %>%
        mutate(countAccountsNonDiscretionary = countAccountsTotal - countAccountsDiscretionary)
    }

    section_5_data
  }


.get_section_6_data <-
  function(url = 'https://files.adviserinfo.sec.gov/IAPD/content/viewform/adv/Sections/iapd_AdvOtherBusinessSection.aspx?ORG_PK=156663&FLNG_PK=02D633120008019C05413701015E4D91056C8CC0',
           return_wide = T) {
    idCRD <-
      url %>%
      .get_pk_url_crd()


    page <-
      url %>%
      .get_html_page()

    get_node_item_df <-
      function() {
        items <- c("isBrokerDealer", "isBrokerDealerRep", "isCommodityPoolOperator", "isFuturesMerchant", "isRealEstateBroker", "isInsuranceBroker", "isBank", "isTrustCompany", "isMunicipalAdvisor", "isSwapDealer", "isSwapParticipant", "isAccountingFirm", "isLawFirm", "hasOtherFinancialProducts",
                   "hasOtherBusiness6a.TRUE", "hasNoOtherBusiness6a.TRUE",
                   "isOtherBusinessPrimary.TRUE", "isOtherBusinessPrimary.FALSE",
                   "hasOtherNonFinancialClientServices.TRUE", "hasNoOtherNonFinancialClientServices.TRUE")
        node_item_df <-
          tibble(nameItem = items) %>%
          separate(nameItem,
                   into = c("nameItem", "valueItem"),
                   sep = "\\.") %>%
          separate(nameItem, into = c("nameItem", "valueItem"), sep = "\\.") %>%
          mutate(valueItem = ifelse(is.na(valueItem), T, valueItem) %>% as.logical()) %>%
          suppressWarnings() %>%
          mutate(idRow = 1:n())
        return(node_item_df)
      }

    parse_section_6 <-
      function(page, return_wide)  {
        parse_section_6_check_nodes <-
          function(page, return_wide) {
            check_nodes <-
              page %>%
              html_nodes('.main td img') %>%
              html_attr('alt') %>%
              str_trim()


            node_df <-
              tibble(nodeName = check_nodes) %>%
              left_join(.get_check_box_value_df(), by = "nodeName") %>%
              mutate(idRow = 1:n()) %>%
              left_join(
                get_node_item_df(),
                by = "idRow"
              ) %>%
              filter(isNodeChecked) %>%
              dplyr::select(nameItem, valueItem) %>%
              suppressMessages()

            if (return_wide) {
              column_order <-
                node_df$nameItem

              node_df <-
                node_df %>%
                pivot_wider(names_from = nameItem, values_from = valueItem) %>%
                dplyr::select(any_of(column_order))
            }

            return(node_df)
          }

        name_entity_manager <-
          page %>%
          .get_entity_manager_name()

        section_data <-
          page %>%
          parse_section_6_check_nodes(return_wide = return_wide) %>%
          mutate(nameEntityManager = name_entity_manager)
        return(section_data)

      }

    section_data <-
      page %>%
      parse_section_6(return_wide = return_wide) %>%
      mutate(idCRD) %>%
      .select_start_vars()
    return(section_data)
  }

.get_section_7a_data <-
  function(url = "https://files.adviserinfo.sec.gov/IAPD/content/viewform/adv/Sections/iapd_AdvFinancialAffiliationsSection.aspx?ORG_PK=156663&FLNG_PK=02D633120008019C05413701015E4D91056C8CC0",
           return_wide = T) {
    idCRD <-
      url %>%
      .get_pk_url_crd()

    page <-
      url %>%
      .get_html_page()

    name_entity_manager <-
      page %>%
      .get_entity_manager_name()

    get_node_item_df <-
      function() {
        node_item_df <-
          tibble(
            nameItem = c(
              'isBrokerDealer',
              'isOtherInvestmentAdvisor',
              'isRegisteredMunicipalAdvisor',
              'isRegisteredSwapDealer',
              'isMajorSwapParticipant',
              'isCommodityPoolOperator',
              'isFutureCommissionMerchant',
              'isBankThrift',
              'isTrustCompany',
              'isAccountingFirm',
              'isLawFirm',
              'isInsuranceCompany',
              'isPensionConsultant',
              'isRealEstateBrokerDealer',
              'isNonPooledInvestmentSponsor',
              'isPooledInvestmentSponsor'
            )
          )
        return(node_item_df)
      }

    parse_affliations <-
      function(page, return_wide)  {
        parse_section_7_check_nodes <-
          function(page, return_wide) {
            check_nodes <-
              page %>%
              html_nodes('.main td img') %>%
              html_attr('alt') %>%
              str_trim()
            has_nodes <-
              tibble(nodeName = check_nodes) %>%
              left_join(.get_check_box_value_df()) %>%
              bind_cols(get_node_item_df()) %>%
              mutate(valueItem = T) %>%
              suppressMessages() %>%
              dplyr::filter(isNodeChecked == T) %>% nrow > 0 %>%
              suppressMessages()

            if (has_nodes) {
              affiliation_df <-
                tibble(nodeName = check_nodes) %>%
                left_join(.get_check_box_value_df()) %>%
                bind_cols(get_node_item_df()) %>%
                mutate(valueItem = T) %>%
                dplyr::filter(isNodeChecked == T) %>%
                dplyr::select(nameItem, valueItem) %>%
                suppressMessages()
              if (return_wide) {
                column_order <-
                  affiliation_df$nameItem

                affiliation_df <-
                  affiliation_df %>%
                  pivot_wider(names_from = nameItem, values_from = valueItem) %>%
                  dplyr::select(any_of(column_order))
              }
            } else {
              affiliation_df <-
                tibble(idCRD, nameEntityManager = name_entity_manager)
            }
            return(affiliation_df)
          }
        parse_section_7_check_nodes_safe <-
          possibly(parse_section_7_check_nodes, NULL)
        section_data <-
          page %>%
          parse_section_7_check_nodes(return_wide = return_wide) %>%
          mutate(nameEntityManager = name_entity_manager)

        return(section_data)

      }

    section_data <-
      page %>%
      parse_affliations(return_wide = return_wide) %>%
      mutate(idCRD) %>%
      dplyr::select(idCRD, nameEntityManager, everything())

    return(section_data)
  }

.get_section_7b_data <-
  function(url = 'https://files.adviserinfo.sec.gov/IAPD/content/viewform/adv/Sections/iapd_AdvPrivateFundReportingSection.aspx?ORG_PK=162351&FLNG_PK=052DAAB400080184043CE66005E35E29056C8CC0',
           return_wide = F,
           return_message = T) {
    idCRD <-
      url %>%
      .get_pk_url_crd()


    page <-
      url %>%
      .get_html_page()

    parse_private_fund_data <-
      function(page, return_wide, return_message = T) {
        name_entity_manager <-
          page %>%
          .get_entity_manager_name()

        main <-
          page %>%
          html_nodes('.main div')

        all_div_ids <-
          main %>%
          html_attr('id') %>%
          unique() %>%
          discard(is.na)

        has_fund_data <-
          length(all_div_ids) >= 1

        if (has_fund_data) {
          page_table_nodes <- all_div_ids[all_div_ids %>% str_detect("pnlFund")]
          page_table_nodes <- str_c("#", page_table_nodes)
          fund_count <- length(page_table_nodes)

          page_sequences <-
            seq(3, length.out = fund_count) %>% as.character() %>%
            map_chr(function(x) {
              if (x %>% nchar() == 1) {
                paste0('0', x)
              } else {
                x
              }
            })

          table_css_node_df <-
            tibble(
              numberTable = 1:fund_count,
              pageSequenceNode = page_sequences,
              tableCSSNode = page_table_nodes
            )

          section_matrix_df <-
            .get_section_matrix_df()

          all_data <-
            .parse_funds_tables(
              page = page,
              return_message = return_message,
              table_css_node_df = table_css_node_df,
              section_matrix_df = section_matrix_df
            )

          all_data <-
            all_data %>%
            mutate(nameEntityManager = name_entity_manager) %>%
            dplyr::select(nameEntityManager, everything())

          if (return_message) {
            total_aum <-
              all_data$amountFundGrossAUM %>% sum(na.rm = T) %>% formattable::currency()

            glue::glue("Parsed {name_entity_manager} they have {fund_count} fund vehicles
                       and {total_aum} in private fund AUM") %>% cat(fill = T)
          }

          if (return_wide) {
            all_data <-
              all_data %>%
              dplyr::rename(countItem = numberFund) %>%
              .widen_adv_data() %>%
              .mutate_adv_data()

          }
        } else {
          all_data <-
            tibble(nameEntityManager = name_entity_manager)
        }
        all_data
      }

    parse_private_fund_data_safe <-
      possibly(parse_private_fund_data, NULL)
    section_data <-
      page %>%
      parse_private_fund_data_safe(return_wide = return_wide,
                                   return_message = return_message)

    section_data <-
      section_data %>%
      mutate(idCRD) %>%
      .select_start_vars() %>%
      .munge_fund_names()

    return(section_data)
  }

.get_section_8_data <-
  function(url = 'https://files.adviserinfo.sec.gov/IAPD/content/viewform/adv/Sections/iapd_AdvClientTransSection.aspx?ORG_PK=150510&FLNG_PK=00B175BA0008018601B35551000582C5056C8CC0',
           return_wide = T) {
    idCRD <-
      url %>%
      .get_pk_url_crd()


    page <-
      url %>%
      .get_html_page()

    get_node_item_df <-
      function() {
        node_item_df <-
          c(
            "isSecuritiesBuyerFromClientsForSelfToClientsFromOwned",
            "isSecuritiesFirmBoughSoldClientRecommended",
            "isSecuritiesClientRecommendedFirmOwnedSecurity",
            "hasTradeExecutionClient",
            "isSecuritiesUnderwriterPurchaserManagerClientRecommendedSecurity",
            "hasRecommendedPurchaseSaleFirmOwnedSecurity",
            "hasClientDiscretionBuySell",
            "hasClientDiscretionBuySellAmount",
            "hasClientDiscretionBrokerSelection",
            "hasClientDiscretionCommisionCost",
            "isClientBrokerRelatedParty",
            "hasClientBrokerRecommendation",
            "isClientBrokerRecommenationRelatedParty",
            "isBrokerSoftDollarRecipient",
            "isBrokerSoftDollarEligibleResearchService",
            "hasCompensationForClientReferrals",
            "isCompensatedForClientReferrals"
          ) %>%
          .get_item_name_yes_no_df()
        return(node_item_df)
      }

    parse_section_8 <-
      function(page, return_wide)  {
        name_entity_manager <-
          page %>%
          .get_entity_manager_name()
        parse_section_8_nodes <-
          function(page, return_wide) {
            check_nodes <-
              page %>%
              html_nodes('.main td img') %>%
              html_attr('alt') %>%
              str_trim()

            section_data <-
              tibble(nodeName = check_nodes) %>%
              left_join(.get_check_box_value_df()) %>%
              mutate(idRow = 1:n()) %>%
              left_join(get_node_item_df() %>% mutate(idRow = 1:n()), by = "idRow") %>%
              dplyr::filter(isNodeChecked == T) %>%
              dplyr::filter(!is.na(nameItem)) %>%
              mutate(nameEntityManager = name_entity_manager) %>%
              dplyr::select(nameEntityManager, nameItem, valueItem) %>%
              distinct() %>%
              suppressMessages()

            if (return_wide) {
              column_order <-
                c('nameEntityManager', section_data$nameItem)

              section_data <-
                section_data %>%
                pivot_wider(names_from = nameItem, values_from = valueItem) %>%
                dplyr::select(any_of(column_order))
            }

            return(section_data)
          }
        section_data <-
          page %>%
          parse_section_8_nodes(return_wide = return_wide)

        return(section_data)

      }

    section_data <-
      page %>%
      parse_section_8(return_wide = return_wide) %>%
      mutate(idCRD) %>%
      dplyr::select(idCRD, everything())
    return(section_data)
  }

.get_section_9_data <-
  function(url = 'https://adviserinfo.sec.gov/IAPD/content/viewform/adv/Sections/iapd_AdvCustodySection.aspx?ORG_PK=134600&FLNG_PK=04189D5A00080188013572C10022C501056C8CC0',
           return_wide = T) {
    idCRD <-
      url %>%
      .get_pk_url_crd()

    page <-
      url %>%
      .get_html_page()

    name_entity_manager <-
      page %>%
      .get_entity_manager_name()

    get_node_item_df <-
      function() {
        node_item_df <-
          tibble(
            namefullItem =
              c(
                "hasCustodyClientCash.TRUE",
                "hasCustodyClientCash.FALSE",
                "hasCustodyClientSecurities.TRUE",
                "hasCustodyClientSecurities.FALSE",
                "hasCustodyClientCashRelatdPerson.TRUE",
                "hasCustodyClientCashRelatdPerson.FALSE",
                "hasCustodyClientSecuritiesRelatedPerson.TRUE",
                "hasCustodyClientSecuritiesRelatedPerson.FALSE",
                "hasQuarterlyStatementsFromCustodian",
                "hasAnnualIndependentFundAudit",
                "hasSurpriseIndependentFundAudit",
                "hasControlPersonReport",
                "isQualifiedCustodian.TRUE",
                "isQualifiedCustodian.FALSE",
                "hasRelatedQualifiedCustodian.TRUE",
                "hasRelatedQualifiedCustodian.FALSE"
              ),
            nameItem =
              c(
                "hasCustodyClientCash",
                "hasCustodyClientCash",
                "hasCustodyClientSecurities",
                "hasCustodyClientSecurities",
                "hasCustodyClientCashRelatdPerson",
                "hasCustodyClientCashRelatdPerson",
                "hasCustodyClientSecuritiesRelatedPerson",
                "hasCustodyClientSecuritiesRelatedPerson",
                "hasQuarterlyStatementsFromCustodian",
                "hasAnnualIndependentFundAudit",
                "hasSurpriseIndependentFundAudit",
                "hasControlPersonReport",
                "isQualifiedCustodian",
                "isQualifiedCustodian",
                "hasRelatedQualifiedCustodian",
                "hasRelatedQualifiedCustodian"
              ),
            valueItem =
              c(
                TRUE,
                FALSE,
                TRUE,
                FALSE,
                TRUE,
                FALSE,
                TRUE,
                FALSE,
                TRUE,
                TRUE,
                TRUE,
                TRUE,
                TRUE,
                FALSE,
                TRUE,
                FALSE
              )
          )
        return(node_item_df)
      }

    parse_custody <-
      function(page, return_wide)  {
        parse_custody_nodes <-
          function(page, return_wide) {
            check_nodes <-
              page %>%
              html_nodes('.main td img') %>%
              html_attr('alt') %>%
              str_trim()

            node_df <-
              tibble(nodeName = check_nodes) %>%
              left_join(.get_check_box_value_df()) %>%
              bind_cols(get_node_item_df()) %>%
              mutate(value = T) %>%
              dplyr::filter(isNodeChecked == T) %>%
              mutate(nameEntityManager = name_entity_manager) %>%
              dplyr::select(nameEntityManager, nameItem, value) %>%
              suppressMessages()
            if (return_wide) {
              column_order <-
                c('nameEntityManager', node_df$nameItem)

              node_df <-
                node_df %>%
                pivot_wider(names_from = nameItem, values_from = value) %>%
                dplyr::select(any_of(column_order))
            }
            return(node_df)
          }

        section_data <-
          page %>%
          parse_custody_nodes(return_wide = return_wide)
        return(section_data)

      }

    section_data <-
      page %>%
      parse_custody(return_wide = return_wide) %>%
      mutate(idCRD) %>%
      dplyr::select(idCRD, everything())

    node_text <-
      page %>%
      .parse_node_table_to_text(css_node = '#ctl00_ctl00_cphMainContent_cphAdvFormContent_CustodyPH_ctl00_trIAPDHeader + tr + tr')

    has_nodes <-
      node_text %>% str_count('\\$') %>% sum() > 0

    if (has_nodes) {
      custody_name_df <-
        tibble(
          nameItem = c(
            "amountAUMClientFundsInCustody",
            "countClientFundsInCustody",
            "amountAUMClientFundsInCustodyRelatedParty",
            "countClientFundsInCustodyRelatedParty",
            'countQualifiedCustodians',
            'monthYearIndependentInspection'
          ),
          hit_words =
            c(
              '^custody',
              '^custody',
              '^for which your related persons',
              '^for which your related persons',
              'in connection with advisory services you provide to',
              'during your last fiscal year'
            ),
          off_set = c(4, 6, 5, 7, 2, 1),
          is_numeric_node = c(TRUE, TRUE, TRUE, TRUE, TRUE, F)
        )
      .find_text_node_safe <-
        possibly(.find_text_node, NULL)
      custody_value_df <-
        1:nrow(custody_name_df) %>%
        future_map_dfr(function(x) {
          value_exists <-
            .find_text_node_safe(
              node_text = node_text,
              hit_words = custody_name_df$hit_words[[x]],
              off_set = custody_name_df$off_set[[x]],
              is_numeric_node = custody_name_df$is_numeric_node[[x]]
            ) %>% length() > 0

          if (value_exists) {
            value <-
              .find_text_node_safe(
                node_text = node_text,
                hit_words = custody_name_df$hit_words[[x]],
                off_set = custody_name_df$off_set[[x]],
                is_numeric_node = custody_name_df$is_numeric_node[[x]]
              ) %>%
              as.character() %>%
              suppressWarnings()
          } else {
            value <- NA
          }

          val_df <-
            tibble(nameItem =
                         custody_name_df$nameItem[[x]],
                       value = value)
          return(val_df)
        }) %>%
        distinct() %>%
        dplyr::filter(!value %>% is.na()) %>%
        pivot_wider(names_from = nameItem, values_from = value) %>%
        mutate(nameEntityManager = name_entity_manager) %>%
        .mutate_adv_data() %>%
        dplyr::select(nameEntityManager, everything()) %>%
        suppressWarnings()

      section_data <-
        custody_value_df %>%
        left_join(section_data) %>%
        suppressMessages()
    }
    section_data <-
      section_data %>%
      dplyr::select(idCRD, nameEntityManager, everything())
    return(section_data)
  }

.get_section_10_data <-
  function(url = 'https://files.adviserinfo.sec.gov/IAPD/content/viewform/adv/Sections/iapd_AdvControlPersonsSection.aspx?ORG_PK=142979&FLNG_PK=00AB630A0008018801764C5100236B05056C8CC0') {
    idCRD <-
      url %>%
      .get_pk_url_crd()

    page <-
      url %>%
      .get_html_page()

    get_node_item_df <-
      function() {
        node_item_df <-
          c('hasControlPersonUnnamed') %>%
          .get_item_name_yes_no_df()
        return(node_item_df)
      }

    parse_section_10 <-
      function(page)  {
        parse_section_10_nodes <-
          function(page) {
            check_nodes <-
              page %>%
              html_nodes('.main td img') %>%
              html_attr('alt') %>%
              str_trim()

            section_data <-
              tibble(nodeName = check_nodes) %>%
              left_join(.get_check_box_value_df()) %>%
              bind_cols(get_node_item_df()) %>%
              dplyr::filter(isNodeChecked == T) %>%
              dplyr::select(nameItem, valueItem) %>%
              suppressMessages()

            column_order <-
              section_data$nameItem

            section_data <-
              section_data %>%
              pivot_wider(names_from = nameItem, values_from = valueItem) %>%
              dplyr::select(any_of(column_order))

            return(section_data)
          }

        name_entity_manager <-
          page %>%
          .get_entity_manager_name()

        section_data <-
          page %>%
          parse_section_10_nodes()

        section_data <-
          tibble(nameEntityManager = name_entity_manager) %>%
          bind_cols(list(section_data))

        return(section_data)

      }

    section_data <-
      page %>%
      parse_section_10()

    section_data <-
      tibble(idCRD) %>%
      bind_cols(section_data)

    return(section_data)
  }

.get_section_11_data <-
  function(url = 'https://files.adviserinfo.sec.gov/IAPD/content/viewform/adv/Sections/iapd_AdvDisciplinarySection.aspx?ORG_PK=142979&FLNG_PK=00AB630A0008018801764C5100236B05056C8CC0',
           return_wide = T) {
    idCRD <-
      url %>%
      .get_pk_url_crd()

    page <-
      url %>%
      .get_html_page()

    get_node_item_df <-
      function() {
        node_item_df <-
          c(
            "hasManagementSupervisedPersonEvent",
            "hasFelonyPleaConviction",
            "hasFelonyCharge",
            "hasMisdemeanorPleaConviction",
            "hasMisdemeanrCharge",
            "hasSEC_CFTCFalseStatementOmission",
            "hasSEC_CFTCStatuteViolation",
            "hasSEC_CFTCAuthorizationAction",
            "hasSEC_CFTCOrderAgainst",
            "hasSEC_CFCPenaltyCeaseDesist",
            "hasFederalStateForeignFalseStatement",
            "hasFederalStateForeignInvestmentViolation",
            "hasFederalStateForeignBusinessRevokeSuspended",
            "hasFederalStateForeignOrderAgainst",
            "hasFederalStateForeignLicenseRevoked",
            "hasSelfRegulatedBodyFalseStatement",
            "hasSelfRegulatedBodyRuleViolation",
            "hasSelfRegulatedBodyBusinessRevokeSuspension",
            "hasSelfRegulatedBodyActivityBan",
            "hasAttorneyAccountantFederalContractorPriorBanRevoke",
            "isSubjectToRegulatoryProceeding",
            "hasDomesticForeignCourtEnjoinedInvestmentActivity",
            "hasDomesticForeignCourtGuiltyStatuteViolation",
            "hasDomesticForeignCourtDismissedActionSettlementPursuant",
            "isDomesticForeignCourtSubjectToProceeding"
          ) %>%
          .get_item_name_yes_no_df()
        return(node_item_df)
      }

    parse_section <-
      function(page, return_wide)  {
        name_entity_manager <-
          page %>%
          .get_entity_manager_name()

        parse_section_nodes <-
          function(page) {
            check_nodes <-
              page %>%
              html_nodes('.main td img') %>%
              html_attr('alt') %>%
              str_trim()

            section_data <-
              tibble(nodeName = check_nodes) %>%
              left_join(.get_check_box_value_df()) %>%
              bind_cols(get_node_item_df()) %>%
              dplyr::filter(isNodeChecked == T) %>%
              mutate(nameEntityManager = name_entity_manager) %>%
              dplyr::select(nameEntityManager, nameItem, value = valueItem) %>%
              suppressMessages()

            if (return_wide) {
              col_order <-
                c('nameEntityManager', section_data$nameItem)

              section_data <-
                section_data %>%
                pivot_wider(names_from = nameItem, values_from = value) %>%
                suppressWarnings() %>%
                dplyr::select(any_of(col_order))
            }
            return(section_data)
          }

        section_data <-
          page %>%
          parse_section_nodes()

        return(section_data)

      }

    section_data <-
      page %>%
      parse_section(return_wide = return_wide) %>%
      mutate(idCRD) %>%
      dplyr::select(idCRD, everything())

    return(section_data)
  }

.get_section_12_data <-
  function(url = 'https://files.adviserinfo.sec.gov/IAPD/content/viewform/adv/Sections/iapd_AdvSmallBusinessSection.aspx?ORG_PK=150510&FLNG_PK=00B175BA0008018601B35551000582C5056C8CC0') {
    idCRD <-
      url %>%
      .get_pk_url_crd()

    page <-
      url %>%
      .get_html_page()

    get_node_item_df <-
      function() {
        node_item_df <-
          c(
            'hasAssetsOver5mFiscalYearEnd',
            'hasControlRelatedAssetManagerAssetsOver25mFiscalYearEnd',
            'hasControlRelatedAssetManagerAssetsOver5mFiscalYearEnd',
            'isControlledByAssetManagerAssetsOver25mFiscalYearEnd',
            'isControlledByAssetManagerAssetsOver5mFiscalYearEnd'
          ) %>%
          .get_item_name_yes_no_df()
        return(node_item_df)
      }

    parse_section <-
      function(page)  {
        parse_section_nodes <-
          function(page) {
            name_entity_manager <-
              page %>%
              .get_entity_manager_name()

            check_nodes <-
              page %>%
              html_nodes('.main td img') %>%
              html_attr('alt') %>%
              str_trim()

            has_nodes <-
              tibble(nodeName = check_nodes) %>%
              left_join(.get_check_box_value_df()) %>%
              suppressMessages() %>%
              bind_cols(get_node_item_df()) %>%
              dplyr::filter(isNodeChecked == T) %>% nrow > 0


            if (has_nodes) {
              section_data <-
                tibble(nodeName = check_nodes) %>%
                left_join(.get_check_box_value_df()) %>%
                bind_cols(get_node_item_df()) %>%
                dplyr::filter(isNodeChecked == T) %>%
                dplyr::select(nameItem, valueItem) %>%
                dplyr::select(nameEntityManager, everything) %>%
                mutate(nameEntityManager = name_entity_manager) %>%
                dplyr::rename(value = valueItem) %>%
                group_by(nameItem) %>%
                mutate(countItem = 1:n()) %>%
                ungroup() %>%
                suppressMessages() %>%
                mutate(
                  countItem = countItem - 1,
                  countItem = countItem %>% as.character(),
                  countItem = ifelse(countItem == "0", '', countItem)
                ) %>%
                unite(item, nameItem, countItem, sep = '') %>%
                distinct() %>%
                suppressWarnings()

              col_order <-
                c('nameEntityManager', section_data$item)

              section_data <-
                section_data %>%
                pivot_wider(names_from = item, values_from = value) %>%
                dplyr::select(any_of(col_order))
            } else {
              section_data <-
                tibble(nameEntityManager = name_entity_manager)
            }

            return(section_data)
          }

        section_data <-
          page %>%
          parse_section_nodes() %>%
          mutate(idCRD) %>%
          dplyr::select(idCRD, everything())


        return(section_data)

      }

    section_data <-
      page %>%
      parse_section() %>%
      mutate(idCRD) %>%
      dplyr::select(idCRD, everything())

    return(section_data)
  }

.get_schedule_a_data <-
  function(url = 'https://files.adviserinfo.sec.gov/IAPD/content/viewform/adv/Sections/iapd_AdvScheduleASection.aspx?ORG_PK=150510&FLNG_PK=00B175BA0008018601B35551000582C5056C8CC0',
           return_wide = F) {
    idCRD <-
      url %>%
      .get_pk_url_crd()

    page <-
      url %>%
      .get_html_page()

    parse_section <-
      function(page)  {
        name_entity_manager <-
          page %>%
          .get_entity_manager_name()

        table_exists <-
          page %>% html_nodes(css = '#ctl00_ctl00_cphMainContent_cphAdvFormContent_ScheduleAPHSection_ctl00_ownersGrid') %>%
          length() > 0

        if (table_exists) {
          table_data <-
            page %>%
            html_nodes(css = '#ctl00_ctl00_cphMainContent_cphAdvFormContent_ScheduleAPHSection_ctl00_ownersGrid') %>%
            html_table(fill = T, header = F) %>%
            data.frame() %>%
            as_tibble() %>%
            slice(-1) %>%
            mutate(X1 = X1 %>% str_to_upper())

          names(table_data) <-
            c(
              'nameEntityManagerOwner',
              'idTypeEntityManagerOwner',
              'statusEntityManagerOwner',
              'monthYearEntityManagerOwnerPurchased',
              'idRangeManagerEntityOwnership',
              'isControlPerson',
              'isPublicReportingEntity',
              'idEntityManagerOwner'
            )

          table_data <-
            table_data %>%
            mutate(
              idCRD,
              isControlPerson = if_else(isControlPerson == "Y", TRUE, FALSE),
              isPublicReportingEntity = if_else(isPublicReportingEntity == "Y", TRUE, FALSE),
              dateEntityManagerOwnerPurchased = '01/' %>% paste0(monthYearEntityManagerOwnerPurchased) %>% lubridate::dmy() %>% as.Date
            ) %>%
            left_join(.get_type_manager_entity_owner_df()) %>%
            left_join(.get_range_entity_owner_df()) %>%
            dplyr::select(-monthYearEntityManagerOwnerPurchased) %>%
            suppressMessages()
          has_individual_owners <-
            table_data %>%
            dplyr::filter(idTypeEntityManagerOwner == "I") %>% nrow > 0
          if (has_individual_owners) {
            individual_data <-
              table_data %>%
              dplyr::filter(idTypeEntityManagerOwner == "I")

            individual_data <-
              individual_data$nameEntityManagerOwner %>%
              future_map_dfr(.parse_manager_owner_name) %>%
              right_join(individual_data) %>%
              suppressMessages() %>%
              mutate(
                countDash = idEntityManagerOwner %>% str_count('\\-'),
                typeIDEntityManagerOwner = if_else(countDash == 0, 'idEmployee', 'idSSN')
              ) %>%
              dplyr::select(-countDash)
          }

          has_entity_owners <-
            table_data %>% dplyr::filter(isEntityOwnerManagerEntity == T) %>% nrow > 0
          if (has_entity_owners) {
            entity_df <-
              table_data %>% dplyr::filter(isEntityOwnerManagerEntity == T) %>%
              mutate(
                countDash = idEntityManagerOwner %>% str_count('\\-'),
                typeIDEntityManagerOwner = if_else(countDash == 0, 'idCRD', 'idEIN')
              ) %>%
              dplyr::select(-countDash)
            if ('individual_data' %>% exists) {
              table_data <-
                individual_data %>%
                bind_rows(entity_df) %>%
                mutate(
                  nameCommonEntityOwnerManager = if_else(
                    nameCommonEntityOwnerManager %>% is.na,
                    nameEntityManagerOwner,
                    nameCommonEntityOwnerManager
                  ),
                  nameEntityManagerOwner = if_else(
                    nameFullEntityOwnerManager %>% is.na,
                    nameEntityManagerOwner,
                    nameFullEntityOwnerManager
                  )
                ) %>%
                suppressMessages() %>%
                dplyr::select(idCRD, everything())
            } else {
              table_data <-
                entity_df %>%
                mutate(
                  nameFullEntityOwnerManager = nameEntityManagerOwner,
                  nameCommonEntityOwnerManager = nameEntityManagerOwner
                )
            }
          } else {
            table_data <-
              individual_data %>%
              mutate(
                nameCommonEntityOwnerManager = if_else(
                  nameCommonEntityOwnerManager %>% is.na,
                  nameEntityManagerOwner,
                  nameCommonEntityOwnerManager
                ),
                nameEntityManagerOwner = if_else(
                  nameFullEntityOwnerManager %>% is.na,
                  nameFullEntityOwnerManager,
                  nameFullEntityOwnerManager
                )
              ) %>%
              dplyr::select(idCRD, everything())
          }

          table_data <-
            table_data %>%
            mutate(
              nameFullEntityOwnerManager = if_else(
                nameFullEntityOwnerManager %>% is.na,
                nameEntityManagerOwner,
                nameFullEntityOwnerManager
              )
            )

          table_data <-
            table_data %>%
            mutate(nameEntityManager = name_entity_manager) %>%
            dplyr::select(
              nameEntityManager,
              nameCommonEntityOwnerManager,
              statusEntityManagerOwner,
              isEntityOwnerManagerEntity,
              dateEntityManagerOwnerPurchased,
              rangeManagerEntityOwnership,
              idEntityManagerOwner,
              typeEntityManagerOwner,
              isControlPerson,
              idTypeEntityManagerOwner:typeIDEntityManagerOwner,
              everything()
            )
        } else {
          table_data <-
            tibble(nameEntityManager = name_entity_manager)
        }
        return(table_data)
      }

    section_data <-
      page %>%
      parse_section()

    section_data <-
      section_data %>%
      mutate(idCRD) %>%
      dplyr::select(idCRD, everything())

    if (return_wide) {
      section_data <-
        section_data %>%
        mutate(across(everything(), as.character)) %>%
        mutate(countItem = 1:n(),
               idCRD = idCRD %>% as.numeric()) %>%
        pivot_longer(cols = -c(nameEntityManager, countItem, idCRD),
                     names_to = "item",
                     values_to = "value") %>%
        mutate(
          countItem = countItem - 1,
          countItem = countItem %>% as.character(),
          countItem = ifelse(countItem == "0", '', countItem)
        ) %>%
        unite(item, item, countItem, sep = '') %>%
        dplyr::filter(!value %>% is.na()) %>%
        suppressWarnings()

      column_order <-
        c('idCRD', 'nameEntityManager', section_data$item)

      section_data <-
        section_data %>%
        pivot_wider(names_from = item, values_from = value) %>%
        dplyr::select(any_of(column_order))

      section_data <-
        section_data %>%
        mutate(across(matches("^amount|^count"), as.numeric)) %>%
        mutate(across(matches("^has|^is"), as.logical)) %>%
        mutate(across(matches("^date"), ~lubridate::ymd(.)))
    }

    return(section_data %>% distinct())
  }

.get_schedule_b_data <-
  function(url = 'https://files.adviserinfo.sec.gov/IAPD/content/viewform/adv/Sections/iapd_AdvScheduleBSection.aspx?ORG_PK=142979&FLNG_PK=00AB630A0008018801764C5100236B05056C8CC0',
           return_wide = F) {
    idCRD <-
      url %>%
      .get_pk_url_crd()
    page <-
      url %>%
      .get_html_page()

    parse_section <-
      function(page)  {
        name_entity_manager <-
          page %>%
          .get_entity_manager_name()

        table_exists <-
          page %>% html_nodes(css = '#ctl00_ctl00_cphMainContent_cphAdvFormContent_ScheduleBPHSection_ctl00_ownersGrid') %>%
          length() > 0

        if (table_exists) {
          table_data <-
            page %>%
            html_nodes(css = '#ctl00_ctl00_cphMainContent_cphAdvFormContent_ScheduleBPHSection_ctl00_ownersGrid') %>%
            html_table(fill = T, header = F) %>%
            data.frame() %>%
            as_tibble() %>%
            slice(-1) %>%
            mutate(X1 = X1 %>% str_to_upper())

          names(table_data) <-
            c(
              'nameEntityManagerOwnerOwner',
              'idTypeEntityManagerOwnerOwner',
              'nameEntityManagerOwner',
              'statusEntityManagerOwnerOwner',
              'monthYearEntityManagerOwnerOwnerPurchased',
              'idRangeManagerEntityOwnerOwnership',
              'isOwnerOwnerControlPerson',
              'isOwnerOwnerPublicReportingEntity',
              'idEntityManagerOwnerOwner'
            )

          table_data <-
            table_data %>%
            mutate(
              idCRD,
              isOwnerOwnerControlPerson = if_else(isOwnerOwnerControlPerson == "Y", TRUE, FALSE),
              isOwnerOwnerPublicReportingEntity = if_else(isOwnerOwnerPublicReportingEntity == "Y", TRUE, FALSE),
              dateEntityManagerOwnerOwnerPurchased = '01/' %>% paste0(monthYearEntityManagerOwnerOwnerPurchased) %>% lubridate::dmy() %>% as.Date
            ) %>%
            left_join(
              .get_type_manager_entity_owner_df() %>%
                dplyr::rename(
                  idTypeEntityManagerOwnerOwner = idTypeEntityManagerOwner,
                  typeEntityManagerOwnerOwner = typeEntityManagerOwner,
                  isEntityOwnerOwnerManagerEntity = isEntityOwnerManagerEntity
                )
            ) %>%
            left_join(
              .get_range_entity_owner_df() %>% dplyr::rename(
                idRangeManagerEntityOwnerOwnership = idRangeManagerEntityOwnership,
                rangeManagerEntityOwnerOwnership = rangeManagerEntityOwnership
              )
            ) %>%
            dplyr::select(-monthYearEntityManagerOwnerOwnerPurchased) %>%
            suppressMessages()

          has_individual_data <-
            table_data %>%
            dplyr::filter(idTypeEntityManagerOwnerOwner == "I") %>% nrow > 0

          if (has_individual_data) {
            has_entity_df <-
              table_data %>% dplyr::filter(isEntityOwnerOwnerManagerEntity == T) %>% nrow > 0
            if (has_entity_df) {
              entity_df <-
                table_data %>% dplyr::filter(isEntityOwnerOwnerManagerEntity == T)

              entity_df <-
                entity_df %>%
                mutate(
                  countDash = idEntityManagerOwnerOwner %>% str_count('\\-'),
                  typeIDEntityManagerOwner = if_else(countDash == 0, 'idCRD', 'idEIN')
                ) %>%
                dplyr::select(-countDash)

              individual_data <-
                table_data %>%
                dplyr::filter(idTypeEntityManagerOwnerOwner == "I")

              individual_data <-
                individual_data$nameEntityManagerOwnerOwner %>%
                future_map_dfr(.parse_manager_owner_name) %>%
                dplyr::rename(
                  nameEntityManagerOwnerOwner = nameEntityManagerOwner,
                  nameCommonEntityOwnerOwnerManager = nameCommonEntityOwnerManager,
                  nameFullEntityManagerOwnerOwner = nameFullEntityOwnerManager,
                  nameFirstEntityManagerOwnerOwner = nameFirstEntityManagerOwner,
                  nameMiddleEntityManagerOwnerOwner = nameMiddleEntityManagerOwner,
                  nameLastEntityManagerOwnerOwner = nameLastEntityManagerOwner
                ) %>%
                right_join(individual_data) %>%
                suppressMessages() %>%
                mutate(
                  countDash = idEntityManagerOwnerOwner %>% str_count('\\-'),
                  typeIDEntityManagerOwnerOwner = if_else(countDash == 0, 'idEmployee', 'idSSN')
                ) %>%
                dplyr::select(-countDash)

              table_data <-
                individual_data %>%
                bind_rows(entity_df) %>%
                mutate(
                  nameCommonEntityOwnerOwnerManager = if_else(
                    nameCommonEntityOwnerOwnerManager %>% is.na,
                    nameEntityManagerOwnerOwner,
                    nameCommonEntityOwnerOwnerManager
                  ),
                  nameEntityManagerOwnerOwner = if_else(
                    nameFullEntityManagerOwnerOwner %>% is.na,
                    nameEntityManagerOwnerOwner,
                    nameFullEntityManagerOwnerOwner
                  )
                ) %>%
                suppressMessages() %>%
                mutate(
                  countDash = idEntityManagerOwnerOwner %>% str_count('\\-'),
                  typeIDEntityManagerOwnerOwner = if_else(countDash == 0, 'idEmployee', 'idSSN')
                ) %>%
                dplyr::select(-countDash) %>%
                dplyr::select(idCRD, everything())
            } else {
              individual_data <-
                table_data %>%
                dplyr::filter(idTypeEntityManagerOwnerOwner == "I")

              individual_data <-
                individual_data$nameEntityManagerOwnerOwner %>%
                future_map_dfr(.parse_manager_owner_name) %>%
                dplyr::rename(
                  nameEntityManagerOwnerOwner = nameEntityManagerOwner,
                  nameCommonEntityOwnerOwnerManager = nameCommonEntityOwnerManager,
                  nameFullEntityManagerOwnerOwner = nameFullEntityOwnerManager,
                  nameFirstEntityManagerOwnerOwner = nameFirstEntityManagerOwner,
                  nameMiddleEntityManagerOwnerOwner = nameMiddleEntityManagerOwner,
                  nameLastEntityManagerOwnerOwner = nameLastEntityManagerOwner
                ) %>%
                right_join(individual_data) %>%
                suppressMessages() %>%
                mutate(
                  countDash = idEntityManagerOwnerOwner %>% str_count('\\-'),
                  typeIDEntityManagerOwnerOwner = if_else(countDash == 0, 'idEmployee', 'idSSN')
                ) %>%
                dplyr::select(-countDash)

              table_data <-
                individual_data %>%
                mutate(
                  nameCommonEntityOwnerOwnerManager = if_else(
                    nameCommonEntityOwnerOwnerManager %>% is.na,
                    nameEntityManagerOwnerOwner,
                    nameCommonEntityOwnerOwnerManager
                  ),
                  nameEntityManagerOwnerOwner = if_else(
                    nameFullEntityManagerOwnerOwner %>% is.na,
                    nameEntityManagerOwnerOwner,
                    nameFullEntityManagerOwnerOwner
                  )
                ) %>%
                suppressMessages() %>%
                mutate(
                  countDash = idEntityManagerOwnerOwner %>% str_count('\\-'),
                  typeIDEntityManagerOwnerOwner = if_else(countDash == 0, 'idEmployee', 'idSSN')
                ) %>%
                dplyr::select(-countDash) %>%
                dplyr::select(idCRD, everything())
            }
          } else {
            table_data <-
              table_data %>%
              mutate(
                countDash = idEntityManagerOwnerOwner %>% str_count('\\-'),
                typeIDEntityManagerOwnerOwner = if_else(countDash == 0, 'idCRD', 'idEIN')
              ) %>%
              dplyr::select(-countDash) %>%
              dplyr::select(idCRD, everything())
          }


          if ('nameFullEntityOwnerOwnerManager' %in% names(table_data)) {
            table_data <-
              table_data %>%
              mutate(
                nameFullEntityOwnerOwnerManager = if_else(
                  nameFullEntityOwnerOwnerManager %>% is.na,
                  nameEntityManagerOwnerOwner,
                  nameFullEntityOwnerOwnerManager
                )
              )
          } else {
            table_data <-
              table_data %>%
              mutate(nameFullEntityOwnerOwnerManager = nameEntityManagerOwnerOwner)
          }

          if ('nameCommonEntityOwnerOwnerManager' %in% names(table_data)) {
            table_data <-
              table_data %>%
              mutate(
                nameCommonEntityOwnerOwnerManager = if_else(
                  nameCommonEntityOwnerOwnerManager %>% is.na,
                  nameEntityManagerOwner,
                  nameCommonEntityOwnerOwnerManager
                )
              )
          } else {
            table_data <-
              table_data %>%
              mutate(nameCommonEntityOwnerOwnerManager = nameEntityManagerOwnerOwner)
          }

          table_data <-
            table_data %>%
            mutate(nameEntityManager = name_entity_manager) %>%
            dplyr::select(
              nameEntityManager,
              nameEntityManagerOwnerOwner,
              statusEntityManagerOwnerOwner,
              nameEntityManagerOwner,
              isEntityOwnerOwnerManagerEntity,
              dateEntityManagerOwnerOwnerPurchased,
              rangeManagerEntityOwnerOwnership,
              idEntityManagerOwnerOwner,
              typeEntityManagerOwnerOwner,
              isOwnerOwnerControlPerson,
              everything()
            )
        } else {
          table_data <-
            tibble(nameEntityManager = name_entity_manager)
        }
        return(table_data)
      }

    section_data <-
      page %>%
      parse_section()

    section_data <-
      section_data %>%
      mutate(idCRD) %>%
      dplyr::select(idCRD, everything())

    if (return_wide) {
      section_data <-
        section_data %>%
        mutate(across(everything(), as.character)) %>%
        mutate(countItem = 1:n(),
               idCRD = idCRD %>% as.numeric()) %>%
        pivot_longer(cols = -c(nameEntityManager, countItem, idCRD),
                     names_to = "item",
                     values_to = "value") %>%
        mutate(
          countItem = countItem - 1,
          countItem = countItem %>% as.character(),
          countItem = ifelse(countItem == "0", '', countItem)
        ) %>%
        unite(item, item, countItem, sep = '') %>%
        dplyr::filter(!value %>% is.na()) %>%
        suppressWarnings()

      column_order <-
        c('idCRD', 'nameEntityManager', section_data$item)

      section_data <-
        section_data %>%
        pivot_wider(names_from = item, values_from = value) %>%
        dplyr::select(any_of(column_order))

      section_data <-
        section_data %>%
        mutate(across(matches("^amount|^count"), as.numeric)) %>%
        mutate(across(matches("^has|^is"), as.logical)) %>%
        mutate(across(matches("^date"), ~lubridate::ymd(.)))
    }

    return(section_data %>% distinct())
  }

.get_schedule_d_data <-
  function(url = 'https://files.adviserinfo.sec.gov/IAPD/content/viewform/adv/Sections/iapd_AdvScheduleDSection.aspx?ORG_PK=284340&FLNG_PK=0573726A0008018802C736510026C985056C8CC0',
           join_data = F,
           return_wide = F) {
    idCRD <-
      url %>%
      .get_pk_url_crd()

    page <-
      url %>%
      .get_html_page()

    name_entity_manager <-
      page %>% .get_entity_manager_name()

    parse_schedule_d <-
      function(page, join_data, return_wide)  {
        table_ids <-
          page %>%
          html_nodes('.main div') %>%
          html_attr('id')

        table_ids <-
          table_ids[!table_ids %>% is.na()]

        table_ids <-
          table_ids[table_ids %>% str_detect('PFID|pageMessages|Lookup|Header') == F]

        table_node_df <-
          tibble(idCSSTable = table_ids %>% paste0('#', .)) %>%
          mutate(idTable = 1:n())

        table_nodes <-
          page %>%
          html_nodes('.flatBorderTable tr td .PaperFormTableData')

        all_table_node_df <-
          seq_along(table_nodes) %>%
          future_map_dfr(function(x) {
            raw_nodes <-
              table_nodes[[x]] %>%
              html_text(trim = T) %>%
              str_replace_all('\r|\n|\t', '') %>%
              stri_replace_all_charclass("\\p{WHITE_SPACE}", " ") %>%
              stri_trim_both() %>%
              gsub("^\\s+|\\s+$", "", .) %>%
              str_split('\\  ') %>%
              flatten_chr() %>%
              str_trim()

            raw_nodes <-
              raw_nodes[!raw_nodes == '']

            if (raw_nodes %>% length() > 0) {
              tibble(idTable = x, nodeText = raw_nodes)
            }
          }) %>%
          mutate(idRow = 1:n())

        parse_other_office_locations <-
          function(page, return_wide) {
            table_nodes <-
              page %>%
              html_nodes('.flatBorderTable tr td .PaperFormTableData')

            end_table_df <-
              seq_along(table_nodes) %>%
              future_map_dfr(function(x) {
                raw_nodes <-
                  table_nodes[[x]] %>%
                  html_text(trim = T) %>%
                  str_replace_all('\r|\n|\t', '') %>%
                  stri_replace_all_charclass("\\p{WHITE_SPACE}", " ") %>%
                  stri_trim_both() %>%
                  gsub("^\\s+|\\s+$", "", .) %>%
                  str_split('\\  ') %>%
                  flatten_chr()

                raw_nodes <-
                  raw_nodes[!raw_nodes == '']

                is_end_table <-
                  raw_nodes %>% grep("A. PRIVATE FUND|SECTION 6", .) %>%
                  length() > 0
                tibble(
                  idTable = x,
                  isEndTable = is_end_table,
                  nodesText = raw_nodes
                )
              })

            has_end_table <-
              end_table_df %>% dplyr::filter(isEndTable == T) %>% nrow > 0
            if (has_end_table) {
              end_table_no <-
                end_table_df %>%
                dplyr::filter(isEndTable == T) %>%
                slice(1) %>%
                .$idTable - 1
            } else {
              end_table_no <-
                table_nodes %>% length
            }

            location_df <-
              1:(end_table_no) %>%
              future_map_dfr(function(x) {
                raw_nodes <-
                  table_nodes[[x]] %>%
                  html_text(trim = T) %>%
                  str_replace_all('\r|\n|\t', '') %>%
                  stri_replace_all_charclass("\\p{WHITE_SPACE}", " ") %>%
                  stri_trim_both() %>%
                  gsub("^\\s+|\\s+$", "", .) %>%
                  str_split('\\  ') %>%
                  flatten_chr()

                raw_nodes <-
                  raw_nodes[!raw_nodes == '']

                raw_nodes <-
                  raw_nodes[raw_nodes %>% str_detect(":[A-Z a-z 1-9]")] %>% str_trim()
                has_nodes <-
                  raw_nodes %>% length() > 0
                if (has_nodes) {
                  tibble(idTable = x, itemvalueNode = raw_nodes)
                }
              }) %>%
              dplyr::filter(
                !itemvalueNode %>% str_detect(
                  'Name of entity where books|Facis|Facsimile|NAME OF THE INDEPENDENT|SUCH NON-US FUNDS|ITEM|item|Item|SCHEDULE|NVESTMENT ACQUISITION|THE ANALYSIS AND MANAGEMEN|MAIL STOP'
                )
              ) %>%
              dplyr::filter(!itemvalueNode %>% str_detect('NOTE: |Name of the')) %>%
              dplyr::filter(itemvalueNode %>% str_detect('\\:'))
            name_entity_manager <-
              page %>%
              .get_entity_manager_name()
            has_other_locations <-
              location_df %>% nrow > 0
            if (has_other_locations) {
              if (location_df$idTable %>% unique() %>% min() > 1) {
                offset_value <-
                  location_df$idTable %>% unique() %>% min() - 1

                location_df <-
                  location_df %>%
                  mutate(idTable = idTable - offset_value)
              }
              all_locations <-
                location_df$idTable %>% unique() %>%
                future_map_dfr(function(x) {
                  addressOfficeSecondary <-
                    location_df %>%
                    dplyr::filter(idTable == x)

                  addressOfficeSecondary %>%
                    separate(itemvalueNode, c('itemNode', 'valueNode'), '\\:') %>%
                    left_join(.get_location_name_df()) %>%
                    dplyr::select(idTable, nameNode, valueNode) %>%
                    mutate(
                      valueNode = valueNode %>% str_trim() %>% str_to_upper(),
                      nameNode = nameNode %>% paste0('ManagerOfficeSecondary')
                    ) %>%
                    suppressMessages()
                }) %>%
                dplyr::rename(countItem = idTable,
                              item = nameNode,
                              value = valueNode) %>%
                dplyr::filter(!value %>% is.na()) %>%
                mutate(nameEntityManager = name_entity_manager) %>%
                dplyr::select(nameEntityManager, everything()) %>%
                arrange(countItem) %>%
                pivot_wider(names_from = item, values_from = value)

              if (names(all_locations) %>% str_count('^address|^city|^country|^state') %>% sum() >= 4) {
                if (names(all_locations) %>% str_count(
                  'stateManagerOfficeSecondary|zipManagerOfficeSecondary|addressStreet1ManagerOfficeSecondary|cityManagerOfficeSecondary'
                ) %>% sum() == 4) {
                  all_locations <-
                    all_locations %>%
                    mutate(
                      locationSecondary = addressStreet1ManagerOfficeSecondary %>% paste0(
                        ' ',
                        cityManagerOfficeSecondary,
                        ', ',
                        stateManagerOfficeSecondary,
                        ' ',
                        countryManagerOfficeSecondary,
                        ' ',
                        zipManagerOfficeSecondary
                      )
                    )
                }
              }

              if (return_wide) {
                all_locations <-
                  all_locations %>%
                  .widen_adv_data()
              }
            } else {
              all_locations <-
                tibble(nameEntityManager = name_entity_manager)
            }
            return(all_locations)
          }

        parse_for_manager_website_data <-
          function(page) {
            nodes <-
              page %>%
              .get_html_node_text('.PrintHistRed') %>%
              str_to_lower

            name_entity_manager <-
              page %>% .get_entity_manager_name()

            if (nodes %>% grep('http', .) %>% length() > 0) {
              urlManager <-
                nodes[nodes %>% grep('http', .)]

              url_df <-
                tibble(urlManager) %>%
                mutate(countItem = 1:n()) %>%
                pivot_longer(cols = -countItem, names_to = "item", values_to = "value") %>%
                dplyr::filter(!value %>% is.na()) %>%
                pivot_wider(names_from = item, values_from = value) %>%
                mutate(nameEntityManager = name_entity_manager) %>%
                dplyr::select(nameEntityManager, everything())
              rm(nodes)

              url_df <-
                url_df %>%
                .widen_adv_data()
            } else {
              url_df <-
                url_df <-
                tibble(nameEntityManager = name_entity_manager)
            }
            return(url_df)
          }

        parse_related_advisor <-
          function(page, return_wide) {
            name_entity_manager <-
              page %>%
              .get_entity_manager_name()

            page_input_ids <-
              page %>%
              html_nodes('input') %>%
              html_attr('id')
            has_adviser_nodes <-
              page_input_ids %>% str_count(
                'ctl00_ctl00_cphMainContent_cphAdvFormContent_AffiliatedAdvisersList_rptrAfflAdvisers_'
              ) %>% sum() > 0
            if (has_adviser_nodes) {
              node_id_value <-
                page_input_ids[page_input_ids %>% str_detect(
                  'ctl00_ctl00_cphMainContent_cphAdvFormContent_AffiliatedAdvisersList_rptrAfflAdvisers_'
                )] %>%
                paste0('#', ., ' + table')

              related_adviser_node_df <-
                tibble(cssNode = node_id_value) %>%
                mutate(idTable = 1:n()) %>%
                dplyr::select(idTable, everything())

              related_advisor_df <-
                related_adviser_node_df$idTable %>%
                future_map_dfr(function(x) {
                  table_node <-
                    page %>%
                    html_nodes(css = related_adviser_node_df$cssNode[x])

                  # Extract values from span.PrintHistRed elements (SEC Angular SPA format)
                  red_span_values <-
                    table_node %>%
                    html_nodes('span.PrintHistRed') %>%
                    html_text(trim = TRUE) %>%
                    str_trim()

                  # First red span is Legal Name, second is Business Name
                  nameLegalRelatedEntity <- if (length(red_span_values) >= 1) red_span_values[1] else NA_character_
                  nameBusinessRelatedEntity <- if (length(red_span_values) >= 2) red_span_values[2] else NA_character_

                  # Extract CRD from third span if present and numeric
                  idCRDRelatedEntity <- NA_real_
                  if (length(red_span_values) >= 3) {
                    crd_candidate <- red_span_values[3] %>% str_trim()
                    if (str_detect(crd_candidate, "^\\d+$")) {
                      idCRDRelatedEntity <- as.numeric(crd_candidate)
                    }
                  }

                  # Fallback to old method if red spans not found
                  if (is.na(nameLegalRelatedEntity) || nameLegalRelatedEntity == "") {
                    table_nodes <-
                      table_node %>%
                      html_text(trim = T) %>%
                      str_replace_all('\r|\n|\t', '') %>%
                      stri_replace_all_charclass("\\p{WHITE_SPACE}", " ") %>%
                      stri_trim_both() %>%
                      gsub("^\\s+|\\s+$", "", .) %>%
                      str_split("   +") %>%
                      flatten_chr()

                    nameLegalRelatedEntity <-
                      table_nodes[table_nodes %>% grep('Legal Name of', .) + 1] %>% str_replace_all('Related Person: ', '')

                    nameBusinessRelatedEntity <-
                      table_nodes[table_nodes %>% grep('Primary Business Name of', .) + 2]

                    if (table_nodes %>% grep('CRD Number', .) %>% length() > 0) {
                      idCRDRelatedEntity <-
                        table_nodes[table_nodes %>% grep('CRD Number', .)] %>% gsub('\\CRD Number', '', .) %>%
                        gsub('\\(|\\)', '', .) %>% gsub(' if any:', '', .)

                      if (idCRDRelatedEntity == '') {
                        idCRDRelatedEntity <- NA
                      } else {
                        idCRDRelatedEntity <- idCRDRelatedEntity %>% str_trim() %>% as.numeric()
                      }
                    }
                  }

                  data_df <-
                    tibble(
                      idTable = x,
                      nameBusinessRelatedEntity,
                      nameLegalRelatedEntity,
                      idCRDRelatedEntity
                    ) %>%
                    mutate(
                      nameBusinessRelatedEntity = if_else(
                        nameBusinessRelatedEntity == "SAME",
                        nameLegalRelatedEntity,
                        nameBusinessRelatedEntity
                      ),
                      idCRDRelatedEntity = idCRDRelatedEntity %>% as.numeric()
                    )
                  has_image_check_box <-
                    page %>%
                    html_nodes(related_adviser_node_df$cssNode[x]) %>%
                    html_nodes('img') %>% length() == 33
                  if (has_image_check_box) {
                    check_nodes <-
                      page %>%
                      html_nodes(related_adviser_node_df$cssNode[x]) %>%
                      html_nodes('img') %>%
                      html_attr('alt') %>%
                      str_trim()

                    node_df <-
                      tibble(
                        nameItem = c(
                          'isBrokerDealer',
                          'isOtherInvestmentAdvisor',
                          'isRegisteredMunicipalAdvisor',
                          'isRegisteredSwapDealer',
                          'isMajorSwapParticipant',
                          'isCommodityPoolOperator',
                          'isFutureCommissionMerchant',
                          'isBankThrift',
                          'isTrustCompany',
                          'isAccountingFirm',
                          'isLawFirm',
                          'isInsuranceCompany',
                          'isPensionConsultant',
                          'isRealEstateBrokerDealer',
                          'isNonPooledInvestmentSponsor',
                          'isPooledInvestmentSponsor'
                        ),
                        valueItem = T
                      ) %>%
                      mutate(fullnameItem = nameItem) %>%
                      bind_rows(
                        list(
                          c(
                            'isRelatedPersonControlled',
                            'hasRelatedPersonCommonControl',
                            'isRelatedPersonClientCustodian',
                            'isRelatedPersonNotOperationIndependent'
                          ) %>%
                            .get_item_name_yes_no_df(),
                          'isPrivateResidence' %>%
                            .has_item_check_name(),
                          c(
                            'isRelatedPersonExemptInvestmentAdviser',
                            'isRelatedPersonForeignEntityRegistered',
                            'hasRelatedPersonSharedSupervisedPerson',
                            'hasRelatedPersonSharedAddress'
                          ) %>%
                            .get_item_name_yes_no_df()
                        )
                      )

                    box_df <-
                      tibble(nodeName = check_nodes) %>%
                      mutate(idTable = x) %>%
                      left_join(.get_check_box_value_df()) %>%
                      bind_cols(node_df) %>%
                      dplyr::filter(isNodeChecked == T) %>%
                      dplyr::select(idTable, nameItem, valueItem) %>%
                      suppressMessages() %>%
                      pivot_wider(names_from = nameItem, values_from = valueItem)

                    data_df <-
                      data_df %>%
                      left_join(box_df) %>%
                      suppressMessages()
                  }

                  return(data_df)
                }) %>%
                dplyr::rename(countItem = idTable) %>%
                pivot_longer(cols = -countItem, names_to = "item", values_to = "value") %>%
                dplyr::filter(!value %>% is.na()) %>%
                mutate(nameEntityManager = name_entity_manager) %>%
                dplyr::select(nameEntityManager, everything()) %>%
                arrange(countItem, item) %>%
                pivot_wider(names_from = item, values_from = value) %>%
                dplyr::select(
                  c(
                    nameEntityManager,
                    countItem,
                    nameBusinessRelatedEntity,
                    nameLegalRelatedEntity,
                    everything()
                  )
                )

              if (return_wide) {
                related_advisor_df <-
                  related_advisor_df %>%
                  .widen_adv_data() %>%
                  .mutate_adv_data()
              }
            } else {
              related_advisor_df <-
                tibble(nameEntityManager = name_entity_manager)
            }
            return(related_advisor_df)
          }

        parse_record_locations <-
          function(page, all_table_node_df, return_wide) {
            has_book_keeping_data <-
              all_table_node_df %>%
              dplyr::filter(nodeText %>% str_detect('Briefly describe the books')) %>% nrow > 0

            if (has_book_keeping_data) {
              node_locations <-
                all_table_node_df$nodeText %>% grep('Briefly describe the books', .)

              record_df <-
                c(-1, 5, 7, 10, 12, 14, 16, 18, 19, 20, 21) %>%
                future_map_dfr(function(x) {
                  has_value <-
                    all_table_node_df$nodeText[node_locations - x] %>% length() >
                    0

                  if (has_value) {
                    val <- all_table_node_df$nodeText[node_locations - x]
                  } else {
                    val <- NA
                  }

                  tibble(value = val,
                             idNode = x)
                }) %>%
                dplyr::filter(!value %>% is.na()) %>%
                dplyr::filter(
                  !value %>% str_detect(
                    'CANCELLED CHECKS ARE HELD BY THE BANK AND KEPT OFFSITE|Number and Street 2:|City:|Country|Number and Street 1'
                  )
                )
              record_df <-
                record_df %>%
                mutate(
                  value = value %>% str_replace_all('Name of entity where books and records are kept:', '')
                ) %>%
                left_join(tibble(
                  idNode = c(-1, 5, 7, 10, 12, 14, 16, 18, 19, 20, 21) ,
                  nameItem = c(
                    'descriptionRecordsKept',
                    'faxRecordKeeper',
                    'phoneRecordKeeper',
                    'zipRecordKeeper',
                    'countryRecordKeeper',
                    'stateRecordKeeper',
                    'cityRecordKeeper',
                    'addressStreet2RecordKeeper',
                    'addressStreet1RecordKeeper',
                    'addressStreet1RecordKeeper',
                    'nameEntityRecordKeeper'
                  )
                )) %>%
                group_by(nameItem) %>%
                mutate(countItem = 1:n()) %>%
                ungroup() %>%
                suppressMessages() %>%
                dplyr::select(-idNode) %>%
                dplyr::select(countItem, nameItem, everything()) %>%
                arrange(countItem, nameItem) %>%
                mutate(value = value %>% str_to_upper) %>%
                pivot_wider(names_from = nameItem, values_from = value) %>%
                mutate(nameEntityManager = name_entity_manager) %>%
                dplyr::select(nameEntityManager,  countItem, everything())

              if ('addressStreet2RecordKeeper' %in% names(record_df)) {
                record_df <-
                  record_df %>%
                  replace_na(list(addressStreet2RecordKeeper = '')) %>%
                  unite(
                    addressStreet1RecordKeeper,
                    addressStreet1RecordKeeper,
                    addressStreet2RecordKeeper,
                    sep = ' '
                  )
              }

              if (names(record_df) %>% str_count('^addressStreet1|^city|^state|^country') %>% sum() >= 4) {
                record_df <-
                  record_df %>%
                  mutate(
                    locationRecordKeeper = addressStreet1RecordKeeper %>% paste0(
                      ' ',
                      cityRecordKeeper,
                      ', ',
                      stateRecordKeeper,
                      ' ',
                      countryRecordKeeper,
                      ' ',
                      zipRecordKeeper
                    )
                  )
              }

              if (return_wide) {
                record_df <-
                  record_df %>%
                  .widen_adv_data()

              }

              if (record_df %>% ncol > 100 |
                  record_df %>% nrow > 8) {
                record_df <-
                  tibble(nameEntityManager = name_entity_manager)
              }
            } else {
              record_df <-
                tibble(nameEntityManager = name_entity_manager)
            }

            return(record_df)


          }

        parse_control_person_data <-
          function(page, return_wide) {
            nodes <-
              page %>%
              .get_html_node_text('.PrintHistRed')

            name_entity_manager <-
              page %>% .get_entity_manager_name()

            if (nodes[nodes %>% str_detect('INDIRECTLY CONTROLS')] %>% length() > 0) {
              node_locations <-
                nodes %>% grep('INDIRECTLY CONTROLS', .)

              control_person_df <-
                8:0 %>%
                future_map_dfr(function(x) {
                  tibble(value = nodes[node_locations - x],
                             idNode = x)
                }) %>%
                left_join(tibble(
                  idNode = 8:0,
                  nameItem = c(
                    'nameControlPerson',
                    'idCRDControlPerson',
                    'dateEffectiveControlPerson',
                    'addressStreet1ControlPerson',
                    'cityControlPerson',
                    'stateControlPerson',
                    'countryControlPerson',
                    'zipControlPerson',
                    'descriptionControlPerson'
                  )
                )) %>%
                group_by(nameItem) %>%
                mutate(countItem = 1:n()) %>%
                ungroup() %>%
                suppressMessages() %>%
                dplyr::select(-idNode)

              control_person_df <-
                control_person_df %>%
                pivot_wider(names_from = nameItem, values_from = value) %>%
                mutate(nameEntityManager = name_entity_manager) %>%
                dplyr::select(nameEntityManager, everything())

              control_person_df <-
                control_person_df %>%
                mutate(across(matches("^date"), ~lubridate::mdy(.))) %>%
                mutate(across(matches("^country[A-Z]|^name|^state|^city"), str_to_upper)) %>%
                dplyr::select(
                  nameEntityManager,
                  nameControlPerson,
                  descriptionControlPerson,
                  everything()
                )

              if (names(control_person_df) %>% str_count('^address|^city|^country|^state') %>% sum() >= 4) {
                control_person_df <-
                  control_person_df %>%
                  mutate(
                    locationControlPerson = addressStreet1ControlPerson %>% paste0(
                      ' ',
                      cityControlPerson,
                      ', ',
                      stateControlPerson,
                      ' ',
                      countryControlPerson,
                      ' ',
                      zipControlPerson
                    )
                  ) %>%
                  dplyr::select(nameEntityManager:countItem,
                                locationControlPerson,
                                everything())
              }

              if ('nameControlPerson' %in% names(control_person_df)) {
                name_df <-
                  control_person_df$nameControlPerson %>%
                  future_map_dfr(.parse_manager_owner_name)

                names(name_df) <-
                  names(name_df) %>%
                  str_replace_all('EntityManagerOwner|EntityOwnerManager',
                                  'ControlPerson')

                control_person_df <-
                  control_person_df %>%
                  left_join(name_df) %>%
                  suppressMessages() %>%
                  dplyr::select(
                    nameEntityManager,
                    nameCommonControlPerson,
                    descriptionControlPerson,
                    everything()
                  )

              }
              if (return_wide) {
                control_person_df <-
                  control_person_df %>%
                  mutate(
                    countItem = countItem - 1,
                    countItem = countItem %>% as.character(),
                    countItem = ifelse(countItem == "0", '', countItem)
                  ) %>%
                  unite(item, nameItem, countItem, sep = '') %>%
                  mutate(nameEntityManager = name_entity_manager) %>%
                  dplyr::select(nameEntityManager, everything())

                col_order <-
                  c('nameEntityManager', control_person_df$item)

                control_person_df <-
                  control_person_df %>%
                  pivot_wider(names_from = item, values_from = value) %>%
                  dplyr::select(any_of(col_order))

                control_person_df <-
                  control_person_df %>%
                  mutate(across(matches("^idCRD"), as.numeric)) %>%
                  mutate(across(matches("^date"), ~lubridate::mdy(.)))
              }
            } else {
              control_person_df <-
                tibble(nameEntityManager = name_entity_manager)
            }
            return(control_person_df)
          }

        parse_public_control_persons <-
          function(page, all_table_node_df) {
            has_public_control_data <-
              all_table_node_df %>% dplyr::filter(nodeText == 'Full legal name of the public reporting company:') %>% nrow > 0
            if (has_public_control_data) {
              namePublicCompanyLegal <-
                all_table_node_df %>%
                dplyr::filter(
                  idRow  == all_table_node_df %>% dplyr::filter(
                    nodeText == 'Full legal name of the public reporting company:'
                  ) %>% .$idRow + 1
                ) %>% .$nodeText

              idCIKPublicCompany <-
                all_table_node_df %>%
                dplyr::filter(
                  idRow  == all_table_node_df %>% dplyr::filter(
                    nodeText %>% str_detect("The public reporting company's CIK number")
                  ) %>% .$idRow + 1
                ) %>% .$nodeText %>%
                as.numeric()

              public_control_df <-
                tibble(nameEntityManager = name_entity_manager) %>%
                mutate(idCIKPublicCompany, namePublicCompanyLegal)
            } else {
              public_control_df <-
                tibble(nameEntityManager = name_entity_manager)
            }

            return(public_control_df)


          }

        parse_other_disclosures <-
          function(page, return_wide) {
            has_other_text_node <-
              !page %>%
              html_nodes(
                '#ctl00_ctl00_cphMainContent_cphAdvFormContent_SchedDMisc_ctl00_trIAPDHeader + tr span'
              ) %>%
              html_text() %>% str_trim() == ''

            if (has_other_text_node) {
              node_text <-
                page %>%
                html_nodes(
                  '#ctl00_ctl00_cphMainContent_cphAdvFormContent_SchedDMisc_ctl00_trIAPDHeader + tr span'
                ) %>%
                html_text()

              node_text <-
                node_text %>%
                str_split('\n|\r') %>%
                flatten_chr() %>%
                str_to_upper() %>%
                str_trim()

              node_text <-
                node_text[!node_text == '']

              other_node_text_df <-
                tibble(
                  nameEntityManager = name_entity_manager,
                  item = 'descriptionOtherDisclosures',
                  value = node_text
                ) %>%
                mutate(countItem = 1:n()) %>%
                pivot_wider(names_from = item, values_from = value)

              if (return_wide) {
                other_node_text_df <-
                  other_node_text_df %>%
                  .widen_adv_data()
              }
            } else {
              other_node_text_df <-
                tibble(nameEntityManager = name_entity_manager)
            }
            return(other_node_text_df)
          }


        parse_other_office_locations_safe <-
          possibly(parse_other_office_locations, NULL)

        parse_record_locations_safe <-
          possibly(parse_record_locations, NULL)

        other_office_location_df <-
          page %>%
          parse_other_office_locations_safe(return_wide = return_wide)

        record_location_df <-
          page %>%
          parse_record_locations_safe(all_table_node_df = all_table_node_df,
                                      return_wide = return_wide)
        parse_related_advisor_safe <-
          possibly(parse_related_advisor, NULL)

        related_adviser_df <-
          page %>%
          parse_related_advisor_safe(return_wide = F)

        parse_control_person_data_safe <-
          possibly(parse_control_person_data, NULL)

        control_person_df <-
          page %>%
          parse_control_person_data_safe(return_wide = return_wide)

        parse_for_manager_website_data_safe <-
          possibly(parse_for_manager_website_data, NULL)

        website_df <-
          page %>%
          parse_for_manager_website_data_safe()

        parse_public_control_persons_safe <-
          possibly(parse_public_control_persons, NULL)

        public_control_df <-
          page %>%
          parse_public_control_persons_safe(all_table_node_df = all_table_node_df)

        parse_other_disclosures_safe <-
          possibly(parse_other_disclosures, NULL)

        other_data_df <-
          page %>%
          parse_other_disclosures_safe(return_wide = return_wide)

        if (join_data) {
          section_data <-
            other_office_location_df %>%
            left_join(related_adviser_df) %>%
            left_join(record_location_df) %>%
            left_join(website_df) %>%
            left_join(control_person_df) %>%
            left_join(public_control_df) %>%
            left_join(other_data_df) %>%
            suppressMessages()
        } else {
          section_data <-
            tibble(
              nameTable = c(
                'Other Office Locations',
                'Record Locations',
                'Related Advisers',
                'Website',
                'Control Persons',
                'Control Public Entities',
                'Other Disclosures'
              ),
              dataTable = list(
                other_office_location_df,
                record_location_df,
                related_adviser_df,
                website_df,
                control_person_df,
                public_control_df,
                other_data_df
              )
            ) %>%
            mutate(nameEntityManager = name_entity_manager,
                   countColumns = dataTable %>% map_dbl(function(x) {
                     if (is.null(x) || !is.data.frame(x)) return(0)
                     ncol(x)
                   }))
        }

        return(section_data)
      }

    section_data <-
      page %>%
      parse_schedule_d(join_data = join_data, return_wide = return_wide)

    section_data <-
      section_data %>%
      mutate(idCRD) %>%
      dplyr::select(idCRD, everything())

    if (!'nameEntityManager' %in% names(section_data)) {
      section_data <-
        section_data %>%
        mutate(nameEntityManager = name_entity_manager) %>%
        dplyr::select(idCRD, nameEntityManager, everything())
    }

    if ((!return_wide) & join_data == T) {
      section_data <-
        section_data %>%
        pivot_longer(cols = -c(nameEntityManager, idCRD),
                     names_to = "item",
                     values_to = "value") %>%
        mutate(
          countItem = item %>% as.character() %>% readr::parse_number(),
          countItem = if_else(countItem %>% is.na, 0, countItem),
          countItem = countItem + 1,
          item = item %>% str_replace_all('[1-9]', '')
        ) %>%
        arrange(countItem) %>%
        suppressWarnings() %>%
        suppressMessages()
    }

    section_data <-
      section_data %>%
      dplyr::select(idCRD, nameEntityManager, everything())

    return(section_data)
  }

.get_manager_signatory_data <-
  function(url = 'https://files.adviserinfo.sec.gov/IAPD/content/viewform/adv/Sections/iapd_AdvSignatureSection.aspx?ORG_PK=160489&FLNG_PK=02FF0ECC00080185033F257005F016CD056C8CC0') {
    idCRD <-
      url %>%
      .get_pk_url_crd()

    page <-
      url %>%
      .get_html_page()

    name_entity_manager <-
      page %>%
      .get_entity_manager_name()

    parse_signature_page <-
      function(page) {
        is_old <-
          (page %>%
             .get_html_node_text('.PrintHistRed') %>% is.na == T) %>% as.numeric() %>% sum() >= 1
        if (is_old) {
          nodes <-
            page %>%
            .get_html_node_text('font') %>%
            unique() %>%
            .[1:3]
        } else {
          nodes <-
            page %>%
            .get_html_node_text('.PrintHistRed') %>%
            str_replace_all('&', 'AND') %>%
            unique()
        }
        signature_df <- tibble(nameEntityManagerSignatory = NA_character_)

        if (nodes %>% length() == 3) {
          item_names <-
            c('nameEntityManagerSignatory',
              'dateADVFiling',
              'titleSignatory')

          signature_df <-
            tibble(item = item_names,
                       value = nodes) %>%
            pivot_wider(names_from = item, values_from = value) %>%
            mutate(dateADVFiling = dateADVFiling %>% lubridate::mdy())
        }

        if (nodes %>% length() == 2) {
          item_names <-
            c('nameEntityManagerSignatory',
              'dateADVFiling')

          signature_df <-
            tibble(item = item_names,
                       value = nodes) %>%
            pivot_wider(names_from = item, values_from = value)
        }

        if (nodes %>% length() >= 4) {
          # Handle case with more than 3 nodes - use first 3
          item_names <-
            c('nameEntityManagerSignatory',
              'dateADVFiling',
              'titleSignatory')

          signature_df <-
            tibble(item = item_names,
                       value = nodes[1:3]) %>%
            pivot_wider(names_from = item, values_from = value) %>%
            mutate(dateADVFiling = dateADVFiling %>% lubridate::mdy()) %>%
            suppressWarnings()
        }

        rm(nodes)
        rm(page)
        return(signature_df)
      }

    signature_df <-
      page %>%
      parse_signature_page() %>%
      mutate(idCRD, nameEntityManager = name_entity_manager) %>%
      dplyr::select(idCRD, nameEntityManager, everything())
    return(signature_df)
  }

.get_section_drp <-
  function(url = 'https://files.adviserinfo.sec.gov/IAPD/content/viewform/adv/Sections/iapd_AdvDrpSection.aspx?ORG_PK=103863&FLNG_PK=05CBF6920008018902B1D9910035D515056C8CC0',
           return_wide = F) {
    idCRD <-
      url %>%
      .get_pk_url_crd()

    page <-
      url %>%
      .get_html_page()

    name_entity_manager <-
      page %>%
      .get_entity_manager_name()

    clean_adv_drp_data <-
      function(data, widen_data = F) {
        has_data <-
          data %>% ncol > 1
        if (has_data) {
          data <-
            data %>%
            dplyr::select(-countItem) %>%
            mutate(across(matches("^date"), as.character)) %>%
            pivot_longer(cols = -c(nameEntityManager, typeCharges),
                         names_to = "itemName",
                         values_to = "value") %>%
            unite(item, itemName, typeCharges, sep = '') %>%
            dplyr::filter(!value %>% is.na()) %>%
            distinct() %>%
            group_by(item) %>%
            mutate(countItem = 1:n()) %>%
            ungroup() %>%
            pivot_wider(names_from = item, values_from = value) %>%
            .mutate_adv_data()

          if (widen_data) {
            data <-
              data %>%
              .widen_adv_data
          }
        }
        return(data)
      }

    generate_table_name_df <-
      function(item_name = 'dateCharged',
               hit_word = 'Charged',
               offset = 1,
               is_numeric = F,
               is_date = T,
               is_employment_firm = F,
               replace_words = NA,
               filter_words = NA) {
        name_df <-
          tibble(
            nameItem = item_name,
            hit_word,
            offset,
            is_numeric,
            is_date,
            is_employment_firm,
            replace_words,
            filter_words
          )
        return(name_df)
      }

    parse_drp_table_data <-
      function(all_table_node_df,
               item_name = 'descriptionPersonEntityViolator',
               hit_word = 'Other Product Types:',
               offset = 1,
               is_numeric = F,
               is_date = F,
               is_employment_firm = F,
               replace_words = NA,
               filter_words = '^7.') {
        row_locs <-
          all_table_node_df %>%
          dplyr::filter(nodeText %>% str_detect(hit_word)) %>%
          .$idRow + offset

        values <-
          all_table_node_df %>% slice(row_locs) %>% .$nodeText
        if (!filter_words %>% is.na()) {
          has_values <-
            values[!values %>% str_detect(filter_words)] %>% length() > 0
        } else {
          has_values <-
            values %>% length() > 0
        }

        if (has_values) {
          if (!filter_words %>% is.na()) {
            values <-
              values[!values %>% str_detect(filter_words)]
          }
        } else {
          values <-
            NA
        }

        if (is_date) {
          values <-
            seq_along(values) %>%
            future_map(function(x) {
              date_values <-
                values[[x]] %>%
                str_split('\\: ') %>%
                flatten_chr()
              val_length <-
                date_values %>% length()
              if (val_length == 1) {
                date_value <-
                  date_values[[1]]
              }
              if (val_length == 2) {
                date_value <-
                  date_values %>%
                  .[[2]]
              }
              if (!val_length %in% c(1, 2)) {
                date_value <-
                  NA
              }
              return(date_value)
            }) %>%
            flatten_chr() %>%
            lubridate::mdy() %>%
            as.character() %>%
            suppressWarnings()

        }

        if (is_employment_firm) {
          values <-
            seq_along(values) %>%
            map_chr(function(x) {
              emp_length <-
                values[x] %>%
                str_split('\\:') %>%
                flatten_chr()
              if (emp_length %>% length() == 1) {
                emp_val <-
                  emp_length %>%
                  .[[1]]
              }
              if (emp_length %>% length() == 2) {
                emp_val <-
                  emp_length %>%
                  .[[2]]
              }
              if (!emp_length %>% length() %in% c(1, 2)) {
                emp_val <-
                  NA
              }
              return(emp_val)
            })

        }

        if (!replace_words %>% is.na()) {
          values <-
            values %>%
            str_replace_all(replace_words, '')
        }
        if (is_numeric) {
          values <-
            values %>%
            str_replace_all('\\$', '') %>%
            as.character() %>%
            readr::parse_number() %>%
            suppressWarnings()
        }
        values <-
          values[!values == '']
        values <-
          values %>% gsub('N/A', NA, .)
        values <-
          values[!values %>% is.na]
        has_values <-
          values %>% length() > 0
        if (has_values) {
          table_df  <-
            tibble(value = values) %>%
            dplyr::filter(!value %>% is.na()) %>%
            mutate(countItem = 1:n(),
                   nameItem = item_name) %>%
            dplyr::select(countItem, nameItem, value)

          if (!filter_words %>% is.na()) {
            table_df <-
              table_df %>%
              dplyr::filter(!value %>% str_detect(filter_words))
          }

          table_df <-
            table_df %>%
            group_by(nameItem, value) %>%
            mutate(
              lagValue = countItem - dplyr::lag(countItem),
              lagValue = ifelse(lagValue %>% is.na, 0, lagValue)
            ) %>%
            ungroup() %>%
            dplyr::filter(!lagValue == 1) %>%
            mutate(countItem = 1:n()) %>%
            dplyr::select(-lagValue)

        } else {
          table_df <-
            tibble(nameItem = item_name)
        }
        return(table_df)
      }

    parse_drp_table_data_safe <-
      possibly(parse_drp_table_data, NULL)

    parse_criminal_data <-
      function(page) {
        has_no_filings <-
          page %>%
          html_nodes('#aspnetForm a[name*="Criminal"] + table') %>%
          html_text() %>% str_trim() %>% nchar() < 130

        if (!has_no_filings) {
          table_nodes <-
            page %>%
            html_nodes('#aspnetForm a[name*="Criminal"] + .flatBorderTable') %>%
            html_text()

          all_table_node_df <-
            seq_along(table_nodes) %>%
            future_map_dfr(function(x) {
              raw_nodes <-
                table_nodes[[x]] %>%
                str_replace_all('\r|\n|\t', '') %>%
                stri_replace_all_charclass("\\p{WHITE_SPACE}", " ") %>%
                stri_trim_both() %>%
                gsub("^\\s+|\\s+$", "", .) %>%
                str_split('\\  ') %>%
                flatten_chr() %>%
                str_trim()

              raw_nodes <-
                raw_nodes[!raw_nodes == '']

              if (raw_nodes %>% length() > 0) {
                tibble(idTable = x, nodeText = raw_nodes)
              }
            }) %>%
            dplyr::select(nodeText) %>%
            mutate(idRow = 1:n())

          drp_name_df <-
            generate_table_name_df(
              item_name = 'idCRDViolator',
              hit_word = 'CRD Number:',
              offset = 1,
              is_numeric = T,
              is_date = F,
              is_employment_firm = F,
              replace_words = NA,
              filter_words = NA
            ) %>%
            bind_rows(
              list(
                generate_table_name_df(
                  item_name = 'namePersonEntityViolator',
                  hit_word = 'Name:',
                  offset = 1,
                  is_numeric = F,
                  is_date = F,
                  is_employment_firm = F,
                  replace_words = NA,
                  filter_words = NA
                ),
                generate_table_name_df(
                  item_name = 'descriptionPersonEntityViolator',
                  hit_word = 'relatedbusiness and your o',
                  offset = 2,
                  is_numeric = F,
                  is_date = F,
                  is_employment_firm = F,
                  replace_words = NA,
                  filter_words = NA
                ),
                generate_table_name_df(
                  item_name = 'descriptionLocationCharges',
                  hit_word = 'Formal Charge(s) were brought in:',
                  offset = 1,
                  is_numeric = F,
                  is_date = F,
                  is_employment_firm = F,
                  replace_words = NA,
                  filter_words = NA
                ),
                generate_table_name_df(
                  item_name = 'dateCharged',
                  hit_word = 'Charged',
                  offset = 1,
                  is_numeric = F,
                  is_date = T,
                  is_employment_firm = F,
                  replace_words = NA,
                  filter_words = NA
                ),
                generate_table_name_df(
                  item_name = 'dateEventStatus',
                  hit_word = 'Event Status Date',
                  offset = 1,
                  is_numeric = F,
                  is_date = T,
                  is_employment_firm = F,
                  replace_words = NA,
                  filter_words = NA
                ),
                generate_table_name_df(
                  item_name = 'descriptionDispositionDisclosure',
                  hit_word = 'Disposition Disclosure',
                  offset = 2,
                  is_numeric = F,
                  is_date = F,
                  is_employment_firm = F,
                  replace_words = NA,
                  filter_words = NA
                ),
                generate_table_name_df(
                  item_name = 'descriptionViolationCircumstances',
                  hit_word = 'Disposition Disclosure',
                  offset = 2,
                  is_numeric = F,
                  is_date = F,
                  is_employment_firm = F,
                  replace_words = NA,
                  filter_words = NA
                )

              )
            )

          all_drp_data <-
            1:nrow(drp_name_df) %>%
            future_map_dfr(function(x) {
              parse_drp_table_data_safe(
                all_table_node_df = all_table_node_df,
                item_name = drp_name_df$nameItem[x],
                hit_word = drp_name_df$hit_word[x],
                replace_words = drp_name_df$replace_words[x],
                filter_words = drp_name_df$filter_words[x],
                offset = drp_name_df$offset[x],
                is_numeric = drp_name_df$is_numeric[x],
                is_date = drp_name_df$is_date[x],
                is_employment_firm =  drp_name_df$is_employment_firm[x]
              )
            }) %>%
            mutate(nameEntityManager = name_entity_manager) %>%
            dplyr::select(nameEntityManager, everything()) %>%
            distinct() %>%
            arrange(countItem) %>%
            dplyr::filter(!countItem %>% is.na()) %>%
            pivot_wider(names_from = nameItem, values_from = value) %>%
            .mutate_adv_data() %>%
            dplyr::select(c(
              nameEntityManager,
              countItem,
              dplyr::matches('namePerson'),
              everything()
            )) %>%
            mutate(typeCharges = 'Criminal') %>%
            suppressWarnings()
        } else{
          all_drp_data <-
            tibble(nameEntityManager = name_entity_manager)
        }
        return(all_drp_data)
      }

    parse_regulary_action_data <-
      function(page) {
        has_no_filings <-
          page %>%
          html_nodes('#aspnetForm a[name*="Regulatory"] + table') %>%
          html_text() %>% str_trim() %>% nchar() < 200

        if (!has_no_filings) {
          table_nodes <-
            page %>%
            html_nodes('#aspnetForm a[name*="Regulatory"] + .flatBorderTable') %>%
            html_text()

          all_table_node_df <-
            seq_along(table_nodes) %>%
            future_map_dfr(function(x) {
              raw_nodes <-
                table_nodes[[x]] %>%
                str_replace_all('\r|\n|\t', '') %>%
                stri_replace_all_charclass("\\p{WHITE_SPACE}", " ") %>%
                stri_trim_both() %>%
                gsub("^\\s+|\\s+$", "", .) %>%
                str_split('\\  ') %>%
                flatten_chr() %>%
                str_trim()

              raw_nodes <-
                raw_nodes[!raw_nodes == '']

              if (raw_nodes %>% length() > 0) {
                tibble(idTable = x, nodeText = raw_nodes)
              }
            }) %>%
            dplyr::select(nodeText) %>%
            mutate(idRow = 1:n())

          drp_name_df <-
            generate_table_name_df(
              item_name = 'idCRDViolator',
              hit_word = 'CRD Number:',
              offset = 1,
              is_numeric = T,
              is_date = F,
              is_employment_firm = F,
              replace_words = NA,
              filter_words = NA
            ) %>%
            bind_rows(
              list(
                generate_table_name_df(
                  item_name = 'namePersonEntityViolator',
                  hit_word = 'Name:',
                  offset = 1,
                  is_numeric = F,
                  is_date = F,
                  is_employment_firm = F,
                  replace_words = NA,
                  filter_words = NA
                ),
                generate_table_name_df(
                  item_name = 'entityEnforcmentAgencyViolation',
                  hit_word = 'foreign financial regulatory authority,',
                  offset = 3,
                  is_numeric = F,
                  is_date = F,
                  is_employment_firm = F,
                  replace_words = NA,
                  filter_words = NA
                ),
                generate_table_name_df(
                  item_name = 'sanctionPrincpalViolation',
                  hit_word = 'Principal Sanction:',
                  offset = 0,
                  is_numeric = F,
                  is_date = F,
                  is_employment_firm = F,
                  replace_words = 'Principal Sanction:',
                  filter_words = NA
                ),
                generate_table_name_df(
                  item_name = 'descriptionSanctions',
                  hit_word = 'Other Sanctions:',
                  offset = 1,
                  is_numeric = F,
                  is_date = F,
                  is_employment_firm = F,
                  replace_words = NA,
                  filter_words = c('^3.')
                ),
                generate_table_name_df(
                  item_name = 'dateInitiated',
                  hit_word = 'Date Initiated',
                  offset = 0,
                  is_numeric = F,
                  is_date = T,
                  is_employment_firm = F,
                  replace_words = NA,
                  filter_words = NA
                ),
                generate_table_name_df(
                  item_name = 'idDocket',
                  hit_word = 'Docket/Case Number',
                  offset = 0,
                  is_numeric = F,
                  is_date = F,
                  is_employment_firm = F,
                  replace_words = 'Docket/Case Number:',
                  filter_words = NA
                ),
                generate_table_name_df(
                  item_name = 'nameEmployingFirmViolationTime',
                  hit_word = 'Employing Firm when activity ',
                  offset = 0,
                  is_numeric = F,
                  is_date = F,
                  is_employment_firm = T,
                  replace_words = NA,
                  filter_words = NA
                ),
                generate_table_name_df(
                  item_name = 'typeProductViolation',
                  hit_word = 'Principal Product Type:',
                  offset = 0,
                  is_numeric = F,
                  is_date = F,
                  is_employment_firm = F,
                  replace_words = 'Principal Product Type:',
                  filter_words = NA
                ),
                generate_table_name_df(
                  item_name = 'descriptionProductViolation',
                  hit_word = 'Other Product Types:',
                  offset = 1,
                  is_numeric = F,
                  is_date = F,
                  is_employment_firm = F,
                  replace_words = NA,
                  filter_words =  c('^7.')
                ),
                generate_table_name_df(
                  item_name = 'descriptionAllegationViolation',
                  hit_word = 'Describe the allegations',
                  offset = 1,
                  is_numeric = F,
                  is_date = F,
                  is_employment_firm = F,
                  replace_words = NA,
                  filter_words = c('^8.')
                ),
                generate_table_name_df(
                  item_name = 'dateResolution',
                  hit_word = 'Resolution Date',
                  offset = 1,
                  is_numeric = F,
                  is_date = T,
                  is_employment_firm = F,
                  replace_words = NA,
                  filter_words = NA
                ),
                generate_table_name_df(
                  item_name = 'amountFineViolation',
                  hit_word = 'Monetary/Fine Amount:',
                  offset = 1,
                  is_numeric = T,
                  is_date = F,
                  is_employment_firm = F,
                  replace_words = NA,
                  filter_words = NA
                ),
                generate_table_name_df(
                  item_name = 'detailOrderViolation',
                  hit_word = 'Ordered:',
                  offset = 1,
                  is_numeric = F,
                  is_date = F,
                  is_employment_firm = F,
                  replace_words = NA,
                  filter_words = NA
                ),
                generate_table_name_df(
                  item_name = 'descriptionSatisfactionConditions',
                  hit_word = 'waived:',
                  offset = 1,
                  is_numeric = F,
                  is_date = F,
                  is_employment_firm = F,
                  replace_words = NA,
                  filter_words = NA
                ),
                generate_table_name_df(
                  item_name = 'descriptionViolationCircumstances',
                  hit_word = 'Provide a brief summary of details related to the action status:',
                  offset = 2,
                  is_numeric = F,
                  is_date = F,
                  is_employment_firm = F,
                  replace_words = NA,
                  filter_words = NA
                )

              )
            )

          all_drp_data <-
            1:nrow(drp_name_df) %>%
            future_map_dfr(function(x) {
              parse_drp_table_data_safe(
                all_table_node_df = all_table_node_df,
                item_name = drp_name_df$nameItem[x],
                hit_word = drp_name_df$hit_word[x],
                replace_words = drp_name_df$replace_words[x],
                filter_words = drp_name_df$filter_words[x],
                offset = drp_name_df$offset[x],
                is_numeric = drp_name_df$is_numeric[x],
                is_date = drp_name_df$is_date[x],
                is_employment_firm =  drp_name_df$is_employment_firm[x]
              )
            }) %>%
            mutate(nameEntityManager = name_entity_manager) %>%
            dplyr::select(nameEntityManager, everything()) %>%
            distinct() %>%
            dplyr::filter(!countItem %>% is.na()) %>%
            arrange(countItem) %>%
            pivot_wider(names_from = nameItem, values_from = value) %>%
            .mutate_adv_data() %>%
            dplyr::select(c(nameEntityManager,
                            countItem,
                            everything())) %>%
            mutate(typeCharges = 'Regulatory') %>%
            suppressWarnings()
        } else{
          all_drp_data <-
            tibble(nameEntityManager = name_entity_manager)
        }
        return(all_drp_data)
      }

    parse_civil_action_data <-
      function(page) {
        has_no_filings <-
          page %>%
          html_nodes('#aspnetForm a[name*="Civil"] + table') %>%
          html_text() %>% str_trim() %>% nchar() < 200

        if (!has_no_filings) {
          table_nodes <-
            page %>%
            html_nodes('#aspnetForm a[name*="Civil"] + .flatBorderTable') %>%
            html_text()


          all_table_node_df <-
            seq_along(table_nodes) %>%
            future_map_dfr(function(x) {
              raw_nodes <-
                table_nodes[[x]] %>%
                str_replace_all('\r|\n|\t', '') %>%
                stri_replace_all_charclass("\\p{WHITE_SPACE}", " ") %>%
                stri_trim_both() %>%
                gsub("^\\s+|\\s+$", "", .) %>%
                str_split('\\  ') %>%
                flatten_chr() %>%
                str_trim()

              raw_nodes <-
                raw_nodes[!raw_nodes == '']

              if (raw_nodes %>% length() > 0) {
                tibble(idTable = x, nodeText = raw_nodes)
              }
            }) %>%
            dplyr::select(nodeText) %>%
            mutate(idRow = 1:n())

          drp_name_df <-
            generate_table_name_df(
              item_name = 'idCRDViolator',
              hit_word = 'CRD Number:',
              offset = 1,
              is_numeric = T,
              is_date = F,
              is_employment_firm = F,
              replace_words = NA,
              filter_words = NA
            ) %>%
            bind_rows(
              list(
                generate_table_name_df(
                  item_name = 'namePersonEntityViolator',
                  hit_word = 'Name:',
                  offset = 1,
                  is_numeric = F,
                  is_date = F,
                  is_employment_firm = F,
                  replace_words = NA,
                  filter_words = NA
                ),
                generate_table_name_df(
                  item_name = 'entityEnforcmentAgencyViolation',
                  hit_word = 'foreign financial regulatory authority,',
                  offset = 3,
                  is_numeric = F,
                  is_date = F,
                  is_employment_firm = F,
                  replace_words = NA,
                  filter_words = NA
                ),
                generate_table_name_df(
                  item_name = 'sanctionPrincpalViolation',
                  hit_word = 'Principal Sanction:',
                  offset = 0,
                  is_numeric = F,
                  is_date = F,
                  is_employment_firm = F,
                  replace_words = 'Principal Sanction:',
                  filter_words = NA
                ),
                generate_table_name_df(
                  item_name = 'descriptionSanctions',
                  hit_word = 'Other Sanctions:',
                  offset = 1,
                  is_numeric = F,
                  is_date = F,
                  is_employment_firm = F,
                  replace_words = NA,
                  filter_words = c('^3.')
                ),
                generate_table_name_df(
                  item_name = 'dateInitiated',
                  hit_word = 'Date Initiated',
                  offset = 0,
                  is_numeric = F,
                  is_date = T,
                  is_employment_firm = F,
                  replace_words = NA,
                  filter_words = NA
                ),
                generate_table_name_df(
                  item_name = 'idDocket',
                  hit_word = 'Docket/Case Number',
                  offset = 0,
                  is_numeric = F,
                  is_date = F,
                  is_employment_firm = F,
                  replace_words = 'Docket/Case Number:',
                  filter_words = NA
                ),
                generate_table_name_df(
                  item_name = 'nameEmployingFirmViolationTime',
                  hit_word = 'Employing Firm when activity ',
                  offset = 0,
                  is_numeric = F,
                  is_date = F,
                  is_employment_firm = T,
                  replace_words = NA,
                  filter_words = NA
                ),
                generate_table_name_df(
                  item_name = 'typeProductViolation',
                  hit_word = 'Principal Product Type',
                  offset = 0,
                  is_numeric = F,
                  is_date = F,
                  is_employment_firm = F,
                  replace_words = 'Principal Product Type:',
                  filter_words = NA
                ),
                generate_table_name_df(
                  item_name = 'descriptionProductViolation',
                  hit_word = 'Other Product Types',
                  offset = 1,
                  is_numeric = F,
                  is_date = F,
                  is_employment_firm = F,
                  replace_words = NA,
                  filter_words =  c('^7.')
                ),
                generate_table_name_df(
                  item_name = 'descriptionAllegationViolation',
                  hit_word = 'Describe the allegations',
                  offset = 1,
                  is_numeric = F,
                  is_date = F,
                  is_employment_firm = F,
                  replace_words = NA,
                  filter_words = c('\\^8.')
                ),
                generate_table_name_df(
                  item_name = 'dateResolution',
                  hit_word = 'Resolution Date',
                  offset = 1,
                  is_numeric = F,
                  is_date = T,
                  is_employment_firm = F,
                  replace_words = NA,
                  filter_words = NA
                ),
                generate_table_name_df(
                  item_name = 'amountFineViolation',
                  hit_word = 'Monetary/Fine Amount:',
                  offset = 1,
                  is_numeric = T,
                  is_date = F,
                  is_employment_firm = F,
                  replace_words = NA,
                  filter_words = NA
                ),
                generate_table_name_df(
                  item_name = 'detailOrderViolation',
                  hit_word = 'Ordered:',
                  offset = 1,
                  is_numeric = F,
                  is_date = F,
                  is_employment_firm = F,
                  replace_words = NA,
                  filter_words = NA
                ),
                generate_table_name_df(
                  item_name = 'descriptionSatisfactionConditions',
                  hit_word = 'waived:',
                  offset = 1,
                  is_numeric = F,
                  is_date = F,
                  is_employment_firm = F,
                  replace_words = NA,
                  filter_words = NA
                ),
                generate_table_name_df(
                  item_name = 'descriptionViolationCircumstances',
                  hit_word = 'Provide a brief summary of details related to the action status:',
                  offset = 2,
                  is_numeric = F,
                  is_date = F,
                  is_employment_firm = F,
                  replace_words = NA,
                  filter_words = NA
                )

              )
            )

          all_drp_data <-
            1:nrow(drp_name_df) %>%
            future_map_dfr(function(x) {
              parse_drp_table_data_safe(
                all_table_node_df = all_table_node_df,
                item_name = drp_name_df$nameItem[x],
                hit_word = drp_name_df$hit_word[x],
                replace_words = drp_name_df$replace_words[x],
                filter_words = drp_name_df$filter_words[x],
                offset = drp_name_df$offset[x],
                is_numeric = drp_name_df$is_numeric[x],
                is_date = drp_name_df$is_date[x],
                is_employment_firm =  drp_name_df$is_employment_firm[x]
              )
            }) %>%
            mutate(nameEntityManager = name_entity_manager) %>%
            dplyr::select(nameEntityManager, everything()) %>%
            distinct()

          all_drp_data <-
            all_drp_data %>%
            dplyr::filter(!countItem %>% is.na()) %>%
            arrange(countItem) %>%
            pivot_wider(names_from = nameItem, values_from = value) %>%
            .mutate_adv_data() %>%
            dplyr::select(c(nameEntityManager,
                            countItem,
                            everything())) %>%
            mutate(typeCharges = 'Civil') %>%
            suppressWarnings()
        } else{
          all_drp_data <-
            tibble(nameEntityManager = name_entity_manager)
        }
        return(all_drp_data)
      }

    criminal_df <-
      page %>%
      parse_criminal_data() %>%
      clean_adv_drp_data(widen_data = return_wide)

    regulatory_df <-
      page %>%
      parse_regulary_action_data() %>%
      clean_adv_drp_data(widen_data = return_wide)

    civil_df <-
      page %>%
      parse_civil_action_data() %>%
      clean_adv_drp_data(widen_data = return_wide)

    if (return_wide) {
      all_drp_data <-
        criminal_df %>% dplyr::select(-dplyr::matches("^countItem")) %>%
        left_join(regulatory_df %>%
                    dplyr::select(-dplyr::matches("^countItem"))) %>%
        left_join(civil_df %>%
                    dplyr::select(-dplyr::matches("^countItem"))) %>%
        suppressMessages()
      if ('nameEntityManager' %in% names(all_drp_data)) {
        all_drp_data <-
          all_drp_data %>%
          mutate(nameEntityManager = name_entity_manager)
      }
      all_drp_data <-
        all_drp_data %>%
        distinct() %>%
        mutate(idCRD) %>%
        dplyr::select(idCRD, nameEntityManager, everything())
    } else {
      all_drp_data <-
        tibble(
          nameTable = c('Criminal DRP', 'Regulatory CRD', 'Civil CRD'),
          dataTable = list(criminal_df, regulatory_df, civil_df)
        ) %>%
        mutate(idCRD)

      if (!'nameEntityManager' %in% names(all_drp_data)) {
        all_drp_data <-
          all_drp_data %>%
          mutate(nameEntityManager = name_entity_manager)
      }
      all_drp_data <-
        all_drp_data %>%
        dplyr::select(idCRD, nameEntityManager, everything())

      all_drp_data <-
        all_drp_data %>%
        mutate(countColumns = dataTable %>% map_dbl(ncol)) %>%
        dplyr::select(-countColumns)
    }
    return(all_drp_data)

  }

.get_crd_sections_data <-
  function(id_crd = 159127,
           all_sections = TRUE,
           score_threshold = .2,
           section_names = c(
             "Registration",
             "Identifying Information",
             "Organization",
             "Successions",
             "Private Fund Reporting",
             "Direct Manager Owners",
             "Indirect Manager Owners",
             "Other Manager Information",
             "Manager Signatories"
           ),
           flatten_tables = TRUE,
           use_html_scraping = TRUE) {

    # Try HTML scraping from files.adviserinfo.sec.gov FIRST (detailed Form ADV data)
    # This is the preferred method as it provides complete Form ADV sections
    if (use_html_scraping) {
      paste0('Scraping HTML from files.adviserinfo.sec.gov for CRD: ', id_crd) %>% message()

      data <- tryCatch({
        .get_managers_adv_sitemap_adv(idCRDs = id_crd, score_threshold = score_threshold) %>%
          distinct()
      }, error = function(e) {
        message(paste0('Error in sitemap: ', e$message))
        tibble()
      })

      message(paste0('Sitemap returned ', nrow(data), ' rows'))

      # Check if we got valid sitemap data with URLs
      if (nrow(data) > 0 && "urlADVSection" %in% names(data)) {
        # Verify a URL actually works before proceeding
        test_url <- data$urlADVSection[1]
        message(paste0('Testing URL: ', test_url))
        test_page <- tryCatch({
          test_url %>% .get_html_page()
        }, error = function(e) {
          message(paste0('Error loading page: ', e$message))
          NULL
        })

        if (!is.null(test_page)) {
          # HTML scraping is working, continue with scraping logic below
          paste0('HTML scraping successful for CRD: ', id_crd) %>% message()

          # Check if idSection exists and filter
          if ("idSection" %in% names(data) && nrow(data) > 0) {
            data <- data %>%
              dplyr::filter(!idSection %>% str_detect('section12SmallBusiness')) %>%
              suppressWarnings() %>%
              suppressMessages()
          }

          # Continue to the HTML scraping section below
          goto_html_scraping <- TRUE
        } else {
          paste0('HTML page not accessible, falling back to API for CRD: ', id_crd) %>% message()
          goto_html_scraping <- FALSE
          data <- tibble()
        }
      } else {
        message(paste0('No urlADVSection in data or empty data'))
        goto_html_scraping <- FALSE
      }
    } else {
      goto_html_scraping <- FALSE
      data <- tibble()
    }

    # If HTML scraping didn't work, try API as fallback
    if (!goto_html_scraping || nrow(data) == 0) {
      api_data <- tryCatch({
        .get_iapd_api_data(id_crd)
      }, error = function(e) NULL)

      if (!is.null(api_data) && !is.null(api_data$iacontent_parsed)) {
        # Use new API-based data extraction
        paste0('Using SEC IAPD API fallback for CRD: ', id_crd) %>% message()

        api_sections <- .parse_api_sections_data(id_crd, api_data)
        basic_info <- api_data$iacontent_parsed$basicInformation
        nameEntityManager <- basic_info$firmName %||% NA_character_

        # Build result in expected format (using nameTable for legacy compatibility)
        results <- list()

      # Registration data
      if (!is.null(api_sections$dataRegistration)) {
        results <- c(results, list(tibble(
          idCRD = id_crd,
          nameEntityManager = nameEntityManager,
          nameTable = "Registration",
          dataTable = list(api_sections$dataRegistration)
        )))
      }

      # Address/Identifying Information
      if (!is.null(api_sections$dataAddress)) {
        results <- c(results, list(tibble(
          idCRD = id_crd,
          nameEntityManager = nameEntityManager,
          nameTable = "Identifying Information",
          dataTable = list(api_sections$dataAddress)
        )))
      }

      # Registration Status
      if (!is.null(api_sections$dataRegistrationStatus)) {
        results <- c(results, list(tibble(
          idCRD = id_crd,
          nameEntityManager = nameEntityManager,
          nameTable = "SEC Reporting",
          dataTable = list(api_sections$dataRegistrationStatus)
        )))
      }

      # Notice Filings
      if (!is.null(api_sections$dataNoticeFilings)) {
        results <- c(results, list(tibble(
          idCRD = id_crd,
          nameEntityManager = nameEntityManager,
          nameTable = "Notice Filings",
          dataTable = list(api_sections$dataNoticeFilings)
        )))
      }

      # Brochures
      if (!is.null(api_sections$dataBrochureDetails)) {
        results <- c(results, list(tibble(
          idCRD = id_crd,
          nameEntityManager = nameEntityManager,
          nameTable = "Brochures",
          dataTable = list(api_sections$dataBrochureDetails)
        )))
      }

      # Accountant Exams
      if (!is.null(api_sections$dataAccountantExams)) {
        results <- c(results, list(tibble(
          idCRD = id_crd,
          nameEntityManager = nameEntityManager,
          nameTable = "Accountant Surprise Exams",
          dataTable = list(api_sections$dataAccountantExams)
        )))
      }

      # Other Names
      if (!is.null(api_sections$dataOtherNames)) {
        results <- c(results, list(tibble(
          idCRD = id_crd,
          nameEntityManager = nameEntityManager,
          nameTable = "Other Names",
          dataTable = list(api_sections$dataOtherNames)
        )))
      }

      # CRS
      if (!is.null(api_sections$dataCRS)) {
        results <- c(results, list(tibble(
          idCRD = id_crd,
          nameEntityManager = nameEntityManager,
          nameTable = "Client Relationship Summary",
          dataTable = list(api_sections$dataCRS)
        )))
      }

      # Org Scope Flags
      if (!is.null(api_sections$dataOrgScopeFlags)) {
        results <- c(results, list(tibble(
          idCRD = id_crd,
          nameEntityManager = nameEntityManager,
          nameTable = "Organization Scope",
          dataTable = list(api_sections$dataOrgScopeFlags)
        )))
      }

      # Relying Advisors (umbrella registration)
      if (!is.null(api_sections$dataRelyingAdvisors)) {
        results <- c(results, list(tibble(
          idCRD = id_crd,
          nameEntityManager = nameEntityManager,
          nameTable = "Relying Advisors",
          dataTable = list(api_sections$dataRelyingAdvisors)
        )))
      }

      # Compilation Data
      if (!is.null(api_sections$dataCompilation)) {
        results <- c(results, list(tibble(
          idCRD = id_crd,
          nameEntityManager = nameEntityManager,
          nameTable = "Compilation Data",
          dataTable = list(api_sections$dataCompilation)
        )))
      }

      # Exempt Reporting Advisers
      if (!is.null(api_sections$dataExemptReportingAdvisers)) {
        results <- c(results, list(tibble(
          idCRD = id_crd,
          nameEntityManager = nameEntityManager,
          nameTable = "Exempt Reporting Advisers",
          dataTable = list(api_sections$dataExemptReportingAdvisers)
        )))
      }

      if (length(results) > 0) {
        all_data <- bind_rows(results)
        return(all_data)
      }
    }

    # Return empty tibble if both methods failed
    return(tibble())
    }

    # HTML SCRAPING PATH (when goto_html_scraping is TRUE)
    # data variable was already populated above when goto_html_scraping was set

    section_null <-
      section_names %>% purrr::is_null()

    if (all_sections) {
      section_names <-
        data$nameSectionActual
    }

    if (section_null &
        !all_sections) {
      stop("You must select a section, possibilties for this search are:\n" %>%
             paste0(paste0(
               data$nameSectionActual, collapse = '\n'
             )))
    }

    get_adv_sections <-
      function(data, all_sections) {
        .get_manager_sec_page_safe <-
          possibly(.get_manager_sec_page, tibble())
        .get_section_drp_safe <-
          possibly(.get_section_drp, tibble())
        .get_section_1_data_safe <-
          possibly(.get_section_1_data, tibble())
        .get_section_2_data_safe <-
          possibly(.get_section_2_data, tibble())
        .get_section_3_data_safe <-
          possibly(.get_section_3_data, tibble())
        .get_section_4_data_safe <-
          possibly(.get_section_4_data, tibble())
        .get_section_5_data_safe <-
          possibly(.get_section_5_data, tibble())
        .get_section_6_data_safe <-
          possibly(.get_section_6_data, tibble())
        .get_section_7a_data_safe <-
          possibly(.get_section_7a_data, tibble())
        .get_section_7b_data_safe <-
          possibly(.get_section_7b_data, tibble())
        .get_section_8_data_safe <-
          possibly(.get_section_8_data, tibble())
        .get_section_9_data_safe <-
          possibly(.get_section_9_data, tibble())
        .get_section_10_data_safe <-
          possibly(.get_section_10_data, tibble())
        .get_section_11_data_safe <-
          possibly(.get_section_11_data, tibble())
        .get_section_12_data_safe <-
          possibly(.get_section_12_data, tibble())
        .get_schedule_a_data_safe <-
          possibly(.get_schedule_a_data, tibble())
        .get_schedule_b_data_safe <-
          possibly(.get_schedule_b_data, tibble())
        .get_schedule_d_data_safe <-
          possibly(.get_schedule_d_data, tibble())
        .get_manager_signatory_data_safe <-
          possibly(.get_manager_signatory_data, tibble())

        if (!all_sections) {
          no_rows <-
            data %>%
            dplyr::filter(nameSectionActual %in% section_names) %>% nrow == 0

          if (no_rows) {
            stop(
              "Sorry tables can only be:\n",
              data$nameSectionActual %>% paste0(collapse = '\n')
            )
          }

          data <-
            data %>%
            dplyr::filter(nameSectionActual %in% section_names)
        }

        all_data <-
          1:nrow(data) %>%
          map_dfr(function(x) {
            f <-
              data$nameFunction[[x]] %>% lazyeval::as_name() %>% eval()

            url <-
              data$urlADVSection[[x]]
            df_name <-
              data$nameData[[x]] %>%
              str_remove_all("data_") %>%
              str_to_title() %>%
              paste0('data', .)
            nameADVPage <-
              data$nameSectionActual[[x]]
            nameADVPage %>% message()
            paste0('idCRD: ', id_crd, ' - ', nameADVPage) %>% cat(fill = T)

            g <-
              f %>%
              list(quote(url)) %>%
              as.call()

            assign(x = df_name, eval(g))
            data <-
              tibble(nameADVPage = nameADVPage,
                         dataTable = list(df_name %>% as_name() %>% eval()))
            return(data)
          })
        return(all_data)
      }

    get_adv_sections_safe <-
      possibly(get_adv_sections, tibble())

    all_data <-
      get_adv_sections_safe(data = data, all_sections = all_sections) %>%
      suppressWarnings()

    all_data <-
      all_data %>%
      mutate(isNULL = dataTable %>% map_dbl(length) == 0) %>%
      dplyr::filter(!isNULL) %>%
      dplyr::select(-isNULL) %>%
      mutate(countCols = dataTable %>% map_dbl(ncol),
             countRows = dataTable %>% map_dbl(nrow))

    # Get the entity manager name from the first row's dataTable
    name_entity_manager <- tryCatch({
      first_data <- all_data$dataTable[[1]]
      if ('nameEntityManager' %in% names(first_data)) {
        first_data$nameEntityManager[[1]]
      } else {
        NA_character_
      }
    }, error = function(e) NA_character_)

    if (is.na(name_entity_manager) || length(name_entity_manager) == 0) {
      name_entity_manager <- "Unknown"
    }

    not_wide_tables <-
      c('Indirect Manager Owners')

    all_data <-
      all_data %>%
      mutate(idCRD = id_crd,
             nameEntityManager = name_entity_manager) %>%
      dplyr::select(idCRD, nameEntityManager, nameADVPage, everything()) %>%
      dplyr::filter(countCols > 2) %>%
      dplyr::filter(countRows > 0) %>%
      mutate(isDataWide = if_else(countRows == 1, T, F)) %>%
      mutate(isDataWide = if_else(nameADVPage %in% not_wide_tables, F, isDataWide))

    if ('Advisory Business Information' %in% all_data$nameADVPage) {
      has_aum_total <-
        all_data %>%
        dplyr::filter(nameADVPage == 'Advisory Business Information') %>%
        dplyr::select(dataTable) %>%
        unnest(cols = dataTable) %>%
        dplyr::select(dplyr::matches("amountAUMTotal")) %>% ncol() == 1

      if (has_aum_total) {
        total_aum <-
          all_data %>%
          dplyr::filter(nameADVPage == 'Advisory Business Information') %>%
          dplyr::select(dataTable) %>%
          unnest(cols = dataTable) %>%
          .$amountAUMTotal %>%
          formattable::currency(digits = 0)
        "Parsed " %>%
          paste0(name_entity_manager,
                 '\nThey have ',
                 total_aum,
                 ' in Total Assets Under Management') %>% cat(fill = T)
      }
    }  else {
      "Parsed " %>%
        paste0(name_entity_manager) %>%
        cat(fill = T)

    }

    for (x in 1:nrow(all_data)) {
      all_data$dataTable[[x]]$nameEntityManager <-
        name_entity_manager

      if (all_data$nameADVPage[x] %in% c('Other Manager Information', 'DRPs')) {
        count_rows <-
          all_data$dataTable[[x]]$dataTable %>% length

        for (count in 1:(count_rows)) {
          all_data$dataTable[[x]]$dataTable[[count]]$nameEntityManager <-
            name_entity_manager
        }
      }
    }

    if (flatten_tables) {
      get_all_manager_description_data <-
        function(all_data) {
          select_nesting_vars <-
            function(data) {
              data <-
                data %>%
                dplyr::select(nameADVPage, dataTable)
              return(data)
            }
          widen_data <-
            all_data %>%
            dplyr::filter(isDataWide)

          widened_data <-
            1:nrow(widen_data) %>%
            map_dfr(function(x) {
              data <-
                widen_data %>%
                select_nesting_vars() %>%
                dplyr::select(-nameADVPage) %>%
                slice(x) %>%
                unnest(cols = dataTable) %>%
                mutate(across(everything(), as.character)) %>%
                pivot_longer(cols = -c(idCRD, nameEntityManager),
                             names_to = "nameItem",
                             values_to = "value")
              return(data)
            }) %>%
            distinct() %>%
            group_by(nameItem) %>%
            mutate(countItem = 1:n()) %>%
            ungroup() %>%
            suppressWarnings() %>%
            dplyr::filter(!value %>% is.na()) %>%
            mutate(idCRD = idCRD %>% as.numeric()) %>%
            mutate(
              countItem = countItem - 1,
              countItem = countItem %>% as.character(),
              countItem = ifelse(countItem == "0", '', countItem)
            ) %>%
            unite(item, nameItem, countItem, sep = '') %>%
            distinct() %>%
            suppressWarnings() %>%
            mutate(value = value %>% str_trim())

          col_order <-
            c('idCRD', 'nameEntityManager', widened_data$item)

          data <-
            widened_data %>%
            pivot_wider(names_from = item, values_from = value) %>%
            dplyr::select(any_of(col_order)) %>%
            .mutate_adv_data()
          return(data)
        }

      manager_description_data <-
        all_data %>%
        get_all_manager_description_data() %>%
        mutate(across(where(is.character), str_trim))

      # Return flat Manager Description data directly
      # Non-wide tables are available in dataTablesNonFlat attribute
      non_wide_data <- all_data %>%
        dplyr::filter(isDataWide == F) %>%
        dplyr::select(idCRD, nameEntityManager,
                      nameTable = nameADVPage,
                      dataTable)

      all_data <- manager_description_data

      # Attach non-wide tables as an attribute for access if needed
      if (nrow(non_wide_data) > 0) {
        attr(all_data, "dataTablesNonFlat") <- non_wide_data
      }
    }
    gc()

    return(all_data)
  }


.get_search_crd_ids <-
  function(entity_names = c('EJF Capital', '137 Ventures'),
           crd_ids = NULL) {
    if (entity_names %>% purrr::is_null() & (crd_ids %>% purrr::is_null())) {
      stop("Please enter search names or CRD IDs")
    }

    crd_df <-
      tibble(idCRD = NA)

    if (!entity_names %>% purrr::is_null()) {
      finra_entities_safe <-
        purrr::possibly(finra_entities, tibble())
      search_name_df <-
        entity_names %>%
        future_map_dfr(function(x) {
          finra_entities(
            entity_names = x,
            return_message = FALSE,
            ocr_pdf = FALSE
          )
        })

      id_crds <-
        search_name_df %>%
        .$idCRD %>%
        as.character() %>%
        readr::parse_number()

      crd_df <-
        crd_df %>%
        bind_rows(tibble(idCRD = id_crds))
    }

    if (!crd_ids %>% purrr::is_null()) {
      crd_df <-
        crd_df %>%
        bind_rows(tibble(idCRD = crd_ids))
    }
    crds <-
      crd_df %>%
      dplyr::filter(!idCRD %>% is.na()) %>%
      distinct() %>%
      .$idCRD

    return(crds)
  }

.return_selected_adv_tables <-
  function(data,
           all_sections,
           table_names,
           gather_data) {
    table_names <-
      data$nameTable %>% unique()
    return_selected_adv_table <-
      function(data,
               table_name,
               gather_data) {
        table_names <-
          c('Manager Description', data$nameTable %>% unique())

        if (table_name %>% str_count(table_names) %>% sum() == 0) {
          stop("Names can only be\n" %>% paste0(paste0(table_names, collapse = '\n')))
        }

        data_selected <-
          data %>%
          dplyr::filter(nameTable %>% str_detect(table_name)) %>%
          dplyr::select(idCRD, nameTable, dataTable) %>%
          mutate(idRow = 1:n())

        crd_df <-
          data_selected %>%
          dplyr::select(idRow, idCRD)

        has_nested_list <-
          data_selected %>%
          dplyr::select(dataTable) %>%
          unnest(cols = dataTable) %>%
          future_map(class) %>%
          as_tibble() %>%
          pivot_longer(cols = everything(),
                       names_to = "column",
                       values_to = "valueCol") %>%
          .$valueCol %>% str_count('list') %>% sum() >= 1

        if (!has_nested_list) {
          data_selected <-
            data_selected %>%
            mutate(countColumns = dataTable %>% map_dbl(ncol)) %>%
            dplyr::filter(countColumns > 1) %>%
            dplyr::select(-countColumns) %>%
            dplyr::filter(nameTable == table_name) %>%
            dplyr::select(dataTable) %>%
            unnest(cols = dataTable)

          section_df <-
            .get_sec_sitemap_df() %>%
            dplyr::select(nameSectionActual, idSection) %>%
            bind_rows(
              tibble(nameSectionActual = 'Manager Description',
                         idSection = 'managerDescription')
            )
          df_name <-
            tibble(nameSectionActual = table_name) %>%
            left_join(section_df) %>%
            suppressMessages() %>%
            .$idSection

          if (gather_data) {
            data_selected <-
              data_selected %>%
              dplyr::select(-dplyr::matches('^count[I]|^numberFund')) %>%
              mutate(across(-idCRD, as.character)) %>%
              pivot_longer(cols = -c(idCRD, nameEntityManager),
                           names_to = "nameItem",
                           values_to = "valueItem") %>%
              arrange(idCRD) %>%
              mutate(nameItem = nameItem %>% str_replace_all('[0-9]', '')) %>%
              mutate(
                nameTable = table_names[x],
                nameItem = nameItem %>% str_replace_all('[0-9]', '')
              ) %>%
              group_by(idCRD, nameEntityManager, nameItem) %>%
              mutate(countItemManager = 1:n()) %>%
              ungroup() %>%
              dplyr::select(nameTable,
                            idCRD:nameItem,
                            countItemManager,
                            valueItem) %>%
              suppressWarnings() %>%
              arrange(idCRD, nameItem, countItemManager)
          }

          assign(x = df_name, eval(data_selected %>% .mutate_adv_data()), envir = .GlobalEnv)
          section_df %>%
            dplyr::filter(nameSectionActual == table_name) %>%
            .$idSection %>% cat(fill = T)
        }
        if (has_nested_list) {
          has_no_count_col <-
            data_selected %>%
            unnest(cols = dataTable, names_repair = "unique") %>% names() %>% str_count('countColumns') %>% sum() == 0
          if (has_no_count_col) {
            count_values <-
              data_selected %>%
              .$dataTable %>% map_dbl(ncol)

            data_selected <-
              data_selected %>%
              mutate(countColumns = count_values)
          }
          data_selected <-
            data_selected %>%
            dplyr::select(idRow, dataTable) %>%
            unnest(cols = dataTable, names_repair = "unique") %>%
            dplyr::select(idRow, nameTable, dataTable) %>%
            mutate(countColumn = map_dbl(dataTable, function(x) {
              if (is.data.frame(x)) ncol(x) else 0
            })) %>%
            dplyr::filter(countColumn > 1) %>%
            dplyr::select(-countColumn)


          if (data_selected %>% nrow >= 1) {
            table_names <-
              data_selected$nameTable %>%
              str_replace_all(' ', '') %>%
              paste0('manager', .) %>%
              unique()

            table_name_df <-
              tibble(idTable = table_names,
                         nameTable = data_selected$nameTable %>% unique())

            data_selected <-
              data_selected %>%
              dplyr::select(idRow, dataTable, nameTable) %>%
              left_join(crd_df) %>%
              left_join(table_name_df) %>%
              suppressMessages() %>%
              dplyr::select(idCRD, idTable, nameTable, dataTable)

            seq_along(table_names) %>%
              future_map(function(x) {
                table_names[x] %>% message
                table_data <-
                  data_selected %>%
                  dplyr::filter(idTable == table_names[x]) %>%
                  dplyr::select(idCRD, dataTable) %>%
                  unnest(cols = dataTable, names_repair = "unique")

                table_data <-
                  table_data %>%
                  mutate(across(matches("^amount[A-Z]"), ~currency(., digits = 0))) %>%
                  mutate(across(matches("^pct[A-Z]"), ~percent(., digits = 0)))

                if (gather_data) {
                  non_id_cols <- setdiff(names(table_data), c("idCRD", "countI", "numberFund"))
                  table_data <-
                    table_data %>%
                    dplyr::select(-dplyr::matches('^count[I]|^numberFund')) %>%
                    mutate(across(-idCRD, as.character)) %>%
                    pivot_longer(cols = -c(idCRD, nameEntityManager),
                                 names_to = "nameItem",
                                 values_to = "valueItem") %>%
                    arrange(idCRD) %>%
                    dplyr::filter(!valueItem %>% is.na()) %>%
                    mutate(
                      nameTable = table_names[x],
                      nameItem = nameItem %>% str_replace_all('[0-9]', '')
                    ) %>%
                    group_by(idCRD, nameEntityManager, nameItem) %>%
                    mutate(countItemManager = 1:n()) %>%
                    ungroup() %>%
                    dplyr::select(nameTable,
                                  idCRD:nameItem,
                                  countItemManager,
                                  valueItem) %>%
                    suppressWarnings() %>%
                    arrange(idCRD, nameItem, countItemManager)
                }

                df_name <-
                  table_names[x]
                assign(x = df_name, eval(table_data %>% .mutate_adv_data()), envir = .GlobalEnv)
              })
          }
        }
        invisible()
      }

    return_selected_adv_table_safe <-
      possibly(return_selected_adv_table, NULL)
    table_names %>%
      walk(function(x) {
        return_selected_adv_table_safe(data = data,
                                       table_name = x,
                                       gather_data = gather_data)
      })
    invisible()
  }

#' IAPD registered managers ADV data
#'
#' This function parses the specified sections of the
#' \href{https://www.sec.gov/answers/formadv.htm}{Form ADV} for specified managers.
#'
#' @param entity_names vector entities you want to search
#' @param crd_ids vector of CRDs to search
#' @param all_sections include all sections \code{TRUE}(default)
#' searches all sections
#' @param section_names sections to search \itemize{
#' \item \code{Registration}: registration information
#' \item \code{Identifying Information}: indentifying information
#' \item \code{Organization}: organizational structure information
#' \item \code{Successions}: succession information
#' \item \code{Private Fund Reporting}: private funds controlled by the manager
#' \item \code{Direct Manager Owners}: direct owners of the fund manager
#' \item \code{Indirect Manager Owners}: indirect owners of the fund manager
#' \item \code{Other Manager Information}: other information
#' \item \code{Manager Signatories}: manager signatories
#' #' }
#' @param flatten_tables \code{TRUE} flattens data with multiple values into wide form
#' @param gather_data \code{TRUE} returns a long data frame
#' @param assign_to_environment \code{TRUE} assign individual data frames to your environment
#' @param parallel Logical. If \code{TRUE} (default), uses parallel processing via furrr.
#'
#' @return a \code{tibble}
#' @export
#' @import tidyverse curl dplyr formattable httr lubridate magrittr purrr readr lazyeval rvest stringi stringr tibble tidyr xml2
#' @importFrom lazyeval as_name
#' @importFrom curl curl_download
#' @importFrom magrittr %>%
#' @importFrom lubridate mdy
#' @importFrom lubridate ymd
#' @family IAPD
#' @family ADV
#' @family entity search
#' @family fund data
#' @examples
#' \dontrun{
#' adv_managers_filings(entity_names = c('Blackstone Real Estate'), crd_ids = NULL,
#'  all_sections = TRUE,  section_names = NULL,
#'  flatten_tables = TRUE, gather_data = FALSE,
#'  assign_to_environment = TRUE)
#' }
adv_managers_filings <-
  function(entity_names = NULL,
           crd_ids = NULL,
           all_sections = TRUE,
           section_names = c(
             "Registration",
             "Identifying Information",
             "Organization",
             "Successions",
             "Private Fund Reporting",
             "Direct Manager Owners",
             "Indirect Manager Owners",
             "Other Manager Information",
             "Manager Signatories"
           ),
           flatten_tables = TRUE,
           gather_data = FALSE,
           assign_to_environment = TRUE,
           parallel = TRUE) {
    if (section_names %>% length() == 1) {
      flatten_tables <-
        FALSE

      all_sections <-
        FALSE

    }
    nothing_entered <-
      (crd_ids %>% purrr::is_null()) & (entity_names %>% purrr::is_null())
    if (nothing_entered) {
      stop("Please enter a CRD ID or a search name")
    }
    .get_search_crd_ids_safe <-
      possibly(.get_search_crd_ids, tibble())

    crds <-
      .get_search_crd_ids(entity_names = entity_names,
                              crd_ids = crd_ids)

    .get_crd_sections_data_safe <-
      possibly(.get_crd_sections_data, tibble())

    # If assign_to_environment is TRUE, we need nested data for assignment
    # Get nested data first, then flatten if needed for return value
    use_flatten_for_fetch <- if (assign_to_environment) FALSE else flatten_tables

    all_data <-
      seq_along(crds) %>%
      map_dfr(function(x) {
        .get_crd_sections_data(
          id_crd = crds[x],
          all_sections = all_sections,
          section_names = section_names,
          flatten_tables = use_flatten_for_fetch
        )
      })

    .return_selected_adv_tables_safe <-
      possibly(.return_selected_adv_tables, tibble())

    only_1 <-
      section_names %>% length() == 1 & assign_to_environment

    assign_all <-
      section_names %>% length() > 1 & assign_to_environment

    # When assign_to_environment is TRUE, we have nested data - assign tables first
    if (assign_all) {
      # Handle nameADVPage vs nameTable column naming for .return_selected_adv_tables
      # API path uses nameTable, HTML scraping path uses nameADVPage
      # When mixing sources, both columns may exist - coalesce them
      data_for_assign <- all_data

      has_adv_page <- "nameADVPage" %in% names(data_for_assign)
      has_name_table <- "nameTable" %in% names(data_for_assign)

      if (has_adv_page && has_name_table) {
        # Both exist (mixed sources) - coalesce into nameTable
        data_for_assign <- data_for_assign %>%
          dplyr::mutate(nameTable = dplyr::coalesce(nameTable, nameADVPage)) %>%
          dplyr::select(-nameADVPage)
      } else if (has_adv_page && !has_name_table) {
        # Only nameADVPage exists (HTML scraping only) - rename it
        data_for_assign <- data_for_assign %>% dplyr::rename(nameTable = nameADVPage)
      }
      # If only nameTable exists (API only), no action needed

      # Verify we have the required columns before calling
      if ("nameTable" %in% names(data_for_assign) &&
          "dataTable" %in% names(data_for_assign) &&
          nrow(data_for_assign) > 0) {
        data_for_assign %>%
          .return_selected_adv_tables(all_sections = TRUE,
                                          gather_data = gather_data)
      } else {
        warning("Could not assign tables to environment - data structure mismatch")
      }
      # Tables are assigned to environment, return the nested data as-is
      return(all_data)
    }

    # When flatten_tables is TRUE and all_data is a flat tibble (no dataTable column),
    # return the flat data directly
    is_flat_data <- flatten_tables && !('dataTable' %in% names(all_data))

    if (is_flat_data) {
      # Return flat Manager Description data directly
      return(all_data)
    }

    if (only_1) {
      # Use nameADVPage if nameTable doesn't exist (SEC Angular SPA format)
      name_col <- if ("nameTable" %in% names(all_data)) all_data$nameTable else all_data$nameADVPage
      table_name <-
        list('manager',
             name_col %>% str_replace_all('\\ ', '')) %>%
        purrr::reduce(paste0) %>%
        unique()

      data <-
        all_data %>%
        select(dataTable) %>%
        unnest(cols = dataTable)

      data <-
        data %>%
        mutate(across(where(is.character), ~ifelse(. == "N/A", NA, .))) %>%
        mutate(across(where(is.character), ~ifelse(. == "", NA, .)))

      # Define columns for text uppercase transformation (excluding nameElement)
      text_cols <- names(data)[str_detect(names(data),
        "^name[A-Z]|^details[A-Z]|^description[A-Z]|^city[A-Z]|^state[A-Z]|^country[A-Z]|^count[A-Z]|^street[A-Z]|^address[A-Z]"
      ) & !str_detect(names(data), "nameElement")]

      # Define price columns (excluding priceNotation)
      price_cols <- names(data)[str_detect(names(data), "^price[A-Z]|pershare") &
                                  !str_detect(names(data), "priceNotation")]

      # Define count columns (excluding country/county)
      count_cols <- names(data)[str_detect(names(data), "^count[A-Z]|^number[A-Z]|^year[A-Z]") &
                                  !str_detect(names(data), "country|county")]

      data <-
        data %>%
        mutate(across(matches("^idCRD|idCIK"), as.numeric)) %>%
        mutate(across(all_of(text_cols), str_to_upper)) %>%
        mutate(across(matches("^amount"), ~as.numeric(.) %>% formattable::currency(digits = 0))) %>%
        mutate(across(matches("^is|^has"), as.logical)) %>%
        mutate(across(matches("latitude|longitude"), ~as.numeric(.) %>% formattable::digits(digits = 5))) %>%
        mutate(across(all_of(price_cols), ~formattable::currency(., digits = 3))) %>%
        mutate(across(all_of(count_cols), ~formattable::comma(., digits = 0))) %>%
        mutate(across(matches("codeInterestAccrualMethod|codeOriginalInterestRateType|codeLienPositionSecuritization|codePaymentType|codePaymentFrequency|codeServicingAdvanceMethod|codePropertyStatus"),
                      as.integer)) %>%
        mutate(across(matches("^ratio|^multiple|^priceNotation|^value"), ~formattable::comma(., digits = 3))) %>%
        mutate(across(matches("^pct|^percent"), ~formattable::percent(., digits = 3))) %>%
        mutate(across(matches("^amountFact"), ~as.numeric(.) %>% formattable::currency(digits = 3))) %>%
        suppressWarnings()

      assign(x = table_name, eval(data %>% .mutate_adv_data()),  envir = .GlobalEnv)
    }

# gc()
return(all_data)

}


# pdf ---------------------------------------------------------------------

.parse_manager_brochure_data <-
  function(url = 'https://adviserinfo.sec.gov/IAPD/IAPDFirmSummary.aspx?ORG_PK=156663') {
    idCRD <-
      url %>%
      .get_pk_url_crd()

    page <-
      url %>%
      .get_html_page()

    name_entity_manager <-
      page %>%
      .get_entity_manager_name()

    brochure_exists <-
      page %>%
      .check_html_node(node_css = '#ctl00_cphMain_landing_p2BrochureLink')

    .parse_sec_manager_pdf_url <-
      function(page) {
        # Get direct brochure PDF links from the registration page
        # These are now available directly as crd_iapd_Brochure.aspx links
        brochure_links <-
          page %>%
          html_nodes('a[href*="crd_iapd_Brochure.aspx"]') %>%
          html_attr('href') %>%
          unique()

        if (length(brochure_links) > 0) {
          url_brochure_pdf <-
            brochure_links %>%
            paste0('https://files.adviserinfo.sec.gov', .)
        } else {
          # Fallback: try to get from Part2Brochures page (legacy approach)
          p2_links <- page %>%
            html_nodes('a[href*="Part2Brochures.aspx"]') %>%
            html_attr('href') %>%
            unique()

          if (length(p2_links) > 0) {
            brochure_page <- tryCatch({
              p2_links[1] %>%
                paste0('https://files.adviserinfo.sec.gov', .) %>%
                .get_html_page()
            }, error = function(e) NULL)

            if (!is.null(brochure_page)) {
              url_brochure_pdf <-
                brochure_page %>%
                html_nodes('.main td a') %>%
                html_attr('href')

              if (length(url_brochure_pdf) > 0) {
                url_brochure_pdf <- url_brochure_pdf %>%
                  paste0('https://files.adviserinfo.sec.gov', .)
              } else {
                url_brochure_pdf <- NA
              }
            } else {
              url_brochure_pdf <- NA
            }
          } else {
            url_brochure_pdf <- NA
          }
        }
        return(url_brochure_pdf)
      }
    pdf_urls <-
      page %>% .parse_sec_manager_pdf_url()
    no_brochure <-
      pdf_urls %>% is.na() %>% as.numeric %>% sum() >= 1

    if (no_brochure) {
      brochure_exists <-
        F
    }
    parse_pdf_brochure <-
      function(url = 'https://adviserinfo.sec.gov/IAPD/Content/Common/crd_iapd_Brochure.aspx?BRCHR_VRSN_ID=379240') {
        info <-
          url %>%
          pdf_info()

        info$created <-
          info$created %>% as.character()

        info$modified <-
          info$modified %>% as.character()

        info <-
          info %>%
          flatten_df()

        sec_name_df <-
          tibble(
            nameSEC = c(
              "version",
              "pages",
              "encrypted",
              "linearized",
              "Author",
              "Creator",
              "Producer",
              "created",
              "modified",
              "metadata",
              "locked",
              "attachments",
              "layout",
              'Title'
            ),
            nameActual = c(
              "idVersion",
              "countPages",
              "isEncrypted",
              "isLinearized",
              "nameAuthor",
              "nameCreator",
              "nameProducer",
              "datetimeCreated",
              "datetimeModified",
              "detailsMetadata",
              "isLocked",
              "hasAttachments",
              "detailLayout",
              'titleDocument'
            )
          )

        sec_names <-
          names(info)

        actual_name_df <-
          seq_along(sec_names) %>%
          future_map_dfr(function(x) {
            name_exists <-
              sec_name_df %>%
              dplyr::filter(nameSEC == sec_names[x]) %>% nrow > 0
            if (name_exists)  {
              nameActual <-
                sec_name_df %>%
                dplyr::filter(nameSEC == sec_names[x]) %>%
                .$nameActual
            } else {
              nameActual <-
                NA
            }
            tibble(idColumn = x, nameActual)
          })

        columns_selected <-
          actual_name_df %>%
          dplyr::filter(!nameActual %>% is.na()) %>%
          .$idColumn

        info <-
          info %>%
          dplyr::select(columns_selected)

        actual_names <-
          actual_name_df %>%
          dplyr::filter(!nameActual %>% is.na()) %>%
          .$nameActual

        names(info) <-
          actual_names

        info <-
          info %>%
          dplyr::select(
            dplyr::matches(
              "countPages|idVersion|dateTime|titleDocument|nameAuthor|nameProducer"
            )
          ) %>%
          mutate(urlPDFManagerADVBrochure = url)

        pdf_pages <-
          url %>%
          pdf_text() %>%
          str_split('\n')

        pdf_text_df <-
          seq_along(pdf_pages) %>%
          future_map_dfr(function(x) {
            page_text <-
              pdf_pages[[x]] %>%
              str_trim()

            page_text <-
              page_text %>% str_replace_all('Form ADV Part 2A: |Form ADV Part 2A Brochure|Part 2A of ADV: ',
                                            '') %>%
              str_replace_all('Firm Brochure', '') %>%
              str_trim()

            page_text <-
              page_text[!page_text == '']

            remove_last_line <-
              !page_text[page_text %>% length() %>% max] %>%
              as.character() %>%
              readr::parse_number() %>% is.na %>% suppressWarnings()

            if (remove_last_line) {
              page_text <-
                page_text[1:(page_text %>% length() - 1)]
            }

            clean_text <-
              function (text.var) {
                text.var <-
                  gsub("\\s+",
                       " ",
                       gsub("\\\\r|\\\\n|\\n|\\\\t", " ", text.var))
                return(text.var)
              }

            page_text <-
              page_text %>%
              stri_replace_all_charclass("\\p{WHITE_SPACE}", " ") %>%
              stri_trim_both() %>%
              clean_text

            page_text <-
              page_text %>% paste0(collapse = ' ') %>%
              clean_text

            page_text <-
              page_text %>%
              stringi::stri_trans_general("latin-ascii")

            page_data <-
              tibble(numberPage = x, textPage = page_text)
            return(page_data)
          }) %>%
          dplyr::select(textPage) %>%
          summarise(textBrochure = textPage %>% paste0(collapse = '\n')) %>%
          mutate(urlPDFManagerADVBrochure = url) %>%
          right_join(info) %>%
          suppressMessages() %>%
          dplyr::select(datetimeCreated,
                        textBrochure,
                        everything())

        return(pdf_text_df)

      }

    if (brochure_exists) {
      urlPDFManagerADVBrochure <-
        page %>%
        .parse_sec_manager_pdf_url()

      brochure_data <-
        urlPDFManagerADVBrochure %>%
        future_map_dfr(function(x) {
          parse_pdf_brochure(url = x)
        }) %>%
        mutate(nameEntityManager = name_entity_manager, idCRD)

      brochure_data <-
        brochure_data %>%
        mutate(across(matches("datetime"), ymd_hms)) %>%
        arrange(desc(datetimeCreated))
    } else {
      brochure_data <-
        tibble(nameEntityManager = name_entity_manager, idCRD)
    }

    brochure_data <-
      brochure_data %>%
      .select_start_vars()

    return(brochure_data)

  }

.get_manager_brochure_data <-
  function(id_crd = 156663,
           split_pages = TRUE) {
    url <-
      .get_managers_adv_sitemap_adv(idCRDs = id_crd) %>%
      distinct() %>%
      dplyr::filter(nameSectionActual == 'Registration') %>%
      .$urlADVSection %>%
      suppressWarnings() %>%
      suppressMessages() %>%
      str_replace('^http://', 'https://')

    pdf_data <-
      url %>%
      .parse_manager_brochure_data()

    if ('textBrochure' %in% names(pdf_data)) {
      if (split_pages) {
        pdf_data <-
          pdf_data %>%
          separate_rows(textBrochure, sep = '\n')
      }
    }

    pdf_data

  }

#' OCR IAPD Managers Brochures
#'
#' This function OCRs a firm's brochure
#' required for Part 2A of the Form-ADV
#'
#' @param entity_names vector names of the companies you want to search
#' @param crd_ids numeric vector CRDs you want to search
#' @param nest_data \code{TRUE} return nested data frame
#' @param split_pages \code{TRUE} split brochure into individual pages
#' @param parallel if \code{TRUE} uses parallel processing
#'
#' @return A tibble containing OCR'd brochure text and metadata for the specified managers
#' @export
#' @import curl dplyr formattable httr lubridate magrittr purrr readr lazyeval rvest stringi stringr tibble pdftools tidyr jsonlite
#' @examples
#' adv_managers_brochures(entity_names = c('137 Ventures', 'Divco'), crd_ids = 156663, split_pages = TRUE, nest_data = TRUE)
adv_managers_brochures <-
  function(entity_names = NULL,
           crd_ids = NULL,
           split_pages = TRUE,
           nest_data = FALSE,
           parallel = TRUE) {
    nothing_entered <-
      (crd_ids %>% purrr::is_null()) & (entity_names %>% purrr::is_null())
    if (nothing_entered) {
      stop("Please enter a CRD ID or a search name")
    }
    .get_search_crd_ids_safe <-
      possibly(.get_search_crd_ids, tibble())

    crds <-
      .get_search_crd_ids_safe(entity_names = entity_names, crd_ids = crd_ids)

    .get_manager_brochure_data_safe <-
      possibly(.get_manager_brochure_data, tibble())

    all_data <-
     crds %>%
      future_map_dfr(function(crd) {
        manager_pdf <-
          .get_manager_brochure_data_safe(id_crd = as.numeric(crd), split_pages = split_pages)

        if (nrow(manager_pdf) >= 1) {
          manager <- manager_pdf$nameEntityManager %>% unique()
          date_file <- manager_pdf$datetimeCreated %>% unique()
          glue::glue("Acquired ADV brochure for {manager} filed on {date_file}") %>%
            cat(fill = T)
        }

        manager_pdf
      })

    if (nest_data) {
      all_data <-
        all_data %>%
        nest(dataBrochure = -c(
          idCRD,
          nameEntityManager,
          titleDocument,
          datetimeCreated,
          countPages
        ))
    }
    all_data
  }


# Registered_Advisor_Data Downloands -------------------------------------------------

#' Get ADV summary filing urls for all available periods
#'
#' Fetches URLs for SEC Form ADV summary data files from the data.gov API.
#' The SEC provides monthly data files for both registered and exempt
#' investment advisers.
#'
#' @param use_api If \code{TRUE} (default), fetch URLs from data.gov API.
#'   If \code{FALSE}, generate URLs based on date patterns (may include
#'   unavailable files).
#'
#' @return A \code{tibble} with columns:
#'   \itemize{
#'     \item typeReport - Type of report (REGISTERED or EXEMPT)
#'     \item periodReport - Month and year of the report
#'     \item urlZip - URL to download the file
#'     \item isExempt - Whether this is exempt adviser data
#'     \item baseNameURL - Base filename
#'     \item dateData - Date of the data
#'     \item quarterData - Quarter number
#'     \item yearData - Year
#'     \item periodData - Period in YYYY-MM format
#'   }
#' @export
#' @import dplyr tibble lubridate purrr stringr httr jsonlite
#' @examples
#' \dontrun{
#' adv_period_urls()
#' }
adv_period_urls <-
  function(use_api = TRUE) {

    if (use_api) {
      # Fetch URLs from data.gov API - this returns verified, available files
      api_url <- "https://catalog.data.gov/api/3/action/package_show?id=information-about-registered-investment-advisers-and-exempt-reporting-advisers"

      response <- tryCatch({
        httr::GET(
          api_url,
          httr::add_headers(
            "User-Agent" = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36",
            "Accept" = "application/json"
          ),
          httr::timeout(30)
        )
      }, error = function(e) NULL)

      if (!is.null(response) && httr::status_code(response) == 200) {
        json_data <- httr::content(response, as = "text", encoding = "UTF-8") %>%
          jsonlite::fromJSON(flatten = TRUE)

        resources <- json_data$result$resources

        if (!is.null(resources) && nrow(resources) > 0) {
          # Filter to only zip and xlsx files (exclude "no-data" PDFs)
          data <- resources %>%
            as_tibble() %>%
            dplyr::filter(str_detect(url, "\\.(zip|xlsx)$")) %>%
            dplyr::select(urlZip = url) %>%
            mutate(
              baseNameURL = basename(urlZip),
              isExempt = str_detect(baseNameURL, "exempt"),
              typeReport = ifelse(isExempt, "EXEMPT INVESTMENT ADVISERS", "REGISTERED INVESTMENT ADVISERS")
            )

          # Extract dates from filenames
          # Pattern: ia{MMDDYY}.zip or ia{MMDDYYYY}.xlsx
          data <- data %>%
            mutate(
              date_str = str_extract(baseNameURL, "ia([0-9]+)") %>% str_remove("ia"),
              dateData = case_when(
                nchar(date_str) == 6 ~ lubridate::mdy(date_str),
                nchar(date_str) == 8 ~ lubridate::mdy(date_str),
                TRUE ~ NA_Date_
              ),
              periodReport = format(dateData, "%B %Y"),
              quarterData = lubridate::quarter(dateData),
              yearData = lubridate::year(dateData),
              periodData = format(dateData, "%Y-%m")
            ) %>%
            dplyr::select(-date_str) %>%
            dplyr::filter(!is.na(dateData)) %>%
            arrange(desc(dateData), isExempt)

          gc()
          return(data)
        }
      }
    }

    # Fallback: Generate URLs based on date patterns
    base_url <- "https://www.sec.gov/files/investment/data/information-about-registered-investment-advisers-exempt-reporting-advisers/"
    start_dt <- lubridate::ymd("2023-01-01")
    end_dt <- Sys.Date()

    dates <- seq(
      from = lubridate::floor_date(start_dt, "month"),
      to = lubridate::floor_date(end_dt, "month"),
      by = "month"
    )

    data <- purrr::map_dfr(dates, function(dt) {
      date_str <- format(dt, "%m%d%y")
      tibble(
        typeReport = c("REGISTERED INVESTMENT ADVISERS", "EXEMPT INVESTMENT ADVISERS"),
        periodReport = format(dt, "%B %Y"),
        isExempt = c(FALSE, TRUE),
        dateData = dt
      ) %>%
        mutate(
          baseNameURL = ifelse(isExempt,
                               paste0("ia", date_str, "-exempt.zip"),
                               paste0("ia", date_str, ".zip")),
          urlZip = paste0(base_url, baseNameURL),
          quarterData = lubridate::quarter(dateData),
          yearData = lubridate::year(dateData),
          periodData = format(dateData, "%Y-%m")
        )
    }) %>%
      arrange(desc(dateData), isExempt)

    gc()
    data
  }
.assign_sec_names <-
  function(data) {
    df_actual_names <-
      dictionary_sec_names()

    actual_names <-
      names(data) %>%
      map_chr(function(name) {
        no_name <-
          df_actual_names %>%
          filter(nameSEC == name) %>%
          nrow() == 0

        if (no_name) {
          glue::glue("Missing {name} in dictionary") %>% cat(fill = T)
          return(name)
        }
        df_actual_names %>%
          filter(nameSEC == name) %>%
          pull(nameActual) %>%
          unique() %>%
          .[[1]]
      })

    actual_names
  }


#' SEC Name Dictionary
#'
#' Returns a tibble mapping SEC column names to standardized actual names
#' used throughout the package.
#'
#' @return A tibble with columns nameSEC and nameActual for column name mappings
#' @export
#'
#' @examples
#' \dontrun{
#' dictionary_sec_names()
#' }
dictionary_sec_names <-
  function() {
    sec_name_df <-
      tibble(
        nameSEC = c("SEC Region", "Organization CRD#", "SEC#", "Firm Type", "Primary Business Name",
                    "Legal Name", "Main Office Street Address 1", "Main Office Street Address 2",
                    "Main Office City", "Main Office State", "Main Office Country",
                    "Main Office Postal Code", "Main Office Telephone Number", "Main Office Facsimile Number",
                    "Mail Office Street Address 1", "Mail Office Street Address 2",
                    "Mail Office City", "Mail Office State", "Mail Office Country",
                    "Mail Office Postal Code", "SEC Current Status", "SEC Status Effective Date",
                    "Jurisdiction Notice Filed-Effective Date", "Latest ADV Filing Date",
                    "Form Version", "1I", "Website Address", "1M", "1N", "CIK#",
                    "1O", "1P", "2A(1)", "2A(2)", "2A(3)", "2A(4)", "2A(5)", "2A(6)",
                    "2A(7)", "2A(8)", "2A(9)", "2A(10)", "2A(11)", "2A(12)", "2A(13)",
                    "3A", "3A-Other", "3B", "3C-State", "3C-Country", "5A", "5B(1)",
                    "5B(2)", "5B(3)", "5B(4)", "5B(5)", "5B(6)", "5C(1)", "5C(1)-If more than 100, how many",
                    "5C(2)", "5D(1)(a)", "5D(1)(b)", "5D(1)(c)", "5D(1)(d)", "5D(1)(e)",
                    "5D(1)(f)", "5D(1)(g)", "5D(1)(h)", "5D(1)(i)", "5D(1)(j)", "5D(1)(k)",
                    "5D(1)(l)", "5D(1)(m)", "5D(1)(m)-Other", "5D(2)(a)", "5D(2)(b)",
                    "5D(2)(c)", "5D(2)(d)", "5D(2)(e)", "5D(2)(f)", "5D(2)(g)", "5D(2)(h)",
                    "5D(2)(i)", "5D(2)(j)", "5D(2)(k)", "5D(2)(l)", "5D(2)(m)", "5D(2)(m)-Other",
                    "5E(1)", "5E(2)", "5E(3)", "5E(4)", "5E(5)", "5E(6)", "5E(7)",
                    "5E(7)-Other", "5F(1)", "5F(2)(a)", "5F(2)(b)", "5F(2)(c)", "5F(2)(d)",
                    "5F(2)(e)", "5F(2)(f)", "5G(1)", "5G(2)", "5G(3)", "5G(4)", "5G(5)",
                    "5G(6)", "5G(7)", "5G(8)", "5G(9)", "5G(10)", "5G(11)", "5G(12)",
                    "5G(12)-Other", "5H", "5H-If more than 500, how many", "5I(1)",
                    "5I(2)", "5J", "6A(1)", "6A(2)", "6A(3)", "6A(4)", "6A(5)", "6A(6)",
                    "6A(7)", "6A(8)", "6A(9)", "6A(10)", "6A(11)", "6A(12)", "6A(13)",
                    "6A(14)", "6A(14)-Other", "6B(1)", "6B(2)", "6B(3)", "7A(1)",
                    "7A(2)", "7A(3)", "7A(4)", "7A(5)", "7A(6)", "7A(7)", "7A(8)",
                    "7A(9)", "7A(10)", "7A(11)", "7A(12)", "7A(13)", "7A(14)", "7A(15)",
                    "7A(16)", "7B", "8A(1)", "8A(2)", "8A(3)", "8B(1)", "8B(2)",
                    "8B(3)", "8C(1)", "8C(2)", "8C(3)", "8C(4)", "8D", "8E", "8F",
                    "8G(1)", "8G(2)", "8H", "8I", "9A(1)(a)", "9A(1)(b)", "9A(2)(a)",
                    "9A(2)(b)", "9B(1)(a)", "9B(1)(b)", "9B(2)(a)", "9B(2)(b)", "9C(1)",
                    "9C(2)", "9C(3)", "9C(4)", "9D(1)", "9D(2)", "9E", "9F", "10A",
                    "11", "11A(1)", "11A(2)", "11B(1)", "11B(2)", "11C(1)", "11C(2)",
                    "11C(3)", "11C(4)", "11C(5)", "11D(1)", "11D(2)", "11D(3)", "11D(4)",
                    "11D(5)", "11E(1)", "11E(2)", "11E(3)", "11E(4)", "11F", "11G",
                    "11H(1)(a)", "11H(1)(b)", "11H(1)(c)", "11H(2)", "SEC Region Name",
                    "Organization CRD #", "SEC #", "Main Street Address 1", "Main Street Address 2",
                    "Main Office City, State, Postal Code", "Mail Office City, State, Postal Code",
                    "Contact Name", "Telephone Number", "Legal Status", "Types of Advisory Activities",
                    "Regulator Status", "Effective Date", "Criminal Disclosures",
                    "Regulatory Action Disclosures", "Civil Judicial Disclosures",
                    "Bankruptcy Disclosures", "Judgment/Lien Disclosures", "Bond Payout Disclosures",
                    "SEC Registration Status", "Status Effective Date", "World Wide Web Site Address",
                    "1L", "5A-If more than 1,000, how many", "5B(1)-If more than 1,000, how many",
                    "5B(2)-If more than 1,000, how many", "5B(3)-If more than 1,000, how many",
                    "5C", "5C-If more than 500, how many", "5D(1)", "5D(2)", "5D(3)",
                    "5D(4)", "5D(5)", "5D(6)", "5D(7)", "5D(8)", "5D(9)", "5D(10)",
                    "5D(10)-Other", "5G(10)-Other", "6A(7)-Other", "9A(1)", "9A(2)",
                    "9B(1)", "9B(2)", "9C", "10", "Current Status", "FINRA BD Status",
                    "2B(1)", "2B(2)", "2B(3)", "Total number of offices, other than your Principal Office and place of business",
                    "1O - If yes, approx. amount of assets", "5D(a)(1)", "5D(a)(2)",
                    "5D(a)(3)", "5D(b)(1)", "5D(b)(2)", "5D(b)(3)", "5D(c)(1)", "5D(c)(2)",
                    "5D(c)(3)", "5D(d)(1)", "5D(d)(3)", "5D(e)(1)", "5D(e)(3)", "5D(f)(1)",
                    "5D(f)(3)", "5D(g)(1)", "5D(g)(2)", "5D(g)(3)", "5D(h)(1)", "5D(h)(2)",
                    "5D(h)(3)", "5D(i)(1)", "5D(i)(2)", "5D(i)(3)", "5D(j)(1)", "5D(j)(2)",
                    "5D(j)(3)", "5D(k)(1)", "5D(k)(2)", "5D(k)(3)", "5D(l)(1)", "5D(l)(2)",
                    "5D(l)(3)", "5D(m)(1)", "5D(m)(2)", "5D(m)(3)", "5D(n)(1)", "5D(n)(2)",
                    "5D(n)(3)", "5D(n)(3) - Other", "5F(3)", "5I(2)(a)", "5I(2)(b)",
                    "5I(2)(c)", "5J(1)", "5J(2)", "8H(1)", "8H(2)",
                    "7A8(a) BD Qual Cust", "Additional CRD Number", "Additional Regulatory Contact Name",
                     "Additional Regulatory Contact Person City", "Additional Regulatory Contact Person Country",
                     "Additional Regulatory Contact Person E-mail", "Additional Regulatory Contact Person Facsimile",
                     "Additional Regulatory Contact Person Postal Code", "Additional Regulatory Contact Person State",
                     "Additional Regulatory Contact Person Street Address 1", "Additional Regulatory Contact Person Street Address 2",
                     "Additional Regulatory Contact Person Telephone", "Additional Regulatory Contact Person Titles",
                     "Amount ERA US Private Fund Assets", "Any Hedge Funds", "Any Liquidity Funds",
                     "Any Other Funds", "Any PE Funds", "Any PFs a Master", "Any Real Estate Funds",
                     "Any Securitized Funds", "Any VC Funds", "Chief Compliance Officer City",
                     "Chief Compliance Officer Country", "Chief Compliance Officer E-mail",
                     "Chief Compliance Officer Facsimile", "Chief Compliance Officer Name",
                     "Chief Compliance Officer Other Titles", "Chief Compliance Officer Postal Code",
                     "Chief Compliance Officer State", "Chief Compliance Officer Street Address 1",
                     "Chief Compliance Officer Street Address 2", "Chief Compliance Officer Telephone",
                     "Control/Controlled by Related Person", "Count of 11A(1) disclosures",
                     "Count of 11A(2) disclosures", "Count of 11B(1) disclosures",
                     "Count of 11B(2) disclosures", "Count of 11C(1) disclosures",
                     "Count of 11C(2) disclosures", "Count of 11C(3) disclosures",
                     "Count of 11C(4) disclosures", "Count of 11C(5) disclosures",
                     "Count of 11D(1) disclosures", "Count of 11D(2) disclosures",
                     "Count of 11D(3) disclosures", "Count of 11D(4) disclosures",
                     "Count of 11D(5) disclosures", "Count of 11E(1) disclosures",
                     "Count of 11E(2) disclosures", "Count of 11E(3) disclosures",
                     "Count of 11E(4) disclosures", "Count of 11F disclosures", "Count of 11G disclosures",
                     "Count of 11H(1)(a) disclosures", "Count of 11H(1)(b) disclosures",
                     "Count of 11H(1)(c) disclosures", "Count of 11H(2) disclosures",
                     "Count of BD Affiliates", "Count of Control person Public Reporting Company",
                     "Count of IA Affiliates", "Count of IA/BD Affiliates", "Count of Private Funds - 7B(1)",
                     "Count of Private Funds - 7B(2)", "EIN of the Other person compensating CCO",
                     "First ADV Filing Date", "Location of Books and Records City",
                     "Location of Books and Records Country", "Location of Books and Records Postal Code",
                     "Location of Books and Records State", "Location of Books and Records Street Address 1",
                     "Location of Books and Records Street Address 2", "Mail Office Private Residence Flag",
                     "Main Office Private Residence Flag", "Name of the other person compensating CCO",
                     "Share Location", "Share Supervised Persons", "Sole Proprietor City",
                     "Sole Proprietor Country", "Sole Proprietor Postal Code", "Sole Proprietor State",
                     "Sole Proprietor Street Address 1", "Sole Proprietor Street Address 2",
                     "Total Gross Assets of Private Funds", "Total number of additional CRD numbers",
                     "Total Number of Books and Records Locations", "Total number of CIK numbers",
                     "Total number of Hedge funds", "Total number of Liquidity funds",
                     "Total number of Other funds", "Total number of PE funds", "Total number of Real Estate funds",
                     "Total number of Securitized funds", "Total number of VC funds",
                     "Total Number of Website Addresses", "Under Common Control",
                    "12A", "12B(1)", "12B(2)", "12C(1)", "12C(2)", "4A", "4B",
                    "5.G.(3) - Total amount of Parallel Assets", "5.G.(3) - Total number of RICs or BDCs",
                    "5.I.(2) - Total number of wrap fee programs", "5.K.(1)(a)(i) end year percentage",
                    "5.K.(1)(a)(i) midyear percentage", "5.K.(1)(a)(ii) end year percentage",
                    "5.K.(1)(a)(ii) midyear percentage", "5.K.(1)(a)(iii) end year percentage",
                    "5.K.(1)(a)(iii) midyear percentage", "5.K.(1)(a)(iv) end year percentage",
                    "5.K.(1)(a)(iv) midyear percentage", "5.K.(1)(a)(ix) end year percentage",
                    "5.K.(1)(a)(ix) midyear percentage", "5.K.(1)(a)(v) end year percentage",
                    "5.K.(1)(a)(v) midyear percentage", "5.K.(1)(a)(vi) end year percentage",
                    "5.K.(1)(a)(vi) midyear percentage", "5.K.(1)(a)(vii) end year percentage",
                    "5.K.(1)(a)(vii) midyear percentage", "5.K.(1)(a)(viii) end year percentage",
                    "5.K.(1)(a)(viii) midyear percentage", "5.K.(1)(a)(x) end year percentage",
                    "5.K.(1)(a)(x) midyear percentage", "5.K.(1)(a)(xi) end year percentage",
                    "5.K.(1)(a)(xi) midyear percentage", "5.K.(1)(a)(xii)  - Other description",
                    "5.K.(1)(a)(xii) end year percentage", "5.K.(1)(a)(xii) midyear percentage",
                    "5.K.(1)(b)(i) end year percentage", "5.K.(1)(b)(ii) end year percentage",
                    "5.K.(1)(b)(iii) end year percentage", "5.K.(1)(b)(iv) end year percentage",
                    "5.K.(1)(b)(ix) end year percentage", "5.K.(1)(b)(v) end year percentage",
                    "5.K.(1)(b)(vi) end year percentage", "5.K.(1)(b)(vii) end year percentage",
                    "5.K.(1)(b)(viii) end year percentage", "5.K.(1)(b)(x) end year percentage",
                    "5.K.(1)(b)(xi) end year percentage", "5.K.(1)(b)(xii)  - Other description",
                    "5.K.(1)(b)(xii) end year percentage", "5.K.(2)(a)(i)(1) 10-149",
                    "5.K.(2)(a)(i)(1) less 10", "5.K.(2)(a)(i)(1) over 150", "5.K.(2)(a)(i)(2) 10-149",
                    "5.K.(2)(a)(i)(2) less 10", "5.K.(2)(a)(i)(2) over 150", "5.K.(2)(a)(i)(3)(a) 10-149 percentage",
                    "5.K.(2)(a)(i)(3)(a) less 10 percentage", "5.K.(2)(a)(i)(3)(a) over 150 percentage",
                    "5.K.(2)(a)(i)(3)(b) 10-149 percentage", "5.K.(2)(a)(i)(3)(b) less 10 percentage",
                    "5.K.(2)(a)(i)(3)(b) over 150 percentage", "5.K.(2)(a)(i)(3)(c) 10-149 percentage",
                    "5.K.(2)(a)(i)(3)(c) less 10 percentage", "5.K.(2)(a)(i)(3)(c) over 150 percentage",
                    "5.K.(2)(a)(i)(3)(d) 10-149 percentage", "5.K.(2)(a)(i)(3)(d) less 10 percentage",
                    "5.K.(2)(a)(i)(3)(d) over 150 percentage", "5.K.(2)(a)(i)(3)(e) 10-149 percentage",
                    "5.K.(2)(a)(i)(3)(e) less10 percentage", "5.K.(2)(a)(i)(3)(e) over 150 percentage",
                    "5.K.(2)(a)(i)(3)(f) 10-149 percentage", "5.K.(2)(a)(i)(3)(f) less 10 percentage",
                    "5.K.(2)(a)(i)(3)(f) over 150 percentage", "5.K.(2)(a)(ii)(1) 10-149",
                    "5.K.(2)(a)(ii)(1) less 10", "5.K.(2)(a)(ii)(1) over 150", "5.K.(2)(a)(ii)(2) 10-149",
                    "5.K.(2)(a)(ii)(2) less 10", "5.K.(2)(a)(ii)(2) over 150", "5.K.(2)(a)(ii)(3)(a) 10-149 percentage",
                    "5.K.(2)(a)(ii)(3)(a) less 10 percentage", "5.K.(2)(a)(ii)(3)(a) over 150 percentage",
                    "5.K.(2)(a)(ii)(3)(b) 10-149 percentage", "5.K.(2)(a)(ii)(3)(b) less 10 percentage",
                    "5.K.(2)(a)(ii)(3)(b) over 150 percentage", "5.K.(2)(a)(ii)(3)(c) 10-149 percentage",
                    "5.K.(2)(a)(ii)(3)(c) less 10 percentage", "5.K.(2)(a)(ii)(3)(c) over 150 percentage",
                    "5.K.(2)(a)(ii)(3)(d) 10-149 percentage", "5.K.(2)(a)(ii)(3)(d) less 10 percentage",
                    "5.K.(2)(a)(ii)(3)(d) over 150 percentage", "5.K.(2)(a)(ii)(3)(e) 10-149 percentage",
                    "5.K.(2)(a)(ii)(3)(e) less10 percentage", "5.K.(2)(a)(ii)(3)(e) over 150 percentage",
                    "5.K.(2)(a)(ii)(3)(f) 10-149 percentage", "5.K.(2)(a)(ii)(3)(f) less 10 percentage",
                    "5.K.(2)(a)(ii)(3)(f) over 150 percentage", "5.K.(2)(b)(1) 10-149",
                    "5.K.(2)(b)(1) less 10", "5.K.(2)(b)(1) over150", "5.K.(2)(b)(2) 10-149",
                    "5.K.(2)(b)(2) less 10", "5.K.(2)(b)(2) over150", "5.K.(4) - Total amount of custodians that hold 10% or more of separately managed assets",
                    "5.K.(4) - Total number of custodians that hold 10% or more of separately managed assets",
                    "5K(1)", "5K(2)", "5K(3)", "5K(4)", "9C Unqual Opinion", "Acquired Firm",
                    "Acquired Firm CRD#", "Acquired Firm SEC#", "Total Custody Amount",
                    "Total Number of Acquired Firms", "Total number of relying advisers",
                    "Umbrella Registration",
                    "firm_ia_disclosure_fl", "ia_firm_name",
                    "5L(1)(a)", "5L(1)(b)", "5L(1)(c)", "5L(1)(d)", "5L(1)(e)",
                    "5L(2)", "5L(3)", "5L(4)"

                    ),
        nameActual =c("idRegionSEC", "idCRD", "idSEC", "typeRegulationSEC", "nameEntityManager",
                      "nameEntityManagerLegal", "addressStreet1OfficePrimary", "addressStreet2OfficePrimary",
                      "cityOfficePrimary", "stateOfficePrimary", "countryOfficePrimary",
                      "zipcodeOfficePrimary", "phoneOfficePrimary", "faxOfficePrimary",
                      "addressStreet1OfficeMail", "addressStreet2OfficeMail", "cityOfficeMail",
                      "stateOfficeMail", "countryOfficeMail", "zipcodeOfficeMail", "statusSEC",
                      "dateStatusSEC", "stateDateJurisdictionNotice", "dateADVLatest",
                      "dateFormVersion", "hasEntityMultipleURLs", "urlManager", "isForeignRegisteredEntity",
                      "isSECSection12_15Reporter", "idCIK", "hasAUMGreater1B", "idLEI",
                      "hasAUMGreater100M", "hasAUMUnder100MOver25m", "hasPrincipalOfficeWY",
                      "hasPrincipalOfficeForeign", "isAdviser1940InvestmentActCompany",
                      "isAdviserBusinessDevelopmentCompany25MInCapital", "isAdviserPensionCapitalGreater200M",
                      "isAdviserRelated203A", "isAdviserNew203A", "isAdviserMultiState203A",
                      "isAdviserInternet", "hasSECOrderProhibitingRegistration", "isAdviserSECIneligible",
                      "typeEntity", "typeEntityDetail", "monthFiscalYearEnd", "stateEntityOrganized",
                      "countryEntityOrganized", "countEmployeesTotal", "countEmployeesInvestmentAdvisory",
                      "countEmployeesBrokerDealer", "countEmployeesStateRegisteredInvestmentAdviser",
                      "countEmployeesStateRegisteredInvestmentAdviserMultipleEntities",
                      "countEmployeesLicensedInsuranceAgents", "countEmployeesSolicitAdvisoryClients",
                      "rangeClients", "countClientsOver100Rounded", "pctClientsForeign",
                      "rangeClientsIndividualNonHighNetWorth", "rangeClientsIndividualHighNetWorth",
                      "rangeClientsBankThrift", "rangeClientsInvestmentCompany", "rangeClientsBusinessDevelopmentCompany",
                      "rangeClientsPooledInvestmentVehicle", "rangeClientsPensionPlan",
                      "rangeClientsCharitableOrganization", "rangeClientsCorporationOther",
                      "rangeClientsStateMunicipalGovernment", "rangeClientsInvestmentAdviserOther",
                      "rangeClientsInsuranceCompany", "rangeClientsOther", "typeClientsOther",
                      "rangeAUMIndividualNonHighNetWorth", "rangeAUMIndividualHighNetWorth",
                      "rangeAUMBankThrift", "rangeAUMInvestmentCompany", "rangeAUMBusinessDevelopmentCompany",
                      "rangeAUMPooledInvestmentVehicle", "rangeAUMPensionPlan", "rangeAUMCharitableOrganization",
                      "rangeAUMCorporationOthr", "rangeAUMStateMunicipalGovernment",
                      "rangeAUMInvestmentAdviserOther", "rangeAUMInsuranceCompany",
                      "rangeAUMOther", "typeAUMOther", "hasFeeAUM", "hasFeeHourlyCharge",
                      "hasFeeSubscription", "hasFeeFixed", "hasFeeCommission", "hasFeePerformance",
                      "hasFeeOther", "typeFeeOther", "isManagerSecuritiesPortfolio",
                      "amountAUMDiscretionary", "amountAUMNonDiscretionary", "amountAUMTotal",
                      "countAccountsDiscretionary", "countAccountsNonDiscretionary",
                      "countAccountsTotal", "hasFinancialPlanning", "hasPortfolioManagementIndividualSmallBusiness",
                      "hasPortfolioManagementInvestmentCompanies", "hasPortfolioManagementPooledInvestmentVehicles",
                      "hasPortfolioManagementInstitutionalClients", "hasServicePensionConsulting",
                      "hasServiceInvestmentAdviserSelection", "hasServicePeriodicalPublication",
                      "hasServiceSecurityRating", "hasServiceMarketTiming", "hasServiceEducationSeminars",
                      "hasServiceOther", "typeServiceOther", "rangeClientsFinancialPlanning",
                      "countClientsFinancialPlanningOver500Rounded", "hasFeeWrapSponsor",
                      "hasFeeWrapPortfolioManager", "isAdviserLimitedInvestmentTypes",
                      "isBrokerDealer", "isBrokerDealerRepresentative", "isCommodityPoolOperator",
                      "isFuturesMerchant", "isRealEstateBrokerDealerAgent", "isInsuranceBrokerAgent",
                      "isBank", "isTrustCompany", "isRegisteredMunicipalAdviser", "isRegisteredSecuritySwapDealer",
                      "isRegistredSecuritySwapParticipant", "isAccountingFirm", "isLawFirm",
                      "isOtherFinancialProductSalesperson", "typeOtherFinancialProductSalesperson",
                      "isBusinessActiveNonListedActivity", "typeBusinessActiveNonListedActivity",
                      "hasProductNonInvestmentAdvice", "hasRelatedBrokerDealer", "hasRelatedInvestmentAdviserOthr",
                      "hasRelatedRegisteredMunicipalAdviser", "hasRelatedRegisteredSecuritySwapDealer",
                      "hasRelatedRegisteredSecuritySwapParticipant", "hasRelatedRegisteredCommodityPoolOperator",
                      "hasRelatedRegisteredFuturesMerchant", "hasRelatedBankThrift",
                      "hasRelatedTrust", "hasRelatedAccountingFirm", "hasRelatedLawFirm",
                      "hasRelatedInsuranceCompany", "hasRelatedPensionConsultant",
                      "hasRelatedRealEstateBrokerDealer", "hasRelatedLimitedPartnershipSyndicator",
                      "hasRelatedGeneralPartnerManagingMemberSyndicator", "hasRelatedPrivateFundAdviser",
                      "isSecuritiesBuyerFromClientsForSelfToClientsFromOwned", "isSecuritiesFirmBoughSoldClientRecommended",
                      "isSecuritiesClientRecommendedFirmOwnedSecurity", "hasTradeExecutionClient",
                      "isSecuritiesUnderwriterPurchaserManagerClientRecommendedSecurity",
                      "hasRecommendedPurchaseSaleFirmOwnedSecurity", "hasClientDiscretionBuySell",
                      "hasClientDiscretionBuySellAmount", "hasClientDiscretionBrokerSelection",
                      "hasClientDiscretionCommisionCost", "isClientBrokerRelatedParty",
                      "hasClientBrokerRecommendation", "isClientBrokerRecommenationRelatedParty",
                      "isBrokerSoftDollarRecipient", "isBrokerSoftDollarEligibleResearchService",
                      "hasCompensationForClientReferrals", "isCompensatedForClientReferrals",
                      "hasCustodyClientCash", "hasCustodyClientSecurities", "amountAUMClientSecurities",
                      "countCustodyClients", "hasAdvisoryCustodyClientCash", "hasAdvisoryCustodyClientSecurities",
                      "amountAUMAdvisoryClientCash", "countAdvisoryCustodyClientCash",
                      "hasQualifiedCustodianSendInvestorQuarterlyReports", "hasIndependentAccountAuditPooledInvestments",
                      "hasIndependentAccountSurpriseAuditClientFunds", "hasIndependentAccountantPrepareInternalControlReports",
                      "isQualifiedCustodian", "hasRelatedQualifiedCustodian", "monthYearLastSurpriseAudit",
                      "countQualifiedCustodians", "hasControlPersonUnnamed", "hasManagementSupervisedPersonEvent",
                      "hasFelonyPleaConviction", "hasFelonyCharge", "hasMisdemeanorPleaConviction",
                      "hasMisdemeanrCharge", "hasSEC_CFTCFalseStatementOmission",
                      "hasSEC_CFTCStatuteViolation", "hasSEC_CFTCAuthorizationAction",
                      "hasSEC_CFTCOrderAgainst", "hasSEC_CFCPenaltyCeaseDesist",
                      "hasFederalStateForeignFalseStatement", "hasFederalStateForeignInvestmentViolation",
                      "hasFederalStateForeignBusinessRevokeSuspended", "hasFederalStateForeignOrderAgainst",
                      "hasFederalStateForeignLicenseRevoked", "hasSelfRegulatedBodyFalseStatement",
                      "hasSelfRegulatedBodyRuleViolation", "hasSelfRegulatedBodyBusinessRevokeSuspension",
                      "hasSelfRegulatedBodyActivityBan", "hasAttorneyAccountantFederalContractorPriorBanRevoke",
                      "isSubjectToRegulatoryProceeding", "hasDomesticForeignCourtEnjoinedInvestmentActivity",
                      "hasDomesticForeignCourtGuiltyStatuteViolation", "hasDomesticForeignCourtDismissedActionSettlementPursuant",
                      "isManagerDomesticForeignCourtSubjectToProceeding", "nameSECRegion",
                      "idCRD", "idSEC", "addressStreet1OfficePrimary", "addressStreet1OfficePrimary2",
                      "cityStateZipOfficePrimary", "cityStateZipOfficeMail", "nameContact",
                      "phoneOfficePrimary", "typeEntity", "descriptionManagerServices",
                      "statusSEC", "dateStatusSEC", "countCriminalDisclosures", "countRegulatoryActions",
                      "countCivilDisclosures", "countBankruptcyDisclosures", "countJudgementLiens",
                      "countBondPayoutDisclosures", "statusSEC", "dateStatusSEC", "urlManager",
                      "hasMultipleEntityURLs", "countEmployeesTotalOver1000", "countEmployeesInvestmentAdvisoryOver1000",
                      "countEmployeesBrokerDealerOver1000", "countEmployeesStateRegisteredInvestmentAdviserOver1000",
                      "rangeClients", "countClientsOver500", "rangeClientsIndividualNonHighNetWorth",
                      "rangeClientsIndividualHighNetWorth", "rangeClientsBankThrift",
                      "rangeClientsInvestmentCompany", "rangeClientsBusinessDevelopmentCompany",
                      "rangeClientsPooledInvestmentVehicle", "rangeClientsPensionPlan",
                      "rangeClientsCharitableOrganization", "rangeClientsCorporationOther",
                      "rangeClientsStateMunicipalGovernment", "rangeClientsInvestmentAdviserOther",
                      "typeServicesOther", "typeBusinessOther", "hasCustodyClientCash",
                      "hasCustodyClientSecurities", "amountAUMAdvisoryClientCashSecurities",
                      "countAdvisoryCustodyClientCashSecurities", "hasAdvisoryCustodyClientCashSecurities",
                      "hasControlPersonUnnamed", "statusSEC", "statusFINRA", "hasExemptionAsSolelyVentureAdviser",
                      "hasExemptionAsPrivateFundManagerUnder150MAUM", "hasExemptionSoleyPrivateFundManagerAUMOver150M",
                      "countOfficesOther", "rangeAssetsOther", "countClientsIndividualNonHighNetWorth",
                      "hasClientsLessThan5IndividualNonHighNetWorth", "amountRegulatoryAUMClientIndividualNonHighNetWorth",
                      "countClientsIndividualHighNetWorth", "hasClientsLessThan5IndividualHighNetWorth",
                      "amountRegulatoryAUMIndividualHighNetWorth", "countClientsBankingThrift",
                      "hasClientsLessThan5BankingThrift", "amountRegulatoryAUMBankingThrift",
                      "countClientsInvestmentCompanies", "amountRegulatoryAUMInvestmentCompanies",
                      "countClientsBusinessDevelopmentCompanies", "amountRegulatoryAUMBusinessDevelopmentCompanies",
                      "countClientsPooledInvestmentCompanies", "amountRegulatoryAUMPooledInvestmentCompanies",
                      "countClientsPensionPlans", "hasClientsLessThan5PensionPlans",
                      "amountRegulatoryAUMPensionPlans", "countClientsCharitableOrgs",
                      "hasClientsLessThan5CharitableOrgs", "amountRegulatoryAUMCharitableOrgs",
                      "countClientsMunicipalEntities", "hasClientsLessThan5MunicipalEntities",
                      "amountRegulatoryAUMMunicipalEntities", "countClientsInvestmentAdvisorsOther",
                      "hasClientsLessThan5InvestmentAdvsiorsOther", "amountRegulatoryAUMInvestmentAdvisorsOther",
                      "countClientsInsuranceCompanies", "hasClientsLessThan5InsuranceCompanies",
                      "amountRegulatoryAUMInsuranceCompanies", "countClientsSovereignWealthFunds",
                      "hasClientsLessThan5SovereignWealthFunds", "amountRegulatoryAUMSovereignWealthFunds",
                      "countClientsCorporationsOther", "hasClientsLessThan5CorporationsOther",
                      "amountRegulatoryAUMCorporationsOther", "countClientsOther",
                      "hasClientsLessThan5Other", "amountRegulatoryAUMOOther", "descriptionOther",
                      "amountRegulatoryAUMNonUSPersons", "amountRegulatoryAUMWrapFee",
                      "amountRegulatoryAUMWrapFeePortfolioManager", "amountRegulatoryAUMWrapFeeSponsor",
                      "hasLimitedInvestmentAdvice", "hasClientReportingDifferentThanIAPD",
                      "hasEmployeeClientReferralFees", "hasEmployeeCompensationClientReferralFees",
                      "hasQualified7ACustomers", "idCRDAdditional", "nameRegulatoryContactAdditional",
                      "cityRegulatoryContactAdditional", "countryRegulatoryContactAdditional",
                      "emailRegulatoryContactAdditional", "faxRegulatoryContactAdditional",
                      "zipcodeRegulatoryContactAdditional", "stateRegulatoryContactAdditional",
                      "addressStreet1RegulatoryContactAdditional", "addressStreet2RegulatoryContactAdditional",
                      "phoneRegulatoryContactAdditional", "titleRegulatoryContactAdditional",
                      "amountUSPrivateFundAssetsERA", "hasHedgeFunds", "hasLiquidityFunds",
                      "hasFundsOther", "hasPEFunds", "hasPFMasterFund", "hasRealEstateFunds",
                      "hasSecuritizedFunds", "hasVCFunds", "cityChiefComplianceOfficer",
                      "countryChiefComplianceOfficer", "emailChiefComplianceOfficer",
                      "faxChiefComplianceOfficer", "nameChiefComplianceOfficer",
                      "titleOtherChiefComplianceOfficer", "zipcodeChiefComplianceOfficer",
                      "stateChiefComplianceOfficer", "addressStreet1ChiefComplianceOfficer",
                      "addresssStreet2ChiefComplianceOfficer", "phoneChiefComplianceOfficer",
"isRelatedPersonControlledManager", "countDisclosuresFelonyPleaConviction",
"countDisclosuresFelonyCharge", "countDisclosuresMisdemeanorPleaConviction",
"countDisclosuresMisdemeanorCharge", "countDisclosuresSEC_CFTCFalseStatementOmission",
                      "countDisclosuresSEC_CFTCStatuteViolation", "countDisclosuresSEC_CFTCAuthorizationAction",
                      "countDisclosuresSEC_CFTCOrderAgainst", "countDisclosuresSEC_CFCPenaltyCeaseDesist",
                      "countDisclosuresFederalStateForeignFalseStatement", "countDisclosuresFederalStateForeignInvestmentViolation",
                      "countDisclosuresFederalStateForeignBusinessRevokeSuspended", "countDisclosuresFederalStateForeignOrderAgainst",
                      "countDisclosuresFederalStateForeignLicenseRevoked", "countDisclosuresSelfRegulatedBodyFalseStatement",
                      "countDisclosuresSelfRegulatedBodyRuleViolation", "countDisclosuresSelfRegulatedBodyBusinessRevokeSuspension",
                      "countDisclosuresSelfRegulatedBodyActivityBan", "countDisclosuresAttorneyAccountantFederalContractorPriorBanRevoke",
                      "countDisclosuresSubjectToRegulatoryProceeding",
                      "countDisclosuresDomesticForeignCourtEnjoinedInvestmentActivity",
                      "countDisclosuresDomesticForeignCourtGuiltyStatuteViolation",
                      "countDisclosuresDomesticForeignCourtDismissedActionSettlementPursuant",
                      "countDisclosuresDomesticForeignCourtSubjectToProceeding",
                      "countAffiliatesBusinessDevelopment", "countControlPersonsPublicCompany",
                      "countAffiliatesInvestmentAdvisors", "countAffiliatesInvestmentAdvisorsBusinesDevelopmentTotal", "countPrivateFunds7B1",
                      "countPrivateFunds7B2", "idEINCompanyCOO",
                      "dateADVFilingFirast",
                      "cityBooks",
                      "countryBooks", "zipcodeBooks",
                      "stateBooks", "addressStreet1Books",
                      "addressStreet2Books",
                      "isMailOfficeAddressResidence",
                      "isMainOfficeAddressResidence", "nameEntityCOOCompensator",
                      "isSharedOffice", "hasSharedSupervisedPersons", "citySoleProprietor",
                      "countrySoleProprietor", "zipcodeSoleProprietor", "stateSoleProprietor",
                      "addressStreet1SoleProprietor", "addressStreet2SoleProprietor",
                      "amountAUMTotal", "countCRDs",
                      "countRecordsLocations", "countCIKs",
                      "countHedgeFunds", "countLiquidityFunds",
                      "countFundsOther", "countPEFunds", "countRealEstateFunds",
                      "countSecuritizedFunds", "countVentureFunds",
                      "countWebsites", "isUnderCommonControl",
"hasOverAssets5M", "hasControlOtherAdvisorOverAssets25M", "hasControlOtherAdvisorOverAssets5M", "isControlledByAdvisorOverAssets25M", "isControlledByAdvisorOverAssets5M", "hasLegalStatusChanged", "dateLegalStatusChange",
"countParallelAssets", "countRIC_BIC",
"countWrapFeePrograms",
"pctManagedAccountExchangeTradedSecuritiesYearEnd",
"pctManagedAccountExchangeTradedSecuritiesYearMid", "pctManagedAccountNonExchangeTradedSecuritiesYearEnd",
"pctManagedAccountNonExchangeTradedSecuritiesYearMid",
"pctManagedAccountBondsGovernmentAgencyYearEnd",
"pctManagedAccountBondsGovernmentAgencyYearMid",
"pctManagedAccountBondsStateLocalYearEnd",
"pctManagedAccountBondsStateLocalYearMid",
"pctManagedAccountSecuritiesIC_BCYearEnd",
"pctManagedAccountSecuritiesIC_BCYearMid",
"pctManagedAccountBondsSovereignYearEnd",
"pctManagedAccountBondsSovereignYearMid",
"pctManagedAccountBondsCorporateInvestmentGradeYearEnd",
"pctManagedAccountBondsCorporateInvestmentGradeYearMid",
"pctManagedAccountBondsCorporateNonInvestmentGradeYearEnd",
"pctManagedAccountBondsCorporateNonInvestmentGradeYearMid",
"pctManagedAccountDerivativesYearEnd",
"pctManagedAccountDerivativesYearMid",
"pctManagedAccountSecuritiesPooledInvestmentYearEnd",
"pctManagedAccountSecuritiesPooledInvestmentYearMid",
"pctManagedAccountCashYearEnd",
"pctManagedAccountCashYearMid",
"descriptionManagedAccountOther",
"pctManagedAccountOtherYearEnd",
"pctManagedAccountOtherYearMid",
"pctManagedAccountExchangeTradedSecuritiesYearEndFinal",
"pctManagedAccountNonExchangeTradedSecuritiesYearEndFinal",
"pctManagedAccountBondsGovernmentAgencyYearEndFinal",
"pctManagedAccountBondsStateLocalYearEndFinal",
"pctManagedAccountSecuritiesIC_BCYearEndFinal",
"pctManagedAccountBondsSovereignYearEndFinal",
"pctManagedAccountBondsCorporateInvestmentGradeYearEndFinal",
"pctManagedAccountBondsCorporateNonInvestmentGradeYearEndFinal",
"pctManagedAccountDerivativesYearEndFinal",
"pctManagedAccountSecuritiesPooledInvestmentYearEndFinal",
"pctManagedAccountCashYearEnd",
"descriptionManagedAccountOtherFinal",
"pctManagedAccountOtherYearEndFinal",
"amountAUMRegulatory10_149PercentNotionalYearMid",
"amountAUMRegulatory10LessPercentNotionalYearMid",
"amountAUMRegulatory150OverPercentNotionalYearMid",
"amountBorrowings10_149PercentNotionalYearMid",
"amountBorrowings10LessPercentNotionalYearMid",
"amountBorrowings150OverPercentNotionalYearMid",
"pctDerivativeInterestRate10_149PercentNotionalYearMid",
"pctDerivativeInterestRateDerivative10LessPercentNotionalYearMid",
"pctDerivativeInterestRateDerivative150OverPercentNotionalYearMid",
"pctDerivativeFOREXD10_149PercentNotionalYearMid",
"pctDerivativeFOREXD10LessPercentNotionalYearMid",
"pctDerivativeFOREXD150OverPercentNotionalYearMid",
"pctDerivativeCredit10_149PercentNotionalYearMid",
"pctDerivativeCredit10LessPercentNotionalYearMid",
"pctDerivativeCredit150OverPercentNotionalYearMid",
"pctDerivativeEquity10_149PercentNotionalYearMid",
"pctDerivativeEquity10LessPercentNotionalYearMid",
"pctDerivativeEquity150OverPercentNotionalYearMid",
"pctDerivativeCommodity10_149PercentNotionalYearMid",
"pctDerivativeCommodity10LessPercentNotionalYearMid", "pctDerivativeCommodity150OverPercentNotionalYearMid",
"pctDerivativeOther10_149PercentNotionalYearMid", "pctDerivativeOther10LessPercentNotionalYearMid",
"pctDerivativeOther150OverPercentNotionalYearMid",
"amountAUMRegulatory10_149PercentNotionalYearEnd",
"amountAUMRegulatory10LessPercentNotionalYearEnd",
"amountAUMRegulatory150OverPercentNotionalYearEnd",
"amountBorrowings10_149PercentNotionalYearEnd",
"amountBorrowings10LessPercentNotionalYearEnd",
"amountBorrowings150OverPercentNotionalYearEnd",
"pctDerivativeInterestRate10_149PercentNotionalYearEnd",
"pctDerivativeInterestRateExposure10LessPercentNotionalYearEnd", "pctDerivativeInterestRate150OverPercentNotionalYearEnd",
"pctDerivativeFOREXDExposure10_149PercentNotionalYearEnd", "pctDerivativeFOREXDExposure10LessPercentNotionalYearEnd",
"pctDerivativeFOREXD150OverPercentNotionalYearEnd",
"pctDerivativeCreditExposure10_149PercentNotionalYearEnd",
"pctDerivativeCreditExposure10LessPercentNotionalYearEnd",
"pctDerivativeCredit150OverPercentNotionalYearEnd",
"pctDerivativeEquityExposure10_149PercentNotionalYearEnd", "pctDerivativeEquityExposure10LessPercentNotionalYearEnd",
"pctDerivativeEquity150OverPercentNotionalYearEnd",
"pctDerivativeCommodityExposure10_149PercentNotionalYearEnd",
"pctDerivativeCommodity10LessPercentNotionalYearEnd",
"pctDerivativeCommodity150OverPercentNotionalYearEnd",
"pctDerivativeOther10_149PercentNotionalYearEnd", "pctDerivativeOther10LessPercentNotional",
"pctDerivativeOther150OverPercentNotionalYearEnd",
"amountBorrowings10_149PercentNotionalYearEndSubAdviser",
"amountAUMRegulatory10LessPercentNotionalYearSubAdviser", "amountAUMRegulatory150OverPercentNotionalYearEndSubAdviser",
"amountBorrowings10_149PercentNotionalYearEndSubAdviser",
"amountBorrowings10LessPercentNotionalYearEndSubAdviser",
"amountBorrowings150OverPercentNotionalYearEndSubAdviser",
"amountCustodiansHoldingOver10PCTAssetsManaged",
"countCustodiansHoldingOver10PCTAssetsManaged",
"hasSeperateAccounts", "hasSeperateAccountsWithBorrowingsDerivatives", "hasSeperateAccountCustodians", "has5K4", "hasUnEqualAccountingOpinion", "nameFirmAcquired",
"idCRDFirmsAcquired", "idSECFirmsAcquired", "amountCustodyAUM",
"countFirmsAcquired", "countRelyingAdvisors",
"hasUmbrellaRegistration",
"hasFirmIADisclosure", "nameEntityManagerIA",
"has5L1a", "has5L1b", "has5L1c", "has5L1d", "has5L1e",
"has5L2", "has5L3", "has5L4"

        )
      )
    sec_name_df
  }

.parse_adv_excel_data <-
  function(file_path = "/Users/alexbresler/Desktop/adv_data/ia080116.xlsx") {
    file_path %>%
      readxl::read_excel()

  }

.parse_adv_csv <-
  function(file_path = "/Users/alexbresler/Desktop/adv_data/IA FOIA Download 7-30-10.CSV") {
    file_path %>%
      read_csv()
  }
.parse_adv_txt_data <-
  function(file_path = "/Users/alexbresler/Desktop/adv_data/5010912_10044_00050000_00050000.txt") {

    file_path %>%
      read_delim(delim = '|', col_names = T) %>%
      dplyr::select(-dplyr::matches("X26"))

    data
  }

.parse_sec_adv_data_url <-
  function(url = 'https://www.sec.gov/foia/iareports/ia090116.zip',
           return_message = TRUE) {
    base_url <- url %>% basename()

    is_exempt <-
      url %>% str_detect("exempt")

    tmp <-
      tempfile()

    # Use curl with cookie handling to bypass SEC Akamai CDN protection
    h <- curl::new_handle()
    curl::handle_setopt(h,
      useragent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36",
      followlocation = TRUE,
      ssl_verifypeer = TRUE,
      cookiefile = "",
      cookiejar = "",
      httpheader = c(
        "Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
        "Accept-Language: en-US,en;q=0.9",
        "Accept-Encoding: gzip, deflate, br",
        "Connection: keep-alive",
        "Upgrade-Insecure-Requests: 1",
        "Sec-Fetch-Dest: document",
        "Sec-Fetch-Mode: navigate",
        "Sec-Fetch-Site: none",
        "Sec-Fetch-User: ?1"
      )
    )

    download_success <- tryCatch({
      curl::curl_download(url, tmp, handle = h)
      TRUE
    }, error = function(e) {
      if (return_message) {
        glue::glue("Failed to download {url}: {e$message}") %>% message()
      }
      FALSE
    })

    if (!download_success || !file.exists(tmp) || file.info(tmp)$size == 0) {
      if (return_message) {
        glue::glue("Failed to download {url}") %>% message()
      }
      return(tibble())
    }

    # Check if file is a direct xlsx (not zipped)
    is_direct_xlsx <- url %>% str_detect("\\.xlsx$")

    if (is_direct_xlsx) {
      # Parse xlsx directly
      adv_data <- tryCatch({
        tmp %>% .parse_adv_excel_data()
      }, error = function(e) {
        if (return_message) message(glue::glue("Failed to parse xlsx: {e$message}"))
        tibble()
      })
    } else {
      # Unzip and parse
      con <- tryCatch({
        unzip(tmp)
      }, error = function(e) {
        if (return_message) message(glue::glue("Failed to unzip: {e$message}"))
        return(character(0))
      })

      if (length(con) == 0) {
        return(tibble())
      }

      is_excel <-
        con %>% stringr::str_detect("XLS|xls|xlsx|XLSX") %>% sum(na.rm = TRUE) > 0

      if (is_excel) {
        adv_data <-
          con[[1]] %>% .parse_adv_excel_data()
      }

      is_csv <-
        con %>% str_detect("csv|CSV") %>% sum(na.rm = TRUE) > 0
      if (is_csv) {
        adv_data <-
          con %>%
          .parse_adv_csv() %>%
          suppressWarnings()
      }
      is_txt <-
        con %>% str_detect("txt|TXT") %>% sum(na.rm = TRUE) > 0
      if (is_txt) {
        adv_data <-
          con %>%
          .parse_adv_txt_data()
      }

      con %>%
        unlink()
    }

    # Clean up temp file
    unlink(tmp)

    # Check if adv_data was successfully parsed
    if (!exists("adv_data") || nrow(adv_data) == 0) {
      return(tibble())
    }

    actual_names <-
      .assign_sec_names(data = adv_data)

    if (adv_data %>% tibble::has_name("5H__1")) {
      adv_data <-
        adv_data %>%
        dplyr::rename(`5I(1)` = `5H__1`)
    }

    adv_data <-
      adv_data %>%
      purrr::set_names(actual_names
      )

    column_ids <-
      tibble(nameActual = names(adv_data)) %>%
      mutate(idRow = 1:n()) %>%
      group_by(nameActual) %>%
      filter(idRow == min(idRow)) %>%
      pull(idRow)

    adv_data <-
      adv_data[, column_ids] %>%
      as_tibble() %>%
      select(idCRD, dplyr::matches("^nameEntityManager"), everything())

    has_columns <-
      (
        adv_data %>%
          dplyr::select(-dplyr::matches("country")) %>%
          dplyr::select(dplyr::matches("^count[A-Z]")) %>% ncol() > 0
      ) &
      (
        adv_data %>%
          dplyr::select(-dplyr::matches("country")) %>%
          dplyr::select(dplyr::matches("^count[A-Z]")) %>%
          future_map(class) %>%
          as_tibble() %>%
          pivot_longer(cols = everything(), names_to = "column", values_to = "class") %>%
          dplyr::filter(class == 'character') %>% nrow() > 0
      )

    if (has_columns) {
      change_to_range_cols <-
        adv_data %>%
        dplyr::select(-dplyr::matches("country")) %>%
        dplyr::select(dplyr::matches("^count[A-Z]")) %>%
        future_map(class) %>%
        as_tibble() %>%
        pivot_longer(cols = everything(), names_to = "column", values_to = "class") %>%
        dplyr::filter(class == 'character') %>%
        .$column
      for (x in seq_along(change_to_range_cols)) {
        name_loc <-
          change_to_range_cols[x] %>% grep(names(adv_data)) %>% min

        names(adv_data)[name_loc] <-
          names(adv_data)[name_loc] %>% str_replace("count", 'range')
      }
    }

    if (adv_data %>% dplyr::select(dplyr::matches("^has[A-Z]|^is[A-Z]")) %>% names() %>% length() > 0) {
      adv_data <-
        adv_data %>%
        mutate(across(matches("^has[A-Z]|^is[A-Z]"), str_trim)) %>%
        mutate(across(matches("^has[A-Z]|^is[A-Z]"), ~if_else(. == "Y", TRUE, FALSE))) %>%
        suppressWarnings()
    }

    if (any(str_detect(names(adv_data), "^url[M]"))) {
      adv_data <-
        adv_data %>%
        mutate(across(matches("^url[M]"), str_to_lower)) %>%
        suppressWarnings()
    }

    status_cols <- names(adv_data)[str_detect(names(adv_data), "^status[SEC]") &
                                     !str_detect(names(adv_data), "date")]
    if (length(status_cols) > 0) {
      adv_data <-
        adv_data %>%
        mutate(across(all_of(status_cols), stringr::str_to_upper)) %>%
        suppressWarnings()
    }

    whack_date <-
      adv_data %>%
      dplyr::select(dplyr::matches("^date")) %>%
      keep(is.character) %>%
      names() %>% length() > 0

    if (whack_date) {
      char_col <-
        adv_data %>%
        dplyr::select(dplyr::matches("^date")) %>%
        keep(is.character) %>%
        names

      adv_data[, char_col] <-
        adv_data[, char_col] %>%
        pull(1) %>%
        lubridate::mdy()


    }

    adv_data <-
      adv_data %>%
      mutate(across(matches("^type[A-Z]|^range[A-Z]|^address[A-Z]|^city[A-Z]|^zip[A-Z]|^fax[A-Z]|^name[A-Z]|^state[A-Z]|^status[A-Z]|monthYearLastSurpriseAudit|^date[A-Z]"),
                    as.character)) %>%
      mutate(idCRD = idCRD %>% as.integer())

    if ('idLEI' %in% names(adv_data)) {
      if (adv_data$idLEI %>% class() == 'numeric') {
        adv_data <-
          adv_data %>%
          mutate(idLEI = idLEI %>% as.character() %>% str_replace_all("N/A", NA))
      }
    }

    if (adv_data %>% tibble::has_name("countClientsFinancialPlanningOver500Rounded")) {
      if (adv_data$countClientsFinancialPlanningOver500Rounded %>% class() == "logical") {
        adv_data <-
          adv_data %>% dplyr::select(-countClientsFinancialPlanningOver500Rounded)
      }
    }

    if (adv_data %>% tibble::has_name("idCIK")) {
      if (adv_data$idCIK %>% class() == "logical") {
        adv_data <- adv_data %>% dplyr::select(-idCIK)
      }
    }

    if ('rangeClientsFinancialPlanning' %in% names(adv_data)) {
      if (adv_data$rangeClientsFinancialPlanning %>% class() == 'numeric') {
        adv_data <-
          adv_data %>%
          mutate(rangeClientsFinancialPlanning = rangeClientsFinancialPlanning %>% as.character())
      }
    }

    if (return_message) {
      glue::glue("Parsed {url}") %>% cat(fill = T)
    }

    adv_data <-
      adv_data %>%
      mutate(isExempt = is_exempt) %>%
      dplyr::select(isExempt, everything())
    to_upper_names <- names(adv_data)[str_detect(names(adv_data),
      "^country|^name|^city|^state|^range[A-Z]|^type[A-Z]")]
    adv_data <-
      adv_data %>%
      mutate(across(all_of(to_upper_names), stringr::str_to_upper)) %>%
      mutate(urlZip = url)

    adv_data
  }

.parse_adv_urls <- function(urls = 'https://www.sec.gov/foia/iareports/ia090116.zip', return_message = TRUE) {
  df <-
    tibble()
  success <- function(res) {



    parse_sec_adv_data_url_safe <-
      purrr::possibly(.parse_sec_adv_data_url, tibble())

    page_url <- res$url

      glue::glue("parsing {page_url}") %>% cat(fill = T)

    data <-
      page_url %>%
      parse_sec_adv_data_url_safe(return_message = return_message)

    df <<-
      df %>%
      bind_rows(data)
  }

  failure <- function(msg) {
    tibble()
  }


  urls %>%
    future_map(function(x) {
      curl_fetch_multi(url = x, success, failure)
    })
  multi_run()
  df
}

#' ADV managers periods data
#'
#' This function returns monthly summary
#' information for every ADV filing manager
#' from 2006 onwards.
#'
#' @param periods dates in year-month form (e.g., "2018-06")
#' @param all_periods Logical. If \code{TRUE}, includes all available periods.
#' @param only_most_recent Logical. If \code{TRUE}, returns only the most recent period.
#' @param include_exempt Logical. If \code{TRUE} (default), includes exempt filers.
#' @param nest_data Logical. If \code{TRUE}, returns a nested data frame.
#' @param return_message Logical. If \code{TRUE}, returns progress messages.
#' @param parallel Logical. If \code{TRUE} (default), uses parallel processing via furrr.
#'
#' @import dplyr stringr lubridate readr readxl rvest purrr httr tidyr tibble glue
#' @importFrom curl curl_download
#' @returns When \code{nest_data} is \code{TRUE}, a nested tibble by period and type of filer.
#'   When \code{nest_data} is \code{FALSE}, a flat tibble with ADV summary data.
#' @export
#' @family IAPD
#' @family ADV
#' @family entity search
#' @family fund data
#' @examples
#' \dontrun{
#' adv_managers_periods_summaries(periods = c("2006-06", "2016-12", "2017-01"),
#'   all_periods = FALSE, include_exempt = TRUE, only_most_recent = FALSE, nest_data = FALSE)
#'
#' adv_managers_periods_summaries(only_most_recent = TRUE)
#' }
adv_managers_periods_summaries <-
  function(periods = c("2018-06"),
           all_periods = FALSE,
           only_most_recent = FALSE,
           include_exempt = TRUE,
           nest_data = FALSE,
           return_message = TRUE,
           parallel = TRUE) {
    if (!'sec_adv_url_df' %>% exists()) {
      sec_adv_url_df <-
        adv_period_urls()

      assign(x = 'sec_url_df', eval(sec_adv_url_df),  envir = .GlobalEnv)
    }

    df_urls <- sec_adv_url_df

    if (!include_exempt) {
      df_urls <-
        df_urls %>%
        filter(!isExempt)
    }

    if (periods %>% length() > 0) {
      urls <-
        df_urls %>%
        filter(periodData %in% periods) %>%
        pull(urlZip)
    }

    if (all_periods) {
      urls <-
        df_urls %>%
        pull(urlZip)
    }

    if (only_most_recent) {
      urls <-
        df_urls %>%
        filter(dateData == max(dateData)) %>%
        pull(urlZip)
    }

    .parse_adv_urls_safe <-
      purrr::possibly(.parse_adv_urls, tibble())

    all_adv_data <-
      .parse_adv_urls(urls = urls, return_message = return_message)

    if (all_adv_data %>% nrow() == 0) {
      return(all_adv_data)
    }

      all_adv_data <-
        all_adv_data %>%
        dplyr::select(-isExempt) %>%
        left_join(df_urls) %>%
        suppressMessages()

      all_adv_data <-
        all_adv_data %>%
        select(-any_of(c("periodReport", "typeReport"))) %>%
        select(any_of(
          c(
            "isExempt",
            "periodData",
            "yearData",
            "quarterData",
            "dateData"
          )

        ),  everything())

      # Define columns for uppercase (excluding stateEntityOrganized)
      upper_cols <- names(all_adv_data)[str_detect(names(all_adv_data),
        "^name[E]|^address|^city|^status|^state|^type|^country[A-Z]"
      ) & !str_detect(names(all_adv_data), "stateEntityOrganized")]

      # Define count columns (excluding country)
      count_cols <- names(all_adv_data)[str_detect(names(all_adv_data), "^count[A-Z]") &
                                          !str_detect(names(all_adv_data), "country")]

      all_adv_data <-
        all_adv_data %>%
        mutate(across(matches("^date[A-Z]"), ~lubridate::ymd(.))) %>%
        mutate(across(all_of(upper_cols), str_to_upper))

      all_adv_data <-
        all_adv_data %>%
        mutate(across(all_of(count_cols), ~formattable::comma(., digits = 0)))

      has_amounts <-
        any(str_detect(names(all_adv_data), "^amount"))

      if (has_amounts) {
        all_adv_data <-
          all_adv_data %>%
          mutate(across(matches("^amount[A-Z]"), ~as.numeric(.) %>% formattable::currency())) %>%
          arrange(dateData, desc(amountAUMTotal)) %>%
          suppressWarnings()
      }

      all_adv_data <-
        all_adv_data %>%
        dplyr::rename(nameEntityManagerBusiness = nameEntityManager) %>%
        mutate(nameEntityManager = nameEntityManagerLegal) %>%
        dplyr::select(
          dateData:idSEC,
          nameEntityManager,
          nameEntityManagerLegal,
          nameEntityManagerBusiness,
          everything()
        ) %>%
        suppressMessages()

      all_adv_data <-
        all_adv_data %>%
        mutate(across(matches("^address|^country[A-Z]|^city|^state"), stringr::str_to_upper))
      has_office_sum <-
        names(all_adv_data) %>% str_count('^addressStreet2OfficePrimary') %>% sum() > 0

      if (has_office_sum) {
        addressOfficePrimary <-
          all_adv_data %>%
          tidyr::replace_na(list(
            addressStreet2OfficePrimary = '',
            stateOfficePrimary = ''
          )) %>%
          mutate(
            addressOfficePrimary =
              addressStreet1OfficePrimary %>% paste0(
                ' ',
                addressStreet2OfficePrimary,
                ' ',
                cityOfficePrimary,
                ', ',
                stateOfficePrimary,
                ', ',
                countryOfficePrimary
              ) %>% str_trim()
          ) %>%
          .$addressOfficePrimary

        all_adv_data <-
          all_adv_data %>%
          mutate(addressOfficePrimary) %>%
          dplyr::select(
            dateData:typeRegulationSEC,
            nameEntityManager,
            nameEntityManagerLegal,
            nameEntityManagerBusiness,
            addressOfficePrimary,
            everything()
          )
      }


    if (nest_data) {
      all_adv_data <-
        all_adv_data %>%
        nest(dataADV = -c(dateData, isExempt))
    }

      gc()


    all_adv_data
  }

#' ADV managers most recent summary data
#'
#' This function returns abbreviated ADV data for
#' all filing managers for the most recent period.
#'
#' For multiple periods and all information see \code{\link{adv_managers_periods_summaries}}
#'
#' @param select_names Character vector of column names to include in the output.
#'   Default includes key identifiers, contact info, AUM, and registration status fields.
#' @param return_message Logical. If \code{TRUE}, returns progress messages.
#'
#' @returns A tibble containing ADV summary data for the most recent period with
#'   columns specified by \code{select_names}.
#' @export
#' @family IAPD
#' @family ADV
#' @family entity search
#' @family fund data
#' @examples
#' \dontrun{
#' adv_managers_current_period_summary(select_names = c("dateDataADV", "isExempt", "idRegionSEC", "idCRD", "idSEC", "typeRegulationSEC", "nameEntityManager", "nameEntityManagerLegal", 'addressOfficePrimary', "addressStreet1OfficePrimary", "addressStreet2OfficePrimary", "cityOfficePrimary", "stateOfficePrimary", "countryOfficePrimary", "zipOfficePrimary", "phoneOfficePrimary", "statusSEC", "dateStatusSEC", "dateADVLatest", "urlManager", "isForeignRegisteredEntity", "stateDateJurisdictionNotice", "idCIK", "hasAUMGreater1B", "idLEI", "hasAUMGreater100M", "typeEntity", "countryEntityOrganized", "countEmployeesTotal", "countEmployeesInvestmentAdvisory", "amountAUMTotal", "amountAUMDiscretionary", "amountAUMNonDiscretionary", "countAccountsDiscretionary", "countAccountsNonDiscretionary", "countAccountsTotal", "isManagerSecuritiesPortfolio", "hasFeeAUM", "hasFeeHourlyCharge", "hasFeeSubscription", "hasFeeFixed", "hasFeeCommission", "hasFeePerformance", "hasFeeOther", "typeFeeOther", "isBrokerDealer", "isBrokerDealerRepresentative", "isCommodityPoolOperator", "isFuturesMerchant", "isRealEstateBrokerDealerAgent", "isInsuranceBrokerAgent", "isBank", "isTrustCompany", "isRegisteredMunicipalAdviser", "isRegisteredSecuritySwapDealer", "isRegistredSecuritySwapParticipant", "isAccountingFirm", "isLawFirm", "isOtherFinancialProductSalesperson", "typeOtherFinancialProductSalesperson", "countEmployeesBrokerDealer", "countEmployeesStateRegisteredInvestmentAdviser", "countEmployeesStateRegisteredInvestmentAdviserMultipleEntities", "countEmployeesLicensedInsuranceAgents", "countEmployeesSolicitAdvisoryClients", "hasFelonyPleaConviction", "hasFelonyCharge", "hasMisdemeanorPleaConviction"))
#' }

adv_managers_current_period_summary <-
  function(select_names = c(
    "dateDataADV",
    "isExempt",
    "idRegionSEC",
    "idCRD",
    "idSEC",
    "typeRegulationSEC",
    "nameEntityManager",
    "nameEntityManagerLegal",
    'addressOfficePrimary',
    "addressStreet1OfficePrimary",
    "addressStreet2OfficePrimary",
    "cityOfficePrimary",
    "stateOfficePrimary",
    "countryOfficePrimary",
    "zipOfficePrimary",
    "phoneOfficePrimary",
    "statusSEC",
    "dateStatusSEC",
    "dateADVLatest",
    "urlManager",
    "isForeignRegisteredEntity",
    "stateDateJurisdictionNotice",
    "idCIK",
    "hasAUMGreater1B",
    "idLEI",
    "hasAUMGreater100M",
    "typeEntity",
    "countryEntityOrganized",
    "countEmployeesTotal",
    "countEmployeesInvestmentAdvisory",
    "amountAUMTotal",
    "amountAUMDiscretionary",
    "amountAUMNonDiscretionary",
    "countAccountsDiscretionary",
    "countAccountsNonDiscretionary",
    "countAccountsTotal",
    "isManagerSecuritiesPortfolio",
    "hasFeeAUM",
    "hasFeeHourlyCharge",
    "hasFeeSubscription",
    "hasFeeFixed",
    "hasFeeCommission",
    "hasFeePerformance",
    "hasFeeOther",
    "typeFeeOther",
    "isBrokerDealer",
    "isBrokerDealerRepresentative",
    "isCommodityPoolOperator",
    "isFuturesMerchant",
    "isRealEstateBrokerDealerAgent",
    "isInsuranceBrokerAgent",
    "isBank",
    "isTrustCompany",
    "isRegisteredMunicipalAdviser",
    "isRegisteredSecuritySwapDealer",
    "isRegistredSecuritySwapParticipant",
    "isAccountingFirm",
    "isLawFirm",
    "isOtherFinancialProductSalesperson",
    "typeOtherFinancialProductSalesperson",
    "countEmployeesBrokerDealer",
    "countEmployeesStateRegisteredInvestmentAdviser",
    "countEmployeesStateRegisteredInvestmentAdviserMultipleEntities",
    "countEmployeesLicensedInsuranceAgents",
    "countEmployeesSolicitAdvisoryClients",
    "hasFelonyPleaConviction",
    "hasFelonyCharge",
    "hasMisdemeanorPleaConviction"
  ),
  return_message = TRUE) {
    adv_managers_periods_summaries_safe <-
      purrr::possibly(adv_managers_periods_summaries, tibble())

    all_data <-
      adv_managers_periods_summaries(only_most_recent = TRUE,
                                              include_exempt = TRUE)

    all_data <-
      all_data %>%
      dplyr::select(any_of(select_names))

    return(all_data)
  }



# mung --------------------------------------------------------------------

#' Extract fee references
#'
#' @param data A \code{tibble} with the OCR'd brochure data
#' @param word_threshhold numeric threshold for filtering fee percentage values
#' @param print_sentences if \code{TRUE} prints fee reference data
#'
#' @return A tibble containing extracted fee references and related sentences from brochure data
#' @export
#' @importFrom tidytext unnest_tokens
#' @import dplyr stringr purrr
#'
#' @examples
#' \dontrun{
#' brochure_data <- adv_managers_brochures(crd_ids = 156663)
#' extract_fee_references(brochure_data)
#' }
extract_fee_references <- function(data, word_threshhold = 5,
                                   print_sentences = TRUE) {
  data <-
    data %>%
    filter(!nameAuthor %>% is.na())

  data <-
    data %>%
    dplyr::select(idCRD, nameEntityManager, textBrochure) %>%
    tidytext::unnest_tokens(sentence, textBrochure, token = "sentences") %>% # tokenize to sentences
    mutate(idSentence = 1:n()) %>% # create sentence IDs to check accuracy later
    mutate(
      hasMGMTFeeReference = sentence %>% str_detect('[0-99]%')  # add column for possible fee reference
    )
  possible_fees <-
    data %>%
    dplyr::filter(hasMGMTFeeReference == T) %>%  # filter down to possible sentences
    dplyr::select(idCRD, nameEntityManager, sentence, idSentence) %>%
    tidytext::unnest_tokens(word, sentence, token = 'words') %>%  # tokenize to words
    dplyr::filter(word %>% str_detect("^[0-9]")) %>%  # look for numbers 1-9
    mutate(word = word %>% as.numeric()) %>%  # convert number to numeric
    dplyr::filter(word <= word_threshhold) # look for numbers <=


  if (print_sentences) {
    possible_fees$idSentence %>%
      unique() %>%
      map_chr(function(x) {
        setence_df <-
          data %>%
          dplyr::filter(idSentence == x)
        fee_text <-
          setence_df %>%
          .$sentence %>% paste0('\n', ., '\n')
        setence_df$nameEntityManager %>% paste0('Manager: ',., '\n', fee_text)
      }) %>%
      paste0(collapse = '\n') %>%
      cat(fill = T)
  }

  possible_fees %>%
    left_join(
      data
    ) %>%
    suppressMessages()
}
