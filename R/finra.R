.dictionary_finra_names <-
  function() {
    tibble(
      nameFINRA = c(
        "bc_branch_city",
        "bc_branch_zip",
        "bc_firm_name",
        "bc_branch_state",
        "bc_firm_id",
        "bc_branch_id",
        "firmId",
        "name",
        "status",
        "nameStatus",
        "firm_source_id",
        "firm_ia_sec_number",
        "firm_ia_full_sec_number",
        "firm_name",
        "firm_ia_scope",
        "firm_branches_count",
        "firm_bd_sec_number",
        "firm_bd_full_sec_number",
        "firm_scope",
        "firm_disclosure_fl",
        "firm_approved_finra_registration_count",
        "street1",
        "street2",
        "city",
        "state",
        "country",
        "postalCode",
        "dataRelyingAdvisors",
        "namesEntityOther",
        "countOtherNames",
        "id",

        "ind_source_id",
        "ind_firstname",
        "ind_middlename",
        "ind_lastname",
        "ind_bc_scope",
        "ind_ia_scope",
        "ind_bc_disclosure_fl",
        "ind_approved_finra_registration_count",
        "ind_employments_count",
        "ind_industry_days",
        "addressStreet1Company",
        "addressStreet2Company",
        "cityCompany",
        "stateCompany",
        "countryCompany",
        "zipcodeCompany",
        "firm_expelled_date"
      ),
      nameActual = c(
        "cityCompany",
        "zipcodeCompany",
        "nameCompany",
        "stateCompany",
        "idCompany",
        "idBranch",
        "idFINRAEntity",
        "nameEntity",
        "statusFINRA",
        "statusFINRARemove",
        "idCRD",
        "codeSEC",
        "idSEC",
        "nameCompany",
        "isActiveInvestmentAdvisory",
        "countOffices",
        "codeSECBrokerDealer",
        "idSECBrokerDealer",
        "isActiveBrokerDealer",
        "hasFINRADisclosuresFirm",
        "countApprovedFINRARegistration",
        "addressStreet1Company",
        "addressStreet2Company",
        "cityCompany",
        "stateCompany",
        "countryCompany",
        "zipcodeCompany",
        "dataRelyingAdvisors",
        "namesEntityOther",
        "countOtherNames",
        "id",

        "idFINRAPerson",
        "nameFirst",
        "nameMiddle",
        "nameLast",
        "isActiveBroker",
        "isActiveInvestmentAdvisor",
        "hasFINRADisclosuresIndividual",
        "countApprovedFINRARegistrationIndividual",
        "countJobs",
        "countDaysInIndustry",
        "addressStreet1Company",
        "addressStreet2Company",
        "cityCompany",
        "stateCompany",
        "countryCompany",
        "zipcodeCompany",
        "dateFirmExpelled"

      )

    )
  }

.resolve_finra_names <- function(data) {
  df_finra <-
    .dictionary_finra_names()

  finra_names <-
    names(data)

  actual_names <-
    finra_names %>%
    map_chr(function(id) {
      no_name <-
        df_finra %>%
        filter(nameFINRA == id) %>%
        nrow() == 0

      if (no_name) {
        glue::glue("Missing {id} in dictionary") %>% cat(fill = T)
        return(id)
      }
      df_finra %>%
        filter(nameFINRA == id) %>%
        pull(nameActual) %>%
        unique() %>%
        .[[1]]
    })

  data %>%
    setNames(actual_names)
}


# new_finra ---------------------------------------------------------------

.get_finra_name_df <-
  function() {
    tibble(
      nameFINRA = c(
        "fields.score",
        "fields.bc_source_id",
        "fields.bc_firm_name",
        "fields.bc_scope",
        "fields.bc_sec_number",
        "fields.bc_branches_count",
        "fields.bc_approved_finra_registration_count",
        "fields.bc_disclosure_fl",
        "highlightedFields.bc_firm_name",
        "fields.bc_ia_address_details",
        "firmId",
        "firmName",
        "secNumber",
        "otherNames",
        "bcScope",
        "iaScope",
        "isLegacy",
        "finraRegistered",
        "districtName",
        "firmType",
        "formedState",
        "formedDate",
        "fiscalMonthEndCode",
        "legacyReportStatus",
        "disclosureType",
        "disclosureCount",
        "approvedFinraRegistrationCount",
        "approvedSECRegistrationCount",
        "approvedSRORegistrationCount",
        "approvedStateRegistrationCount",
        "businessTypeCount",
        "legalName",
        "position",
        "crdNumber",
        "fields.bc_firstname",
        "fields.bc_industry_days",
        "fields.bc_employments_count",
        "fields.bc_lastname",
        "fields.bc_middlename",
        "category",
        "regulator",
        "messages",
        "capacity",
        'individualId',
        'firstName',
        'middleName',
        'lastName',
        'daysInIndustry',
        "street1",
        "street2",
        "city",
        "state",
        "country",
        "zipcode",
        "registrationBeginDate",
        "registrationEndDate",
        "firmBCScope",
        "firmIAScope",
        "eventDate",
        "disclosureResolution",
        "docketNumber",
        "Initiated By",
        "Allegations",
        "Resolution",
        "SanctionDetails",
        "Sanction Details",
        "Broker Comment",
        "stateExamCount",
        "principalExamCount",
        "productExamCount",
        "examCategory",
        "examName",
        "examTakenDate",
        "hasBCComments",
        "hasIAComments",
        "legacyReportStatusDescription",
        'firmSize'
      ),
      nameActual = c(
        "scoreWord",
        "idCRD",
        "nameFirm",
        "typeActiveFiler",
        "idSEC",
        "countBranches",
        "countFINRARegistrations",
        "idDisclosure",
        "htmlName",
        'addressFiler',
        "idCRD",
        "nameFirm",
        "idSEC",
        "nameOther",
        "scopeBC",
        "scopeIA",
        "isLegacy",
        "finraRegistered",
        "namDistrict",
        "typeFirm",
        "stateFormed",
        "dateFormed",
        "monthFiscalEnd",
        "statusLegacyReporting",
        "typeDisclosure",
        "countDisclosures",
        "countApprovedFinraRegistration",
        "countApprovedSECRegistration",
        "countApprovedSRORegistration",
        "countApprovedStateRegistration",
        "countBusinessType",
        "nameLegal",
        "descriptionPosition",
        "idCRD",
        "nameFirst",
        "countDaysIndustry",
        "countEmployments",
        "nameLast",
        "nameMiddle",
        "categoryAction",
        "idRegulator",
        "messageAction",
        "capacityAction",
        'idCRD',
        'nameFirst',
        'nameMiddle',
        'nameLast',
        'countDaysIndustry',
        "street1Firm",
        "street2Firm",
        "cityFirm",
        "stateFirm",
        "countryFirm",
        "zipcodeFrim",
        "dateRegistrationBegin",
        "dateRegistrationEnd",
        "scopeBCS",
        "scopeIAFirm",
        "dateEvent",
        "detailsDisclosureResolution",
        "idDocket",
        "entityInitiatedBy",
        "descriptionAllegations",
        "typeResolution",
        "idSanctions",
        "detailsSanctions",
        "commentBroker",
        "countStateExam",
        "countPrincipalExam",
        "countProductExam",
        "categoryExam",
        "nameExam",
        "dateExamTaken",
        "hasBCComments",
        "hasIAComments",
        "descriptionLegacyReportStatus",
        'descriptionSizeFirm'
      )
    )
  }

.generate_finra_url <-
  function(search_name = "Rockwood Capital",
           is_firm = TRUE) {


    slug <-
      case_when(is_firm ~ 'firm?query=',
              TRUE ~ 'individual?query=')

      search_slug <- URLencode(search_name)


      url <- glue("https://api.brokercheck.finra.org/search/{slug}{search_slug}&nrows=100") %>%
        as.character()

    url
  }

.generate_broker_urls <-
  function(search_name = "EJF",
           is_firm = TRUE
  ) {
    base_url <-
      .generate_finra_url(search_name = search_name, is_firm = is_firm)

    json <- fromJSON(base_url)
    total_results <- json$hits$total

    results <- total_results / 100

    if (results < 1) {
      return(base_url)
    }

    total_pages <- round(results, digits = 0)
    p <- 1:total_pages
    pages <- seq(from =0, by = 100 ,length.out = total_pages)

    urls <- glue("{base_url}&start={pages}") %>% as.character()

    urls
  }

.parse_broker_json_url <-
  function(url = "https://api.brokercheck.finra.org/search/firm?query=EJF") {
    json <-
      url %>%
      fromJSON()


    json <- json$hits$hits
    json <- json[["_source"]]



    cols <- json %>% map_df(class) %>%
      gather(column, type) %>%
      filter(type != "list") %>%
      pull(column)

    data  <-
      json %>% select(cols) %>% as_tibble() %>%
      mutate(id = 1:n()) %>%
      select(id, everything())

    if (data %>% hasName("firm_address_details") & data %>% hasName("firm_ia_address_details")) {
      data <-
        data %>%
        mutate(
          firm_ia_address_details = case_when(
            is.na(firm_ia_address_details) ~ firm_address_details,
            TRUE ~ firm_ia_address_details
          )
        )
      data <-
        data %>%
        select(-firm_address_details)
    }

    if (json %>% hasName("firm_ia_address_details")) {
      df_addresses <-
        1:nrow(data) %>%
        map_dfr(function(x) {
          d <- json[["firm_ia_address_details"]][[x]]
          if (is.na(d)) {
            return(tibble(id = x))
          }
          d %>% fromJSON() %>% flatten_df() %>%
            mutate(id = x)
        })

      df_addresses <-
        df_addresses %>%
        .resolve_finra_names()



      data <- data %>%
        left_join(df_addresses, by = "id")

      data <-
        data %>%
        select(-one_of("firm_ia_address_details"))

    }

    if (json %>% hasName("firm_relying_advisors")) {
      df_advisors <-
        1:nrow(data) %>%
        map_dfr(function(x) {
          d <- json[["firm_relying_advisors"]][[x]]
          if (length(d) == 0) {
            return(NULL)
          }
          d <- as_tibble(d)
          d %>% .resolve_finra_names() %>%
            mutate(id = x)
        }) %>%
        group_by(id) %>%
        nest() %>%
        rename(dataRelyingAdvisors = data)

      data <-
        data %>%
        left_join(df_advisors, by = "id") %>%
        mutate(dataRelyingAdvisors = dataRelyingAdvisors %>% map_dbl(length) > 0)
    }


    if (json %>% hasName("firm_other_names")) {
      df_names <-
        1:nrow(data) %>%
        map_dfr(function(x) {
          namesEntityOther <-
            json[["firm_other_names"]][[x]] %>% unique() %>% sort() %>%
            str_c(collapse = " | ")

          tibble(id = x, namesEntityOther) %>%
            mutate(countOtherNames = namesEntityOther %>% str_count("\\|") + 1)
        })

      data <-
        data %>%
        left_join(df_names, by = "id")
    }

    data <-
      data %>%
      select(-id)

    data <-
      .resolve_finra_names(data = data) %>%
      mutate(urlFINRABrokerJSON = url)

    to_num <-
      data %>% select(matches("idCRD|codeSEC|count[A-Z]")) %>% select_if(is.character) %>% names()

    if (length(to_num) > 0) {
      data <-
        data %>%
        mutate_at(to_num, as.numeric)
    }

    to_logical <-
      data %>% select(matches("^is[A-Z]|^has[A-Z]")) %>% select_if(is.character) %>% names()

    if (length(to_logical) > 0) {
      data <-
        data %>%
        mutate_at(to_logical,
                  list(function(x) {
                    case_when(x %in% c("ACTIVE", "Y") ~ TRUE,
                              TRUE ~ FALSE)
                  }))
    }

    date_cols <-
      data %>% select(matches("date")) %>%
      select_if(is.character) %>%
      names()

    if (length(date_cols) > 0) {
      data <- data %>%
        mutate_at(date_cols, mdy)
    }

    if (data %>% hasName("idCRD")) {
      data <- data %>%
        mutate(
          urlFINRABrokerPDF = glue(
            "https://files.brokercheck.finra.org/firm/firm_{idCRD}.pdf"
          ) %>% as.character()
        )
    }


    data
  }

.parse_finra_pdf_brochure <-
  function(url = 'https://files.brokercheck.finra.org/firm/firm_18718.pdf') {
    info <-
      url %>%
      pdf_info()


    df_info <-
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
      names(df_info)

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
      df_info %>%
      dplyr::select(columns_selected)

    actual_names <-
      actual_name_df %>%
      dplyr::filter(!nameActual %>% is.na()) %>%
      .$nameActual

    names(df_info) <-
      actual_names

    pdf_pages <-
      url %>%
      pdf_text() %>%
      str_split('\n')

    seq_along(pdf_pages) %>%
      future_map_dfr(function(x) {
        page_text <-
          pdf_pages[[x]] %>%
          str_squish() %>%
          discard(function(x) {
            x == ""
          })

        page_text <-
          stringi::stri_trans_general(page_text, "Latin-ASCII")

        page_text <-
          page_text %>% str_replace_all(
            'www.finra.org/brokercheck|2016 FINRA. All rights reserved.|User Guidance|End of Report|This page is intentionally left blank.',
            ''
          ) %>%
          str_replace_all('Firm Brochure', '') %>%
          str_trim() %>%
          gsub("^ *|(?<= ) | *$", "", ., perl = TRUE)

        page_text <-
          page_text[!page_text == '']

        page_text <-
          page_text %>% paste0(collapse = ' ')

        page_text <-
          page_text %>%
          stringi::stri_trans_general("latin-ascii")

        df_page <-
          tibble(numberPage = x, textPage = page_text)
        df_page
      }) %>%
      dplyr::select(textPage) %>%
      summarise(textFINRABrochure = textPage %>% paste0(collapse = '\n')) %>%
      mutate(urlFINRABrokerPDF = url)
  }

.parse_finra_pdf_brochure_urls <-
   function(urls, return_message = T) {
     .parse_finra_pdf_brochure_safe <-
       possibly(.parse_finra_pdf_brochure, tibble())

     urls %>%
       map_dfr(function(url){
         if (return_message) {
           glue("Parsing {url} for PDF text") %>% message()
         }
         .parse_finra_pdf_brochure_safe(url = url)
       })
   }


.finra_entity <-
  function(search_name = "Rockwood Capital",
           is_firm = TRUE,
           ocr_pdf = TRUE,
           return_message = TRUE) {
    options(warn = -1)
    if (length(search_name) == 0) {
      stop("Please enter a search term")
    }
    url_df <-
      search_name %>%
      future_map_dfr(function(x) {
        url <-
          .generate_broker_urls(is_firm = is_firm, search_name = x)

        tibble(nameSearch = x, urlJSON = url)
      })

    .parse_broker_json_url_safe <-
      possibly(.parse_broker_json_url, tibble())

    all_data <-
      url_df$urlJSON %>%
      future_map_dfr(function(x) {
        if (return_message) {
          glue("Parsing {x} for FINRA data") %>% message()
        }
        data <-
          .parse_broker_json_url(url = x)



        if (return_message) {
          glue("Found {nrow(data)} FINRA matches for {search_name}") %>% message()
        }
        data
      })
    has_url <- all_data %>% hasName("urlFINRABrokerPDF")
    if (ocr_pdf && has_url) {
      df_pdfs <-
        all_data$urlFINRABrokerPDF %>%
        .parse_finra_pdf_brochure_urls(return_message = T)

      if (nrow(df_pdfs) > 0) {
        all_data <-
          all_data %>%
          left_join(df_pdfs, by = "urlFINRABrokerPDF")
      }

      all_data
    }

    all_data

  }


#' FINRA registered entities
#'
#' This function returns information for any
#' Financial Industry Regulatory Authority [FINRA] registered
#' entity.
#' @param ocr_pdf if \code{TRUE} returns OCR'd broker report PDF
#' @param score_threshold matching score threshold for the search name
#' if \code{NULL} there is no threshold
#' @param entity_names vector of names to search
#' @param return_message return a message upon parsing \code{TRUE, FALS}
#' @import jsonlite dplyr tidyr purrr stringr pdftools stringi magrittr
#' @return a \code{data frame}
#' @note Use \code{\link{finra_people}} for registered people
#' @references \href{http://www.finra.org/}{FINRA}
#' @export
#' @family FINRA
#' @family IAPD
#' @family entity search
#' @examples
#' finra_entities(entity_names = c("EJF", "Blackstone", "Next Play", "Simple Capital"),
#' ocr_pdf = TRUE)

finra_entities <-
  function(entity_names = NULL,
           ocr_pdf = F,
           return_message = TRUE) {
    if (length(entity_names) == 0) {
      stop("Please enter entities to search for")
    }
    search_df <-
      expand.grid(
        nameSearch = entity_names,
        isFirm = TRUE,
        stringsAsFactors = FALSE
      ) %>%
      as_tibble()
    .finra_entity_safe <-
      purrr::possibly(.finra_entity, tibble())

    all_data <-
      1:nrow(search_df) %>%
      future_map_dfr(function(x) {
        .finra_entity_safe(
          search_name = search_df$nameSearch[[x]],
          ocr_pdf = ocr_pdf,
          is_firm = search_df$isFirm[[x]],
          return_message = return_message
        )
      })

    if (all_data %>% nrow() == 0) {
      return(tibble())
    }


    all_data <-
      all_data %>%
      mutate(
        urlManagerSummaryADV = glue("https://adviserinfo.sec.gov/firm/summary/{idCRD}") %>% as.character(),
        urlADVRecentPDF = glue(
          "https://reports.adviserinfo.sec.gov/reports/ADV/{idCRD}/PDF/{idCRD}.pdf"
        ) %>% as.character()
      )
    gc()

    all_data
  }

#' FINRA registered people
#'
#' This function returns information for any
#' Financial Industry Regulatory Authority [FINRA] registered
#' entity.
#' @param ocr_pdf if \code{TRUE} returns OCR'd broker report PDF
#' @param search_name vector of names to search
#' @param score_threshold matching score threshold for the search name
#' if \code{NULL} there is no threshold
#' @param return_message return a message upon parsing \code{TRUE, FALS}
#' @import jsonlite dplyr tidyr purrr stringr pdftools stringi
#' @return a \code{data frame}
#' @note Use \code{\link{finra_people()}} for registered people
#' @references \href{http://www.finra.org/}{FINRA}
#' @export
#' @family FINRA
#' @family IAPD
#' @family person search
#' @examples
#' finra_people(search_name = 'Llyod Blankfein', ocr_pdf = TRUE)
finra_people <-
  function(search_name = NULL,
           ocr_pdf = TRUE,
           return_message = TRUE) {
    if (search_name %>% purrr::is_null()) {
      stop("Please enter a person to search for")
    }
    search_df <-
      expand.grid(
        nameSearch = search_name,
        isFirm = FALSE,
        stringsAsFactors = FALSE
      ) %>%
      as_tibble()
    .finra_entity_safe <-
      purrr::possibly(.finra_entity, tibble())

    all_data <-
      1:nrow(search_df) %>%
      future_map_dfr(function(x) {
        .finra_entity_safe(
          search_name = search_df$nameSearch[[x]],
          is_firm = search_df$isFirm[[x]],
          ocr_pdf = ocr_pdf,
          return_message = return_message
        )
      })

    if (all_data %>% nrow() == 0) {
      return(tibble())
    }


    all_data
  }

