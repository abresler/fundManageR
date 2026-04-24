# ycombinator -------------------------------------------------------------

#' Y Combinator data
#'
#' This function returns information on
#' Y Combinator alumni and active particpants.
#'
#' @param return_message \code{TRUE} return a message after data import
#' includes information about a random YC company
#' @param nest_data \code{TRUE} return nested data frame
#' @references \href{https://www.ycombinator.com/}{YCombinator}
#' @return where \code{nest_data} is \code{TRUE} a nested tibble by batch,
#' where \code{nest_data} is \code{FALSE} a tibble
#' @export
#' @import stringr dplyr readr
#' @importFrom jsonlite fromJSON
#' @family venture capital
#' @family entity search
#' @examples
#' ycombinator_alumni(nest_data = FALSE)
ycombinator_alumni <-
  function(nest_data = FALSE,
           return_message = TRUE) {
    # Source migrated 2026: api.ycombinator.com retired; the community-
    # maintained yc-oss/api mirrors the YC directory from the official
    # website into a stable JSON endpoint hosted on GitHub Pages.
    ua <- getOption("fundManageR.user_agent",
                    "SHELDON Research alexbresler@pwcommunications.com")
    resp <- httr::GET(
      "https://yc-oss.github.io/api/companies/all.json",
      httr::user_agent(ua)
    )
    httr::stop_for_status(resp)
    raw <- jsonlite::fromJSON(
      httr::content(resp, as = "text", encoding = "UTF-8"),
      simplifyDataFrame = TRUE, flatten = TRUE
    )
    data <- tibble::as_tibble(raw) %>%
      dplyr::transmute(
        idYC = as.integer(.data$id),
        nameCompany = stringr::str_to_upper(as.character(.data$name)),
        slugCompany = as.character(.data$slug),
        urlCompany = as.character(.data$website),
        batchYC = as.character(.data$batch),
        locationCompany = as.character(.data$all_locations),
        industryCompany = as.character(.data$industry),
        subIndustryCompany = as.character(.data$subindustry),
        descriptionOneLiner = as.character(.data$one_liner),
        descriptionCompany = as.character(.data$long_description),
        countTeamSize = suppressWarnings(as.integer(.data$team_size)),
        stageCompany = as.character(.data$stage),
        statusCompany = as.character(.data$status),
        isTopCompany = as.logical(.data$top_company),
        isHiring = as.logical(.data$isHiring),
        isNonprofit = as.logical(.data$nonprofit),
        dateLaunched = suppressWarnings(as.POSIXct(.data$launched_at, origin = "1970-01-01", tz = "UTC")),
        tags = .data$tags,
        regions = .data$regions,
        industries = .data$industries,
        urlLogo = as.character(.data$small_logo_thumb_url),
        urlYCProfile = as.character(.data$url),
        urlApi = as.character(.data$api)
      ) %>%
      dplyr::mutate(
        yearYC = suppressWarnings(readr::parse_number(.data$batchYC)),
        idSeasonYC = substr(.data$batchYC, 1, 1),
        isDeadCompany = .data$statusCompany %in% c("Inactive", "Dead"),
        isAcquiredCompany = .data$statusCompany == "Acquired"
      ) %>%
      dplyr::left_join(
        tibble::tibble(
          idSeasonYC   = c("W", "S", "F", "X"),
          nameSeasonYC = c("Winter", "Summer", "Fall", "Special")
        ),
        by = "idSeasonYC"
      ) %>%
      dplyr::select(-.data$idSeasonYC) %>%
      dplyr::arrange(dplyr::desc(.data$yearYC), .data$nameSeasonYC, .data$nameCompany)

    if (return_message) {
      try(.fm_time_range(
        start_year = min(data$yearYC, na.rm = TRUE),
        end_year   = max(data$yearYC, na.rm = TRUE),
        n_records  = nrow(data),
        data_type  = "YCombinator companies"
      ), silent = TRUE)
    }
    if (nest_data) {
      data <- data %>% tidyr::nest(dataClass = -.data$batchYC)
    }
    data
  }


# cbinsights --------------------------------------------------------------

#' CB Insight data
#'
#' This function returns information about startups whose valuation
#' exceeds $1B as identified by CB Insights.
#'
#' @param return_message \code{TRUE} return a message after data import
#' @return \code{tibble}
#' @references \href{https://cbinsights/}{CB Insights}
#' @export
#' @import dplyr rvest formattable stringr purrr
#' @importFrom readr parse_number
#' @family venture capital
#' @examples
#' cb_unicorns(return_message = TRUE)
cb_unicorns <-
  function(return_message = TRUE) {
    page <-
      "https://www.cbinsights.com/research-unicorn-companies" %>%
      read_html()

    raw_table <- page %>% html_table(fill = TRUE) %>% first() %>% as_tibble()

    # Handle dynamic column structure - CB Insights adds columns over time
    expected_cols_7 <- c(
      "nameCompany",
      "amountValuation",
      "dateJoined",
      "country",
      "city",
      "industry",
      "namesInvestors"
    )
    expected_cols_6 <- c(
      "nameCompany",
      "amountValuation",
      "dateJoined",
      "country",
      "industry",
      "namesInvestors"
    )

    if (ncol(raw_table) == 7) {
      data <- raw_table %>% set_names(expected_cols_7)
    } else if (ncol(raw_table) == 6) {
      data <- raw_table %>% set_names(expected_cols_6)
    } else {
      # Fallback: use cleaned original names
      data <- raw_table %>% janitor::clean_names()
    }

    data <- data %>%
      filter(!is.na(nameCompany)) %>%
      mutate(
        # Clean any HTML artifacts from valuation (e.g., "$480/td>")
        amountValuation = amountValuation %>% str_remove_all("<.*?>|/td>"),
        amountValuation = readr::parse_number(amountValuation) * 1000000000,
        dateJoined  = lubridate::mdy(dateJoined)
      ) %>%
      mutate(id = seq_len(n()),
             amountValuation = currency(amountValuation, digits = 0),
      ) %>%
      select(id, everything())

    data <- data %>%
      separate(nameCompany,
               into = c("nameCompany", "nameDBA"),
               sep = "\\(") %>%
      mutate(nameDBA = nameDBA %>% str_remove_all("\\)")) %>%
      mutate(across(where(is.character), ~str_trim(.)))

    df_investors <-
      data %>%
      select(id, namesInvestors) %>%
      separate_rows(namesInvestors, sep = "\\,") %>%
      mutate(across(where(is.character), ~str_squish(.))) %>%
      filter(namesInvestors != "") %>%
      group_by(id) %>%
      summarise(
        countInvestors = n(),
        namesInvestors = unique(namesInvestors) %>% sort() %>% str_c(collapse = " | ")
      )

    data <-
      data %>%
      select(-namesInvestors) %>%
      left_join(df_investors, by = "id") %>%
      mutate(across(where(is.character), ~str_to_upper(.)))



    company_urls <-
      page %>%
      html_nodes('td:nth-child(1) a') %>%
      html_attr('href') %>%
      str_replace_all(
        'https://www.cbinsights.com/company/https://www.cbinsights.com/company/',
        'https://www.cbinsights.com/company/'
      )


    data <- data %>%
      mutate(urlCBInsights = company_urls)

    if (return_message) {
      total <- data$amountValuation %>% sum()
      total_formatted <- formattable::currency(total, digits = 0)
      .fm_data_acquired(
        n_rows = nrow(data),
        source = "CB Insights Unicorn List",
        extra = paste0("Total private market valuation: ", total_formatted)
      )
    }
    data
  }