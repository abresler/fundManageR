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
    data <-
      "https://api.ycombinator.com/companies/export.json" %>%
      fromJSON(simplifyDataFrame = TRUE, flatten = TRUE) %>%
      as_tibble() %>%
      set_names(
        c(
          'nameCompany',
          'urlCompany',
          'batchYC',
          'verticalCompany',
          'descriptionCompany',
          'isDeadCompany',
          "hasFF",
          "hasFFAll"
        )
      )


    data <-
      data %>%
      mutate(yearYC = batchYC %>% readr::parse_number(),
             idSeasonYC = batchYC %>% substr(1, 1),) %>%
      mutate(across(where(is.character), ~ifelse(. == "", NA, .))) %>%
      left_join(tibble(
        idSeasonYC = c('w', 's'),
        nameSeasonYC = c('Winter', 'Summer'),
      ),  by = "idSeasonYC") %>%
      mutate(
        description = descriptionCompany %>% str_to_upper(),
        isAcquiredCompany = description %>% str_detect("ACQUIRED|WE SOLD|SOLD TO")
      ) %>%
      dplyr::select(-description) %>%
      dplyr::select(-idSeasonYC) %>%
      dplyr::select(
        nameCompany,
        isDeadCompany,
        isAcquiredCompany,
        batchYC,
        yearYC,
        nameSeasonYC,
        everything()
      ) %>%
      suppressMessages() %>%
      arrange(desc(yearYC), nameSeasonYC)

    data <-
      data %>%
      mutate(across(matches("name"), ~str_to_upper(.))) %>%
      mutate(across("urlCompany", ~ifelse(. == '', NA, .))) %>%
      mutate(across(where(is.character), ~str_trim(.)))


    if (return_message)  {
      random_company <-
        data %>%
        dplyr::filter(!descriptionCompany %>% is.na) %>%
        sample_n(1)

      .fm_time_range(
        start_year = min(data$yearYC),
        end_year = max(data$yearYC),
        n_records = nrow(data),
        data_type = "YCombinator companies"
      )

      .fm_spotlight(
        company = random_company$nameCompany,
        description = random_company$descriptionCompany,
        url = random_company$urlCompany,
        extra = list(
          "Batch" = paste(random_company$nameSeasonYC, random_company$yearYC)
        )
      )
    }

    if (nest_data) {
      data <-
        data %>%
        nest(dataClass = -batchYC)
    }
    gc()
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