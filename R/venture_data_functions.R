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
#' @return where \code{nest_data} is \code{TRUE} a nested data_frame by batch,
#' where \code{nest_data} is \code{FALSE} a data_frame
#' @export
#' @import stringr dplyr readr
#' @importFrom jsonlite fromJSON
#' @family venture capital
#' @family entity search
#' @examples
#' get_data_ycombinator_alumni(nest_data = FALSE)
get_data_ycombinator_alumni <-
  function(nest_data = FALSE,
           return_message = TRUE) {
    url <-
      'https://api.ycombinator.com/companies/export.json'

    json_data <-
      url %>%
      fromJSON(simplifyDataFrame = T, flatten = T) %>%
      as_data_frame() %>%
      set_names(
        c(
          'nameCompany',
          'urlCompany',
          'batchYC',
          'verticalCompany',
          'descriptionCompany',
          'isCompanyDead'
        )
      )


    json_data <-
      json_data %>%
      mutate(
        yearYC = batchYC %>% readr::parse_number(),
        idSeasonYC = batchYC %>% substr(1, 1),
        descriptionCompany = ifelse(descriptionCompany == '', NA, descriptionCompany)
      ) %>%
      left_join(data_frame(
        idSeasonYC = c('w', 's'),
        nameSeasonYC = c('Winter', 'Summer')
      )) %>%
      mutate(description = descriptionCompany %>% str_to_upper(),
             isCompanyAcquired = description %>% str_detect("ACQUIRED|WE SOLD|SOLD TO")) %>%
      dplyr::select(-description) %>%
      dplyr::select(-idSeasonYC) %>%
      dplyr::select(nameCompany,
                    isCompanyDead,
                    isCompanyAcquired,
                    batchYC,
                    yearYC,
                    nameSeasonYC,
                    everything()) %>%
      suppressMessages() %>%
      arrange(desc(yearYC), nameSeasonYC)

    json_data <-
      json_data %>%
      mutate_at(json_data %>% dplyr::select(matches("name")) %>% names(),
                funs(. %>% str_to_upper())) %>%
      mutate_at(.vars = "urlCompany",
                funs(ifelse(. == '', NA, .)))


    if (return_message)  {
      random_company <-
        json_data %>%
        dplyr::filter(!descriptionCompany %>% is.na) %>%
        sample_n(1)

      random_company_message <-
        '\nCheckout ' %>%
        paste0(
          random_company$nameCompany,
          ' from ',
          random_company$nameSeasonYC,
          ' ',
          random_company$yearYC,
          '\nThey describe themselves as:\n',
          random_company$descriptionCompany,
          '\nTheir website is ',
          random_company$urlCompany
        )

      "You returned " %>%
        paste0(
          json_data %>% nrow() %>% formattable::comma(digits = 0),
          ' YCombinator Companies from ',
          json_data$yearYC %>% min(),
          ' to ',
          json_data$yearYC %>% max(),
          random_company_message
        ) %>%
        message()
    }

    if (nest_data) {
      json_data <-
        json_data %>%
        nest(-batchYC, .key = 'dataClass')
    }
    closeAllConnections()
    gc()
    json_data
  }


# cbinsights --------------------------------------------------------------

#' CB Insight data
#'
#' This function returns information about startups whose valuation
#' exceeds $1B as identified by CB Insights.
#'
#' @param return_message \code{TRUE} return a message after data import
#' @return \code{data_frame}
#' @references \href{https://cbinsights/}{CB Insights}
#' @export
#' @import dplyr rvest formattable stringr purrr
#' @importFrom readr parse_number
#' @family venture capital
#' @examples
#' get_data_cb_unicorns(return_message = TRUE)
get_data_cb_unicorns <-
  function(return_message = TRUE) {
    page <-
      "https://www.cbinsights.com/research-unicorn-companies" %>%
      read_html()

    companies <-
      page %>%
      html_nodes('td:nth-child(1)') %>%
      html_text() %>%
      stringr::str_replace('\n\t','') %>%
      gsub("^ *|(?<= ) | *$", "", ., perl = TRUE)

    company_urls <-
      page %>%
      html_nodes('td:nth-child(1) a') %>%
      html_attr('href') %>%
      str_replace_all('https://www.cbinsights.com/company/https://www.cbinsights.com/company/',
                      'https://www.cbinsights.com/company/')

    valuation_billions <-
      page %>%
      html_nodes('td:nth-child(2)') %>%
      html_text() %>%
      readr::parse_number() %>%
      formattable::currency(digits = 2)

    date_joined <-
      page %>%
      html_nodes('td:nth-child(3)') %>%
      html_text() %>%
      lubridate::mdy()

    country <-
      page %>%
      html_nodes('td:nth-child(4)') %>%
      html_text()

    industry <-
      page %>%
      html_nodes('td:nth-child(5)') %>%
      html_text()

    investors <-
      page %>%
      html_nodes('td:nth-child(6)') %>%
      html_text()

    unicorn_df <-
      data_frame(
      nameCompany = companies %>% stringr::str_to_upper(),
      amountValuationBillions = valuation_billions,
      dateJoined = date_joined,
      countryCompany = country %>% stringr::str_to_upper(),
      nameIndustry = industry %>% stringr::str_to_upper(),
      nameInvestors = investors %>% stringr::str_to_upper(),
      urlCompanyCBInsights = company_urls
    )

    if (return_message) {
      list("You acquired data on ",
           unicorn_df %>% nrow() %>% formattable::comma(digits = 0),
           ' unicorns with a total private market valuation of $',
           unicorn_df$amountValuationBillions %>% sum %>% formattable::currency(),
           ' Billion') %>%
        message()
    }
    closeAllConnections()
    return(unicorn_df)
  }