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
#' ycombinator_alumni(nest_data = FALSE)
ycombinator_alumni <-
  function(nest_data = FALSE,
           return_message = TRUE) {
    data <-
      "https://api.ycombinator.com/companies/export.json" %>%
      fromJSON(simplifyDataFrame = T, flatten = T) %>%
      as_data_frame() %>%
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
      mutate_if(is.character,
                funs(ifelse(. == "", NA, .))) %>%
      left_join(data_frame(
        idSeasonYC = c('w', 's'),
        nameSeasonYC = c('Winter', 'Summer')
      )) %>%
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
      mutate_at(data %>% dplyr::select(dplyr::matches("name")) %>% names(),
                funs(. %>% str_to_upper())) %>%
      mutate_at(.vars = "urlCompany",
                funs(ifelse(. == '', NA, .))) %>%
      mutate_if(is.character,
                str_trim)


    if (return_message)  {
      random_company <-
        data %>%
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
          data %>% nrow() %>% formattable::comma(digits = 0),
          ' YCombinator Companies from ',
          data$yearYC %>% min(),
          ' to ',
          data$yearYC %>% max(),
          random_company_message
        ) %>%
        cat(fill = T)
    }

    if (nest_data) {
      data <-
        data %>%
        nest(-batchYC, .key = dataClass)
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
#' @return \code{data_frame}
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
        cat(fill = T)
    }
    return(unicorn_df)
  }