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
      fromJSON(simplifyDataFrame = T, flatten = T) %>%
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
      mutate_if(is.character,
                funs(ifelse(. == "", NA, .))) %>%
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

    data <-
      page %>% html_table(fill = T) %>% first() %>%
      as_tibble() %>%
      set_names(
        c(
          "nameCompany",
          "amountValuation",
          "dateJoined",
          "country",
          "industry",
          "namesInvestors"
        )
      ) %>%
      filter(!is.na(nameCompany)) %>%
      mutate(
        amountValuation = readr::parse_number(amountValuation) * 1000000000,
        dateJoined  = lubridate::mdy(dateJoined)
      ) %>%
      mutate(id = 1:n(),
             amountValuation = currency(amountValuation, digits = 0),
      ) %>%
      select(id, everything())

    data <- data %>%
      separate(nameCompany,
               into = c("nameCompany", "nameDBA"),
               sep = "\\(") %>%
      mutate(nameDBA = nameDBA %>% str_remove_all("\\)")) %>%
      mutate_if(is.character, str_trim)

    df_investors <-
      data %>%
      select(id, namesInvestors) %>%
      separate_rows(namesInvestors, sep = "\\,") %>%
      mutate_if(is.character, str_squish) %>%
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
      mutate_if(is.character, str_to_upper)



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
      glue("Acquired data on {nrow(data)} companies with a private market valuation of {total}") %>% cat(fill = T)
    }
    data
  }