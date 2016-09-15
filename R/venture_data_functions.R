# ycombinator -------------------------------------------------------------
#' Get data on all YCombinator graduates
#'
#' @param return_message
#'
#' @return
#' @export
#' @import jsonlite stringr dplyr readr
#' @examples
get_data_ycombinator_alumni <-
  function(return_message = T) {
    url <-
      'https://api.ycombinator.com/companies/export.json?callback=setupCompanies'

    json_nodes <-
      url %>%
      read_lines() %>%
      str_replace_all('^setupCompanies', '')

    json_data <-
      json_nodes %>% substr(start = 2,
                            stop = json_nodes %>% nchar - 2) %>%
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
      dplyr::select(-idSeasonYC) %>%
      dplyr::select(nameCompany,
                    isCompanyDead,
                    batchYC,
                    yearYC,
                    nameSeasonYC,
                    everything()) %>%
      suppressMessages() %>%
      arrange(desc(yearYC), nameSeasonYC)

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
          json_data %>% nrow(),
          ' YCombinator Companies from ',
          json_data$yearYC %>% min,
          ' to ',
          json_data$yearYC %>% max,
          random_company_message
        ) %>%
        message
    }

    return(json_data)
  }




# fintech -----------------------------------------------------------------
