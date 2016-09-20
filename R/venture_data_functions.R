# ycombinator -------------------------------------------------------------

#' Get data on Y-Combinator alumni companies
#'
#' @param return_message Return a fun message
#'
#' @return
#' @export
#' @import stringr dplyr readr
#' @importFrom jsonlite fromJSON
#' @examples get_data_ycombinator_alumni()
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

# fintech transactions -----------------------------------------------------------------
 # http://db.ftpartners.com/transactionsearch

get_ft_partners_sector_df <-
  function(){
  name_df <-
  data_frame(
    nameSectorFTPartners = c(
      "Banking / Lending",
      "Financial BPO",
      "Financial Mgmt Solutions",
      "FinHCIT",
      "Insurance",
      "Payments / Loyalty / eCommerce",
      "Securities / Cap Mkts / Wealth Management"
    ),
    slugSectorFTPartners = c(";FS:Banking%20/%20Lending", "?keyword=;FS:Financial%20BPO",
                             ";FS:Financial%20Mgmt%20Solutions", "?keyword=;FS:FinHCIT",
                             ";FS:Insurance", "?keyword=;FS:Payments%20/%20Loyalty%20/%20eCommerce",
                             ";FS:Securities%20/%20Cap%20Mkts%20/%20Wealth%20Management"
    )
  )
  return(name_df)
  }

parse_ftpartners_page_for_data <-
  function(url = 'http://db.ftpartners.com/TransactionSearch') {
    options(scipen = 99999)
    page_lines <-
      url %>%
      read_lines()

    raw_table <-
      page_lines[page_lines %>%
                   str_detect(pattern = 'var dataGrd = ')]

    raw_table <-
      raw_table %>% str_split(pattern = ' var dataGrd = ') %>%
      flatten_chr %>%
      .[[2]] %>%
      str_replace_all('\\;', '') %>%
      jsonlite::fromJSON(simplifyDataFrame = T, flatten = T) %>%
      as_data_frame %>%
      mutate(idRow = 1:n()) %>%
      dplyr::select(idRow, everything())

    company_data <-
      1:nrow(raw_table) %>%
      map_df(function(x) {
        descriptionTransaction <-
          raw_table$V3[[x]] %>%
          read_html() %>%
          html_nodes('a') %>%
          html_text

        urlTransactionFTPartners <-
          textSummary <-
          raw_table$V3[[x]] %>%
          read_html() %>%
          html_nodes('a') %>%
          html_attr('href') %>%
          paste0('http://db.ftpartners.com/TransactionSearch', .)

        nameCompany <-
          raw_table$V4[[x]] %>%
          read_html() %>%
          html_nodes('a') %>%
          html_text

        urlCompanyFTPartners <-
          raw_table$V4[[x]] %>%
          read_html() %>%
          html_nodes('a') %>%
          html_attr('href') %>%
          paste0('http://db.ftpartners.com/TransactionSearch', .)

        row_data <-
          data_frame(
            idRow = x,
            descriptionTransaction,
            urlTransactionFTPartners,
            nameCompany,
            urlCompanyFTPartners
          )
        return(row_data)
      })

    investor_sector_data <-
      1:nrow(raw_table) %>%
      map_df(function(x) {
        investor_node_exists <-
          !raw_table$V5[[x]] == ''
        if (investor_node_exists) {
          nameBuyerInvestor <-
            raw_table$V5[[x]] %>%
            read_html() %>%
            html_nodes('a') %>%
            html_text

          urlBuyerInvestorFTPartners <-
            raw_table$V5[[x]] %>%
            read_html() %>%
            html_nodes('a') %>%
            html_attr('href') %>%
            paste0('http://db.ftpartners.com/TransactionSearch', .)


          if (nameBuyerInvestor %>% length > 1) {
            nameBuyerInvestor <-
              nameBuyerInvestor %>%
              paste0(collapse = '; ')

            urlBuyerInvestorFTPartners <-
              urlBuyerInvestorFTPartners %>%
              paste0(collapse = '; ')
          }

        } else {
          nameBuyerInvestor <-
            NA
          urlBuyerInvestorFTPartners <-
            NA
        }
        sector_node_exists <-
          !raw_table$V6[[x]] == ''

        if (sector_node_exists) {
          nameSectorFTPartners <-
            raw_table$V6[[x]] %>%
            read_html() %>%
            html_nodes('a') %>%
            html_text

          urlSectorFTPartners <-
            raw_table$V6[[x]] %>%
            read_html() %>%
            html_nodes('a') %>%
            html_attr('href') %>%
            paste0('http://db.ftpartners.com/TransactionSearch', .)


          if (nameSectorFTPartners %>% length > 1) {
            nameSectorFTPartners <-
              nameSectorFTPartners %>%
              paste0(collapse = '; ')

            urlSectorFTPartners <-
              urlSectorFTPartners %>%
              paste0(collapse = '; ')
          }

        } else {
          nameSectorFTPartners <-
            NA
          urlSectorFTPartners <-
            NA
        }
        sector_investor_data <-
          data_frame(
            idRow = x,
            nameBuyerInvestor,
            urlBuyerInvestorFTPartners,
            nameSectorFTPartners,
            urlSectorFTPartners
          )
        return(sector_investor_data)
      })

    table_data <-
      raw_table %>%
      dplyr::select(1:3, 8) %>%
      set_names(c(
        'idRow',
        'dateTransaction',
        'typeTransaction',
        'amountTransaction'
      )) %>%
      mutate(
        dateTransaction = dateTransaction %>% lubridate::mdy,
        amountTransaction = amountTransaction %>% readr::parse_number * 1000000 %>% currency(digits = 0)
      ) %>%
      left_join(company_data) %>%
      left_join(investor_sector_data) %>%
      dplyr::select(dateTransaction, typeTransaction, nameCompany, nameBuyerInvestor, everything()) %>%
      mutate(urlSearchFTPartners = url) %>%
      suppressMessages() %>%
      dplyr::select(-idRow)

    return(table_data)
  }

parse_transaction_url <-
  function(free_text,
           transaction_type,
           announce_date_start,
           announce_date_end,
           transaction_minimum_millions,
           transaction_maximum_millions,
           transaction_status,
           sector_names) {
    data_frame(nameInput = c('free_text', 'transaction_type',
                             'announce_date_start', 'announce_date_end',
                             'transaction_minimum_millions', 'transaction_maximum_millions',
                              'transaction_status',
                             'sector_names'),
               nameSlug = c(';Key:', ';TT:', ';SD:', ';ED:',
                            'AMi', 'AMa:', ';TS:'
                            )

    )
  }

get_ft_partners_transaction_data <-
  function() {

    base <-
      'http://db.ftpartners.com/TransactionSearch'

   table_data <-
      url %>%
      parse_ftpartners_page_for_data()
}