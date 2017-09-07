
# constituency ------------------------------------------------------------
get_nareit_constiutent_urls <-
  function() {
    page <-
      "https://www.reit.com/investing/index-data/monthly-index-constituents" %>%
      read_html()

    slugs <-
      page %>%
      html_nodes('a') %>%
      html_attr('href')

    slugs <-
      slugs[slugs %>% str_detect('pdf')]

    slugs <-
      slugs[!slugs %>% is.na()]

    year <-
      slugs %>%
      str_replace_all('/sites/default/files/returns/|.pdf', '') %>%
      readr::parse_number() %>%
      suppressWarnings()

    urls <-
      slugs %>% paste0('https://www.reit.com', .)

    df <-
      data_frame(yearData = year,
               urlData = urls)
    closeAllConnections()
    return(df)
  }

parse_nareit_constituent_url <-
  function(url = "https://www.reit.com/sites/default/files/returns/FNUSIC2016.pdf",
           return_message = TRUE) {
    df <-
      data_frame()
    success <- function(res) {
      url <-
        res$url
      df_metadata <-
        url %>%
        tabulizer::extract_metadata() %>%
        flatten_df()

      year_data <-
        url %>%
        str_replace_all(
          'https://www.reit.com/sites/default/files/returns/|.pdf|https://www.reit.com//sites/default/files/returns/',
          ''
        ) %>%
        readr::parse_number()

      if ('modified' %in% names(df_metadata)) {
        date_parts <-
          df_metadata$modified %>% str_replace_all(" EDT | EST ", ' ') %>%
          str_split('\\ ') %>%
          flatten_chr()

        date_year <-
          date_parts[date_parts %>% length()]

        other_parts <-
          date_parts[1:(date_parts %>% length() - 1)] %>%
          .[2:length(.)]

        datetime_file <-
          c(date_year, other_parts) %>%
          paste0(collapse = ' ') %>%
          lubridate::ymd_hms()

        date_data <-
          datetime_file %>%
          as.Date()
      } else {
        datetime_file <- NA
        date_data <- NA
      }

      tables <-
        url %>%
        extract_tables(1:df_metadata$pages)

      all_data <-
        1:length(tables) %>%
        map_df(function(x) {
          tables[[x]] %>%
            as_data_frame() %>%
            mutate(idTable = x)

        })

      data_cols <-
        all_data %>% ncol() %>%
        as.numeric()

      if (data_cols == 5) {
        all_data <-
          all_data %>%
          select(-matches("idTable")) %>%
          purrr::set_names(c('nameCompany',
                             'idTicker',
                             'typeInvestment',
                             'nameSector')) %>%
          mutate_all(funs(. %>% str_trim() %>% str_to_upper())) %>%
          mutate(idRow = 1:n())

        sector_df <-
          all_data %>%
          filter(nameCompany %>% str_detect(":")) %>%
          select(nameCompany, idRow) %>%
          separate(nameCompany, '\\:', into = c('ignore', 'nameSector')) %>%
          mutate(idRow = idRow + 1) %>%
          select(idRow, nameSector)

        all_data <-
          all_data %>%
          filter(!idTicker == '') %>%
          mutate(nameCompany = nameCompany %>% str_replace(' ', ':')) %>%
          tidyr::separate(nameCompany,
                          into = c('rank', 'nameCompany'),
                          sep = '\\:') %>%
          left_join(sector_df) %>%
          fill(nameSector) %>%
          select(-c(rank, idRow)) %>%
          mutate(nameSector = ifelse(nameSector == '', NA, nameSector)) %>%
          select(nameCompany, idTicker, nameSector, everything()) %>%
          mutate(
            yearData = year_data,
            urlData = url,
            yearData = year_data,
            dateFile = date_data
          ) %>%
          select(yearData, dateFile, everything()) %>%
          suppressMessages() %>%
          suppressWarnings()

      }

      if (data_cols == 6) {
        all_data <-
          all_data %>%
          select(-idTable) %>%
          purrr::set_names(
            c(
              'nameCompany',
              'idTicker',
              'typeInvestment',
              'idSubSector',
              'amountEquityMarketCap'
            )
          ) %>%
          mutate_all(funs(. %>% str_trim() %>% str_to_upper()))

        all_data <-
          all_data %>%
          filter(!idTicker == '') %>%
          filter(!idTicker %>% str_detect("TICKER|SYMBOLS")) %>%
          mutate(
            typeInvestment =  ifelse(typeInvestment == '', NA, typeInvestment),
            idSubSector =  ifelse(idSubSector == '', NA, idSubSector),
            amountEquityMarketCap = amountEquityMarketCap %>% readr::parse_number() * 1000000 %>% formattable::currency(digits = 0),
            yearData = year_data,
            urlData = url,
            yearData = year_data,
            dateFile = date_data
          ) %>%
          select(yearData, dateFile, everything()) %>%
          suppressWarnings()
      }

      if ((data_cols == 7) & (year_data <= 2011)) {
        all_data <-
          all_data %>%
          mutate(idRow = 1:n())

        sector_df <-
          all_data %>%
          select(V1, idRow) %>%
          filter(V1 %>% str_detect(":")) %>%
          separate(V1, c('ignore', 'nameSector'), sep = '\\: ') %>%
          select(nameSector, idRow) %>%
          filter(!nameSector %>% is.na()) %>%
          mutate(idRow = idRow + 1,
                 nameSector = nameSector %>% str_trim() %>% str_to_upper()) %>%
          suppressWarnings()

        all_data <-
          all_data %>%
          filter(!V3 == '') %>%
          filter(!V3 %>% str_detect('Ticker|Symbol')) %>%
          select(-idTable)

        all_data <-
          all_data %>%
          mutate(V1 = V1 %>% str_replace(' ', '\\:')) %>%
          separate(V1,
                   sep = '\\:',
                   into = c('rank', 'nameCompany')) %>%
          mutate(
            idTicker = ifelse(V6 %>% is.na(), V2, V3),
            typeInvestment = ifelse(V6 %>% is.na(), V3, V4),
            nameSubSector = ifelse(V6 %>% is.na(), V4, V5),
            amountEquityMarketCap = ifelse(V6 %>% is.na(), V5, V6)
          ) %>%
          select(
            nameCompany,
            idTicker,
            typeInvestment,
            nameSubSector,
            amountEquityMarketCap,
            idRow
          ) %>%
          left_join(sector_df) %>%
          mutate_all(str_to_upper) %>%
          fill(nameSector) %>%
          select(-idRow) %>%
          select(nameCompany, idTicker, nameSector, everything()) %>%
          mutate(
            amountEquityMarketCap = amountEquityMarketCap %>% readr::parse_number() * 1000000 %>% formattable::currency(digits = 0),
            nameSubSector = ifelse(nameSubSector == '', NA, nameSubSector)
          ) %>%
          mutate(dateFile = date_data,
                 yearData = year_data) %>%
          select(yearData, dateFile, everything()) %>%
          suppressMessages()

      }

      if ((data_cols == 7) & (year_data > 2011)) {
        all_data <-
          all_data %>%
          mutate(idRow = 1:n(),
                 V2 = ifelse(V2 == '' , V1 %>% substr(3, nchar(V1)), V2))

        sector_df <-
          all_data %>%
          select(V2, idRow) %>%
          filter(V2 %>% str_detect(":")) %>%
          separate(V2, c('ignore', 'nameSector'), sep = '\\: ') %>%
          select(nameSector, idRow) %>%
          filter(!nameSector %>% is.na()) %>%
          mutate(idRow = idRow + 1,
                 nameSector = nameSector %>% str_trim() %>% str_to_upper()) %>%
          suppressWarnings()

        all_data <-
          all_data %>%
          filter(!V3 == '') %>%
          select(-V1) %>%
          filter(!V3 %>% str_detect("Ticker|Symbols")) %>%
          select(-idTable) %>%
          left_join(sector_df) %>%
          fill(nameSector) %>%
          select(-idRow) %>%
          select(nameSector, everything()) %>%
          mutate_all(funs(. %>% str_trim() %>% str_to_upper())) %>%
          suppressMessages()

        all_data <-
          all_data %>%
          purrr::set_names(
            c(
              'nameSector',
              'nameCompany',
              'idTicker',
              'typeInvestment',
              'nameSubSector',
              'amountEquityMarketCap'
            )
          ) %>%
          mutate(
            nameSubSector =  ifelse(nameSubSector == '', NA, nameSubSector),
            amountEquityMarketCap = amountEquityMarketCap %>% readr::parse_number() * 1000000 %>% formattable::currency(digits = 0)
          )

        all_data <-
          all_data %>%
          select(-matches("urlData")) %>%
          mutate(urlData = url) %>%
          mutate(dateFile = date_data,
                 yearData = year_data) %>%
          select(yearData, dateFile, everything())
      }

      if (return_message) {
        list(
          "Acquired ",
          all_data %>% nrow() %>% formattable::comma(digits = 0),
          ' REITs for the year ',
          year_data
        ) %>%
          purrr::invoke(paste0, .) %>%
          message()
      }
      closeAllConnections()
      df <<-
        df %>%
        bind_rows(all_data)
    }
    failure <- function(msg) {
      data_frame()
    }
    url %>%
      map(function(x) {
        curl_fetch_multi(url = x, success, failure)
      })
    multi_run()
    df
  }
#' NAREIT index constituency data by year
#'
#' This function allows the user to acquire data on
#' companies in the FTSE NAREIT index for a given year.
#'
#' @param years vector of years to search starting in 1991
#' @param resolve_names \code{TRUE} resolve mis-spelled or abbreviated names
#' @param return_message \code{TRUE} return a message after data import
#' @param nest_data \code{TRUE} return nested data frame
#' @references \href{http://nareit.org}{National Association of Real Estate Investment Trusts}
#' @return nested \code{data_frame} or \code{data_frame} if \code{nest_data = FALSE}
#' @export
#' @family NAREIT
#' @family index constituents
#' @import purrr stringr dplyr rvest tabulizer formattable lubridate tidyr readr curl
#' @examples
#' get_data_nareit_constituent_years(years = 1991:2016, resolve_names = TRUE, nest_data = TRUE, return_message = TRUE)
get_data_nareit_constituent_years <-
  function(years = 2010:2016,
           resolve_names = TRUE,
           nest_data = TRUE,
           return_message = TRUE) {
    if (years %>% purrr::is_null()) {
      stop("Please enter years, they can start in 1991")
    }
    url_df <-
      get_nareit_constiutent_urls()

    urls <-
      url_df %>%
      filter(yearData %in% years) %>%
      .$urlData

    parse_nareit_constituent_url_safe <-
      purrr::possibly(parse_nareit_constituent_url, data_frame())

    all_data <-
      urls %>%
      map_df(function(x) {
        x %>% message()
        parse_nareit_constituent_url_safe(url = x, return_message = return_message)
      }) %>%
      suppressWarnings()

    all_data <-
      all_data %>%
      mutate(nameCompanyChar = nameCompany %>% nchar()) %>%
      filter(!idTicker %>% str_detect("SUMMARY|PROPERTY SECTOR")) %>%
      mutate(
        nameCompany = ifelse(nameCompanyChar < 4, idTicker, nameCompany),
        idTicker = ifelse(nameCompany == idTicker, typeInvestment, idTicker)
      ) %>%
      filter(nameCompanyChar > 4) %>%
      select(-nameCompanyChar)

    if ('idSubSector' %in% names(all_data)) {
      all_data <-
        all_data %>%
        mutate(
          typeInvestment = ifelse(typeInvestment == idTicker, idSubSector, typeInvestment),
          idSubSector = ifelse(idSubSector == typeInvestment, NA, idSubSector)
        )
    }

    all_data <-
      all_data %>%
      mutate(
        urlData = list(
          'https://www.reit.com/sites/default/files/returns/FNUSIC',
          yearData,
          '.pdf'
        ) %>% purrr::invoke(paste0, .)
      ) %>%
      mutate(amountEquityMarketCap = amountEquityMarketCap %>% formattable::currency(digits = 0))

    replace_number <-
      list("^", 1:99, " ") %>%
      purrr::invoke(paste0, .) %>%
      paste0(collapse = '|')

    all_data <-
      all_data %>%
      filter(!idTicker %in% c('COMPANY')) %>%
      mutate(
        nameCompany = nameCompany %>% str_replace(replace_number, ''),
        idTicker = idTicker %>% str_trim()
      )

    if ('idSubsSector' %in% names(all_data)) {
      all_data <-
        all_data %>%
        select(yearData:idSubSector,
               nameSector,
               nameSubSector,
               everything())
    }

    if ('nameSector' %in% names(all_data)) {
      all_data <-
        all_data %>%
        select(-nameSector) %>%
        left_join(
          all_data %>%
            select(idTicker, nameSector) %>%
            mutate(nameSector = nameSector %>% str_trim()) %>%
            distinct() %>%
            filter(!nameSector %>% is.na()) %>%
            mutate(idRow = 1:n()) %>%
            group_by(idTicker) %>%
            filter(idRow == max(idRow)) %>%
            ungroup() %>%
            select(-idRow)
        ) %>%
        suppressMessages() %>%
        select(yearData:idTicker, nameSector, everything()) %>%
        suppressWarnings()

      if ('nameSubSector' %in% names(data)) {
        all_data <-
          all_data %>%
          select(-nameSubSector) %>%
          left_join(
            all_data %>%
              select(idTicker, nameSubSector) %>%
              distinct() %>%
              mutate(nameSubSector = nameSubSector %>% str_trim()) %>%
              filter(!nameSubSector %>% is.na()) %>%
              mutate(idRow = 1:n()) %>%
              group_by(idTicker) %>%
              filter(idRow == max(idRow)) %>%
              ungroup() %>%
              select(-idRow)
          ) %>%
          suppressMessages() %>%
          select(yearData:idTicker,
                 nameSector,
                 nameSubSector,
                 everything()) %>%
          suppressWarnings()
      }
    }

    if (resolve_names) {
      ticker_df <-
        all_data %>%
        select(nameCompany, idTicker) %>%
        count(nameCompany, sort = T) %>%
        left_join(all_data %>% select(nameCompany, idTicker)) %>%
        distinct() %>%
        group_by(idTicker) %>%
        filter(n == max(n)) %>%
        ungroup() %>%
        suppressMessages() %>%
        distinct() %>%
        mutate(idTicker = idTicker %>% str_trim())

      ticker_df <-
        ticker_df %>%
        mutate(idRow = 1:n()) %>%
        group_by(idTicker) %>%
        filter(idRow == max(idRow)) %>%
        ungroup()

      all_data <-
        all_data %>%
        select(-nameCompany) %>%
        left_join(ticker_df %>% select(-c(idRow, n))) %>%
        select(yearData, dateFile, nameCompany, everything()) %>%
        suppressMessages()
    }
    if (return_message) {
      list(
        "Acquired REIT index constituency data from ",
        all_data$dateFile %>% min(na.rm = T),
        ' to ',
        all_data$dateFile %>% max(na.rm = TRUE)
      ) %>%
        purrr::invoke(paste0, .) %>%
        message()
    }
    if (nest_data) {
      all_data <-
        all_data %>%
        nest(-c(yearData, dateFile), .key = 'dataConstituents')
    }
    closeAllConnections()
    return(all_data)
  }


# reits -------------------------------------------------------------------

#' NAREIT member dictionary
#'
#' This function returns a
#' data frame of all NAREIT members
#'
#' @return \code{data_frame}
#' @export
#' @references \href{http://nareit.org}{National Association of Real Estate Investment Trusts}
#' @import purrr stringr dplyr rvest formattable lubridate tidyr readr
#' @family dictionary
#' @family NAREIT
#' @examples
#' get_reit_entity_dictionary()
get_reit_entity_dictionary <-
  function() {
    url <-
      "https://www.reit.com/investing/investor-resources/reit-directories/reits-by-ticker-symbol?field_rtc_stock_exchange_tid=All"
    page <-
      url %>%
      read_html()

    entities <-
      page %>%
      html_nodes('.views-field-title a') %>%
      html_text() %>%
      unique() %>%
      str_to_upper()

    companies_tickers <-
      page %>%
      html_nodes('td') %>%
      html_text() %>%
      str_trim()

    tickers <-
      1:length(companies_tickers) %>%
      map_chr(function(x) {
        items <-
          companies_tickers[[x]] %>%
          str_split('\n') %>%
          flatten_chr() %>%
          str_trim()

        if (items %>% length() == 1) {
          return(NA)
        }

        ticker <-
          items[[2]]

        if (ticker == "N/A") {
          ticker <-
            NA
        }
        return(ticker)
      })

    urls <-
      page %>%
      html_nodes('.views-field-title a') %>%
      html_attr('href')

    url_nareit_data <-
      urls[c(TRUE, FALSE)] %>%
      paste0('http://www.reit.com', .)

    url_webpage <-
      urls[c(FALSE, TRUE)] %>%
      str_to_lower()

    url_webpage[url_webpage == 'http://'] <-
      NA

    url_webpage <-
      url_webpage %>% str_replace_all('http://https://|http://http://', 'http://')

    url_df <-
      data_frame(
        nameCompany = entities,
        idTicker = tickers[1:length(entities)],
        urlNAREIT = url_nareit_data,
        urlCompany = url_webpage,
        urlData = url
      ) %>%
      mutate_all(str_trim) %>%
      mutate(isPrivateNonPublicREIT = ifelse(idTicker %>% is.na(), TRUE, FALSE)) %>%
      select(nameCompany, idTicker, isPrivateNonPublicREIT, everything()) %>%
      tidyr::separate(idTicker, into = c('idTicker', 'idExchange'), sep = '\\.|\\ ') %>%
      mutate_if(is.character, str_trim)

    closeAllConnections()
    return(url_df)
  }

parse_reit_info_page <-
  function(urls = "https://www.reit.com/company/alexandria-real-estate-equities-inc",
           return_message = TRUE) {
    df <-
      data_frame()
    success <- function(res){
      if (return_message) {
        list("Parsing: ", res$url, "\n") %>% purrr::reduce(paste0) %>% message()
      }
      page <-
        res$content %>%
        read_html()

      company <-
        page %>%
        html_nodes('#page-title') %>%
        html_text() %>%
        str_to_upper()

      has_price <-
        page %>%
        html_nodes('.price') %>% length() > 0

      if (has_price) {
        price <-
          page %>%
          html_nodes('.price') %>%
          html_text() %>%
          readr::parse_number()
      } else {
        price <- NA
      }

      has_return <-
        page %>%
        html_nodes('.return') %>% length() > 0


      if (has_return) {
        return_1mo <-
          page %>%
          html_nodes('.return') %>%
          html_text() %>%
          str_trim() %>%
          str_replace_all('1 Mo. Total Return: ','') %>%
          str_split('\\ ') %>%
          flatten_chr() %>%
          .[[1]] %>% readr::parse_number() / 100

      } else {
        return_1mo <- NA
      }

      has_description <-
        page %>%
        html_nodes('.body') %>% length() > 0

      if (has_description) {
        description <-
          page %>%
          html_nodes('.body') %>%
          html_text() %>%
          str_trim()
      } else {
        description <- NA
      }

      values <-
        page %>%
        html_nodes('.overview .reit-values__value') %>%
        html_text() %>%
        str_to_upper() %>%
        str_trim()

      items <-
        c(
          'typeCompany',
          'statusListing',
          'nameSector',
          'typeInvestment',
          'idTicker',
          'nameExchange'
        )

      data <-
        data_frame(value = values,
                   item = items[1:length(values)],
        ) %>%
        spread(item, value) %>%
        mutate(nameCompany = company,
               descriptionCompany = description,
               pctReturn1Month = return_1mo,
               priceLast = price,
               urlNAREIT = res$url)

      has_performance <-
        page %>% html_nodes('.performance .reit-values__value') %>% length() > 0

      if (has_performance) {
        values <-
          page %>%
          html_nodes('.performance .reit-values__value') %>%
          html_text() %>%
          str_trim()
        items <-
          c('priceClose', 'rangeDay', 'volumeDay',
            'volume30DayAvg',
            'amountMarketCapitalization',
            'dividendLastQuarter',
            'pctYield', 'amountFFOPriorQuarter')

        df_performance <-
          data_frame(items, values) %>%
          spread(items, values) %>%
          separate(rangeDay, into = c('priceDayHigh', 'priceDayLow'), sep = '\\ - ')

        df_performance <-
          df_performance %>%
          mutate_at(
            c('volume30DayAvg' , 'volumeDay'),
            funs(. %>% readr::parse_number() %>% formattable::comma(digits = 0))
          ) %>%
          mutate_at(
            c('amountFFOPriorQuarter' , 'amountMarketCapitalization'),
            funs(
              ifelse(
                . %>% str_detect('mil'),
                . %>% readr::parse_number() * 1000000,
                . %>% readr::parse_number() * 1000000000
              ) %>% formattable::currency(digits = 0)
            )) %>%
          mutate_at(
            c('dividendLastQuarter' , 'priceClose', 'priceDayLow', 'priceDayHigh'),
            funs(. %>% readr::parse_number() %>% formattable::currency(digits = 2))
          ) %>%
          mutate(pctYield = (pctYield %>% readr::parse_number() / 100) %>% formattable::percent(digits = 2),
                 nameCompany = company)

        data <-
          data %>%
          left_join(df_performance) %>%
          suppressWarnings() %>%
          suppressMessages()
      }
      df <<-
        df %>%
        bind_rows(data)
    }
    failure <- function(msg){
      data_frame()
    }
    urls %>%
      walk(function(x){
        curl_fetch_multi(url = x, success, failure)
      })
    multi_run()
    closeAllConnections()
    df
  }


#' NAREIT constituents
#'
#' This function returns information about all active
#' NAREIT members.
#'
#' @param include_private_non_public \code{TRUE} include privately traded or over-the-counter traded REITs \code{TRUE}, \code{FALSE}
#' @param return_message \code{TRUE} return a message after data import
#' @references \href{http://nareit.org}{National Association of Real Estate Investment Trusts}
#' @return data_frame
#' @family NAREIT
#' @family entity search
#' @export
#' @import purrr stringr dplyr rvest formattable lubridate tidyr readr
#' @examples
#' \dontrun{
#' get_data_nareit_entities(include_private_non_public = TRUE, return_message = TRUE)
#' }
get_data_nareit_entities <-
  function(include_private_non_public = TRUE,
           return_message = TRUE) {
    parse_reit_info_page_safe <-
      purrr::possibly(parse_reit_info_page, data_frame())

    reits_df <-
      get_reit_entity_dictionary()

    if (!include_private_non_public) {
      reits_df <-
        reits_df %>%
        filter(!isPrivateNonPublicREIT)
    }

    all_data <-
      reits_df$urlNAREIT %>%
      map_df(function(x) {
        parse_reit_info_page_safe(urls = x, return_message = return_message)
      }) %>%
      suppressWarnings()

    all_data <-
      all_data %>%
      mutate_all(funs(ifelse(. == '', NA, .))) %>%
      mutate(
        idTicker = ifelse(idTicker %in% c('', 'N/A'), NA, idTicker),
        nameExchange = ifelse(idTicker %>% is.na(), NA, nameExchange)
      ) %>%
      filter(!nameCompany == "NAREIT")

    all_data <-
      all_data %>%
      mutate_at(.vars = all_data %>% dplyr::select(matches("^price|^dividend")) %>% names(),
                funs(. %>% formattable::currency(digits = 2))) %>%
      mutate_at(.vars = all_data %>% dplyr::select(matches("^amount")) %>% names(),
                funs(. %>% formattable::currency(digits = 0))) %>%
      mutate_at(.vars = all_data %>% dplyr::select(matches("^volume")) %>% names(),
                funs(. %>% formattable::comma(digits = 0))) %>%
      mutate_at(.vars = all_data %>% dplyr::select(matches("^pct")) %>% names(),
                funs(. %>% formattable::percent(digits = 2))) %>%
      select(typeCompany, statusListing, nameSector, nameCompany, idTicker, everything())

    all_data <-
      all_data %>%
      left_join(reits_df %>% dplyr::select(idTicker, nameCompany, urlCompany)) %>%
      dplyr::select(typeCompany:idTicker, urlCompany, everything()) %>%
      suppressMessages()

    if (return_message) {
      list("Returned information for ", all_data %>% nrow(), ' REITs') %>%
        purrr::invoke(paste0, .) %>%
        message()
    }
    closeAllConnections()
    return(all_data)
  }




# reit_properties ---------------------------------------------------------
#' NAREIT notable properties
#'
#' This function returns information about notable NAREIT identified properties.
#'
#' @references \href{http://nareit.org}{National Association of Real Estate Investment Trusts}
#' @return \code{data_frame}
#' @export
#' @import purrr stringr dplyr jsonlite formattable lubridate tidyr readr
#' @family NAREIT
#' @family property data
#' @examples
#' get_data_nareit_notable_properties()
get_data_nareit_notable_properties <-
  function() {
    json_data <-
      "http://app.reitsacrossamerica.com/properties/notable" %>%
      fromJSON()

    df <-
      json_data$properties$property %>%
      as_data_frame()

    df <-
      df %>%
      mutate(video_url = video_url %>% map_chr(function(x) {
        if (x %>% length() == 0) {
          return(NA)
        }

        x
      }))

    images <-
      1:nrow(df) %>%
      map_chr(function(x) {
        if (df$image_url[[x]] %>% length() == 0) {
          return(NA)
        }
        df$image_url[[x]] %>%
          .[[1]] %>%
          paste0('http://app.reitsacrossamerica.com', .)
      })

    df <-
      df %>%
      mutate(image_url = images) %>%
      select(-c(slug_url, msa_code, primary_property_type)) %>%
      purrr::set_names(
        c(
          'idProperty',
          'nameProperty',
          'nameCompany',
          'coordinateLatitude',
          'coordinateLongitude',
          'nameSector',
          'stateProperty',
          'cityProperty',
          'msaProperty',
          'zipcodeProperty',
          'addressProperty',
          'urlImageProperty',
          'urlVideoProperty',
          'descriptionProperty'
        )
      ) %>%
      mutate_at(.vars = c('coordinateLongitude', 'coordinateLongitude'),
                funs(. %>% as.numeric())) %>%
      mutate(idProperty = idProperty %>% as.integer())

    df <-
      df %>%
      mutate(idProperty = idProperty %>% as.numeric()) %>%
      mutate_at(
        df %>%
          keep(is_character) %>%
          select(-matches("url")) %>% names(),
        funs(. %>% str_to_upper())
      )
    closeAllConnections()
    return(df)
  }

parse_json_hq <-
  function(url = "http://app.reitsacrossamerica.com/states/NY/headquartered",
           return_message = TRUE) {
    codeState <-
      url %>%
      str_replace_all('http://app.reitsacrossamerica.com/states/|/headquartered',
                      '')

    json_data <-
      url %>%
      fromJSON()

    no_data <-
      json_data$reits %>% length() == 0

    if (no_data) {
      return(data_frame(codeState))
    }

    df_hq <-
      json_data$reits$reit %>%
      as_data_frame() %>%
      select(-matches("slug_url")) %>%
      purrr::set_names(c('idSNL', 'nameCompany', 'urlCompany'))

    df_hq <-
      df_hq %>%
      mutate(idSNL = idSNL %>% as.numeric(),
             nameCompany = nameCompany %>% str_to_upper()) %>%
      mutate(codeState)

    df_long <-
      df_hq %>%
      gather(item, value, -codeState) %>%
      group_by(item) %>%
      mutate(countItem = (1:n()) - 1) %>%
      ungroup() %>%
      mutate(item = ifelse(countItem == 0, item, paste(item, countItem, sep = ''))) %>%
      arrange(countItem) %>%
      select(-countItem)

    col_order <-
      c('codeState', df_long$item)

    df_long <-
      df_long %>%
      spread(item, value) %>%
      select(one_of(col_order))

    if (return_message) {
      list(
        df_hq %>% nrow() %>% formattable::comma(digits = 0),
        " REITs are headquartered in ",
        codeState
      ) %>%
        purrr::invoke(paste0, .) %>%
        message()
    }
    closeAllConnections()
    return(df_long)
  }

parse_json_holdings <-
  function(url = "http://app.reitsacrossamerica.com/companies/holding/NY",
           return_message = TRUE) {
    codeState <-
      url %>%
      str_replace_all('http://app.reitsacrossamerica.com/companies/holding/',
                      '')

    json_data <-
      url %>%
      fromJSON()

    no_data <-
      json_data$companies$company %>% length() == 0

    if (no_data) {
      return(data_frame(codeState))
    }

    df_holdings <-
      json_data$companies$company %>%
      as_data_frame() %>%
      select(-matches("slug_url")) %>%
      purrr::set_names(c('idSNL', 'nameCompany', 'urlCompany'))

    df_holdings <-
      df_holdings %>%
      mutate(
        idSNL = idSNL %>% as.numeric(),
        nameCompany = nameCompany %>% str_to_upper(),
        urlCompany = ifelse(urlCompany == '', NA, urlCompany)
      ) %>%
      mutate(codeState)

    df_long <-
      df_holdings %>%
      gather(item, value, -codeState) %>%
      group_by(item) %>%
      mutate(countItem = (1:n()) - 1) %>%
      ungroup() %>%
      mutate(item = ifelse(countItem == 0, item, paste(item, countItem, sep = ''))) %>%
      arrange(countItem) %>%
      select(-countItem)

    col_order <-
      c('codeState', df_long$item)

    df_long <-
      df_long %>%
      spread(item, value) %>%
      select(one_of(col_order))

    if (return_message) {
      list(
        df_holdings %>% nrow() %>% formattable::comma(digits = 0),
        " REITs own assets in ",
        codeState
      ) %>%
        purrr::invoke(paste0, .) %>%
        message()
    }
    closeAllConnections()
    return(df_long)
  }


parse_json_state_metadata <-
  function() {
    json_data <-
      "http://app.reitsacrossamerica.com/states" %>%
      fromJSON()

    df <-
      json_data$states$state %>%
      as_data_frame() %>%
      mutate(image_path = image_path %>% flatten_chr() %>% paste0('http://app.reitsacrossamerica.com/', .)) %>%
      mutate(id = id %>% as.numeric()) %>%
      purrr::set_names(
        c(
          'idState',
          'codeState',
          'amountValuationOwnedProperties',
          'countProperties',
          'countAcresTimberland',
          'countCellTowers',
          'countAssets',
          'countHeadquarters',
          'urlImageSample',
          'countSingleFamilyHomes'
        )
      )

    df <-
      df %>%
      mutate_at(df %>% select(matches('idState|count|amount')) %>% names(),
                funs(. %>% as.numeric())) %>%
      mutate(amountValuationOwnedProperties = amountValuationOwnedProperties * 1000000 %>% formattable::currency(digits = 0)) %>%
      select(idState:countHeadquarters,
             countSingleFamilyHomes,
             urlImageSample)
    closeAllConnections()
    return(df)

  }

#' NAREIT properties by MSA
#'
#' This function returns aggregated REIT owned
#' properties by Metropolitan Statistical Area [MSA].
#'
#' @param return_message \code{TRUE} return a message after data import
#' @param nest_data \code{TRUE} return nested data frame
#' @references \href{http://nareit.org}{National Association of Real Estate Investment Trusts}
#' @return nested \code{data_frame} or \code{data_frame} if \code{nest_data = FALSE}
#' @export
#' @import purrr stringr dplyr jsonlite formattable lubridate tidyr readr
#' @family NAREIT
#' @family property data
#' @examples
#' \dontrun{
#' get_data_nareit_property_msa(nest_data = TRUE, return_message = TRUE)
#' }
get_data_nareit_property_msa <-
  function(nest_data = TRUE,
           return_message = TRUE) {
    json_data <-
      "http://www.reitsacrossamerica.com/data/msa_info.json" %>%
      fromJSON()

    df_property <-
      json_data$features$properties %>%
      as_data_frame() %>%
      select(-matches("_string")) %>%
      select(-type) %>%
      purrr::set_names(c(
        'countProperties',
        'amountValuationOwnedProperties',
        'nameMSA',
        'nameSector'
      )) %>%
      mutate(idLocation = 1:n()) %>%
      select(idLocation, nameMSA, nameSector, everything())

    df_lat_lon <-
      json_data$features$geometry %>%
      as_data_frame() %>%
      mutate(idLocation = 1:n()) %>%
      unnest()

    df_lat_lon <-
      df_lat_lon[c(TRUE, FALSE),] %>%
      mutate(item = 'coordinateLongitude') %>%
      list(.,
           df_lat_lon[c(FALSE, TRUE),] %>%
             mutate(item = 'coordinateLatitude')) %>%
      bind_rows() %>%
      select(idLocation, coordinates, item) %>%
      spread(item, coordinates)

    df_property <-
      df_property %>%
      inner_join(df_lat_lon) %>%
      select(idLocation, everything()) %>%
      mutate(
        amountValuationOwnedProperties = amountValuationOwnedProperties * 1000000 %>% formattable::currency(digits = 0),
        nameMSA = nameMSA %>% str_to_upper(),
        nameSector = nameSector %>% str_to_upper() %>% str_replace_all('\\-', ' ') %>% str_replace_all("DATA CENTER", "DATA CENTERS")
      ) %>%
      filter(!nameSector == "ALL TYPES") %>%
      suppressWarnings() %>%
      suppressMessages() %>%
      select(-idLocation)

    if (return_message) {
      list(
        df_property$countProperties %>% sum(na.rm = T) %>% formattable::comma(digits = 0),
        " REIT owned properties across ",
        df_property$nameMSA %>% unique() %>% length(),
        ' MSAs'
      ) %>%
        purrr::invoke(paste0, .) %>%
        message()
    }

    if (nest_data) {
      df_property <-
        df_property %>%
        nest(-c(nameMSA, coordinateLatitude, coordinateLongitude),
             .key = 'dataProperties')
    }
    closeAllConnections()
    return(df_property)

  }

#' NAREIT properties by state
#'
#' This function returns information about REIT owned
#' holdings by state.
#'
#' @param include_reit_hq \code{TRUE} include REIT headquarter locations
#' @param include_reit_holdings \code{TRUE} include REIT aggregate holdings
#' @param return_message \code{TRUE} return a message after data import
#' @param nest_data \code{TRUE} return nested data frame
#' @references \href{http://nareit.org}{National Association of Real Estate Investment Trusts}
#'
#' @return nested \code{data_frame} or \code{data_frame} if \code{nest_data = FALSE}
#' @export
#' @family NAREIT
#' @family property data
#' @examples
#' \dontrun{
#' get_data_nareit_state_info(include_reit_hq = TRUE, include_reit_holdings = TRUE, nest_data = TRUE, return_message = TRUE)
#' }
get_data_nareit_state_info <-
  function(include_reit_hq = TRUE,
           include_reit_holdings = TRUE,
           nest_data = TRUE,
           return_message = TRUE) {
    json_data <-
      "http://www.reitsacrossamerica.com/data/state_info.json" %>%
      fromJSON()

    df_property <-
      json_data$features$properties %>%
      as_data_frame() %>%
      select(-matches("_string")) %>%
      select(-type) %>%
      purrr::set_names(
        c(
          'countProperties',
          'amountValuationOwnedProperties',
          'codeState',
          'nameSector'
        )
      ) %>%
      mutate(idLocation = 1:n()) %>%
      select(idLocation, codeState, nameSector, everything())

    df_lat_lon <-
      json_data$features$geometry %>%
      as_data_frame() %>%
      mutate(idLocation = 1:n()) %>%
      unnest()

    df_lat_lon <-
      df_lat_lon[c(TRUE, FALSE),] %>%
      mutate(item = 'coordinateLongitude') %>%
      list(.,
           df_lat_lon[c(FALSE, TRUE),] %>%
             mutate(item = 'coordinateLatitude')) %>%
      bind_rows() %>%
      select(idLocation, coordinates, item) %>%
      spread(item, coordinates)

    df_property <-
      df_property %>%
      inner_join(df_lat_lon) %>%
      select(idLocation, everything()) %>%
      mutate(
        amountValuationOwnedProperties = amountValuationOwnedProperties * 1000000 %>% formattable::currency(digits = 0),
        nameSector = nameSector %>% str_to_upper() %>% str_replace_all('\\-', ' ') %>% str_replace_all("DATA CENTER", "DATA CENTERS")
      ) %>%
      filter(!nameSector == "ALL TYPES") %>%
      suppressWarnings() %>%
      suppressMessages()

    df_md <-
      parse_json_state_metadata()

    df_property <-
      df_md %>%
      left_join(df_property %>%
                  nest(-codeState, .key = dataPropertiesOwned)) %>%
      suppressWarnings() %>%
      suppressMessages()

    if (include_reit_hq) {
      states <-
        df_property$codeState %>%
        unique() %>%
        sort()

      url_states <-
        list("http://app.reitsacrossamerica.com/states/",
             states ,
             "/headquartered") %>%
        purrr::invoke(paste0, .)

      parse_json_hq_safe <-
        purrr::possibly(parse_json_hq, data_frame())

      df_hqs <-
        url_states %>%
        map_df(function(x) {
          parse_json_hq_safe(url = x,
                             return_message = return_message)
        })

      if (nest_data) {
        df_hqs <-
          df_hqs %>%
          nest(-codeState, .key = dataCompanyHQs)
      }

      df_property <-
        df_property %>%
        left_join(df_hqs) %>%
        suppressMessages()
    }

    if (include_reit_holdings) {
      states <-
        df_property$codeState %>%
        unique() %>%
        sort()

      url_states <-
        list("http://app.reitsacrossamerica.com/companies/holding/",
             states) %>%
        purrr::invoke(paste0, .)

      parse_json_holdings_safe <-
        purrr::possibly(parse_json_holdings, data_frame())

      holdings_df <-
        url_states %>%
        map_df(function(x) {
          parse_json_holdings_safe(url = x,
                                   return_message = return_message)
        })

      if (nest_data) {
        holdings_df <-
          holdings_df %>%
          nest(-codeState, .key = dataCompanyHoldings)
      }


      df_property <-
        df_property %>%
        left_join(holdings_df) %>%
        suppressMessages()
    }

    df_property <-
      df_property %>%
      mutate_at(df_property %>% select(matches("count")) %>% names(),
                funs(. %>% formattable::comma(digits = 0))) %>%
      arrange(desc(codeState))
    closeAllConnections()
    return(df_property)

  }


# returns -----------------------------------------------------------------

#' NAREIT monthly returns by sector
#'
#' This function acquires FTSE NAREIT monthly return data
#' by sector from 1971 to the prior month's end.
#'
#' @param return_wide \code{TRUE} return data in wide form
#' @param return_message \code{TRUE} return a message after data import
#'
#' @references \href{http://nareit.org}{National Association of Real Estate Investment Trusts}
#' @return \code{data_frame}
#' @export
#' @import dplyr purrr curl tidyr stringr formattable
#' @importFrom xlsx read.xlsx
#' @family NAREIT
#' @family index values
#' @examples
#' \dontrun{
#' get_data_nareit_monthly_returns(return_wide = FALSE)
#' }
get_data_nareit_monthly_returns <-
  function(return_wide = TRUE,
           return_message = TRUE) {
    url <-
      "https://www.reit.com/sites/default/files/returns/MonthlyHistoricalReturns.xls"

    tmp <-
      tempfile()
    curl::curl_download(url, tmp)
    data <-
      xlsx::read.xlsx(file = tmp, sheetIndex = 2) %>%
      data.frame(stringsAsFactors = FALSE) %>%
      suppressWarnings()

    tmp %>%
      unlink()

    index_items <-
      data %>%
      select(-1) %>%
      slice(5:7) %>%
      t() %>%
      as_data_frame() %>%
      fill(V1) %>%
      fill(V2) %>%
      purrr::set_names(c('nameIndex', 'item1', 'item2')) %>%
      mutate(
        nameIndex = nameIndex %>% str_replace_all('\\ ', '\\_') %>% str_to_upper(),
        item = list(item1, item2) %>% purrr::invoke(paste0, .),
        indexItem = paste(nameIndex, item, sep = ':')
      ) %>%
      .$indexItem

    items <-
      c('dateData', index_items)

    data <-
      data %>%
      slice(7:nrow(data)) %>%
      as_data_frame() %>%
      mutate_all(as.character) %>%
      purrr::set_names(items)

    data <-
      data %>%
      mutate(dateData = dateData %>% as.numeric() %>% as.Date(origin = "1899-12-30")) %>%
      gather(item, valueItem, -dateData) %>%
      mutate(valueItem = valueItem %>% as.numeric()) %>%
      filter(!valueItem %>% is.na()) %>%
      tidyr::separate(item, c('nameIndex', 'metricItem'), sep = '\\:') %>%
      mutate(
        nameIndex = nameIndex %>% str_replace_all('\\_', '\\ '),
        metricItem = metricItem %>% str_replace_all('\\ ', '')
      ) %>%
      left_join(data_frame(
        metricItem = c(
          "DividendYield",
          "IncomeReturn",
          "PriceIndex",
          "PriceReturn",
          "TotalIndex",
          "TotalReturn"
        ),
        nameItem = c(
          "pctDividendYield",
          "pctIncomeReturn",
          "indexPrice",
          "pctPriceReturn",
          "indexTotal",
          "pctTotalReturn"
        )
      )) %>%
      select(-metricItem) %>%
      suppressMessages() %>%
      suppressWarnings()

    data <-
      data %>%
      spread(nameItem, valueItem) %>%
      suppressWarnings()

    data <-
      data %>%
      mutate_at(.vars = data %>% select(matches("^index")) %>% names(),
                funs(. %>% formattable::comma(digits = 5))) %>%
      mutate_at(.vars = data %>% select(matches("^pct")) %>% names(),
                funs((. / 100) %>% formattable::percent(digits = 4))) %>%
      mutate(urlData = url) %>%
      suppressWarnings()

    if (return_message) {
      list(
        "Acquired FTSE NAREIT returns by sector from ",
        data$dateData %>% min(),
        ' to ',
        data$dateData %>% max()
      ) %>%
        purrr::invoke(paste0, .) %>%
        message()
    }

    if (!return_wide) {
      data <-
        data %>%
        gather(item, value, -c(dateData, nameIndex, urlData), convert = T)
    }
    closeAllConnections()
    return(data)
  }

#' NAREIT annual sub sector returns
#'
#' This function returns annual subsector returns
#' for NAREIT sub sectors from 1994 to prior year-end.
#'
#' @param return_wide \code{TRUE} return data in wide form
#' @param return_message \code{TRUE} return a message after data import
#' @references \href{http://nareit.org}{National Association of Real Estate Investment Trusts}
#' @return \code{data_frame}
#' @export
#' @import dplyr purrr curl tidyr stringr formattable
#' @importFrom xlsx read.xlsx
#' @family NAREIT
#' @family index values
#' @examples
#' \dontrun{
#' get_data_nareit_annual_subsector_returns(return_wide = TRUE, return_message = TRUE)
#' }
get_data_nareit_annual_subsector_returns <-
  function(return_wide = TRUE,
           return_message = TRUE) {
    url <-
      "https://www.reit.com/sites/default/files/returns/AnnualSectorReturns.xls"

    tmp <-
      tempfile()
    curl::curl_download(url, tmp)

    data <-
      xlsx::read.xlsx(file = tmp, sheetIndex = 2) %>%
      data.frame(stringsAsFactors = FALSE) %>%
      suppressWarnings()

    tmp %>%
      unlink()

    index_items <-
      data %>%
      select(-1) %>%
      slice(4:6) %>%
      t() %>%
      as_data_frame() %>%
      fill(V1) %>%
      fill(V2) %>%
      purrr::set_names(c('nameSector', 'nameSubSector', 'nameIndex')) %>%
      mutate(
        nameSubSector = nameSubSector %>% str_replace_all('\\ ', '\\_') %>% str_to_upper(),
        namePS = paste(nameSector, nameSubSector, sep = '-'),
        indexItem = paste(namePS, nameIndex, sep = ':')
      ) %>%
      .$indexItem

    items <-
      c('dateData', index_items)

    data <-
      data %>%
      slice(8:nrow(data)) %>%
      as_data_frame() %>%
      mutate_all(as.character) %>%
      select(1:24) %>%
      purrr::set_names(items[1:24]) %>%
      filter(!dateData %>% is.na())

    data <-
      data %>%
      mutate(dateData = dateData %>% as.numeric() %>% as.Date(origin = "1899-12-30")) %>%
      gather(item, valueItem, -dateData) %>%
      mutate(valueItem = valueItem %>% as.numeric()) %>%
      filter(!valueItem %>% is.na()) %>%
      tidyr::separate(item, c('nameSector', 'nameSubSector'), sep = '\\-') %>%
      tidyr::separate(nameSubSector, c('nameSubSector', 'metricItem'), sep = '\\:') %>%
      mutate(
        nameSubSector = nameSubSector %>% str_replace_all('\\_', '\\ ') %>% str_to_upper(),
        nameSector = nameSector %>% str_to_upper(),
        metricItem = metricItem %>% str_replace_all('\\ ', ''),
        metricItem = ifelse(metricItem == "Price", "pctPriceReturn", "pctTotalReturn")
      ) %>%
      suppressWarnings()

    data <-
      data %>%
      spread(metricItem, valueItem) %>%
      suppressWarnings()

    data <-
      data %>%
      mutate_at(.vars = data %>% select(matches("^pct")) %>% names(),
                funs((. / 100) %>% formattable::percent(digits = 4))) %>%
      mutate(urlData = url)

    if (return_message) {
      list(
        "Acquired annual FTSE NAREIT returns by subsector from ",
        data$dateData %>% min(),
        ' to ',
        data$dateData %>% max()
      ) %>%
        purrr::invoke(paste0, .) %>%
        message()
    }

    if (!return_wide) {
      data <-
        data %>%
        gather(item,
               value,
               -c(dateData, nameSector, nameSubSector, urlData),
               convert = T)
    }
    closeAllConnections()
    return(data)
  }


# offerings ---------------------------------------------------------------

# https://www.reit.com/data-research/data/reit-capital-offerings

get_nareit_offering_name_df <-
  function() {
    data_frame(
      nameNAREIT = c(
        "Name",
        "Type",
        "Property.Sector",
        "Property.Subsector",
        "Chairman",
        "City",
        "State",
        "Trade.Date",
        "Underwriter.1",
        "Underwriter.2",
        "Underwriter.3",
        "Underwriter.4",
        "Underwriter.5",
        "Underwriter.6",
        "Shares",
        "Price",
        "Total1",
        "Greenshoe",
        "Total2",
        "Grand.Total",
        "PRRANGE1",
        "PRRANGE2",
        "Offering.Description",
        "Overallotment",
        "Date",
        "Total",
        "Offering.Type"
      ),
      nameActual = c(
        "nameCompany",
        "typeInvestment",
        "nameSector",
        "nameSubSector",
        "nameCompanyChairman",
        "cityCompany",
        "stateCompany",
        "dateOffering",
        "nameUnderwriter",
        "nameUnderwriter1",
        "nameUnderwriter2",
        "nameUnderwriter3",
        "nameUnderwriter4",
        "nameUnderwriter5",
        "countShares",
        "priceOffering",
        "amountProceeds",
        "amountGreenShoe",
        "amountProceedsOther",
        "amountProceedsTotal",
        "priceRangeLow",
        "priceRangeHigh",
        'descriptionOffering',
        'countSharesOverallotment',
        'dateOffering',
        'amountProceedsTotal',
        "typeOffering"
      )

    )
  }

get_nareit_capital_urls <-
  function() {
    page <-
      "https://www.reit.com/data-research/data/reit-capital-offerings" %>%
      read_html()

    capital_urls <-
      page %>%
      html_nodes('p:nth-child(4) a') %>%
      html_attr('href') %>%
      paste0('https://www.reit.com', .) %>% {
        .[2:length(.)]
      }

    url_df <-
      data_frame(
        typeCapital = c('IPO', 'Secondary', 'Debt'),
        indexSheet = c(1, 2, 1),
        urlData = capital_urls
      )
    closeAllConnections()
    return(url_df)
  }

parse_nareit_offering_url <-
  function(url = "https://www.reit.com/sites/default/files/IndustryData/IPOs.xls",
           sheet = 1,
           return_message = TRUE) {
    tmp <-
      tempfile()
    con <-
      curl::curl_fetch_disk(url = url, path = tmp)

    data <-
      con$content %>%
      xlsx::read.xlsx(sheetIndex = sheet) %>%
      data.frame(stringsAsFactors = FALSE) %>%
      suppressWarnings() %>%
      as_data_frame()
    con %>% unlink()
    rm(con)
    data <-
      data %>%
      mutate_if(is.factor,
                as.character)

    if ("NA." %in% names(data)) {
      data <-
        data[, !names(data)  == "NA."]
    }

    nareit_offering_name_df <-
      get_nareit_offering_name_df()

    actual_names <-
      names(data) %>%
      map_chr(function(x) {
        nareit_offering_name_df %>%
          filter(nameNAREIT == x) %>%
          .$nameActual
      })

    data <-
      data %>%
      purrr::set_names(actual_names) %>%
      mutate_if(is_character,
                str_to_upper) %>%
      mutate(
        nameSubSector = ifelse(nameSubSector == '', NA, nameSubSector),
        dateOffering = dateOffering %>% lubridate::ymd()
      )

    data <-
      data %>%
      mutate_at(data %>% select(matches("^countShares|^amountProceeds")) %>% names(),
                funs(. %>% as.numeric() * 1000000)) %>%
      mutate_at(data %>% select(matches("^price")) %>% names(),
                funs(. %>% readr::parse_number())) %>%
      mutate(urlData = url) %>%
      select(dateOffering, nameCompany, everything())

    data <-
      data %>%
      mutate_at(data %>% select(matches(
        "nameUnderwriter|descriptionOffering"
      )) %>% names(),
      funs(ifelse(. == '', NA, .))) %>%
      mutate_if(is.character,
                funs(str_trim)) %>%
      mutate_if(is.character,
                funs(ifelse(. == '', NA, .)))

    has_shares <-
      names(data) %>% str_detect('countShares') %>% sum() > 0
    if (has_shares) {
      if ('countSharesOverallotment' %in% names(data)) {
        data <-
          data %>%
          mutate(
            priceOffering = ifelse(
              priceOffering %>% is.na(),
              amountProceedsTotal / sum(countSharesOverallotment, countShares, na.rm = T),
              priceOffering
            )
          )
      } else {
        data <-
          data %>%
          mutate(
            priceOffering = ifelse(
              priceOffering %>% is.na(),
              amountProceedsTotal / sum(countShares, na.rm = T),
              priceOffering
            )
          )
      }
    }

    if (return_message) {
      list("Parsed: ", url) %>%
        purrr::invoke(paste0, .) %>%
        message()
    }
    closeAllConnections()
    return(data)
  }

#' NAREIT capital raises
#'
#' This function acquires data on REIT
#' capital raises by form of capital since 1989.
#'
#' @param capital_type form of capital raised \itemize{
#' \item \code{NULL}: returns all types (default)
#' \item \code{IPO}: Initial Public Offering
#' \item \code{SECONDARY} :Secondary Equity Offering
#' \item \code{DEBT}: Debt Issuance
#' }
#' @param return_message \code{TRUE} return a message after data import
#' @param nest_data \code{TRUE} return nested data frame
#'
#' @return nested \code{data_frame} or \code{data_frame} if \code{nest_data = FALSE}#' @export
#' @import dplyr purrr curl tidyr stringr formattable
#' @importFrom xlsx read.xlsx
#' @family NAREIT
#' @family capital raises
#' @examples
#' \dontrun{
#' get_data_nareit_capital_raises(capital_type = NULL, nest_data = FALSE, return_message = TRUE)
#' }
#'
get_data_nareit_capital_raises <-
  function(capital_type = NULL,
           nest_data = TRUE,
           return_message = TRUE) {
    url_df <-
      get_nareit_capital_urls() %>%
      mutate(typeCapital = typeCapital %>% str_to_upper())

    if (!capital_type %>% is_null()) {
      capital_type <-
        capital_type %>% str_to_upper()
      missing_capital <-
        capital_type %in% url_df$typeCapital
      if (missing_capital) {
        stop("Capital type can only be IPO, SECONDARY or DEBT")
      }
      url_df <-
        url_df %>%
        filter(typeCapital %in% capital_type)
    }
    parse_nareit_offering_url_safe <-
      purrr::possibly(parse_nareit_offering_url, data_frame())

    all_data <-
      1:nrow(url_df) %>%
      map_df(function(x) {
        parse_nareit_offering_url_safe(
          url = url_df$urlData[[x]],
          sheet = url_df$indexSheet[[x]],
          return_message = return_message
        )
      }) %>%
      suppressWarnings()

    all_data <-
      all_data %>%
      left_join(url_df %>% select(urlData, typeCapital)) %>%
      select(typeCapital, everything()) %>%
      arrange(dateOffering) %>%
      mutate_at(all_data %>% select(matches("amount")) %>% names(),
                funs(. %>% formattable::currency(digits = 0))) %>%
      mutate_at(all_data %>% select(matches("count")) %>% names(),
                funs(. %>% formattable::comma(digits = 0))) %>%
      mutate_at(all_data %>% select(matches("price")) %>% names(),
                funs(. %>% formattable::comma(digits = 3))) %>%
      filter(!dateOffering %>% is.na()) %>%
      suppressMessages()
    if (return_message) {
      list(
        "Acquired NAREIT capital raise data for ",
        all_data %>% nrow() %>% formattable::comma(digits = 0),
        ' REITs from ',
        all_data$dateOffering %>% min(na.rm = TRUE),
        ' to ',
        all_data$dateOffering %>% max(na.rm = TRUE)
      ) %>%
        purrr::invoke(paste0, .) %>%
        message()
    }
    if (nest_data) {
      all_data <-
        all_data %>%
        nest(-typeCapital, .key = dataOfferingNAREIT)
    }
    closeAllConnections()
    return(all_data)
  }


# m&a ---------------------------------------------------------------------
#' NAREIT merger and acquisitions
#'
#' This function OCRs data on NAREIT member
#' M&A activity since 2004.
#'
#' @param return_wide \code{TRUE} return data in wide form
#' @param return_message \code{TRUE} return a message after data import
#' @param nest_data \code{TRUE} return nested data frame
#' @return nested \code{data_frame} or \code{data_frame} if \code{nest_data = FALSE}
#' @export
#' @import purrr stringr dplyr rvest formattable lubridate tidyr readr
#' @family NAREIT
#' @family transaction data
#' @examples
#' \dontrun{
#' get_data_nareit_mergers_acquisitions(nest_data = FALSE, return_message = TRUE)
#' }
get_data_nareit_mergers_acquisitions <-
  function(pages = 32:33,
           nest_data = FALSE,
           return_message = TRUE) {
    page <-
      "https://www.reit.com/data-research/data/reitwatch" %>%
      read_html()

    url <-
      page %>%
      html_nodes('#block-system-main a') %>%
      html_attr('href') %>%
      .[[1]] %>%
      paste0('https://www.reit.com', .)

    tables <-
      url %>%
      extract_tables(pages = pages)

    all_data <-
      1:length(tables) %>%
      map_df(function(x) {
        tables[[x]] %>%
          as_data_frame()
      }) %>%
      slice(3:nrow(.)) %>%
      mutate(V1 = ifelse(V1 == '', NA, V1)) %>%
      filter(!V2 %>% str_detect("Total")) %>%
      mutate_all(funs(. %>% str_trim() %>% str_to_upper()))

    all_data <-
      all_data %>%
      dplyr::rename(
        yearMerger = V1,
        nameAcquiror = V2,
        nameTarget = V3
      ) %>%
      fill(yearMerger) %>%
      mutate(nameAcquiror = ifelse(nameAcquiror == '', NA, nameAcquiror)) %>%
      filter(!nameAcquiror %>% is.na()) %>%
      filter(!yearMerger == 'YEAR') %>%
      select(-c(V5, V7))

    all_data <-
      all_data %>%
      mutate(
        typeAcquiror = ifelse(V4 == '', V6, V4),
        amountAcquisitionPrice =  ifelse(V4 == '', V8, V6) %>% readr::parse_number() * 1000000,
        dateAnnounced = ifelse(V4 == '', V11, V8) %>% lubridate::dmy(),
        dateComplete =  ifelse(V4 == '', V12, V9) %>% lubridate::dmy(),
        statusTransaction = ifelse(V4 == '', V13, V10)
      ) %>%
      select(-matches("^V")) %>%
      suppressWarnings()

    all_data <-
      all_data %>%
      mutate(
        isPublicAcquiror = ifelse(typeAcquiror %>% str_detect("PUBLIC"), TRUE, FALSE),
        isJV = ifelse(typeAcquiror %>% str_detect("JV|JOINT VENTURE"), TRUE, FALSE),
        isComplete = ifelse(statusTransaction == "COMPLETED", TRUE, FALSE),
        idTransaction = 1:n()
      ) %>%
      select(idTransaction, everything()) %>%
      mutate(nameAcquiror = nameAcquiror %>% str_replace_all(' & |\\/|\\ / ', '; ')) %>%
      suppressWarnings()

    all_data <-
      all_data %>%
      filter(!nameTarget == '') %>%
      bind_rows(
        all_data %>%
          filter(nameTarget == '') %>%
          select(-c(nameAcquiror, nameTarget)) %>%
          left_join(
            data_frame(
              idTransaction = c(32, 60),
              nameAcquiror = c(
                'Westmont Hospitality; Cadim Inc. (Braveheart Holdings LP)',
                "Credit-Based Asset Servicing & Securitization LLC"
              ),
              nameTarget = c('Boykin Lodging Company',
                             'Fieldstone Investment Corporation')
            ) %>%
              mutate_if(is.character,
                        str_to_upper)
          )
      ) %>%
      arrange(idTransaction) %>%
      suppressWarnings() %>%
      mutate(nameAcquiror = nameAcquiror %>% str_replace_all(" AND ", '; ')) %>%
      suppressWarnings() %>%
      suppressMessages()

    all_data <-
      all_data %>%
      mutate(
        nameAcquiror = ifelse(
          nameAcquiror == "PL RETAIL LLC (KIMCO REALTY; DRA ADVISORS)",
          "KIMCO REALTY; DRA ADVISORS (PL RETAIL LLC)",
          nameAcquiror
        )
      ) %>%
      separate(
        nameAcquiror,
        into = c('nameAcquiror', 'nameAcquirorAKA'),
        sep = '\\('
      ) %>%
      separate(nameTarget,
               into = c('nameTarget', 'nameTargetAKA'),
               sep = '\\(') %>%
      mutate(
        nameAcquirorAKA = nameAcquirorAKA %>% str_replace_all('[)]', ''),
        nameTargetAKA = nameTargetAKA %>% str_replace_all('[)]', ''),
        nameAcquirorAKA = nameAcquirorAKA %>% str_trim(),
        yearMerger = yearMerger %>% as.integer(),
        nameAcquiror = nameAcquiror %>% str_trim(),
        nameTargetAKA = nameTargetAKA %>% str_trim(),
        nameTarget = nameTarget %>% str_trim(),
        amountAcquisitionPrice = amountAcquisitionPrice %>% formattable::currency(digits = 0),
        urlData = url
      ) %>%
      suppressWarnings()

    all_data <-
      all_data %>%
      mutate(
        isJV = typeAcquiror %>% str_detect("JV"),
        typeAcquiror = typeAcquiror %>% str_replace_all("JV|JV-", '') %>% str_trim(),
        typeAcquiror = typeAcquiror %>% str_replace_all('\\-', '') %>% str_trim()
      )

    if (return_message) {
      list(
        "You acquired ",
        all_data %>% nrow() %>% formattable::comma(digits = 0),
        ' REIT M&A transactions accounting for ',
        all_data$amountAcquisitionPrice %>% sum(na.rm = T),
        ' in value from ',
        all_data$dateAnnounced %>% min(na.rm = T),
        ' to ',
        all_data$dateAnnounced %>% max(na.rm = T)
      ) %>%
        purrr::invoke(paste0, .) %>%
        message()
    }

    if (nest_data) {
      all_data <-
        all_data %>%
        nest(-yearMerger, .key = 'dataTransactions')
    }
    closeAllConnections()
    return(all_data)
  }


# t-trackr ----------------------------------------------------------------

#' NAREIT quarterly industry tracking data
#'
#' This function acquires data on NAREIT
#' performance metrics including Funds From Operation [FFO],
#' Dividends, and Net Operating Income from 2000.
#'
#' @param return_wide \code{TRUE} return data in wide form
#' @param return_message \code{TRUE} return a message after data import
#' @param nest_data \code{TRUE} return nested data frame
#' @return nested \code{data_frame} or \code{data_frame} if \code{nest_data = FALSE}
#' @export
#' @import dplyr purrr curl tidyr stringr formattable rvest
#' @importFrom xlsx read.xlsx
#' @family NAREIT
#' @examples
#' \dontrun{
#' get_data_nareit_industry_tracker(return_message = TRUE, return_wide = TRUE)
#' }
get_data_nareit_industry_tracker <-
  function(nest_data = FALSE,
           return_wide = TRUE,
           return_message = TRUE) {
    page <-
      "https://www.reit.com/data-research/data/nareit-t-tracker-quarterly-total-return-index-series" %>%
      read_html()

    links <-
      page %>%
      html_nodes('a') %>%
      html_attr('href')

    links <-
      links[links %>% str_detect(".xlsx")]

    url <-
      links[!links %>% is.na()] %>%
      paste0('https://www.reit.com', .) %>%
      .[[1]]

    tmp <-
      tempfile()
    curl::curl_download(url, tmp)

    data <-
      xlsx::read.xlsx(file = tmp,
                      sheetIndex = 1,
                      header = FALSE) %>%
      data.frame(stringsAsFactors = FALSE) %>%
      suppressWarnings() %>%
      as_data_frame()

    tmp %>%
      unlink()

    data <-
      data %>%
      mutate_if(is.factor,
                as.character)

    years_quarters <-
      data %>%
      select(-X1) %>%
      slice(2) %>%
      t() %>%
      as.character()

    data <-
      data %>%
      filter(!X1 %>% is.na()) %>%
      mutate(idRow = 1:n())

    data <-
      data %>%
      filter(
        !X1 %>% str_detect(
          "Percent change|NOI per share|NOI/shr|All |Dividends per share|Div/shr|Source:|FFO/shr|FFO per share"
        )
      )

    item_df <-
      data %>%
      filter(X2 %>% is.na()) %>%
      select(idRow, item = X1) %>%
      left_join(data_frame(
        item = c("FFO", "NOI", "Dividends paid", "Same Store NOI"),
        nameItem = c('FFO', "NOI", 'Dividends', 'NOISameStore')
      )) %>%
      mutate(idRow = idRow + 2) %>%
      select(idRow, nameItem) %>%
      suppressMessages()

    units_measure_df <-
      data %>%
      filter(X2 == "2000.1") %>%
      select(idRow, nameMeasure = X1) %>%
      left_join(data_frame(
        nameMeasure = c("millions of dollars", "percent change over 4 quarters"),
        typeUOM = c('amount', 'pctChangeYear')
      )) %>%
      select(idRow, typeUOM) %>%
      mutate(idRow = idRow + 1) %>%
      suppressMessages()

    data <-
      data %>%
      filter(!X2 == "2000.1") %>%
      select(idRow, everything())

    item_df <-
      item_df %>%
      left_join(units_measure_df) %>%
      unite(nameItem, typeUOM, nameItem, sep = '') %>%
      suppressMessages()

    data <-
      data %>%
      purrr::set_names(c('idRow', 'nameSector', years_quarters))

    data <-
      data %>%
      left_join(item_df) %>%
      select(nameItem, everything()) %>%
      fill(nameItem) %>%
      select(-idRow) %>%
      gather(yearQuarter, value, -c(nameSector, nameItem)) %>%
      mutate(
        value = value %>% readr::parse_number(),
        nameSector = nameSector %>% str_to_upper()
      ) %>%
      filter(!value %>% is.na()) %>%
      mutate(value = ifelse(nameItem %>% str_detect('amount'), value * 1000000, value / 100)) %>%
      suppressMessages() %>%
      suppressWarnings()

    data <-
      data %>%
      spread(nameItem, value) %>%
      tidyr::separate(
        yearQuarter,
        remove = FALSE,
        sep = '\\.',
        into = c('yearData', 'quarterData')
      ) %>%
      mutate_at(c('yearData', 'quarterData'),
                funs(. %>% as.numeric())) %>%
      arrange(yearData, quarterData) %>%
      mutate(urlData = url)

    data <-
      data %>%
      mutate(pctDividendPayout = amountDividends / amountFFO,
             pctFFOMargin = amountFFO / amountNOI) %>%
      select(
        yearQuarter,
        yearData,
        quarterData,
        nameSector,
        amountDividends:pctChangeYearNOISameStore,
        pctDividendPayout,
        pctFFOMargin,
        everything()
      )

    data <-
      data %>%
      mutate_at(data %>% select(matches("amount")) %>% names(),
                funs(. %>% formattable::currency(digits = 0))) %>%
      mutate_at(data %>% select(matches("pct")) %>% names(),
                funs(. %>% formattable::percent(digits = 3))) %>%
      mutate(urlData = url)

    if (!return_wide) {
      data <-
        data %>%
        gather(item,
               value,
               -c(nameSector, yearQuarter, yearData, quarterData, urlData))
    }

    if (return_message) {
      list("Parsed: ", url) %>%
        purrr::invoke(paste0, .) %>%
        message()
    }

    if (nest_data) {
      data <-
        data %>%
        nest(-yearData, .key = 'dataTracker')
    }
    closeAllConnections()
    return(data)
  }



# reit_funds --------------------------------------------------------------


parse_fund_reit_page <-
  function(return_message = TRUE) {
    page <-
      "https://www.reit.com/investing/investing-reits/list-reit-funds" %>%
      xml2::read_html()

    fund_stems <-
      page %>%
      html_nodes('.data a') %>%
      html_attr('href')

    urls <-
      fund_stems %>%
      paste0('https://www.reit.com', .)

    tickers <-
      fund_stems %>%
      str_replace_all('\\/morningstar/ticker/', '') %>% str_to_upper()

    funds <-
      page %>%
      html_nodes('.data a') %>%
      html_text() %>%
      str_to_upper()
    column_df <-
      data_frame(
        number = 2:7,
        nameColumn = c(
          'ratingMorningstar',
          'pctReturnYTD',
          'pctReturn3YR',
          'pctReturn5YR',
          'amountInvestmentMinimum',
          'pctExpenses'
        )
      )
    df_values <-
      2:7 %>%
      map_df(function(x) {
        item <-
          column_df %>% filter(number == x) %>% .$nameColumn

        css_node <-
          list('td:nth-child(' , x, ') .data') %>%
          purrr::reduce(paste0)

        items <-
          page %>%
          html_nodes(css_node) %>%
          html_text()

        items <-
          items %>% gsub('n/a', NA, .)

        data_frame(item, value = items)
      }) %>%
      group_by(item) %>%
      mutate(idRow = 1:n()) %>%
      ungroup() %>%
      spread(item, value) %>%
      filter(!amountInvestmentMinimum %>% is.na())

    df_values <-
      df_values %>%
      mutate_at(df_values %>% select(matches("amount|pct")) %>% names(),
                funs(. %>% readr::parse_number())) %>%
      mutate_at(df_values %>% select(matches("amount")) %>% names(),
                funs(. %>% formattable::currency())) %>%
      mutate_at(df_values %>% select(matches("pct")) %>% names(),
                funs((. / 100) %>% formattable::percent(digits = 3)))

    df <-
      data_frame(nameManager = funds,
                 idTicker = tickers,
                 urlNAREIT = urls) %>%
      mutate(idRow = 1:n()) %>%
      left_join(df_values) %>%
      suppressMessages()
    closeAllConnections()
    return(df)

  }

parse_reit_fund_info_page <-
  function(urls,
           return_message = TRUE) {
    df <-
      data_frame()
    success <- function(res) {
      works <- res$status_code == 200
      if (works) {
        page <-
          res$url %>%
          xml2::read_html()

        values <-
          page %>%
          html_nodes('.fund-summary .data') %>%
          html_text()

        table_values <-
          page %>%
          html_nodes('.number .data') %>%
          html_text()
        is_billions <-
          values %>% str_detect("bil") %>% sum() > 0
        items <-
          c(
            'amountNAV',
            'pctReturn1Day',
            'pctYieldTTM',
            'amountAssets',
            'pctExpenses',
            'pctTurnover',
            'amountInvestmentMinimum',
            'pctYield30Day'
          )

        data <-
          data_frame(item = items, value  = values) %>%
          mutate(value = value %>% readr::parse_number()) %>%
          spread(item, value) %>%
          suppressWarnings()

        data <-
          data %>%
          mutate(
            amountAssets = ifelse(
              is_billions,
              amountAssets * 1000000000,
              amountAssets * 1000000
            ),
            amountAssets = amountAssets %>% formattable::currency(digits = 0)
          ) %>%
          mutate(urlNAREIT = res$url)

        period <-
          c('YTD' , '1Month', '1YR', '3YR', '5YR', '10YR')

        periods <-
          rep(period, 3)

        items <-
          c(
            rep('pctFundPerformance', 6),
            rep('pctRankFundPerformance', 6),
            rep('rankFundPerformance', 6)
          )
        is_equal_length <- length(items) == length(table_values)
        if (is_equal_length) {
          data_perform <-
            data_frame(item = items,
                       period = periods,
                       value = table_values)

          data_perform <-
            data_perform %>%
            mutate(value = value %>% readr::parse_number()) %>%
            mutate(value = ifelse(item %>% str_detect("pct"), value / 100, value)) %>%
            tidyr::unite(item, item, period, sep = '') %>%
            spread(item, value) %>%
            suppressWarnings() %>%
            mutate(urlNAREIT = res$url)

          data <-
            data %>%
            left_join(data_perform) %>%
            suppressMessages()
        }

        if (return_message) {
          list("Parsed: ", res$url) %>%
            purrr::reduce(paste0) %>%
            message()
        }
        rm(page)
        closeAllConnections()

        df <<-
          df %>%
          bind_rows(data)
      }
    }
    failure <- function(msg) {
      cat(sprintf("Fail: %s (%s)\n", res$url, msg))
    }
    urls %>%
      walk(function(x) {
        curl_fetch_multi(url = x, success, failure)
      })
    multi_run()
    closeAllConnections()
    df
  }

#' REIT fund manager data.
#'
#' This function acquires information for
#' REIT fund vehicles
#'
#' @param parse_fund_details \code{TRUE} parses fund details
#' @param return_message \code{TRUE} return a message after data import
#'
#' @return \code{data_frame}
#' @export
#' @import dplyr stringr xml2 rvest purrr tidyr formattable
#' @family NAREIT
#' @family entity search
#' @family fund data
#' @examples
#' \dontrun{
#' get_data_reit_funds(parse_fund_details = TRUE, return_message = TRUE))
#' }

get_data_reit_funds <-
  function(parse_fund_details = TRUE,
           return_message = TRUE) {
    df_table <-
      parse_fund_reit_page()

    if (parse_fund_details) {
      parse_reit_fund_info_page_safe <-
        purrr::possibly(parse_reit_fund_info_page, data_frame())

      urls <-
        df_table$urlNAREIT

      detail_df <-
        urls %>%
        map_df(function(x){
          parse_reit_fund_info_page_safe(urls = x, return_message = return_message)
        })

      detail_df <-
        detail_df %>%
        mutate_at(detail_df %>% select(matches("pct")) %>% names(),
                  funs(. / 100))

      df_table <-
        df_table %>%
        left_join(detail_df) %>%
        suppressMessages()

      df_table <-
        df_table %>%
        mutate_at(df_table %>% select(matches("amount")) %>% names(),
                  funs(. %>% formattable::currency())) %>%
        mutate_at(df_table %>% select(matches("pct")) %>% names(),
                  funs(. %>% formattable::percent()))
    }

    df_table <-
      df_table %>%
      select(-matches("idRow"))

    if (return_message) {
      list(
        "Acquired ",
        df_table %>% nrow() %>% formattable::comma(digits = 0),
        ' REIT funds'
      ) %>%
        purrr::invoke(paste0, .) %>%
        message()
    }

    return(df_table)
  }

# reitwatch ---------------------------------------------------------------
