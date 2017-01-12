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
      slugs %>% paste0('https://www.reit.com',.)

    data_frame(yearData = year,
               urlData = urls)
  }

parse_nareit_constituent_url <-
  function(url = "https://www.reit.com/sites/default/files/returns/FNUSIC2016.pdf",
           return_message = TRUE) {
  df_metadata <-
    url %>%
    tabulizer::extract_metadata() %>%
    flatten_df()

  year_data <-
    url %>%
    str_replace_all('https://www.reit.com/sites/default/files/returns/|.pdf|https://www.reit.com//sites/default/files/returns/', '') %>%
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
      purrr::set_names(c(
        'nameCompany',
        'idTicker',
        'typeInvestment',
        'nameSector'
      )) %>%
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
      mutate(nameCompany = nameCompany %>% str_replace(' ',':')) %>%
      tidyr::separate(nameCompany, into = c('rank', 'nameCompany'), sep = '\\:') %>%
      left_join(sector_df) %>%
      fill(nameSector) %>%
      select(-c(rank, idRow)) %>%
      mutate(nameSector = ifelse(nameSector == '', NA, nameSector)) %>%
      select(nameCompany, idTicker, nameSector, everything()) %>%
      mutate(yearData = year_data,
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
      purrr::set_names(c(
        'nameCompany',
        'idTicker',
        'typeInvestment',
        'idSubSector',
        'amountEquityMarketCap'
      )) %>%
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
      separate(V1, sep = '\\:', into = c('rank', 'nameCompany')) %>%
      mutate(idTicker = ifelse(V6 %>% is.na(), V2, V3),
             typeInvestment = ifelse(V6 %>% is.na(), V3, V4),
             nameSubSector = ifelse(V6 %>% is.na(), V4, V5),
             amountEquityMarketCap = ifelse(V6 %>% is.na(), V5, V6)
             ) %>%
      select(nameCompany, idTicker, typeInvestment, nameSubSector, amountEquityMarketCap, idRow) %>%
      left_join(sector_df) %>%
      mutate_all(str_to_upper) %>%
      fill(nameSector) %>%
      select(-idRow) %>%
      select(nameCompany, idTicker, nameSector, everything()) %>%
      mutate(amountEquityMarketCap = amountEquityMarketCap %>% readr::parse_number() * 1000000 %>% formattable::currency(digits = 0),
             nameSubSector = ifelse(nameSubSector == '', NA, nameSubSector)) %>%
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
      purrr::set_names(c('nameSector', 'nameCompany', 'idTicker', 'typeInvestment', 'nameSubSector','amountEquityMarketCap')) %>%
      mutate(nameSubSector =  ifelse(nameSubSector == '', NA, nameSubSector),
             amountEquityMarketCap = amountEquityMarketCap %>% readr::parse_number() * 1000000 %>% formattable::currency(digits = 0))

  all_data <-
    all_data %>%
    select(-matches("urlData")) %>%
    mutate(urlData = url) %>%
    mutate(dateFile = date_data,
           yearData = year_data) %>%
    select(yearData, dateFile, everything())
  }

  if (return_message) {
    list("Acquired ", all_data %>% nrow() %>% formattable::comma(digits = 0), ' REITs for the year ', year_data) %>%
      purrr::invoke(paste0, .) %>%
      message()
  }
  return(all_data)
  }

#' Get NAREIT index constuiency data by year
#'
#' @param years years of data you wa
#' @param resolve_names
#' @param nest_data
#' @param return_message
#'
#' @return
#' @export
#' @import purrr stringr dplyr rvest tabulizer formattable lubridate tidyr readr
#' @examples
#' get_data_nareit_constituent_years(years = 1991:2016, resolve_names = TRUE, nest_data = TRUE, return_messag = TRUE)
get_data_nareit_constituent_years <-
  function(years = 1991:2016,
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
      map_df(function(x){
        x %>% message()
        parse_nareit_constituent_url(url = x, return_message = return_message)
      })

    all_data <-
      all_data %>%
      mutate(nameCompanyChar = nameCompany %>% nchar()) %>%
      filter(!idTicker %>% str_detect("SUMMARY|PROPERTY SECTOR")) %>%
      mutate(nameCompany = ifelse(nameCompanyChar < 4, idTicker, nameCompany),
             idTicker = ifelse(nameCompany == idTicker, typeInvestment, idTicker),
             typeInvestment = ifelse(typeInvestment == idTicker, idSubSector, typeInvestment),
             idSubSector = ifelse(idSubSector == typeInvestment, NA, idSubSector)
             ) %>%
      filter(nameCompanyChar > 4) %>%
      select(-nameCompanyChar)

    all_data <-
      all_data %>%
      mutate(urlData = list('https://www.reit.com/sites/default/files/returns/FNUSIC', yearData, '.pdf') %>% purrr::invoke(paste0,.)) %>%
      mutate(amountEquityMarketCap = amountEquityMarketCap %>% formattable::currency(digits = 0))

    replace_number <-
      list("^", 1:99, " ") %>%
      purrr::invoke(paste0,.) %>%
      paste0(collapse = '|')

    all_data <-
      all_data %>%
      filter(!idTicker %in% c('COMPANY')) %>%
      mutate(nameCompany = nameCompany %>% str_replace(replace_number, ''),
             idTicker = idTicker %>% str_trim())

    if ('idSubsSector' %in% names(all_data)) {
      all_data <-
        all_data %>%
        select(yearData:idSubSector, nameSector, nameSubSector, everything())
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
          select(yearData:idTicker, nameSector, nameSubSector, everything()) %>%
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
        left_join(ticker_df %>% select(-c(idRow,n))) %>%
        select(yearData, dateFile, nameCompany, everything()) %>%
        suppressMessages()
    }
    if (return_message) {
      list("Acquired REIT index constituency data from ", all_data$dateFile %>% min(na.rm = T), ' to ', all_data$dateFile %>% max(na.rm = TRUE)) %>%
        purrr::invoke(paste0,.) %>%
        message()
    }
    if (nest_data) {
      all_data <-
        all_data %>%
        nest(-c(yearData, dateFile), .key = 'dataConstituents')
    }
    return(all_data)
  }


# reits -------------------------------------------------------------------

#' Get NAREIT member dictionary
#'
#' @return
#' @export
#' @import purrr stringr dplyr rvest formattable lubridate tidyr readr
#' @examples
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
      map_chr(function(x){
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
      paste0('http://www.reit.com',.)

    url_webpage <-
      urls[c(FALSE, TRUE)] %>%
      str_to_lower()

    url_webpage[url_webpage == 'http://'] <-
      NA

    url_webpage <-
      url_webpage %>% str_replace_all('http://https://|http://http://', 'http://')

    url_df <-
      data_frame(nameCompany = entities,
                 idTicker = tickers[1:length(entities)],
                 urlNAREIT = url_nareit_data,
                 urlCompany = url_webpage,
                 urlData = url) %>%
      mutate_all(str_trim) %>%
      mutate(isPrivateNonPublicREIT = ifelse(idTicker %>% is.na(), TRUE, FALSE)) %>%
      select(nameCompany, idTicker, isPrivateNonPublicREIT, everything())

    return(url_df)
  }

parse_reit_info_page <-
  function(url = "https://www.reit.com/company/alexandria-real-estate-equities-inc",
           return_message = TRUE) {
    page <-
      url %>%
      read_html()

    company <-
      page %>%
      html_nodes('#page-title') %>%
      html_text() %>%
      str_to_upper()


    values <-
      page %>%
      html_nodes('.field-content') %>%
      html_text() %>%
      str_to_upper()

    items <-
      c('locationCompany',
        'typeCompany',
        'statusListing',
        'nameSector',
        'typeInvestment',
        'idTicker',
        'nameExchange')

    df <-
      data_frame(value = values,
               item = items[1:length(values)],
               nameCompany = company) %>%
      spread(item, value) %>%
      mutate(urlNAREIT = url)

    if (return_message) {
      list("Parsed: ", url) %>%
        purrr::invoke(paste0, .) %>%
        message()
    }

    return(df)
  }


#' Get NAREIT constituent data
#'
#' @param include_private_non_public include privately traded or over-the-counter traded REITs \code{TRUE, FALSE}
#' @param return_message return a message \code{TRUE, FALSE}
#'
#' @return
#' @export
#' @import purrr stringr dplyr rvest formattable lubridate tidyr readr
#' @examples
#' get_data_nareit_entities(include_private_non_public = TRUE, return_message = TRUE)
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
        filter(isPrivateNonPublicREIT == F)
    }

    all_data <-
      reits_df$urlNAREIT %>%
      map_df(function(x){
        parse_reit_info_page(url = x, return_message = return_message)
      })

    all_data <-
      all_data %>%
      mutate_all(funs(ifelse(. == '', NA, .))) %>%
      mutate(idTicker = ifelse(idTicker %in% c('','N/A'), NA, idTicker),
             locationCompany = ifelse(locationCompany %in% c('','N/A'), NA, locationCompany),
             nameExchange = ifelse(idTicker %>% is.na(), NA, nameExchange)) %>%
      filter(!nameCompany == "NAREIT") %>%
      tidyr::separate(locationCompany, sep = '\\, ', remove = FALSE, into = c('cityCompany', 'stateCompany'))

    if (return_message) {
      list("Returned information for ", all_data %>% nrow(), ' REITs') %>%
        purrr::invoke(paste0,.) %>%
        message()
    }

    return(all_data)
  }




# reit_properties ---------------------------------------------------------
#' Get NAREIT notable properties
#'
#' @return
#' @export
#' @import purrr stringr dplyr jsonlite formattable lubridate tidyr readr
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
      map_chr(function(x){
        if (df$image_url[[x]] %>% length() == 0) {
          return(NA)
        }
        df$image_url[[x]] %>%
          .[[1]] %>%
          paste0('http://app.reitsacrossamerica.com',.)
      })

    df <-
      df %>%
      mutate(image_url = images) %>%
      select(-c(slug_url, msa_code, primary_property_type)) %>%
      purrr::set_names(c('idProperty', 'nameProperty', 'nameCompany', 'coordinateLatitude', 'coordinateLongitude',
                         'nameSector', 'stateProperty', 'cityProperty', 'msaProperty', 'zipcodeProperty', 'addressProperty', 'urlImageProperty', 'urlVideoProperty',
                         'descriptionProperty')) %>%
      mutate_at(.cols = c('coordinateLongitude', 'coordinateLongitude'),
                funs(. %>% as.numeric()))

    df <-
      df %>%
      mutate_at(df %>%
                  keep(is_character) %>%
                  select(-matches("url")) %>% names(),
                funs(. %>% str_to_upper()))

    return(df)
  }

parse_json_hq <-
  function(url = "http://app.reitsacrossamerica.com/states/NY/headquartered", return_message = TRUE) {
    codeState <-
      url %>%
      str_replace_all('http://app.reitsacrossamerica.com/states/|/headquartered', '')

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
      list(df_hq %>% nrow() %>% formattable::comma(digits = 0), " REITs are headquartered in ", codeState) %>%
        purrr::invoke(paste0,.) %>%
        message()
    }
    return(df_long)
  }

parse_json_holdings <-
  function(url = "http://app.reitsacrossamerica.com/companies/holding/NY", return_message = TRUE) {
    codeState <-
      url %>%
      str_replace_all('http://app.reitsacrossamerica.com/companies/holding/', '')

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
      mutate(idSNL = idSNL %>% as.numeric(),
             nameCompany = nameCompany %>% str_to_upper(),
             urlCompany = ifelse(urlCompany == '', NA, urlCompany)) %>%
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
      list(df_holdings %>% nrow() %>% formattable::comma(digits = 0), " REITs own assets in ", codeState) %>%
        purrr::invoke(paste0,.) %>%
        message()
    }
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
      mutate(image_path = image_path %>% flatten_chr() %>% paste0('http://app.reitsacrossamerica.com/',.)) %>%
      mutate(id = id %>% as.numeric()) %>%
      purrr::set_names(c('idState', 'codeState', 'amountValuationOwnedProperties', 'countProperties', 'countAcresTimberland',
                         'countCellTowers', 'countAssets', 'countHeadquarters', 'urlImageSample', 'countSingleFamilyHomes'))

    df <-
      df %>%
      mutate_at(df %>% select(matches('idState|count|amount')) %>% names(),
                funs(. %>% as.numeric())) %>%
      mutate(amountValuationOwnedProperties = amountValuationOwnedProperties * 1000000 %>% formattable::currency(digits = 0)) %>%
      select(idState:countHeadquarters, countSingleFamilyHomes, urlImageSample)

    return(df)

  }

#' Get NAREIT Member Property Holdings by MSA
#'
#' @param return_message return a message \code{TRUE, FALSE}
#'
#' @return
#' @export
#' @import purrr stringr dplyr jsonlite formattable lubridate tidyr readr
#' @examples
#' get_data_nareit_property_msa(nest_data = TRUE, return_message = TRUE)
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
      purrr::set_names(c('countProperties', 'amountValuationOwnedProperties', 'nameMSA', 'nameSector')) %>%
      mutate(idLocation = 1:n()) %>%
      select(idLocation, nameMSA, nameSector, everything())

    df_lat_lon <-
      json_data$features$geometry %>%
      as_data_frame() %>%
      mutate(idLocation = 1:n()) %>%
      unnest()

    df_lat_lon <-
      df_lat_lon[c(TRUE, FALSE), ] %>%
      mutate(item = 'coordinateLongitude') %>%
      list(.,
           df_lat_lon[c(FALSE, TRUE), ] %>%
             mutate(item = 'coordinateLatitude')) %>%
      bind_rows() %>%
      select(idLocation, coordinates, item) %>%
      spread(item, coordinates)

    df_property <-
      df_property %>%
      inner_join(df_lat_lon) %>%
      select(idLocation, everything()) %>%
      mutate(amountValuationOwnedProperties = amountValuationOwnedProperties * 1000000 %>% formattable::currency(digits = 0),
             nameMSA = nameMSA %>% str_to_upper(),
             nameSector = nameSector %>% str_to_upper() %>% str_replace_all('\\-', ' ') %>% str_replace_all("DATA CENTER", "DATA CENTERS")) %>%
      filter(!nameSector == "ALL TYPES") %>%
      suppressWarnings() %>%
      suppressMessages() %>%
      select(-idLocation)

    if (nest_data) {
      df_property <-
        df_property %>%
        nest(-c(nameMSA, coordinateLatitude, coordinateLongitude), .key = 'dataProperties')
    }

    if (return_message) {
      list(df_property$countProperties %>% sum(na.rm = T) %>% formattable::comma(digits = 0), " REIT owned properties across ", df_property$nameMSA %>% unique() %>% length(), ' MSAs') %>%
        purrr::invoke(paste0,.) %>%
        message()
    }

    return(df_property)

  }

#' Get NAREIT member property holdings by state
#'
#' @param include_reit_hq include REIT headquarter locations \code{TRUE, FALSE}
#' @param include_reit_holdings include REIT aggregate holdings \code{TRUE, FALSE}
#' @param nest_data return a nested data frame \code{TRUE, FALSE}
#' @param return_message return a message \code{TRUE, FALSE}
#'
#' @return
#' @export
#'
#' @examples
#' get_data_nareit_state_info(include_reit_hq = TRUE, include_reit_holdings = TRUE, nest_data = TRUE, return_message = TRUE)
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
      purrr::set_names(c('countProperties', 'amountValuationOwnedProperties', 'codeState', 'nameSector')) %>%
      mutate(idLocation = 1:n()) %>%
      select(idLocation, codeState, nameSector, everything())

    df_lat_lon <-
      json_data$features$geometry %>%
      as_data_frame() %>%
      mutate(idLocation = 1:n()) %>%
      unnest()

    df_lat_lon <-
      df_lat_lon[c(TRUE, FALSE), ] %>%
      mutate(item = 'coordinateLongitude') %>%
      list(.,
           df_lat_lon[c(FALSE, TRUE), ] %>%
             mutate(item = 'coordinateLatitude')) %>%
      bind_rows() %>%
      select(idLocation, coordinates, item) %>%
      spread(item, coordinates)

    df_property <-
      df_property %>%
      inner_join(df_lat_lon) %>%
      select(idLocation, everything()) %>%
      mutate(amountValuationOwnedProperties = amountValuationOwnedProperties * 1000000 %>% formattable::currency(digits = 0),
             nameSector = nameSector %>% str_to_upper() %>% str_replace_all('\\-', ' ') %>% str_replace_all("DATA CENTER", "DATA CENTERS")) %>%
      filter(!nameSector == "ALL TYPES") %>%
      suppressWarnings() %>%
      suppressMessages()

    df_md <-
      parse_json_state_metadata()

    df_property <-
      df_md %>%
      left_join(
        df_property %>%
          nest(-codeState, .key = dataPropertiesOwned)
      ) %>%
      suppressWarnings() %>%
      suppressMessages()

    if (include_reit_hq) {
      states <-
        df_property$codeState %>%
        unique() %>%
        sort()

      url_states <-
        list("http://app.reitsacrossamerica.com/states/",states ,"/headquartered") %>%
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
        list("http://app.reitsacrossamerica.com/companies/holding/",states ) %>%
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

    return(df_property)

  }


# returns -----------------------------------------------------------------

#' Get monthly historic NAREIT returns by sector
#'
#' @param return_wide return data in wide form \code{TRUE, FALSE}
#' @param return_message return a message \code{T}
#'
#' @return
#' @export
#' @import dplyr purrr curl tidyr stringr formattable
#' @importFrom xlsx read.xlsx
#' @examples
#' get_data_nareit_historic_monthly_returns(return_wide = FALSE)
get_data_nareit_historic_monthly_returns <-
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
      mutate(nameIndex = nameIndex %>% str_replace_all('\\ ', '\\_') %>% str_to_upper(),
             item = list(item1, item2) %>% purrr::invoke(paste0,.),
             indexItem = paste(nameIndex, item, sep = ':')) %>%
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
      mutate(nameIndex = nameIndex %>% str_replace_all('\\_', '\\ '),
             metricItem = metricItem %>% str_replace_all('\\ ','')) %>%
      left_join(
        data_frame(metricItem = c("DividendYield", "IncomeReturn", "PriceIndex", "PriceReturn",
                                  "TotalIndex", "TotalReturn"),
                   nameItem = c("pctDividendYield", "pctIncomeReturn", "indexPrice", "pctPriceReturn",
                                "indexTotal", "pctTotalReturn")
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
        mutate_at(.cols = data %>% select(matches("^index")) %>% names(),
                  funs(. %>% formattable::comma(digits = 5))) %>%
        mutate_at(.cols = data %>% select(matches("^pct")) %>% names(),
                  funs((. / 100) %>% formattable::percent(digits = 4))) %>%
        mutate(urlData = url) %>%
        suppressWarnings()

      if (return_message) {
        list("Acquired FTSE NAREIT returns by sector from ",
             data$dateData %>% min(), ' to ', data$dateData %>% max()) %>%
          purrr::invoke(paste0, .) %>%
          message()
      }

      if (!return_wide) {
        data <-
          data %>%
          gather(item, value,-c(dateData, nameIndex, urlData), convert = T)
      }
      return(data)
  }

#' Get NAREIT subsector returns
#'
#' @param return_wide
#' @param return_message
#'
#' @return
#' @export
#' @import dplyr purrr curl tidyr stringr formattable
#' @importFrom xlsx read.xlsx
#' @examples
#' get_data_nareit_annual_subsector_returns(return_wide = TRUE, return_message = TRUE)
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
      mutate(nameSubSector = nameSubSector %>% str_replace_all('\\ ', '\\_') %>% str_to_upper(),
             namePS = paste(nameSector, nameSubSector, sep = '-'),
             indexItem = paste(namePS, nameIndex, sep = ':')) %>%
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
      mutate(nameSubSector = nameSubSector %>% str_replace_all('\\_', '\\ ') %>% str_to_upper(),
             nameSector = nameSector %>% str_to_upper(),
             metricItem = metricItem %>% str_replace_all('\\ ',''),
             metricItem = ifelse(metricItem == "Price", "pctPriceReturn", "pctTotalReturn")) %>%
      suppressWarnings()

    data <-
      data %>%
      spread(metricItem, valueItem) %>%
      suppressWarnings()

    data <-
      data %>%
      mutate_at(.cols = data %>% select(matches("^pct")) %>% names(),
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
        gather(item, value,-c(dateData, nameSector, nameSubSector, urlData), convert = T)
    }

    return(data)
  }


# offerings ---------------------------------------------------------------

# https://www.reit.com/data-research/data/reit-capital-offerings

get_nareit_offering_name_df <-
  function(){
    data_frame(nameNAREIT = c("Name", "Type", "Property.Sector", "Property.Subsector", "Chairman",
                              "City", "State", "Trade.Date", "Underwriter.1", "Underwriter.2",
                              "Underwriter.3", "Underwriter.4", "Underwriter.5", "Underwriter.6",
                              "Shares", "Price", "Total1", "Greenshoe", "Total2", "Grand.Total",
                              "PRRANGE1", "PRRANGE2", "Offering.Description", "Overallotment", "Date", "Total", "Offering.Type"),
               nameActual = c("nameCompany", "typeInvestment", "nameSector", "nameSubSector", "nameCompanyChairman",
                              "cityCompany", "stateCompany", "dateOffering", "nameUnderwriter", "nameUnderwriter1",
                              "nameUnderwriter2", "nameUnderwriter3", "nameUnderwriter4", "nameUnderwriter5",
                              "countShares", "priceOffering", "amountProceeds", "amountGreenShoe", "amountProceedsOther", "amountProceedsTotal",
                              "priceRangeLow", "priceRangeHigh", 'descriptionOffering', 'countSharesOverallotment', 'dateOffering', 'amountProceedsTotal',
                              "typeOffering")

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
      paste0('https://www.reit.com',.) %>% {
        .[2:length(.)]
      }

    url_df <-
      data_frame(typeCapital = c('IPO', 'Secondary', 'Debt'),
                 indexSheet = c(1, 2, 1),
               urlData = capital_urls)

    return(url_df)
  }

parse_nareit_offering_url <-
  function(url = "https://www.reit.com/sites/default/files/IndustryData/IPOs.xls",
           sheet = 2,
           return_message = TRUE) {
    tmp <-
      tempfile()
    curl::curl_download(url, tmp)

    data <-
      xlsx::read.xlsx(file = tmp, sheetIndex = sheet) %>%
      data.frame(stringsAsFactors = FALSE) %>%
      suppressWarnings() %>%
      as_data_frame()

    data <-
      data %>%
      mutate_if(is.factor,
                as.character)

    if ("NA." %in% names(data)) {
      data <-
        data[,!names(data)  == "NA."]
    }

    nareit_offering_name_df <-
      get_nareit_offering_name_df()

    actual_names <-
      names(data) %>%
      map_chr(function(x){
        nareit_offering_name_df %>%
          filter(nameNAREIT == x) %>%
          .$nameActual
      })

    data <-
      data %>%
      purrr::set_names(actual_names) %>%
      mutate_if(is_character,
                str_to_upper) %>%
      mutate(nameSubSector = ifelse(nameSubSector == '', NA, nameSubSector),
             dateOffering = dateOffering %>% lubridate::ymd())

    data <-
      data %>%
      mutate_at(
        data %>% select(matches("^countShares|^amountProceeds")) %>% names(),
                funs(. %>% as.numeric() * 1000000)) %>%
      mutate_at(
        data %>% select(matches("^price")) %>% names(),
        funs(. %>% readr::parse_number())) %>%
      mutate(urlData = url) %>%
      select(dateOffering, nameCompany, everything())

    data <-
      data %>%
      mutate_at(data %>% select(matches("nameUnderwriter|descriptionOffering")) %>% names(),
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
    return(data)
  }

#' Get NAREIT capital raise data by year and form of capital
#'
#' @param capital_type form of capital raised \code{NULL, IPO, SECONDARY, DEBT}
#' @param nest_data return a nested data frame \code{TRUE, FALSE}
#' @param return_message return a message \code{TRUE, FALSE}
#'
#' @return
#' @export
#' @import dplyr purrr curl tidyr stringr formattable
#' @importFrom xlsx read.xlsx
#' @examples
#' get_data_nareit_capital_raises(capital_type = NULL, nest_data = FALSE,return_message = TRUE)
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
      filter(!dateData %>% is.na()) %>%
      suppressMessages()
    if (return_message) {
      list("Acquired NAREIT capital raise data for ", all_data %>% nrow() %>% formattable::comma(digits = 0),
           ' REITs from ', all_data$dateOffering %>% min(na.rm = TRUE), ' to ', all_data$dateOffering %>% max(na.rm = TRUE)) %>%
        purrr::invoke(paste0, .) %>%
        message()
    }
    if (nest_data) {
      all_data <-
        all_data %>%
        nest(-typeCapital, .key = dataOfferingNAREIT)
    }

    return(all_data)
  }


# m&a ---------------------------------------------------------------------
#' Get NAREIT merger & acquisition data by year
#'
#' @param pages location of the pages in the REIT monthly file, only use this if the location of the M&A table changes
#' @param return_message return a message \code{TRUE, FALSE}
#' @param nest_data return a nested data frame \code{TRUE, FALSE}
#' @return
#' @export
#' @import purrr stringr dplyr rvest formattable lubridate tidyr readr
#' @examples
#' get_data_nareit_mergers_acquisitions(nest_data = FALSE, return_message = TRUE)
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
      paste0('https://www.reit.com',.)

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
      dplyr::rename(yearMerger = V1,
                    nameAcquiror = V2,
                    nameTarget = V3) %>%
      fill(yearMerger) %>%
      mutate(nameAcquiror = ifelse(nameAcquiror == '', NA, nameAcquiror)) %>%
      filter(!nameAcquiror %>% is.na()) %>%
      filter(!yearMerger == 'YEAR') %>%
      select(-c(V5, V7))

    all_data <-
      all_data %>%
      mutate(
        typeAcquiror = ifelse(V4 == '', V6, V4),
        amountAcquisitionPrice =  ifelse(V4 == '', V8, V6) %>% readr::parse_number() * 1000000 %>% formattable::currency(digits = 0),
        dateAnnounced = ifelse(V4 == '', V11, V8) %>% lubridate::dmy(),
        dateComplete =  ifelse(V4 == '', V12, V9) %>% lubridate::dmy(),
        statusTransaction = ifelse(V4 == '', V13, V10)
      ) %>%
      select(-matches("^V")) %>%
      suppressWarnings()

    all_data <-
      all_data %>%
      mutate(isPublicAcquiror = ifelse(typeAcquiror %>% str_detect("PUBLIC"), TRUE, FALSE),
             isJV = ifelse(typeAcquiror %>% str_detect("JV|JOINT VENTURE"), TRUE, FALSE),
             isComplete = ifelse(statusTransaction == "COMPLETED", TRUE, FALSE),
             idTransaction = 1:n()) %>%
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
      separate(nameAcquiror,
               into = c('nameAcquiror', 'nameAcquirorAKA'),
               sep = '\\(') %>%
      separate(nameTarget,
               into = c('nameTarget', 'nameTargetAKA'),
               sep = '\\(') %>%
      mutate(
        nameAcquirorAKA = nameAcquirorAKA %>% str_replace_all('[)]', ''),
        nameTargetAKA = nameTargetAKA %>% str_replace_all('[)]', ''),
        nameAcquirorAKA = nameAcquirorAKA %>% str_trim(),
        nameAcquiror = nameAcquiror %>% str_trim(),
        nameTargetAKA = nameTargetAKA %>% str_trim(),
        nameTarget = nameTarget %>% str_trim(),
        amountAcquisitionPrice = amountAcquisitionPrice %>% formattable::currency(digits = 0),
        urlData = url
      ) %>%
      suppressWarnings()

    if (return_message) {
      list("You acquired ", all_data %>% nrow() %>% formattable::comma(digits = 0), ' REIT M&A transactions accounting for ', all_data$amountAcquisitionPrice %>% sum(na.rm = T), ' in value from ', all_data$dateAnnounced %>% min(na.rm = T), ' to ', all_data$dateAnnounced %>% max(na.rm = T)) %>%
        purrr::invoke(paste0,.) %>%
        message()
    }

    if (nest_data) {
      all_data < -
        all_data %>%
        nest(-yearMerger, .key = 'dateTransactions')
    }

    return(all_data)
  }


# t-trackr ----------------------------------------------------------------

#' Get NAREIT Sector Tracking Metrics by Quarter
#' @param return_wide
#' @param return_messag
#' @return
#' @export
#' @import dplyr purrr curl tidyr stringr formattable rvest
#' @importFrom xlsx read.xlsx
#' @examples
#' get_data_nareit_industry_tracker(capital_type = NULL, nest_data = FALSE,return_message = TRUE)
get_data_nareit_industry_tracker <-
  function(return_wide = TRUE, return_message = TRUE) {
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
      paste0('https://www.reit.com',.) %>%
      .[[1]]

    tmp <-
      tempfile()
    curl::curl_download(url, tmp)

    data <-
      xlsx::read.xlsx(file = tmp, sheetIndex = 1,header = FALSE) %>%
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
      filter(!X1 %>% str_detect("Percent change|NOI per share|NOI/shr|All |Dividends per share|Div/shr|Source:|FFO/shr|FFO per share"))

    item_df <-
      data %>%
      filter(X2 %>% is.na()) %>%
      select(idRow, item = X1) %>%
      left_join(
        data_frame(item = c("FFO", "NOI", "Dividends paid", "Same Store NOI"),
                   nameItem = c('FFO', "NOI", 'Dividends', 'NOISameStore'))
      ) %>%
      mutate(idRow = idRow + 2) %>%
      select(idRow, nameItem) %>%
      suppressMessages()

    units_measure_df <-
      data %>%
      filter(X2 == "2000.1") %>%
      select(idRow, nameMeasure = X1) %>%
      left_join(
        data_frame(nameMeasure = c("millions of dollars", "percent change over 4 quarters"),
                   typeUOM = c('amount', 'pctChangeYear'))
      ) %>%
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
      unite(nameItem, typeUOM, nameItem, sep ='') %>%
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
      mutate(value = value %>% readr::parse_number(),
             nameSector = nameSector %>% str_to_upper()) %>%
      filter(!value %>% is.na()) %>%
      mutate(value = ifelse(nameItem %>% str_detect('amount'), value * 1000000, value / 100)) %>%
      suppressMessages() %>%
      suppressWarnings()

    data <-
      data %>%
      spread(nameItem, value) %>%
      tidyr::separate(yearQuarter, remove = FALSE, sep = '\\.', into = c('yearData', 'quarterData')) %>%
      mutate_at(c('yearData', 'quarterData'),
                funs(. %>% as.numeric())) %>%
      arrange(yearData, quarterData) %>%
      mutate(urlData = url)

    data <-
      data %>%
      mutate(pctDividendPayout = amountDividends / amountFFO,
             pctFFOMargin = amountFFO / amountNOI) %>%
      select(yearQuarer, yearData, quarterData, nameSector, amountDividends:pctChangeYearNOISameStore,
             pctDividendPayout, pctFFOMargin, everything())

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
        gather(item, value, -c(nameSector, yearQuarter, yearData, quarterData, urlData))
    }

    if (return_message) {
      list("Parsed: ", url) %>%
        purrr::invoke(paste0, .) %>%
        message()
    }

    return(data)
  }



# reit_watch_metrics ------------------------------------------------------

get_nareit_monthly_year_urls <-
  function() {
    page <-
      "https://www.reit.com/data-research/data/reitwatch" %>%
      read_html()

    url_df <-
      page %>%
      html_nodes('#block-system-main p a') %>%
      map_df(function(x){
        year <-
          x %>%
          html_text() %>%
          as.numeric()

        url <-
          x %>%
          html_attr('href') %>%
          paste0('https://www.reit.com',.)

        data_frame(yearData = year,
                   urlYear = url)
      })

    return(url_df)
  }

parse_nareit_monthly_year <-
  function(url = "https://www.reit.com/investing/investing-tools/nareit-statistical-publications/reitwatch/reitwatch-2001") {
    page <-
      url %>%
      read_html()

    url_df <-
      page %>%
      html_nodes('#block-system-main a') %>%
      map_df(function(x){
        month <-
          x %>%
          html_text() %>%
          str_to_upper()

        url <-
          x %>%
          html_attr('href') %>%
          paste0('https://www.reit.com',.)

        data_frame(monthData = month,
                   urlData = url)
      }) %>%
      left_join(
        data_frame(monthData = month.name %>% str_to_upper(),idMonth = 1:12)
      ) %>%
      mutate(urlYear = url) %>%
      suppressMessages()

    return(url_df)
  }

get_nariet_monthly_urls <-
  function(years = NULL, months = NULL) {
    df_year <-
      get_nareit_monthly_year_urls()

    if (!years %>% is_null()) {
      if(!years %in% df_year$yearData %>% sum() > 0){
        possible_years <-
          df_year$yearData
        stop(paste0("REITWATCH years can only be:\n", paste0(possible_years,collapse = '\n')))
      }

      df_year <-
        df_year %>%
        filter(yearData %in% years)
    }

    all_years <-
      df_year$urlYear %>%
      map_df(function(x){
        x %>% message()
        parse_nareit_monthly_year(url = x)
      }) %>%
      left_join(
        df_year
      ) %>%
      suppressWarnings() %>%
      suppressMessages()

    all_years <-
      all_years %>%
      arrange(yearData, (idMonth)) %>%
      mutate(idPeriod = 1:n())

    df_dates <-
      1:nrow(all_years) %>%
      map_df(function(x) {
        df_row <-
          all_years %>% slice(x)

        month_no <-
          df_row$idMonth

        if (month_no %>% nchar == 1) {
          month_no <-
            month_no %>%
            paste0("0", .)
        }

        if (month_no == 13) {
          month_no <-
            "01"
        }



        date_data <-
          list(month_no, "-01-", df_row$yearData) %>%
          purrr::invoke(paste0, .) %>%
          lubridate::mdy() - 1

        df_data <-
          data_frame(idPeriod = x, dateData = date_data)
        return(df_data)
      })

    all_years <-
      all_years %>%
      left_join(df_dates) %>%
      select(idPeriod, dateData, yearData, idMonth, urlData) %>%
      suppressWarnings() %>%
      suppressMessages()

    df_page_nos <-
      data_frame(idMonth = c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 1L, 2L,
                             3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 1L, 2L, 3L, 4L, 5L,
                             6L, 7L, 8L, 9L, 10L, 11L, 12L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L,
                             9L, 10L, 11L, 12L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L,
                             12L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 1L, 2L,
                             3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 1L, 2L, 3L, 4L, 5L,
                             6L, 7L, 8L, 9L, 10L, 11L, 12L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L,
                             9L, 10L, 11L, 12L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L,
                             12L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 1L, 2L,
                             3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 7L, 8L, 9L, 10L, 11L,
                             12L),
                 yearData = c(2011, 2011, 2011, 2011, 2011, 2011, 2011, 2011, 2011, 2011,
                              2011, 2011, 2010, 2010, 2010, 2010, 2010, 2010, 2010, 2010, 2010,
                              2010, 2010, 2010, 2009, 2009, 2009, 2009, 2009, 2009, 2009, 2009,
                              2009, 2009, 2009, 2009, 2008, 2008, 2008, 2008, 2008, 2008, 2008,
                              2008, 2008, 2008, 2008, 2008, 2007, 2007, 2007, 2007, 2007, 2007,
                              2007, 2007, 2007, 2007, 2007, 2007, 2006, 2006, 2006, 2006, 2006,
                              2006, 2006, 2006, 2006, 2006, 2006, 2006, 2005, 2005, 2005, 2005,
                              2005, 2005, 2005, 2005, 2005, 2005, 2005, 2005, 2004, 2004, 2004,
                              2004, 2004, 2004, 2004, 2004, 2004, 2004, 2004, 2004, 2003, 2003,
                              2003, 2003, 2003, 2003, 2003, 2003, 2003, 2003, 2003, 2003, 2002,
                              2002, 2002, 2002, 2002, 2002, 2002, 2002, 2002, 2002, 2002, 2002,
                              2001, 2001, 2001, 2001, 2001, 2001, 2001, 2001, 2001, 2001, 2001,
                              2001, 2000, 2000, 2000, 2000, 2000, 2000, 2000, 2000, 2000, 2000,
                              2000, 2000, 1999, 1999, 1999, 1999, 1999, 1999),
                 pageStart =  c(34, 34, 34, 34, 34, 33, 33, 33, 33, 33, 33, 33, 35, 35, 35,
                                35, 35, 35, 35, 35, 35, 35, 34, 34, 36, 36, 35, 35, 35, 35, 35,
                                35, 35, 35, 35, 35, 8, 9, 11, 11, 11, 11, 11, 11, 11, 11, 11,
                                11, 8, 8, 8, 8, 8, 8, 8, 9, 9, 8, 8, 8, 9, 9, 8, 8, 8, 8, 8,
                                8, 8, 8, 8, 8, 9, 9, 9, 9, 9, 9, 9, 9, 8, 9, 9, 9, 9, 9, 9, 9,
                                9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 10, 9, 9,
                                9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 16, 13, 12, 10, 12, 9, 9,
                                9, 9, 9, 9, 9, 12, 12, 12, 12, 12, 12, 14, 16, 15, 16, 16, 16,
                                12, 12, 12, 12, 12, 13),
                 pageEnd =c(40, 40, 40, 40, 40, 37, 37, 37, 37, 37, 37, 37, 41, 41, 41,
                            41, 41, 41, 41, 41, 41, 41, 40, 40, 42, 42, 41, 41, 41, 41, 41,
                            41, 41, 41, 41, 41, 21, 22, 17, 17, 17, 17, 17, 17, 17, 17, 17,
                            17, 21, 21, 21, 21, 21, 21, 21, 22, 22, 21, 21, 21, 22, 22, 21,
                            21, 21, 21, 21, 21, 21, 21, 21, 21, 22, 22, 22, 22, 22, 22, 22,
                            22, 21, 22, 22, 22, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 22,
                            22, 20, 20, 20, 20, 20, 20, 20, 20, 20, 21, 20, 20, 20, 20, 20,
                            20, 20, 20, 20, 20, 20, 20, 20, 20, 25, 24, 21, 21, 23, 20, 20,
                            20, 20, 20, 20, 20, 21, 21, 21, 21, 21, 21, 23, 25, 24, 25, 25,
                            25, 21, 21, 21, 21, 21, 22)

      )

    all_years <-
      all_years %>%
      left_join(df_page_nos) %>%
      mutate(pageStart = ifelse(pageStart %>% is.na(), 34, pageStart),
             pageEnd = ifelse(pageEnd %>% is.na(), 38, pageEnd)) %>%
      suppressWarnings() %>%
      suppressMessages()

    if (!months %>% is_null()) {
      if(!months %in% 1:12 %>% sum() > 0){

        stop(paste0("REITWATCH months can only be:\n", paste0(1:12,collapse = '\n')))
      }

      all_years <-
        all_years %>%
        filter(idMonth %in% months)
    }
    return(all_years)

  }

parse_most_recent_reit_watch <-
  function(pages = 32:33) {

    page <-
      'https://www.reit.com/data-research/data/reitwatch' %>%
      read_html()

    url <-
      page %>%
      html_nodes('#block-system-main a') %>%
      html_attr('href') %>%
      .[[1]] %>%
      paste0('https://www.reit.com',.)

    tables <-
      url %>%
      extract_tables(pages = pages)

    return(tables)
  }

parse_most_recent_reit_watch <-
  function(url = 'https://www.reit.com/data-research/data/reitwatch',
           use_guess = FALSE,
           pages = 32:33) {
    tables <-
      url %>%
      extract_tables(pages = pages, guess = use_guess)

    return(tables)
  }

import_rw_pdf <-
  function(url = "https://www.reit.com/sites/default/files/reitwatch/RW1207.pdf",
           use_guess = FALSE,
           pages = 34:38) {
      tables <-
        parse_most_recent_reit_watch(url = url,
                                     pages = pages,
                                     use_guess = use_guess)

      data <-
        1:length(tables) %>%
        map_df(function(x) {
          tables[[x]] %>%
            as_data_frame()
        })

    return(data)
  }

parse_over_15 <-
  function(data) {
    data <-
      data %>%
      mutate_all(funs(. %>% str_trim() %>% str_to_upper())) %>%
      mutate(idRow = 1:n())

    subsector_df <-
      data %>%
      filter((V2 == '') & (V3 == '') & (V4 == '')) %>%
      select(idRow, nameSubSector = V1) %>%
      mutate(idRow = idRow + 1)

    all_data <-
      data %>%
      filter(!V1 %>% str_detect("AVERAGE")) %>%
      filter(!V1 %in% c('', "NAME", 'REIT NAME')) %>%
      filter(!((V2 == '') & (V3 == '') & (V4 == '')))

    all_data <-
      all_data %>%
      left_join(subsector_df) %>%
      fill(nameSubSector) %>%
      select(-idRow) %>%
      select(nameSubSector, everything()) %>%
      suppressWarnings() %>%
      suppressMessages()

    all_data <-
      all_data %>%
      mutate(countV3 = V3 %>% str_count('\\ '))

    counts <-
      all_data$countV3 %>%
      unique()

    all_data <-
      counts %>%
      map_df(function(x){
          is_zero <-
            x == 0

          df <-
            all_data %>%
            filter(countV3 == x) %>%
            select(-countV3) %>%
            mutate_all(funs(ifelse(. == '', NA, .))) %>%
            select(which(colMeans(is.na(.)) < 1))

          if (is_zero) {
            is_23 <-
              df %>%
              ncol() == 23

            is_22 <-
              df %>% ncol() == 22

            is_24 <-
              df %>% ncol == 24

            is_25 <-
              df %>% ncol() == 25

            is_26 <-
              df %>% ncol() == 26

            is_27 <-
              df %>% ncol() == 27

            if (is_26) {
              df_a <-
                df %>%
                filter(!V25 %>% is.na()) %>%
                mutate_all(funs(ifelse(. == '', NA, .))) %>%
                select(which(colMeans(is.na(.)) < 1))

              df_a <-
                df_a %>%
                purrr::set_names(c('nameSector', 'nameCompany', 'idTicker','priceEndOfMonth', 'price52WeekHighLow',
                                   'pershareFFOCurrentYearNextYear',
                                   'ratioFFOPriceCurrentYearNext', 'pctFFOGrowth', 'pctFFOPayOut', 'ratioDebtEBITDA', 'pctReturnMonth', 'pctReturnQuarter', 'pctReturnYearToDate1YR',
                                   'pctReturn3YR', 'pctReturn5YR',
                                   'pctDividendYield', 'amountEquityMarketCap', 'amountEquityMarketCapDilluted', 'pctDebtEquity', 'countAVGShareVolume', 'countAVGDollarVolume','ratioRelativeLiquidity',
                                   'idCreditRating')
                ) %>%
                separate(price52WeekHighLow, c('price52WeekHigh', 'price52WeekLow'), sep = '\\ ') %>%
                separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
                separate(ratioFFOPriceCurrentYearNext, c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
                separate(pctReturnYearToDate1YR, c('pctReturnYearToDate', 'pctReturn1YR'), sep = '\\ ') %>%
                suppressWarnings()


              df_b_1 <-
                df %>%
                filter(V25 %>% is.na()) %>%
                filter(V17 %>% is.na()) %>%
                mutate_all(funs(ifelse(. == '', NA, .))) %>%
                select(which(colMeans(is.na(.)) < 1)) %>%
                purrr::set_names(c('nameSector', 'nameCompany', 'idTicker','priceEndOfMonth', 'price52WeekHighLow',
                                   'pershareFFOCurrentYearNextYear',
                                   'ratioFFOPriceCurrentYearNext', 'pctFFOGrowth', 'pctFFOPayOut', 'ratioDebtEBITDA', 'pctReturnMonth', 'pctReturnQuarter', 'pctReturnYearToDate1YR',
                                   'pctReturn3YR', 'pctReturn5YR',
                                   'pctDividendYield', 'amountEquityMarketCap', 'amountEquityMarketCapDilluted', 'pctDebtEquity', 'countAVGShareVolume', 'countAVGDollarVolume','ratioRelativeLiquidity')
                ) %>%
                separate(price52WeekHighLow, c('price52WeekHigh', 'price52WeekLow'), sep = '\\ ') %>%
                separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
                separate(ratioFFOPriceCurrentYearNext, c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
                separate(pctReturnYearToDate1YR, c('pctReturnYearToDate', 'pctReturn1YR'), sep = '\\ ') %>%
                suppressWarnings()

              df_b_2 <-
                df %>%
                filter(V25 %>% is.na()) %>%
                filter(!V17 %>% is.na()) %>%
                mutate_all(funs(ifelse(. == '', NA, .))) %>%
                select(which(colMeans(is.na(.)) < 1)) %>%
                purrr::set_names(c('nameSector', 'nameCompany', 'idTicker','priceEndOfMonth', 'price52WeekHighLow',
                                   'pershareFFOCurrentYearNextYear',
                                   'ratioFFOPriceCurrentYearNext', 'pctFFOGrowth', 'pctFFOPayOut', 'ratioDebtEBITDA', 'pctReturnMonth', 'pctReturnQuarter', 'pctReturnYearToDate1YR',
                                   'pctReturn3YR', 'pctReturn5YR',
                                   'pctDividendYield', 'amountEquityMarketCap', 'amountEquityMarketCapDilluted', 'pctDebtEquity', 'countAVGShareVolume', 'countAVGDollarVolume','ratioRelativeLiquidity',
                                   'idCreditRating')
                ) %>%
                separate(price52WeekHighLow, c('price52WeekHigh', 'price52WeekLow'), sep = '\\ ') %>%
                separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
                separate(ratioFFOPriceCurrentYearNext, c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
                separate(pctReturnYearToDate1YR, c('pctReturnYearToDate', 'pctReturn1YR'), sep = '\\ ') %>%
                suppressWarnings()

              df <-
                df_a %>%
                bind_rows(list(df_b_1, df_b_2))

              return(df)

            }

            if (is_27) {
              df_a <-
                df %>%
                filter(!V26 %>% is.na()) %>%
                mutate_all(funs(ifelse(. == '', NA, .))) %>%
                select(which(colMeans(is.na(.)) < 1))

              df_a_27 <-
                df_a %>% ncol() == 27

              df_a_25 <-
                df_a %>% ncol() == 25

              if(df_a_27) {
                df_a <-
                df_a %>%
                purrr::set_names(c('nameSubSector', 'nameCompany', 'idTicker','priceEndOfMonth',
                                   'price52WeekHigh', 'price52WeekLow',
                                   'pershareFFOCurrentYear', 'pershareFFONextYear',
                                   'ratioFFOPriceCurrent', 'ratioFFOPriceYearNext', 'pctFFOGrowth', 'pctFFOPayOut', 'ratioDebtEBITDA', 'pctReturnMonth', 'pctReturnQuarter',
                                   'pctReturnYearToDate', 'pctReturn1YR',
                                   'pctReturn3YR', 'pctReturn5YR',
                                   'pctDividendYield', 'amountEquityMarketCap', 'amountEquityMarketCapDilluted', 'pctDebtEquity', 'countAVGShareVolume', 'countAVGDollarVolume','ratioRelativeLiquidity',
                                   'idCreditRating'))
              }

              if (df_a_25) {
                df_a <-
                  df_a %>%
                  purrr::set_names(c('nameCompany', 'idTicker','priceEndOfMonth',
                                     'price52WeekHigh', 'price52WeekLow',
                                     'pershareFFOCurrentYear', 'pershareFFONextYear',
                                     'ratioFFOPriceCurrent', 'ratioFFOPriceYearNext', 'pctFFOGrowth', 'pctFFOPayOutDebt', 'pctReturnMonth', 'pctReturnQuarter',
                                     'pctReturnYearToDate', 'pctReturn1YR',
                                     'pctReturn3YR', 'pctReturn5YR',
                                     'pctDividendYield', 'amountEquityMarketCap', 'amountEquityMarketCapDilluted', 'pctDebtEquity', 'countAVGShareVolume', 'countAVGDollarVolume',
                                     'ratioRelativeLiquidity', 'idCreditRating'))

                df_a <-
                  df_a %>%
                  separate(pctFFOPayOutDebt, c('pctFFOPayOut', 'ratioDebtEBITDA'), sep = '\\ ')
                }

              df_b <-
                df %>%
                filter(V26 %>% is.na()) %>%
                mutate_all(funs(ifelse(. == '', NA, .))) %>%
                select(which(colMeans(is.na(.)) < 1))

             df_b1 <-
               df_b %>%
               filter(!V25 %>% is.na()) %>%
               select(which(colMeans(is.na(.)) < 1))

             df_b1_is_26 <-
               df_b1 %>% ncol() == 26

             df_b1_is_24 <-
               df_b1 %>% ncol() == 24

             if (df_b1_is_24) {
               df_b1 <-
                 df_b1 %>%
                 purrr::set_names(c('nameCompany', 'idTicker','priceEndOfMonth',
                                    'price52WeekHigh', 'price52WeekLow',
                                    'pershareFFOCurrentYear', 'pershareFFONextYear',
                                    'ratioFFOPriceCurrent', 'ratioFFOPriceYearNext', 'pctFFOGrowth', 'pctFFOPayOutDebt', 'pctReturnMonth', 'pctReturnQuarter',
                                    'pctReturnYearToDate', 'pctReturn1YR',
                                    'pctReturn3YR', 'pctReturn5YR',
                                    'pctDividendYield', 'amountEquityMarketCap', 'amountEquityMarketCapDilluted', 'pctDebtEquity', 'countAVGShareVolume', 'countAVGDollarVolume',
                                    'ratioRelativeLiquidity'
                                    )) %>%
                 separate(pctFFOPayOutDebt, c('pctFFOPayOut', 'ratioDebtEBITDA'), sep = '\\ ')

             }

             if (df_b1_is_26) {
               df_b1 <-
               df_b1 %>%
                purrr::set_names(c('nameSubSector', 'nameCompany', 'idTicker','priceEndOfMonth',
                                   'price52WeekHigh', 'price52WeekLow',
                                   'pershareFFOCurrentYear', 'pershareFFONextYear',
                                   'ratioFFOPriceCurrent', 'ratioFFOPriceYearNext', 'pctFFOGrowth', 'pctFFOPayOut', 'ratioDebtEBITDA', 'pctReturnMonth', 'pctReturnQuarter',
                                   'pctReturnYearToDate', 'pctReturn1YR',
                                   'pctReturn3YR', 'pctReturn5YR',
                                   'pctDividendYield', 'amountEquityMarketCap', 'amountEquityMarketCapDilluted', 'pctDebtEquity', 'countAVGShareVolume', 'countAVGDollarVolume',
                                   'ratioRelativeLiquidity'))

             }

              df_b2 <-
                df_b %>%
                filter(V25 %>% is.na()) %>%
                select(which(colMeans(is.na(.)) < 1))

              df_b2 <-
                df_b2 %>%
                purrr::set_names(c('nameSubSector', 'nameCompany', 'idTicker','priceEndOfMonth', 'price52WeekHighLow',
                                   'pershareFFOCurrentYearNextYear',
                                   'ratioFFOPriceCurrentYearNext', 'pctFFOGrowth', 'pctFFOPayOut', 'ratioDebtEBITDA', 'pctReturnMonth', 'pctReturnQuarter', 'pctReturnYearToDate1YR',
                                   'pctReturn3YR', 'pctReturn5YR',
                                   'pctDividendYield', 'amountEquityMarketCap', 'amountEquityMarketCapDilluted', 'pctDebtEquity', 'countAVGShareVolume', 'countAVGDollarVolume','ratioRelativeLiquidity',
                                   'idCreditRating')
                )

              df_b2 <-
                df_b2 %>%
                separate(price52WeekHighLow, c('price52WeekHigh', 'price52WeekLow'), sep = '\\ ') %>%
                separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
                separate(ratioFFOPriceCurrentYearNext, c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
                separate(pctReturnYearToDate1YR, c('pctReturnYearToDate', 'pctReturn1YR'), sep = '\\ ') %>%
                suppressWarnings()

              df_b <-
                df_b1 %>%
                bind_rows(df_b2)

              df <-
                df_a %>%
                bind_rows(df_b)

              return(df)


            }

            if (is_25) {
              df_a <-
                df %>%
                filter(!V24 %>% is.na()) %>%
                mutate_all(funs(ifelse(. == '', NA, .))) %>%
                select(which(colMeans(is.na(.)) < 1))

              df_a <-
                df_a %>%
                purrr::set_names(c('nameSector', 'nameCompany', 'idTicker','priceEndOfMonth', 'price52WeekHighLow',
                                   'pershareFFOCurrentYearNextYear',
                                   'ratioFFOPriceCurrentYearNext', 'pctFFOGrowth', 'pctFFOPayOut', 'ratioDebtEBITDA', 'pctReturnMonth', 'pctReturnQuarter', 'pctReturnYearToDate1YR',
                                   'pctReturn3YR', 'pctReturn5YR',
                                   'pctDividendYield', 'amountEquityMarketCap', 'amountEquityMarketCapDilluted', 'pctDebtEquity', 'countAVGShareVolume', 'countAVGDollarVolume','ratioRelativeLiquidity',
                                   'idCreditRating')
                )

              df_a <-
                df_a %>%
                separate(price52WeekHighLow, c('price52WeekHigh', 'price52WeekLow'), sep = '\\ ') %>%
                separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
                separate(ratioFFOPriceCurrentYearNext, c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
                separate(pctReturnYearToDate1YR, c('pctReturnYearToDate', 'pctReturn1YR'), sep = '\\ ') %>%
                suppressWarnings()

              df_b <-
                df %>%
                filter(V24 %>% is.na()) %>%
                mutate_all(funs(ifelse(. == '', NA, .))) %>%
                select(which(colMeans(is.na(.)) < 1))

              df_b1 <-
                df_b %>%
                filter(!V23 %>% is.na()) %>%
                mutate_all(funs(ifelse(. == '', NA, .))) %>%
                select(which(colMeans(is.na(.)) < 1)) %>%
                purrr::set_names(c('nameSector', 'nameCompany', 'idTicker', 'priceEndOfMonth','price52WeekHighLow',
                                   'pershareFFOCurrentYearNextYear', 'ratioFFOPriceCurrentYearNext',
                                   'pctFFOGrowth', 'pctFFOPayOut', 'ratioDebtEBITDA', 'pctReturnMonth', 'pctReturnQuarter', 'pctReturnYearToDate1YR',
                                   'pctReturn3YR', 'pctReturn5YR',
                                   'pctDividendYield', 'amountEquityMarketCap', 'amountEquityMarketCapDilluted', 'pctDebtEquity', 'countAVGShareVolume', 'countAVGDollarVolume','ratioRelativeLiquidity'
                )
                )

              df_b1 <-
                df_b1 %>%
                separate(price52WeekHighLow, c('price52WeekHigh', 'price52WeekLow'), sep = '\\ ') %>%
                separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
                separate(ratioFFOPriceCurrentYearNext, c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
                separate(pctReturnYearToDate1YR, c('pctReturnYearToDate', 'pctReturn1YR'), sep = '\\ ') %>%
                suppressWarnings()

              df_b2 <-
                df_b %>%
                filter(V23 %>% is.na()) %>%
                mutate_all(funs(ifelse(. == '', NA, .))) %>%
                select(which(colMeans(is.na(.)) < 1)) %>%
                purrr::set_names(c('nameSector', 'nameCompany', 'idTicker','priceEndOfMonth', 'price52WeekHighLow',
                                   'pershareFFOCurrentYearNextYear',
                                   'ratioFFOPriceCurrentYearNext', 'pctFFOGrowth', 'pctFFOPayOut', 'ratioDebtEBITDA', 'pctReturnMonth', 'pctReturnQuarter', 'pctReturnYearToDate1YR',
                                   'pctReturn3YR', 'pctReturn3YR',
                                   'pctDividendYield', 'amountEquityMarketCap', 'amountEquityMarketCapDilluted', 'pctDebtEquity', 'countAVGShareVolume', 'countAVGDollarVolume','ratioRelativeLiquidity',
                                   'idCreditRating')
                )

              df_b2 <-
                df_b2 %>%
                separate(price52WeekHighLow, c('price52WeekHigh', 'price52WeekLow'), sep = '\\ ') %>%
                separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
                separate(ratioFFOPriceCurrentYearNext, c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
                separate(pctReturnYearToDate1YR, c('pctReturnYearToDate', 'pctReturn1YR'), sep = '\\ ') %>%
                suppressWarnings()

              df_b <-
                df_b1 %>%
                bind_rows(df_b2)

              df <-
                df_a %>%
                bind_rows(df_b)

              return(df)


            }

            if (is_24) {
              df_a <-
                df %>%
                filter(!V23 %>% is.na()) %>%
                mutate_all(funs(ifelse(. == '', NA, .))) %>%
                select(which(colMeans(is.na(.)) < 1))

              df_a <-
                df_a %>%
                purrr::set_names(c('nameSector', 'nameCompany', 'idTicker','priceEndOfMonth', 'price52WeekHighLow',
                                   'pershareFFOCurrentYearNextYear',
                                   'ratioFFOPriceCurrentYearNext', 'pctFFOGrowth', 'pctFFOPayOut', 'ratioDebtEBITDA', 'pctReturnMonth', 'pctReturnQuarter', 'pctReturnYearToDate1YR',
                                   'pctReturn3YR', 'pctReturn5YR',
                                   'pctDividendYield', 'amountEquityMarketCap', 'amountEquityMarketCapDilluted', 'pctDebtEquity', 'countAVGShareVolume', 'countAVGDollarVolume','ratioRelativeLiquidity',
                                   'idCreditRating')
                )

              df_a <-
                df_a %>%
                separate(price52WeekHighLow, c('price52WeekHigh', 'price52WeekLow'), sep = '\\ ') %>%
                separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
                separate(ratioFFOPriceCurrentYearNext, c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
                separate(pctReturnYearToDate1YR, c('pctReturnYearToDate', 'pctReturn1YR'), sep = '\\ ') %>%
                suppressWarnings()

              df_b <-
                df %>%
                filter(V23 %>% is.na()) %>%
                mutate_all(funs(ifelse(. == '', NA, .))) %>%
                select(which(colMeans(is.na(.)) < 1))

              df_b1 <-
                df_b %>%
                filter(!V15 %>% is.na()) %>%
                mutate_all(funs(ifelse(. == '', NA, .))) %>%
                select(which(colMeans(is.na(.)) < 1)) %>%
                purrr::set_names(c('nameSector', 'nameCompany', 'idTicker', 'priceEndOfMonth','price52WeekHighLow',
                                   'pershareFFOCurrentYearNextYear', 'ratioFFOPriceCurrentYearNext',
                                   'pctFFOGrowth', 'pctFFOPayOut', 'ratioDebtEBITDA', 'pctReturnMonth', 'pctReturnQuarter', 'pctReturnYearToDate1YR',
                                   'pctReturn3YR', 'pctReturn5YR',
                                   'pctDividendYield', 'amountEquityMarketCap', 'amountEquityMarketCapDilluted', 'pctDebtEquity', 'countAVGShareVolume', 'countAVGDollarVolume','ratioRelativeLiquidity',
                                   'idCreditRating'
                )
                )

              df_b1 <-
                df_b1 %>%
                separate(price52WeekHighLow, c('price52WeekHigh', 'price52WeekLow'), sep = '\\ ') %>%
                separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
                separate(ratioFFOPriceCurrentYearNext, c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
                separate(pctReturnYearToDate1YR, c('pctReturnYearToDate', 'pctReturn1YR'), sep = '\\ ') %>%
                suppressWarnings()

              df_b2 <-
                df_b %>%
                filter(V15 %>% is.na()) %>%
                mutate_all(funs(ifelse(. == '', NA, .))) %>%
                select(which(colMeans(is.na(.)) < 1)) %>%
                purrr::set_names(c('nameSector', 'nameCompany', 'idTicker','priceEndOfMonth', 'price52WeekHighLow',
                                   'pershareFFOCurrentYearNextYear',
                                   'ratioFFOPriceCurrentYearNext', 'pctFFOGrowth', 'pctFFOPayOut', 'ratioDebtEBITDA', 'pctReturnMonth', 'pctReturnQuarter', 'pctReturnYearToDate1YR',
                                   'pctReturn3YR', 'pctReturn3YR',
                                   'pctDividendYield', 'amountEquityMarketCap', 'amountEquityMarketCapDilluted', 'pctDebtEquity', 'countAVGShareVolume', 'countAVGDollarVolume','ratioRelativeLiquidity')
                )

              df_b2 <-
                df_b2 %>%
                separate(price52WeekHighLow, c('price52WeekHigh', 'price52WeekLow'), sep = '\\ ') %>%
                separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
                separate(ratioFFOPriceCurrentYearNext, c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
                separate(pctReturnYearToDate1YR, c('pctReturnYearToDate', 'pctReturn1YR'), sep = '\\ ') %>%
                suppressWarnings()

              df_b <-
                df_b1 %>%
                bind_rows(df_b2)

              df <-
                df_a %>%
                bind_rows(df_b)

              return(df)


            }

            if (is_23) {
              df <-
                df %>%
                purrr::set_names(c('nameSector', 'nameCompany', 'idTicker', 'priceEndOfMonth', 'price52WeekHighLow', 'pershareFFOCurrentYearNextYear',
                                   'ratioFFOPriceCurrentYearNext', 'pctFFOGrowth', 'pctFFOPayOut', 'ratioDebtEBITDA', 'pctReturnMonth', 'pctReturnQuarter', 'pctReturnYearToDate1YR',
                                   'pctReturn3YR', 'pctReturn5YR',
                                   'pctDividendYield', 'amountEquityMarketCap', 'amountEquityMarketCapDilluted', 'pctDebtEquity', 'countAVGShareVolume', 'countAVGDollarVolume','ratioRelativeLiquidity', 'idCreditRating' ))

              df <-
                df %>%
                separate(price52WeekHighLow, c('price52WeekHigh', 'price52WeekLow'), sep = '\\ ') %>%
                separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
                separate(ratioFFOPriceCurrentYearNext, c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
                separate(pctReturnYearToDate1YR, c('pctReturnYearToDate', 'pctReturn1YR'), sep = '\\ ')

              return(df)

            }

            if (is_22) {
              df <-
                df %>%
                purrr::set_names(c('nameSector', 'nameCompany', 'idTicker', 'priceEndOfMonth52WeekHighLow', 'pershareFFOCurrentYearNextYear',
                                   'ratioFFOPriceCurrentYearNext', 'pctFFOGrowth', 'pctFFOPayOut', 'ratioDebtEBITDA', 'pctReturnMonth', 'pctReturnQuarter', 'pctReturnYearToDate1YR',
                                   'pctReturn3YR', 'pctReturn5YR',
                                   'pctDividendYield', 'amountEquityMarketCap', 'amountEquityMarketCapDilluted', 'pctDebtEquity', 'countAVGShareVolume', 'countAVGDollarVolume','ratioRelativeLiquidity', 'idCreditRating' ))

              df <-
                df %>%
                separate(priceEndOfMonth52WeekHighLow, c('priceEndOfMonth', 'price52WeekHigh', 'price52WeekLow'), sep = '\\ ') %>%
                separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
                separate(ratioFFOPriceCurrentYearNext, c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
                separate(pctReturnYearToDate1YR, c('pctReturnYearToDate', 'pctReturn1YR'), sep = '\\ ')
              return(df)
            }


          }

          is_2 <-
            x == 2

          if (is_2) {
            df <-
              all_data %>%
              filter(countV3 == x) %>%
              select(-countV3) %>%
              mutate_all(funs(ifelse(. == '', NA, .))) %>%
              select(which(colMeans(is.na(.)) < 1))

            is_26 <-
              df %>% ncol() == 26 | (df %>% ncol() == 27)

            if (is_26) {
              df_a <-
                df %>%
                filter(!V24 %>% is.na()) %>%
                mutate_all(funs(ifelse(. == '', NA, .))) %>%
                select(which(colMeans(is.na(.)) < 1))

              df_a <-
                df_a %>%
                purrr::set_names(c('nameSector', 'nameCompany', 'idTicker','priceEndOfMonthHighLow',
                                   'pershareFFOCurrentYearNextYear',
                                   'ratioFFOPriceCurrentYearNext', 'pctFFOGrowth', 'pctFFOPayOut', 'ratioDebtEBITDA', 'pctReturnMonth', 'pctReturnQuarter', 'pctReturnYearToDate1YR',
                                   'pctReturn3YR', 'pctReturn5YR',
                                   'pctDividendYield', 'amountEquityMarketCap', 'amountEquityMarketCapDilluted', 'pctDebtEquity', 'countAVGShareVolume', 'countAVGDollarVolume','ratioRelativeLiquidity',
                                   'idCreditRating')
                )

              df_a <-
                df_a %>%
                separate(priceEndOfMonthHighLow, c('priceEndOfMonth','price52WeekHigh', 'price52WeekLow'), sep = '\\ ') %>%
                separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
                separate(ratioFFOPriceCurrentYearNext, c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
                separate(pctReturnYearToDate1YR, c('pctReturnYearToDate', 'pctReturn1YR'), sep = '\\ ') %>%
                suppressWarnings()

              df_b1 <-
                df %>%
                filter(V24 %>% is.na()) %>%
                filter(!V4 %>% is.na()) %>%
                mutate_all(funs(ifelse(. == '', NA, .))) %>%
                select(which(colMeans(is.na(.)) < 1))

              df_b1 <-
                df_b1 %>%
                purrr::set_names(c('nameSector', 'nameCompany', 'idTicker', 'priceEndOfMonthHighLow',
                                   'pershareFFOCurrentYearNextYear', 'ratioFFOPriceCurrentYearNext',
                                   'pctFFOGrowth', 'pctFFOPayOut', 'ratioDebtEBITDA', 'pctReturnMonth', 'pctReturnQuarter', 'pctReturnYearToDate1YR',
                                   'pctReturn3YR', 'pctReturn5YR',
                                   'pctDividendYield', 'amountEquityMarketCap', 'amountEquityMarketCapDilluted', 'pctDebtEquity', 'countAVGShareVolume', 'countAVGDollarVolume','ratioRelativeLiquidity', 'idCreditRating')
                )

              df_b1 <-
                df_b1 %>%
                separate(priceEndOfMonthHighLow, c('priceEndOfMonth','price52WeekHigh', 'price52WeekLow'), sep = '\\ ') %>%
                separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
                separate(ratioFFOPriceCurrentYearNext, c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
                separate(pctReturnYearToDate1YR, c('pctReturnYearToDate', 'pctReturn1YR'), sep = '\\ ') %>%
                suppressWarnings()

              has_extra <-
                df %>%
                filter(V24 %>% is.na()) %>%
                filter(V4 %>% is.na()) %>%
                nrow() > 0
              if (has_extra) {
                df_b2 <-
                df %>%
                filter(V24 %>% is.na()) %>%
                filter(V4 %>% is.na()) %>%
                mutate_all(funs(ifelse(. == '', NA, .))) %>%
                select(which(colMeans(is.na(.)) < 1))

              df_b2 <-
                df_b2 %>%
                purrr::set_names(c('nameSector', 'nameCompany', 'idTicker', 'priceEndOfMonthHighLow',
                                   'pershareFFOCurrentYearNextYear', 'ratioFFOPriceCurrentYearNext',
                                   'pctFFOGrowth', 'pctFFOPayOut', 'ratioDebtEBITDA', 'pctReturnMonth', 'pctReturnQuarter', 'pctReturnYearToDate1YR',
                                   'pctReturn3YR', 'pctReturn5YR',
                                   'pctDividendYield', 'amountEquityMarketCap', 'amountEquityMarketCapDilluted', 'pctDebtEquity', 'countAVGShareVolume', 'countAVGDollarVolume','ratioRelativeLiquidity', 'idCreditRating')
                )

              df_b2 <-
                df_b2 %>%
                separate(priceEndOfMonthHighLow, c('priceEndOfMonth','price52WeekHigh', 'price52WeekLow'), sep = '\\ ') %>%
                separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
                separate(ratioFFOPriceCurrentYearNext, c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
                separate(pctReturnYearToDate1YR, c('pctReturnYearToDate', 'pctReturn1YR'), sep = '\\ ') %>%
                suppressWarnings()

              df_b <-
                df_b1 %>%
                bind_rows(df_b2)
              } else {
                df_b <-
                  df_b1
              }

              df <-
                df_a %>%
                bind_rows(df_b)

              return(df)

            }

            if ('V22' %in% names(df)) {

              has_v22 <-
                df %>%
                filter(!V22 %>% is.na()) %>% nrow() > 0
              if (has_v22) {
                df <-
                  df %>%
                  filter(!V22 %>% is.na()) %>%
                  select(which(colMeans(is.na(.)) < 1)) %>%
                  purrr::set_names(c('nameSector', 'nameCompany', 'idTicker', 'priceEndOfMonth52WeekHighLow', 'pershareFFOCurrentYearNextYear',
                                     'ratioFFOPriceCurrentYearNext', 'pctFFOGrowth', 'pctFFOPayOut', 'ratioDebtEBITDA', 'pctReturnMonth', 'pctReturnQuarter', 'pctReturnYearToDate1YR',
                                     'pctReturn3YR', 'pctReturn5YR',
                                     'pctDividendYield', 'amountEquityMarketCap', 'amountEquityMarketCapDilluted', 'pctDebtEquity', 'countAVGShareVolume', 'countAVGDollarVolume','ratioRelativeLiquidity', 'idCreditRating' )) %>%
                  bind_rows(
                    df %>%
                      filter(V22 %>% is.na()) %>%
                      select(which(colMeans(is.na(.)) < 1)) %>%
                      purrr::set_names(c('nameSector', 'nameCompany', 'idTicker', 'priceEndOfMonth52WeekHighLow', 'pershareFFOCurrentYearNextYear',
                                         'ratioFFOPriceCurrentYearNext', 'pctFFOGrowth', 'pctFFOPayOut', 'ratioDebtEBITDA', 'pctReturnMonth', 'pctReturnQuarter', 'pctReturnYearToDate1YR',
                                         'pctReturn3YR', 'pctReturn5YR',
                                         'pctDividendYield', 'amountEquityMarketCap', 'amountEquityMarketCapDilluted', 'pctDebtEquity', 'countAVGShareVolume', 'countAVGDollarVolume','ratioRelativeLiquidity', 'idCreditRating' ))

                  )

                df <-
                  df %>%
                  separate(priceEndOfMonth52WeekHighLow, c('priceEndOfMonth', 'price52WeekHigh', 'price52WeekLow'), sep = '\\ ') %>%
                  separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
                  separate(ratioFFOPriceCurrentYearNext, c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
                  separate(pctReturnYearToDate1YR, c('pctReturnYearToDate', 'pctReturn1YR'), sep = '\\ ')

                return(df)
              }
            }

            df <-
              df %>%
              purrr::set_names(c('nameSector', 'nameCompany', 'idTicker', 'priceEndOfMonth52WeekHighLow', 'pershareFFOCurrentYearNextYear',
                                 'ratioFFOPriceCurrentYearNext', 'pctFFOGrowth', 'pctFFOPayOut', 'ratioDebtEBITDA', 'pctReturnMonth', 'pctReturnQuarter', 'pctReturnYearToDate1YR',
                                 'pctReturn3YR', 'pctReturn5YR',
                                 'pctDividendYield', 'amountEquityMarketCap', 'amountEquityMarketCapDilluted', 'pctDebtEquity', 'countAVGShareVolume', 'countAVGDollarVolume','ratioRelativeLiquidity', 'idCreditRating' ))

            df <-
              df %>%
              separate(priceEndOfMonth52WeekHighLow, c('priceEndOfMonth', 'price52WeekHigh', 'price52WeekLow'), sep = '\\ ') %>%
              separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
              separate(ratioFFOPriceCurrentYearNext, c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
              separate(pctReturnYearToDate1YR, c('pctReturnYearToDate', 'pctReturn1YR'), sep = '\\ ')
          }
          return(df)
      })

    all_data <-
      all_data %>%
      mutate_at(all_data %>% select(matches("^price|^pershare|^count|^pct|^amount|^ratio")) %>% names(),
                funs(. %>% readr::parse_number())) %>%
      mutate_at(all_data %>% select(matches("^amount")) %>% names(),
                funs(. * 1000000)) %>%
      mutate_at(all_data %>% select(matches("^count")) %>% names(),
                funs(. * 1000)) %>%
      mutate_at(all_data %>% select(matches("^pct")) %>% names(),
                funs((. / 100))) %>%
      mutate_at(all_data %>% select(matches("^count")) %>% names(),
                funs(. %>% comma(digits = 0))) %>%
      mutate_at(all_data %>% select(matches("^price|^pershare")) %>% names(),
                funs(. %>%  currency(digits = 2))) %>%
      mutate_at(all_data %>% select(matches("^amount")) %>% names(),
                funs(.  %>%  currency(digits = 0))) %>%
      mutate(pctDebtYield = (1 / ratioDebtEBITDA) %>% formattable::percent(),
             pct52WeekHigh = (priceEndOfMonth / price52WeekHigh) %>% formattable::percent()) %>%
      select(nameSector:idTicker, pct52WeekHigh, amountEquityMarketCapDilluted, amountEquityMarketCap, everything()) %>%
      suppressWarnings()

    all_data <-
      all_data %>%
      mutate(
        amountDebt = (pctDebtEquity * amountEquityMarketCapDilluted) %>% formattable::currency(digits = 0),
        amountEnterpriseValue = ifelse(amountDebt %>% is.na(), amountEquityMarketCapDilluted, amountEquityMarketCapDilluted + amountDebt),
        amountEBITDA = (amountDebt / ratioDebtEBITDA) %>% formattable::currency(digits = 0),
        pctLeverage = (amountDebt / amountEnterpriseValue) %>% formattable::percent(digits = 2),
        pctImpliedCapRate = (amountEBITDA / amountEnterpriseValue)
      ) %>%
      select(nameCompany:pct52WeekHigh,
             pctImpliedCapRate,
             amountEBITDA,
             amountEnterpriseValue,
             pctLeverage,
             amountDebt,
             pctDebtYield,
             everything())

    all_data <-
      all_data %>%
      mutate_at(all_data %>% select(matches("^pct")) %>% names(),
                funs(. %>% formattable::percent(digits = 2)))
    return(all_data)

  }

parse_rw_13 <-
  function(data) {
    data <-
      data %>%
      mutate_all(funs(. %>% str_trim() %>% str_to_upper()))

    start_row_v1 <-
      data %>%
      mutate(idRow = 1:n()) %>%
      filter(V1 %>% str_detect('NAME')) %>%
      slice(1) %>%
      .$idRow + 2

    start_row_v2 <-
      data %>%
      mutate(idRow = 1:n()) %>%
      filter(V2 %>% str_detect('NAME')) %>%
      slice(1) %>%
      .$idRow + 2

    start_row <-
      min(start_row_v1, start_row_v2)

    all_data <-
      data %>%
      filter(!V1 %>% str_detect("AVERAGE")) %>%
      mutate_all(str_to_upper) %>%
      mutate(idRow = 1:n())

    data_reit <-
      all_data %>%
      filter(!((V2 == '' & V3 == '' & V4 == ''))) %>%
      filter(!(V1 == ''& V2 == '' & V3 == '')) %>%
      filter(!V1 %in% c('REIT NAME TYPE', 'YIELD SPREAD MONTH', 'REIT NAME TICKER',
                        "ONE YEAR:",
                        "TWO YEAR:", "THREE YEAR:", "FIVE YEAR:")) %>%
      filter(!V2 %in% c('REIT NAME', 'TO DATE', 'REIT NAME TYPE')) %>%
      filter(!V6 %in% c('ESTIMATES')) %>%
      filter(!V7 %in% c('ESTIMATES')) %>%
      filter(!V8 %in% c('ESTIMATES')) %>%
      filter(!V1 %in% c('REIT NAME', 'DIVIDEND', 'YIELD SPREAD')) %>%
      filter(!V3 %in% c('ESTIMATES', 'PRICE PER SHARE', 'DIVERSIFIED', 'TOTAL RETURN', 'LODGING/RESORTS', 'SELF STORAGE', 'DIVERSIFIED', "HYBRID", 'ONE TWO')) %>%
      filter(!V4 %in% c('ESTIMATES', 'PRICE PER SHARE', 'DIVERSIFIED', 'TOTAL RETURN', 'LODGING/RESORTS', 'SELF STORAGE', 'DIVERSIFIED', 'ONE TWO')) %>%
      filter(!V5 %in% c('ESTIMATES', 'PRICE PER SHARE', 'DIVERSIFIED', 'TOTAL RETURN', 'LODGING/RESORTS', 'SELF STORAGE', 'DIVERSIFIED')) %>%
      filter(!V6 %in% c('ESTIMATES', 'PRICE PER SHARE', 'DIVERSIFIED', 'TOTAL RETURN', 'LODGING/RESORTS', 'SELF STORAGE', 'DIVERSIFIED')) %>%
      filter(!V6 %in% c('ESTIMATES', 'PRICE PER SHARE', 'DIVERSIFIED', 'TOTAL RETURN', 'LODGING/RESORTS', 'SELF STORAGE', 'DIVERSIFIED')) %>%
      mutate(isNumberData = ifelse(!V2 %>% as.numeric() %>% abs() %>%  substr(1,1) %>% is.na(),
                                   TRUE, FALSE)) %>%
      select(idRow, isNumberData, everything()) %>%
      suppressWarnings() %>%
      mutate_all(funs(ifelse(. == '', NA, .))) %>%
      mutate_all(funs(ifelse(. == 'NA', NA, .))) %>%
      mutate(V2 = ifelse(V2 %>% is.na(), V3, V2),
             V3 = ifelse(V3 == V2, NA, V3))

    data_reit <-
      data_reit %>%
      mutate(isNumberData = ifelse(V1 %>% is.na(), TRUE, isNumberData),
             isNumberData =  ifelse(V2 == "NA", TRUE, isNumberData),
             isNumberData =  ifelse(isNumberData %>% is.na(), FALSE, isNumberData),
             isNumberData =  ifelse(V1 %>% substr(1,1) %>% str_detect("[A-Z]"), FALSE, isNumberData),
             isNumberData =  ifelse(!V1 %>% substr(1,2) %>% as.numeric() %>% abs() %>% is.na(), TRUE, isNumberData),
             isNumberData = ifelse(V1 %>% nchar > 1, TRUE, isNumberData),
             isNumberData = ifelse(V2 %>% readr::parse_number() > 0, TRUE, isNumberData),
             isNumberData = ifelse(V1 %>% substr(1,2) %>% as.numeric() %>% abs() %in% c(0:9), T, isNumberData),
             isNumberData = ifelse(V1 %>% substr(1,1) %>% str_detect("^[A-Z]"), FALSE, isNumberData)
             ) %>%
      filter(!V1 %>% is.na()) %>%
      suppressMessages() %>%
      suppressWarnings()

    data_reit <-
      data_reit %>%
      mutate(
        isNumberData = ifelse(!V2 %>% readr::parse_number() %>% is.na(), T, isNumberData),
        isNumberData = ifelse(V2 %>% is.na(), T, isNumberData),
        isNumberData = ifelse(V1 %>% substr(1,2) %>% readr::parse_number() %>% is.na(), F, isNumberData)
      ) %>%
      suppressWarnings()

    is_mixed <-
      data_reit %>% mutate(V2 = V2 %>% readr::parse_number) %>% filter(!V2 %>% is.na()) %>% nrow() / nrow(data_reit) > .1

    if (is_mixed) {
      data_reit <-
        data_reit %>%
        mutate(isNumberData = ifelse(!V2 %>% readr::parse_number() %>% is.na(), T, F),
               isNumberData = ifelse((V2 %>% is.na()) & (V3 %>% is.na()) & (V4 %>% is.na()),
                                     TRUE, isNumberData
                                     ))
    }

    df_descriptions <-
      data_reit %>%
      filter(isNumberData == FALSE) %>%
      mutate(idRow = 1:n()) %>%
      select(which(colMeans(is.na(.)) < 1)) %>%
      mutate(isV1NA = ifelse(V1 %>% is.na(), TRUE, FALSE),
             V1Num = ifelse(!V1 %>% as.numeric() %>% is.na(), TRUE, FALSE)
             ) %>%
      filter(!V1Num) %>%
      select(-c(isNumberData, V1Num)) %>%
      select(isV1NA, everything()) %>%
      suppressWarnings()

    all_descriptions <-
      c(TRUE, FALSE) %>%
      map_df(function(x){
        has_no_data <-
          df_descriptions %>%
          filter(isV1NA == x) %>% nrow() == 0
        if (has_no_data) {
          return(data_frame())
        }

        df <-
          df_descriptions %>%
          filter(isV1NA == x) %>%
          select(-isV1NA) %>%
          select(which(colMeans(is.na(.)) < 1))

        is_8 <-
          df %>% ncol() == 8

        is_9 <-
          df %>% ncol == 9

        is_10 <-
          df %>% ncol() == 10

        is_11 <-
          df %>% ncol() == 11

        is_14 <-
          df %>% ncol() == 14

        is_12 <-
          df %>%
          ncol() == 12

        if (is_12) {
          df_1 <-
            df %>%
            filter(!V9 %>% is.na()) %>%
            select(which(colMeans(is.na(.)) < 1))

          df_1 <-
            df_1 %>%
            purrr::set_names(c('idRow', 'nameCompany', 'idTypeInvestment',
                               'idTicker', 'priceEndOfMonth', 'price52WeekHighLow', 'ratioFFOPriceCurrentYearNext', 'pershareFFOCurrentYearNextYear', 'pctFFOGrowth', 'pctDividendYieldSpread')) %>%
            separate(ratioFFOPriceCurrentYearNext, c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
            separate(price52WeekHighLow, c('price52WeekLow', 'price52WeekHigh'), sep = '\\ ') %>%
            separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
            separate(pctDividendYieldSpread, c('pctDividendYield', 'pctDividendSpread'), sep = '\\ ')

          has_no_v7 <-
            !'V7' %in% (df %>%
            filter(V9 %>% is.na()) %>%
            select(which(colMeans(is.na(.)) < 1)) %>% names())

          if (has_no_v7) {
            df_2 <-
              df %>%
              filter(V9 %>% is.na()) %>%
              select(which(colMeans(is.na(.)) < 1)) %>%
              purrr::set_names(c('idRow', 'nameCompany', 'idTypeInvestment',
                                 'idTicker', 'priceEndOfMonth', 'price52WeekLow', 'price52WeekHigh', 'pctDividendYield', 'pctDividendSpread'))

            df <-
              df_1 %>%
              bind_rows(df_2)

            return(df)
          }

          df_2 <-
            df %>%
            filter(V9 %>% is.na()) %>%
            select(which(colMeans(is.na(.)) < 1)) %>%
            filter(!V7 %>% is.na()) %>%
            select(which(colMeans(is.na(.)) < 1)) %>%
            purrr::set_names(c('idRow', 'nameCompany', 'idTypeInvestment',
                               'idTicker', 'priceEndOfMonth', 'price52WeekHighLowratioFFOPriceCurrentYearNext', 'pershareFFOCurrentYearNextYear', 'pctFFOGrowth', 'pctDividendYieldSpread')) %>%
            separate(price52WeekHighLowratioFFOPriceCurrentYearNext, c('price52WeekLow', 'price52WeekHigh', 'ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
            separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
            separate(pctDividendYieldSpread, c('pctDividendYield', 'pctDividendSpread'), sep = '\\ ')

          df_3 <-
            df %>%
            filter(V9 %>% is.na()) %>%
            select(which(colMeans(is.na(.)) < 1)) %>%
            filter(V7 %>% is.na()) %>%
            select(which(colMeans(is.na(.)) < 1))

        is_zero_one <-
          df$V4 %>% str_count('\\ ') %>% unique() %>% paste0(collapse = ":") == "0:1"

        if (is_zero_one){
          df_3 <-
            df_3 %>%
            mutate(countV4 = V4 %>% str_count('\\ ')) %>%
            filter(countV4 == 0) %>%
            select(which(colMeans(is.na(.)) < 1)) %>%
            select(-countV4) %>%
            purrr::set_names(c('idRow', 'nameCompany', 'idTypeInvestment',
                               'idTicker', 'priceEndOfMonth', 'price52WeekHighLowratioFFOPriceCurrentYearNext', 'pershareFFOCurrentYearNextYear', 'pctDividendYieldSpread')) %>%
            separate(price52WeekHighLowratioFFOPriceCurrentYearNext, c('price52WeekLow', 'price52WeekHigh', 'ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
            separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
            separate(pctDividendYieldSpread, c('pctDividendYield', 'pctDividendSpread'), sep = '\\ ') %>%
            bind_rows(
              df_3 %>%
                mutate(countV4 = V4 %>% str_count('\\ ')) %>%
                filter(countV4 == 1) %>%
                select(which(colMeans(is.na(.)) < 1)) %>%
                select(-countV4) %>%
                purrr::set_names(c('idRow', 'nameCompany', 'idTypeInvestment',
                                   'idTicker', 'priceEndOfMonthprice52WeekHigh', 'price52WeekLow', 'ratioFFOPriceCurrentYearNext','pershareFFOCurrentYearNextYear', 'pctDividendYieldSpread', 'pctFFOGrowth')) %>%
                separate(priceEndOfMonthprice52WeekHigh, c('priceEndOfMonth', 'price52WeekHigh'), sep = '\\ ') %>%

                separate(ratioFFOPriceCurrentYearNext, c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
                separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
                separate(pctDividendYieldSpread, c('pctDividendYield', 'pctDividendSpread'), sep = '\\ ')
            )
          df <-
            df_1 %>%
            bind_rows(list(df_2, df_3))

          return(df)
          }

        df_3 <-
          df_3 %>%
          purrr::set_names(c('idRow', 'nameCompany', 'idTypeInvestment',
                             'idTicker', 'priceEndOfMonth52WeekHigh', 'price52WeekLow', 'ratioFFOPriceCurrentYearNext', 'pershareFFOCurrentYearNextYear', 'pctFFOGrowthYield', 'pctDividendSpread')) %>%
          separate(ratioFFOPriceCurrentYearNext, c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
          separate(priceEndOfMonth52WeekHigh, c('priceEndOfMonth', 'price52WeekHigh'), sep = '\\ ') %>%
          separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
          separate(pctFFOGrowthYield, c('pctFFOGrowth', 'pctDividendYield'), sep = '\\ ')

        df <-
          df_1 %>%
          bind_rows(list(df_2, df_3))

        return(df)

        }

        if (is_11) {
          is_10 <-
            df %>%
            filter(!V2 %>% is.na()) %>%
            select(which(colMeans(is.na(.)) < 1)) %>% ncol() == 10

          is_0_3_2_1 <-
            df$V5 %>% str_count('\\ ') %>% unique() %>% paste0(collapse = ':')  == "0:3:2:1"

          if (is_0_3_2_1) {
            df_0 <-
              df %>%
              mutate(countV5 = V5 %>% str_count('\\ ')) %>%
              filter(countV5 == 0) %>%
              select(-c(countV5)) %>%
              select(which(colMeans(is.na(.)) < 1)) %>%
              mutate(countV6 = V6 %>% str_count('\\ ')) %>%
              filter(countV6 == 3) %>%
              select(-c(countV6)) %>%
              select(which(colMeans(is.na(.)) < 1)) %>%
              purrr::set_names(c('idRow', 'nameCompany', 'idTypeInvestment',
                                 'idTicker',
                                 'priceEndOfMonth', 'price52WeekHighLowratioFFOPriceCurrent' ,
                                 'pershareFFOCurrentYearNextYear',
                                 'pctFFOGrowth',
                                 'pctDividendYieldSpread')) %>%
              separate(price52WeekHighLowratioFFOPriceCurrent, c('price52WeekHigh', 'price52WeekLow', 'ratioFFOPriceCurrent'), sep = '\\ ') %>%
              separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
              separate(pctDividendYieldSpread, c('pctDividendYield', 'pctDividendSpread'), sep = '\\ ') %>%
              bind_rows(
                df %>%
                  mutate(countV5 = V5 %>% str_count('\\ ')) %>%
                  filter(countV5 == 0) %>%
                  select(-c(countV5)) %>%
                  select(which(colMeans(is.na(.)) < 1)) %>%
                  mutate(countV6 = V6 %>% str_count('\\ ')) %>%
                  filter(countV6 == 1) %>%
                  select(-c(countV6)) %>%
                  select(which(colMeans(is.na(.)) < 1)) %>%
                  mutate(V10 = ifelse(V10 %>% is.na(), V9, V10),
                         V9 = ifelse(V9 == V10, NA, V9)) %>%
                  purrr::set_names(c('idRow', 'nameCompany', 'idTypeInvestment',
                                     'idTicker',
                                     'priceEndOfMonth', 'price52WeekHighLow' ,
                                     'ratioFFOPriceCurrentYearNext',
                                     'pershareFFOCurrentYearNextYear',
                                     'pctFFOGrowth',
                                     'pctDividendYieldSpread')) %>%
                  separate(price52WeekHighLow, c('price52WeekHigh', 'price52WeekLow', 'ratioFFOPriceCurrent'), sep = '\\ ') %>%
                  separate(ratioFFOPriceCurrentYearNext, c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
                  separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
                  separate(pctDividendYieldSpread, c('pctDividendYield', 'pctDividendSpread'), sep = '\\ ')
              ) %>%
              bind_rows(
                df %>%
                  mutate(countV5 = V5 %>% str_count('\\ ')) %>%
                  filter(countV5 == 0) %>%
                  select(-c(countV5)) %>%
                  select(which(colMeans(is.na(.)) < 1)) %>%
                  mutate(countV6 = V6 %>% str_count('\\ ')) %>%
                  filter(countV6 == 1) %>%
                  select(-c(countV6)) %>%
                  select(which(colMeans(is.na(.)) < 1)) %>%
                  mutate(V10 = ifelse(V10 %>% is.na(), V9, V10),
                         V9 = ifelse(V9 == V10, NA, V9)) %>%
                  purrr::set_names(c('idRow', 'nameCompany', 'idTypeInvestment',
                                     'idTicker',
                                     'priceEndOfMonth', 'price52WeekHighLow' ,
                                     'ratioFFOPriceCurrentYearNext',
                                     'pershareFFOCurrentYearNextYear',
                                     'pctFFOGrowth',
                                     'pctDividendYieldSpread')) %>%
                  separate(price52WeekHighLow, c('price52WeekHigh', 'price52WeekLow', 'ratioFFOPriceCurrent'), sep = '\\ ') %>%
                    separate(ratioFFOPriceCurrentYearNext, c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
                  separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
                  separate(pctDividendYieldSpread, c('pctDividendYield', 'pctDividendSpread'), sep = '\\ ')
              )

            df_1 <-
              df %>%
              mutate(countV5 = V5 %>% str_count('\\ ')) %>%
              filter(countV5 == 1) %>%
              select(-c(countV5)) %>%
              select(which(colMeans(is.na(.)) < 1))

            df_1 <-
              df_1 %>%
              filter(!V9 %>% is.na()) %>%
              filter(!V8 %>% is.na()) %>%
              purrr::set_names(c('idRow', 'nameCompany', 'idTypeInvestment',
                                 'idTicker',
                                 'priceEndOfMonth', 'price52WeekHighLow' ,
                                 'ratioFFOPriceCurrentYearNext',
                                 'pershareFFOCurrentYearNextYear',
                                 'pctFFOGrowth',
                                 'pctDividendYieldSpread')) %>%
              separate(price52WeekHighLow, c('price52WeekHigh', 'price52WeekLow'), sep = '\\ ') %>%
              separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
              separate(ratioFFOPriceCurrentYearNext, c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
              separate(pctDividendYieldSpread, c('pctDividendYield', 'pctDividendSpread'), sep = '\\ ') %>%
              suppressWarnings() %>%
              bind_rows(
                df_1 %>%
                  filter(!V9 %>% is.na()) %>%
                  filter(V8 %>% is.na()) %>%
                  select(which(colMeans(is.na(.)) < 1)) %>%
                  purrr::set_names(c('idRow', 'nameCompany', 'idTypeInvestment',
                                     'idTicker',
                                     'priceEndOfMonth', 'price52WeekHighLow' ,
                                     'ratioFFOPriceCurrent',
                                     'pershareFFOCurrentYear',
                                     'pctDividendYieldSpread')) %>%
                  separate(price52WeekHighLow, c('price52WeekHigh', 'price52WeekLow'), sep = '\\ ') %>%
                  separate(pctDividendYieldSpread, c('pctDividendYield', 'pctDividendSpread'), sep = '\\ ') %>%
                  suppressWarnings()
              ) %>%
              bind_rows(
                df_1 %>%
                  filter(V9 %>% is.na()) %>%
                  select(which(colMeans(is.na(.)) < 1)) %>%
                  purrr::set_names(c('idRow', 'nameCompany', 'idTypeInvestment',
                                     'idTicker',
                                     'priceEndOfMonth', 'price52WeekHighLow' ,
                                     'pctDividendYieldSpread')) %>%
                  separate(price52WeekHighLow, c('price52WeekHigh', 'price52WeekLow'), sep = '\\ ') %>%
                  separate(pctDividendYieldSpread, c('pctDividendYield', 'pctDividendSpread'), sep = '\\ ') %>%
                  suppressWarnings())

            df_2 <-
              df %>%
              mutate(countV5 = V5 %>% str_count('\\ ')) %>%
              filter(countV5 == 2) %>%
              select(-c(countV5)) %>%
              select(which(colMeans(is.na(.)) < 1)) %>%
              purrr::set_names(c('idRow', 'nameCompany', 'idTypeInvestment',
                                 'idTicker',
                                 'priceEndOfMonth', 'price52WeekHighLowratioFFOPriceCurrent' ,
                                 'pershareFFOCurrentYearNextYear',
                                 'pctDividendYieldSpread')) %>%
              separate(price52WeekHighLowratioFFOPriceCurrent, c('price52WeekHigh', 'price52WeekLow', 'ratioFFOPriceCurrent'), sep = '\\ ') %>%
              separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
              separate(pctDividendYieldSpread, c('pctDividendYield', 'pctDividendSpread'), sep = '\\ ') %>%
              suppressWarnings()

            df_3 <-
              df %>%
              mutate(countV5 = V5 %>% str_count('\\ ')) %>%
              filter(countV5 == 3) %>%
              select(-c(countV5)) %>%
              select(which(colMeans(is.na(.)) < 1)) %>%
              purrr::set_names(c('idRow', 'nameCompany', 'idTypeInvestment',
                                 'idTicker',
                                 'priceEndOfMonth', 'price52WeekHighLowratioFFOPriceCurrentYearNext' ,
                                 'pershareFFOCurrentYearNextYear',
                                 'pctFFOGrowth',
                                 'pctDividendYieldSpread')) %>%
              separate(price52WeekHighLowratioFFOPriceCurrentYearNext, c('price52WeekHigh', 'price52WeekLow',
                                                                         'ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
              separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
              separate(pctDividendYieldSpread, c('pctDividendYield', 'pctDividendSpread'), sep = '\\ ') %>%
              suppressWarnings()


            df <-
              df_1 %>%
              bind_rows(list(df_0, df_2, df_3))
            return(df)
          }

          if (is_10) {
            df_a <-
              df %>%
              filter(!V2 %>% is.na()) %>%
              select(which(colMeans(is.na(.)) < 1)) %>%
              mutate(V5Count = V5 %>% str_count('\\ ')) %>%
              filter(V5Count == 3) %>%
              select(which(colMeans(is.na(.)) < 1)) %>%
              select(-V5Count)

            df_a <-
              df_a %>%
              purrr::set_names(c('idRow', 'nameCompany',
                                 'idTypeInvestment', 'idTicker',
                                 'priceEndOfMonth', 'price52WeekHighLowratioFFO',
                                 'pershareFFOCurrentYearNextYear',
                                 'pctFFOGrowth', 'pctDividendYieldSpread'))

            df_a <-
              df_a %>%
              separate(price52WeekHighLowratioFFO, c('price52WeekHigh', 'price52WeekLow', 'ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
              separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
              separate(pctDividendYieldSpread, c('pctDividendYield', 'pctDividendSpread'), sep = '\\ ') %>%
              suppressWarnings() %>%
              suppressMessages()



            df_b1 <-
              df %>%
              filter(!V2 %>% is.na()) %>%
              select(which(colMeans(is.na(.)) < 1)) %>%
              mutate(V5Count = V5 %>% str_count('\\ ')) %>%
              filter(V5Count == 1) %>%
              select(which(colMeans(is.na(.)) < 1)) %>%
              select(-V5Count) %>%
              filter(!V9 %>% is.na())

            df_b1 <-
              df_b1 %>%
              purrr::set_names(c('idRow', 'nameCompany',
                                 'idTypeInvestment', 'idTicker',
                                 'priceEndOfMonth', 'price52WeekHighLow',
                                 'ratioFFOPriceCurrentYearNext',
                                 'pershareFFOCurrentYearNextYear',
                                 'pctFFOGrowth', 'pctDividendYieldSpread')) %>%
              separate(ratioFFOPriceCurrentYearNext, c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
              separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
              separate(price52WeekHighLow, c('price52WeekHigh', 'price52WeekLow'), sep = '\\ ') %>%
              separate(pctDividendYieldSpread, c('pctDividendYield', 'pctDividendSpread'), sep = '\\ ') %>%
              suppressWarnings()

            df_b2 <-
              df %>%
              filter(!V2 %>% is.na()) %>%
              select(which(colMeans(is.na(.)) < 1)) %>%
              mutate(V5Count = V5 %>% str_count('\\ ')) %>%
              filter(V5Count == 1) %>%
              select(which(colMeans(is.na(.)) < 1)) %>%
              select(-V5Count) %>%
              filter(V9 %>% is.na()) %>%
              select(which(colMeans(is.na(.)) < 1)) %>%
              purrr::set_names(c('idRow', 'nameCompany',
                                 'idTypeInvestment', 'idTicker',
                                 'priceEndOfMonth', 'price52WeekHighLow',
                                 'pctDividendYieldSpread')) %>%
              separate(price52WeekHighLow, c('price52WeekHigh', 'price52WeekLow'), sep = '\\ ') %>%
              separate(pctDividendYieldSpread, c('pctDividendYield', 'pctDividendSpread'), sep = '\\ ')



            df_c <-
              df %>%
              filter(V2 %>% is.na()) %>%
              mutate(V5Count = V6 %>% str_count('\\ ')) %>%
              filter(V5Count == 3) %>%
              select(which(colMeans(is.na(.)) < 1)) %>%
              select(-V5Count) %>%
              purrr::set_names(c('idRow', 'nameCompany',
                                 'idTypeInvestment', 'idTicker',
                                 'priceEndOfMonth', 'price52WeekHighLowratioFFO',
                                 'pershareFFOCurrentYearNextYear',
                                 'pctFFOGrowth', 'pctDividendYieldSpread')) %>%
              separate(price52WeekHighLowratioFFO, c('price52WeekHigh', 'price52WeekLow', 'ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
              separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
              separate(pctDividendYieldSpread, c('pctDividendYield', 'pctDividendSpread'), sep = '\\ ') %>%
              suppressWarnings() %>%
              suppressMessages()

            df_2 <-
                df %>%
                  filter(V2 %>% is.na()) %>%
                  mutate(V5Count = V6 %>% str_count('\\ ')) %>%
                  filter(V5Count == 1) %>%
                  select(which(colMeans(is.na(.)) < 1)) %>%
                  select(-V5Count)

              df_e <-
                df_2 %>%
                filter(!is.na(V10)) %>%
                select(which(colMeans(is.na(.)) < 1)) %>%
                purrr::set_names(c('idRow', 'nameCompany',
                                   'idTypeInvestment', 'idTicker',
                                   'priceEndOfMonth', 'price52WeekHighLow',
                                   'ratioFFOPriceCurrentYearNext',
                                   'pershareFFOCurrentYearNextYear',
                                   'pctFFOGrowth', 'pctDividendYieldSpread')) %>%
                separate(ratioFFOPriceCurrentYearNext, c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
                separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
                separate(price52WeekHighLow, c('price52WeekHigh', 'price52WeekLow'), sep = '\\ ') %>%
                separate(pctDividendYieldSpread, c('pctDividendYield', 'pctDividendSpread'), sep = '\\ ') %>%
                suppressWarnings()

              df_f <-
                df_2 %>%
                filter(V10 %>% is.na()) %>%
                select(which(colMeans(is.na(.)) < 1)) %>%
                purrr::set_names(c('idRow', 'nameCompany',
                                   'idTypeInvestment', 'idTicker',
                                   'priceEndOfMonth', 'price52WeekHighLow',
                                   'pctDividendYieldSpread')) %>%
                separate(price52WeekHighLow, c('price52WeekHigh', 'price52WeekLow'), sep = '\\ ') %>%
                separate(pctDividendYieldSpread, c('pctDividendYield', 'pctDividendSpread'), sep = '\\ ')

              df <-
                df_a %>%
                bind_rows(list(df_b1, df_b2, df_c, df_e, df_f))

                return(df)

          }


          df_a <-
            df %>%
            filter(!V2 %>% is.na()) %>%
            select(which(colMeans(is.na(.)) < 1)) %>%
            purrr::set_names(list('V',1:9) %>% purrr::invoke(paste0,.))

          df_b <-
            df %>%
            filter(V2 %>% is.na()) %>%
            filter(!V7 %>% is.na()) %>%
            select(which(colMeans(is.na(.)) < 1)) %>%
            purrr::set_names(list('V',1:9) %>% purrr::invoke(paste0,.))
            bind_rows(
              df %>%
                filter(V2 %>% is.na()) %>%
                filter(V7 %>% is.na()) %>%
                select(which(colMeans(is.na(.)) < 1)) %>%
                purrr::set_names(list('V',1:9) %>% purrr::invoke(paste0,.))
            )

          df <-
            df_a %>%
            bind_rows(df_b)

          df <-
            df %>%
            purrr::set_names(c('idRow', 'nameCompany', 'idTypeInvestment',
                               'idTicker', 'priceEndOfMonth', 'price52WeekHighLow', 'ratioFFOPriceCurrentYearNext', 'pershareFFOCurrentYearNextYear', 'pctFFOGrowth')) %>%
            separate(ratioFFOPriceCurrentYearNext, c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
            separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
            separate(price52WeekHighLow, c('price52WeekHigh', 'price52WeekLow'), sep = '\\ ') %>%
            suppressWarnings()

          return(df)
        }

        if (is_14) {
          df <-
            df %>%
            mutate(countV5 = V5 %>% str_count('\\ '))
          counts <-
            df$countV5 %>%
            sort() %>%
            unique()

          df <-
            counts %>%
            map_df(function(x){{
              data <-
                df %>%
                filter(countV5 == x) %>%
                select(-countV5) %>%
                select(which(colMeans(is.na(.)) < 1))

              is_0 <-
                x == 0

              is_1 <-
                x == 1
              is_2 <-
                x == 2
              is_3 <-
                x == 3

              if (is_0) {

               df_a <-
                 data %>%
                 mutate(countV4 = V4 %>% str_count('\\ ')) %>%
                 filter(countV4 == 0) %>%
                 select(-countV4) %>%
                 mutate_all(funs(ifelse(. == "NA", NA, .))) %>%
                 select(which(colMeans(is.na(.)) < 1))
               is_14 <-
                 df_a %>%
                 ncol() == 14
               if (is_14) {
                 df_a <-
                 df_a %>%
                 purrr::set_names(c('idRow','nameCompany', 'idTypeInvestment',
                                    'idTicker',
                                    'priceEndOfMonth',
                                    'price52WeekHigh',
                                    'price52WeekLow',
                                    'ratioFFOPriceCurrent', 'ratioFFOPriceYearNext',
                                    'pershareFFOCurrentYear', 'pershareFFONextYear',
                                    'pctFFOGrowth',
                                    'pctDividendYield', 'pctDividendSpread'))

               no_match <-
                 !df_a %>% nrow() == nrow(data)
               if (!no_match) {
                 return(df_a)
               }
                 if (no_match) {
                 df_b <-
                 data %>%
                 mutate(countV4 = V4 %>% str_count('\\ ')) %>%
                 filter(countV4 > 0) %>%
                 select(-countV4) %>%
                 select(which(colMeans(is.na(.)) < 1))

               df_b <-
                 df_b %>%
                 purrr::set_names(c('idRow', 'nameCompany',
                                    'idTypeInvestment', 'idTicker', 'priceEndofMonthHigh', 'price52WeekLow',
                                    'ratioFFOPriceCurrentYearNext',
                                    'pershareFFOCurrentYearNextYear',
                                    'pctFFOGrowthDividendYield', 'pctDividendSpread'))

               df_b <-
                 df_b %>%
                 separate(priceEndofMonthHigh, c('priceEndOfMonth', 'price52WeekHigh'), sep = '\\ ') %>%
                 separate(ratioFFOPriceCurrentYearNext, c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
                 separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
                 separate(pctFFOGrowthDividendYield, c('pctFFOGrowth', 'pctDividendYield'), sep = '\\ ') %>%
                 suppressWarnings() %>%
                 suppressMessages()

               data <-
                  df_a %>%
                  bind_rows(df_b)

               return(data)
                 }
               }
               is_7 <-
                 df_a %>%
                 ncol() == 7

               is_9 <-
                 df_a %>%
                 ncol() == 9

               if (is_9) {
                 data <-
                   df_a %>%
                   purrr::set_names(c('idRow', 'nameCompany',
                                      'idTypeInvestment', 'idTicker','priceEndOfMonth', 'price52WeekHigh', 'price52WeekLow', 'pctDividendYield',
                                      'pctDividendSpread'
                   ))
               }
               return(data)
              }

              if(is_1) {
                has_na <-
                  data %>%
                  filter(V2 == "NA") %>%
                  nrow() > 0

                if (has_na) {
                  data <-
                    data %>%
                    mutate_all(funs(ifelse(. == "NA", NA, .)))

                  df_1 <-
                    data %>%
                    filter(!V2 %>% is.na()) %>%
                    select(which(colMeans(is.na(.)) < 1))

                  df_2 <-
                    data %>%
                    filter(V2 %>% is.na()) %>%
                    select(which(colMeans(is.na(.)) < 1)) %>%
                    purrr::set_names(c('idRow', 'nameCompany',
                                       ))
                }

                data <-
                  data %>%
                  purrr::set_names(c('idRow', 'nameCompany',
                                     'idTypeInvestment', 'idTicker',
                                     'priceEndOfMonth', 'price52WeekHighLow',
                                     'ratioFFOPriceCurrentYearNext',
                                     'pershareFFOCurrentYearNextYear',
                                     'pctFFOGrowth', 'pctDividendYieldSpread'))

                data <-
                  data %>%
                  separate(price52WeekHighLow, c('price52WeekHigh', 'price52WeekLow'), sep = '\\ ') %>%
                  separate(ratioFFOPriceCurrentYearNext, c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
                  separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
                  separate(pctDividendYieldSpread, c('pctDividendYield', 'pctDividendSpread'), sep = '\\ ') %>%
                  suppressWarnings() %>%
                  suppressMessages()
              }

              if(is_3) {
                data <-
                  data %>%
                  purrr::set_names(c('idRow', 'nameCompany',
                                     'idTypeInvestment', 'idTicker',
                                     'priceEndOfMonth', 'price52WeekHighLowratioFFO',
                                     'pershareFFOCurrentYearNextYear',
                                     'pctFFOGrowth', 'pctDividendYieldSpread'))

                data <-
                  data %>%
                  separate(price52WeekHighLowratioFFO, c('price52WeekHigh', 'price52WeekLow', 'ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
                  separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
                  separate(pctDividendYieldSpread, c('pctDividendYield', 'pctDividendSpread'), sep = '\\ ') %>%
                  suppressWarnings() %>%
                  suppressMessages()
              }

              return(data)
            }})

          return(df)
        }

        if (is_8) {
          df <-
            df %>%
            purrr::set_names(c('idRow','nameCompany', 'idTypeInvestment',
                               'idTickerpriceEndOfMonth', 'price52WeekHighLow',
                               'ratioFFOPriceCurrentYearNext',
                               'pershareFFOCurrentYearNextYear',
                               'pctFFOGrowth'))

          df <-
            df %>%
            separate(idTickerpriceEndOfMonth, c('idTicker', 'priceEndOfMonth'), sep = '\\ ') %>%
            separate(price52WeekHighLow, c('price52WeekHigh', 'price52WeekLow'), sep = '\\ ') %>%
            separate(ratioFFOPriceCurrentYearNext, c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
            separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
            suppressWarnings() %>%
            suppressMessages()
        }

        if (is_9) {
          has_3 <-
            df %>% mutate(V4Count = V4 %>% str_count('\\ ')) %>%
            .$V4Count %>%
            max(na.rm = TRUE) == 3

          if (has_3) {
            df_a <-
              df %>%
              mutate(V2char = V2 %>% nchar()) %>%
              filter(V2char == 1) %>%
              select(which(colMeans(is.na(.)) < 1)) %>%
              select(-V2char)

            df_a <-
              df_a %>%
              purrr::set_names(c('idRow','nameCompany', 'idTypeInvestment',
                                 'idTickerPriceEndOfMonth',
                                 'price52WeekHighLow',
                                 'ratioFFOPriceCurrentYearNext',
                                 'pershareFFOCurrentYearNextYear',
                                 'pctFFOGrowth'))

            df_a <-
              df_a %>%
              separate(idTickerPriceEndOfMonth, c('idTicker', 'priceEndOfMonth'), sep = '\\ ') %>%
              separate(price52WeekHighLow, c('price52WeekHigh', 'price52WeekLow'), sep = '\\ ') %>%
              separate(ratioFFOPriceCurrentYearNext, c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
              separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
              suppressWarnings() %>%
              suppressMessages()

            df_b <-
              df %>%
              filter(!idRow %in% df_a$idRow) %>%
              select(which(colMeans(is.na(.)) < 1))

            df_b <-
              df_b %>%
              purrr::set_names(c('idRow','nameCompany', 'idTypeInvestment',
                                 'idTickerPriceEndOfMonth',
                                 'price52WeekHighLow',
                                 'ratioFFOPriceCurrentYearNext',
                                 'pershareFFOCurrentYearNextYear',
                                 'pctFFOGrowth'))

            df_b <-
              df_b %>%
              separate(idTickerPriceEndOfMonth, c('idTicker', 'priceEndOfMonth'), sep = '\\ ') %>%
              separate(price52WeekHighLow, c('price52WeekHigh', 'price52WeekLow'), sep = '\\ ') %>%
              separate(ratioFFOPriceCurrentYearNext, c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
              separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
              suppressWarnings() %>%
              suppressMessages()

            df <-
              df_a %>%
              bind_rows(df_b)
            return(df)
          }

          has_mixed <-
            df %>% filter(V2 %>% is.na()) %>% nrow() / nrow(df) > 0

          if (has_mixed) {
            df_1 <-
              df %>%
              filter(!V2 %>% is.na()) %>%
              select(which(colMeans(is.na(.)) < 1))

            df_1 <-
              df_1 %>%
              purrr::set_names(c('idRow','nameCompany', 'idTypeInvestment',
                                 'idTicker', 'priceEndOfMonth',
                                 'price52WeekHighLow',
                                 'ratioFFOPriceCurrentYearNext',
                                 'pershareFFOCurrentYearNextYear',
                                 'pctFFOGrowth')) %>%
              separate(price52WeekHighLow, c('price52WeekHigh', 'price52WeekLow'), sep = '\\ ') %>%
              separate(ratioFFOPriceCurrentYearNext, c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
              separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
              suppressWarnings() %>%
              suppressMessages()


            df_2 <-
              df %>%
              filter(V2 %>% is.na()) %>%
              select(which(colMeans(is.na(.)) < 1)) %>%
              purrr::set_names(c('idRow','nameCompany', 'idTypeInvestment',
                                 'idTicker', 'priceEndOfMonth',
                                 'price52WeekHighLow',
                                 'ratioFFOPriceCurrentYearNext',
                                 'pershareFFOCurrentYearNextYearpctFFOGrowth')) %>%
              separate(price52WeekHighLow, c('price52WeekHigh', 'price52WeekLow'), sep = '\\ ') %>%
              separate(ratioFFOPriceCurrentYearNext, c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
              separate(pershareFFOCurrentYearNextYearpctFFOGrowth, c('pershareFFOCurrentYear', 'pershareFFONextYear', 'pctFFOGrowth'), sep = '\\ ') %>%
              suppressWarnings() %>%
              suppressMessages()

            df <-
              df_1 %>%
              bind_rows(df_2)

            return(df)
          }
          is_mixed <-
            df %>%
            filter(V4 %>% is.na()) %>% nrow() / nrow(df) > 0
          if (is_mixed) {
            df_1 <-
              df %>%
              filter(!V4 %>% is.na()) %>%
              select(which(colMeans(is.na(.)) < 1)) %>%
              mutate(countV2 = V2 %>% str_count('\\ ')) %>%
              filter(countV2 == 0) %>%
              select(-countV2) %>%
              select(which(colMeans(is.na(.)) < 1)) %>%
              mutate(V2Num = V2 %>% readr::parse_number()) %>%
              filter(!V2Num %>% is.na()) %>%
              select(-V2Num) %>%
              purrr::set_names(c('idRow', 'nameCompanyIdTicker',
                                 'priceEndOfMonth', 'price52WeekHigh','price52WeekLowratioFFOPriceCurrentYearNext','pershareFFOCurrentYearNextYear',
                                 'pctFFOGrowth')) %>%
              separate(nameCompanyIdTicker, c('nameCompany', 'idTicker'), sep =  ' (?=[^ ]+$)') %>%
              separate(price52WeekLowratioFFOPriceCurrentYearNext, c('price52WeekLow', 'ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
              separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
              mutate(nameCompany = nameCompany %>% str_trim()) %>%
              bind_rows(
                df %>%
                  filter(!V4 %>% is.na()) %>%
                  select(which(colMeans(is.na(.)) < 1)) %>%
                  mutate(countV2 = V2 %>% str_count('\\ ')) %>%
                  filter(countV2 == 0) %>%
                  select(-countV2) %>%
                  select(which(colMeans(is.na(.)) < 1)) %>%
                  mutate(V2Num = V2 %>% readr::parse_number()) %>%
                  filter(V2Num %>% is.na()) %>%
                  select(-V2Num) %>%
                  purrr::set_names(c('idRow', 'nameCompanyidTypeInvestment', 'idTicker',
                                     'priceEndOfMonth','price52WeekHighprice52WeekLowratioFFOPriceCurrentYearNext','pershareFFOCurrentYearNextYear',
                                     'pctFFOGrowth')) %>%
                  separate(nameCompanyidTypeInvestment, c('nameCompany', 'idTypeInvestment'), sep =  ' (?=[^ ]+$)') %>%
                  separate(price52WeekHighprice52WeekLowratioFFOPriceCurrentYearNext, c('price52WeekHigh','price52WeekLow', 'ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
                  separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
                  mutate(nameCompany = nameCompany %>% str_trim())
              ) %>%
              bind_rows(
                df %>%
                  filter(!V4 %>% is.na()) %>%
                  select(which(colMeans(is.na(.)) < 1)) %>%
                  mutate(countV2 = V2 %>% str_count('\\ ')) %>%
                  filter(countV2 == 1) %>%
                  select(-countV2) %>%
                  select(which(colMeans(is.na(.)) < 1)) %>%
                  purrr::set_names(c('idRow', 'nameCompany', 'idTypeInvestmentTicker',
                                     'priceEndOfMonth', 'price52WeekHighLow', 'ratioFFOPriceCurrentYearNext','pershareFFOCurrentYearNextYear',
                                     'pctFFOGrowth')) %>%
                  separate(idTypeInvestmentTicker, c('idTypeInvestment', 'idTicker'), sep = '\\ ') %>%
                  separate(price52WeekHighLow, c('price52WeekHigh', 'price52WeekLow'), sep = '\\ ') %>%
                  separate(ratioFFOPriceCurrentYearNext, c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
                  separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
                  suppressWarnings()
              )
            df_2 <-
              df %>%
              filter(V4 %>% is.na()) %>%
              select(which(colMeans(is.na(.)) < 1)) %>%
              purrr::set_names(
                c(
                  'idRow',
                  'nameCompany',
                  'idTypeInvestmentidTicker',
                  'priceEndOfMonth',
                  'price52WeekHighLow',
                  'ratioFFOPriceCurrentYearNext',
                  'pershareFFOCurrentYearNextYear',
                  'pctFFOGrowth'
                )
              ) %>%
              separate(idTypeInvestmentidTicker,
                       c('idTypeInvestment', 'idTicker'),
                       sep = '\\ ') %>%
              separate(price52WeekHighLow,
                       c('price52WeekHigh', 'price52WeekLow'),
                       sep = '\\ ') %>%
              separate(
                ratioFFOPriceCurrentYearNext,
                c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'),
                sep = '\\ '
              ) %>%
              separate(
                pershareFFOCurrentYearNextYear,
                c('pershareFFOCurrentYear', 'pershareFFONextYear'),
                sep = '\\ '
              ) %>%
              suppressWarnings() %>%
              suppressMessages()

            df <-
              df_1 %>%
              bind_rows(df_2)
            return(df)
          }

          if (!has_3) {
            df <-
              df %>%
              purrr::set_names(c('idRow','nameCompany', 'idTypeInvestment',
                                 'idTicker', 'priceEndOfMonth',
                                 'price52WeekHighLow',
                                 'ratioFFOPriceCurrentYearNext',
                                 'pershareFFOCurrentYearNextYear',
                                 'pctFFOGrowth'))

            df <-
              df %>%
              separate(price52WeekHighLow, c('price52WeekHigh', 'price52WeekLow'), sep = '\\ ') %>%
              separate(ratioFFOPriceCurrentYearNext, c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
              separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
              suppressWarnings() %>%
              suppressMessages()
          }
        }

        if (is_10) {
          df <-
            df %>%
            mutate(V2NA = ifelse(V2 %>% is.na(), T, F)) %>%
            select(V2NA, everything())

          has_3 <-
            df$V5 %>% str_count('\\ ') %>% max() == 3

          if (has_3) {
            is_messed <-
              df$V2 %>% str_detect("^[0-9]") %>% sum(na.rm = T) > 0

            is_3_1 <-
              df$V5 %>% str_count('\\ ') %>% unique() %>% paste0(collapse = ':') == "3:1"

            is_0_1_3_2 <-
              df$V5 %>% str_count('\\ ') %>% unique() %>% sort() %>% paste0(collapse = ':') == "0:1:2:3"

            is_3_2_1 <-
              df$V5 %>% str_count('\\ ') %>% unique() %>% paste0(collapse = ':')  == "3:2:1"

            if (is_3_2_1) {
              df_1 <-
                df %>%
                mutate(countV5 = V5 %>% str_count('\\ ')) %>%
                filter(countV5 == 1) %>%
                select(-c(V2NA, countV5)) %>%
                select(which(colMeans(is.na(.)) < 1))

              df_1 <-
                df_1 %>%
                filter(!V9 %>% is.na()) %>%
                filter(!V8 %>% is.na()) %>%
                purrr::set_names(c('idRow', 'nameCompany', 'idTypeInvestment',
                                   'idTicker',
                                   'priceEndOfMonth', 'price52WeekHighLow' ,
                                   'ratioFFOPriceCurrentYearNext',
                                   'pershareFFOCurrentYearNextYear',
                                   'pctFFOGrowth',
                                   'pctDividendYieldSpread')) %>%
                separate(price52WeekHighLow, c('price52WeekHigh', 'price52WeekLow'), sep = '\\ ') %>%
                separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
                separate(ratioFFOPriceCurrentYearNext, c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
                separate(pctDividendYieldSpread, c('pctDividendYield', 'pctDividendSpread'), sep = '\\ ') %>%
                suppressWarnings() %>%
                bind_rows(
                  df_1 %>%
                    filter(!V9 %>% is.na()) %>%
                    filter(V8 %>% is.na()) %>%
                    select(which(colMeans(is.na(.)) < 1)) %>%
                    purrr::set_names(c('idRow', 'nameCompany', 'idTypeInvestment',
                                       'idTicker',
                                       'priceEndOfMonth', 'price52WeekHighLow' ,
                                       'ratioFFOPriceCurrent',
                                       'pershareFFOCurrentYear',
                                       'pctDividendYieldSpread')) %>%
                    separate(price52WeekHighLow, c('price52WeekHigh', 'price52WeekLow'), sep = '\\ ') %>%
                    separate(pctDividendYieldSpread, c('pctDividendYield', 'pctDividendSpread'), sep = '\\ ') %>%
                    suppressWarnings()
                ) %>%
                bind_rows(
                df_1 %>%
                filter(V9 %>% is.na()) %>%
                select(which(colMeans(is.na(.)) < 1)) %>%
                purrr::set_names(c('idRow', 'nameCompany', 'idTypeInvestment',
                                   'idTicker',
                                   'priceEndOfMonth', 'price52WeekHighLow' ,
                                   'pctDividendYieldSpread')) %>%
                separate(price52WeekHighLow, c('price52WeekHigh', 'price52WeekLow'), sep = '\\ ') %>%
                separate(pctDividendYieldSpread, c('pctDividendYield', 'pctDividendSpread'), sep = '\\ ') %>%
                suppressWarnings())

              df_2 <-
                df %>%
                mutate(countV5 = V5 %>% str_count('\\ ')) %>%
                filter(countV5 == 2) %>%
                select(-c(V2NA, countV5)) %>%
                select(which(colMeans(is.na(.)) < 1)) %>%
                purrr::set_names(c('idRow', 'nameCompany', 'idTypeInvestment',
                                   'idTicker',
                                   'priceEndOfMonth', 'price52WeekHighLowratioFFOPriceCurrent' ,
                                   'pershareFFOCurrentYearNextYear',
                                   'pctDividendYieldSpread')) %>%
                separate(price52WeekHighLowratioFFOPriceCurrent, c('price52WeekHigh', 'price52WeekLow', 'ratioFFOPriceCurrent'), sep = '\\ ') %>%
                separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
                separate(pctDividendYieldSpread, c('pctDividendYield', 'pctDividendSpread'), sep = '\\ ') %>%
                suppressWarnings()

              df_3 <-
                df %>%
                mutate(countV5 = V5 %>% str_count('\\ ')) %>%
                filter(countV5 == 3) %>%
                select(-c(V2NA, countV5)) %>%
                select(which(colMeans(is.na(.)) < 1)) %>%
                purrr::set_names(c('idRow', 'nameCompany', 'idTypeInvestment',
                                   'idTicker',
                                   'priceEndOfMonth', 'price52WeekHighLowratioFFOPriceCurrentYearNext' ,
                                   'pershareFFOCurrentYearNextYear',
                                   'pctFFOGrowth',
                                   'pctDividendYieldSpread')) %>%
                separate(price52WeekHighLowratioFFOPriceCurrentYearNext, c('price52WeekHigh', 'price52WeekLow',
                                                                           'ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
                separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
                separate(pctDividendYieldSpread, c('pctDividendYield', 'pctDividendSpread'), sep = '\\ ') %>%
                suppressWarnings()


              df <-
                df_1 %>%
                bind_rows(list(df_2, df_3))
              return(df)
            }

            if (is_0_1_3_2) {
              is_zero <-
                df %>%
                select(-V2NA) %>%
                filter(V2 %>% is.na()) %>%
                select(which(colMeans(is.na(.)) < 1)) %>%
                nrow() == 0

              if (is_zero) {
               df_1 <-
                 df %>%
                  select(-V2NA) %>%
                  filter(!V3 %>% is.na()) %>%
                  select(which(colMeans(is.na(.)) < 1)) %>%
                  purrr::set_names(c('idRow', 'nameCompany', 'idTypeInvestment',
                                     'idTicker',
                                     'priceEndOfMonth', 'price52WeekHighLow', 'ratioFFOPriceCurrentYearNext','pershareFFOCurrentYearNextYear',
                                     'pctFFOGrowth')) %>%
                  separate(price52WeekHighLow, c('price52WeekHigh', 'price52WeekLow'), sep = '\\ ') %>%
                  separate(ratioFFOPriceCurrentYearNext, c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
                  separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
                  suppressWarnings()

               df_2 <-
                 df %>%
                 select(-V2NA) %>%
                 filter(V3 %>% is.na()) %>%
                 select(which(colMeans(is.na(.)) < 1)) %>%
                 purrr::set_names(c('idRow', 'nameCompany', 'idTypeInvestment',
                                    'idTicker',
                                    'priceEndOfMonth', 'price52WeekHighLow', 'ratioFFOPriceCurrentYearNext','pershareFFOCurrentYearNextYear',
                                    'pctFFOGrowth')) %>%
                 separate(price52WeekHighLow, c('price52WeekHigh', 'price52WeekLow'), sep = '\\ ') %>%
                 separate(ratioFFOPriceCurrentYearNext, c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
                 separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
                 suppressWarnings()

               df <-
                 df_1 %>%
                 bind_rows(df_2)
               return(df)
              }

              is_8 <-
                df %>%
                select(-V2NA) %>%
                filter(V2 %>% is.na()) %>%
                select(which(colMeans(is.na(.)) < 1)) %>% ncol() == 8

              is_9 <-
                df %>%
                select(-V2NA) %>%
                filter(V2 %>% is.na()) %>%
                select(which(colMeans(is.na(.)) < 1)) %>% ncol() == 9

              df %>%
                select(-V2NA) %>%
                filter(V2 %>% is.na()) %>%
                select(which(colMeans(is.na(.)) < 1)) %>% ncol() == 11

              if (is_9) {
              df_1 <-
                df %>%
                select(-V2NA) %>%
                filter(V2 %>% is.na()) %>%
                select(which(colMeans(is.na(.)) < 1)) %>%
                purrr::set_names(list("V", 1:9) %>% purrr::invoke(paste0,.))

              df_1 <-
                df_1 %>%
                purrr::set_names(c('idRow', 'nameCompany', 'idTypeInvestment',
                                   'idTicker',
                                   'priceEndOfMonth', 'price52WeekHighLow', 'ratioFFOPriceCurrentYearNext','pershareFFOCurrentYearNextYear',
                                   'pctFFOGrowth')) %>%
                separate(price52WeekHighLow, c('price52WeekHigh', 'price52WeekLow'), sep = '\\ ') %>%
                separate(ratioFFOPriceCurrentYearNext, c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
                separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
                suppressWarnings()

              df_2 <-
                df %>%
                select(-V2NA) %>%
                filter(!V2 %>% is.na()) %>%
                select(which(colMeans(is.na(.)) < 1)) %>%
                purrr::set_names(list("V", 1:9) %>% purrr::invoke(paste0,.)) %>%
                mutate(countV6 = V6 %>% str_count('\\ '))

              df_2a <-
                df_2 %>%
                filter(countV6 == 1) %>%
                select(-countV6) %>%
                select(which(colMeans(is.na(.)) < 1)) %>%
                purrr::set_names(c('idRow', 'nameCompany', 'idTypeInvestment',
                                   'idTicker',
                                   'priceEndOfMonth', 'price52WeekHighLow', 'ratioFFOPriceCurrentYearNext','pershareFFOCurrentYearNextYear',
                                   'pctFFOGrowth')) %>%
                separate(price52WeekHighLow, c('price52WeekHigh', 'price52WeekLow'), sep = '\\ ') %>%
                separate(ratioFFOPriceCurrentYearNext, c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
                separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
                suppressWarnings()

              df_2b <-
                df_2 %>%
                filter(countV6 == 3) %>%
                select(-countV6) %>%
                select(which(colMeans(is.na(.)) < 1)) %>%
                purrr::set_names(c('idRow', 'nameCompany', 'idTypeInvestment',
                                   'idTicker',
                                   'priceEndOfMonth', 'price52WeekHighLowratioFFOPriceCurrentYearNext',
                                   'pershareFFOCurrentYearNextYear',
                                   'pctFFOGrowth')) %>%
                separate(price52WeekHighLowratioFFOPriceCurrentYearNext, c('price52WeekHigh', 'price52WeekLow', 'ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
                separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
                suppressWarnings()


              df <-
                df_1 %>%
                bind_rows(list(df_2a, df_2b))
              return(df)
            }
              if (is_8) {
                df_1 <-
                df %>%
                select(-V2NA) %>%
                filter(V2 %>% is.na()) %>%
                select(which(colMeans(is.na(.)) < 1)) %>%
                purrr::set_names(list("V", 1:8) %>% purrr::invoke(paste0,.))

              df_1 <-
                df_1 %>%
                purrr::set_names(c('idRow', 'nameCompany', 'idTypeInvestmentTicker',
                                   'priceEndOfMonth', 'price52WeekHighLow', 'ratioFFOPriceCurrentYearNext','pershareFFOCurrentYearNextYear',
                                   'pctFFOGrowth')) %>%
                separate(idTypeInvestmentTicker, c('idTypeInvestment', 'idTicker'), sep = '\\ ') %>%
                separate(price52WeekHighLow, c('price52WeekHigh', 'price52WeekLow'), sep = '\\ ') %>%
                separate(ratioFFOPriceCurrentYearNext, c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
                separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
                suppressWarnings()

              df_2 <-
                df %>%
                select(-V2NA) %>%
                filter(!V2 %>% is.na()) %>%
                select(which(colMeans(is.na(.)) < 1)) %>%
                purrr::set_names(list("V", 1:8) %>% purrr::invoke(paste0,.)) %>%
                mutate(countV3 = V3 %>% str_count('\\ '))

              df_2a <-
                df_2 %>%
                filter(countV3 == 0) %>%
                select(-countV3) %>%
                select(which(colMeans(is.na(.)) < 1)) %>%
                purrr::set_names(c('idRow', 'nameCompanyIdTypeInvestment', 'idTicker',
                                   'priceEndOfMonth', 'price52WeekHighLowratioFFOPriceCurrentYearNext','pershareFFOCurrentYearNextYear',
                                   'pctFFOGrowth')) %>%
                separate(nameCompanyIdTypeInvestment, c('nameCompany', 'idTypeInvestment'), sep =  ' (?=[^ ]+$)') %>%
                separate(price52WeekHighLowratioFFOPriceCurrentYearNext, c('price52WeekHigh', 'price52WeekLow', 'ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
                separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
                mutate(nameCompany = nameCompany %>% str_trim())

              df_2b <-
                df_2 %>%
                filter(countV3 == 1) %>%
                select(-countV3) %>%
                select(which(colMeans(is.na(.)) < 1)) %>%
                purrr::set_names(c('idRow', 'nameCompany', 'idTypeInvestmentTicker',
                                   'priceEndOfMonth', 'price52WeekHighLow', 'ratioFFOPriceCurrentYearNext','pershareFFOCurrentYearNextYear',
                                   'pctFFOGrowth')) %>%
                separate(idTypeInvestmentTicker, c('idTypeInvestment', 'idTicker'), sep = '\\ ') %>%
                separate(price52WeekHighLow, c('price52WeekHigh', 'price52WeekLow'), sep = '\\ ') %>%
                separate(ratioFFOPriceCurrentYearNext, c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
                separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ')


            df <-
              df_1 %>%
              bind_rows(list(df_2a, df_2b))
              return(df)
              }
            }

            if (is_3_1) {
              df_1 <-
                df %>%
                mutate(countV5 = V5 %>% str_count('\\ ')) %>%
                filter(countV5 == 1) %>%
                select(-c(V2NA, countV5)) %>%
                select(which(colMeans(is.na(.)) < 1))

              df_1 <-
                df_1 %>%
                purrr::set_names(c('idRow', 'nameCompany', 'idTypeInvestment',
                                   'idTicker',
                                   'priceEndOfMonth', 'price52WeekHighLow' ,
                                   'ratioFFOPriceCurrentYearNext',
                                   'pershareFFOCurrentYearNextYear',
                                   'pctFFOGrowth',
                                   'pctDividendYieldSpread'))

              df_1 <-
                df_1 %>%
                separate(price52WeekHighLow, c('price52WeekHigh', 'price52WeekLow'), sep = '\\ ') %>%
                separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
                separate(ratioFFOPriceCurrentYearNext, c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
                separate(pctDividendYieldSpread, c('pctDividendYield', 'pctDividendSpread'), sep = '\\ ') %>%
                suppressWarnings()

              df_2 <-
                df %>%
                mutate(countV5 = V5 %>% str_count('\\ ')) %>%
                filter(!countV5 == 1) %>%
                select(-c(V2NA, countV5)) %>%
                select(which(colMeans(is.na(.)) < 1))

              df_2 <-
                df_2 %>%
                purrr::set_names(c('idRow', 'nameCompany', 'idTypeInvestment',
                                   'idTicker',
                                   'priceEndOfMonth', 'price52WeekLowratioFFOPriceCurrentYearNext' ,
                                   'pershareFFOCurrentYearNextYear',
                                   'pctFFOGrowth',
                                   'pctDividendYieldSpread'))

              df_2 <-
                df_2 %>%
                separate(price52WeekLowratioFFOPriceCurrentYearNext, c('price52WeekLow', 'price52WeekHigh','ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
                separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
                separate(pctDividendYieldSpread, c('pctDividendYield', 'pctDividendSpread'), sep = '\\ ') %>%
                suppressWarnings()

              df <-
                df_1 %>%
                bind_rows(df_2)
              return(df)
            }

            if (is_messed) {
              df <-
                df %>%
                mutate(V2 = ifelse(V2 %>% is.na(), V3, V2),
                       V3 = ifelse(V3 == V2, NA, V3)) %>%
                select(-V2NA) %>%
                select(which(colMeans(is.na(.)) < 1))

              df_no <-
                df %>%
                filter(V2 %>% str_detect("^[0-9]") ) %>%
                select(which(colMeans(is.na(.)) < 1))

              df_no <-
                df_no %>%
                purrr::set_names(c('idRow', 'nameCompanyIdTicker',
                                 'priceEndOfMonth', 'price52WeekHigh' ,
                                 'price52WeekLowratioFFOPriceCurrentYearNext', 'pershareFFOCurrentYearNextYear',
                                 'pctFFOGrowth'))

              df_no <-
                df_no %>%
                separate(nameCompanyIdTicker, c('nameCompany', 'idTicker'), sep =  ' (?=[^ ]+$)') %>%
                mutate(nameCompany = nameCompany %>% str_trim()) %>%
                separate(price52WeekLowratioFFOPriceCurrentYearNext, c('price52WeekLow', 'ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
                separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
                suppressWarnings()


              df_1_space <-
                df %>%
                filter(!V2 %>% str_detect("^[0-9]") ) %>%
                mutate(countV2 = V2 %>% str_count('\\ ')) %>%
                filter(countV2 == 1) %>%
                select(which(colMeans(is.na(.)) < 1)) %>%
                select(-countV2)

              df_space_1 <-
                df_1_space %>%
                filter(!V4 %>% is.na()) %>%
                select(which(colMeans(is.na(.)) < 1)) %>%
                purrr::set_names((list("V", 1:8) %>% purrr::invoke(paste0,.))) %>%
                bind_rows(df_1_space %>%
                filter(V4 %>% is.na()) %>%
                select(which(colMeans(is.na(.)) < 1)) %>%
                purrr::set_names((list("V", 1:8) %>% purrr::invoke(paste0,.))))

              df_space_1 <-
                df_space_1 %>%
                purrr::set_names(c('idRow', 'nameCompany', 'idTypeTicker',
                                   'priceEndOfMonth', 'price52WeekHighLow',
                                   'ratioFFOPriceCurrentYearNext', 'pershareFFOCurrentYearNextYear',
                                   'pctFFOGrowth'))

              df_space_1 <-
                df_space_1 %>%
                separate(idTypeTicker, c('idTypeInvestment', 'idTicker'), sep = '\\ ') %>%
                separate(price52WeekHighLow, c('price52WeekHigh', 'price52WeekLow'), sep = '\\ ') %>%
                separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
                separate(ratioFFOPriceCurrentYearNext, c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
                suppressWarnings()

              df_space_zero <-
                df %>%
                filter(!V2 %>% str_detect("^[0-9]") ) %>%
                mutate(countV2 = V2 %>% str_count('\\ ')) %>%
                filter(countV2 == 0) %>%
                select(which(colMeans(is.na(.)) < 1)) %>%
                select(-countV2)

              df_space_zero <-
                df_space_zero %>%
                purrr::set_names(c('idRow', 'nameCompanyType', 'idTicker',
                                   'priceEndOfMonth', 'price52WeekHighLowratioFFOPriceCurrentYearNext',
                                   'pershareFFOCurrentYearNextYear',
                                   'pctFFOGrowth'))

              df_space_zero <-
                df_space_zero %>%
                separate(nameCompanyType, c('nameCompany', 'idTypeInvestment'), sep =  ' (?=[^ ]+$)') %>%
                mutate(nameCompany = nameCompany %>% str_trim()) %>%
                separate(price52WeekHighLowratioFFOPriceCurrentYearNext, c('price52WeekHigh', 'price52WeekLow', 'ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
                separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
                suppressWarnings()

              df <-
                df_no %>%
                bind_rows(list(df_space_zero, df_space_1)) %>%
                arrange(idRow)

              return(df)

            }

            df_a <-
              df %>%
              mutate(V5Count = V5 %>% str_count('\\ ')) %>%
              filter(V5Count == 3) %>%
              select(-c(V5Count, V2NA)) %>%
              select(which(colMeans(is.na(.)) < 1))

            is_9 <-
              df %>%
              ncol() == 9

            is_8 <-
              df_a %>%
              ncol() == 8

            is_7 <-
              df_a %>%
              ncol() == 7

            if (is_9) {
              df_a <-
              df_a %>%
              purrr::set_names(c('idRow', 'nameCompany', 'idTypeInvestment', 'idTicker',
                                 'priceEndOfMonth', 'price52WeekHighLowFFOMultiple', 'pershareFFOCurrentYearNextYear',
                                 'pctFFOGrowth', 'pctDividendYieldSpread'))

            df_a <-
              df_a %>%
              separate(price52WeekHighLowFFOMultiple, c('price52WeekHigh', 'price52WeekLow', 'ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
              separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
              separate(pctDividendYieldSpread, c('pctDividendYield', 'pctDividendSpread'), sep = '\\ ') %>%
              suppressWarnings() %>%
              suppressMessages()

            }

            if (is_8) {
              df_a <-
                df_a %>%
                purrr::set_names(c('idRow', 'nameCompany', 'idTypeInvestment', 'idTicker',
                                   'priceEndOfMonth', 'price52WeekHighLowFFOMultiple', 'pershareFFOCurrentYearNextYear',
                                   'pctFFOGrowth'))

              df_a <-
                df_a %>%
                separate(price52WeekHighLowFFOMultiple, c('price52WeekHigh', 'price52WeekLow', 'ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
                separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
                suppressWarnings() %>%
                suppressMessages()

              df_b <-
                df %>%
                mutate(V5Count = V5 %>% str_count('\\ ')) %>%
                filter(V5Count < 3) %>%
                select(-c(V5Count, V2NA)) %>%
                select(which(colMeans(is.na(.)) < 1))

              df_b1 <-
                df_b %>%
                filter(!V2 %>% is.na()) %>%
                select(which(colMeans(is.na(.)) < 1)) %>%
                purrr::set_names(c('idRow','nameCompany', 'idTypeInvestment',
                                   'idTicker', 'priceEndOfMonth',
                                   'price52WeekHighLow',
                                   'ratioFFOPriceCurrentYearNext',
                                   'pershareFFOCurrentYearNextYear',
                                   'pctFFOGrowth'))

              df_b1 <-
                df_b1 %>%
                separate(price52WeekHighLow, c('price52WeekHigh', 'price52WeekLow'), sep = '\\ ') %>%
                separate(ratioFFOPriceCurrentYearNext, c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
                separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
                suppressWarnings() %>%
                suppressMessages()

              df_b2 <-
                df_b %>%
                filter(V2 %>% is.na()) %>%
                select(which(colMeans(is.na(.)) < 1)) %>%
                purrr::set_names(c('idRow','nameCompany', 'idTypeInvestment',
                                   'idTicker', 'priceEndOfMonth',
                                   'price52WeekHighLow',
                                   'ratioFFOPriceCurrentYearNext',
                                   'pershareFFOCurrentYearNextYear',
                                   'pctFFOGrowth'))

              df_b2 <-
                df_b2 %>%
                separate(price52WeekHighLow, c('price52WeekHigh', 'price52WeekLow'), sep = '\\ ') %>%
                separate(ratioFFOPriceCurrentYearNext, c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
                separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
                suppressWarnings() %>%
                suppressMessages() %>%
                suppressWarnings()

              df <-
                df_a %>%
                bind_rows(df_b1,
                          df_b2)
              return(df)
            }

            if (is_7) {
              df_a <-
                df_a %>%
                purrr::set_names(c('idRow', 'nameCompany', 'idTicker',
                                   'priceEndOfMonth', 'price52WeekHighLowFFOMultiple',
                                   'pershareFFOCurrentYearNextYear',
                                   'pctFFOGrowth'))

              df_a <-
                df_a %>%
                mutate(idTypeInvestment = nameCompany %>% substr(nchar(nameCompany), nchar(nameCompany)),
                       nameCompany = nameCompany %>% substr(1, nchar(nameCompany) -1) %>% str_trim()) %>%
                separate(price52WeekHighLowFFOMultiple, c('price52WeekHigh', 'price52WeekLow', 'ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
                separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
                suppressWarnings() %>%
                suppressMessages()

              df_b <-
                df %>%
                mutate(V5Count = V5 %>% str_count('\\ ')) %>%
                filter(V5Count < 3) %>%
                select(-c(V5Count, V2NA)) %>%
                select(which(colMeans(is.na(.)) < 1))

              df_b1 <-
                df_b %>%
                filter(!V2 %>% is.na()) %>%
                select(which(colMeans(is.na(.)) < 1))

              is_9 <-
                df_b1 %>% ncol() == 9

              is_8 <-
                df_b1 %>% ncol() == 8
              if (is_9) {
              df_b1 <-
                df_b1 %>%
                purrr::set_names(c('idRow','nameCompany', 'idTypeInvestment',
                                   'idTicker', 'priceEndOfMonth',
                                   'price52WeekHighLow',
                                   'ratioFFOPriceCurrentYearNext',
                                   'pershareFFOCurrentYearNextYear',
                                   'pctFFOGrowth'))
              df_b1 <-
                df_b1 %>%
                separate(price52WeekHighLow, c('price52WeekHigh', 'price52WeekLow'), sep = '\\ ') %>%
                separate(ratioFFOPriceCurrentYearNext, c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
                separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
                suppressWarnings() %>%
                suppressMessages()

              df_b2 <-
                df_b %>%
                filter(V2 %>% is.na()) %>%
                select(which(colMeans(is.na(.)) < 1)) %>%
                purrr::set_names(c('idRow','nameCompany', 'idTypeInvestment',
                                   'idTicker', 'priceEndOfMonth',
                                   'price52WeekHighLow',
                                   'ratioFFOPriceCurrentYearNext',
                                   'pershareFFOCurrentYearNextYear',
                                   'pctFFOGrowth'))

              df_b2 <-
                df_b2 %>%
                separate(price52WeekHighLow, c('price52WeekHigh', 'price52WeekLow'), sep = '\\ ') %>%
                separate(ratioFFOPriceCurrentYearNext, c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
                separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
                suppressWarnings() %>%
                suppressMessages() %>%
                suppressWarnings()

              df <-
                df_a %>%
                bind_rows(df_b1,
                          df_b2)
              }

              if (is_8) {
                df_b1 <-
                  df_b1 %>%
                  purrr::set_names(c('idRow','nameCompany',
                                     'idTypeTicker', 'priceEndOfMonth',
                                     'price52WeekHighLow',
                                     'ratioFFOPriceCurrentYearNext',
                                     'pershareFFOCurrentYearNextYear',
                                     'pctFFOGrowth'))
                df_b1 <-
                  df_b1 %>%
                  filter(!idTicker %>% is.na())
                  separate(idTypeTicker, c('idTypeInvestment', 'idTicker'), sep = '\\ ') %>%
                  separate(price52WeekHighLow, c('price52WeekHigh', 'price52WeekLow'), sep = '\\ ') %>%
                  separate(ratioFFOPriceCurrentYearNext, c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
                  separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
                  suppressWarnings() %>%
                  suppressMessages()

                df_b2 <-
                  df_b %>%
                  filter(V2 %>% is.na()) %>%
                  select(which(colMeans(is.na(.)) < 1)) %>%
                  purrr::set_names(c('idRow','nameCompany',
                                     'idTicker', 'priceEndOfMonth',
                                     'price52WeekHighLow',
                                     'ratioFFOPriceCurrentYearNext',
                                     'pershareFFOCurrentYearNextYear',
                                     'pctFFOGrowth'))

                df_b2 <-
                  df_b2 %>%
                  separate(price52WeekHighLow, c('price52WeekHigh', 'price52WeekLow'), sep = '\\ ') %>%
                  separate(ratioFFOPriceCurrentYearNext, c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
                  separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
                  suppressWarnings() %>%
                  suppressMessages() %>%
                  suppressWarnings()

                df <-
                  df_a %>%
                  bind_rows(df_b1,
                            df_b2)
              }


              return(df)
            }

            df_b <-
              df %>%
              mutate(V5Count = V5 %>% str_count('\\ ')) %>%
              filter(V5Count < 3) %>%
              select(-c(V5Count, V2NA)) %>%
              select(which(colMeans(is.na(.)) < 1))

            df_b <-
              df_b %>%
              purrr::set_names(c('idRow','nameCompany', 'idTypeInvestment',
                                 'idTicker', 'priceEndOfMonth',
                                 'price52WeekHighLow',
                                 'ratioFFOPriceCurrentYearNext',
                                 'pershareFFOCurrentYearNextYear',
                                 'pctFFOGrowth', 'pctDividendYieldSpread'))

            df_b <-
              df_b %>%
              separate(price52WeekHighLow, c('price52WeekHigh', 'price52WeekLow'), sep = '\\ ') %>%
              separate(ratioFFOPriceCurrentYearNext, c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
              separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
              separate(pctDividendYieldSpread, c('pctDividendYield', 'pctDividendSpread'), sep = '\\ ') %>%
              suppressWarnings() %>%
              suppressMessages()

            df <-
              df_a %>%
              bind_rows(df_b)
          }

          if (!has_3) {

           if ('V3' %in% names(df)){
             has_v3_na <-
              df %>% filter(V3 %>% is.na()) %>% nrow() > 0

            if (has_v3_na) {
              df <-
                df %>%
                select(-V2NA) %>%
                filter(!V3 %>% is.na()) %>%
                select(which(colMeans(is.na(.)) < 1)) %>%
                purrr::set_names((list("V", 1:9)) %>% purrr::invoke(paste0, .)) %>%
                bind_rows(
                  df %>%
                    select(-V2NA) %>%
                    filter(V3 %>% is.na()) %>%
                    select(which(colMeans(is.na(
                      .
                    )) < 1)) %>%
                    purrr::set_names((list("V", 1:9)) %>% purrr::invoke(paste0, .))
                ) %>%
                purrr::set_names(
                  c(
                    'idRow',
                    'nameCompany',
                    'idTypeInvestment',
                    'idTicker',
                    'priceEndOfMonth',
                    'price52WeekHighLow',
                    'ratioFFOPriceCurrentYearNext',
                    'pershareFFOCurrentYearNextYear',
                    'pctFFOGrowth'
                  )
                ) %>%
                  separate(price52WeekHighLow, c('price52WeekHigh', 'price52WeekLow'), sep = '\\ ') %>%
                  separate(ratioFFOPriceCurrentYearNext, c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
                  separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ')

              return(df)

            }
           }
            has_na <-
              df %>%
              filter(V2NA == T) %>% nrow() > 0

            if (has_na) {
              df_na <-
                df %>%
                filter(V2NA == T) %>%
                select(which(colMeans(is.na(.)) < 1)) %>%
                select(-V2NA)

              df_na <-
                df_na %>%
                purrr::set_names(c('idRow','nameCompany', 'idTypeInvestment',
                                   'idTicker', 'priceEndOfMonth',
                                   'price52WeekHighLow',
                                   'ratioFFOPriceCurrentYearNext',
                                   'pershareFFOCurrentYearNextYear',
                                   'pctFFOGrowth'))

              df_na <-
                df_na %>%
                separate(price52WeekHighLow, c('price52WeekHigh', 'price52WeekLow'), sep = '\\ ') %>%
                separate(ratioFFOPriceCurrentYearNext, c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
                separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
                suppressWarnings() %>%
                suppressMessages()

              df_no_na <-
                df %>%
                filter(V2NA == F) %>%
                select(which(colMeans(is.na(.)) < 1)) %>%
                select(-V2NA)

              df_no_na <-
                df_no_na %>%
                purrr::set_names(c('idRow','nameCompany', 'idTypeInvestment',
                                   'idTicker', 'priceEndOfMonth',
                                   'price52WeekHighLow',
                                   'ratioFFOPriceCurrentYearNext',
                                   'pershareFFOCurrentYearNextYear',
                                   'pctFFOGrowth'))

              df_no_na <-
                df_no_na %>%
                separate(price52WeekHighLow, c('price52WeekHigh', 'price52WeekLow'), sep = '\\ ') %>%
                separate(ratioFFOPriceCurrentYearNext, c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
                separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
                suppressWarnings() %>%
                suppressMessages()

              df <-
                df_na %>%
                bind_rows(df_no_na) %>%
                arrange(idRow)
              return(df)
            }

            if(!has_na) {
              df <-
                df %>%
                select(which(colMeans(is.na(.)) < 1)) %>%
                select(-V2NA)

              df <-
                df %>%
                group_by(V1) %>%
                filter(idRow == min(idRow)) %>%
                ungroup() %>%
                select(which(colMeans(is.na(.)) < 1))

              df <-
                df %>%
                purrr::set_names(c('idRow','nameCompany', 'idTypeInvestment',
                                   'idTicker', 'priceEndOfMonth',
                                   'price52WeekHighLow',
                                   'ratioFFOPriceCurrentYearNext',
                                   'pershareFFOCurrentYearNextYear',
                                   'pctFFOGrowth', 'pctDividendYieldSpread'))

              df <-
                df %>%
                separate(price52WeekHighLow, c('price52WeekHigh', 'price52WeekLow'), sep = '\\ ') %>%
                separate(ratioFFOPriceCurrentYearNext, c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
                separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
                separate(pctDividendYieldSpread, c('pctDividendYield', 'pctDividendSpread'), sep = '\\ ')

              return(df)
            }
          }

        }

        return(df)
      }) %>%
      arrange(idRow)

    df_data <-
      data_reit %>%
      filter(isNumberData == TRUE) %>%
      select(-isNumberData) %>%
      mutate(idRow = 1:n()) %>%
      select(which(colMeans(is.na(.)) < 1))

    df_data <-
      df_data %>%
      mutate(isV11_1 = ifelse(V11 %>% str_count('\\ ') == 1, TRUE, FALSE)) %>%
      select(isV11_1, everything())

    change_to_ten <-
      df_data %>% filter(V11 %>% is.na()) %>% nrow() > 0

    if (change_to_ten) {
      df_data <-
        df_data %>%
        mutate(V11 = ifelse(V11 %>% is.na(), V10, V11),
               V10 = ifelse(V10 == V11, NA, V10)) %>%
        mutate(isV11_1 = ifelse(V11 %>% str_count('\\ ') == 1, TRUE, FALSE)) %>%
        select(isV11_1, everything())
    }

    df_data$isV11_1[df_data$isV11_1 %>% is.na()] <-
      TRUE

    all_df_data <-
      c(TRUE, FALSE) %>%
      map_df(function(x){
        if (df_data %>%
            filter(isV11_1 == x) %>% nrow() == 0) {
          return(data_frame())
        }

        df <-
          df_data %>%
          filter(isV11_1 == x) %>%
          select(-isV11_1) %>%
          select(which(colMeans(is.na(.)) < 1))

        if (x == TRUE) {
          is_10 <-
            df %>% ncol() == 10

          has_11 <-
            df %>% ncol() == 11

          has_12 <-
            df %>% ncol() == 12

          has_13 <-
            df %>% ncol() == 13

          if (has_13) {
            df <-
              df %>%
              filter(!V13 %>% is.na()) %>%
              select(which(colMeans(is.na(.)) < 1)) %>%
              purrr::set_names(c('idRow','pctDividendYieldSpread',
                                 'pctReturnMonth', 'pctReturnYearToDate', 'pctReturn1YR2YR', 'pctReturn3YR5YR',
                                 'amountEquityMarketCap',
                                 'amountEquityMarketCapDilluted', 'pctDebtEquity','countAVGShareVolumeDollarVolume', 'ratioRelativeLiquidity')) %>%
              separate(pctDividendYieldSpread, c('pctDividendYield', 'pctDividendSpread'), sep = '\\ ') %>%
              separate(pctReturn1YR2YR, c('pctReturn1YR', 'pctReturn2YR'), sep = '\\ ') %>%
              separate(pctReturn3YR5YR, c('pctReturn3YR', 'pctReturn5YR'), sep = '\\ ') %>%
              separate(countAVGShareVolumeDollarVolume, c('countAVGShareVolume', 'countAVGDollarVolume'), sep = '\\ ') %>%
              suppressWarnings() %>%
              suppressMessages() %>%
              bind_rows(
                df %>%
                  filter(V13 %>% is.na()) %>%
                  select(which(colMeans(is.na(.)) < 1)) %>%
                  filter(!V3 %>% is.na()) %>%
                  purrr::set_names(c('idRow','pctDividendYieldSpread',
                                     'pctReturnMonth', 'pctReturnYearToDate1YR2YR', 'pctReturn3YR5YR',
                                     'amountEquityMarketCap',
                                     'amountEquityMarketCapDilluted', 'pctDebtEquity','countAVGShareVolumeDollarVolume', 'ratioRelativeLiquidity')) %>%
                  separate(pctDividendYieldSpread, c('pctDividendYield', 'pctDividendSpread'), sep = '\\ ') %>%
                  separate(pctReturnYearToDate1YR2YR, c('pctReturnYearToDate','pctReturn1YR', 'pctReturn2YR'), sep = '\\ ') %>%
                  separate(pctReturn3YR5YR, c('pctReturn3YR', 'pctReturn5YR'), sep = '\\ ') %>%
                  separate(countAVGShareVolumeDollarVolume, c('countAVGShareVolume', 'countAVGDollarVolume'), sep = '\\ ') %>%
                  suppressWarnings() %>%
                  suppressMessages()
              ) %>%
              bind_rows(df %>%
              filter(V13 %>% is.na()) %>%
              filter(V3 %>% is.na()) %>%
              select(which(colMeans(is.na(.)) < 1)) %>%
              purrr::set_names(c('idRow','pctReturnMonth','pctReturnYearToDate1YR',
                                 'amountEquityMarketCap',
                                 'amountEquityMarketCapDilluted', 'pctDebtEquity','countAVGShareVolumeDollarVolume', 'ratioRelativeLiquidity')) %>%
              separate(pctReturnYearToDate1YR, c('pctReturnYearToDate', 'pctReturn1YR'), sep = '\\ ') %>%
              separate(countAVGShareVolumeDollarVolume, c('countAVGShareVolume', 'countAVGDollarVolume'), sep = '\\ ') %>%
              suppressWarnings() %>%
              suppressMessages())

            return(df)

          }

          if (is_10) {
            df <-
              df %>%
              purrr::set_names(c('idRow','pctDividendYieldSpread',
                                 'pctReturnMonth', 'pctReturnYearToDate1YR2YR', 'pctReturn3YR5YR',
                                 'amountEquityMarketCap',
                                 'amountEquityMarketCapDilluted', 'pctDebtEquity','countAVGShareVolumeDollarVolume', 'ratioRelativeLiquidity'))

            df <-
              df %>%
              separate(pctDividendYieldSpread, c('pctDividendYield', 'pctDividendSpread'), sep = '\\ ') %>%
              separate(pctReturnYearToDate1YR2YR, c('pctReturnYearToDate','pctReturn1YR', 'pctReturn2YR'), sep = '\\ ') %>%
              separate(pctReturn3YR5YR, c('pctReturn3YR', 'pctReturn5YR'), sep = '\\ ') %>%
              separate(countAVGShareVolumeDollarVolume, c('countAVGShareVolume', 'countAVGDollarVolume'), sep = '\\ ') %>%
              suppressWarnings() %>%
              suppressMessages()
            return(df)
          }

          if (has_11) {
            df <-
              df %>%
              purrr::set_names(c('idRow','pctDividendYieldSpread',
                                 'pctReturnMonth', 'pctReturnYearToDate', 'pctReturn1YR2YR', 'pctReturn3YR5YR',
                                 'amountEquityMarketCap',
                                 'amountEquityMarketCapDilluted', 'pctDebtEquity','countAVGShareVolumeDollarVolume', 'ratioRelativeLiquidity'))

            df <-
              df %>%
              separate(pctDividendYieldSpread, c('pctDividendYield', 'pctDividendSpread'), sep = '\\ ') %>%
              separate(pctReturn1YR2YR, c('pctReturn1YR', 'pctReturn2YR'), sep = '\\ ') %>%
              separate(pctReturn3YR5YR, c('pctReturn3YR', 'pctReturn5YR'), sep = '\\ ') %>%
              separate(countAVGShareVolumeDollarVolume, c('countAVGShareVolume', 'countAVGDollarVolume'), sep = '\\ ') %>%
              suppressWarnings() %>%
              suppressMessages()
          }

          if (has_12) {
            is_letter <-
              df$V1 %>% substr(1,1) %>% str_detect("^[A-Z]") %>% sum(na.rm = T) / nrow(df) == 1
            if (is_letter) {
              df <-
                df %>%
                purrr::set_names(c('idRow','nameCompany',
                                   'pctReturnMonth', 'pctReturnYearToDate', 'pctReturn1YR2YR', 'pctReturn3YR',
                                   'pctReturn5YR',
                                   'amountEquityMarketCap',
                                   'amountEquityMarketCapDilluted', 'pctDebtEquity','countAVGShareVolumeDollarVolume', 'ratioRelativeLiquidity'))

              df <-
                df %>%
                separate(pctReturn1YR2YR, c('pctReturn1YR', 'pctReturn2YR'), sep = '\\ ') %>%
                separate(countAVGShareVolumeDollarVolume, c('countAVGShareVolume', 'countAVGDollarVolume'), sep = '\\ ') %>%
                suppressWarnings() %>%
                suppressMessages()
              return(df)
            }
            df <-
              df %>%
              purrr::set_names(c('idRow','pctDividendYieldSpread',
                                 'pctReturnMonth', 'pctReturnYearToDate', 'pctReturn1YR2YR', 'pctReturn3YR',
                                 'pctReturn5YR',
                                 'amountEquityMarketCap',
                                 'amountEquityMarketCapDilluted', 'pctDebtEquity','countAVGShareVolumeDollarVolume', 'ratioRelativeLiquidity'))

            df <-
              df %>%
              separate(pctDividendYieldSpread, c('pctDividendYield', 'pctDividendSpread'), sep = '\\ ') %>%
              separate(pctReturn1YR2YR, c('pctReturn1YR', 'pctReturn2YR'), sep = '\\ ') %>%
              separate(countAVGShareVolumeDollarVolume, c('countAVGShareVolume', 'countAVGDollarVolume'), sep = '\\ ') %>%
              suppressWarnings() %>%
              suppressMessages()
          }
          }

        if (!x == TRUE) {
          is_15 <-
            df %>% ncol() == 15
          is_14 <-
            df %>% ncol() == 14

          is_13 <-
            df %>% ncol() == 13
          if (is_13) {
            df <-
              df %>%
              purrr::set_names(c("idRow", 'pctDividendYieldSpread',"pctReturnMonth", "pctReturnYearToDate", "pctReturn1YR", "pctReturn2YR",
                                 "pctReturn5YR", "amountEquityMarketCap", "amountEquityMarketCapDilluted", "pctDebtEquity",
                                 "countAVGShareVolume", "countAVGDollarVolume", "ratioRelativeLiquidity" )) %>%
              separate(pctDividendYieldSpread, c('pctDividendSpread', 'pctDividendYield'), sep = '\\ ')

            return(df)
          }
          if (is_15) {
            df <-
              df %>%
              mutate(V14NA = ifelse(V14 %>% is.na(), T, F)) %>%
              select(V14NA, everything())

            df_na <-
              df %>%
              filter(V14NA == T) %>%
              select(which(colMeans(is.na(.)) < 1)) %>%
              select(-V14NA)

            df_na <-
              df_na %>%
              purrr::set_names(c("idRow", "pctDividendYieldSpread", "pctReturnMonth",
                                 "pctReturnYearToDate", "pctReturn1YR2YR","pctReturn3YR5YR",
                                 "amountEquityMarketCap", "amountEquityMarketCapDilluted",
                                 "pctDebtEquity", "countAVGShareDollarVolume",
                                 "ratioRelativeLiquidity")
              )

            df_na <-
              df_na %>%
              separate(pctDividendYieldSpread, c('pctDividendYield', 'pctDividendSpread'), sep = '\\ ') %>%
              separate(pctReturn1YR2YR, c('pctReturn1YR', 'pctReturn2YR'), sep = '\\ ') %>%
              separate(pctReturn3YR5YR, c('pctReturn3YR', 'pctReturn5YR'), sep = '\\ ') %>%
              separate(countAVGShareDollarVolume, c('countAVGShareVolume', 'countAVGDollarVolume'), sep = '\\ ') %>%
              suppressWarnings() %>%
              suppressMessages()

            df_no_na <-
              df %>%
              filter(V14NA == F) %>%
              select(which(colMeans(is.na(.)) < 1)) %>%
              select(-V14NA)

            df_no_na <-
              df_no_na %>%
              purrr::set_names(c("idRow", "pctDividendYield", 'pctDividendSpread',"pctReturnMonth",
                                 "pctReturnYearToDate", "pctReturn1YR", "pctReturn2YR","pctReturn3YR",
                                 'pctReturn5YR',
                                 "amountEquityMarketCap", "amountEquityMarketCapDilluted",
                                 "pctDebtEquity", "countAVGShareVolume", 'countAVGDollarVolume',
                                 "ratioRelativeLiquidity"))

            df <-
              df_na %>%
              bind_rows(df_no_na) %>%
              arrange(idRow)
          }

          if (is_14) {

            has_v4_1 <-
              df$V4 %>% str_count(' ') %>% max(na.rm = T) == 1

            if (has_v4_1) {

              not_v13 <-
                df %>%
                filter(!V13 %>% is.na()) %>%
                mutate(countV11 = V11 %>% str_count(' ')) %>%
                filter(countV11 == 1) %>%
                nrow() == 0

              if (not_v13) {
                has_name <-
                  df$V1 %>% substr(1,3) %>% readr::parse_number() %>% sum(na.rm = T) == 0
                if (has_name) {
                  df_2 <-
                    df %>%
                    filter(V7 %>% is.na()) %>%
                    select(which(colMeans(is.na(.)) < 1)) %>%
                    group_by(V1) %>%
                    filter(idRow == min(idRow)) %>%
                    ungroup() %>%
                    select(which(colMeans(is.na(.)) < 1))

                  df_10 <-
                    df_2 %>% ncol() == 10
                  if (df_10) {
                    df_1 <-
                      df %>%
                      filter(!V7 %>% is.na()) %>%
                      select(which(colMeans(is.na(.)) < 1))

                    df_1 <-
                      df_1 %>%
                      purrr::set_names(c("idRow", "nameCompany","pctReturnMonth",
                                         "pctReturnYearToDate", "pctReturn1YR", "pctReturn2YR", "pctReturn3YR",
                                         "pctReturn5YR", "amountEquityMarketCap", "amountEquityMarketCapDilluted",
                                         "pctDebtEquity", "countAVGShareVolume", "countAVGDollarVolume",
                                         "ratioRelativeLiquidity"))


                    df_2 <-
                    df_2 %>%
                    purrr::set_names(c("idRow", "nameCompany", "pctReturnMonth",
                                       "pctReturnYearToDate", "pctReturn1YR2YR", "pctReturn3YR",
                                       'pctReturn5YRamountEquityMarketCap',
                                       "amountEquityMarketCapDillutedpctDebtEquity",
                                       "countAVGShareVolumeDollarVolume",
                                       "ratioRelativeLiquidity"))

                  df_2 <-
                    df_2 %>%
                    separate(pctReturn1YR2YR, c('pctReturn1YR', 'pctReturn2YR'), sep = '\\ ') %>%
                    separate(amountEquityMarketCapDillutedpctDebtEquity, c('amountEquityMarketCapDilluted', 'pctDebtEquity'), sep = '\\ ') %>%
                    separate(pctReturn5YRamountEquityMarketCap, c('pctReturn5YR', 'amountEquityMarketCap'), sep = '\\ ') %>%
                    separate(countAVGShareVolumeDollarVolume, c('countAVGShareVolume', 'countAVGDollarVolume'), sep = '\\ ') %>%
                    suppressWarnings()
                  }
                  df_11 <-
                    df_2 %>% ncol() == 11

                  df_13 <-
                    df_2 %>% ncol() == 13

                  if (df_13) {
                    df_1 <-
                      df %>%
                      filter(!V7 %>% is.na()) %>%
                      select(which(colMeans(is.na(.)) < 1)) %>%
                      group_by(V1) %>%
                      filter(idRow == min(idRow)) %>%
                      ungroup() %>%
                      select(which(colMeans(is.na(.)) < 1)) %>%
                      purrr::set_names(c("idRow", 'nameCompany', "pctReturnMonth",
                                         "pctReturnYearToDate", "pctReturn1YR", "pctReturn2YR", "pctReturn3YR",
                                         "pctReturn5YR", "amountEquityMarketCap", "amountEquityMarketCapDilluted",
                                         "pctDebtEquity", "countAVGShareVolume", "countAVGDollarVolume",
                                         "ratioRelativeLiquidity" ))

                    df_2 <-
                      df_2 %>%
                      filter(!V9 %>% is.na()) %>%
                      select(which(colMeans(is.na(.)) < 1)) %>%
                      purrr::set_names(c("idRow", 'nameCompany', "pctReturnMonth",
                                         "pctReturnYearToDate", "pctReturn1YR", "pctReturn2YR",
                                         "pctReturn5YR", "amountEquityMarketCap", "amountEquityMarketCapDilluted",
                                         "pctDebtEquity", "countAVGShareVolume", "countAVGDollarVolume",
                                         "ratioRelativeLiquidity" )) %>%
                      bind_rows(
                        df_2 %>%
                          filter(V9 %>% is.na()) %>%
                          select(which(colMeans(is.na(.)) < 1)) %>%
                          purrr::set_names(c("idRow", 'nameCompany', "pctReturnMonth",
                                             "pctReturnYearToDate", "pctReturn1YRpctReturn2YR",
                                             "pctReturn3YR", "pctReturn5YRamountEquityMarketCap",
                                             "amountEquityMarketCapDillutedpctDebtEquity", "countAVGShareVolumecountAVGDollarVolume",
                                             "ratioRelativeLiquidity" )) %>%
                          separate(pctReturn1YRpctReturn2YR, c('pctReturn1YR', 'pctReturn2YR'), sep = '\\ ') %>%
                          separate(countAVGShareVolumecountAVGDollarVolume, c('countAVGShareVolume', 'countAVGDollarVolume'), sep = '\\ ') %>%
                          separate(pctReturn5YRamountEquityMarketCap, c('pctReturn5YR', 'amountEquityMarketCap'), sep = '\\ ') %>%
                          separate(amountEquityMarketCapDillutedpctDebtEquity, c('amountEquityMarketCapDilluted', 'pctDebtEquity'), sep = '\\ ')
                      )

                    df <-
                      df_1 %>%
                      bind_rows(df_2)
                    return(df)
                  }

                  if (df_11) {
                    df_1 <-
                      df %>%
                      filter(!V7 %>% is.na()) %>%
                      select(which(colMeans(is.na(.)) < 1))

                    df_1a <-
                      df_1 %>%
                      filter(!V13 %>% is.na()) %>%
                      select(which(colMeans(is.na(.)) < 1))

                    df_1a <-
                      df_1a %>%
                      purrr::set_names(c("idRow", "pctDividendYieldSpread","pctReturnMonth",
                                         "pctReturnYearToDate", "pctReturn1YR", "pctReturn2YR", "pctReturn3YR",
                                         "pctReturn5YR", "amountEquityMarketCap", "amountEquityMarketCapDilluted",
                                         "pctDebtEquity", "countAVGShareVolume", "countAVGDollarVolume",
                                         "ratioRelativeLiquidity"))

                    df_1a <-
                      df_1a %>%
                      separate(pctDividendYieldSpread, c('pctDividendSpread', 'pctDividendYield'), sep = '\\ ')

                    df_1b <-
                      df_1 %>%
                      filter(V13 %>% is.na()) %>%
                      select(which(colMeans(is.na(.)) < 1))

                    df_1b <-
                      df_1b %>%
                      purrr::set_names(c("idRow", "pctDividendYieldSpread", "pctReturnMonth",
                                         "pctReturnYearToDate", "pctReturn1YR2YR", "pctReturn3YR",
                                         'pctReturn5YR',
                                         "amountEquityMarketCap", "amountEquityMarketCapDilluted",
                                         "pctDebtEquity", "countAVGShareVolumeDollarVolume",
                                         "ratioRelativeLiquidity"))

                    df_1b <-
                      df_1b %>%
                      separate(pctDividendYieldSpread, c('pctDividendYield', 'pctDividendSpread'), sep = '\\ ') %>%
                      separate(pctReturn1YR2YR, c('pctReturn1YR', 'pctReturn2YR'), sep = '\\ ') %>%
                      separate(countAVGShareVolumeDollarVolume, c('countAVGShareVolume', 'countAVGDollarVolume'), sep = '\\ ') %>%
                      suppressWarnings()

                    df_2 <-
                      df_2 %>%
                      purrr::set_names(c("idRow", "pctDividendYieldSpread", "pctReturnMonth",
                                         "pctReturnYearToDate", "pctReturn1YR", 'pctReturn2YR', "pctReturn3YR",
                                         'pctReturn5YRamountEquityMarketCap',
                                         "amountEquityMarketCapDillutedpctDebtEquity",
                                         "countAVGShareVolumeDollarVolume",
                                         "ratioRelativeLiquidity")) %>%
                      separate(pctDividendYieldSpread, c('pctDividendSpread', 'pctDividendYield'), sep = '\\ ')

                    df <-
                      df_1a %>%
                      bind_rows(list(df_1b, df_2))
                    return(df)
                  }

                  df <-
                    df_1 %>%
                    bind_rows(df_2)

                  return(df)
                }

                df_1 <-
                  df %>%
                  filter(!V13 %>% is.na()) %>%
                  select(which(colMeans(is.na(.)) < 1))

                df_1 <-
                  df_1 %>%
                  purrr::set_names(c("idRow", "pctDividendYieldSpread","pctReturnMonth",
                    "pctReturnYearToDate", "pctReturn1YR", "pctReturn2YR", "pctReturn3YR",
                    "pctReturn5YR", "amountEquityMarketCap", "amountEquityMarketCapDilluted",
                    "pctDebtEquity", "countAVGShareVolume", "countAVGDollarVolume",
                    "ratioRelativeLiquidity")) %>%
                  separate(pctDividendYieldSpread, c('pctDividendYield', 'pctDividendSpread'), sep = '\\ ')

                df_2 <-
                  df %>%
                  filter(V13 %>% is.na()) %>%
                  select(which(colMeans(is.na(.)) < 1))

                df_2 <-
                  df_2 %>%
                  purrr::set_names(c("idRow", "pctDividendYieldSpread", "pctReturnMonth",
                                     "pctReturnYearToDate", "pctReturn1YR2YR", "pctReturn3YR",
                                     'pctReturn5YR',
                                     "amountEquityMarketCap", "amountEquityMarketCapDilluted",
                                     "pctDebtEquity",
                                     "countAVGShareVolumeDollarVolume",
                                     "ratioRelativeLiquidity"))

                df_2 <-
                  df_2 %>%
                  separate(pctDividendYieldSpread, c('pctDividendYield', 'pctDividendSpread'), sep = '\\ ') %>%
                  separate(pctReturn1YR2YR, c('pctReturn1YR', 'pctReturn2YR'), sep = '\\ ') %>%
                  separate(countAVGShareVolumeDollarVolume, c('countAVGShareVolume', 'countAVGDollarVolume'), sep = '\\ ') %>%
                  suppressWarnings()

                df <-
                  df_1 %>%
                  bind_rows(df_2)
                return(df)
              }

              df_1a <-
                df %>%
                filter(!V13 %>% is.na()) %>%
                mutate(countV11 = V11 %>% str_count(' ')) %>%
                filter(countV11 == 1) %>%
                select(-countV11) %>%
                select(which(colMeans(is.na(.)) < 1))

              df_1a <-
                df_1a %>%
                purrr::set_names(c("idRow", "pctDividendYieldSpread", "pctReturnMonth",
                                   "pctReturnYearToDate", "pctReturn1YR2YR", "pctReturn3YR5YR",
                                   "amountEquityMarketCap", "amountEquityMarketCapDilluted",
                                   "pctDebtEquity", "countAVGShareVolumeDollarVolume",
                                   "ratioRelativeLiquidity"))

              df_1a <-
                df_1a %>%
                separate(pctDividendYieldSpread, c('pctDividendYield', 'pctDividendSpread'), sep = '\\ ') %>%
                separate(pctReturn1YR2YR, c('pctReturn1YR', 'pctReturn2YR'), sep = '\\ ') %>%
                separate(pctReturn3YR5YR, c('pctReturn3YR', 'pctReturn5YR'), sep = '\\ ') %>%
                separate(countAVGShareVolumeDollarVolume, c('countAVGShareVolume', 'countAVGDollarVolume'), sep = '\\ ') %>%
                suppressWarnings()

              df_1b <-
                df %>%
                filter(!V13 %>% is.na()) %>%
                mutate(countV11 = V11 %>% str_count(' ')) %>%
                filter(!countV11 == 1) %>%
                select(-countV11) %>%
                select(which(colMeans(is.na(.)) < 1))

              df_1b <-
                df_1b %>%
                purrr::set_names(c("idRow", "pctDividendYieldSpread","pctReturnMonth",
                                    "pctReturnYearToDate", "pctReturn1YR", "pctReturn2YR", "pctReturn3YR",
                                    "pctReturn5YR", "amountEquityMarketCap", "amountEquityMarketCapDilluted",
                                    "pctDebtEquity", "countAVGShareVolume", "countAVGDollarVolume",
                                    "ratioRelativeLiquidity")
                )

              df_1b <-
                df_1b %>%
                separate(pctDividendYieldSpread, c('pctDividendYield', 'pctDividendSpread'), sep = '\\ ') %>%
                suppressWarnings()

              df_2a <-
                df %>%
                filter(V13 %>% is.na()) %>%
                filter(!V8 %>% is.na()) %>%
                select(which(colMeans(is.na(.)) < 1))

              df_2a <-
                df_2a %>% unite(V3, V3, V4, sep = ' ') %>%
                unite(V3, V3, V5, sep = ' ') %>%
                purrr::set_names(c("idRow", "pctDividendYieldSpread", "pctReturnMonth",
                                   "pctReturnYearToDate1YR2YrYR5YR",
                                   "amountEquityMarketCap", "amountEquityMarketCapDilluted",
                                   "pctDebtEquity", "countAVGShareVolume",
                                   'countAVGDollarVolume',
                                   "ratioRelativeLiquidity"))

              df_2a <-
                df_2a %>%
                separate(pctDividendYieldSpread, c('pctDividendYield', 'pctDividendSpread'), sep = '\\ ') %>%
                separate(pctReturnYearToDate1YR2YrYR5YR, c('pctReturnYearToDate','pctReturn1YR', 'pctReturn2YR',
                                                           'pctReturn3YR', 'pctReturn5YR'), sep = '\\ ') %>%
                suppressWarnings()

              df_2b <-
                df %>%
                filter(V13 %>% is.na()) %>%
                filter(V8 %>% is.na()) %>%
                select(which(colMeans(is.na(.)) < 1))

              df_2b <-
                df_2b %>%
                purrr::set_names(c("idRow", "pctDividendYieldSpread", "pctReturnMonthYearToDate",
                                   "pctReturn1YR",
                                   "amountEquityMarketCap",
                                   "pctDebtEquity",
                                   "countAVGShareVolume",
                                   'countAVGDollarVolume',
                                   "ratioRelativeLiquidity"))

              df_2b <-
                df_2b %>%
                separate(pctDividendYieldSpread, c('pctDividendYield', 'pctDividendSpread'), sep = '\\ ') %>%
                separate(pctReturnMonthYearToDate, c('pctReturnMonth','pctReturnYearToDate'), sep = '\\ ') %>%
                suppressWarnings()

              df <-
                df_1a %>%
                bind_rows(df_1b, df_2a, df_2b) %>%
                mutate(amountEquityMarketCapDilluted = ifelse(amountEquityMarketCapDilluted %>% is.na(), amountEquityMarketCap, amountEquityMarketCapDilluted))

              return(df)
            }

            has_v1_1 <-
              (df$V1 %>% str_count(' ') %>% max(na.rm = T) == 1)

            if (has_v1_1)   {
              is_ok <-
                df$V13 %>% str_detect(".") %>% sum(na.rm = T) /
                df %>% nrow() > .5

              if (is_ok) {
                df <-
                  df %>%
                  purrr::set_names(c("idRow", "pctDividendYieldSpread", "pctReturnMonth",
                                     "pctReturnYearToDate", "pctReturn1YR",  "pctReturn2YR",
                                     "pctReturn3YR", 'pctReturn5YR',"amountEquityMarketCap", "amountEquityMarketCapDilluted",
                                     "pctDebtEquity", "countAVGShareVolume", "countAVGDollarVolume",
                                     "ratioRelativeLiquidity")) %>%
                  separate(pctDividendYieldSpread, c('pctDividendYield', 'pctDividendSpread'), sep = '\\ ')

                return(df)
              }
            }
            is_letter <-
              df$V1 %>% substr(1,1) %>% str_detect("^[A-Z]") %>% sum(na.rm = T) / nrow(df) == 1
            if (is_letter) {
              is_na <-
                df %>%
                filter(V13 %>% is.na()) %>%
                nrow() > 0

              if (is_na) {
                df_1 <-
                  df %>%
                  filter(V13 %>% is.na()) %>%
                  select(which(colMeans(is.na(.)) < 1)) %>%
                  purrr::set_names(
                    c(
                      'idRow',
                      'nameCompany',
                      "pctReturnMonthYearToDate",
                      "pctReturn1YR2YR",
                      'pctReturn3YR',
                      'pctReturn5YR',
                      "amountEquityMarketCap",
                      'amountEquityMarketCapDilluted',
                      "pctDebtEquity",
                      "countAVGShareVolumecountAVGDollarVolume",
                      "ratioRelativeLiquidity"
                    )
                  ) %>%
                  separate(pctReturnMonthYearToDate, c('pctReturnMonth','pctReturnYearToDate'), sep = '\\ ') %>%
                  separate(pctReturn1YR2YR, c('pctReturn1YR','pctReturn2YR'), sep = '\\ ') %>%
                  separate(countAVGShareVolumecountAVGDollarVolume, c('countAVGShareVolume','countAVGDollarVolume'), sep = '\\ ') %>%
                  suppressWarnings()

                df_2 <-
                  df %>%
                  filter(!V13 %>% is.na()) %>%
                  select(which(colMeans(is.na(.)) < 1)) %>%
                  purrr::set_names(c("idRow", "nameCompany", "pctReturnMonth", "pctReturnYearToDate",
                                     "pctReturn1YR", "pctReturn2YR", "pctReturn3YR", "pctReturn5YR",
                                     "amountEquityMarketCap", "amountEquityMarketCapDilluted", "pctDebtEquity",
                                     "countAVGShareVolume", "countAVGDollarVolume", "ratioRelativeLiquidity"
                  )
                  )

                df <-
                  df_1 %>%
                  bind_rows(df_2)
                return(df)
              }

              df <-
                df %>%
                purrr::set_names(c("idRow", "nameCompany","pctReturnMonth",
                                   "pctReturnYearToDate", "pctReturn1YR", "pctReturn2YR", "pctReturn3YR",
                                   "pctReturn5YR", "amountEquityMarketCap", "amountEquityMarketCapDilluted",
                                   "pctDebtEquity", "countAVGShareVolume", "countAVGDollarVolume",
                                   "ratioRelativeLiquidity"))

              return(df)
            }

            has_name <-
              df$V1 %>% as.numeric() %>% is.na() %>% sum() / nrow(df) == 1 %>%
              suppressWarnings()

            if (has_name) {
              no_v7_na <-
                df %>% filter(V7 %>% is.na()) %>% nrow() == 0
              if (no_v7_na) {
                df <-
                df %>%
                purrr::set_names(c("idRow", "nameCompany", "pctReturnMonth",
                                   "pctReturnYearToDate", "pctReturn1YR", "pctReturn2YR", "pctReturn3YR",
                                   "pctReturn5YR", "amountEquityMarketCap", "amountEquityMarketCapDilluted",
                                   "pctDebtEquity", "countAVGShareVolume", "countAVGDollarVolume",
                                   "ratioRelativeLiquidity"))
              }

              if (!no_v7_na) {
               df_na <-
                 df %>%
                  filter(V7 %>% is.na()) %>%
                  select(which(colMeans(is.na(.)) < 1))

               is_10 <-
                 df_na %>% ncol() == 10

               is_13 <-
                 df_na %>% ncol() == 13

               if (is_10) {
               df_na <-
                 df_na %>%
                 purrr::set_names(c('idRow', 'nameCompany',
                                    'pctReturnMonth', 'pctReturnYearToDate', 'pctReturn1YR2Yr',
                                    'pctReturn3YR', 'pctReturn5YRMarketCap', 'amountEquityMarketCapDillutedDebt',
                                    'countAVGShareVolumeDollarVolume', 'ratioRelativeLiquidity'))

               df_na <-
                 df_na %>%
                 separate(pctReturn1YR2Yr, c('pctReturn1YR', 'pctReturn2YR'), sep = '\\ ') %>%
                 separate(pctReturn5YRMarketCap, c('pctReturn5YR', 'amountEquityMarketCap'), sep = '\\ ') %>%
                 separate(amountEquityMarketCapDillutedDebt, c('amountEquityMarketCapDilluted', 'pctDebtEquity'), sep = '\\ ') %>%
                 separate(countAVGShareVolumeDollarVolume, c('countAVGShareVolume', 'countAVGDollarVolume'), sep = '\\ ')

               df_no_na <-
                 df %>%
                 filter(!V7 %>% is.na()) %>%
                 select(which(colMeans(is.na(.)) < 1))

               df_no_na <-
                 df_no_na %>%
                 purrr::set_names(c("idRow", "nameCompany", "pctReturnMonth", "pctReturnYearToDate",
                                    "pctReturn1YR", "pctReturn2YR", "pctReturn3YR", "pctReturn5YR",
                                    "amountEquityMarketCap", "amountEquityMarketCapDilluted", "pctDebtEquity",
                                    "countAVGShareVolume", "countAVGDollarVolume", "ratioRelativeLiquidity"
                 )
                 )

               df <-
                 df_na %>%
                 bind_rows(df_no_na)
               }

               if (is_13) {
                 df_na <-
                   df_na %>%
                   purrr::set_names(c("idRow", "pctDividendYieldSpread", "pctReturnMonth",
                                      "pctReturnYearToDate", "pctReturn1YR",  "pctReturn2YR",
                                      "pctReturn3YR", "amountEquityMarketCap", "amountEquityMarketCapDilluted",
                                      "pctDebtEquity", "countAVGShareVolume", "countAVGDollarVolume",
                                      "ratioRelativeLiquidity"))

                 df_na <-
                   df_na %>%
                   separate(pctDividendYieldSpread, c('pctDividendYield', 'pctDividendSpread'), sep = '\\ ') %>%
                   suppressWarnings() %>%
                   suppressMessages()

                 df_no_na <-
                   df %>%
                   filter(!V7 %>% is.na()) %>%
                   select(which(colMeans(is.na(.)) < 1))

                 df_no_na <-
                   df_no_na %>%
                   purrr::set_names(c("idRow", "pctDividendYieldSpread", "pctReturnMonth",
                     "pctReturnYearToDate", "pctReturn1YR", 'pctReturn2YR' ,"pctReturn3YR",
                     "pctReturn5YR", "amountEquityMarketCap", "amountEquityMarketCapDilluted",
                     "pctDebtEquity", "countAVGShareVolume", "countAVGDollarVolume",
                     "ratioRelativeLiquidity"))

                 df_no_na <-
                   df_no_na %>%
                   separate(pctDividendYieldSpread, c('pctDividendYield', 'pctDividendSpread'), sep = '\\ ')

                 df <-
                   df_na %>%
                   bind_rows(df_no_na)
                 return(df)

               }

               return(df)
              }
            }

            if (!has_name) {
              df <-
                df %>%
                purrr::set_names(c("idRow", "pctDividendYieldSpread", "pctReturnMonth",
                                   "pctReturnYearToDate", "pctReturn1YR", "pctReturn2YR", "pctReturn3YR",
                                   "pctReturn5YR", "amountEquityMarketCap", "amountEquityMarketCapDilluted",
                                   "pctDebtEquity", "countAVGShareVolume", "countAVGDollarVolume",
                                   "ratioRelativeLiquidity")
                )

              df <-
                df %>%
                separate(pctDividendYieldSpread, c('pctDividendYield', 'pctDividendSpread'), sep = '\\ ') %>%
                suppressWarnings() %>%
                suppressMessages()
              return(df)
            }
          }
        }

        return(df)
      }) %>%
      arrange(idRow) %>%
      suppressWarnings()

    all_data <-
        all_descriptions %>%
        left_join(all_df_data %>% select(-matches("nameCompany"))) %>%
        suppressWarnings() %>%
        suppressMessages() %>%
        select(-idRow) %>%
        distinct()

    all_data <-
      all_data %>%
      mutate_at(all_data %>% select(matches(
        "^price|^pershare|^count|^pct|^amount|^ratio"
      )) %>% names(),
      funs(. %>% readr::parse_number())) %>%
      mutate_at(all_data %>% select(matches("^amount")) %>% names(),
                funs(. * 1000000)) %>%
      mutate_at(all_data %>% select(matches("^count")) %>% names(),
                funs(. * 1000)) %>%
      mutate_at(all_data %>% select(matches("^pct")) %>% names(),
                funs((. / 100))) %>%
      mutate_at(all_data %>% select(matches("^count")) %>% names(),
                funs(. %>% comma(digits = 0))) %>%
      mutate_at(all_data %>% select(matches("^price|^pershare")) %>% names(),
                funs(. %>%  currency(digits = 2))) %>%
      mutate_at(all_data %>% select(matches("^amount")) %>% names(),
                funs(.  %>%  currency(digits = 0))) %>%
      suppressWarnings()

    all_data <-
      all_data %>%
      mutate(pct52WeekHigh = (priceEndOfMonth / price52WeekHigh) %>% formattable::percent()) %>%
      select(
        nameCompany:idTicker,
        pct52WeekHigh,
        amountEquityMarketCapDilluted,
        amountEquityMarketCap,
        everything()
      ) %>%
      suppressWarnings() %>%
      mutate(
        amountDebt = (pctDebtEquity * amountEquityMarketCapDilluted) %>% formattable::currency(digits = 0),
        amountEnterpriseValue = ifelse(
          amountDebt %>% is.na(),
          amountEquityMarketCapDilluted,
          amountEquityMarketCapDilluted + amountDebt
        ),
        pctLeverage = (amountDebt / amountEnterpriseValue) %>% formattable::percent(digits = 2)
      ) %>%
      select(
        nameCompany,
        idTicker,
        priceEndOfMonth,
        price52WeekLow,
        pct52WeekHigh,
        amountEnterpriseValue,
        pctLeverage,
        amountDebt,
        everything()
      )

    all_data <-
      all_data %>%
      mutate_at(all_data %>% select(matches("^pct")) %>% names(),
                funs(. %>% formattable::percent(digits = 2)))

    return(all_data)
  }

parse_rw_10 <-
  function(data) {
    data <-
      data %>%
      mutate_all(funs(. %>% str_trim() %>% str_to_upper()))

    start_row_v1 <-
      data %>%
      mutate(idRow = 1:n()) %>%
      filter(V1 %>% str_detect('NAME')) %>%
      slice(1) %>%
      .$idRow + 2

    start_row_v2 <-
      data %>%
      mutate(idRow = 1:n()) %>%
      filter(V2 %>% str_detect('NAME')) %>%
      slice(1) %>%
      .$idRow + 2

    start_row <-
      min(start_row_v1, start_row_v2)

    all_data <-
      data %>%
      filter(!V1 %>% str_detect("AVERAGE")) %>%
      mutate_all(str_to_upper) %>%
      mutate(idRow = 1:n())

    data_reit <-
      all_data %>%
      filter(!((V2 == '' & V3 == '' & V4 == ''))) %>%
      filter(!(V1 == '' & V2 == '' & V3 == '')) %>%
      filter(!V1 %in% c("YIELD SPREAD MONTH", "N.A.", "ONE YEAR:",
                        "TWO YEAR:", "THREE YEAR:", "FIVE YEAR:")) %>%
      filter(!V2 %in% c('REIT NAME')) %>%
      filter(!V6 %in% c('ESTIMATES')) %>%
      filter(!V7 %in% c('ESTIMATES')) %>%
      filter(!V8 %in% c('ESTIMATES')) %>%
      filter(!V1 %in% c('REIT NAME', 'DIVIDEND', 'YIELD SPREAD', 'REIT NAME TYPE')) %>%
      filter(
        !V3 %in% c(
          'ESTIMATES',
          'PRICE PER SHARE',
          'DIVERSIFIED',
          'TOTAL RETURN',
          'LODGING/RESORTS',
          'HYBRID',
          'SELF STORAGE',
          'DIVERSIFIED'
        )
      ) %>%
      filter(
        !V4 %in% c(
          'ESTIMATES',
          'PRICE PER SHARE',
          'DIVERSIFIED',
          'TOTAL RETURN',
          'LODGING/RESORTS',
          'SELF STORAGE',
          'DIVERSIFIED'
        )
      ) %>%
      filter(
        !V5 %in% c(
          'ESTIMATES',
          'PRICE PER SHARE',
          'DIVERSIFIED',
          'TOTAL RETURN',
          'LODGING/RESORTS',
          'SELF STORAGE',
          'DIVERSIFIED'
        )
      ) %>%
      filter(
        !V6 %in% c(
          'ESTIMATES',
          'PRICE PER SHARE',
          'DIVERSIFIED',
          'TOTAL RETURN',
          'LODGING/RESORTS',
          'SELF STORAGE',
          'DIVERSIFIED'
        )
      ) %>%
      filter(
        !V6 %in% c(
          'ESTIMATES',
          'PRICE PER SHARE',
          'DIVERSIFIED',
          'TOTAL RETURN',
          'LODGING/RESORTS',
          'SELF STORAGE',
          'DIVERSIFIED'
        )
      ) %>%
      mutate(isNumberData = ifelse(!V1 %>% substr(1, 2) %>% as.numeric() %>% is.na(),
                                   TRUE, FALSE)) %>%
      select(idRow, isNumberData, everything()) %>%
      suppressWarnings() %>%
      filter(!V3 == 'YEAR') %>%
      mutate_all(funs(ifelse(. == '', NA, .))) %>%
      filter(!(V1 %>% is.na() & V8 %>% is.na())) %>%
      filter(!V1 %>% is.na()) %>%
      mutate(isNumberData = ifelse(V1 %>% is.na(), TRUE, isNumberData))

    if (data_reit %>% mutate(V2char = V2 %>% nchar()) %>%
        filter(V2char == 1) %>% nrow() > 0) {
      is_id_type <-
        data_reit %>% mutate(V2char = V2 %>% nchar()) %>%
        filter(V2char == 1) %>% nrow() / nrow(data_reit) > .25
      if (is_id_type) {
        data_reit <-
          data_reit %>%
          mutate(isNumberData = ifelse(V2 %>% nchar() <= 1, FALSE, TRUE))
      } else {
        is_id_type <-
          FALSE
      }
    } else {
      is_id_type <-
        F
    }

    data_reit <-
      data_reit %>%
      mutate(isNumberData = ifelse(V1 %>% substr(1,2) %>% readr::parse_number %>% is.na(), F, T)) %>%
      suppressMessages() %>%
      suppressWarnings()

    is_mixed <-
      data_reit %>% mutate(V2 = V2 %>% readr::parse_number) %>% filter(!V2 %>% is.na()) %>% nrow() / nrow(data_reit) > .1

    if (is_mixed) {
      data_reit <-
        data_reit %>%
        mutate(isNumberData = ifelse(!V2 %>% readr::parse_number() %>% is.na(), T, F),
               isNumberData = ifelse((V2 %>% is.na()) & (V3 %>% is.na()) & (V4 %>% is.na()),
                                     TRUE, isNumberData
               ),
               isNumberData = ifelse((V2 == "NA") & (V3 == "NA"),
                                     TRUE, isNumberData
               ))
    }


    df_descriptions <-
      data_reit %>%
      filter(isNumberData == FALSE) %>%
      select(which(colMeans(is.na(.)) < 1)) %>%
      mutate(idRow = 1:n())

    if (!'V2' %in% names(df_descriptions)) {
      df_descriptions <-
        df_descriptions %>%
        dplyr::rename(V2 = V3)
    }

    df_descriptions <-
      df_descriptions %>%
      mutate(isV2NA = ifelse(V2 %>% is.na(), TRUE, FALSE)) %>%
      select(-isNumberData) %>%
      select(isV2NA, everything()) %>%
      select(which(colMeans(is.na(.)) < 1))

    all_descriptions <-
      c(TRUE, FALSE) %>%
      map_df(function(x) {
            df <-
              df_descriptions %>%
              filter(isV2NA == x) %>%
              select(-isV2NA) %>%
              select(which(colMeans(is.na(.)) < 1))

            is_new_8 <-
              (data %>% ncol() == 8 & df %>% ncol() == 9)

            if (is_new_8) {
                df_1 <-
                  df %>%
                  filter(!V8 %>% is.na()) %>%
                  select(which(colMeans(is.na(.)) < 1)) %>%
                  mutate(countV8 = V8 %>% str_count('\\ ')) %>%
                  filter(countV8 == 0) %>%
                  select(-countV8) %>%
                purrr::set_names(
                  c(
                    'idRow',
                    'nameCompany',
                    'idTypeInvestment',
                    'idTicker',
                    'priceEndOfMonth',
                    'price52WeekHighLowratioFFOPriceCurrentYearNext',
                    'pershareFFOCurrentYearNextYear',
                    'pctFFOGrowthDividendYield',
                    'pctDividendSpread'
                  )
                ) %>%
                separate(price52WeekHighLowratioFFOPriceCurrentYearNext, c('price52WeekHigh', 'price52WeekLow', 'ratioFFOPriceCurrent',
                                                                           'ratioFFOPriceYearNext'), sep = '\\ ') %>%
                separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
                separate(pctFFOGrowthDividendYield, c('pctFFOGrowth', 'pctDividendYield'), sep = '\\ ') %>%
                bind_rows(
                  df %>%
                    filter(!V8 %>% is.na()) %>%
                    select(which(colMeans(is.na(.)) < 1)) %>%
                    mutate(countV8 = V8 %>% str_count('\\ ')) %>%
                    filter(countV8 >= 1) %>%
                    select(-countV8) %>%
                    select(which(colMeans(is.na(.)) < 1)) %>%
                    purrr::set_names(
                      c(
                        'idRow',
                        'nameCompany',
                        'idTypeInvestment',
                        'idTicker',
                        'priceEndOfMonth',
                        'price52WeekHighLow',
                        'ratioFFOPriceCurrentYearNext',
                        'pershareFFOCurrentYearNextYear',
                        'pctFFOGrowthDividendYieldpctDividendSpread'
                      )
                    ) %>%
                    separate(price52WeekHighLow, c('price52WeekHigh', 'price52WeekLow'), sep = '\\ ') %>%
                    separate(ratioFFOPriceCurrentYearNext, c('ratioFFOPriceCurrent',
                                                   'ratioFFOPriceYearNext'), sep = '\\ ') %>%
                    separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
                    separate(pctFFOGrowthDividendYieldpctDividendSpread, c('pctFFOGrowth', 'pctDividendYield', 'pctDividendSpread'), sep = '\\ ')
                )

                  df_2 <-
                    df %>%
                    filter(V8 %>% is.na()) %>%
                    select(which(colMeans(is.na(.)) < 1)) %>%
                    mutate(countV4 = V4 %>% str_count('\\ ')) %>%
                    filter(countV4 == 0) %>%
                    select(-countV4) %>%
                      purrr::set_names(
                        c(
                          'idRow',
                          'nameCompany',
                          'idTypeInvestment',
                          'idTicker',
                          'priceEndOfMonth',
                          'price52WeekHighLowratioFFOPriceCurrentYearNext',
                          'pershareFFOCurrentYearNextYear',
                          'pctFFOGrowthDividendYieldpctDividendSpread'
                        )
                      ) %>%
                      separate(price52WeekHighLowratioFFOPriceCurrentYearNext, c('price52WeekHigh', 'price52WeekLow', 'ratioFFOPriceCurrent',
                                                                                 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
                      separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
                      separate(pctFFOGrowthDividendYieldpctDividendSpread, c('pctFFOGrowth', 'pctDividendYield', 'pctDividendSpread'), sep = '\\ ') %>%
                        bind_rows(
                          df %>%
                            filter(V8 %>% is.na()) %>%
                            select(which(colMeans(is.na(.)) < 1)) %>%
                            mutate(countV4 = V4 %>% str_count('\\ ')) %>%
                            filter(countV4 > 0) %>%
                            select(-countV4) %>%
                            select(which(colMeans(is.na(.)) < 1)) %>%
                            purrr::set_names(
                            c(
                              'idRow',
                              'nameCompany',
                              'idTypeInvestment',
                              'idTicker',
                              'priceEndOfMonthprice52WeekHighLowratioFFOPriceCurrentYearNext',
                              'pershareFFOCurrentYearNextYear',
                              'pctFFOGrowthDividendYield',
                              'pctDividendSpread'
                            )
                          ) %>%
                            separate(priceEndOfMonthprice52WeekHighLowratioFFOPriceCurrentYearNext, c('priceEndOfMonth','price52WeekHigh', 'price52WeekLow', 'ratioFFOPriceCurrent',
                                                                                       'ratioFFOPriceYearNext'), sep = '\\ ') %>%
                            separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
                            separate(pctFFOGrowthDividendYield, c('pctFFOGrowth', 'pctDividendYield'), sep = '\\ ')
                        )

                  df <-
                    df_1 %>%
                    bind_rows(df_2)
                  return(df)
            }

            is_8_1 <-
              df[,3] %>% magrittr::extract2(1) %>% nchar() %>% max(na.rm = T) == 1 &
              df %>% ncol() == 8

            if (is_8_1) {
              df <-
                df %>%
                purrr::set_names(c(
                  'idRow',
                  'nameCompany',
                  'idTypeInvestment',
                  'idExchangeTicker',
                  'priceMonth52Week',
                  'ratioFFOHighLow',
                  'pershareFFOCurrentYearNextYear',
                  'pctFFOGrowth'
                )) %>%
                separate(idExchangeTicker, c('idExchange', 'idTicker'), sep = '\\ ') %>%
                separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
                separate(
                  priceMonth52Week,
                  c(
                    'priceEndOfMonth',
                    'price52WeekHigh',
                    'price52WeekLow'
                  ),
                  sep = '\\ '
                ) %>%
                separate(ratioFFOHighLow, c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
                suppressWarnings() %>%
                suppressMessages()
              return(df)
            }

            is_new_8_2 <-
              (df$V4 %>% str_count("\\ ") %>% unique() %>% paste0(collapse = ":")  == "2:4:3" & df %>% ncol() == 8)

            if (is_new_8_2) {
              df <-
                df %>%
                mutate(countV4 = V4 %>% str_count('\\ '))

              df <-
                df %>%
                filter(countV4 > 2) %>%
                select(-countV4) %>%
                select(which(colMeans(is.na(.)) < 1)) %>%
                purrr::set_names(c('idRow', 'nameCompany', 'idTypeInvestment',
                                   'idTicker', 'priceEndOfMonth52WeekHighLowratioFFOPriceCurrentYearNext',  'pershareFFOCurrentYearNextYear', 'pctFFOGrowthYield')) %>%
                separate(priceEndOfMonth52WeekHighLowratioFFOPriceCurrentYearNext, c('priceEndOfMonth', 'price52WeekHigh', 'price52WeekLow', 'ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
                separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
                separate(pctFFOGrowthYield, c('pctFFOGrowth', 'pctDividendYield'), sep = '\\ ') %>%
                bind_rows(
                  df %>%
                    filter(countV4 == 2) %>%
                    select(-countV4) %>%
                    purrr::set_names(c('idRow', 'nameCompany', 'idTypeInvestment',
                                       'idTicker', 'priceEndOfMonth52WeekHighLow', 'ratioFFOPriceCurrentYearNext', 'pershareFFOCurrentYearNextYear', 'pctFFOGrowthYield')) %>%
                    separate(ratioFFOPriceCurrentYearNext, c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
                    separate(priceEndOfMonth52WeekHighLow, c('priceEndOfMonth', 'price52WeekHigh', 'price52WeekLow'), sep = '\\ ') %>%
                    separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
                    separate(pctFFOGrowthYield, c('pctFFOGrowth', 'pctDividendYield'), sep = '\\ ')
                )

              return(df)
            }

            is_6 <-
              df %>% ncol() == 6

            is_7 <-
              df %>% ncol() == 7

            is_8 <-
              df %>% ncol() == 8

            is_9 <-
              df %>% ncol == 9

            is_10 <-
              df %>% ncol == 10

            is_14 <-
              df %>% ncol() == 14

            if (is_6) {
              df <-
                df %>%
                purrr::set_names(c('idRow', 'nameCompany',
                                   'idTypeExchangeTicker',
                                   'all',
                                   'pershareFFOCurrentYearNextYear',
                                   'pctFFOGrowth'))

              df <-
                df %>%
                separate(idTypeExchangeTicker, c('idTypeInvestment', 'idExchange', 'idTicker'), sep = '\\ ') %>%
                separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
                separate(all, c('priceEndOfMonth', 'price52WeekHigh', 'price52WeekLow', 'ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
                suppressWarnings()

              return(df)

            }

            is_7_7 <-
              df %>% ncol() == 7 &
              df[,3] %>% magrittr::extract2(1) %>% nchar() %>% max(na.rm = T) == 1

            if (is_7_7) {
              df <-
                df %>%
                purrr::set_names(c(
                  'idRow',
                  'nameCompany',
                  'idTypeInvestment',
                  'idExchangeTicker',
                  'priceMonth52WeekHighLowRatioFFO',
                  'pershareFFOCurrentYearNextYear',
                  'pctFFOGrowth'
                )) %>%
                separate(idExchangeTicker, c('idExchange', 'idTicker'), sep = '\\ ') %>%
                separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
                separate(
                  priceMonth52WeekHighLowRatioFFO,
                  c(
                    'priceEndOfMonth',
                    'price52WeekHigh',
                    'price52WeekLow',
                    'ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'
                  ),
                  sep = '\\ '
                ) %>%
                suppressWarnings() %>%
                suppressMessages()
              return(df)
            }

            if (is_7) {
              if ('V5' %in% names(df)) {
              has_v5_2 <-
                df$V5 %>% str_count('\\ ') %>% max(na.rm = TRUE) == 2

              if (has_v5_2) {
                df <-
                  df %>%
                  purrr::set_names(
                  c(
                    'idRow',
                    'nameCompany',
                    'idTypeInvestmentTicker',
                    'priceMonth52WeekHighLow',
                    'ratioFFOPriceCurrentYearNext',
                    'pershareFFOCurrentYearNextYear',
                    'pctFFOGrowth'
                  )
                )

                df <-
                  df %>%
                  separate(idTypeInvestmentTicker, c('idTypeInvestment','idTicker'), sep = '\\ ') %>%
                  separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
                  separate(
                    priceMonth52WeekHighLow,
                    c(
                      'priceEndOfMonth',
                      'price52WeekHigh',
                      'price52WeekLow'
                    ),
                    sep = '\\ '
                  ) %>%
                  separate(
                    ratioFFOPriceCurrentYearNext,
                    c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'),
                    sep = '\\ '
                  ) %>%
                  suppressWarnings() %>%
                  suppressMessages()
                return(df)
              }

              has_v5_4 <-
                df$V5 %>% str_count('\\ ') %>% max(na.rm = TRUE) == 4

              if (has_v5_4) {
                df_1 <-
                  df %>%
                  mutate(V5Count = V5 %>% str_count('\\ ')) %>%
                  filter(V5Count == 4) %>%
                  select(which(colMeans(is.na(.)) < 1)) %>%
                  select(-V5Count)

                df_1 <-
                  df_1 %>%
                  purrr::set_names(
                    c(
                      'idRow',
                      'nameCompany',
                      'idTypeInvestmentExchangeTicker',
                      'priceMonth52WeekHighLowratioFFOPriceCurrentYearNext',
                      'pershareFFOCurrentYearNextYear',
                      'pctFFOGrowth'
                    )
                  ) %>%
                  separate(idTypeInvestmentExchangeTicker, c('idTypeInvestment', 'idExchange','idTicker'), sep = '\\ ') %>%
                  separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
                  separate(
                    priceMonth52WeekHighLowratioFFOPriceCurrentYearNext,
                    c(
                      'priceEndOfMonth',
                      'price52WeekHigh',
                      'price52WeekLow',
                      'ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'
                    ),
                    sep = '\\ '
                  ) %>%
                  suppressWarnings() %>%
                  suppressMessages()

                df_2 <-
                  df %>%
                  mutate(V5Count = V5 %>% str_count('\\ ')) %>%
                  filter(V5Count == 2) %>%
                  select(which(colMeans(is.na(.)) < 1)) %>%
                  select(-V5Count) %>%
                  purrr::set_names(
                    c(
                      'idRow',
                      'nameCompany',
                      'idTypeInvestmentExchangeTicker',
                      'priceMonth52WeekHighLow',
                      'ratioFFOPriceCurrentYearNext',
                      'pershareFFOCurrentYearNextYear',
                      'pctFFOGrowth'
                    )
                  ) %>%

                  separate(idTypeInvestmentExchangeTicker, c('idTypeInvestment', 'idExchange','idTicker'), sep = '\\ ') %>%
                  separate(ratioFFOPriceCurrentYearNext, c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
                  separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
                  separate(
                    priceMonth52WeekHighLow,
                    c(
                      'priceEndOfMonth',
                      'price52WeekHigh',
                      'price52WeekLow'

                    ),
                    sep = '\\ '
                  ) %>%
                  suppressWarnings() %>%
                  suppressMessages()

                has_3 <-
                  df %>%
                  mutate(V5Count = V5 %>% str_count('\\ ')) %>%
                  filter(V5Count == 3) %>% nrow() > 3

                if (has_3) {
                  df_3 <-
                  df %>%
                  mutate(V5Count = V5 %>% str_count('\\ ')) %>%
                  filter(V5Count == 3) %>%
                  select(which(colMeans(is.na(.)) < 1)) %>%
                  select(-V5Count) %>%
                  purrr::set_names(
                    c(
                      'idRow',
                      'nameCompany',
                      'idTypeInvestmentExchangeTicker',
                      'priceMonth52WeekHighLowratioFFOPriceCurrentYearNext',
                      'pctFFOGrowth'
                    )
                  ) %>%
                  separate(idTypeInvestmentExchangeTicker, c('idTypeInvestment', 'idExchange','idTicker'), sep = '\\ ') %>%
                  separate(
                    priceMonth52WeekHighLowratioFFOPriceCurrentYearNext,
                    c(
                      'priceEndOfMonth',
                      'price52WeekHigh',
                      'price52WeekLow',
                      'ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'
                    ),
                    sep = '\\ '
                  ) %>%
                  suppressMessages()

                df <-
                  df_1 %>%
                  bind_rows(list(df_2, df_3))
                return(df)
                }
                df <-
                  df_1 %>%
                  bind_rows(df_2)

                return(df)
              }

              }

              if ('V4' %in% names(df)) {
                has_v4_2 <-
                  df$V4 %>% str_count('\\ ') %>% max(na.rm = TRUE) == 2

                if (has_v4_2) {
                  df <-
                    df %>%
                    purrr::set_names(
                      c(
                        'idRow',
                        'nameCompany',
                        'idTypeInvestmentTicker',
                        'priceMonth52WeekHighLow',
                        'ratioFFOPriceCurrentYearNext',
                        'pershareFFOCurrentYearNextYear',
                        'pctFFOGrowth'
                      )
                    )

                  df <-
                    df %>%
                    separate(idTypeInvestmentTicker, c('idTypeInvestment','idTicker'), sep = '\\ ') %>%
                    separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
                    separate(
                      priceMonth52WeekHighLow,
                      c(
                        'priceEndOfMonth',
                        'price52WeekHigh',
                        'price52WeekLow'
                      ),
                      sep = '\\ '
                    ) %>%
                    separate(
                      ratioFFOPriceCurrentYearNext,
                      c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'),
                      sep = '\\ '
                    ) %>%
                    suppressWarnings() %>%
                    suppressMessages()
                  return(df)
                }

              }

              has_v2 <-
                'V2' %in% names(df)


              if (has_v2){
              is_2 <-
                df$V2 %>% str_count('\\ ') %>% max(na.rm = T) >= 2
              }

              if ((!has_v2) & ('V1' %in% names(df))) {
                is_v1 <-
                  df$V1 %>% str_count('\\ ') %>% max() >= 2
              } else {
                is_v1 <-
                  FALSE
              }
              if (has_v2) {
              is_special_v2 <-
                df %>% mutate(countV2 = V2 %>% str_count('\\ ')) %>%
                filter(countV2 >= 2) %>% nrow() / nrow(df) > .75
              if (is_special_v2) {
                df_1 <-
                  df %>%
                  mutate(countV4 = V4 %>% str_count('\\ ')) %>%
                  filter(countV4 == 2) %>%
                  select(-countV4) %>%
                  select(which(colMeans(is.na(.)) < 1)) %>%
                  purrr::set_names(
                    c(
                      'idRow',
                      'nameCompany',
                      'idTypeInvestmentExchangeTicker',
                      'priceMonth52WeekHighLow',
                      'ratioFFOPriceCurrentYearNext',
                      'pershareFFOCurrentYearNextYear',
                      'pctFFOGrowth'
                    )
                  )

                df_1 <-
                  df_1 %>%
                  separate(idTypeInvestmentExchangeTicker, c('idTypeInvestment', 'idExchange','idTicker'), sep = '\\ ') %>%
                  separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
                  separate(
                    priceMonth52WeekHighLow,
                    c(
                      'priceEndOfMonth',
                      'price52WeekHigh',
                      'price52WeekLow'
                    ),
                    sep = '\\ '
                  ) %>%
                  separate(
                    ratioFFOPriceCurrentYearNext,
                    c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'),
                    sep = '\\ '
                  ) %>%
                  suppressWarnings() %>%
                  suppressMessages()

                df_2 <-
                  df %>%
                  mutate(countV4 = V4 %>% str_count('\\ ')) %>%
                  filter(countV4 > 2) %>%
                  select(-countV4) %>%
                  select(which(colMeans(is.na(.)) < 1)) %>%
                  purrr::set_names(
                    c(
                      'idRow',
                      'nameCompany',
                      'idTypeInvestmentExchangeTicker',
                      'priceMonth52WeekHighLowratioFFOPriceCurrentYearNext',
                      'pershareFFOCurrentYearNextYear',
                      'pctFFOGrowth'
                    )
                  )

                df_2 <-
                  df_2 %>%
                  separate(idTypeInvestmentExchangeTicker, c('idTypeInvestment', 'idExchange','idTicker'), sep = '\\ ') %>%
                  separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
                  separate(
                    priceMonth52WeekHighLowratioFFOPriceCurrentYearNext,
                    c(
                      'priceEndOfMonth',
                      'price52WeekHigh',
                      'price52WeekLow',
                      'ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'
                    ),
                    sep = '\\ '
                  ) %>%
                  suppressWarnings() %>%
                  suppressMessages()

                df <-
                  df_1 %>%
                  bind_rows(df_2)
                return(df)
              }

              is_special_2 <-
                df %>% mutate(countV2 = V2 %>% str_count('\\ ')) %>%
                filter(countV2 == 1) %>% nrow() / nrow(df) > .75

              if (is_special_2) {
                df_1 <-
                  df %>%
                  mutate(countV4 = V4 %>% str_count('\\ ')) %>%
                  filter(countV4 == 2) %>%
                  select(-countV4) %>%
                  select(which(colMeans(is.na(.)) < 1)) %>%
                  purrr::set_names(
                    c(
                      'idRow',
                      'nameCompany',
                      'idTypeInvestmentTicker',
                      'priceMonth52WeekHighLow',
                      'ratioFFOPriceCurrentYearNext',
                      'pershareFFOCurrentYearNextYear',
                      'pctFFOGrowth'
                    )
                  )

                df_1 <-
                  df_1 %>%
                  separate(idTypeInvestmentTicker, c('idTypeInvestment', 'idTicker'), sep = '\\ ') %>%
                  separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
                  separate(
                    priceMonth52WeekHighLow,
                    c(
                      'priceEndOfMonth',
                      'price52WeekHigh',
                      'price52WeekLow'
                    ),
                    sep = '\\ '
                  ) %>%
                  separate(
                    ratioFFOPriceCurrentYearNext,
                    c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'),
                    sep = '\\ '
                  ) %>%
                  suppressWarnings() %>%
                  suppressMessages()

                df_2 <-
                  df %>%
                  mutate(countV4 = V4 %>% str_count('\\ ')) %>%
                  filter(countV4 > 2) %>%
                  select(-countV4) %>%
                  select(which(colMeans(is.na(.)) < 1)) %>%
                  purrr::set_names(
                    c(
                      'idRow',
                      'nameCompany',
                      'idTypeInvestmentTicker',
                      'priceMonth52WeekHighLowratioFFOPriceCurrentYearNext',
                      'pershareFFOCurrentYearNextYear',
                      'pctFFOGrowth'
                    )
                  )

                df_2 <-
                  df_2 %>%
                  separate(idTypeInvestmentTicker, c('idTypeInvestment', 'idTicker'), sep = '\\ ') %>%
                  separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
                  separate(
                    priceMonth52WeekHighLowratioFFOPriceCurrentYearNext,
                    c(
                      'priceEndOfMonth',
                      'price52WeekHigh',
                      'price52WeekLow',
                      'ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'
                    ),
                    sep = '\\ '
                  ) %>%
                  suppressWarnings() %>%
                  suppressMessages()

                df <-
                  df_1 %>%
                  bind_rows(df_2)
                return(df)
              }

              }

              if (is_v1) {
                df <-
                  df %>%
                  mutate(V5Count = V5 %>% str_count('\\ '))

                df_1 <-
                  df %>%
                  filter(V5Count == 2) %>%
                  select(-V5Count) %>%
                  select(which(colMeans(is.na(.)) < 1))

                df_2 <-
                  df %>%
                  filter(V5Count > 2) %>%
                  select(-V5Count) %>%
                  select(which(colMeans(is.na(.)) < 1))
                is_5 <-
                  df_1 %>% ncol() == 5

                if (is_5) {
                  df_1 <-
                    df_1 %>%
                    purrr::set_names(
                      c(
                        'idRow',
                        'nameCompany',
                        'idTypeInvestment',
                        'idExchangeTicker',
                        'priceMonth52WeekHigh'
                      )
                    )

                  df_1 <-
                    df_1 %>%
                    separate(idExchangeTicker, c('idExchange', 'idTicker'), sep = '\\ ') %>%
                    separate(
                      priceMonth52WeekHigh,
                      c(
                        'priceEndOfMonth',
                        'price52WeekHigh',
                        'price52WeekLow'
                      ),
                      sep = '\\ '
                    ) %>%
                    suppressWarnings() %>%
                    suppressMessages()

                  df_2 <-
                    df_2 %>%
                    purrr::set_names(
                      c(
                        'idRow',
                        'nameCompany',
                        'idTypeInvestment',
                        'idExchangeTicker',
                        'priceMonth52WeekHighLowRatioFFO',
                        'pershareFFOCurrentYearNextYear',
                        'pctFFOGrowth'
                      )
                    )

                  df_2 <-
                    df_2 %>%
                    separate(idExchangeTicker, c('idExchange', 'idTicker'), sep = '\\ ') %>%
                    separate(
                      priceMonth52WeekHighLowRatioFFO,
                      c(
                        'priceEndOfMonth',
                        'price52WeekHigh',
                        'price52WeekLow',
                        'ratioFFOPriceCurrent',
                        'ratioFFOPriceYearNext'
                      ),
                      sep = '\\ '
                    ) %>%
                    separate(
                      pershareFFOCurrentYearNextYear,
                      c('pershareFFOCurrentYear', 'pershareFFONextYear'),
                      sep = '\\ '
                    ) %>%
                    suppressWarnings() %>%
                    suppressMessages()

                  df <-
                    df_1 %>%
                    bind_rows(df_2)

                  return(df)
                }

                df_1 <-
                  df_1 %>%
                  purrr::set_names(
                    c(
                      'idRow',
                      'nameCompany',
                      'idTypeInvestmentTicker',
                      'priceMonth52WeekHighLow',
                      'ratioFFOPriceCurrentYearNext',
                      'pershareFFOCurrentYearNextYear',
                      'pctFFOGrowth'
                    )
                  )

                df_1 <-
                  df_1 %>%
                  separate(idTypeInvestmentTicker, c('idTypeInvestment','idTicker'), sep = '\\ ') %>%
                  separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
                  separate(
                    priceMonth52WeekHighLow,
                    c(
                      'priceEndOfMonth',
                      'price52WeekHigh',
                      'price52WeekLow'
                    ),
                    sep = '\\ '
                  ) %>%
                  separate(
                    ratioFFOPriceCurrentYearNext,
                    c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'),
                    sep = '\\ '
                  ) %>%
                  suppressWarnings() %>%
                  suppressMessages()

                df_2 <-
                  df_2 %>%
                  purrr::set_names(
                    c(
                      'idRow',
                      'nameCompany',
                      'idTypeInvestmentTicker',
                      'priceMonth52WeekHighLowratioFFOPriceCurrentYearNext',
                      'pershareFFOCurrentYearNextYear',
                      'pctFFOGrowth'
                    )
                  )

                df_2 <-
                  df_2 %>%
                  separate(idTypeInvestmentTicker, c('idTypeInvestment','idTicker'), sep = '\\ ') %>%
                  separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
                  separate(
                    priceMonth52WeekHighLowratioFFOPriceCurrentYearNext,
                    c(
                      'priceEndOfMonth',
                      'price52WeekHigh',
                      'price52WeekLow',
                      'ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'
                    ),
                    sep = '\\ '
                  ) %>%
                  suppressWarnings() %>%
                  suppressMessages()


                df <-
                  df_1 %>%
                  bind_rows(df_2)
                return(df)
              }

              if (is_2) {
                if (df$V4 %>% str_count('\\ ') %>% unique() %>% length() > 1) {
                  df <-
                    df %>%
                    mutate(V4Count = V4 %>% str_count('\\ '))

                  df_1 <-
                    df %>%
                    filter(V4Count == 2) %>%
                    select(-V4Count) %>%
                    select(which(colMeans(is.na(.)) < 1))

                  df_2 <-
                    df %>%
                    filter(V4Count > 2) %>%
                    select(-V4Count) %>%
                    select(which(colMeans(is.na(.)) < 1))

                  df_1 <-
                    df_1 %>%
                    purrr::set_names(
                      c(
                        'idRow',
                        'nameCompany',
                        'idTypeInvestmentTicker',
                        'priceMonth52WeekHighLow',
                        'ratioFFOPriceCurrentYearNext',
                        'pershareFFOCurrentYearNextYear',
                        'pctFFOGrowth'
                      )
                    )

                  df_1 <-
                    df_1 %>%
                    separate(idTypeInvestmentTicker, c('idTypeInvestment','idTicker'), sep = '\\ ') %>%
                    separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
                    separate(
                      priceMonth52WeekHighLow,
                      c(
                        'priceEndOfMonth',
                        'price52WeekHigh',
                        'price52WeekLow'
                      ),
                      sep = '\\ '
                    ) %>%
                    separate(
                      ratioFFOPriceCurrentYearNext,
                      c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'),
                      sep = '\\ '
                    ) %>%
                    suppressWarnings() %>%
                    suppressMessages()

                  df_2 <-
                    df_2 %>%
                    purrr::set_names(
                      c(
                        'idRow',
                        'nameCompany',
                        'idTypeInvestmentTicker',
                        'priceMonth52WeekHighLowratioFFOPriceCurrentYearNext',
                        'pershareFFOCurrentYearNextYear',
                        'pctFFOGrowth'
                      )
                    )

                  df_2 <-
                    df_2 %>%
                    separate(idTypeInvestmentTicker, c('idTypeInvestment','idTicker'), sep = '\\ ') %>%
                    separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
                    separate(
                      priceMonth52WeekHighLowratioFFOPriceCurrentYearNext,
                      c(
                        'priceEndOfMonth',
                        'price52WeekHigh',
                        'price52WeekLow',
                        'ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'
                      ),
                      sep = '\\ '
                    ) %>%
                    suppressWarnings() %>%
                    suppressMessages()

                  df <-
                    df_1 %>%
                    bind_rows(df_2)
                  return(df)

                }

                df <-
                  df %>%
                  purrr::set_names(
                    c(
                      'idRow',
                      'nameCompany',
                      'idTypeInvestmentExchangeTicker',
                      'priceMonth52WeekHighLow',
                      'ratioFFOPriceCurrentYearNext',
                      'pershareFFOCurrentYearNextYear',
                      'pctFFOGrowth'
                    )
                  )

                df <-
                  df %>%
                  separate(idTypeInvestmentExchangeTicker, c('idTypeInvestment','idExchange', 'idTicker'), sep = '\\ ') %>%
                  separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
                  separate(
                    priceMonth52WeekHighLow,
                    c(
                      'priceEndOfMonth',
                      'price52WeekHigh',
                      'price52WeekLow'
                    ),
                    sep = '\\ '
                  ) %>%
                  separate(
                    ratioFFOPriceCurrentYearNext,
                    c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'),
                    sep = '\\ '
                  ) %>%
                  suppressWarnings() %>%
                  suppressMessages()
                return(df)
              }

              df <-
                df %>%
                purrr::set_names(
                  c(
                    'idRow',
                    'nameCompany',
                    'idTypeInvestment',
                    'idExchangeTicker',
                    'priceMonth52WeekHighLowRatioFFO',
                    'pershareFFOCurrentYearNextYear',
                    'pctFFOGrowth'
                  )
                )

              df <-
                df %>%
                separate(idExchangeTicker, c('idExchange', 'idTicker'), sep = '\\ ') %>%
                separate(
                  priceMonth52WeekHighLowRatioFFO,
                  c(
                    'priceEndOfMonth',
                    'price52WeekHigh',
                    'price52WeekLow',
                    'ratioFFOPriceCurrent',
                    'ratioFFOPriceYearNext'
                  ),
                  sep = '\\ '
                ) %>%
                separate(
                  pershareFFOCurrentYearNextYear,
                  c('pershareFFOCurrentYear', 'pershareFFONextYear'),
                  sep = '\\ '
                ) %>%
                suppressWarnings() %>%
                suppressMessages()
            }

            if (is_14) {
              df_a <-
                df %>%
                mutate(V2char = V2 %>% nchar()) %>%
                filter(V2char == 1) %>%
                select(which(colMeans(is.na(.)) < 1)) %>%
                select(-V2char)

              df_a <-
                df_a %>%
                purrr::set_names(
                  c(
                    'idRow',
                    'nameCompany',
                    'idTypeInvestment',
                    'idTickerPriceEndOfMonth',
                    'price52WeekHighLow',
                    'ratioFFOPriceCurrentYearNext',
                    'pershareFFOCurrentYearNextYear',
                    'pctFFOGrowth'
                  )
                )

              df_a <-
                df_a %>%
                separate(idTickerPriceEndOfMonth,
                         c('idTicker', 'priceEndOfMonth'),
                         sep = '\\ ') %>%
                separate(price52WeekHighLow,
                         c('price52WeekHigh', 'price52WeekLow'),
                         sep = '\\ ') %>%
                separate(
                  ratioFFOPriceCurrentYearNext,
                  c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'),
                  sep = '\\ '
                ) %>%
                separate(
                  pershareFFOCurrentYearNextYear,
                  c('pershareFFOCurrentYear', 'pershareFFONextYear'),
                  sep = '\\ '
                ) %>%
                suppressWarnings() %>%
                suppressMessages()

              df_b <-
                df %>%
                filter(!idRow %in% df_a$idRow) %>%
                select(which(colMeans(is.na(.)) < 1))

              df_b <-
                df_b %>%
                mutate(V11Count = V11 %>% str_count('\\ ')) %>%
                map_df(function(x) {
                  df_zero <-
                    x %>%
                    filter(V11Count == 0) %>%
                    select(-V11Count) %>%
                    select(which(colMeans(is.na(.)) < 1))

                  df_one <-
                    x %>%
                    filter(V11Count > 0) %>%
                    select(-V11Count) %>%
                    select(which(colMeans(is.na(.)) < 1))

                })
              filter(V2char == 1) %>%
                select(which(colMeans(is.na(.)) < 1)) %>%
                select(-V2char)

              df_b <-
                df_b %>%
                separate(idTickerPriceEndOfMonth,
                         c('idTicker', 'priceEndOfMonth'),
                         sep = '\\ ') %>%
                separate(price52WeekHighLow,
                         c('price52WeekHigh', 'price52WeekLow'),
                         sep = '\\ ') %>%
                separate(
                  ratioFFOPriceCurrentYearNext,
                  c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'),
                  sep = '\\ '
                ) %>%
                separate(
                  pershareFFOCurrentYearNextYear,
                  c('pershareFFOCurrentYear', 'pershareFFONextYear'),
                  sep = '\\ '
                ) %>%
                suppressWarnings() %>%
                suppressMessages()

              df <-
                df_a %>%
                bind_rows(df_b)
            }

            if (is_8) {
              has_4 <-
                df$V5 %>% str_count('\\ ') %>% max(na.rm = T) == 4

              if ('V4' %in% names(df)) {
              has_4_v4 <-
                df$V4 %>% str_count('\\ ') %>% max(na.rm = T) == 4
              has_v1 <-
                df$V4 %>% str_count('\\ ') %>% max(na.rm = T) == 1
              } else {
                has_v1 <-
                  F
                has_4_v4 <-
                  F
              }

              if ('V3' %in% names(df)) {
                if ('V2' %in% names(df)) {
                has_v2_double_3 <-
                (df$V2 %>% str_count('\\ ') %>% max(na.rm = T) == 1) &
                (df %>% filter(V3 %>% is.na()) %>% nrow() > 0)
                } else {
                  has_v2_double_3 <-
                    FALSE
                  has_v2_double_2 <-
                    TRUE
                }
              } else {
                has_v2_double_2 <-
                  df$V2 %>% str_count('\\ ') %>% max(na.rm = T) == 1
                has_v2_double_3 <-
                  FALSE
              }

              if (!'has_v2_double_2' %>% exists() & has_v2_double_3 == F) {
                has_v2_double_2 <-
                  TRUE
              }

              if (has_v2_double_3) {
                df <-
                  df %>%
                  filter(!V3 %>% is.na()) %>%
                  unite(V3, V3, V4, sep = ' ') %>%
                  purrr::set_names(list('V', 1:7) %>% purrr::invoke(paste0,.)) %>%
                  bind_rows(
                    df %>%
                      filter(V3 %>% is.na()) %>%
                      select(which(colMeans(is.na(.)) < 1)) %>%
                      purrr::set_names(list('V', 1:7) %>% purrr::invoke(paste0,.))
                  )
               df <-
                 df %>%
                purrr::set_names(
                  c(
                    'idRow',
                    'nameCompany',
                    'idTypeInvestmentTicker',
                    'priceMonth52WeekHigh',
                    'ratioFFOPriceCurrentNext',
                    'pershareFFOCurrentYearNextYear',
                    'pctFFOGrowth'
                  )) %>%
                 separate(idTypeInvestmentTicker, c('idTypeInvestment', 'idTicker'), sep = '\\ ') %>%
                 separate(ratioFFOPriceCurrentNext, c('ratioFFOPriceCurrent',
                                                      'ratioFFOPriceYearNext'), sep = '\\ ') %>%
                 separate(
                   priceMonth52WeekHigh,
                   c(
                     'priceEndOfMonth',
                     'price52WeekHigh',
                     'price52WeekLow'
                   ),
                   sep = '\\ '
                 ) %>%
                 separate(
                   pershareFFOCurrentYearNextYear,
                   c('pershareFFOCurrentYear', 'pershareFFONextYear'),
                   sep = '\\ '
                 ) %>%
                 suppressWarnings() %>%
                 suppressMessages()
               return(df)
              }

              if (has_v2_double_2) {
                is_8 <-
                  df %>%
                  filter(!V5 %>% is.na()) %>%
                  filter(!V6 %>% is.na()) %>%
                  select(which(colMeans(is.na(.)) < 1)) %>% ncol() == 8

                if (is_8) {
                  not_1 <-
                    df$V3 %>% str_count('\\ ') %>% unique() %>% length() == 0
                  if (!not_1){
                  is_1 <-
                    (df$V3 %>% str_count('\\ ') %>% unique() %>% max(na.rm = T) == 1)
                  } else {
                    is_1 <-
                      FALSE
                  }

                  if (is_1) {
                   df <-
                     df %>%
                      filter(!V5 %>% is.na()) %>%
                      filter(!V6 %>% is.na()) %>%
                      select(which(colMeans(is.na(.)) < 1)) %>%
                    purrr::set_names(
                      c(
                        'idRow',
                        'nameCompany',
                        'idTypeInvestmentTicker',
                        'priceEndOfMonth',
                        'price52WeekHighLow',
                        'ratioFFOPriceCurrentNext',
                        'pershareFFOCurrentYearNextYear',
                        'pctFFOGrowth'
                      )) %>%
                      separate(idTypeInvestmentTicker, c('idTypeInvestment', 'idTicker'), sep = '\\ ') %>%
                      separate(ratioFFOPriceCurrentNext, c('ratioFFOPriceCurrent',
                                                           'ratioFFOPriceYearNext'), sep = '\\ ') %>%
                      separate(
                        price52WeekHighLow,
                        c(
                          'price52WeekHigh',
                          'price52WeekLow'
                        ),
                        sep = '\\ '
                      ) %>%
                      separate(
                        pershareFFOCurrentYearNextYear,
                        c('pershareFFOCurrentYear', 'pershareFFONextYear'),
                        sep = '\\ '
                      ) %>%
                      suppressWarnings() %>%
                      suppressMessages()
                    return(df)
                  }

                  if (not_1) {
                    is_1_2 <-
                      df$V2 %>% str_count('\\ ') %>% unique() == 1
                    if (is_1_2) {
                      df <-
                        df %>%
                        filter(!V5 %>% is.na()) %>%
                        filter(!V6 %>% is.na()) %>%
                        select(which(colMeans(is.na(.)) < 1)) %>%
                        purrr::set_names(
                          c(
                            'idRow',
                            'nameCompany',
                            'idTypeInvestmentTicker',
                            'priceEndOfMonth',
                            'price52WeekHighLow',
                            'ratioFFOPriceCurrentNext',
                            'pershareFFOCurrentYearNextYear',
                            'pctFFOGrowth'
                          )) %>%
                        separate(idTypeInvestmentTicker, c('idTypeInvestment', 'idTicker'), sep = '\\ ') %>%
                        separate(ratioFFOPriceCurrentNext, c('ratioFFOPriceCurrent',
                                                             'ratioFFOPriceYearNext'), sep = '\\ ') %>%
                        separate(
                          price52WeekHighLow,
                          c(
                            'price52WeekHigh',
                            'price52WeekLow'
                          ),
                          sep = '\\ '
                        ) %>%
                        separate(
                          pershareFFOCurrentYearNextYear,
                          c('pershareFFOCurrentYear', 'pershareFFONextYear'),
                          sep = '\\ '
                        ) %>%
                        suppressWarnings() %>%
                        suppressMessages()
                      return(df)
                    }
                  }

                  df_1 <-
                    df %>%
                    filter(!V5 %>% is.na()) %>%
                    filter(!V6 %>% is.na()) %>%
                    select(which(colMeans(is.na(.)) < 1))

                  is_exchanges <-
                    (df_1[,3] %>% magrittr::extract2(1) %>% nchar() %>% max(na.rm = T) == 1) &
                    (df_1[,3] %>% magrittr::extract2(1) %>% str_count('\\ ') %>% unique() == 1) |
                    (df_1[,3] %>% magrittr::extract2(1) %>% nchar() %>% max(na.rm = T) == 1) &
                    (df_1[,4] %>% magrittr::extract2(1) %>% str_count('\\ ') %>% unique() == 1) %>%
                    .[[1]]


                  if (!'is_exchanges' %>% exists()) {
                    is_exchanges <-
                      FALSE
                  }

                  if (is_exchanges) {
                    if(df$V4 %>% str_count('\\ ') %>% unique() %>% length() == 2) {
                      df_1a <-
                        df_1 %>%
                        mutate(countV4 = V4 %>% str_count('\\ ')) %>%
                        filter(countV4 == 2) %>%
                        select(which(colMeans(is.na(.)) < 1)) %>%
                        select(-countV4)

                      df_1a <-
                        df_1a %>%
                        purrr::set_names(
                          c(
                            'idRow',
                            'nameCompany',
                            'idTypeInvestment',
                            'idExchangeTicker',
                            'priceMonth52WeekHigh',
                            'ratioFFOPriceCurrentNext',
                            'pershareFFOCurrentYearNextYear',
                            'pctFFOGrowth'
                          )) %>%
                        separate(idExchangeTicker, c('idExchange',
                                                     'idTicker'), sep = '\\ ') %>%
                        separate(ratioFFOPriceCurrentNext, c('ratioFFOPriceCurrent',
                                                             'ratioFFOPriceYearNext'), sep = '\\ ') %>%
                        separate(
                          priceMonth52WeekHigh,
                          c(
                            'priceEndOfMonth',
                            'price52WeekHigh',
                            'price52WeekLow'
                          ),
                          sep = '\\ '
                        ) %>%
                        separate(
                          pershareFFOCurrentYearNextYear,
                          c('pershareFFOCurrentYear', 'pershareFFONextYear'),
                          sep = '\\ '
                        ) %>%
                        suppressWarnings() %>%
                        suppressMessages()

                      df_1b <-
                        df_1 %>%
                        mutate(countV4 = V4 %>% str_count('\\ ')) %>%
                        filter(countV4 == 4) %>%
                        select(which(colMeans(is.na(.)) < 1)) %>%
                        select(-countV4)

                      df_1b <-
                        df_1b %>%
                        purrr::set_names(
                          c(
                            'idRow',
                            'nameCompany',
                            'idTypeInvestment',
                            'idExchangeTicker',
                            'ratioFFOPriceCurrentNextpershareFFOCurrentYearNextYear',
                            'pershareFFOCurrentYearNextYear',
                            'pctFFOGrowth'
                          )) %>%
                        separate(idExchangeTicker, c('idExchange',
                                                     'idTicker'), sep = '\\ ') %>%
                        separate(
                          ratioFFOPriceCurrentNextpershareFFOCurrentYearNextYear,
                          c(
                            'priceEndOfMonth',
                            'price52WeekHigh',
                            'price52WeekLow',
                            'ratioFFOPriceCurrent',
                            'ratioFFOPriceYearNext'
                          ),
                          sep = '\\ '
                        ) %>%
                        separate(
                          pershareFFOCurrentYearNextYear,
                          c('pershareFFOCurrentYear', 'pershareFFONextYear'),
                          sep = '\\ '
                        ) %>%
                        suppressWarnings() %>%
                        suppressMessages()

                      df_2 <-
                        df %>%
                        filter(V5 %>% is.na()) %>%
                        select(which(colMeans(is.na(.)) < 1))

                      df_2 <-
                        df_2 %>%
                        purrr::set_names(
                          c(
                            'idRow',
                            'nameCompany',
                            'idTypeInvestment',
                            'idExchangeTicker',
                            'price52WeekHighLowMonth'
                          )) %>%
                        separate(idExchangeTicker, c('idExchange',
                                                     'idTicker'), sep = '\\ ') %>%
                        separate(
                          price52WeekHighLowMonth,
                          c(
                            'priceEndOfMonth',
                            'price52WeekHigh',
                            'price52WeekLow'
                          ),
                          sep = '\\ '
                        )

                      df <-
                        df_1a %>%
                        bind_rows(list(df_1b, df_2))
                      return(df)
                    }

                    df_1 <-
                      df_1 %>%
                      purrr::set_names(
                        c(
                          'idRow',
                          'nameCompany',
                          'idTypeInvestment',
                          'idExchangeTicker',
                          'priceMonth52WeekHigh',
                          'ratioFFOPriceCurrentNext',
                          'pershareFFOCurrentYearNextYear',
                          'pctFFOGrowth'
                        )) %>%
                      separate(idExchangeTicker, c('idExchange',
                                                           'idTicker'), sep = '\\ ') %>%
                      separate(ratioFFOPriceCurrentNext, c('ratioFFOPriceCurrent',
                                                           'ratioFFOPriceYearNext'), sep = '\\ ') %>%
                      separate(
                        priceMonth52WeekHigh,
                        c(
                          'priceEndOfMonth',
                          'price52WeekHigh',
                          'price52WeekLow'
                        ),
                        sep = '\\ '
                      ) %>%
                      separate(
                        pershareFFOCurrentYearNextYear,
                        c('pershareFFOCurrentYear', 'pershareFFONextYear'),
                        sep = '\\ '
                      ) %>%
                      suppressWarnings() %>%
                      suppressMessages()

                    has_v6 <-
                      df %>%
                      filter(!V5 %>% is.na()) %>%
                      filter(V6 %>% is.na()) %>%
                      select(which(colMeans(is.na(.)) < 1)) %>% nrow() > 0
                    if (has_v6) {
                      df_2 <-
                      df %>%
                      filter(!V5 %>% is.na()) %>%
                      filter(V6 %>% is.na()) %>%
                      select(which(colMeans(is.na(.)) < 1))

                      if (df_2 %>% ncol() == 5) {
                        df_2 <-
                          df_2 %>%
                          purrr::set_names(
                            c(
                              'idRow',
                              'nameCompany',
                              'idTypeInvestment',
                              'idExchangeTicker',
                              'priceMonth52WeekHighFFO')) %>%
                          separate(idExchangeTicker, c('idExchange',
                                                       'idTicker'), sep = '\\ ') %>%
                          separate(
                            priceMonth52WeekHighFFO,
                            c(
                              'priceEndOfMonth',
                              'price52WeekHigh',
                              'price52WeekLow',
                              'ratioFFOPriceCurrent'
                            ),
                            sep = '\\ '
                          ) %>%
                          suppressWarnings()

                        df <-
                          df_1 %>%
                          bind_rows(df_2)

                        return(df)
                      }

                    df_2 <-
                      df_2 %>%
                      purrr::set_names(
                        c(
                          'idRow',
                          'nameCompany',
                          'idTypeInvestment',
                          'idExchangeTicker',
                          'priceMonth52WeekHighFFO',
                          'pershareFFOCurrentYear')) %>%
                      separate(idExchangeTicker, c('idExchange',
                                                   'idTicker'), sep = '\\ ') %>%
                      separate(
                        priceMonth52WeekHighFFO,
                        c(
                          'priceEndOfMonth',
                          'price52WeekHigh',
                          'price52WeekLow',
                          'ratioFFOPriceCurrent'
                        ),
                        sep = '\\ '
                      )

                    df <-
                      df_1 %>%
                      bind_rows(df_2)

                    return(df)
                    }
                    df_2 <-
                      df %>%
                      filter(V5 %>% is.na()) %>%
                      select(which(colMeans(is.na(.)) < 1))

                    df_2 <-
                      df_2 %>%
                      purrr::set_names(
                      c(
                        'idRow',
                        'nameCompany',
                        'idTypeInvestment',
                        'idExchangeTicker',
                        'priceMonth52WeekHighratioFFOPriceCurrentNext',
                        'pershareFFOCurrentYearNextYear',
                        'pctFFOGrowth'
                      )) %>%
                      separate(idExchangeTicker, c('idExchange',
                                                   'idTicker'), sep = '\\ ') %>%
                      separate(
                        priceMonth52WeekHighratioFFOPriceCurrentNext,
                        c(
                          'priceEndOfMonth',
                          'price52WeekHigh',
                          'price52WeekLow',
                          'ratioFFOPriceCurrent',
                          'ratioFFOPriceYearNext'
                        ),
                        sep = '\\ '
                      ) %>%
                      separate(
                        pershareFFOCurrentYearNextYear,
                        c('pershareFFOCurrentYear', 'pershareFFONextYear'),
                        sep = '\\ '
                      ) %>%
                      suppressWarnings() %>%
                      suppressMessages()

                    df <-
                      df_1 %>%
                      bind_rows(df_2)

                    return(df)
                  }

                  is_exchange <-
                    (df_1[,3] %>% magrittr::extract2(1) %>% nchar() %>% max(na.rm = T) == 1) &
                    (df_1[,3] %>% magrittr::extract2(1) %>% str_count('\\ ') %>% unique() == 0)

                  if (is_exchange) {
                    df_1 <-
                      df_1 %>%
                      purrr::set_names(
                        c(
                          'idRow',
                          'nameCompany',
                          'idTypeInvestment',
                          'idTicker',
                          'priceMonth52WeekHigh',
                          'ratioFFOPriceCurrentNext',
                          'pershareFFOCurrentYearNextYear',
                          'pctFFOGrowth'
                        )) %>%
                      separate(ratioFFOPriceCurrentNext, c('ratioFFOPriceCurrent',
                                                           'ratioFFOPriceYearNext'), sep = '\\ ') %>%
                      separate(
                        priceMonth52WeekHigh,
                        c(
                          'priceEndOfMonth',
                          'price52WeekHigh',
                          'price52WeekLow'
                        ),
                        sep = '\\ '
                      ) %>%
                      separate(
                        pershareFFOCurrentYearNextYear,
                        c('pershareFFOCurrentYear', 'pershareFFONextYear'),
                        sep = '\\ '
                      ) %>%
                      suppressWarnings() %>%
                      suppressMessages()

                    df_2 <-
                      df %>%
                      filter(V5 %>% is.na()) %>%
                      select(which(colMeans(is.na(.)) < 1))
                    if (df_2 %>% nrow() > 0) {
                    is_5 <-
                      df_2 %>% ncol() == 5

                    is_6 <-
                      df_2 %>% ncol() == 6

                      if (is_5) {
                      df_2 <-
                      df_2 %>%
                      purrr::set_names(
                        c(
                          'idRow',
                          'nameCompany',
                          'idTypeInvestment',
                          'idTicker',
                          'priceMonth52WeekHigh')) %>%
                      separate(
                        priceMonth52WeekHigh,
                        c(
                          'priceEndOfMonth',
                          'price52WeekHigh',
                          'price52WeekLow'
                        ),
                        sep = '\\ '
                      )

                    df <-
                      df_1 %>%
                      bind_rows(df_2)

                    return(df)
                      }

                    if (is_6) {
                      df_2 <-
                        df_2 %>%
                        purrr::set_names(
                          c(
                            'idRow',
                            'nameCompany',
                            'idTypeInvestment',
                            'idTicker',
                            'priceMonth52WeekHigh',
                            'pctFFOGrowth')) %>%
                        separate(
                          priceMonth52WeekHigh,
                          c(
                            'priceEndOfMonth',
                            'price52WeekHigh',
                            'price52WeekLow'
                          ),
                          sep = '\\ '
                        )

                      df <-
                        df_1 %>%
                        bind_rows(df_2)

                      return(df)
                    }
                    } else {
                      return(df_1)
                    }
                  }

                  is_id_exchange <-
                    (df_1[,3] %>% magrittr::extract2(1) %>% str_count('\\ ') %>% unique() == 1 ) &
                    df_1[,4] %>% magrittr::extract2(1) %>% str_count('\\ ') %>% unique() %>% length() == 2

                  if (is_id_exchange) {
                    df_1 <-
                      df_1 %>%
                      filter(!V4 %>% is.na()) %>%
                      unite(V3, V4, V3, sep = ' ') %>%
                      select(which(colMeans(is.na(.)) < 1)) %>%
                      purrr::set_names(list('V', 1:7) %>% purrr::invoke(paste0,.)) %>%
                      bind_rows(
                        df_1 %>%
                          filter(V4 %>% is.na()) %>%
                          select(which(colMeans(is.na(.)) < 1)) %>%
                          purrr::set_names(list('V', 1:7) %>% purrr::invoke(paste0,.))
                      )

                      df_1 <-
                        df_1 %>%
                        purrr::set_names(
                        c(
                          'idRow',
                          'nameCompany',
                          'idTypeInvestmentTicker',
                          'priceMonth52WeekHighLow',
                          'ratioFFOPriceCurrentYearNext',
                          'pershareFFOCurrentYearNextYear',
                          'pctFFOGrowth'
                        )
                      ) %>%
                      separate(idTypeInvestmentTicker, c('idTypeInvestment', 'idTicker'), sep = '\\ ') %>%
                      separate(
                        priceMonth52WeekHighLow,
                        c('priceEndOfMonth', 'price52WeekHigh', 'price52WeekLow'),
                        sep = '\\ '
                      ) %>%
                      separate(
                        ratioFFOPriceCurrentYearNext,
                        c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'),
                        sep = '\\ '
                      ) %>%
                      separate(
                        pershareFFOCurrentYearNextYear,
                        c('pershareFFOCurrentYear', 'pershareFFONextYear'),
                        sep = '\\ '
                      ) %>%
                      suppressWarnings() %>%
                      suppressMessages()

                      df_2 <-
                        df %>%
                        filter(V5 %>% is.na()) %>%
                        filter(V4 %>% is.na()) %>%
                        select(which(colMeans(is.na(.)) < 1)) %>%
                        purrr::set_names(list('V', 1:4) %>% purrr::invoke(paste0,.)) %>%
                        bind_rows(
                          df %>%
                            filter(V5 %>% is.na()) %>%
                            filter(!V4 %>% is.na()) %>%
                            select(which(colMeans(is.na(.)) < 1))  %>%
                            unite(V3, V3, V4, sep = ' ') %>%
                            purrr::set_names(list('V', 1:4) %>% purrr::invoke(paste0,.))
                        ) %>%
                        purrr::set_names(
                          c(
                            'idRow',
                            'nameCompany',
                            'idTypeInvestmentTicker',
                            'priceMonth52WeekHighLow')) %>%
                        separate(idTypeInvestmentTicker, c('idTypeInvestment', 'idTicker'), sep = '\\ ') %>%
                        separate(
                          priceMonth52WeekHighLow,
                          c('priceEndOfMonth', 'price52WeekHigh', 'price52WeekLow'),
                          sep = '\\ '
                        )


                      df <-
                        df_1 %>%
                        bind_rows(df_2)
                      return(df)
                  }

                  df_1 <-
                    df_1 %>%
                    purrr::set_names(
                      c(
                        'idRow',
                        'nameCompany',
                        'idTypeInvestment',
                        'idTicker',
                        'priceMonth52WeekHigh',
                        'ratioFFOPriceCurrentNext',
                        'pershareFFOCurrentYearNextYear',
                        'pctFFOGrowth'
                      )) %>%
                    separate(ratioFFOPriceCurrentNext, c('ratioFFOPriceCurrent',
                                                         'ratioFFOPriceYearNext'), sep = '\\ ') %>%
                    separate(
                      priceMonth52WeekHigh,
                      c(
                        'priceEndOfMonth',
                        'price52WeekHigh',
                        'price52WeekLow'
                      ),
                      sep = '\\ '
                    ) %>%
                    separate(
                      pershareFFOCurrentYearNextYear,
                      c('pershareFFOCurrentYear', 'pershareFFONextYear'),
                      sep = '\\ '
                    ) %>%
                    suppressWarnings() %>%
                    suppressMessages()


                  df_2 <-
                    df %>%
                    filter(!V5 %>% is.na()) %>%
                    filter(V6 %>% is.na()) %>%
                    select(which(colMeans(is.na(.)) < 1))

                  df_2 <-
                    df_2 %>%
                    purrr::set_names(
                      c(
                        'idRow',
                        'nameCompany',
                        'idTypeInvestment',
                        'idTicker',
                        'priceMonth52WeekHigh')) %>%
                    separate(
                      priceMonth52WeekHigh,
                      c(
                        'priceEndOfMonth',
                        'price52WeekHigh',
                        'price52WeekLow'
                      ),
                      sep = '\\ '
                    )

                  df <-
                    df_1 %>%
                    bind_rows(df_2)

                  return(df)

                }

                df <-
                  df %>%
                  filter(!V5 %>% is.na()) %>%
                  filter(!V6 %>% is.na()) %>%
                  select(which(colMeans(is.na(.)) < 1)) %>%
                  purrr::set_names(list('V', 1:7) %>% purrr::invoke(paste0,.)) %>%
                  bind_rows(
                    df %>%
                      filter(!V5 %>% is.na()) %>%
                      filter(V6 %>% is.na()) %>%
                      select(which(colMeans(is.na(.)) < 1)) %>%
                      purrr::set_names(list('V', 1:7) %>% purrr::invoke(paste0,.))
                  )

                df <-
                  df %>%
                  purrr::set_names(
                    c(
                      'idRow',
                      'nameCompany',
                      'idTypeInvestmentTicker',
                      'priceMonth52WeekHigh',
                      'ratioFFOPriceCurrentNext',
                      'pershareFFOCurrentYearNextYear',
                      'pctFFOGrowth'
                    )) %>%
                  separate(idTypeInvestmentTicker, c('idTypeInvestment', 'idTicker'), sep = '\\ ') %>%
                  separate(ratioFFOPriceCurrentNext, c('ratioFFOPriceCurrent',
                                                       'ratioFFOPriceYearNext'), sep = '\\ ') %>%
                  separate(
                    priceMonth52WeekHigh,
                    c(
                      'priceEndOfMonth',
                      'price52WeekHigh',
                      'price52WeekLow'
                    ),
                    sep = '\\ '
                  ) %>%
                  separate(
                    pershareFFOCurrentYearNextYear,
                    c('pershareFFOCurrentYear', 'pershareFFONextYear'),
                    sep = '\\ '
                  ) %>%
                  suppressWarnings() %>%
                  suppressMessages()

                return(df)
              }

              if (has_v1) {
                df <-
                  df %>%
                  purrr::set_names(
                    c(
                      'idRow',
                      'nameCompany',
                      'idTypeInvestment',
                      'idExchangeTicker',
                      'priceMonth52WeekHigh',
                      'ratioFFOPriceCurrentNext',
                      'pershareFFOCurrentYearNextYear',
                      'pctFFOGrowth'

                    )
                  )
                df <-
                  df %>%
                  separate(idExchangeTicker, c('idExchange', 'idTicker'), sep = '\\ ') %>%
                  separate(ratioFFOPriceCurrentNext, c('ratioFFOPriceCurrent',
                                                       'ratioFFOPriceYearNext'), sep = '\\ ') %>%
                  separate(
                    priceMonth52WeekHigh,
                    c(
                      'priceEndOfMonth',
                      'price52WeekHigh',
                      'price52WeekLow'
                    ),
                    sep = '\\ '
                  ) %>%
                  separate(
                    pershareFFOCurrentYearNextYear,
                    c('pershareFFOCurrentYear', 'pershareFFONextYear'),
                    sep = '\\ '
                  ) %>%
                  suppressWarnings() %>%
                  suppressMessages()
                return(df)

              }

              if (has_4) {
                df <-
                  df %>%
                  mutate(V5Count = V5 %>% str_count('\\ '))

                df_2 <-
                  df %>%
                  filter(V5Count == 2) %>%
                  select(-V5Count) %>%
                  select(which(colMeans(is.na(.)) < 1))

                df_4 <-
                  df %>%
                  filter(V5Count == 4) %>%
                  select(-V5Count) %>%
                  select(which(colMeans(is.na(.)) < 1))

                df_4 <-
                  df_4 %>%
                  purrr::set_names(
                    c(
                      'idRow',
                      'nameCompany',
                      'idTypeInvestment',
                      'idExchangeTicker',
                      'priceMonth52WeekHighLowRatioFFO',
                      'pershareFFOCurrentYearNextYear',
                      'pctFFOGrowth'
                    )
                  )

                df_4 <-
                  df_4 %>%
                  separate(idExchangeTicker, c('idExchange', 'idTicker'), sep = '\\ ') %>%
                  separate(
                    priceMonth52WeekHighLowRatioFFO,
                    c(
                      'priceEndOfMonth',
                      'price52WeekHigh',
                      'price52WeekLow',
                      'ratioFFOPriceCurrent',
                      'ratioFFOPriceYearNext'
                    ),
                    sep = '\\ '
                  ) %>%
                  separate(
                    pershareFFOCurrentYearNextYear,
                    c('pershareFFOCurrentYear', 'pershareFFONextYear'),
                    sep = '\\ '
                  ) %>%
                  suppressWarnings() %>%
                  suppressMessages()

                df_2 <-
                  df_2 %>%
                  purrr::set_names(
                    c(
                      'idRow',
                      'nameCompany',
                      'idTypeInvestment',
                      'idExchangeTicker',
                      'priceMonth52WeekHighLow',
                      'ratioFFOPriceCurrentYearNext',
                      'pershareFFOCurrentYearNextYear',
                      'pctFFOGrowth'
                    )
                  )

                df_2 <-
                  df_2 %>%
                  separate(idExchangeTicker, c('idExchange', 'idTicker'), sep = '\\ ') %>%
                  separate(
                    priceMonth52WeekHighLow,
                    c('priceEndOfMonth', 'price52WeekHigh', 'price52WeekLow'),
                    sep = '\\ '
                  ) %>%
                  separate(
                    ratioFFOPriceCurrentYearNext,
                    c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'),
                    sep = '\\ '
                  ) %>%
                  separate(
                    pershareFFOCurrentYearNextYear,
                    c('pershareFFOCurrentYear', 'pershareFFONextYear'),
                    sep = '\\ '
                  ) %>%
                  suppressWarnings() %>%
                  suppressMessages()

                df <-
                  df_2 %>%
                  bind_rows(df_4)
              }
              if (has_4_v4) {
                df <-
                  df %>%
                  mutate(V4Count = V4 %>% str_count('\\ '))

                df_2 <-
                  df %>%
                  filter(V4Count == 2) %>%
                  select(-V4Count) %>%
                  select(which(colMeans(is.na(.)) < 1))

                df_4 <-
                  df %>%
                  filter(V4Count == 4) %>%
                  select(-V4Count) %>%
                  select(which(colMeans(is.na(.)) < 1))

                has_max_4 <-
                  df_4$V4 %>% str_count('\\ ') %>% max(na.rm = T) == 4

                if (has_max_4) {
                has_6 <-
                  df %>% ncol() == 6
                has_7 <-
                  df %>% ncol() %in% c(7,9)

                if (has_7){
                  if (df_4 %>% ncol() == 7) {
                    df_4 <-
                      df_4 %>%
                      purrr::set_names(
                        c(
                          'idRow',
                          'nameCompany',
                          'idTypeInvestment',
                          'idExchangeTicker',
                          'priceMonth52WeekHighLowRatioFFO',
                          'pershareFFOCurrentYearNextYear',
                          'pctFFOGrowth'
                        )
                      )

                    df_4 <-
                      df_4 %>%
                      separate(idExchangeTicker, c('idExchange', 'idTicker'), sep = '\\ ') %>%
                      separate(
                        priceMonth52WeekHighLowRatioFFO,
                        c(
                          'priceEndOfMonth',
                          'price52WeekHigh',
                          'price52WeekLow',
                          'ratioFFOPriceCurrent',
                          'ratioFFOPriceYearNext'
                        ),
                        sep = '\\ '
                      ) %>%
                      separate(
                        pershareFFOCurrentYearNextYear,
                        c('pershareFFOCurrentYear', 'pershareFFONextYear'),
                        sep = '\\ '
                      ) %>%
                      suppressWarnings() %>%
                      suppressMessages()

                    df_2 <-
                      df_2 %>%
                      purrr::set_names(
                        c(
                          'idRow',
                          'nameCompany',
                          'idTypeInvestment',
                          'idExchangeTicker',
                          'priceMonth52Week',
                          'ratioFFOPriceCurrentNext',
                          'pershareFFOCurrentYearNextYear',
                          'pctFFOGrowth'
                        )
                      )

                    df_2 <-
                      df_2 %>%
                      separate(idExchangeTicker, c('idExchange', 'idTicker'), sep = '\\ ') %>%
                      separate(
                        priceMonth52Week,
                        c('priceEndOfMonth',
                          'price52WeekHigh',
                          'price52WeekLow'),

                        sep = '\\ '
                      ) %>%
                      separate(
                        ratioFFOPriceCurrentNext,
                        c(
                          'ratioFFOPriceCurrent',
                          'ratioFFOPriceYearNext'),
                          sep = '\\ '

                      ) %>%
                      separate(
                        pershareFFOCurrentYearNextYear,
                        c('pershareFFOCurrentYear', 'pershareFFONextYear'),
                        sep = '\\ '
                      ) %>%
                      suppressWarnings() %>%
                      suppressMessages()

                    df <-
                      df_4 %>%
                      bind_rows(df_2)
                    return(df)
                  }

                  df_4 <-
                    df_4 %>%
                    purrr::set_names(
                      c(
                        'idRow',
                        'nameCompany',
                        'idTypeInvestmentTicker',
                        'priceMonth52WeekHighLowRatioFFO',
                        'pershareFFOCurrentYearNextYear',
                        'pctFFOGrowth'
                      )
                    )

                  df_4 <-
                    df_4 %>%
                    separate(idTypeInvestmentTicker, c('idTypeInvestment', 'idTicker'), sep = '\\ ') %>%
                    separate(
                      priceMonth52WeekHighLowRatioFFO,
                      c(
                        'priceEndOfMonth',
                        'price52WeekHigh',
                        'price52WeekLow',
                        'ratioFFOPriceCurrent',
                        'ratioFFOPriceYearNext'
                      ),
                      sep = '\\ '
                    ) %>%
                    separate(
                      pershareFFOCurrentYearNextYear,
                      c('pershareFFOCurrentYear', 'pershareFFONextYear'),
                      sep = '\\ '
                    ) %>%
                    suppressWarnings() %>%
                    suppressMessages()

                  df_2 <-
                    df_2 %>%
                    filter(V6 %>% is.na()) %>%
                    select(which(colMeans(is.na(.)) < 1)) %>%
                    purrr::set_names(list('V', 1:7) %>% purrr::invoke(paste0,.))
                    bind_rows(
                      df_2 %>%
                        filter(!V6 %>% is.na()) %>%
                        select(which(colMeans(is.na(.)) < 1)) %>%
                        purrr::set_names(list('V', 1:7) %>% purrr::invoke(paste0,.))
                    )


                  df_2 <-
                    df_2 %>%
                    purrr::set_names(
                      c(
                        'idRow',
                        'nameCompany',
                        'idTypeInvestmentTicker',
                        'priceMonth52WeekHighLow',
                        'ratioFFOPriceCurrentYearNext',
                        'pershareFFOCurrentYearNextYear',
                        'pctFFOGrowth'
                      )
                    ) %>%
                    separate(idTypeInvestmentTicker, c('idTypeInvestment', 'idTicker'), sep = '\\ ') %>%
                    separate(
                      priceMonth52WeekHighLow,
                      c('priceEndOfMonth', 'price52WeekHigh', 'price52WeekLow'),
                      sep = '\\ '
                    ) %>%
                    separate(
                      ratioFFOPriceCurrentYearNext,
                      c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'),
                      sep = '\\ '
                    ) %>%
                    separate(
                      pershareFFOCurrentYearNextYear,
                      c('pershareFFOCurrentYear', 'pershareFFONextYear'),
                      sep = '\\ '
                    ) %>%
                    suppressWarnings() %>%
                    suppressMessages()

                  df <-
                    df_2 %>%
                    bind_rows(df_4)
                  return(df)
                }
                has_7 <-
                  df_4 %>% ncol() == 7
                  if (has_7){
                  df_4 <-
                    df_4 %>%
                    purrr::set_names(
                      c(
                        'idRow',
                        'nameCompany',
                        'idTypeInvestment',
                        'idTicker',
                        'priceMonth52WeekHighLowRatioFFO',
                        'pershareFFOCurrentYearNextYear',
                        'pctFFOGrowth'
                      )
                    )

                  df_4 <-
                    df_4 %>%
                    separate(
                      priceMonth52WeekHighLowRatioFFO,
                      c(
                        'priceEndOfMonth',
                        'price52WeekHigh',
                        'price52WeekLow',
                        'ratioFFOPriceCurrent',
                        'ratioFFOPriceYearNext'
                      ),
                      sep = '\\ '
                    ) %>%
                    separate(
                      pershareFFOCurrentYearNextYear,
                      c('pershareFFOCurrentYear', 'pershareFFONextYear'),
                      sep = '\\ '
                    ) %>%
                    suppressWarnings() %>%
                    suppressMessages()

                  df_2 <-
                    df_2 %>%
                    filter(!V3 %>% is.na()) %>%
                    unite(V2, V2, V3, sep = ' ') %>%
                    bind_rows(
                      df_2 %>%
                        filter(V3 %>% is.na()) %>%
                        select(which(colMeans(is.na(.)) < 1))
                    )


                  df_2 <-
                    df_2 %>%
                    purrr::set_names(
                      c(
                        'idRow',
                        'nameCompany',
                        'idTypeInvestmentTicker',
                        'priceMonth52WeekHighLow',
                        'ratioFFOPriceCurrentYearNext',
                        'pershareFFOCurrentYearNextYear',
                        'pctFFOGrowth'
                      )
                    ) %>%
                    separate(idTypeInvestmentTicker, c('idTypeInvestment', 'idTicker'), sep = '\\ ') %>%
                    separate(
                      priceMonth52WeekHighLow,
                      c('priceEndOfMonth', 'price52WeekHigh', 'price52WeekLow'),
                      sep = '\\ '
                    ) %>%
                    separate(
                      ratioFFOPriceCurrentYearNext,
                      c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'),
                      sep = '\\ '
                    ) %>%
                    separate(
                      pershareFFOCurrentYearNextYear,
                      c('pershareFFOCurrentYear', 'pershareFFONextYear'),
                      sep = '\\ '
                    ) %>%
                    suppressWarnings() %>%
                    suppressMessages()

                  df <-
                    df_2 %>%
                    bind_rows(df_4)
                  return(df)
                }
                }

                df_4 <-
                  df_4 %>%
                  purrr::set_names(
                    c(
                      'idRow',
                      'nameCompany',
                      'idTypeInvestment',
                      'idExchangeTicker',
                      'priceMonth52WeekHighLowRatioFFO',
                      'pershareFFOCurrentYearNextYear',
                      'pctFFOGrowth'
                    )
                  )

                df_4 <-
                  df_4 %>%
                  separate(idExchangeTicker, c('idExchange', 'idTicker'), sep = '\\ ') %>%
                  separate(
                    priceMonth52WeekHighLowRatioFFO,
                    c(
                      'priceEndOfMonth',
                      'price52WeekHigh',
                      'price52WeekLow',
                      'ratioFFOPriceCurrent',
                      'ratioFFOPriceYearNext'
                    ),
                    sep = '\\ '
                  ) %>%
                  separate(
                    pershareFFOCurrentYearNextYear,
                    c('pershareFFOCurrentYear', 'pershareFFONextYear'),
                    sep = '\\ '
                  ) %>%
                  suppressWarnings() %>%
                  suppressMessages()

                df_2 <-
                  df_2 %>%
                  purrr::set_names(
                    c(
                      'idRow',
                      'nameCompany',
                      'idTypeInvestment',
                      'idExchangeTicker',
                      'priceMonth52WeekHighLow',
                      'ratioFFOPriceCurrentYearNext',
                      'pershareFFOCurrentYearNextYear',
                      'pctFFOGrowth'
                    )
                  )

                df_2 <-
                  df_2 %>%
                  separate(idExchangeTicker, c('idExchange', 'idTicker'), sep = '\\ ') %>%
                  separate(
                    priceMonth52WeekHighLow,
                    c('priceEndOfMonth', 'price52WeekHigh', 'price52WeekLow'),
                    sep = '\\ '
                  ) %>%
                  separate(
                    ratioFFOPriceCurrentYearNext,
                    c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'),
                    sep = '\\ '
                  ) %>%
                  separate(
                    pershareFFOCurrentYearNextYear,
                    c('pershareFFOCurrentYear', 'pershareFFONextYear'),
                    sep = '\\ '
                  ) %>%
                  suppressWarnings() %>%
                  suppressMessages()

                df <-
                  df_2 %>%
                  bind_rows(df_4)
              }

              df <-
                df %>%
                purrr::set_names(
                  c(
                    'idRow',
                    'nameCompany',
                    'idTypeInvestment',
                    'idTicker',
                    'priceMonth52WeekHigh',
                    'ratioFFOPriceCurrentYearNext',
                    'pershareFFOCurrentYearNextYear',
                    'pctFFOGrowth'
                  )
                )

              df <-
                df %>%
                separate(priceMonth52WeekHigh,
                         c('priceEndOfMonth','price52WeekHigh', 'price52WeekLow'),
                         sep = '\\ ') %>%
                separate(
                  ratioFFOPriceCurrentYearNext,
                  c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'),
                  sep = '\\ '
                ) %>%
                separate(
                  pershareFFOCurrentYearNextYear,
                  c('pershareFFOCurrentYear', 'pershareFFONextYear'),
                  sep = '\\ '
                ) %>%
                suppressWarnings() %>%
                suppressMessages()

              return(df)

            }

            if (is_9) {
              is_mixed <-
                df %>%
                filter(V4 %>% is.na()) %>% nrow() / nrow(df) > 0

              if (is_mixed) {
                df_1 <-
                  df %>%
                  filter(!V4 %>% is.na()) %>%
                  select(which(colMeans(is.na(.)) < 1)) %>%
                  purrr::set_names(
                    c(
                      'idRow',
                      'nameCompany',
                      'idTypeInvestmentidTicker',
                      'priceEndOfMonth',
                      'price52WeekHighLow',
                      'ratioFFOPriceCurrentYearNext',
                      'pershareFFOCurrentYearNextYear',
                      'pctFFOGrowth'
                    )
                  ) %>%
                  separate(idTypeInvestmentidTicker,
                           c('idTypeInvestment', 'idTicker'),
                           sep = '\\ ') %>%
                  separate(price52WeekHighLow,
                           c('price52WeekHigh', 'price52WeekLow'),
                           sep = '\\ ') %>%
                  separate(
                    ratioFFOPriceCurrentYearNext,
                    c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'),
                    sep = '\\ '
                  ) %>%
                  separate(
                    pershareFFOCurrentYearNextYear,
                    c('pershareFFOCurrentYear', 'pershareFFONextYear'),
                    sep = '\\ '
                  ) %>%
                  suppressWarnings() %>%
                  suppressMessages()

                df_2 <-
                  df %>%
                  filter(V4 %>% is.na()) %>%
                  select(which(colMeans(is.na(.)) < 1)) %>%
                  purrr::set_names(
                    c(
                      'idRow',
                      'nameCompany',
                      'idTypeInvestmentidTicker',
                      'priceEndOfMonth',
                      'price52WeekHighLow',
                      'ratioFFOPriceCurrentYearNext',
                      'pershareFFOCurrentYearNextYear',
                      'pctFFOGrowth'
                    )
                  ) %>%
                  separate(idTypeInvestmentidTicker,
                           c('idTypeInvestment', 'idTicker'),
                           sep = '\\ ') %>%
                  separate(price52WeekHighLow,
                           c('price52WeekHigh', 'price52WeekLow'),
                           sep = '\\ ') %>%
                  separate(
                    ratioFFOPriceCurrentYearNext,
                    c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'),
                    sep = '\\ '
                  ) %>%
                  separate(
                    pershareFFOCurrentYearNextYear,
                    c('pershareFFOCurrentYear', 'pershareFFONextYear'),
                    sep = '\\ '
                  ) %>%
                  suppressWarnings() %>%
                  suppressMessages()

                df <-
                  df_1 %>%
                  bind_rows(df_2)
                return(df)
              }

              df <-
                df %>%
                purrr::set_names(
                  c(
                    'idRow',
                    'nameCompany',
                    'idTypeInvestment',
                    'idTicker',
                    'priceEndOfMonth',
                    'price52WeekHighLow',
                    'ratioFFOPriceCurrentYearNext',
                    'pershareFFOCurrentYearNextYear',
                    'pctFFOGrowth'
                  )
                )

              df <-
                df %>%
                separate(price52WeekHighLow,
                         c('price52WeekHigh', 'price52WeekLow'),
                         sep = '\\ ') %>%
                separate(
                  ratioFFOPriceCurrentYearNext,
                  c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'),
                  sep = '\\ '
                ) %>%
                separate(
                  pershareFFOCurrentYearNextYear,
                  c('pershareFFOCurrentYear', 'pershareFFONextYear'),
                  sep = '\\ '
                ) %>%
                suppressWarnings() %>%
                suppressMessages()
            }

            if (is_10) {
              is_31 <-
                df$V5 %>% str_count('\\ ') %>% unique() %>%
                paste0(collapse = ":") == "3:1"

              if (is_31) {
                df <-
                  df %>%
                  mutate(countV5 = V5 %>% str_count('\\ '))

                df_1a <-
                  df %>%
                  filter(countV5 == 3) %>%
                  select(-countV5) %>%
                  select(which(colMeans(is.na(.)) < 1)) %>%
                  filter(!V8 %>% is.na()) %>%
                  select(which(colMeans(is.na(.)) < 1)) %>%
                  purrr::set_names(c('idRow', 'nameCompany',
                                     'idTypeInvestment', 'idTicker', 'priceEndOfMonth', 'price52WeekLowratioFFOPriceCurrentYearNext',
                                     'pershareFFOCurrentYearNextYear',
                                     'pctFFOGrowthDividendYield', 'pctDividendSpread'))

                df_1a <-
                  df_1a %>%
                  separate(price52WeekLowratioFFOPriceCurrentYearNext, c('price52WeekHigh', 'price52WeekLow', 'ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
                  separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
                  separate(pctFFOGrowthDividendYield, c('pctFFOGrowth', 'pctDividendYield'), sep = '\\ ') %>%
                  suppressWarnings() %>%
                  suppressMessages()

                df_1b <-
                  df %>%
                  filter(countV5 == 3) %>%
                  select(-countV5) %>%
                  select(which(colMeans(is.na(.)) < 1)) %>%
                  filter(V8 %>% is.na()) %>%
                  select(which(colMeans(is.na(.)) < 1))

                df_1b <-
                  df_1b %>%
                  mutate(isNonNumeric = ifelse(V2 %>% readr::parse_number() %>% is.na(), T, F)) %>%
                  filter(isNonNumeric == T) %>%
                  select(which(colMeans(is.na(.)) < 1)) %>%
                  select(-isNonNumeric) %>%
                  purrr::set_names(c('idRow', 'nameCompany',
                                     'idTypeInvestment', 'idTicker', 'priceEndOfMonth', 'price52WeekLowratioFFOPriceCurrentYearNext',
                                     'pershareFFOCurrentYearNextYear',
                                     'pctFFOGrowthDividendYieldpctDividendSpread')) %>%
                  separate(price52WeekLowratioFFOPriceCurrentYearNext, c('price52WeekHigh', 'price52WeekLow', 'ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
                  separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
                  separate(pctFFOGrowthDividendYieldpctDividendSpread, c('pctFFOGrowth', 'pctDividendYield', 'pctDividendSpread'), sep = '\\ ') %>%
                  suppressWarnings() %>%
                  suppressMessages()

                df_2 <-
                  df %>%
                  filter(countV5 == 1) %>%
                  select(-countV5)

                df_2 <-
                  df_2 %>%
                  purrr::set_names(
                    c(
                      'idRow',
                      'nameCompany',
                      'idTypeInvestment',
                      'idTicker',
                      'priceEndOfMonth',
                      'price52WeekHighLow',
                      'ratioFFOPriceCurrentYearNext',
                      'pershareFFOCurrentYearNextYear',
                      'pctDividendYieldSpread',
                      'pctFFOGrowth'
                    )
                  ) %>%
                  separate(price52WeekHighLow,
                           c('price52WeekHigh', 'price52WeekLow'),
                           sep = '\\ ') %>%
                  separate(
                    ratioFFOPriceCurrentYearNext,
                    c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'),
                    sep = '\\ '
                  ) %>%
                  separate(
                    pctDividendYieldSpread,
                    c('pctDividendYield', 'pctDividendSpread'),
                    sep = '\\ '
                  ) %>%
                  separate(
                    pershareFFOCurrentYearNextYear,
                    c('pershareFFOCurrentYear', 'pershareFFONextYear'),
                    sep = '\\ '
                  ) %>%
                  suppressWarnings() %>%
                  suppressMessages()

                df <-
                  df_2 %>%
                  bind_rows(list(df_1a , df_1b))
                return(df)
              }

              df <-
                df %>%
                purrr::set_names(
                  c(
                    'idRow',
                    'nameCompany',
                    'idTypeInvestment',
                    'idTicker',
                    'priceEndOfMonth',
                    'price52WeekHighLow',
                    'ratioFFOPriceCurrentYearNext',
                    'pershareFFOCurrentYearNextYear',
                    'pctFFOGrowth'
                  )
                )

              df <-
                df %>%
                separate(price52WeekHighLow,
                         c('price52WeekHigh', 'price52WeekLow'),
                         sep = '\\ ') %>%
                separate(
                  ratioFFOPriceCurrentYearNext,
                  c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'),
                  sep = '\\ '
                ) %>%
                separate(
                  pershareFFOCurrentYearNextYear,
                  c('pershareFFOCurrentYear', 'pershareFFONextYear'),
                  sep = '\\ '
                ) %>%
                suppressWarnings() %>%
                suppressMessages()
            }

            return(df)
          }) %>%
      arrange(idRow) %>%
      suppressWarnings() %>%
      select(which(colMeans(is.na(.)) < 1))

    if (!'V10' %in% names(data_reit)) {
      if ('V9' %in% names(data)) {
      data_reit <-
        data_reit %>%
        dplyr::rename(V10 = V9)
      } else {('V8' %in% names(data))
        data_reit <-
          data_reit %>%
          dplyr::rename(V10 = V8)
      }
    }

    df_data <-
      data_reit %>%
      mutate(isNumberData = ifelse(!V1 %>% readr::parse_number() %>% is.na(), TRUE, isNumberData)) %>%
      filter(isNumberData == TRUE) %>%
      select(-isNumberData) %>%
      mutate(idRow = 1:n()) %>%
      mutate(isV10NA = ifelse(V10 %>% is.na(), TRUE, FALSE)) %>%
      select(isV10NA, everything()) %>%
      select(which(colMeans(is.na(.)) < 1)) %>%
      suppressWarnings()


    all_df_data <-
      c(TRUE, FALSE) %>%
      map_df(function(x) {
        has_no_data <-
          df_data %>%
          filter(isV10NA == x) %>% nrow() == 0
        if (has_no_data) {
          return(data_frame())
        }

        df <-
          df_data %>%
          filter(isV10NA == x) %>%
          select(-isV10NA) %>%
          select(which(colMeans(is.na(.)) < 1))

        if (x == TRUE) {
          is_10_col <-
            df %>% ncol() == 10
          is_new_8 <-
            (data %>% ncol() == 8 & df %>% ncol() == 8)

          if (is_new_8) {
            df_1 <-
              df %>%
              mutate(countV7 = V7 %>% str_count('\\ ')) %>%
              filter(countV7 == 0) %>%
              select(-countV7) %>%
              select(which(colMeans(is.na(.)) < 1))

            df_1 <-
              df_1 %>%
              purrr::set_names(c("idRow", 'nameCompany',"pctReturnMonth",
                                 "pctReturnYearToDate", "pctReturn1YRpctReturn2YR3YRpctReturn5YRamountEquityMarketCap",
                                 "amountEquityMarketCapDillutedpctDebtEquity",
                                 "countAVGShareVolumecountAVGDollarVolume",
                                 "ratioRelativeLiquidity")) %>%
              separate(pctReturn1YRpctReturn2YR3YRpctReturn5YRamountEquityMarketCap,
                       c('pctReturn1YR', 'pctReturn2YR' ,'pctReturn3YR', 'pctReturn5YR', 'amountEquityMarketCap'),
                       sep = '\\ ') %>%
              separate(amountEquityMarketCapDillutedpctDebtEquity,
                       c('amountEquityMarketCapDilluted', 'pctDebtEquity'),
                       sep = '\\ ') %>%
              separate(countAVGShareVolumecountAVGDollarVolume,
                       c('countAVGShareVolume', 'countAVGDollarVolume'),
                       sep = '\\ ')

            df_2 <-
              df %>%
              mutate(countV7 = V7 %>% str_count('\\ ')) %>%
              filter(countV7 > 0) %>%
              select(-countV7) %>%
              select(which(colMeans(is.na(.)) < 1)) %>%
              purrr::set_names(c("idRow", 'nameCompany',"pctReturnMonth",
                                 "pctReturnYearToDate", "pctReturn1YR", "pctReturn2YR3YRpctReturn5YRamountEquityMarketCap",
                                 "amountEquityMarketCapDillutedpctDebtEquity",
                                 "countAVGShareVolumecountAVGDollarVolumeratioRelativeLiquidity")) %>%
              separate(pctReturn2YR3YRpctReturn5YRamountEquityMarketCap,
                       c('pctReturn2YR', 'pctReturn3YR', 'pctReturn5YR', 'amountEquityMarketCap'),
                       sep = '\\ ') %>%
              separate(amountEquityMarketCapDillutedpctDebtEquity,
                       c('amountEquityMarketCapDilluted', 'pctDebtEquity'),
                       sep = '\\ ') %>%
              separate(countAVGShareVolumecountAVGDollarVolumeratioRelativeLiquidity,
                       c('countAVGShareVolume', 'countAVGDollarVolume' , 'ratioRelativeLiquidity'),
                       sep = '\\ ')

            df <-
              df_1 %>%
              bind_rows(df_2)

            return(df)
          }

          is_9 <-
            df %>% ncol() == 9

          if (is_9) {
            df_1 <-
              df %>%
              filter(!V8 %>% is.na()) %>%
              select(which(colMeans(is.na(.)) < 1))

            df_1 <-
              df_1 %>%
              purrr::set_names(c("idRow", 'nameCompany',"pctReturnMonth",
                                 "pctReturnYearToDate", "pctReturn1YR", "pctReturn2YR3YRpctReturn5YRamountEquityMarketCap",
                                 "amountEquityMarketCapDillutedpctDebtEquity",
                                 "countAVGShareVolumecountAVGDollarVolume",
                                 "ratioRelativeLiquidity")) %>%
              separate(pctReturn2YR3YRpctReturn5YRamountEquityMarketCap,
                       c('pctReturn2YR', 'pctReturn3YR', 'pctReturn5YR', 'amountEquityMarketCap'),
                       sep = '\\ ') %>%
              separate(amountEquityMarketCapDillutedpctDebtEquity,
                       c('amountEquityMarketCapDilluted', 'pctDebtEquity'),
                       sep = '\\ ') %>%
              separate(countAVGShareVolumecountAVGDollarVolume,
                       c('countAVGShareVolume', 'countAVGDollarVolume'),
                       sep = '\\ ')

            df_2 <-
              df %>%
              filter(V8 %>% is.na()) %>%
              select(which(colMeans(is.na(.)) < 1)) %>%
              purrr::set_names(c("idRow", 'nameCompany',"pctReturnMonth",
                                 "pctReturnYearToDate", "pctReturn1YR", "pctReturn2YR3YRpctReturn5YRamountEquityMarketCap",
                                 "amountEquityMarketCapDillutedpctDebtEquity",
                                 "countAVGShareVolumecountAVGDollarVolumeratioRelativeLiquidity")) %>%
              separate(pctReturn2YR3YRpctReturn5YRamountEquityMarketCap,
                       c('pctReturn2YR', 'pctReturn3YR', 'pctReturn5YR', 'amountEquityMarketCap'),
                       sep = '\\ ') %>%
              separate(amountEquityMarketCapDillutedpctDebtEquity,
                       c('amountEquityMarketCapDilluted', 'pctDebtEquity'),
                       sep = '\\ ') %>%
              separate(countAVGShareVolumecountAVGDollarVolumeratioRelativeLiquidity,
                       c('countAVGShareVolume', 'countAVGDollarVolume' , 'ratioRelativeLiquidity'),
                       sep = '\\ ')

            df <-
              df_1 %>%
              bind_rows(df_2)

            return(df)
          }

          if (is_10_col) {
            df <-
              df %>%
              purrr::set_names(c(
                'idRow',
                'pctDividendYieldSpread',
                'pctReturnMonth',
                'pctReturnYearToDate',
                'pctReturn1YR2YR',
                'pctReturn3YR5YRAmountEquity',
                'amountEquityMarketCapDilluted',
                'pctDebtEquity',
                'countAVGShareVolumeDollarVolume',
                'ratioRelativeLiquidity'
              ))

            df <-
              df %>%
              mutate(countEquityPcts = pctReturn3YR5YRAmountEquity %>% str_count('\\ ')) %>%
              separate(pctDividendYieldSpread,
                       c('pctDividendYield', 'pctDividendSpread'),
                       sep = '\\ ') %>%
              separate(pctReturn1YR2YR, c('pctReturn1YR', 'pctReturn2YR'), sep = '\\ ') %>%
              separate(
                pctReturn3YR5YRAmountEquity,
                c('pctReturn3YR', 'pctReturn5YR', 'amountEquityMarketCap'),
                sep = '\\ '
              ) %>%
              separate(
                countAVGShareVolumeDollarVolume,
                c('countAVGShareVolume', 'countAVGDollarVolume'),
                sep = '\\ '
              ) %>%
              suppressWarnings() %>%
              suppressMessages() %>%
              mutate(
                amountEquityMarketCap = ifelse(countEquityPcts == 0, pctReturn3YR, amountEquityMarketCap),
                amountEquityMarketCap = ifelse(countEquityPcts == 1, pctReturn5YR, amountEquityMarketCap)
              ) %>%
              select(-c(
                pctReturn1YR,
                pctReturn2YR,
                pctReturn3YR,
                pctReturn5YR,
                countEquityPcts
              ))

            return(df)
          }
          is_8 <-
            df %>%
            ncol() == 8

          if (is_8) {
            df <-
              df %>%
              purrr::set_names(c(
                'idRow',
                'pctDividendYieldSpread',
                'pctReturnMonth',
                'pctReturnYearToDate',
                'pctReturn1YR2YR',
                'pctReturn3YR',
                'pctReturn5YR',
                'amountEquityMarketCap'
              )) %>%
              separate(pctDividendYieldSpread,
                       c('pctDividendYield', 'pctDividendSpread'),
                       sep = '\\ ') %>%
              separate(pctReturn1YR2YR,
                       c('pctReturn1YR', 'pctReturn2YR'),
                       sep = '\\ ') %>%
              suppressWarnings()

            return(df)
          }

          is_10 <-
            df %>% ncol() == 10

          df <-
            df %>%
            mutate(countV7 = V7 %>% str_count('\\ '))

          df_0 <-
            df %>%
            filter(countV7 == 0) %>%
            select(-countV7) %>%
            select(which(colMeans(is.na(.)) < 1))

          df_1 <-
            df %>%
            filter(countV7 > 0) %>%
            select(-countV7) %>%
            select(which(colMeans(is.na(.)) < 1))

          df_1 <-
            df_1 %>%
            purrr::set_names(
              c(
                'idRow',
                'pctDividendYieldSpread',
                'pctReturnMonth',
                'pctReturnYearToDate',
                'pctReturn1YR2YR3YR5REquityMarketCap',
                'amountEquityMarketCapDilluted',
                'pctDebtEquity',
                'countAVGShareVolumeDollarVolume',
                'ratioRelativeLiquidity'
              )
            )

          df_1 <-
            df_1 %>%
            mutate(countEquityPcts = pctReturn1YR2YR3YR5REquityMarketCap %>% str_count('\\ ')) %>%
            separate(pctDividendYieldSpread,
                     c('pctDividendYield', 'pctDividendSpread'),
                     sep = '\\ ') %>%
            separate(
              pctReturn1YR2YR3YR5REquityMarketCap,
              c(
                'pctReturn1YR',
                'pctReturn2YR',
                'pctReturn3YR',
                'pctReturn5YR',
                'amountEquityMarketCap'
              ),
              sep = '\\ '
            ) %>%
            separate(
              countAVGShareVolumeDollarVolume,
              c('countAVGShareVolume', 'countAVGDollarVolume'),
              sep = '\\ '
            ) %>%
            suppressWarnings() %>%
            suppressMessages() %>%
            mutate(
              amountEquityMarketCap = ifelse(countEquityPcts == 0, pctReturn1YR, amountEquityMarketCap),
              amountEquityMarketCap = ifelse(countEquityPcts == 1, pctReturn2YR, amountEquityMarketCap),
              amountEquityMarketCap = ifelse(countEquityPcts == 2, pctReturn3YR, amountEquityMarketCap),
              amountEquityMarketCap = ifelse(countEquityPcts == 3, pctReturn5YR, amountEquityMarketCap)
            ) %>%
            select(-c(
              pctReturn1YR,
              pctReturn2YR,
              pctReturn3YR,
              pctReturn5YR,
              countEquityPcts
            ))

          is_10 <-
            df_0 %>% ncol() == 10

          is_9 <-
            df_0 %>% ncol() == 9

          if (is_10) {
            df_0 <-
            df_0 %>%
            purrr::set_names(
              c(
                'idRow',
                'pctDividendYieldSpread',
                'pctReturnMonth',
                'pctReturnYearToDate',
                'pctReturn1YR2YR',
                'pctReturn3YR5YRAmountEquity',
                'amountEquityMarketCapDilluted',
                'pctDebtEquity',
                'countAVGShareVolumeDollarVolume',
                'ratioRelativeLiquidity'
              )
            )

          df_0 <-
            df_0 %>%
            mutate(countEquityPcts = pctReturn3YR5YRAmountEquity %>% str_count('\\ ')) %>%
            separate(pctDividendYieldSpread,
                     c('pctDividendYield', 'pctDividendSpread'),
                     sep = '\\ ') %>%
            separate(pctReturn1YR2YR, c('pctReturn1YR', 'pctReturn2YR'), sep = '\\ ') %>%
            separate(
              pctReturn3YR5YRAmountEquity,
              c('pctReturn3YR', 'pctReturn5YR', 'amountEquityMarketCap'),
              sep = '\\ '
            ) %>%
            separate(
              countAVGShareVolumeDollarVolume,
              c('countAVGShareVolume', 'countAVGDollarVolume'),
              sep = '\\ '
            ) %>%
            suppressWarnings() %>%
            suppressMessages() %>%
            mutate(
              amountEquityMarketCap = ifelse(countEquityPcts == 0, pctReturn3YR, amountEquityMarketCap),
              amountEquityMarketCap = ifelse(countEquityPcts == 1, pctReturn5YR, amountEquityMarketCap)
            ) %>%
            select(-c(
              pctReturn1YR,
              pctReturn2YR,
              pctReturn3YR,
              pctReturn5YR,
              countEquityPcts
            ))
          }

          if (is_9) {
            df_0 <-
              df_0 %>%
              purrr::set_names(
                c(
                  'idRow',
                  'pctDividendYieldSpreadMonth',
                  'pctReturnYearToDate',
                  'pctReturn1YR2YR',
                  'pctReturn3YR5YRAmountEquity',
                  'amountEquityMarketCapDilluted',
                  'pctDebtEquity',
                  'countAVGShareVolumeDollarVolume',
                  'ratioRelativeLiquidity'
                )
              )

            df_0 <-
              df_0 %>%
              mutate(countEquityPcts = pctReturn3YR5YRAmountEquity %>% str_count('\\ ')) %>%
              separate(pctDividendYieldSpreadMonth,
                       c('pctReturnMonth','pctDividendYield', 'pctDividendSpread'),
                       sep = '\\ ') %>%
              separate(pctReturn1YR2YR, c('pctReturn1YR', 'pctReturn2YR'), sep = '\\ ') %>%
              separate(
                pctReturn3YR5YRAmountEquity,
                c('pctReturn3YR', 'pctReturn5YR', 'amountEquityMarketCap'),
                sep = '\\ '
              ) %>%
              separate(
                countAVGShareVolumeDollarVolume,
                c('countAVGShareVolume', 'countAVGDollarVolume'),
                sep = '\\ '
              ) %>%
              suppressWarnings() %>%
              suppressMessages() %>%
              mutate(
                amountEquityMarketCap = ifelse(countEquityPcts == 0, pctReturn3YR, amountEquityMarketCap),
                amountEquityMarketCap = ifelse(countEquityPcts == 1, pctReturn5YR, amountEquityMarketCap)
              ) %>%
              select(-c(
                pctReturn1YR,
                pctReturn2YR,
                pctReturn3YR,
                pctReturn5YR,
                countEquityPcts
              ))
          }

          df <-
            df_0 %>%
            bind_rows(df_1) %>%
            arrange(idRow)
        }
        if (!x == TRUE) {
          is_15 <-
            df %>% ncol() == 15
          is_12 <-
            df %>% ncol() == 12
          is_11 <-
            df %>% ncol() == 11
          is_10 <-
            df %>% ncol() == 10
          is_15 <-
            df %>%
            ncol() == 15

          is_9 <-
            df %>%
            ncol() == 9

          if (is_9) {
            df <-
              df %>%
              mutate(countV10 = V10 %>% str_count('\\ '))

            df_1 <-
              df %>%
              filter(countV10 == 0) %>%
              select(-countV10) %>%
              select(which(colMeans(is.na(.)) < 1)) %>%
              purrr::set_names(
                c(
                  "idRow",
                  'nameCompany',
                  "pctReturnMonth",
                  "pctReturnYearToDate",
                  'pctReturn1YR',
                  'pctReturn2YR3YRpctReturn5YRamountEquityMarketCap',
                  "amountEquityMarketCapDillutedpctDebtEquity",
                  "countAVGShareVolumecountAVGDollarVolume",
                  'ratioRelativeLiquidity'
                )
              ) %>%
              separate(pctReturn2YR3YRpctReturn5YRamountEquityMarketCap,
                       c('pctReturn2YR', 'pctReturn3YR', 'pctReturn5YR', 'amountEquityMarketCap'),
                       sep = '\\ ') %>%
              separate(amountEquityMarketCapDillutedpctDebtEquity,
                       c('amountEquityMarketCapDilluted', 'pctDebtEquity'),
                       sep = '\\ ') %>%
              separate(countAVGShareVolumecountAVGDollarVolume,
                       c('countAVGShareVolume', 'countAVGDollarVolume' ),
                       sep = '\\ ')

            df_2 <-
              df %>%
              filter(countV10 > 0) %>%
              select(-countV10) %>%
              select(which(colMeans(is.na(.)) < 1)) %>%
              purrr::set_names(
                c(
                  "idRow",
                  'nameCompany',
                  "pctReturnMonth",
                  "pctReturnYearToDate",
                  'pctReturn1YR',
                  "pctReturn2YR3YR",
                  'pctReturn5YRamountEquityMarketCap',
                  "amountEquityMarketCapDillutedpctDebtEquity",
                  "countAVGShareVolumecountAVGDollarVolumeratioRelativeLiquidity"
                )
              ) %>%
              separate(pctReturn2YR3YR,
                       c('pctReturn2YR', 'pctReturn3YR'),
                       sep = '\\ ') %>%
              separate(pctReturn5YRamountEquityMarketCap,
                       c('pctReturn5YR', 'amountEquityMarketCap'),
                       sep = '\\ ') %>%
              separate(amountEquityMarketCapDillutedpctDebtEquity,
                       c('amountEquityMarketCapDilluted', 'pctDebtEquity'),
                       sep = '\\ ') %>%
              separate(countAVGShareVolumecountAVGDollarVolumeratioRelativeLiquidity,
                       c('countAVGShareVolume', 'countAVGDollarVolume' , 'ratioRelativeLiquidity'),
                       sep = '\\ ')
            df <-
              df_1 %>%
              bind_rows(df_2)
            return(df)
          }

          if (is_15) {
            df_1 <-
              df %>%
              filter(!V14 %>% is.na()) %>%
              select(which(colMeans(is.na(.)) < 1)) %>%
              purrr::set_names(c("idRow", "pctDividendYield", "pctDividendSpread", "pctReturnMonth",
                                 "pctReturnYearToDate", "pctReturn1YR", "pctReturn2YR", "pctReturn3YR",
                                 "pctReturn5YR", "amountEquityMarketCap", "amountEquityMarketCapDilluted",
                                 "pctDebtEquity", "countAVGShareVolume", "countAVGDollarVolume",
                                 "ratioRelativeLiquidity")
              )

            df_b <-
              df %>%
              filter(!idRow %in% df_1$idRow) %>%
              select(which(colMeans(is.na(.)) < 1))

            df_b1 <-
              df_b %>%
              filter(!V11 %>% is.na()) %>%
              select(which(colMeans(is.na(.)) < 1))

            df_b1 <-
              df_b1 %>%
              purrr::set_names(
                c(
                  "idRow",
                  "pctDividendYieldSpread",
                  "pctReturnMonth",
                  "pctReturnYearToDate",
                  "pctReturn1YR2YR",
                  "pctReturn3YR",
                  'pctReturn5YR',
                  "amountEquityMarketCap",
                  "amountEquityMarketCapDilluted",
                  "pctDebtEquity",
                  "countAVGShareDollarVolume",
                  "ratioRelativeLiquidity"
                )
              )

            df_b1 <-
              df_b1 %>%
              separate(pctDividendYieldSpread,
                       c('pctDividendYield', 'pctDividendSpread'),
                       sep = '\\ ') %>%
              separate(pctReturn1YR2YR,
                       c('pctReturn1YR', 'pctReturn2YR'),
                       sep = '\\ ') %>%
              separate(
                countAVGShareDollarVolume,
                c('countAVGShareVolume', 'countAVGDollarVolume'),
                sep = '\\ '
              ) %>%
              suppressWarnings() %>%
              suppressMessages()


            df_b2 <-
              df_b %>%
              filter(V11 %>% is.na()) %>%
              select(which(colMeans(is.na(.)) < 1))

            df_b2 <-
              df_b2 %>%
              purrr::set_names(
              c(
                "idRow",
                "pctDividendYieldSpread",
                'pctReturnMonth',
                "pctReturnYearToDate",
                "pctReturn1YR2YR",
                "pctReturn3YR5YR",
                "amountEquityMarketCap",
                "amountEquityMarketCapDilluted",
                "pctDebtEquity",
                "countAVGShareDollarVolume",
                "ratioRelativeLiquidity"
              )
            )

            df_b2 <-
              df_b2 %>%
              separate(pctDividendYieldSpread,
                       c('pctDividendYield', 'pctDividendSpread'),
                       sep = '\\ ') %>%
              separate(pctReturn1YR2YR,
                       c('pctReturn1YR', 'pctReturn2YR'),
                       sep = '\\ ') %>%
              separate(pctReturn3YR5YR,
                       c('pctReturn3YR', 'pctReturn5YR'),
                       sep = '\\ ') %>%
              separate(
                countAVGShareDollarVolume,
                c('countAVGShareVolume', 'countAVGDollarVolume'),
                sep = '\\ '
              ) %>%
              suppressWarnings() %>%
              suppressMessages()

            df <-
              df_1 %>%
              bind_rows(list(df_b1, df_b2))

            return(df)
          }

          if (is_12) {

            has_fix <-
              (df$V1 %>% str_count('\\ ') %>% unique() %>% length() > 1) &
              (df$V11 %>% str_detect(".") %>% sum(na.rm = T) / nrow(df) < .45)

            if (has_fix) {
              df <-
                df %>%
                filter(!V2 %>% is.na()) %>%
                unite(V1, V2, V1, sep = ' ') %>%
                purrr::set_names(list("V", 1:11) %>% purrr::invoke(paste0,.))
              has_2 <-
                df %>%
                filter(V2 %>% is.na()) %>%
                select(which(colMeans(is.na(.)) < 1)) %>% nrow() > 0
              if (has_2) {
                df <-
                  df %>%
                bind_rows(
                  df %>%
                    filter(V2 %>% is.na()) %>%
                    select(which(colMeans(is.na(.)) < 1)) %>%
                    purrr::set_names(list("V", 1:11) %>% purrr::invoke(paste0,.))
                )
              }

             df <-
               df %>%
                purrr::set_names(
                  c(
                    "idRow",
                    "pctDividendYieldSpreadMonth",
                    "pctReturnYearToDate",
                    "pctReturn1YR2YR",
                    'pctReturn3YR',
                    'pctReturn5YR',
                    "amountEquityMarketCap",
                    "amountEquityMarketCapDilluted",
                    "pctDebtEquity",
                    "countAVGShareDollarVolume",
                    'ratioRelativeLiquidity'
                  )
                )
             df <-
               df %>%
               separate(pctDividendYieldSpreadMonth,
                        c('pctDividendYield', 'pctDividendSpread', 'pctReturnMonth'),
                        sep = '\\ ') %>%
               separate(pctReturn1YR2YR,
                        c('pctReturn1YR', 'pctReturn2YR'),
                        sep = '\\ ') %>%
               separate(
                 countAVGShareDollarVolume,
                 c('countAVGShareVolume', 'countAVGDollarVolume'),
                 sep = '\\ '
               ) %>%
               suppressWarnings() %>%
               suppressMessages()

             return(df)
            }

            df <-
              df %>%
              purrr::set_names(
                c(
                  "idRow",
                  "pctDividendYieldSpread",
                  "pctReturnMonth",
                  "pctReturnYearToDate",
                  "pctReturn1YR2YR",
                  "pctReturn3YR",
                  'pctReturn5YR',
                  "amountEquityMarketCap",
                  "amountEquityMarketCapDilluted",
                  "pctDebtEquity",
                  "countAVGShareDollarVolume",
                  "ratioRelativeLiquidity"
                )
              )

            df <-
              df %>%
              separate(pctDividendYieldSpread,
                       c('pctDividendYield', 'pctDividendSpread'),
                       sep = '\\ ') %>%
              separate(pctReturn1YR2YR,
                       c('pctReturn1YR', 'pctReturn2YR'),
                       sep = '\\ ') %>%
              separate(
                countAVGShareDollarVolume,
                c('countAVGShareVolume', 'countAVGDollarVolume'),
                sep = '\\ '
              ) %>%
              suppressWarnings() %>%
              suppressMessages()
          }

          if (is_10) {

            is_diff <-
              df %>% mutate(V1Number = V1 %>% readr::parse_number()) %>%
              filter(V1Number %>% is.na()) %>%
              nrow() / nrow(df) > .5

            if (is_diff) {
              df <-
                df %>%
                purrr::set_names(c("idRow", 'nameCompany',"pctReturnMonth",
                                   "pctReturnYearToDate", "pctReturn1YR", "pctReturn2YR3YR",
                                   'pctReturn5YRamountEquityMarketCap',
                                   "amountEquityMarketCapDillutedpctDebtEquity",
                                   "countAVGShareVolumecountAVGDollarVolume",
                                   "ratioRelativeLiquidity")) %>%
                separate(pctReturn2YR3YR,
                         c('pctReturn2YR', 'pctReturn3YR'),
                         sep = '\\ ') %>%
                separate(pctReturn5YRamountEquityMarketCap,
                         c('pctReturn5YR', 'amountEquityMarketCap'),
                         sep = '\\ ') %>%
                separate(amountEquityMarketCapDillutedpctDebtEquity,
                         c('amountEquityMarketCapDilluted', 'pctDebtEquity'),
                         sep = '\\ ') %>%
                separate(countAVGShareVolumecountAVGDollarVolume,
                         c('countAVGShareVolume', 'countAVGDollarVolume'),
                         sep = '\\ ')

                return(df)
            }

            df <-
              df %>%
              purrr::set_names(
                c(
                  "idRow",
                  "pctDividendYieldSpreadMonth",
                  "pctReturnYearToDate",
                  "pctReturn1YR2YR",
                  "pctReturn3YR5YR",
                  "amountEquityMarketCap",
                  "amountEquityMarketCapDilluted",
                  "pctDebtEquity",
                  "countAVGShareDollarVolume",
                  "ratioRelativeLiquidity"
                )
              )

            df <-
              df %>%
              separate(pctDividendYieldSpreadMonth,
                       c('pctReturnMonth','pctDividendYield', 'pctDividendSpread'),
                       sep = '\\ ') %>%
              separate(pctReturn1YR2YR,
                       c('pctReturn1YR', 'pctReturn2YR'),
                       sep = '\\ ') %>%
              separate(pctReturn3YR5YR,
                       c('pctReturn3YR', 'pctReturn5YR'),
                       sep = '\\ ') %>%
              separate(
                countAVGShareDollarVolume,
                c('countAVGShareVolume', 'countAVGDollarVolume'),
                sep = '\\ '
              ) %>%
              suppressWarnings() %>%
              suppressMessages()
            return(df)
          }

          if (is_11) {
            has_na <-
              df %>%
              filter(V2 %>% is.na()) %>%
              nrow() > 0

            if (has_na) {
              is_9 <-
                df %>%
                filter(V2 %>% is.na()) %>%
                select(which(colMeans(is.na(.)) < 1)) %>% ncol() == 9

              if (is_9) {
                df_1 <-
                  df %>%
                  filter(V2 %>% is.na()) %>%
                  select(which(colMeans(is.na(.)) < 1)) %>%
                  purrr::set_names(
                    c(
                      "idRow",
                      "pctDividendYieldSpreadMonth",
                      "pctReturnYearToDate",
                      "pctReturn1YR2YR",
                      'pctReturn3YR5YRamountEquityMarketCap',
                      "amountEquityMarketCapDilluted",
                      "pctDebtEquity",
                      "countAVGShareDollarVolume",
                      'ratioRelativeLiquidity'
                    )
                  ) %>%
                  mutate(countEquityPcts = pctReturn3YR5YRamountEquityMarketCap %>% str_count('\\ ')) %>%
                  separate(pctDividendYieldSpreadMonth,
                           c('pctDividendYield', 'pctDividendSpread', 'pctReturnMonth'),
                           sep = '\\ ') %>%
                  separate(pctReturn1YR2YR,
                           c('pctReturn1YR', 'pctReturn2YR'),
                           sep = '\\ ') %>%
                    separate(pctReturn3YR5YRamountEquityMarketCap,
                             c('pctReturn3YR','pctReturn5YR', 'amountEquityMarketCap'),
                             sep = '\\ ') %>%
                  separate(
                    countAVGShareDollarVolume,
                    c('countAVGShareVolume', 'countAVGDollarVolume'),
                    sep = '\\ '
                  ) %>%
                  suppressWarnings() %>%
                  suppressMessages() %>%
                  suppressMessages() %>%
                  mutate(
                    amountEquityMarketCap = ifelse(countEquityPcts == 1, pctReturn5YR, amountEquityMarketCap),
                    amountEquityMarketCap = ifelse(countEquityPcts == 0, pctReturn3YR, amountEquityMarketCap)
                  ) %>%
                  select(-c(
                    pctReturn1YR,
                    pctReturn2YR,
                    pctReturn3YR,
                    pctReturn5YR,
                    countEquityPcts
                  ))


                df_2_a <-
                  df %>%
                  filter(!V2 %>% is.na()) %>%
                  unite(V1, V1, V2, sep = ' ') %>%
                  select(which(colMeans(is.na(.)) < 1)) %>%
                  filter(!V6 %>% is.na()) %>%
                  purrr::set_names(
                    c(
                      "idRow",
                      "pctDividendYieldSpreadMonth",
                      "pctReturnYearToDate",
                      "pctReturn1YR2YR",
                      'pctReturn3YR5YR',
                      "amountEquityMarketCap",
                      "amountEquityMarketCapDilluted",
                      "pctDebtEquity",
                      "countAVGShareDollarVolume",
                      'ratioRelativeLiquidity'
                    )
                  ) %>%
                  separate(pctDividendYieldSpreadMonth,
                           c('pctDividendYield', 'pctDividendSpread', 'pctReturnMonth'),
                           sep = '\\ ') %>%
                  separate(pctReturn1YR2YR,
                           c('pctReturn1YR', 'pctReturn2YR'),
                           sep = '\\ ') %>%
                  separate(pctReturn3YR5YR,
                           c('pctReturn3YR', 'pctReturn5YR'),
                           sep = '\\ ') %>%
                  separate(
                    countAVGShareDollarVolume,
                    c('countAVGShareVolume', 'countAVGDollarVolume'),
                    sep = '\\ '
                  ) %>%
                  suppressWarnings() %>%
                  suppressMessages()

                df_2_b <-
                  df %>%
                  filter(!V2 %>% is.na()) %>%
                  select(which(colMeans(is.na(.)) < 1)) %>%
                  filter(V6 %>% is.na()) %>%
                  select(which(colMeans(is.na(.)) < 1)) %>%
                  purrr::set_names(
                    c(
                      "idRow",
                      "pctDividendYieldSpread",
                      'pctReturnMonth',
                      "pctReturnYearToDate",
                      "pctReturn1YR2YR",
                      'pctReturn3YR5YRamountEquityMarketCap',
                      "amountEquityMarketCapDilluted",
                      "pctDebtEquity",
                      "countAVGShareDollarVolume",
                      'ratioRelativeLiquidity'
                    )
                  ) %>%
                  mutate(countEquityPcts = pctReturn3YR5YRamountEquityMarketCap %>% str_count('\\ ')) %>%
                  separate(pctDividendYieldSpread,
                           c('pctDividendYield', 'pctDividendSpread'),
                           sep = '\\ ') %>%
                  separate(pctReturn1YR2YR,
                           c('pctReturn1YR', 'pctReturn2YR'),
                           sep = '\\ ') %>%
                  separate(pctReturn3YR5YRamountEquityMarketCap,
                           c('pctReturn3YR', 'pctReturn5YR', 'amountEquityMarketCap'),
                           sep = '\\ ') %>%
                  separate(
                    countAVGShareDollarVolume,
                    c('countAVGShareVolume', 'countAVGDollarVolume'),
                    sep = '\\ '
                  ) %>%
                  suppressWarnings() %>%
                  suppressMessages() %>%
                    mutate(
                      amountEquityMarketCap = ifelse(countEquityPcts == 1, pctReturn5YR, amountEquityMarketCap),
                      amountEquityMarketCap = ifelse(countEquityPcts == 0, pctReturn3YR, amountEquityMarketCap)
                    ) %>%
                    select(-c(
                      pctReturn1YR,
                      pctReturn2YR,
                      pctReturn3YR,
                      pctReturn5YR,
                      countEquityPcts
                    ))
                df <-
                  df_1 %>%
                  bind_rows(list(df_2_a , df_2_b))
                return(df)
              }

              df <-
                df %>%
                filter(V2 %>% is.na()) %>%
                select(which(colMeans(is.na(.)) < 1)) %>%
                purrr::set_names(list("V", 1:10) %>% purrr::invoke(paste0,.)) %>%
                bind_rows(
                  df %>%
                    filter(!V2 %>% is.na()) %>%
                    unite(V1, V1, V2, sep = ' ') %>%
                    select(which(colMeans(is.na(.)) < 1)) %>%
                      purrr::set_names(list("V", 1:10) %>% purrr::invoke(paste0,.))
                )
              df <-
                df %>%
                purrr::set_names(
                  c(
                    "idRow",
                    "pctDividendYieldSpreadMonth",
                    "pctReturnYearToDate",
                    "pctReturn1YR2YR",
                    'pctReturn3YR',
                    'pctReturn5YR',
                    "amountEquityMarketCap",
                    "amountEquityMarketCapDilluted",
                    "pctDebtEquity",
                    "countAVGShareDollarVolume"
                  )
                )

              df <-
                df %>%
                separate(pctDividendYieldSpreadMonth,
                         c('pctDividendYield', 'pctDividendSpread', 'pctReturnMonth'),
                         sep = '\\ ') %>%
                separate(pctReturn1YR2YR,
                         c('pctReturn1YR', 'pctReturn2YR'),
                         sep = '\\ ') %>%
                separate(
                  countAVGShareDollarVolume,
                  c('countAVGShareVolume', 'countAVGDollarVolume'),
                  sep = '\\ '
                ) %>%
                suppressWarnings() %>%
                suppressMessages()

              return(df)
            }

            df <-
              df %>%
              purrr::set_names(
                c(
                  "idRow",
                  "pctDividendYieldSpread",
                  "pctReturnMonth",
                  "pctReturnYearToDate",
                  "pctReturn1YR2YR",
                  "pctReturn3YR5YR",
                  "amountEquityMarketCap",
                  "amountEquityMarketCapDilluted",
                  "pctDebtEquity",
                  "countAVGShareDollarVolume",
                  "ratioRelativeLiquidity"
                )
              )

            df <-
              df %>%
              separate(pctDividendYieldSpread,
                       c('pctDividendYield', 'pctDividendSpread'),
                       sep = '\\ ') %>%
              separate(pctReturn1YR2YR,
                       c('pctReturn1YR', 'pctReturn2YR'),
                       sep = '\\ ') %>%
              separate(pctReturn3YR5YR,
                       c('pctReturn3YR', 'pctReturn5YR'),
                       sep = '\\ ') %>%
              separate(
                countAVGShareDollarVolume,
                c('countAVGShareVolume', 'countAVGDollarVolume'),
                sep = '\\ '
              ) %>%
              suppressWarnings() %>%
              suppressMessages()
          }
          is_14 <-
            df %>% ncol() == 14
          if (is_14) {
            df <-
              df %>%
              mutate(V11Count = V11 %>% str_count('\\ '))

            df_na <-
              df %>%
              filter(V11Count == 0) %>%
              select(which(colMeans(is.na(.)) < 1)) %>%
              select(-V11Count)

            df_na <-
              df_na %>%
              purrr::set_names(
                c(
                  "idRow",
                  "pctDividendYieldSpread",
                  "pctReturnMonth",
                  "pctReturnYearToDate",
                  "pctReturn1YR",
                  'pctReturn2YR',
                  'pctReturn3YR',
                  'pctReturn5YR',
                  "amountEquityMarketCap",
                  "amountEquityMarketCapDilluted",
                  'countAVGDollarVolume',
                  "pctDebtEquity",
                  "countAVGShareVolume",
                  "ratioRelativeLiquidity"
                )
              )

            df_na <-
              df_na %>%
              separate(pctDividendYieldSpread,
                       c('pctDividendYield', 'pctDividendSpread'),
                       sep = '\\ ') %>%
              suppressWarnings() %>%
              suppressMessages()

            df_no_na <-
              df %>%
              filter(V11Count > 0) %>%
              select(which(colMeans(is.na(.)) < 1)) %>%
              select(-V11Count)

            df_no_na <-
              df_no_na %>%
              purrr::set_names(
                c(
                  'idRow',
                  'pctDividendYieldSpread',
                  'pctReturnMonth',
                  'pctReturnYearToDate',
                  'pctReturn1YR2YR',
                  'pctReturn3YR5YR',
                  'amountEquityMarketCap',
                  'amountEquityMarketCapDilluted',
                  'pctDebtEquity',
                  'countAVGShareVolumeDollarVolume',
                  'ratioRelativeLiquidity'
                )
              )

            df_no_na <-
              df_no_na %>%
              separate(pctDividendYieldSpread,
                       c('pctDividendYield', 'pctDividendSpread'),
                       sep = '\\ ') %>%
              separate(pctReturn1YR2YR,
                       c('pctReturn1YR', 'pctReturn2YR'),
                       sep = '\\ ') %>%
              separate(pctReturn3YR5YR,
                       c('pctReturn3YR', 'pctReturn5YR'),
                       sep = '\\ ') %>%
              separate(
                countAVGShareVolumeDollarVolume,
                c('countAVGShareVolume', 'countAVGDollarVolume'),
                sep = '\\ '
              ) %>%
              suppressWarnings() %>%
              suppressMessages()

            df <-
              df_na %>%
              bind_rows(df_no_na) %>%
              arrange(idRow)
          }
        }

        return(df)
      }) %>%
      arrange(idRow) %>%
      suppressWarnings()

    all_data <-
      all_descriptions %>%
      left_join(all_df_data) %>%
      suppressWarnings() %>%
      suppressMessages() %>%
      select(-idRow)

    all_data <-
      all_data %>%
      mutate_at(all_data %>% select(matches(
        "^price|^pershare|^count|^pct|^amount|^ratio"
      )) %>% names(),
      funs(. %>% readr::parse_number())) %>%
      mutate_at(all_data %>% select(matches("^amount")) %>% names(),
                funs(. * 1000000)) %>%
      mutate_at(all_data %>% select(matches("^count")) %>% names(),
                funs(. * 1000)) %>%
      mutate_at(all_data %>% select(matches("^pct")) %>% names(),
                funs((. / 100))) %>%
      mutate_at(all_data %>% select(matches("^count")) %>% names(),
                funs(. %>% comma(digits = 0))) %>%
      mutate_at(all_data %>% select(matches("^price|^pershare")) %>% names(),
                funs(. %>%  currency(digits = 2))) %>%
      mutate_at(all_data %>% select(matches("^amount")) %>% names(),
                funs(.  %>%  currency(digits = 0))) %>%
      suppressWarnings()

    all_data <-
      all_data %>%
      mutate(
        pct52WeekHigh = (priceEndOfMonth / price52WeekHigh) %>% formattable::percent()
      ) %>%
      select(
        nameCompany:idTicker,
        pct52WeekHigh,
        amountEquityMarketCapDilluted,
        amountEquityMarketCap,
        everything()
      ) %>%
      suppressWarnings() %>%
      mutate(
        amountDebt = (pctDebtEquity * amountEquityMarketCapDilluted) %>% formattable::currency(digits = 0),
        amountEnterpriseValue = ifelse(
          amountDebt %>% is.na(),
          amountEquityMarketCapDilluted,
          amountEquityMarketCapDilluted + amountDebt
        ),
        pctLeverage = (amountDebt / amountEnterpriseValue) %>% formattable::percent(digits = 2)
      ) %>%
      select(
        nameCompany,
        idTicker,
        idTypeInvestment,
        priceEndOfMonth,
        price52WeekLow,
        pct52WeekHigh,
        amountEnterpriseValue,
        pctLeverage,
        amountDebt,
        everything()
      )

    all_data <-
      all_data %>%
      mutate_at(all_data %>% select(matches("^pct")) %>% names(),
                funs(. %>% formattable::percent(digits = 2)))

    return(all_data)

  }

parse_rw_14 <-
  function(data) {
    data <-
      data %>%
      mutate_all(funs(. %>% str_trim() %>% str_to_upper())) %>%
      mutate(idRow = 1:n())

    subsector_df <-
      data %>%
      filter((V2 == '') & (V3 == '') & (V4 == '')) %>%
      filter(!V1 %>% str_detect("AVERAGE")) %>%
      filter(!V1 %>% str_detect("^[0-9]")) %>%
      select(idRow, nameSubSector = V1) %>%
      mutate(idRow = idRow + 1) %>%
      filter(!nameSubSector == '')

    all_data <-
      data %>%
      filter(!V1 %>% str_detect("AVERAGE|YIELD SPREAD")) %>%
      filter(!V1 == 'DIVIDEND') %>%
      filter(!V1 %in% c('', "NAME", 'REIT NAME')) %>%
      filter(!((V2 == '') & (V3 == '') & (V4 == '')))

    all_data <-
      all_data %>%
      left_join(subsector_df) %>%
      fill(nameSubSector) %>%
      select(-idRow) %>%
      select(nameSubSector, everything()) %>%
      suppressWarnings() %>%
      suppressMessages()

    all_data <-
      all_data %>%
      mutate(idRow = 1:n()) %>%
      mutate(isNumberData = ifelse(!V1 %>% substr(1,2) %>% as.numeric() %>% abs() %>% is.na(),
                                   TRUE, FALSE)) %>%
      select(idRow, isNumberData, everything()) %>%
      suppressWarnings() %>%
      mutate_all(funs(ifelse(. == '', NA, .))) %>%
      mutate(isNumberData = ifelse(V1 %>% is.na(), TRUE, isNumberData))

    wrong_column <-
      all_data %>%
      filter(isNumberData == FALSE) %>%
      mutate(idRow = 1:n()) %>%
      select(which(colMeans(is.na(.)) < 1)) %>%
      mutate(isV2NA = ifelse(V2 %>% is.na(), TRUE, FALSE)) %>%
      select(-isNumberData) %>%
      select(isV2NA, everything()) %>% nrow() / nrow(all_data) == 1

    if (wrong_column) {
      all_data <-
        all_data %>%
        mutate(idRow = 1:n()) %>%
        mutate(isNumberData = ifelse(!V2 %>% as.numeric() %>% abs() %>%  substr(1,1) %>% is.na(),
                                     TRUE, FALSE)) %>%
        select(idRow, isNumberData, everything()) %>%
        suppressWarnings() %>%
        mutate_all(funs(ifelse(. == '', NA, .))) %>%
        mutate(isNumberData = ifelse(V1 %>% is.na(), TRUE, isNumberData))

    }

    is_mixed <-
      all_data %>% mutate(V2 = V2 %>% readr::parse_number) %>% filter(!V2 %>% is.na()) %>% nrow() / nrow(data_reit) > .1

    if (is_mixed) {
      all_data <-
        all_data %>%
        mutate_all(funs(ifelse(. == "NA", NA, .))) %>%
        mutate(isNumberData = ifelse(!V2 %>% readr::parse_number() %>% is.na(), T, F),
               isNumberData = ifelse((V2 %>% is.na()) & (V3 %>% is.na()) & (V4 %>% is.na()),
                                     TRUE, isNumberData
               ))
    }

    df_descriptions <-
      all_data %>%
      filter(isNumberData == FALSE) %>%
      mutate(idRow = 1:n()) %>%
      select(which(colMeans(is.na(.)) < 1)) %>%
      mutate(isV2NA = ifelse(V2 %>% is.na(), TRUE, FALSE)) %>%
      select(-isNumberData) %>%
      select(isV2NA, everything())

    all_descriptions <-
      c(TRUE, FALSE) %>%
      map_df(function(x){
        no_data <-
          df_descriptions %>%
          filter(isV2NA == x) %>%
          select(-isV2NA) %>% nrow() == 0

        if (no_data) {
          return(data_frame())
        }

        df <-
          df_descriptions %>%
          filter(isV2NA == x) %>%
          select(-isV2NA) %>%
          select(which(colMeans(is.na(.)) < 1))

        is_8 <-
          df %>% ncol() == 8

        is_9 <-
          df %>% ncol == 9

        if (is_8) {
          df <-
            df %>%
            purrr::set_names(c('idRow','nameCompany', 'idTypeInvestment',
                               'idTickerpriceEndOfMonth', 'price52WeekHighLow',
                               'ratioFFOPriceCurrentYearNext',
                               'pershareFFOCurrentYearNextYear',
                               'pctFFOGrowth'))

          df <-
            df %>%
            separate(idTickerpriceEndOfMonth, c('idTicker', 'priceEndOfMonth'), sep = '\\ ') %>%
            separate(price52WeekHighLow, c('price52WeekHigh', 'price52WeekLow'), sep = '\\ ') %>%
            separate(ratioFFOPriceCurrentYearNext, c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
            separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
            suppressWarnings() %>%
            suppressMessages()
        }

        is_12 <-
          df %>% ncol() == 12

        if (is_12) {
          df <-
            df %>%
            mutate(V3Count = V3 %>% str_count('\\ '))

          all_df <-
            df$V3Count %>%
            unique() %>%
            map_df(function(x){
              data <-
                df %>%
                filter(V3Count == x) %>%
                select(-V3Count) %>%
                select(which(colMeans(is.na(.)) < 1))

              if (x == 2) {
                data <-
                  data %>%
                  purrr::set_names(c('idRow', 'nameSubSector','nameCompany','idTicker', 'priceEndOfMonth52WeekHighLow',
                                     'ratioFFOPriceCurrentYearNext',
                                     'pershareFFOCurrentYearNextYear',
                                     'pctFFOGrowth', 'ratioDebtEBITDA', 'pctFFOPayOut','pctDividendYieldSpread'))

                data <-
                  data %>%
                  separate(priceEndOfMonth52WeekHighLow, c('priceEndOfMonth','price52WeekHigh', 'price52WeekLow'), sep = '\\ ') %>%
                  separate(ratioFFOPriceCurrentYearNext, c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
                  separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
                  separate(pctDividendYieldSpread, c('pctDividendYield', 'pctDividendSpread'), sep = '\\ ')

                return(data)
              }

              if (x == 0) {
                data <-
                  data %>%
                  purrr::set_names(c('idRow', 'nameSubSector','nameCompany','idTicker','priceEndOfMonth', 'price52WeekHighLow',
                                     'ratioFFOPriceCurrentYearNext',
                                     'pershareFFOCurrentYearNextYear',
                                     'pctFFOGrowth', 'ratioDebtEBITDA', 'pctFFOPayOut','pctDividendYieldSpread'))

                data <-
                  data %>%
                  separate(price52WeekHighLow, c('price52WeekHigh', 'price52WeekLow'), sep = '\\ ') %>%
                  separate(ratioFFOPriceCurrentYearNext, c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
                  separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
                  separate(pctDividendYieldSpread, c('pctDividendYield', 'pctDividendSpread'), sep = '\\ ')
                return(data)
              }
            })

          return(all_df)
        }

        is_14 <-
          df %>% ncol() == 14

        if (is_14) {

          is_whittle <-
            df %>% mutate_all(funs(ifelse(. == 'NA', NA, .))) %>%
            select(which(colMeans(is.na(.)) < 1)) %>%
            ncol() < 14

          if (is_whittle) {
            df <-
              df %>%
              mutate_all(funs(ifelse(. == 'NA', NA, .))) %>%
              select(which(colMeans(is.na(.)) < 1))

            is_9 <-
              df %>% ncol() == 9

            if (is_9) {
              df <-
                df %>%
                purrr::set_names(c('idRow','nameCompany', 'idTypeInvestment', 'idTicker', 'priceEndOfMonth', 'price52WeekHigh', 'price52WeekLow', 'pcdtDividendYield', 'pctDividendSpread'))
              return(df)
            }

          }
          if(!is_whittle) {
            df <-
            df %>%
            mutate(V3Count = V3 %>% str_count('\\ '))

          all_df <-
            df$V3Count %>%
            unique() %>%
            map_df(function(x){
              data <-
                df %>%
                filter(V3Count == x) %>%
                select(-V3Count) %>%
                select(which(colMeans(is.na(.)) < 1))

              if (x == 2) {
                data <-
                  data %>%
                  purrr::set_names(c('idRow', 'nameSubSector','nameCompany','idTicker', 'priceEndOfMonth52WeekHighLow',
                                     'ratioFFOPriceCurrentYearNext',
                                     'pershareFFOCurrentYearNextYear',
                                     'pctFFOGrowth', 'ratioDebtEBITDA', 'pctFFOPayOut','pctDividendYieldSpread'))

                data <-
                  data %>%
                  separate(priceEndOfMonth52WeekHighLow, c('priceEndOfMonth','price52WeekHigh', 'price52WeekLow'), sep = '\\ ') %>%
                  separate(ratioFFOPriceCurrentYearNext, c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
                  separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
                  separate(pctDividendYieldSpread, c('pctDividendYield', 'pctDividendSpread'), sep = '\\ ')

                return(data)
              }

              if (x == 0) {
                is_12 <-
                  data %>% ncol() == 12
                if (is_12) {
                  data <-
                    data %>%
                    purrr::set_names(c('idRow', 'nameSubSector','nameCompany','idTicker','priceEndOfMonth', 'price52WeekHighLow',
                                       'ratioFFOPriceCurrentYearNext',
                                       'pershareFFOCurrentYearNextYear',
                                       'pctFFOGrowth', 'ratioDebtEBITDA', 'pctFFOPayOut','pctDividendYieldSpread'))

                  data <-
                    data %>%
                    separate(price52WeekHighLow, c('price52WeekHigh', 'price52WeekLow'), sep = '\\ ') %>%
                    separate(ratioFFOPriceCurrentYearNext, c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
                    separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
                    separate(pctDividendYieldSpread, c('pctDividendYield', 'pctDividendSpread'), sep = '\\ ')
                }

                is_14 <-
                  data %>% ncol() == 14

                if (is_14) {
                  df_a <-
                    data %>%
                    filter(!V6 %>% is.na()) %>%
                    select(which(colMeans(is.na(.)) < 1)) %>%
                    purrr::set_names(c('idRow', 'nameSubSector','nameCompany','idTicker','priceEndOfMonth', 'price52WeekHighLow',
                                       'ratioFFOPriceCurrentYearNext',
                                       'pershareFFOCurrentYearNextYear',
                                       'pctFFOGrowth', 'ratioDebtEBITDA', 'pctFFOPayOut','pctDividendYieldSpread'))

                  df_a <-
                    df_a %>%
                    separate(price52WeekHighLow, c('price52WeekHigh', 'price52WeekLow'), sep = '\\ ') %>%
                    separate(ratioFFOPriceCurrentYearNext, c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
                    separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
                    separate(pctDividendYieldSpread, c('pctDividendYield', 'pctDividendSpread'), sep = '\\ ')

                  df_b <-
                    data %>%
                    filter(V6 %>% is.na()) %>%
                    select(which(colMeans(is.na(.)) < 1)) %>%
                    purrr::set_names(c('idRow', 'nameSubSector','nameCompany','idTicker','priceEndOfMonth', 'price52WeekHighLow',
                                       'ratioFFOPriceCurrentYearNext',
                                       'pershareFFOCurrentYearNextYear',
                                       'pctFFOGrowth', 'ratioDebtEBITDA', 'pctFFOPayOut','pctDividendYieldSpread'))

                  df_b <-
                    df_b %>%
                    separate(price52WeekHighLow, c('price52WeekHigh', 'price52WeekLow'), sep = '\\ ') %>%
                    separate(ratioFFOPriceCurrentYearNext, c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
                    separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
                    separate(pctDividendYieldSpread, c('pctDividendYield', 'pctDividendSpread'), sep = '\\ ')

                  data <-
                    df_a %>%
                    bind_rows(df_b)
                }

                return(data)
              }
            })

          return(all_df)
          }
        }

        if (is_9) {
          is_2 <-
            'nameSubSector' %in% names(df)

          if (is_2) {
            df <-
              df %>%
              purrr::set_names(c('idRow','nameSubSector','nameCompany', 'idTypeInvestment',
                                 'idExchangeTicker', 'priceEndOfMonth52WeekHighLow',
                                 'ratioFFOPriceCurrentYearNext',
                                 'pershareFFOCurrentYearNextYear',
                                 'pctFFOGrowth'))

            df <-
              df %>%
              separate(idExchangeTicker, c('idExchange', 'idTicker'), sep = '\\ ') %>%
              separate(priceEndOfMonth52WeekHighLow, c('priceEndOfMonth','price52WeekHigh', 'price52WeekLow'), sep = '\\ ') %>%
              separate(ratioFFOPriceCurrentYearNext, c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
              separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
              suppressWarnings() %>%
              suppressMessages() %>%
              suppressWarnings()
            return(df)
          }

          if (!is_2) {
            df <-
            df %>%
            purrr::set_names(c('idRow','nameCompany', 'idTypeInvestment',
                               'idTicker', 'priceEndOfMonth',
                               'price52WeekHighLow',
                               'ratioFFOPriceCurrentYearNext',
                               'pershareFFOCurrentYearNextYear',
                               'pctFFOGrowth'))

          df <-
            df %>%
            separate(price52WeekHighLow, c('price52WeekHigh', 'price52WeekLow'), sep = '\\ ') %>%
            separate(ratioFFOPriceCurrentYearNext, c('ratioFFOPriceCurrent', 'ratioFFOPriceYearNext'), sep = '\\ ') %>%
            separate(pershareFFOCurrentYearNextYear, c('pershareFFOCurrentYear', 'pershareFFONextYear'), sep = '\\ ') %>%
            suppressWarnings() %>%
            suppressMessages()
          }
        }

        return(df)
      }) %>%
      arrange(idRow)

    df_data <-
      all_data %>%
      filter(isNumberData == TRUE) %>%
      select(-isNumberData) %>%
      mutate(idRow = 1:n()) %>%
      mutate(isV11_1 = ifelse(V10 %>% str_count('\\ ') == 1, TRUE, FALSE)) %>%
      select(isV11_1, everything())

    df_data$isV11_1[df_data$isV11_1 %>% is.na()] <-
      TRUE

    all_df_data <-
      c(TRUE, FALSE) %>%
      map_df(function(x){
        if (df_data %>%
            filter(isV11_1 == x) %>% nrow() == 0) {
          return(data_frame())
        }

        df <-
          df_data %>%
          filter(isV11_1 == x) %>%
          select(-isV11_1) %>%
          select(which(colMeans(is.na(.)) < 1))

        if (x == TRUE) {
          has_11 <-
            df %>% ncol() == 11

          has_12 <-
            df %>% ncol() == 12

          has_13 <-
            df %>% ncol() == 13

          if (has_11) {
            has_v9_na <-
              df %>%
              filter(V9 %>% is.na()) %>%
              nrow() > 0

            if (has_v9_na) {
              df_na <-
                df %>%
                filter(V9 %>% is.na())

              df_na <-
                df_na %>%
                select(which(colMeans(is.na(.)) < 1)) %>%
                purrr::set_names(c('idRow','nameSubSector','pctDividendYieldSpread',
                                 'pctReturnMonth', 'pctReturnYearToDate', 'pctReturn1YR2YR3YR5YRMarketCap',
                                 'amountEquityMarketCapDilluted', 'pctDebtEquity','countAVGShareVolumeDollarVolume', 'ratioRelativeLiquidity'))

              df_na <-
                df_na %>%
                separate(pctDividendYieldSpread, c('pctDividendYield', 'pctDividendSpread'), sep = '\\ ') %>%
                separate(pctReturn1YR2YR3YR5YRMarketCap, c('pctReturn1YR', 'pctReturn2YR', 'pctReturn3YR', 'pctReturn5YR', 'amountEquityMarketCap'), sep = '\\ ') %>%
                separate(countAVGShareVolumeDollarVolume, c('countAVGShareVolume', 'countAVGDollarVolume'), sep = '\\ ') %>%
                suppressWarnings()


              df_no_na <-
                df %>%
                select(which(colMeans(is.na(.)) < 1)) %>%
                filter(!V9 %>% is.na())

              df_no_na <-
                df_no_na %>%
                purrr::set_names(c('idRow','nameSubSector',
                                   'pctDividendYieldSpread','pctReturnMonth', 'pctReturnYearToDate', 'pctReturn1YR2YR', 'pctReturn3YR5YREquity',
                                   'amountEquityMarketCapDilluted', 'pctDebtEquity','countAVGShareVolumeDollarVolume', 'ratioRelativeLiquidity'))

              df_no_na <-
                df_no_na %>%
                separate(pctDividendYieldSpread, c('pctDividendYield', 'pctDividendSpread'), sep = '\\ ') %>%
                separate(pctReturn1YR2YR, c('pctReturn1YR', 'pctReturn2YR'), sep = '\\ ') %>%
                separate(pctReturn3YR5YREquity, c('pctReturn3YR', 'pctReturn5YR', 'amountEquityMarketCap'), sep = '\\ ') %>%
                separate(countAVGShareVolumeDollarVolume, c('countAVGShareVolume', 'countAVGDollarVolume'), sep = '\\ ') %>%
                suppressWarnings() %>%
                suppressMessages()

              df <-
                df_na %>%
                bind_rows(df_no_na)

              return(df)

            }

            df <-
              df %>%
              purrr::set_names(c('idRow','pctDividendYieldSpread',
                                 'pctReturnMonth', 'pctReturnYearToDate', 'pctReturn1YR2YR', 'pctReturn3YR5YR',
                                 'amountEquityMarketCap',
                                 'amountEquityMarketCapDilluted', 'pctDebtEquity','countAVGShareVolumeDollarVolume', 'ratioRelativeLiquidity'))

            df <-
              df %>%
              separate(pctDividendYieldSpread, c('pctDividendYield', 'pctDividendSpread'), sep = '\\ ') %>%
              separate(pctReturn1YR2YR, c('pctReturn1YR', 'pctReturn2YR'), sep = '\\ ') %>%
              separate(pctReturn3YR5YR, c('pctReturn3YR', 'pctReturn5YR'), sep = '\\ ') %>%
              separate(countAVGShareVolumeDollarVolume, c('countAVGShareVolume', 'countAVGDollarVolume'), sep = '\\ ') %>%
              suppressWarnings() %>%
              suppressMessages()
          }

          if (has_12) {
            is_na_v10 <-
              df %>%
              filter(!V10 %>% is.na()) %>%
              nrow() > 0

            if(!is_na_v10) {
              df <-
              df %>%
              purrr::set_names(c('idRow','pctDividendYieldSpread',
                                 'pctReturnMonth', 'pctReturnYearToDate',
                                 'pctReturn1YR2YR3YR5amountEquityMarketCap',
                                 'amountEquityMarketCapDilluted', 'pctDebtEquity','countAVGShareVolumeDollarVolume', 'ratioRelativeLiquidity',
                                 'X1')
                               )

            df <-
              df %>%
              mutate(amountEquityMarketCap = ifelse(amountEquityMarketCap %>% is.na(), pctReturn5YR, amountEquityMarketCap)) %>%
              select(-pctReturn5YR)

            df <-
              df %>%
              separate(pctDividendYieldSpread, c('pctDividendYield', 'pctDividendSpread'), sep = '\\ ') %>%
              separate(pctReturn1YR2YR, c('pctReturn1YR', 'pctReturn2YR'), sep = '\\ ') %>%
              separate(countAVGShareVolumeDollarVolume, c('countAVGShareVolume', 'countAVGDollarVolume'), sep = '\\ ') %>%
              suppressWarnings() %>%
              suppressMessages()
            }
          }

          if (has_13) {
            is_mixed <-
              df %>% filter(V11 %>% is.na()) %>% nrow() / nrow(df) > 0

            if (is_mixed) {
              df_1 <-
                df %>%
                filter(V11 %>% is.na()) %>%
                select(which(colMeans(is.na(.)) < 1)) %>%
                purrr::set_names(c("idRow", 'nameSubSector',"pctDividendYieldSpread", "pctReturnMonth",
                                   "pctReturnYearToDate", "pctReturn1YR2YR","pctReturn3YR5YRamountEquityMarketCap",
                                   "amountEquityMarketCapDilluted",
                                   "pctDebtEquity", "countAVGShareDollarVolume",
                                   "ratioRelativeLiquidity")
                ) %>%
                separate(pctDividendYieldSpread, c('pctDividendYield', 'pctDividendSpread'), sep = '\\ ') %>%
                separate(pctReturn1YR2YR, c('pctReturn1YR', 'pctReturn2YR'), sep = '\\ ') %>%
                separate(pctReturn3YR5YRamountEquityMarketCap, c('pctReturn3YR', 'pctReturn5YR', 'amountEquityMarketCap'), sep = '\\ ') %>%
                separate(countAVGShareDollarVolume, c('countAVGShareVolume', 'countAVGDollarVolume'), sep = '\\ ') %>%
                suppressWarnings() %>%
                suppressMessages()

              df_2 <-
                df %>%
                filter(!V11 %>% is.na()) %>%
                filter(!V7 %>% is.na()) %>%
                select(which(colMeans(is.na(.)) < 1)) %>%
                purrr::set_names(c("idRow", 'nameSubSector',"pctDividendYieldSpread", "pctReturnMonth",
                                   "pctReturnYearToDate", "pctReturn1YR2YR","pctReturn3YR",
                                   'pctReturn5YR',
                                   "amountEquityMarketCap", "amountEquityMarketCapDilluted",
                                   "pctDebtEquity", "countAVGShareDollarVolume",
                                   "ratioRelativeLiquidity")
                ) %>%
              separate(pctDividendYieldSpread, c('pctDividendYield', 'pctDividendSpread'), sep = '\\ ') %>%
                separate(pctReturn1YR2YR, c('pctReturn1YR', 'pctReturn2YR'), sep = '\\ ') %>%
                separate(countAVGShareDollarVolume, c('countAVGShareVolume', 'countAVGDollarVolume'), sep = '\\ ') %>%
                suppressWarnings() %>%
                bind_rows(
                  df %>%
                    filter(!V11 %>% is.na()) %>%
                    filter(V7 %>% is.na()) %>%
                    select(which(colMeans(is.na(.)) < 1)) %>%
                    purrr::set_names(c("idRow", 'nameSubSector',"pctDividendYieldSpread", "pctReturnMonth",
                                       "pctReturnYearToDate", "pctReturn1YR2YR","pctReturn3YR5YR",
                                       "amountEquityMarketCap", "amountEquityMarketCapDilluted",
                                       "pctDebtEquity", "countAVGShareDollarVolume",
                                       "ratioRelativeLiquidity")
                    ) %>%
                    separate(pctDividendYieldSpread, c('pctDividendYield', 'pctDividendSpread'), sep = '\\ ') %>%
                    separate(pctReturn1YR2YR, c('pctReturn1YR', 'pctReturn2YR'), sep = '\\ ') %>%
                    separate(pctReturn3YR5YR, c('pctReturn3YR', 'pctReturn5YR'), sep = '\\ ') %>%
                    separate(countAVGShareDollarVolume, c('countAVGShareVolume', 'countAVGDollarVolume'), sep = '\\ ') %>%
                    suppressWarnings() %>%
                    suppressMessages()
                )

              df <-
                df_1 %>%
                bind_rows(df_2)

              return(df)
            }

            df <-
              df %>%
              purrr::set_names(c('idRow','nameSubSector','pctDividendYieldSpread',
                                 'pctReturnMonth', 'pctReturnYearToDate', 'pctReturn1YR2YR', 'pctReturn3YR',
                                 'pctReturn5YR',
                                 'amountEquityMarketCap',
                                 'amountEquityMarketCapDilluted', 'pctDebtEquity','countAVGShareVolumeDollarVolume', 'ratioRelativeLiquidity'))

            df <-
              df %>%
              mutate(amountEquityMarketCap = ifelse(amountEquityMarketCap %>% is.na(), pctReturn5YR, amountEquityMarketCap)) %>%
              select(-pctReturn5YR)

            df <-
              df %>%
              separate(pctDividendYieldSpread, c('pctDividendYield', 'pctDividendSpread'), sep = '\\ ') %>%
              separate(pctReturn1YR2YR, c('pctReturn1YR', 'pctReturn2YR'), sep = '\\ ') %>%
              separate(countAVGShareVolumeDollarVolume, c('countAVGShareVolume', 'countAVGDollarVolume'), sep = '\\ ') %>%
              suppressWarnings() %>%
              suppressMessages()
          }


        }
        if (!x == TRUE) {
          no_v14 <-
            !'V14' %in% names(df)
          if (no_v14) {
            is_12 <-
              df %>% ncol() == 12
            is_11 <-
              df %>% ncol() == 11
            if (is_12) {
              df <-
                df %>%
                purrr::set_names(c("idRow", 'nameSubSector',"pctDividendYieldSpread", "pctReturnMonth",
                                 "pctReturnYearToDate", "pctReturn1YR2YR","pctReturn3YR5YR",
                                 "amountEquityMarketCap", "amountEquityMarketCapDilluted",
                                 "pctDebtEquity", "countAVGShareDollarVolume",
                                 "ratioRelativeLiquidity")
              )

              df <-
                df %>%
                separate(pctDividendYieldSpread, c('pctDividendYield', 'pctDividendSpread'), sep = '\\ ') %>%
                separate(pctReturn1YR2YR, c('pctReturn1YR', 'pctReturn2YR'), sep = '\\ ') %>%
                separate(pctReturn3YR5YR, c('pctReturn3YR', 'pctReturn5YR'), sep = '\\ ') %>%
                separate(countAVGShareDollarVolume, c('countAVGShareVolume', 'countAVGDollarVolume'), sep = '\\ ') %>%
                suppressWarnings() %>%
                suppressMessages()

              return(df)
            }
            if (is_11) {
              df <-
                df %>%
                purrr::set_names(c("idRow", 'nameSubSector',"pctDividendYieldSpreadReturnMonth",
                                   "pctReturnYearToDate", "pctReturn1YR2YR","pctReturn3YR5YR",
                                   "amountEquityMarketCap", "amountEquityMarketCapDilluted",
                                   "pctDebtEquity", "countAVGShareDollarVolume",
                                   'ratioRelativeLiquidity'
                                   )
                )

              df <-
                df %>%
                separate(pctDividendYieldSpreadReturnMonth, c('pctDividendYield', 'pctDividendSpread', 'pctReturnMonth'), sep = '\\ ') %>%
                separate(pctReturn1YR2YR, c('pctReturn1YR', 'pctReturn2YR'), sep = '\\ ') %>%
                separate(pctReturn3YR5YR, c('pctReturn3YR', 'pctReturn5YR'), sep = '\\ ') %>%
                separate(countAVGShareDollarVolume, c('countAVGShareVolume', 'countAVGDollarVolume'), sep = '\\ ') %>%
                suppressWarnings() %>%
                suppressMessages()

              return(df)
            }
          }

          df <-
            df %>%
            mutate(V14NA = ifelse(V14 %>% is.na(), T, F)) %>%
            select(V14NA, everything())
          has_na <-
            df %>%
            filter(V14NA == T) %>% nrow() > 0
          if (has_na) {
            df_na <-
              df %>%
              filter(V14NA == T) %>%
              select(-V14NA) %>%
              select(which(colMeans(is.na(.)) < 1))

            is_11 <-
              df_na %>% ncol() == 11

            is_15 <-
              df_na %>% ncol() == 15

            is_12 <-
              df_na %>% ncol() == 12

            if (is_12) {
              df_na <-
              df_na %>%
                purrr::set_names(c("idRow", 'nameSubSector',"pctDividendYieldSpread", "pctReturnMonth",
                                   "pctReturnYearToDate", "pctReturn1YR2YR","pctReturn3YR5YR",
                                   "amountEquityMarketCap", "amountEquityMarketCapDilluted",
                                   "pctDebtEquity", "countAVGShareDollarVolume",
                                   "ratioRelativeLiquidity")
                ) %>%
                separate(pctDividendYieldSpread, c('pctDividendYield', 'pctDividendSpread'), sep = '\\ ') %>%
                separate(pctReturn1YR2YR, c('pctReturn1YR', 'pctReturn2YR'), sep = '\\ ') %>%
                separate(pctReturn3YR5YR, c('pctReturn3YR', 'pctReturn5YR'), sep = '\\ ') %>%
                separate(countAVGShareDollarVolume, c('countAVGShareVolume', 'countAVGDollarVolume'), sep = '\\ ') %>%
                suppressWarnings() %>%
                suppressMessages()

              df_no_na <-
                df %>%
                filter(V14NA == F) %>%
                select(-V14NA) %>%
                select(which(colMeans(is.na(.)) < 1)) %>%
                purrr::set_names(
                  c(
                    "idRow",
                    "nameSubSector",
                    "pctDividendYield",
                    "pctDividendSpread",
                    "pctReturnMonth",
                    "pctReturnYearToDate",
                    "pctReturn1YR",
                    "pctReturn2YR",
                    "pctReturn3YR",
                    "pctReturn5YR",
                    "amountEquityMarketCap",
                    "amountEquityMarketCapDilluted",
                    "pctDebtEquity",
                    "countAVGShareVolume",
                    "countAVGDollarVolume",
                    "ratioRelativeLiquidity"
                  )
                )


              df <-
                df_na %>%
                bind_rows(df_no_na)

              return(df)
            }

            if (is_11) {
              df_na <-
              df_na %>%
              purrr::set_names(c("idRow", "pctDividendYieldSpread", "pctReturnMonth",
                                 "pctReturnYearToDate", "pctReturn1YR2YR","pctReturn3YR5YR",
                                 "amountEquityMarketCap", "amountEquityMarketCapDilluted",
                                 "pctDebtEquity", "countAVGShareDollarVolume",
                                 "ratioRelativeLiquidity")
              )

            df_na <-
              df_na %>%
              separate(pctDividendYieldSpread, c('pctDividendYield', 'pctDividendSpread'), sep = '\\ ') %>%
              separate(pctReturn1YR2YR, c('pctReturn1YR', 'pctReturn2YR'), sep = '\\ ') %>%
              separate(pctReturn3YR5YR, c('pctReturn3YR', 'pctReturn5YR'), sep = '\\ ') %>%
              separate(countAVGShareDollarVolume, c('countAVGShareVolume', 'countAVGDollarVolume'), sep = '\\ ') %>%
              suppressWarnings() %>%
              suppressMessages()

            }

            if (is_15) {
              df_na <-
                df_na %>%
                purrr::set_names(c("idRow", 'nameSubSector',"nameCompany", "pctReturnMonth",
                                   "pctReturnYearToDate", "pctReturn1YR", "pctReturn2YR", "pctReturn3YR",
                                   "pctReturn5YR", "amountEquityMarketCap", "amountEquityMarketCapDilluted",
                                   "pctDebtEquity", "countAVGShareVolume", "countAVGDollarVolume",
                                   "ratioRelativeLiquidity"))
            }

            df_no_na <-
              df %>%
              filter(V14NA == F) %>%
              select(-V14NA) %>%
              select(which(colMeans(is.na(.)) < 1))

            is_16 <-
              df_no_na %>% ncol() == 16
            is_15 <-
              df_no_na %>% ncol() == 15

            if (is_16) {
              df_no_na <-
                df_no_na %>%
                purrr::set_names(
                  c(
                    "idRow",
                    "nameSubSector",
                    'nameCompany',
                    "pctReturnMonth",
                    "pctReturnYearToDate",
                    "pctReturn1YR",
                    "pctReturn2YR",
                    "pctReturn3YR",
                    'pctReturn5YR',
                    "amountEquityMarketCap",
                    "amountEquityMarketCapDilluted",
                    "pctDebtEquity",
                    'idCreditRating',
                    "countAVGShareVolume",
                    'countAVGDollarVolume',
                    "ratioRelativeLiquidity"
                  ))
            }

            if (is_15) {
              df_no_na <-
                df_no_na %>%
                purrr::set_names(
                  c(
                    "idRow",
                    "pctDividendYield",
                    'pctDividendSpread',
                    "pctReturnMonth",
                    "pctReturnYearToDate",
                    "pctReturn1YR",
                    "pctReturn2YR",
                    "pctReturn3YR",
                    'pctReturn5YR',
                    "amountEquityMarketCap",
                    "amountEquityMarketCapDilluted",
                    "pctDebtEquity",
                    "countAVGShareVolume",
                    'countAVGDollarVolume',
                    "ratioRelativeLiquidity"
                  )
                )
            }

            df <-
              df_na %>%
              bind_rows(df_no_na) %>%
              arrange(idRow)
            return(df)
          }

          if (!has_na) {
            df <-
              df %>%
              select(-V14NA)

            is_16 <-
              df %>% ncol() == 16
            if (is_16) {
              df <-
                df %>%
                purrr::set_names(c("idRow",'nameSubSector', 'nameCompany',"pctReturnMonth",
                                   "pctReturnYearToDate", "pctReturn1YR", "pctReturn2YR","pctReturn3YR",
                                   'pctReturn5YR',
                                   "amountEquityMarketCap", "amountEquityMarketCapDilluted",
                                   "pctDebtEquity", 'idCreditRating',"countAVGShareVolume", 'countAVGDollarVolume',
                                   "ratioRelativeLiquidity"))
              return(df)
            }
          }

        }

        return(df)
      }) %>%
      arrange(idRow)

    resolve_name <-
      'nameCompany' %in% names(all_descriptions) & 'nameCompany' %in% names(all_df_data)

    if (resolve_name) {
      all_data <-
        all_descriptions %>%
        select(-idRow) %>%
        left_join(all_df_data %>% select(-matches("idRow|nameSubSector"))) %>%
        suppressWarnings() %>%
        suppressMessages()
    }

    if(!resolve_name) {
      all_data <-
        all_descriptions %>%
        left_join(all_df_data %>% select(-matches("nameCompany"))) %>%
        suppressWarnings() %>%
        suppressMessages() %>%
        select(-idRow)
    }

    all_data <-
      all_data %>%
      mutate_at(all_data %>% select(matches(
        "^price|^pershare|^count|^pct|^amount|^ratio"
      )) %>% names(),
      funs(. %>% readr::parse_number())) %>%
      mutate_at(all_data %>% select(matches("^amount")) %>% names(),
                funs(. * 1000000)) %>%
      mutate_at(all_data %>% select(matches("^count")) %>% names(),
                funs(. * 1000)) %>%
      mutate_at(all_data %>% select(matches("^pct")) %>% names(),
                funs((. / 100))) %>%
      mutate_at(all_data %>% select(matches("^count")) %>% names(),
                funs(. %>% comma(digits = 0))) %>%
      mutate_at(all_data %>% select(matches("^price|^pershare")) %>% names(),
                funs(. %>%  currency(digits = 2))) %>%
      mutate_at(all_data %>% select(matches("^amount")) %>% names(),
                funs(.  %>%  currency(digits = 0))) %>%
      suppressWarnings()

    all_data <-
      all_data %>%
      mutate(
        pct52WeekHigh = (priceEndOfMonth / price52WeekHigh) %>% formattable::percent()
      ) %>%
      select(
        nameCompany:idTicker,
        pct52WeekHigh,
        amountEquityMarketCapDilluted,
        amountEquityMarketCap,
        everything()
      ) %>%
      suppressWarnings() %>%
      mutate(
        amountDebt = (pctDebtEquity * amountEquityMarketCapDilluted) %>% formattable::currency(digits = 0),
        amountEnterpriseValue = ifelse(
          amountDebt %>% is.na(),
          amountEquityMarketCapDilluted,
          amountEquityMarketCapDilluted + amountDebt
        ),
        pctLeverage = (amountDebt / amountEnterpriseValue) %>% formattable::percent(digits = 2)
      ) %>%
      select(
        nameCompany,
        idTicker,
        priceEndOfMonth,
        price52WeekLow,
        pct52WeekHigh,
        amountEnterpriseValue,
        pctLeverage,
        amountDebt,
        everything()
      )

    all_data <-
      all_data %>%
      mutate_at(all_data %>% select(matches("^pct")) %>% names(),
                funs(. %>% formattable::percent(digits = 2)))

    return(all_data)
  }

parse_for_reit_monthly_metrics <-
  function(url = 'https://www.reit.com/sites/default/files/reitwatch/RW1612.pdf',
           pages = 34:38,
           use_guess = TRUE,
           return_message = TRUE) {

    all_data <-
      import_rw_pdf(url = url,
                    pages = pages,
                    use_guess = use_guess)

    is_over_15 <-
      all_data %>%
      ncol() >= 15

    is_14 <-
      all_data %>% ncol() == 14

    is_14_old <-
      (all_data %>%
      ncol() == 14) & (pages %>% length() > 8)

    is_14_new <-
      (all_data %>%
         ncol() == 14) & (pages %>% length() <= 8)

    is_13 <-
      all_data %>% ncol() == 13

    is_10 <-
      all_data %>% ncol() %in% c(7:12)

    if (is_over_15) {
      all_data <-
        all_data %>%
        parse_over_15() %>%
        suppressWarnings() %>%
        mutate(urlData = url) %>%
        select(-matches("^V"))

      all_data <-
        all_data %>%
        mutate_at(all_data %>% select(matches("amount")) %>% names(),
                  funs(. %>% formattable::currency())
                  )

      if (return_message) {
        list("Parsed: ", url) %>%
          purrr::invoke(paste0, .) %>% message()
      }

      return(all_data)
    }

    if (is_10) {
      all_data <-
        all_data %>%
        parse_rw_10() %>%
        suppressWarnings() %>%
        mutate(urlData = url) %>%
        select(which(colMeans(is.na(.)) < 1)) %>%
        select(-matches("^V"))

      all_data <-
        all_data %>%
        mutate_at(all_data %>% select(matches("amount")) %>% names(),
                  funs(. %>% formattable::currency(digits = 0)))

      if (return_message) {
        list("Parsed: ", url) %>%
          purrr::invoke(paste0, .) %>% message()
      }

      return(all_data)
    }

    if (is_13) {
      all_data <-
        all_data %>%
        parse_rw_13() %>%
        suppressWarnings() %>%
        mutate(urlData = url) %>%
        select(which(colMeans(is.na(.)) < 1)) %>%
        select(-matches("^V"))

      all_data <-
        all_data %>%
        mutate_at(all_data %>% select(matches("amount")) %>% names(),
                  funs(. %>% formattable::currency(digits = 0)))

      if (return_message) {
        list("Parsed: ", url) %>%
          purrr::invoke(paste0, .) %>% message()
      }

      return(all_data)
    }

    if (is_14) {
      all_data <-
        all_data %>%
        parse_rw_14() %>%
        select(-matches("^V")) %>%
        suppressWarnings() %>%
        mutate(urlData = url) %>%
        select(nameCompany,
               idTicker,
               priceEndOfMonth,
               price52WeekLow,
               pct52WeekHigh,
               amountEnterpriseValue,
               pctLeverage,
               amountDebt,
               everything()) %>%
      mutate(urlData = url) %>%
      select(which(colMeans(is.na(.)) < 1))

      all_data <-
        all_data %>%
        mutate_at(all_data %>% select(matches("amount")) %>% names(),
                  funs(. %>% formattable::currency(digits = 0)))


      if (return_message) {
        list("Parsed: ", url) %>%
          purrr::invoke(paste0, .) %>% message()
      }
      return(all_data)
    }


  }

