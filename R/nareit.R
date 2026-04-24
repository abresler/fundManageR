
# constituency ------------------------------------------------------------
.get_nareit_constiutent_urls <-
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
      tibble(yearData = year,
               urlData = urls)

    return(df)
  }

.parse_nareit_constituent_url <-
  function(url = "https://www.reit.com/sites/default/files/returns/FNUSIC2016.pdf",
           return_message = TRUE) {
    success <- function(res) {
      url <-
        res$url
      df_metadata <-
        url %>%
        tabulapdf::extract_metadata() %>%
        as_tibble()

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
          list_c()

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
        tabulapdf::extract_tables(1:df_metadata$pages)

      all_data <-
        seq_along(tables) %>%
        future_map_dfr(function(x) {
          tables[[x]] %>%
            as_tibble() %>%
            mutate(idTable = x)

        })

      data_cols <-
        all_data %>% ncol() %>%
        as.numeric()

      if (data_cols == 5) {
        all_data <-
          all_data %>%
          select(-dplyr::matches("idTable")) %>%
          purrr::set_names(c('nameCompany',
                             'idTicker',
                             'typeInvestment',
                             'nameSector')) %>%
          mutate(across(everything(), ~str_trim(.) %>% str_to_upper())) %>%
          mutate(idRow = seq_len(n()))

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
          mutate(across(everything(), ~str_trim(.) %>% str_to_upper()))

        all_data <-
          all_data %>%
          filter(!idTicker == '') %>%
          filter(!idTicker %>% str_detect("TICKER|SYMBOLS")) %>%
          mutate(
            typeInvestment =  ifelse(typeInvestment == '', NA, typeInvestment),
            idSubSector =  ifelse(idSubSector == '', NA, idSubSector),
            amountEquityMarketCap = (amountEquityMarketCap %>% readr::parse_number() * 1000000) %>% formattable::currency(digits = 0),
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
          mutate(idRow = seq_len(n()))

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
          mutate(across(everything(), str_to_upper)) %>%
          fill(nameSector) %>%
          select(-idRow) %>%
          select(nameCompany, idTicker, nameSector, everything()) %>%
          mutate(
            amountEquityMarketCap = (amountEquityMarketCap %>% readr::parse_number() * 1000000) %>% formattable::currency(digits = 0),
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
          mutate(idRow = seq_len(n()),
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
          mutate(across(everything(), ~str_trim(.) %>% str_to_upper())) %>%
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
            amountEquityMarketCap = (amountEquityMarketCap %>% readr::parse_number() * 1000000) %>% formattable::currency(digits = 0)
          )

        all_data <-
          all_data %>%
          select(-dplyr::matches("urlData")) %>%
          mutate(urlData = url) %>%
          mutate(dateFile = date_data,
                 yearData = year_data) %>%
          select(yearData, dateFile, everything())
      }

      if (return_message) {
        .fm_data_acquired(n_rows = nrow(all_data), source = "NAREIT", extra = paste("Year:", year_data))
      }

      state$rows[[length(state$rows) + 1L]] <- all_data
    }
    failure <- function(msg) {
      invisible(NULL)
    }
    state <- new.env(parent = emptyenv())
    state$rows <- list()
    url %>%
      purrr::walk(function(x) {
        curl_fetch_multi(url = x, success, failure)
      })
    multi_run()
    dplyr::bind_rows(state$rows)
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
#' @return nested \code{tibble} or \code{tibble} if \code{nest_data = FALSE}
#' @export
#' @family NAREIT
#' @family index constituents
#' @import purrr stringr dplyr rvest formattable lubridate tidyr readr curl tabulapdf
#' @examples
#' nareit_constituent_years(years = 1991:2016, resolve_names = TRUE, nest_data = TRUE, return_message = TRUE)
nareit_constituent_years <-
  function(years = 2010:2016,
           resolve_names = TRUE,
           nest_data = TRUE,
           return_message = TRUE) {
    if (is.null(years) || length(years) == 0) {
      stop("Please enter years, they can start in 1991")
    }
    url_df <-
      .get_nareit_constiutent_urls()

    urls <-
      url_df %>%
      filter(yearData %in% years) %>%
      .$urlData

    .parse_nareit_constituent_url_safe <-
      purrr::possibly(.parse_nareit_constituent_url, tibble())

    all_data <-
      urls %>%
      future_map_dfr(function(x) {
        x %>% cat(fill = TRUE)
        .parse_nareit_constituent_url_safe(url = x, return_message = return_message)
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
        ) %>% do.call(paste0, .)
      ) %>%
      mutate(amountEquityMarketCap = amountEquityMarketCap %>% formattable::currency(digits = 0))

    replace_number <-
      list("^", 1:99, " ") %>%
      do.call(paste0, .) %>%
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
            mutate(idRow = seq_len(n())) %>%
            group_by(idTicker) %>%
            filter(idRow == max(idRow)) %>%
            ungroup() %>%
            select(-idRow)
        ) %>%
        suppressMessages() %>%
        select(yearData:idTicker, nameSector, everything()) %>%
        suppressWarnings()

      if ('nameSubSector' %in% names(all_data)) {
        all_data <-
          all_data %>%
          select(-nameSubSector) %>%
          left_join(
            all_data %>%
              select(idTicker, nameSubSector) %>%
              distinct() %>%
              mutate(nameSubSector = nameSubSector %>% str_trim()) %>%
              filter(!nameSubSector %>% is.na()) %>%
              mutate(idRow = seq_len(n())) %>%
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
        count(nameCompany, sort = TRUE) %>%
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
        mutate(idRow = seq_len(n())) %>%
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
        all_data$dateFile %>% min(na.rm = TRUE),
        ' to ',
        all_data$dateFile %>% max(na.rm = TRUE)
      ) %>%
        do.call(paste0, .) %>%
        cat(fill = TRUE)
    }
    if (nest_data) {
      all_data <-
        all_data %>%
        nest(dataConstituents = -c(yearData, dateFile))
    }

    return(all_data)
  }


# reits -------------------------------------------------------------------
.dictionary_nareit_names <-
  function() {
    tibble(nameNAREIT = c("Company Type", "Listing Status", "Industry Sector", "Investment Sector",
                              "Ticker", "Exchange", "Previous Day Close", "Current Day High & Low",
                              "Day's Volume", "30 Day Average Volume", "52 Week High & Low",
                              "Market Cap", "Prev. Quarter Dividend", "Yield", "Prev. Quarter FFO"
    )
    ,
    nameActual  = c("typeCompany", "statusListing", "sectorCompany", "typeInvestment",
                    "idTicker", "nameExchangeSlug", "pricePreviousDay", "highlowDay",
                    "volumeDay", "volume30Day", "highlow52Week",
                    "amountMarketCapitalization", "dividendQuarterPrior", "pctYield", "amountFFOQuarterPrior"
    )
    )
  }
.parse_page_item <-
  function(page, item = "priceCompany", css = ".price", is_number = FALSE) {
    values <-
      page %>%
      html_nodes(css) %>%
      html_text() %>%
      str_trim()

    if (is_number) {
      values <-
        values %>%
        str_replace_all("Stock Price: ", "") %>%
        readr::parse_number() %>% suppressWarnings()
    }

    tibble(item, value = values)
  }

.parse_nareit_page <-
  function(url = "https://www.reit.com/investing/reit-directory?page=1") {
    page <-
      url %>%
      read_html()

    company <-
      page %>%
      .parse_page_item(item = "nameCompany", css = ".name a") %>%
      mutate(idRow = seq_len(n())) %>%
      pivot_wider(names_from = item, values_from = value)


    url_company <-
      page %>%
      html_nodes('.name a') %>%
      html_attr('href') %>%
      str_c("https://www.reit.com", .)

    sector <-
      page %>%
      .parse_page_item(item = "sectorCompany", css = ".sector") %>%
      mutate(idRow = seq_len(n())) %>%
      pivot_wider(names_from = item, values_from = value)

    ticker <-
      page %>%
      .parse_page_item(item = "idTicker", css = ".ticker") %>%
      mutate(idRow = seq_len(n())) %>%
      pivot_wider(names_from = item, values_from = value)

    address <-
      page %>%
      .parse_page_item(item = "addressCompany", css = ".address") %>%
      mutate(idRow = seq_len(n())) %>%
      pivot_wider(names_from = item, values_from = value)

    last_price <-
      page %>%
      .parse_page_item(item = "priceLast",
                      css = ".price",
                      is_number = TRUE) %>%
      mutate(idRow = seq_len(n())) %>%
      pivot_wider(names_from = item, values_from = value)

    returns <-
      page %>% html_nodes(".return") %>% html_text() %>% str_trim() %>%
      str_replace_all("1 Mo. Total Return: ", "")

    df_returns <-
      tibble(returns) %>%
      tidyr::separate(returns,
                      into = c("pctReturnMonth", "pctReturnDay"),
                      sep = "\\ ") %>%
      mutate(across(everything(), readr::parse_number)) %>%
      mutate(pctReturnMonth = pctReturnMonth / 100,
             pctReturnDay = pctReturnDay / 100) %>%
      mutate(idRow = seq_len(n()))

    df <-
      list(company, sector, ticker, address, last_price, df_returns) %>%
      purrr::reduce(left_join) %>%
      mutate(urlCompanyNAREIT = url_company,
             dateData = Sys.Date(),
             urlNAREITPage = url) %>%
      select(-idRow) %>%
      select(dateData, sectorCompany, everything()) %>%
      suppressMessages() %>%
      suppressWarnings()

    df

  }

.parse_nareit_pages <-
  function(urls = c("https://www.reit.com/investing/reit-directory?page=1", "https://www.reit.com/investing/reit-directory?page=2"), return_message = TRUE) {
    state <- new.env(parent = emptyenv())
    state$rows <- list()
    success <- function(res) {
      url <-
        res$url

      if (return_message) {
        glue::glue("Parsing {url}") %>%
          cat(fill = TRUE)
      }
      .parse_nareit_page.safe <-
        purrr::possibly(.parse_nareit_page, tibble())

      all_data <-
        .parse_nareit_page.safe(url = url)

      state$rows[[length(state$rows) + 1L]] <- all_data
    }
    failure <- function(msg){
      invisible(NULL)
    }
    urls %>%
      purrr::walk(function(x){
        curl_fetch_multi(url = x, success, failure)
      })
    multi_run()

    dplyr::bind_rows(state$rows)
  }


.parse_nareit_entity_page <-
  function(url = "https://www.reit.com/investing/reit-directory/alexandria-real-estate-equities-inc") {
    page <-
      url %>%
      read_lines() %>%
      str_c(collapse = "") %>%
      read_html()
    company_ticker <-
      page %>%
      html_nodes(".name") %>%
      html_text() %>% str_trim() %>%
      str_split("\n|                      ") %>%
      list_c() %>%
      str_trim() %>%
      discard(~ .x == "") %>%
      str_replace_all("\\(|\\)", "")

    item <-  c("nameCompany", "idTicker")
    df_metadata <-
      tibble(item = item[seq_along(company_ticker)], value = company_ticker) %>%
      pivot_wider(names_from = item, values_from = value)

    description <-
      page %>%
      .parse_page_item(item = "descriptionCompany", css = ".investor__body")

    if (description %>% length() > 0) {
      df_metadata <-
        df_metadata %>%
        mutate(descriptionCompany = description %>% pull(value))
    }


    price <-
      page %>%
      .parse_page_item(item = "priceStock",
                      css = ".value",
                      is_number = TRUE) %>%
      pull(value)

    if (price %>% length() > 0) {
      df_metadata <-
        df_metadata %>%
        mutate(priceLast = price %>% as.integer())
    }

    returns <-
      page %>% html_nodes(".values span") %>% html_text() %>% map_dbl(readr::parse_number)


    if (returns %>% length() > 0) {
      items <- c("pctReturnMonth", "pctReturnDay")
      df_ret <- tibble(item = items[seq_along(returns)],
                           value = returns) %>%
        pivot_wider(names_from = item, values_from = value)
      if (df_ret %>% tibble::has_name("pctReturnMonth")) {
        df_ret <- df_ret %>% mutate(pctReturnMonth = pctReturnMonth / 100)
      }

      if (df_ret %>% tibble::has_name("pctReturnDay")) {
        df_ret <- df_ret %>% mutate(pctReturnDay = pctReturnDay / 100)
      }
      df_metadata <-
        df_metadata %>%
        bind_cols(df_ret)
    }

    overview_items <-
      page %>% html_nodes(".reit-values__title") %>% html_text() %>% str_trim()

    overview_values <-
      page %>% html_nodes(".reit-values__value") %>% html_text() %>% str_trim()

    df_names <-
      .dictionary_nareit_names()

    actual_names <-
      overview_items %>%
      map_chr(function(item){
        df_names %>%
          filter(nameNAREIT == item) %>%
          pull(nameActual)
      })

    data <-
      tibble(item = actual_names, value = overview_values) %>%
      pivot_wider(names_from = item, values_from = value) %>%
      select(any_of(actual_names))

    if (data %>% has_name("nameExchangeSlug")) {
      data <-
        data %>%
        tidyr::separate(nameExchangeSlug, into = c("nameExchange", "slugExchange"), sep = "\\(") %>%
        mutate(slugExchange = slugExchange %>% str_replace_all("\\)", ""))
    }

    if (data %>% has_name("pctYield")) {
      data <-
        data %>%
        mutate(pctYield = pctYield %>% readr::parse_number() / 100)
    }

    if (data %>% has_name("highlowDay")) {
      data <-
        data %>%
        separate(
          highlowDay,
          into = c("priceLowDay", "priceHighDay"),
          sep = "\\-",
          convert = T
        ) %>%
        mutate(across(c("priceLowDay", "priceHighDay"), ~readr::parse_number(.)))
    }

    if (data %>% has_name("highlow52Week")) {
      data <-
        data %>%
        separate(
          highlow52Week,
          into = c("price52WeekLow", "price52WeekHigh"),
          sep = "\\-",
          convert = T
        ) %>%
        mutate(across(c("price52WeekLow", "price52WeekHigh"), ~readr::parse_number(.)))
    }

    if (data %>% has_name("amountMarketCapitalization")) {
      data <- data %>%
        mutate(
          amountMarketCapitalization = case_when(
            amountMarketCapitalization %>% str_to_lower() %>% str_detect("bil") ~ amountMarketCapitalization %>% readr::parse_number() * 1000000000,
            amountMarketCapitalization %>% str_to_lower() %>% str_detect("mil") ~ amountMarketCapitalization %>% readr::parse_number() * 1000000,
            TRUE ~ amountMarketCapitalization %>% readr::parse_number()
          )
        )
    }

    if (data %>% has_name("amountFFOQuarterPrior")) {
      data <- data %>%
        mutate(
          amountFFOQuarterPrior = case_when(
            amountFFOQuarterPrior %>% str_to_lower() %>% str_detect("bil") ~ amountFFOQuarterPrior %>% readr::parse_number() * 1000000000,
            amountFFOQuarterPrior %>% str_to_lower() %>% str_detect("mil") ~ amountFFOQuarterPrior %>% readr::parse_number() * 1000000,
            TRUE ~ amountFFOQuarterPrior %>% readr::parse_number()
          )
        )
    }
    num_names <-
      names(data)[names(data) %in% c("pricePreviousDay",
                                     "dividendQuarterPrior",
                                     "volumeDay",
                                     "volume30Day")]
    if (num_names %>% length() > 0) {
      data <-
        data %>%
        mutate(across(all_of(num_names), ~readr::parse_number(.)))
    }

    data <-
      data %>%
      mutate(across(where(is.character), str_trim))

    data <-
      df_metadata %>%
      left_join(data) %>%
      suppressMessages()

    urlCompany <-
      case_when(
        page %>% html_nodes(".reit-company-address__website a") %>% html_attr("href") %>% length() > 0 ~ page %>% html_nodes(".reit-company-address__website a") %>% html_attr("href"),
        TRUE ~ NA_character_
      )

    addressStreetCompany <-
      page %>% html_nodes(".reit-company-address__line-1") %>% html_text() %>% str_trim()
    suite <- page %>% html_nodes(".reit-company-address__line-2")

    if (suite %>% length() > 0) {
      suiteCompany <-
        suite %>% html_text() %>% str_trim()
    } else {
      suiteCompany <- NA
    }

    cityCompany <-
      page %>% html_nodes(".reit-company-address__city") %>% html_text() %>% str_trim() %>% str_replace_all("\\,", "") %>% str_trim()

    stateCompany <-
      page %>% html_nodes(".reit-company-address__state") %>% html_text() %>% str_trim()
    zipCodeCompany <-
      page %>% html_nodes(".reit-company-address__zip") %>% html_text() %>% str_trim()

    phone <-
      page %>% html_nodes(".reit-company-address__phone")

    if (phone %>% length() > 0) {
      phoneCompany <-
        phone %>% html_text() %>% str_trim()
    } else {
      phoneCompany <- NA
    }

    data <-
      data %>%
      mutate(
        addressStreetCompany,
        suiteCompany,
        cityCompany,
        stateCompany,
        zipCodeCompany,
        phoneCompany,
        urlCompany
      )


    contacts <-
      page %>% html_nodes(".reit-company-contact__name") %>% html_text() %>% str_trim()

    if (contacts %>% length() > 0) {
      titles <-
        page %>% html_nodes(".reit-company-contact__title") %>% html_text() %>% str_trim()

      df_contacts <-
        tibble(titlePerson = titles, namePerson = contacts) %>%
        separate_rows(namePerson, sep = "\\, ") %>%
        separate_rows(titlePerson, sep = "\\ & ") %>%
        mutate(across(everything(), str_trim))

      data <-
        data %>%
        mutate(dataContacts = list(df_contacts))
    }

    news <-
      page %>% html_nodes(".news-container a") %>% html_attr('href')


    if (news %>% length() > 0) {
      titles <-
        page %>% html_nodes(".news-container a") %>% html_text() %>% str_trim()

      dates <-
        page %>% html_nodes(".news-detail .date") %>% html_text() %>% str_trim() %>% lubridate::mdy()

      df_news <-
        tibble(
          dateArticle = dates,
          titleArticle = titles,
          urlArticle = news
        )

      data <-
        data %>%
        mutate(dataNews = list(df_news))
    }

    has_properties <-
      page %>% html_nodes(".reit-icon__chevron-right") %>% length() > 0

    if (has_properties) {
      states <- page %>% html_nodes(".reit-icon__chevron-right")
      df_properties <-
        seq_along(states) %>%
        future_map_dfr(function(x) {
          nameState <- states[x] %>% html_text()
          state_slug <-
            states[x] %>% html_attr("href") %>% substr(1, 3)
          tenant_css <-
            glue::glue("{state_slug}-properties .tenant")
          city_css <-
            glue::glue("{state_slug}-properties .location")
          tenants <- page %>% html_nodes(tenant_css) %>% html_text()
          cities <- page %>% html_nodes(city_css) %>% html_text()

          tibble(addressStreetProperty = tenants,
                     cityProperty = cities) %>%
            slice(2:nrow(.)) %>%
            separate_rows(addressStreetProperty, sep = "              ") %>%
            mutate(across(everything(), str_trim)) %>%
            mutate(stateProperty = nameState) %>%
            mutate(
              addressProperty = glue::glue(
                "{addressStreetProperty}, {cityProperty}, {stateProperty}"
              ) %>% as.character()
            )

        })

      df_properties <-
        df_properties %>%
        filter(!addressStreetProperty == "")

      data <-
        data %>%
        mutate(dataPortfolio = list(df_properties))
    }
    data %>%
      mutate(urlCompanyNAREIT = url)

  }

.parse_nareit_entity_pages <-
  function(urls = c("https://www.reit.com/investing/reit-directory/weyerhaeuser",
                    "https://www.reit.com/investing/reit-directory/rlj-lodging-trust",
                    "https://www.reit.com/investing/reit-directory/rayonier-inc",
                    "https://www.reit.com/investing/reit-directory/pebblebrook-hotel-trust",
                    "https://www.reit.com/investing/reit-directory/corenergy-infrastructure-trust",
                    "https://www.reit.com/investing/reit-directory/host-hotels-resorts-inc",
                    "https://www.reit.com/investing/reit-directory/hersha-hospitality-trust",
                    "https://www.reit.com/investing/reit-directory/vici-properties-inc"),
           return_message = TRUE) {
    state <- new.env(parent = emptyenv())
    state$rows <- list()
    success <- function(res) {
      url <-
        res$url

      if (return_message) {
        glue::glue("Parsing {url}") %>%
          cat(fill = TRUE)
      }
      .parse_nareit_entity_page.safe <-
        purrr::possibly(.parse_nareit_entity_page, tibble())

      all_data <-
        .parse_nareit_entity_page.safe(url = url)

      state$rows[[length(state$rows) + 1L]] <- all_data
    }
    failure <- function(msg){
      invisible(NULL)
    }
    urls %>%
      purrr::walk(function(x){
        curl_fetch_multi(url = x, success, failure)
      })
    multi_run()

    df <- dplyr::bind_rows(state$rows)
    df <-
      df %>%
      mutate(hasPortfolioData = dataPortfolio %>% map_dbl(length) > 0,
             hasContactData = dataContacts %>% map_dbl(length) > 0)
    df
  }

#' NAREIT constituents
#'
#' This function returns information about all active
#' NAREIT members.
#'
#'
#' @param parse_member_data if \code{TRUE} parses information about the member companies including
#' company metadata, company properties, company contacts and more
#' @param return_message \code{TRUE} return a message after data import
#'
#' @return a \code{tibble}
#' @export
#' @references \href{http://nareit.org}{National Association of Real Estate Investment Trusts}
#' @return tibble
#' @family NAREIT
#' @family entity search
#' @export
#' @import purrr stringr dplyr rvest lubridate tidyr readr
#' @examples
#' \dontrun{
#' nareit_entities(parse_member_data = TRUE, return_message = TRUE)
#' }

nareit_entities <-
  function(parse_member_data = TRUE, return_message = TRUE) {
    page_count_nodes <- ".pager__item--last a"
    base_url <-
      "https://www.reit.com/investing/reit-directory?field_rtc_listing_status_tid_selective[]=524&field_address_country_selective[]=US&sort_by=title"
    page <-
      base_url %>%
      read_html()

    pages <-
      page %>%
      html_node(page_count_nodes) %>%
      html_attr("href") %>%
      str_split("page=") %>%
      list_c() %>%
      .[[2]] %>% as.numeric()

    urls <-
      glue::glue("{base_url}&page={1:pages}") %>% as.character()

    urls <-
      c(base_url, urls)

    all_data <-
      .parse_nareit_pages(urls = urls, return_message = return_message) %>%
      distinct()

    all_data <-
      all_data %>%
      separate(
        addressCompany,
        into = c("cityCompany", "stateCompany"),
        sep = "\\, ",
        remove = F
      ) %>%
      mutate(across(where(is.character), str_trim))

    all_data <-
      all_data %>%
      mutate(across(where(is.character), ~if_else(. == "N/A", NA_character_, .))) %>%
      arrange(nameCompany)

    all_data <-
      all_data %>%
      mutate(isPublicCompany = !idTicker %>% is.na()) %>%
      select(dateData, isPublicCompany, everything())

    if (!parse_member_data) {
      return(all_data)
    }

    company_urls <- all_data$urlCompanyNAREIT

    df_companies <-
      .parse_nareit_entity_pages(urls = company_urls, return_message = return_message)

    all_data <-
      all_data %>%
      select(-any_of(
        c(
          "sectorCompany",
          "nameCompany",
          "cityCompany",
          "stateCompany",
          "priceLast",
          "idTicker", "pctReturnMonth", "pctReturnDay"
        )
      )) %>%
      left_join(df_companies) %>%
      suppressMessages() %>%
      mutate(across(where(is.character), ~if_else(. == "N/A", NA_character_, .))) %>%
      mutate(across(where(is.character), ~if_else(. == "", NA_character_, .))) %>%
      select(any_of(c(
        "dateData",
        "sectorCompany",
        "idTicker",
        "nameCompany",
        "cityCompany",
        "stateCompany",
        "priceLast"
      )), everything())

    all_data <-
      all_data %>%
      mutate(
        ratioFFO = ifelse(
          amountFFOQuarterPrior > 0 ,
          amountMarketCapitalization  / (4 * amountFFOQuarterPrior),
          NA
        ),
        countShares = amountMarketCapitalization / priceLast,
        amountDividend = dividendQuarterPrior * countShares,
        ratioDividendCoverage =
          ifelse(
            amountFFOQuarterPrior > 0 ,
            amountFFOQuarterPrior  / amountDividend,
            NA
          )
      ) %>%
      select(
        dateData:amountFFOQuarterPrior,
        ratioDividendCoverage,
        ratioFFO,
        countShares,
        amountDividend,
        everything()
      ) %>%
      arrange(nameCompany)
    all_data
  }

# reit_properties ---------------------------------------------------------
#' NAREIT notable properties
#'
#' This function returns information about notable NAREIT identified properties.
#'
#' @references \href{http://nareit.org}{National Association of Real Estate Investment Trusts}
#' @return \code{tibble}
#' @export
#' @import purrr stringr dplyr jsonlite formattable lubridate tidyr readr
#' @family NAREIT
#' @family property data
#' @examples
#' nareit_notable_properties()
nareit_notable_properties <-
  function() {
    json_data <-
      "https://app.reitsacrossamerica.com/properties/notable" %>%
      fromJSON()

    df <-
      json_data$properties$property %>%
      as_tibble()

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
          paste0('https://app.reitsacrossamerica.com', .)
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
      mutate(across(c('coordinateLongitude', 'coordinateLatitude'), ~as.numeric(.))) %>%
      mutate(idProperty = idProperty %>% as.integer())

    df <-
      df %>%
      mutate(idProperty = idProperty %>% as.numeric()) %>%
      mutate(across(
        names(df %>% keep(is_character) %>% select(-dplyr::matches("url"))),
        ~str_to_upper(.)
      ))

    return(df)
  }

.parse_json_hq <-
  function(url = "https://app.reitsacrossamerica.com/states/NY/headquartered",
           return_message = TRUE) {
    slugState <-
      url %>%
      str_replace_all('https?://app.reitsacrossamerica.com/states/|/headquartered',
                      '')

    json_data <-
      url %>%
      fromJSON()

    no_data <-
      json_data$reits %>% length() == 0

    if (no_data) {
      return(tibble(slugState))
    }

    df_hq <-
      json_data$reits$reit %>%
      as_tibble() %>%
      select(-dplyr::matches("slug_url")) %>%
      purrr::set_names(c(
        'idSNL',
        'nameCompany',
        "cityCompany",
        'urlCompany',
        "urlNAREIT"
      ))

    df_hq <-
      df_hq %>%
      mutate(idSNL = idSNL %>% readr::parse_number(),
             nameCompany = nameCompany %>% str_to_upper()) %>%
      mutate(slugState) %>%
      suppressWarnings()


    if (return_message) {
      list(
        df_hq %>% nrow() %>% formattable::comma(digits = 0),
        " REITs are headquartered in ",
        slugState
      ) %>%
        do.call(paste0, .) %>%
        cat(fill = TRUE)
    }

    df_hq
  }

.parse_json_holdings <-
  function(url = "https://app.reitsacrossamerica.com/companies/holding/NY",
           return_message = TRUE) {
    slugState <-
      url %>%
      str_replace_all('https?://app.reitsacrossamerica.com/companies/holding/',
                      '')

    json_data <-
      url %>%
      fromJSON()

    no_data <-
      json_data$companies$company %>% length() == 0

    if (no_data) {
      return(tibble(slugState))
    }

    df_holdings <-
      json_data$companies$company %>%
      as_tibble() %>%
      select(-dplyr::matches("slug_url")) %>%
      purrr::set_names(c('idSNL', 'nameCompany', 'urlCompany'))

    df_holdings <-
      df_holdings %>%
      mutate(
        idSNL = idSNL %>% as.numeric(),
        nameCompany = nameCompany %>% str_to_upper(),
        urlCompany = ifelse(urlCompany == '', NA, urlCompany)
      ) %>%
      mutate(slugState)

    df_long <-
      df_holdings %>%
      pivot_longer(cols = -slugState, names_to = "item", values_to = "value") %>%
      group_by(item) %>%
      mutate(countItem = (seq_len(n())) - 1) %>%
      ungroup() %>%
      mutate(item = ifelse(countItem == 0, item, paste(item, countItem, sep = ''))) %>%
      arrange(countItem) %>%
      select(-countItem)

    col_order <-
      c('slugState', df_long$item)

    df_long <-
      df_long %>%
      pivot_wider(names_from = item, values_from = value) %>%
      select(any_of(col_order))

    if (return_message) {
      list(
        df_holdings %>% nrow() %>% formattable::comma(digits = 0),
        " REITs own assets in ",
        slugState
      ) %>%
        do.call(paste0, .) %>%
        cat(fill = TRUE)
    }

    return(df_long)
  }


.parse_json_state_metadata <-
  function() {
    json_data <-
      "https://app.reitsacrossamerica.com/states" %>%
      fromJSON()

    df <-
      json_data$states$state %>%
      as_tibble() %>%
      mutate(image_path = image_path %>% list_c() %>% paste0('https://app.reitsacrossamerica.com/', .)) %>%
      mutate(id = id %>% as.numeric()) %>%
      purrr::set_names(
        c(
          'idState',
          'slugState',
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
      mutate(across(matches('idState|count|amount'), ~as.numeric(.))) %>%
      mutate(amountValuationOwnedProperties = (amountValuationOwnedProperties * 1000000) %>% formattable::currency(digits = 0)) %>%
      select(idState:countHeadquarters,
             countSingleFamilyHomes,
             urlImageSample)

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
#' @return nested \code{tibble} or \code{tibble} if \code{nest_data = FALSE}
#' @export
#' @import purrr stringr dplyr jsonlite formattable lubridate tidyr readr
#' @family NAREIT
#' @family property data
#' @examples
#' \dontrun{
#' nareit_property_msa(nest_data = TRUE, return_message = TRUE)
#' }
nareit_property_msa <-
  function(nest_data = TRUE,
           return_message = TRUE) {
    json_data <-
      "https://www.reitsacrossamerica.com/data/msa_info.json" %>%
      fromJSON()

    df_property <-
      json_data$features$properties %>%
      as_tibble() %>%
      select(-dplyr::matches("_string")) %>%
      select(-type)

    df_property <-
      df_property %>%
      purrr::set_names(c(
        'amountValuationOwnedProperties',
        'nameMSA',
        'nameSector'
      )) %>%
      mutate(idLocation = seq_len(n())) %>%
      select(idLocation, nameMSA, nameSector, everything())

    df_lat_lon <-
      json_data$features$geometry %>%
      as_tibble() %>%
      mutate(idLocation = seq_len(n())) %>%
      unnest(cols = c(coordinates))

    df_lat_lon <-
      df_lat_lon[c(TRUE, FALSE),] %>%
      mutate(item = 'coordinateLongitude') %>%
      list(.,
           df_lat_lon[c(FALSE, TRUE),] %>%
             mutate(item = 'coordinateLatitude')) %>%
      bind_rows() %>%
      select(idLocation, coordinates, item) %>%
      pivot_wider(names_from = item, values_from = coordinates)

    df_property <-
      df_property %>%
      inner_join(df_lat_lon) %>%
      select(idLocation, everything()) %>%
      mutate(
        amountValuationOwnedProperties = (amountValuationOwnedProperties * 1000000) %>% formattable::currency(digits = 0),
        nameMSA = nameMSA %>% str_to_upper(),
        nameSector = nameSector %>% str_to_upper() %>% str_replace_all('\\-', ' ') %>% str_replace_all("DATA CENTER", "DATA CENTERS")
      ) %>%
      filter(!nameSector == "ALL TYPES") %>%
      suppressWarnings() %>%
      suppressMessages() %>%
      select(-idLocation)

    if (return_message) {
      list(
        df_property$amountValuationOwnedProperties %>% sum(na.rm = TRUE) %>% formattable::currency(digits = 0),
        " worth of REIT owned properties across ",
        df_property$nameMSA %>% unique() %>% length(),
        ' MSAs'
      ) %>%
        do.call(paste0, .) %>%
        cat(fill = TRUE)
    }

    if (nest_data) {
      df_property <-
        df_property %>%
        nest(dataProperties = -c(nameMSA, coordinateLatitude, coordinateLongitude))
    }

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
#' @return nested \code{tibble} or \code{tibble} if \code{nest_data = FALSE}
#' @export
#' @family NAREIT
#' @family property data
#' @examples
#' \dontrun{
#' nareit_state_info(include_reit_hq = TRUE, include_reit_holdings = TRUE, nest_data = TRUE, return_message = TRUE)
#' }
nareit_state_info <-
  function(include_reit_hq = TRUE,
           include_reit_holdings = TRUE,
           nest_data = TRUE,
           return_message = TRUE) {
    json_data <-
      "https://www.reitsacrossamerica.com/data/state_info.json" %>%
      fromJSON()

    df_property <-
      json_data$features$properties %>%
      as_tibble() %>%
      select(-dplyr::matches("_string")) %>%
      select(-type) %>%
      purrr::set_names(c('amountValuationOwnedProperties',
                         "slugState",
                         'nameSector')) %>%
      mutate(
        idLocation = seq_len(n()),
        amountValuationOwnedProperties = (amountValuationOwnedProperties * 1000000000) %>% formattable::currency(digits = 0)
      ) %>%
      select(idLocation, slugState, nameSector, everything())

    df_lat_lon <-
      json_data$features$geometry %>%
      as_tibble() %>%
      mutate(idLocation = seq_len(n())) %>%
      unnest(cols = c(coordinates))

    df_lat_lon <-
      df_lat_lon[c(TRUE, FALSE), ] %>%
      mutate(item = 'coordinateLongitude') %>%
      list(.,
           df_lat_lon[c(FALSE, TRUE), ] %>%
             mutate(item = 'coordinateLatitude')) %>%
      bind_rows() %>%
      select(idLocation, coordinates, item) %>%
      pivot_wider(names_from = item, values_from = coordinates)

    df_property <-
      df_property %>%
      inner_join(df_lat_lon) %>%
      select(idLocation, everything()) %>%
      mutate(
        amountValuationOwnedProperties = amountValuationOwnedProperties %>% formattable::currency(digits = 0),
        nameSector = nameSector %>% str_to_upper() %>% str_replace_all('\\-', ' ') %>% str_replace_all("DATA CENTER", "DATA CENTERS")
      ) %>%
      filter(!nameSector == "ALL TYPES") %>%
      suppressWarnings() %>%
      suppressMessages()

    df_md <-
      .parse_json_state_metadata()

    df_property <-
      df_md %>%
      left_join(df_property %>%
                  nest(dataPropertiesOwned = -slugState)) %>%
      suppressWarnings() %>%
      suppressMessages()

    if (include_reit_hq) {
      states <-
        df_property$slugState %>%
        unique() %>%
        sort()

      url_states <-
        list("https://app.reitsacrossamerica.com/states/",
             states ,
             "/headquartered") %>%
        do.call(paste0, .)

      .parse_json_hq_safe <-
        purrr::possibly(.parse_json_hq, tibble())

      df_hqs <-
        url_states %>%
        future_map_dfr(function(x) {
          .parse_json_hq_safe(url = x,
                             return_message = return_message)
        })

      if (nest_data) {
        df_hqs <-
          df_hqs %>%
          nest(dataCompanyHQs = -slugState)
      }

      df_property <-
        df_property %>%
        left_join(df_hqs) %>%
        suppressMessages()
    }

    if (include_reit_holdings) {
      states <-
        df_property$slugState %>%
        unique() %>%
        sort()

      url_states <-
        list("https://app.reitsacrossamerica.com/companies/holding/",
             states) %>%
        do.call(paste0, .)

      .parse_json_holdings_safe <-
        purrr::possibly(.parse_json_holdings, tibble())

      holdings_df <-
        url_states %>%
        future_map_dfr(function(x) {
          .parse_json_holdings_safe(url = x,
                                   return_message = return_message)
        })

      if (nest_data) {
        holdings_df <-
          holdings_df %>%
          nest(dataCompanyHoldings = -slugState)
      }


      df_property <-
        df_property %>%
        left_join(holdings_df) %>%
        suppressMessages()
    }

    df_property <-
      df_property %>%
      mutate(across(matches("count"), ~formattable::comma(., digits = 0))) %>%
      arrange(desc(slugState))

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
#' @return \code{tibble}
#' @export
#' @import dplyr purrr curl tidyr stringr formattable
#' @importFrom xlsx read.xlsx
#' @family NAREIT
#' @family index values
#' @examples
#' \dontrun{
#' nareit_monthly_returns(return_wide = FALSE)
#' }
nareit_monthly_returns <-
  function(return_wide = TRUE,
           return_message = TRUE) {
    # Layout (verified 2026-04):
    # Sheet 1 only. Rows 1-4 = title/metadata; row 6 = index name (forward-
    # filled across columns); row 7 = metric group; row 8 = metric detail;
    # row 10+ = data. Col 1 = Excel-serial date.
    url <- "https://www.reit.com/sites/default/files/returns/MonthlyHistoricalReturns.xls"
    ua  <- getOption("fundManageR.user_agent",
                     "SHELDON Research alexbresler@pwcommunications.com")
    tmp <- tempfile(fileext = ".xls")
    resp <- tryCatch(
      httr::GET(url, httr::user_agent(ua), httr::write_disk(tmp, overwrite = TRUE)),
      error = function(e) NULL
    )
    if (is.null(resp) || httr::status_code(resp) >= 400) {
      warning("NAREIT download failed"); return(tibble::tibble())
    }

    raw <- suppressMessages(readxl::read_excel(
      tmp, sheet = 1, col_names = FALSE, .name_repair = "minimal"
    ))
    unlink(tmp)

    # Extract 3-row header into per-column (index, metric_group, metric)
    hdr <- raw[6:8, -1, drop = FALSE]
    fill_forward <- function(v) {
      v <- as.character(v); last <- NA_character_
      for (i in seq_along(v)) {
        if (!is.na(v[i]) && nzchar(trimws(v[i]))) last <- v[i]
        v[i] <- last
      }
      v
    }
    idx_row    <- fill_forward(as.character(hdr[1, ]))
    metric_row1<- fill_forward(as.character(hdr[2, ]))
    metric_row2<- as.character(hdr[3, ])
    col_labels <- paste(
      stringr::str_to_upper(stringr::str_replace_all(idx_row, "\\s+", "_")),
      trimws(paste(metric_row1, metric_row2)),
      sep = "::"
    )

    data_body <- raw[10:nrow(raw), ]
    colnames(data_body) <- c("dateData", col_labels)
    # drop columns with empty/NA label or pure NA group
    keep <- !is.na(col_labels) & nzchar(trimws(col_labels)) &
      !grepl("^NA::", col_labels)
    data_body <- data_body[, c(TRUE, keep)]

    data <- data_body %>%
      dplyr::mutate(dateData = as.Date(suppressWarnings(as.numeric(.data$dateData)),
                                       origin = "1899-12-30")) %>%
      dplyr::filter(!is.na(.data$dateData)) %>%
      tidyr::pivot_longer(cols = -.data$dateData,
                          names_to = "item", values_to = "valueItem",
                          values_transform = list(valueItem = as.character)) %>%
      dplyr::mutate(valueItem = suppressWarnings(as.numeric(.data$valueItem))) %>%
      dplyr::filter(!is.na(.data$valueItem)) %>%
      tidyr::separate(.data$item,
                      into = c("nameIndex", "metricItem"),
                      sep = "::", extra = "merge") %>%
      dplyr::mutate(
        nameIndex = nameIndex %>% str_replace_all('\\_', '\\ '),
        metricItem = metricItem %>% str_replace_all('\\ ', '')
      ) %>%
      left_join(tibble(
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
      pivot_wider(names_from = nameItem, values_from = valueItem) %>%
      suppressWarnings()

    data <-
      data %>%
      mutate(across(matches("^index"), ~formattable::comma(., digits = 5))) %>%
      mutate(across(matches("^pct"), ~formattable::percent(. / 100, digits = 4))) %>%
      mutate(urlData = url) %>%
      suppressWarnings()

    if (return_message) {
      list(
        "Acquired FTSE NAREIT returns by sector from ",
        data$dateData %>% min(),
        ' to ',
        data$dateData %>% max()
      ) %>%
        do.call(paste0, .) %>%
        cat(fill = TRUE)
    }

    if (!return_wide) {
      data <-
        data %>%
        pivot_longer(cols = -c(dateData, nameIndex, urlData), names_to = "item", values_to = "value")
    }

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
#' @return \code{tibble}
#' @export
#' @import dplyr purrr curl tidyr stringr formattable
#' @importFrom xlsx read.xlsx
#' @family NAREIT
#' @family index values
#' @examples
#' \dontrun{
#' nareit_annual_subsector_returns(return_wide = TRUE, return_message = TRUE)
#' }
nareit_annual_subsector_returns <-
  function(return_wide = TRUE,
           return_message = TRUE) {
    url <-
      "https://www.reit.com/sites/default/files/returns/AnnualSectorReturns.xls"

    tmp <-
      tempfile()
    dl_ok <- tryCatch({
      curl::curl_download(url, tmp, mode = "wb")
      TRUE
    }, error = function(e) {
      warning("Download failed for ", url, ": ", conditionMessage(e))
      FALSE
    })
    if (!dl_ok) return(tibble())

    data <-
      xlsx::read.xlsx(file = tmp, sheetIndex = 2) %>%
      data.frame() %>%
      suppressWarnings()

    tmp %>%
      unlink()

    index_items <-
      data %>%
      select(-1) %>%
      slice(4:6) %>%
      t() %>%
      as_tibble() %>%
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
      as_tibble() %>%
      mutate(across(everything(), as.character)) %>%
      select(1:24) %>%
      purrr::set_names(items[1:24]) %>%
      filter(!dateData %>% is.na())

    data <-
      data %>%
      mutate(dateData = dateData %>% as.numeric() %>% as.Date(origin = "1899-12-30")) %>%
      pivot_longer(cols = -dateData, names_to = "item", values_to = "valueItem") %>%
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
      pivot_wider(names_from = metricItem, values_from = valueItem) %>%
      suppressWarnings()

    data <-
      data %>%
      mutate(across(matches("^pct"), ~formattable::percent(. / 100, digits = 4))) %>%
      mutate(urlData = url)

    if (return_message) {
      list(
        "Acquired annual FTSE NAREIT returns by subsector from ",
        data$dateData %>% min(),
        ' to ',
        data$dateData %>% max()
      ) %>%
        do.call(paste0, .) %>%
        cat(fill = TRUE)
    }

    if (!return_wide) {
      data <-
        data %>%
        pivot_longer(cols = -c(dateData, nameSector, nameSubSector, urlData), names_to = "item", values_to = "value")
    }

    return(data)
  }


# offerings ---------------------------------------------------------------

# https://www.reit.com/data-research/data/reit-capital-offerings

.get_nareit_offering_name_df <-
  function() {
    tibble(
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

.get_nareit_capital_urls <-
  function() {
    page <-
      "https://www.reit.com/data-research/data/reit-capital-offerings" %>%
      read_html()

    capital_urls <-
      page %>%
      html_nodes('p:nth-child(3) a') %>%
      html_attr('href') %>%
      paste0('https://www.reit.com', .) %>% {
        .[2:length(.)]
      }

    url_df <-
      tibble(
        typeCapital = c('IPO', 'Secondary', 'Debt'),
        indexSheet = c(1, 2, 1),
        urlData = capital_urls
      )

    return(url_df)
  }

.parse_nareit_offering_url <-
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
      data.frame() %>%
      suppressWarnings() %>%
      as_tibble()

    con %>% unlink()
    rm(con)
    data <-
      data %>%
      mutate(across(where(is.factor), as.character))

    if ("NA." %in% names(data)) {
      data <-
        data[, !names(data)  == "NA."]
    }

    nareit_offering_name_df <-
      .get_nareit_offering_name_df()

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
      mutate(across(where(is_character), str_to_upper)) %>%
      mutate(
        nameSubSector = ifelse(nameSubSector == '', NA, nameSubSector),
        dateOffering = dateOffering %>% lubridate::ymd()
      )

    data <-
      data %>%
      mutate(across(matches("^countShares|^amountProceeds"), ~as.numeric(.) * 1000000)) %>%
      mutate(across(matches("^price"), ~readr::parse_number(as.character(.)))) %>%
      mutate(urlData = url) %>%
      select(dateOffering, nameCompany, everything())

    data <-
      data %>%
      mutate(across(matches("nameUnderwriter|descriptionOffering"), ~ifelse(. == '', NA, .))) %>%
      mutate(across(where(is.character), str_trim)) %>%
      mutate(across(where(is.character), ~ifelse(. == '', NA, .)))

    has_shares <-
      names(data) %>% str_detect('countShares') %>% sum() > 0
    if (has_shares) {
      if ('countSharesOverallotment' %in% names(data)) {
        data <-
          data %>%
          mutate(
            priceOffering = ifelse(
              priceOffering %>% is.na(),
              amountProceedsTotal / sum(countSharesOverallotment, countShares, na.rm = TRUE),
              priceOffering
            )
          )
      } else {
        data <-
          data %>%
          mutate(
            priceOffering = ifelse(
              priceOffering %>% is.na(),
              amountProceedsTotal / sum(countShares, na.rm = TRUE),
              priceOffering
            )
          )
      }
    }

    if (return_message) {
      list("Parsed: ", url) %>%
        do.call(paste0, .) %>%
        cat(fill = TRUE)
    }

    data
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
#' @return nested \code{tibble} or \code{tibble} if \code{nest_data = FALSE}#' @export
#' @import dplyr purrr curl tidyr stringr formattable
#' @importFrom xlsx read.xlsx
#' @family NAREIT
#' @family capital raises
#' @examples
#' \dontrun{
#' nareit_capital_raises(capital_type = NULL, nest_data = FALSE, return_message = TRUE)
#' }
#'
nareit_capital_raises <-
  function(capital_type = NULL,
           nest_data = TRUE,
           return_message = TRUE) {
    url_df <-
      .get_nareit_capital_urls() %>%
      mutate(typeCapital = typeCapital %>% str_to_upper())

    if (!is.null(capital_type)) {
      capital_type <-
        capital_type %>% str_to_upper()
      is_valid_capital <-
        capital_type %in% url_df$typeCapital
      if (!is_valid_capital) {
        stop("Capital type can only be IPO, SECONDARY or DEBT")
      }
      url_df <-
        url_df %>%
        filter(typeCapital %in% capital_type)
    }
    if (nrow(url_df) == 0) {
      return(tibble())
    }
    .parse_nareit_offering_url_safe <-
      purrr::possibly(.parse_nareit_offering_url, tibble())

    all_data <-
      seq_len(nrow(url_df)) %>%
      purrr::map_dfr(function(x) {
        .parse_nareit_offering_url_safe(
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
      mutate(across(matches("amount"), ~formattable::currency(., digits = 0))) %>%
      mutate(across(matches("count"), ~formattable::comma(., digits = 0))) %>%
      mutate(across(matches("price"), ~formattable::comma(., digits = 3))) %>%
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
        do.call(paste0, .) %>%
        cat(fill = TRUE)
    }
    if (nest_data) {
      all_data <-
        all_data %>%
        nest(dataOfferingNAREIT = -typeCapital)
    }

    return(all_data)
  }


# m&a ---------------------------------------------------------------------
#' NAREIT merger and acquisitions
#'
#' This function OCRs data on NAREIT member
#' M&A activity since 2004.
#'
#' @param pages Integer vector. Page numbers to OCR from the PDF (default 32:33).
#' @param nest_data Logical. If \code{TRUE}, returns nested data frame.
#' @param return_message Logical. If \code{TRUE}, returns progress messages.
#'
#' @returns A tibble containing NAREIT M&A activity data.
#' @export
#' @import purrr stringr dplyr rvest formattable lubridate tidyr readr
#' @family NAREIT
#' @family transaction data
#' @examples
#' \dontrun{
#' nareit_mergers_acquisitions(nest_data = FALSE, return_message = TRUE)
#' }
nareit_mergers_acquisitions <-
  function(pages = 32:33,
           nest_data = FALSE,
           return_message = TRUE) {
    page <-
      "https://www.reit.com/data-research/data/reitwatch" %>%
      read_html()

    url <-
      page %>%
      html_nodes('h3 a') %>%
      html_attr('href') %>%
      .[[1]] %>%
      paste0('https://www.reit.com', .)

    tables <-
      url %>%
      tabulapdf::extract_tables(pages = pages)

    all_data <-
      seq_along(tables) %>%
      future_map_dfr(function(x) {
        tables[[x]] %>%
          as_tibble()
      }) %>%
      slice(3:nrow(.)) %>%
      mutate(V1 = ifelse(V1 == '', NA, V1)) %>%
      filter(!V2 %>% str_detect("Total")) %>%
      mutate(across(everything(), ~str_trim(.) %>% str_to_upper()))

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

    if (all_data %>% tibble::has_name("V13")) {
      all_data <-
        all_data %>%
        mutate(
          typeAcquiror = ifelse(V4 == '', V6, V4),
          amountAcquisitionPrice =  ifelse(V4 == '', V8, V6) %>% readr::parse_number() * 1000000,
          dateAnnounced = ifelse(V4 == '', V11, V8) %>% lubridate::dmy(),
          dateComplete =  ifelse(V4 == '', V12, V9) %>% lubridate::dmy(),
          statusTransaction = ifelse(V4 == '', V13, V10)
        ) %>%
        select(-dplyr::matches("^V")) %>%
        suppressWarnings()
    } else {
      all_data <-
        all_data %>%
        filter(V9 == "") %>%
        filter(!yearMerger == "2017") %>%
        mutate(across(everything(), ~ifelse(. == '', NA, .))) %>%
        dplyr::select(which(colMeans(is.na(.)) < 1)) %>%
        purrr::set_names(
          c(
            "yearMerger",
            "nameAcquiror",
            "nameTarget",
            "typeAcquiror",
            "amountAcquisitionPrice",
            "dateAnnounced",
            "dateComplete",
            "statusTransaction"
          )
        ) %>%
        bind_rows(
          all_data %>%
            filter(!V9 == "") %>%
            mutate(across(everything(), ~ifelse(. == '', NA, .))) %>%
            dplyr::select(which(colMeans(is.na(
              .
            )) < 1)) %>%
            mutate(
              typeAcquiror = case_when(
                nameTarget %>% str_detect("ASSET MANAGER") ~ "ASSET MANAGER",
                nameTarget %>% str_detect("PUBLIC REIT") ~ "PUBLIC REIT",
                nameTarget %>% str_detect("PUBLIC REAL ESTATE COMPANY") ~ "PUBLIC REAL ESTATE COMPANY",
                nameTarget %>% str_detect("ASSET MANAGER") ~ "ASSET MANAGER",
                nameTarget %>% str_detect("REAL ESTATE ADVISORY FIRM") ~ "REAL ESTATE ADVISORY FIRM",
                nameTarget %>% str_detect("ASSET MANAGEMENT FIRM") ~ "ASSET MANAGER",
                nameTarget %>% str_detect("BROKERAGE FIRM") ~ "BROKERAGE FIRM",
                nameTarget %>% str_detect("REAL ESTATE OPERATING COMPANY") ~ "REAL ESTATE OPERATING COMPANY",
                nameTarget %>% str_detect("NON-TRADED REIT") ~ "NON-TRADED REIT",
                nameTarget %>% str_detect("PRIVATE EQUITY FIRM") ~ "PRIVATE EQUITY FIRM",
                nameTarget %>% str_detect("INVESTOR GROUP") ~ "INVESTOR GROUP",
                nameTarget %>% str_detect("PRIVATE REIT") ~ "PRIVATE REIT",
                nameTarget %>% str_detect("PRIVATE REAL ESTATE COMPANY") ~ "PRIVATE REAL ESTATE COMPANY"
              )
            ) %>%
            mutate(
              nameTarget = nameTarget %>% str_replace_all(
                "PRIVATE REAL ESTATE COMPANY|PRIVATE REIT|INVESTOR GROUP|NON-TRADED REIT|REAL ESTATE OPERATING COMPANY|BROKERAGE FIRM|ASSET MANAGEMENT FIRM|PUBLIC REAL ESTATE COMPANY|PUBLIC REIT|ASSET MANAGER|PRIVATE EQUITY FIRM|REAL ESTATE ADVISORY FIRM",
                ""
              ) %>% str_trim()
            ) %>%
            purrr::set_names(
              c(
                "yearMerger",
                "nameAcquiror",
                "nameTarget",
                "amountAcquisitionPrice",
                "dateAnnounced",
                "dateComplete",
                "statusTransaction",
                "typeAcquiror"
              )
            )
        ) %>%
        bind_rows(
          all_data %>%
            filter(V9 == "") %>%
            filter(yearMerger == "2017") %>%
            mutate(across(everything(), ~ifelse(. == '', NA, .))) %>%
            dplyr::select(which(colMeans(is.na(
              .
            )) < 1)) %>%
            mutate(
              typeAcquiror = case_when(
                nameTarget %>% str_detect("ASSET MANAGER") ~ "ASSET MANAGER",
                nameTarget %>% str_detect("PUBLIC REIT") ~ "PUBLIC REIT",
                nameTarget %>% str_detect("PUBLIC REAL ESTATE COMPANY") ~ "PUBLIC REAL ESTATE COMPANY",
                nameTarget %>% str_detect("ASSET MANAGER") ~ "ASSET MANAGER",
                nameTarget %>% str_detect("REAL ESTATE ADVISORY FIRM") ~ "REAL ESTATE ADVISORY FIRM",
                nameTarget %>% str_detect("ASSET MANAGEMENT FIRM") ~ "ASSET MANAGER",
                nameTarget %>% str_detect("BROKERAGE FIRM") ~ "BROKERAGE FIRM",
                nameTarget %>% str_detect("REAL ESTATE OPERATING COMPANY") ~ "REAL ESTATE OPERATING COMPANY",
                nameTarget %>% str_detect("NON-TRADED REIT") ~ "NON-TRADED REIT",
                nameTarget %>% str_detect("PRIVATE EQUITY FIRM") ~ "PRIVATE EQUITY FIRM",
                nameTarget %>% str_detect("INVESTOR GROUP") ~ "INVESTOR GROUP",
                nameTarget %>% str_detect("PRIVATE REIT") ~ "PRIVATE REIT",
                nameTarget %>% str_detect("PRIVATE REAL ESTATE COMPANY") ~ "PRIVATE REAL ESTATE COMPANY"
              )
            ) %>%
            mutate(
              nameTarget = nameTarget %>% str_replace_all(
                "PRIVATE REAL ESTATE COMPANY|PRIVATE REIT|INVESTOR GROUP|NON-TRADED REIT|REAL ESTATE OPERATING COMPANY|BROKERAGE FIRM|ASSET MANAGEMENT FIRM|PUBLIC REAL ESTATE COMPANY|PUBLIC REIT|ASSET MANAGER|PRIVATE EQUITY FIRM|REAL ESTATE ADVISORY FIRM",
                ""
              ) %>% str_trim()
            ) %>%
            purrr::set_names(
              c(
                "yearMerger",
                "nameAcquiror",
                "nameTarget",
                "amountAcquisitionPrice",
                "dateAnnounced",
                "statusTransaction",
                "typeAcquiror"
              )
            )
        ) %>%
        distinct() %>%
        mutate(
          yearMerger = yearMerger %>% as.numeric(),
          amountAcquisitionPrice =  amountAcquisitionPrice %>% readr::parse_number() * 1000000,
          dateAnnounced = dateAnnounced %>%  lubridate::dmy(),
          dateComplete = dateComplete %>% lubridate::dmy()
        ) %>%
        suppressMessages()
    }

    all_data <-
      all_data %>%
      mutate(
        isPublicAcquiror = ifelse(typeAcquiror %>% str_detect("PUBLIC"), TRUE, FALSE),
        isJV = ifelse(typeAcquiror %>% str_detect("JV|JOINT VENTURE"), TRUE, FALSE),
        isComplete = ifelse(statusTransaction == "COMPLETED", TRUE, FALSE),
        idTransaction = seq_len(n())
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
            tibble(
              idTransaction = c(32, 60),
              nameAcquiror = c(
                'Westmont Hospitality; Cadim Inc. (Braveheart Holdings LP)',
                "Credit-Based Asset Servicing & Securitization LLC"
              ),
              nameTarget = c('Boykin Lodging Company',
                             'Fieldstone Investment Corporation')
            ) %>%
              mutate(across(where(is.character), str_to_upper))
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

    all_data <-
      all_data %>%
      mutate(
        typeAcquiror = case_when(
          typeAcquiror == "ASSET MANAGEMENT FIRM" ~ "Asset Manager",
          typeAcquiror == "ASSET MANAGER" ~  "Asset Manager",
          typeAcquiror ==  "AUSTRALIAN LPT" ~ "Asset Manager",
          typeAcquiror ==  "AUSTRALIAN LPT & PRIVATE EQUITY FIRM" ~ "Investor Group",
          typeAcquiror == "BROKERAGE FIRM" ~ "Investment Bank",
          typeAcquiror == "CANADIAN REIT" ~ "Public REIT",
          typeAcquiror == "CLOSEDEND INVESTMENT COMPANY" ~ "Private REIT",
          typeAcquiror == "CORRECTIONAL FACILITY OPERATOR" ~ "Public REIT",
          typeAcquiror == "FINANCIAL LENDING COMPANY" ~ "Mortgage Bank/Lender",
          typeAcquiror == "HEALTH CARE PROVIDER (PUBLIC COMPANY)" ~ "Public REIT",
          typeAcquiror == "INVESTMENT ADVIOR/BROKERAGE FIRM" ~ "Investment Bank",
          typeAcquiror == "INVESTMENT ADVISOR" ~ "Investment Bank",
          typeAcquiror == "INVESTMENT ADVISOR/BROKERAGE FIRM" ~ "Investment Bank",
          typeAcquiror == "INVESTMENT ADVISOR/PENSION FUND" ~ "Investor Group",
          typeAcquiror == "INVESTOR GROUP" ~ "Investor Group",
          typeAcquiror == "MORTGAGE BANKING FIRM" ~ "Mortgage Bank/Lender",
          typeAcquiror == "NONTRADED REIT" ~ "Private REIT",
          typeAcquiror == "PENSION FUND" ~ "Pension Fund",
          typeAcquiror == "PENSION TRUST FUND" ~ "Pension Fund",
          typeAcquiror == "PRIVATE EQUITY FIRM" ~ "Private Equity",
          typeAcquiror == "PRIVATE EQUITY FIRM & REOC" ~ "Private Equity",
          typeAcquiror == "PRIVATE EQUITY JOINT VENTURE" ~ "Private Equity",
          typeAcquiror == "PRIVATE REAL ESTATE COMPANY" ~ "Private Company",
          typeAcquiror == "PRIVATE REAL ESTATE JOINT VENTURE" ~ "Private Company",
          typeAcquiror == "PRIVATE REIT" ~ "Private REIT",
          typeAcquiror == "PUBLIC NONREIT AND REIT" ~ "Private REIT",
          typeAcquiror == "PUBLIC PENSION FUND" ~ "Pension Fund",
          typeAcquiror == "PUBLIC REAL ESTATE COMPANY" ~ "Public REIT",
          typeAcquiror == "PUBLIC REIT" ~ "Public REIT",
          typeAcquiror == "PUBLIC REIT; INVESTMENT ADVISOR" ~ "Investor Group",
          typeAcquiror == "PUBLIC REIT/INVESTMENT ADVISOR" ~ "Investor Group",
          typeAcquiror == "PUBLIC TOWER COMPANY"  ~ "Public REIT",
          typeAcquiror == "REAL ESTATE ADVISORY FIRM" ~ "Real Estate Advisor",
          typeAcquiror == "REAL ESTATE COMPANY/ BROKERAGE FIRM" ~ "Investment Bank",
          typeAcquiror == "REAL ESTATE OPERATING COMPANY" ~ "Public REIT",
          typeAcquiror == "REAL ESTATE OPERATING PARTNERSHIP" ~ "Public REIT",
          TRUE  ~ typeAcquiror
        )
      )

    if (return_message) {
      list(
        "You acquired ",
        all_data %>% nrow() %>% formattable::comma(digits = 0),
        ' REIT M&A transactions accounting for ',
        all_data$amountAcquisitionPrice %>% sum(na.rm = TRUE),
        ' in value from ',
        all_data$dateAnnounced %>% min(na.rm = TRUE),
        ' to ',
        all_data$dateAnnounced %>% max(na.rm = TRUE)
      ) %>%
        do.call(paste0, .) %>%
        cat(fill = TRUE)
    }

    if (nest_data) {
      all_data <-
        all_data %>%
        nest(dataTransactions = -yearMerger)
    }

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
#' @return nested \code{tibble} or \code{tibble} if \code{nest_data = FALSE}
#' @export
#' @import dplyr purrr curl tidyr stringr formattable rvest
#' @importFrom xlsx read.xlsx
#' @family NAREIT
#' @examples
#' \dontrun{
#' nareit_industry_tracker(return_message = TRUE, return_wide = TRUE)
#' }
nareit_industry_tracker <-
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
    dl_ok <- tryCatch({
      curl::curl_download(url, tmp, mode = "wb")
      TRUE
    }, error = function(e) {
      warning("Download failed for ", url, ": ", conditionMessage(e))
      FALSE
    })
    if (!dl_ok) return(tibble())

    data <-
      xlsx::read.xlsx(file = tmp,
                      sheetIndex = 1,
                      header = FALSE) %>%
      data.frame() %>%
      suppressWarnings() %>%
      as_tibble()

    tmp %>%
      unlink()

    data <-
      data %>%
      mutate(across(where(is.factor), as.character))

    years_quarters <-
      data %>%
      select(-X1) %>%
      slice(2) %>%
      t() %>%
      as.character()

    data <-
      data %>%
      filter(!X1 %>% is.na()) %>%
      mutate(idRow = seq_len(n()))

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
      left_join(tibble(
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
      left_join(tibble(
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
      pivot_longer(cols = -c(nameSector, nameItem), names_to = "yearQuarter", values_to = "value") %>%
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
      pivot_wider(names_from = nameItem, values_from = value) %>%
      tidyr::separate(
        yearQuarter,
        remove = FALSE,
        sep = '\\.',
        into = c('yearData', 'quarterData')
      ) %>%
      mutate(across(c('yearData', 'quarterData'), ~as.numeric(.))) %>%
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
      mutate(across(matches("amount"), ~formattable::currency(., digits = 0))) %>%
      mutate(across(matches("pct"), ~formattable::percent(., digits = 3))) %>%
      mutate(urlData = url)

    if (!return_wide) {
      data <-
        data %>%
        pivot_longer(cols = -c(nameSector, yearQuarter, yearData, quarterData, urlData), names_to = "item", values_to = "value")
    }

    if (return_message) {
      list("Parsed: ", url) %>%
        do.call(paste0, .) %>%
        cat(fill = TRUE)
    }

    if (nest_data) {
      data <-
        data %>%
        nest(dataTracker = -yearData)
    }

    return(data)
  }



# reit_funds --------------------------------------------------------------


.parse_fund_reit_page <-
  function(url, return_message = TRUE) {

    if (return_message) {
      .fm_parsing(url)
    }

    page <-
      url %>%
      xml2::read_html()

    fund_stems <-
      page %>%
      html_nodes('.fund a') %>%
      html_attr('href')

    fund_stems <-
      fund_stems[fund_stems %>% str_detect("/investing/")]

    urls <-
      fund_stems %>%
      paste0('https://www.reit.com', .) %>%
      unique()

    fund_nodes <-  page %>% html_nodes('.fund')

    df <-
      seq_along(fund_nodes) %>%
      future_map_dfr(function(x) {
        fund_node <- fund_nodes[[x]]
        categoryFund  <-
          fund_node %>% html_nodes(".category") %>% html_text() %>% str_trim()
        nameFundOperator <-
          fund_node %>% html_nodes(".provider") %>% html_text() %>% str_trim()
        nameFund <-
          fund_node %>% html_nodes(".name a") %>% html_text() %>% str_trim()
        idTicker <-
          fund_node %>% html_nodes(".ticker") %>% html_text() %>% str_trim()
        priceNAV <-
          fund_node %>% html_nodes(".nav") %>% html_text()

        pctReturnMonth <-
          fund_node %>% html_nodes(".return") %>% html_text()

        data <-
          tibble(categoryFund,
                   nameFundOperator,
                   nameFund,
                   idTicker)
        if (priceNAV %>% length() > 0) {
          priceNAV <-
            priceNAV %>% str_split("\\$") %>% list_c() %>% .[[2]] %>% readr::parse_number()

          data <-
            data %>%
            mutate(priceNAV)
        }

        if (pctReturnMonth %>% length() > 0) {
          pctReturnMonth <-
            pctReturnMonth %>%
            str_trim() %>% .[[2]] %>%
            readr::parse_number() / 100

          data <-
            data %>%
            mutate(pctReturnMonth)
        }
        data

      })

    df <-
      df %>%
      mutate(urlNAREIT = urls)
    df
  }

.parse_reit_fund_info_page <-
  function(urls,
           return_message = TRUE) {
    state <- new.env(parent = emptyenv())
    state$rows <- list()
    success <- function(res) {
      works <- res$status_code == 200
      if (works) {
        page <-
          res$url %>%
          xml2::read_html()

        values <-
          page %>%
          html_nodes('.reit-values__value') %>%
          html_text() %>%
          str_trim()

        table_values <-
          page %>%
          html_nodes('.data') %>%
          html_text()
        is_billions <-
          values %>% str_detect("bil") %>% sum() > 0

        items <-
          c('categoryFund',
            "amountAssets",
            "amountInvestmentMinimum",
            "pctReturn1Month",
            "pctExpenses",
            "pctYield30Day",
            "pctYieldTTM",
            "pctTurnOver")

        data <-
          tibble(item = items, value  = values) %>%
          pivot_wider(names_from = item, values_from = value) %>%
          suppressWarnings() %>%
          mutate(across(
            c(
              "amountAssets",
              "amountInvestmentMinimum",
              "pctReturn1Month",
              "pctExpenses",
              "pctYield30Day",
              "pctYieldTTM",
              "pctTurnOver"
            ),
            ~readr::parse_number(.)
          )) %>%
          suppressWarnings()

        data <-
          data %>%
          mutate(across(
            c(
              "pctReturn1Month",
              "pctExpenses",
              "pctYield30Day",
              "pctYieldTTM",
              "pctTurnOver"
            ),
            ~. / 100
          ))

        data <-
          data %>%
          mutate(
            amountAssets = ifelse(is_billions,
                                  amountAssets * 1000000000,
                                  amountAssets * 1000000),
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
            tibble(item = items,
                       period = periods,
                       value = table_values)

          data_perform <-
            data_perform %>%
            mutate(value = value %>% readr::parse_number()) %>%
            mutate(value = ifelse(item %>% str_detect("pct"), value / 100, value)) %>%
            tidyr::unite(item, item, period, sep = '') %>%
            pivot_wider(names_from = item, values_from = value) %>%
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
            cat(fill = TRUE)
        }
        rm(page)

        state$rows[[length(state$rows) + 1L]] <- data
      }
    }
    failure <- function(msg) {
      message(sprintf("Fail: %s", msg))
    }
    urls %>%
      purrr::walk(function(x) {
        curl_fetch_multi(url = x, success, failure)
      })
    multi_run()

    dplyr::bind_rows(state$rows)
  }

#' REIT fund manager data.
#'
#' This function acquires information for
#' REIT fund vehicles
#'
#' @param parse_fund_details \code{TRUE} parses fund details
#' @param return_message \code{TRUE} return a message after data import
#'
#' @return \code{tibble}
#' @export
#' @import dplyr stringr xml2 rvest purrr tidyr formattable
#' @family NAREIT
#' @family entity search
#' @family fund data
#' @examples
#' \dontrun{
#' reit_funds(parse_fund_details = TRUE, return_message = TRUE))
#' }

reit_funds <-
  function(parse_fund_details = TRUE,
           return_message = TRUE) {

    url = "https://www.reit.com/investing/reit-funds"

    pages <- url %>% read_html() %>% html_nodes('.pager__item--last a') %>% html_attr('href') %>% str_split("page=") %>% list_c() %>% .[[2]] %>% as.numeric()

    urls <- glue::glue("https://www.reit.com/investing/reit-funds?page={1:pages}") %>% as.character()
    .parse_fund_reit_page_safe <-
      purrr::possibly(.parse_fund_reit_page, tibble())

    df_table <-
      urls %>%
      future_map_dfr(function(url){
        .parse_fund_reit_page_safe(url = url, return_message = return_message)
      })



    if (parse_fund_details) {
      .parse_reit_fund_info_page_safe <-
        purrr::possibly(.parse_reit_fund_info_page, tibble())

      urls <-
        df_table$urlNAREIT

      detail_df <-
        urls %>%
        future_map_dfr(function(x) {
          x %>% cat(fill = TRUE)
          .parse_reit_fund_info_page_safe(urls = x, return_message = return_message)
        })

      detail_df <-
        detail_df %>%
        mutate(across(matches("pct"), ~. / 100))

      df_table <-
        df_table %>%
        left_join(detail_df) %>%
        suppressMessages()

      df_table <-
        df_table %>%
        mutate(across(matches("amount"), ~formattable::currency(.))) %>%
        mutate(across(matches("pct"), ~formattable::percent(.)))
    }

    df_table <-
      df_table %>%
      select(-dplyr::matches("idRow"))

    if (return_message) {
      list(
        "Acquired ",
        df_table %>% nrow() %>% formattable::comma(digits = 0),
        ' REIT funds'
      ) %>%
        do.call(paste0, .) %>%
        cat(fill = TRUE)
    }

    return(df_table)
  }

# reitwatch ---------------------------------------------------------------
