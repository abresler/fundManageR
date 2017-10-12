
# dictionaries ------------------------------------------------------------

get_tradeingview_chart_items <-
  function() {
    json_data <-
      "https://pine-facade.tradingview.com/pine-facade/list?filter=standard" %>%
      jsonlite::fromJSON(simplifyDataFrame = TRUE)

    data <-
      json_data[1:6] %>%
      tibble::as_data_frame()

    data
  }

# events ------------------------------------------------------------------
parse_result_number <-
  function(x) {
    x %>% stringi::stri_trans_general("Latin-ASCII") %>% readr::parse_number()
  }

parse_result <-
  function(x) {
    if (x %>% is.na()) {
      return(NA)
    }
    result <-
      x %>% parse_result_number()
    is_pct <-
      x %>% str_detect('%')

    if (is_pct) {
      result <-
        result / 100
    }

    is_billions <- x %>% str_detect("b")

    if (is_billions) {
      result <-
        result * 1000000000
    }
    is_millions <- x %>% str_detect("m")
    if (is_millions) {
      result <-
        result * 1000000
    }

    is_thousand <- x %>% str_detect("k")
    if (is_thousand) {
      result <-
        result * 1000
    }

    result <-
      result %>% formattable::comma(digits = 3)

    result

  }

#' Get Market Events
#'
#' Returns a list of global financial
#' and economics events
#'
#' @param return_message
#'
#' @return
#' @export
#' @import dplyr jsonlite purrr anytime glue stringr
#' @examples
#' get_data_market_events(retur_message = TRUE)
get_data_market_events <-
  function(return_message = TRUE) {
    data <-
      "https://chartevents.tradingview.com/chartevents/" %>%
      jsonlite::fromJSON(simplifyDataFrame = TRUE) %>%
      as_data_frame() %>%
      dplyr::select(-c(11, 2)) %>%
      purrr::set_names(
        c(
          'idEvent',
          'resultActual',
          'resultPrevious',
          'resultForecast',
          'descriptionEvent',
          'nameEvent',
          'rankImportance',
          'currencyEvent',
          'datetimeEvent'
        )
      ) %>%
      mutate(
        datetimeEvent = anytime::anytime(datetimeEvent),
        dateEvent = datetimeEvent %>% anytime::anydate()
      ) %>%
      dplyr::select(
        dateEvent,
        datetimeEvent,
        currencyEvent,
        nameEvent,
        rankImportance,
        descriptionEvent,
        resultForecast,
        resultActual,
        resultPrevious,
        everything()
      ) %>%
      arrange(dateEvent, desc(rankImportance)) %>%
      dplyr::select(-idEvent)

    data <-
      data %>%
      mutate_if(is.character,
                funs(ifelse(.  == '', NA, .)))

    data %>%
      mutate(resultActual = resultActual)

    data <-
      data %>%
      mutate_at(c('resultActual', 'resultPrevious', 'resultForecast'),
                funs(. %>% map_dbl(function(x) {
                  parse_result(x = x)
                }))) %>%
      suppressWarnings()

    if (return_message) {
      glue::glue(
        "Returned {nrow(data)} events from {min(data$dateEvent)} to {max(data$dateEvent)}"
      ) %>%
        message()
    }
    gc()
    closeAllConnections()
    data
  }


# search ------------------------------------------------------------------
"curl 'https://data.tradingview.com/search/?=FOREST&exchange=&type=&hl=true&lang=en&domain=production' -H 'Origin: https://www.tradingview.com' -H 'Accept-Encoding: gzip, deflate, br' -H 'Accept-Language: en-US,en;q=0.8' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_5) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/60.0.3112.24 Safari/537.36' -H 'Accept: application/json, text/javascript, */*; q=0.01' -H 'Referer: https://www.tradingview.com/chart/pKbLgoMZ/' -H 'Connection: keep-alive' -H 'DNT: 1' --compressed"




# popular -----------------------------------------------------------------
id_exchanges <- c(
  'US',
  'AMEX',
  'ASX',
  'BCBA',
  'BIST',
  'BME',
  'BMFBOVESPA',
  'BMV',
  'BSE',
  'EURONEXT',
  'FWB',
  'HKEX',
  'LSE',
  'LSIN',
  'MOEX',
  'NAG',
  'NASDAQ',
  'NSE',
  'NYSE',
  'NZX',
  'OTC',
  'SGX',
  'SIX',
  'TSE',
  'TSX',
  'TSXV',
  'XETR'
)

hot_list_slugs <- c(
  'volume_gainers',
  'percent_change_loosers',
  'percent_change_gainers',
  'percent_range_gainers',
  'percent_range_loosers',
  'gap_gainers',
  'gap_loosers',
  'percent_gap_gainers',
  'percent_gap_loosers'
)
# https://hotlist.tradingview.com/hotlist/US/volume_gainers/
# https://hotlist.tradingview.com/hotlist/US/percent_change_loosers/
# https://hotlist.tradingview.com/hotlist/US/percent_change_gainers/
# https://hotlist.tradingview.com/hotlist/US/percent_range_gainers/
# https://hotlist.tradingview.com/hotlist/US/gap_gainers/

# company -----------------------------------------------------------------

# https://esd-feed.tradingview.com/estimates?symbol=xbit&exchange=nasdaq

# https://news-headlines.tradingview.com/headlines/yahoo/symbol/FB/?locale=en -- news
generate_slug <-
  function(value = "nyse", sep_pre = "&",parameter = "exchange", symbol = "=", sep_post = "") {
    if (value %>% purrr::is_null()) {
      return("")
    }
    glue::glue("{sep_pre}{parameter}{symbol}{value}{sep_post}") %>%
      as.character()
  }

generate_ticker_estimates_url <-
  function(ticker = "cim", exchange = NULL) {
    slug_exchange <-
      generate_slug(value = exchange)
    base <- "https://esd-feed.tradingview.com/estimates"

    glue::glue("{base}?symbol={ticker}{slug_exchange}") %>%
      as.character()
  }


# scan --------------------------------------------------------------------

# {https://scanner.tradingview.com/uk/scan}

parse_region_security_url <-
  function(url = "https://scanner.tradingview.com/america/scan",
           return_message = TRUE) {
    idRegion <-
      url %>% str_replace_all("https://scanner.tradingview.com/|/scan", '')

    data <-
      url %>%
      jsonlite::fromJSON(flatten = TRUE)

    data <-
      data$data %>%
      select(1) %>%
      as_data_frame() %>%
      dplyr::rename(idExchangeTicker = s) %>%
      tidyr::separate(
        idExchangeTicker,
        into = c('idExchange',
                 'idTickerClass'),
        sep = '\\:'
      ) %>%
      tidyr::separate(idTickerClass,
                      into = c('idTicker', 'classSecurity')) %>%
      mutate_all(str_trim) %>%
      mutate(
        regionSecurities = idRegion,
        urlJSON = url,
        classSecurity = if_else(classSecurity %>% is.na(), 'COMMON', classSecurity)
      ) %>%
      suppressWarnings() %>%
      dplyr::select(regionSecurities, everything()) %>%
      arrange(idTicker)

    if (return_message) {
      glue::glue("Acquired {nrow(data)} listed securities in {idRegion %>% str_to_title()}") %>% message()
    }

    data
  }

parse_regions_security_urls <-
  function(urls,
           return_message = TRUE) {
    df <-
      data_frame()
    success <- function(res) {
      parse_region_security_url_safe <-
        purrr::possibly(parse_region_security_url, data_frame())
      page_url <- res$url
      data <-
        page_url %>%
        parse_region_security_url_safe(return_message = return_message)

      df <<-
        df %>%
        bind_rows(data)
    }
    failure <- function(msg) {
      data_frame()
    }
    urls %>%
      walk(function(x) {
        curl_fetch_multi(url = x, success, failure)
      })
    multi_run()
    closeAllConnections()
    df
  }


#' Tradingview regions traded securities
#'
#' Acquires ticker symbols for specified regions
#'
#' @param regions vector of regions \itemize{
#' \item america
#' \item uk
#' \item australia
#' \item brazil
#' \item canada
#' \item euronext
#' \item germany
#' \item hongkong
#' \item india
#' \item japan
#' \item mexico
#' \item newzealand
#' \item russia
#' \item singapore
#' \item spain
#' \item switzerland
#' \item taiwan
#' \item turkey
#' }
#' @param return_message if \code{TRUE} return message
#' @param nest_data
#' @import jsonlite glue dplyr purrr tidyr stringr
#' @return
#' @export
#'
#' @examples
get_data_tradingview_regions_tickers <-
  function(regions = c(
    'america',
    'uk',
    'australia',
    'brazil',
    'canada',
    'euronext',
    'germany',
    'hongkong',
    'india',
    'japan',
    'mexico',
    'newzealand',
    'russia',
    'singapore',
    'spain',
    'switzerland',
    'taiwan',
    'turkey'
  ),
  return_message = TRUE,
  nest_data = FALSE) {
    regions <-
      regions %>% str_to_lower() %>% str_replace_all('\\ ', '')

    urls <-
      glue::glue("https://scanner.tradingview.com/{regions}/scan")

    all_data <-
      urls %>%
      parse_regions_security_urls(return_message = return_message)

    if (nest_data) {
      all_data <-
        all_data %>%
        nest(-c(urlJSON, regionSecurities),
             .key = 'dataTickers')
    }

    all_data
  }


parse_metric_dictionary_url <-
  function(url = "https://scanner.tradingview.com/america/metainfo",
           return_message = TRUE) {
    idRegion <-
      url %>% str_replace_all("https://scanner.tradingview.com/|/metainfo", '')

    data <-
      url %>%
      jsonlite::fromJSON(flatten = TRUE)

    data <-
      data$fields %>%
      as_data_frame() %>%
      purrr::set_names(c('nameTW',
                         'typeField',
                         'fieldMembers')) %>%
      mutate(regionSecurities = idRegion,
             urlJSON = url) %>%
      dplyr::select(regionSecurities, everything()) %>%
      separate(nameTW,
               into = c('nameTW', 'baseTimeframe'),
               sep = '\\|') %>%
      suppressWarnings()


    df_fields <-
      1:length(data$fieldMembers) %>%
      map_df(function(x) {
        field <-
          data$fieldMembers[[x]]
        if (field %>% is_null()) {
          return(data_frame(idRow = x, dataField = NA))
        }

        if (field %>% class() == 'data.frame') {
          return(data_frame(idRow = x, dataField = list(field)))
        }

        data_frame(
          idRow = x,
          itemField = field  %>% str_c(collapse = ', '),
          dataField = NA
        )


      })

    if (return_message) {
      glue::glue("Acquired {nrow(data)} searchable metrics for {idRegion} securities") %>% message()
    }

    data %>%
      mutate(idRow = 1:n()) %>%
      left_join(df_fields %>%
                  mutate(countFieldRows = dataField %>%  map_dbl(length))) %>%
      dplyr::select(-fieldMembers) %>%
      dplyr::select(-idRow) %>%
      suppressWarnings() %>%
      suppressMessages()

  }

parse_metric_dictionaries_url <-
  function(urls,
           return_message = TRUE) {
    df <-
      data_frame()
    success <- function(res) {
      parse_metric_dictionary_url_safe <-
        purrr::possibly(parse_metric_dictionary_url, data_frame())
      page_url <- res$url
      data <-
        page_url %>%
        parse_metric_dictionary_url_safe(return_message = return_message)

      df <<-
        df %>%
        bind_rows(data)
    }
    failure <- function(msg) {
      data_frame()
    }
    urls %>%
      walk(function(x) {
        curl_fetch_multi(url = x, success, failure)
      })
    multi_run()
    closeAllConnections()
    df
  }

#' Tradingview searchable metrics by region
#'
#' Get searchable metrics by region
#' @param regions vector of regions \itemize{
#' \item america
#' \item uk
#' \item australia
#' \item brazil
#' \item canada
#' \item euronext
#' \item germany
#' \item hongkong
#' \item india
#' \item japan
#' \item mexico
#' \item newzealand
#' \item russia
#' \item singapore
#' \item spain
#' \item switzerland
#' \item taiwan
#' \item turkey
#' }
#' @param return_message if \code{TRUE} return message
#' @param nest_data
#' @import jsonlite glue dplyr purrr tidyr stringr

#' @return
#' @export
#'
#' @examples
get_data_regions_tradingview_metrics <-
  function(regions = c(
    'america',
    'uk',
    'australia',
    'brazil',
    'canada',
    'euronext',
    'germany',
    'hongkong',
    'india',
    'japan',
    'mexico',
    'newzealand',
    'russia',
    'singapore',
    'spain',
    'switzerland',
    'taiwan',
    'turkey'
  ),
  return_message = TRUE,
  nest_data = FALSE) {
    regions <-
      regions %>% str_to_lower() %>% str_replace_all('\\ ', '')

    urls <-
      glue::glue("https://scanner.tradingview.com/{regions}/metainfo")

    all_data <-
      urls %>%
      parse_metric_dictionaries_url(return_message = return_message)

    if (nest_data) {
      all_data <-
        all_data %>%
        nest(-c(urlJSON, regionSecurities),
             .key = 'dataMetrics')
    }

    all_data
  }



# metric_query ------------------------------------------------------------

#' Generate tradeview metric query
#'
#' @param filter
#' @param symbols
#' @param metrics
#' @param sort
#' @param options
#' @param range
#'
#' @return
#' @export
#' @import reticulate magrittr glue dplyr
#' @examples
generate_trade_view_metric_query <-
  function(filter = data_frame(left = 'market_cap_basic',
                               operation = 'nempty'),
           symbols = list(query = list(types = c('stock', 'fund', 'dr'))),
           metrics =c("change", "change_abs", "close", "description", "earnings_per_share_basic_ttm",
                      "market_cap_basic", "name", "number_of_employees", "price_earnings_ttm",
                      "sector", "volume"),
           sort = list(sortBy = 'market_cap_basic',
                       sortOrder = 'desc'),
           options = list(lang = 'eng'),
           range = c(0, 1500000)) {
    metrics <-
      c('name', metrics) %>%
      unique()

    metrics <-
      metrics[!metrics %in% c('component', 'index', 'component')]

    data_query <-
      list(
        filter = filter,
        symbols = symbols,
        columns = metrics,
        sory = sort,
        options = options,
        range = range
      ) %>%
      toJSON(auto_unbox = T)

    data_query
  }


parse_tradeview_metric_url <-
  function(url = 'https://scanner.tradingview.com/america/scan',
           data_query = '{"filter":[{"left":"market_cap_basic","operation":"nempty"}],"symbols":{"query":{"types":["stock","fund","dr"]}},"columns":["name","Recommend.All","sector","close","change_abs","change","volume","market_cap_basic","price_earnings_ttm","earnings_per_share_basic_ttm","number_of_employees","description","name","Recommend.All"],"sort":{"sortBy":"market_cap_basic","sortOrder":"desc"},"options":{"lang":"en"},"range":[0,1500000000]}',
           return_message = TRUE) {
    idRegion <-
      url %>% str_replace_all("https://scanner.tradingview.com/|/scan", "")

    requests <-
      reticulate::import("requests")

    json <-
      requests$post(url = url, data = data_query)

    json_data <-
      json$content %>%
      fromJSON(simplifyDataFrame = TRUE, flatten = TRUE)

    json$close()

    data <-
      json_data$data %>% as_data_frame() %>% unnest() %>%
      purrr::set_names(c('idExchangeTicker', 'value')) %>%
      group_by(idExchangeTicker) %>%
      mutate(countItem = 1:n()) %>%
      ungroup() %>%
      tidyr::separate(
        idExchangeTicker,
        into = c('idExchange',
                 'idTickerClass'),
        sep = '\\:'
      ) %>%
      tidyr::separate(idTickerClass,
                      into = c('idTicker', 'classSecurity')) %>%
      mutate_all(str_trim) %>%
      mutate(idRegion,
             classSecurity = if_else(classSecurity %>% is.na(), 'COMMON', classSecurity)) %>%
      suppressWarnings() %>%
      dplyr::select(idRegion, everything())

    if (return_message) {
      tickers <-
        data$idTicker %>% unique() %>% length() %>% formattable::comma(digits = 0)
      glue::glue(
        "Acquired {nrow(data) %>% formattable::comma(digits = 0)} listed metrics for {tickers} securities in {idRegion %>% str_to_title()}"
      ) %>% message()
    }
    closeAllConnections()
    gc()
    data
  }

#' Tradeview region metrics
#'
#' Get data for trade view query
#'
#' @param regions vector of regions \itemize{
#' \item america
#' \item uk
#' \item australia
#' \item brazil
#' \item canada
#' \item euronext
#' \item germany
#' \item hongkong
#' \item india
#' \item japan
#' \item mexico
#' \item newzealand
#' \item russia
#' \item singapore
#' \item spain
#' \item switzerland
#' \item taiwan
#' \item turkey
#' }
#' @param query list of query parameters \itemize{
#' \item filter - list of query parameters
#' \item symbols - list of types
#' \item metrics - vector of parameters see \code{get_data_tradingview_regions_tickers} for options
#' \item sort - sort paramters
#' \item options- sort options
#' }
#' @param return_message
#'
#' @return
#' @export
#' @import reticulate dplyr purrr stringr glue
#'
#' @examples
get_tradeview_regions_metrics <-
  function(regions = c('canada', 'america'),
           query = list(
             filter = data_frame(left = 'market_cap_basic',
                                 operation = 'nempty'),
             symbols = list(query = list(types = c(
               'stock', 'fund', 'dr'
             ))),
             metrics = c("beta_5_year", "earnings_release_date", "earnings_per_share_forecast_next_fq",
                         "operating_margin", "return_on_equity", "current_ratio", "debt_to_assets",
                         "price_revenue_ttm", "amount_recent", "market_cap_basic", "ebitda",
                         "fundamental_currency_code", "total_assets", "current_session",
                         "earnings_per_share_fq", "earnings_per_share_forecast_fq", "earnings_release_next_time",
                         "cash_ratio", "yield_upcoming", "sector", "basic_eps_net_income",
                         "price_book_ratio", "quick_ratio", "net_debt", "total_shares_outstanding_fundamental",
                         "enterprise_value_fq", "beta_3_year", "total_capital", "earnings_per_share_diluted_ttm",
                         "last_annual_eps", "revenue_fq", "ex_dividend_date_recent", "price_earnings_ttm",
                         "debt_to_equity", "pre_tax_margin", "debt_to_equity_fq", "number_of_employees",
                         "total_current_assets", "last_annual_revenue", "revenue_forecast_fq",
                         "industry", "return_on_assets", "return_of_invested_capital_percent_ttm",
                         "return_on_invested_capital", "gross_profit", "dividends_paid",
                         "preferred_dividends", "earnings_release_next_date", "dividends_yield",
                         "price_sales_ratio", "yield_recent", "ex_dividend_date_upcoming",
                         "total_shares_outstanding", "price_earnings_to_growth_ttm", "price_book_fq",
                         "enterprise_value_ebitda_ttm", "rtc", "amount_upcoming", "average_volume",
                         "revenue_per_employee", "after_tax_margin", "net_income", "earnings_release_time",
                         "type_recent", "dividends_per_share_fq", "payment_date_upcoming",
                         "gross_margin_percent_ttm", "earnings_per_share_basic_ttm", "price_free_cash_flow_ttm",
                         "long_term_capital", "total_debt", "country", "total_revenue",
                         "gross_margin", "number_of_shareholders", "type_upcoming", "beta_1_year",
                         "goodwill", "expected_annual_dividends", "revenue_forecast_next_fq",
                         "payment_date_recent", "low", "volume", "pre_change_abs", "gap",
                         "open", "volume", "pre_change_abs", "time", "change_from_open",
                         "low", "high", "close", "volume", "change_abs", "open", "change_from_open",
                         "change_abs", "time", "change", "pre_change_abs", "time", "gap",
                         "high", "open", "change_from_open", "change_abs", "low", "close",
                         "change", "change_abs", "close", "time", "change", "pre_change",
                         "close", "high", "gap", "change", "open", "high", "pre_change",
                         "pre_change", "pre_change_abs", "gap", "pre_change", "change_from_open",
                         "low", "volume", "relative_volume", "type", "subtype", "eps_surprise_fq",
                         "market_cap_calc", "exchange", "price_sales", "eps_surprise_percent_fq"
             )
             ,
             sort = list(sortBy = 'market_cap_basic',
                         sortOrder = 'desc'),
             options = list(lang = 'eng'),
             range = c(0, 15000000000000)
           ),
           return_message = TRUE) {
    options(scipen = 99999)
    glue::glue("\n\nWARNING -- this function requires Python and the requests module!!!!\n\n") %>%
      message()

    urls <-
      glue::glue("https://scanner.tradingview.com/{regions}/scan")

    data_query <-
      query %$%
      generate_trade_view_metric_query(
        filter = filter,
        symbols = symbols,
        metrics = metrics,
        sort = sort,
        options = options,
        range = range
      )

    data <-
      1:length(urls) %>%
      map_df(function(x) {
        parse_tradeview_metric_url(url = urls[x], data_query = data_query)
      }) %>%
      mutate(countItem = countItem %>% as.integer())

    metrics <-
      data_frame(nameTW = c('name', query$metrics) %>% unique()) %>%
      mutate(countItem = 1:n())

    data <-
      data %>%
      left_join(metrics) %>%
      suppressMessages()

    data <-
      data %>%
      mutate(idTicker = ifelse(nameTW == "name", value, NA)) %>%
      fill(idTicker) %>%
      filter(!nameTW == 'name')

    df_metrics <-
      get_data_regions_tradingview_metrics(regions = regions[1])

    data <-
      data %>%
      left_join(df_metrics %>%
                  select(nameTW, typeField)) %>%
      distinct() %>%
      suppressMessages() %>%
      filter(!nameTW %in% c('component', 'index'))

    df_companies <-
      data %>%
      filter(typeField %in% c(NA, 'text')) %>%
      dplyr::select(idRegion:value, nameTW) %>%
      spread(nameTW, value)

    df_values <-
      data %>%
      filter(!typeField %in% c(NA, 'text')) %>%
      dplyr::select(idExchange:value, nameTW) %>%
      mutate(value = value %>% readr::parse_number()) %>%
      spread(nameTW, value)

    data <-
      df_companies %>%
      left_join(df_values) %>%
      suppressMessages() %>%
      dplyr::select(which(colMeans(is.na(.)) < 1))

    data

  }


# news --------------------------------------------------------------------

# https://news-headlines.tradingview.com/headlines/yahoo/symbol/FB/?locale=en

get_ticker_tradingview_news <-
  function(ticker = "FB") {
    url <-
      glue::glue("https://news-headlines.tradingview.com/headlines/yahoo/symbol/{ticker}")

    data <-
      url %>%
      jsonlite::fromJSON(simplifyDataFrame = TRUE) %>%
      dplyr::as_data_frame() %>%
      dplyr::select(-1) %>%
      purrr::set_names(c(
        'urlArticle',
        'titleArticle',
        'descriptionArticle',
        'datetimePublished'
      )) %>%
      mutate(
        idTicker = ticker,
        datetimePublished = anytime::anytime(datetimePublished),
        urlJSON = url
      ) %>%
      dplyr::select(idTicker,
                    datetimePublished,
                    titleArticle,
                    descriptionArticle,
                    everything())

    data
  }

parse_trading_view_news_url <-
  function(url = "https://news-headlines.tradingview.com/headlines/yahoo/symbol/FB",
           return_message = TRUE) {
    ticker <-
      url %>% str_replace_all("https://news-headlines.tradingview.com/headlines/yahoo/symbol/",
                              '')

    if (return_message) {
      glue::glue("Acquiring Tradingview news for {ticker}") %>%
        message()
    }

    data <-
      url %>%
      jsonlite::fromJSON(simplifyDataFrame = TRUE) %>%
      dplyr::as_data_frame() %>%
      dplyr::select(-1) %>%
      purrr::set_names(c(
        'urlArticle',
        'titleArticle',
        'descriptionArticle',
        'datetimePublished'
      )) %>%
      mutate(
        idTicker = ticker,
        datetimePublished = anytime::anytime(datetimePublished),
        urlJSON = url
      ) %>%
      dplyr::select(idTicker,
                    datetimePublished,
                    titleArticle,
                    descriptionArticle,
                    everything())
    gc()
    closeAllConnections()
    data
  }

parse_tradingview_news_urls <-
  function(urls,
           return_message = TRUE) {
    df <-
      data_frame()
    success <- function(res) {
      parse_trading_view_news_url_safe <-
        purrr::possibly(parse_trading_view_news_url, data_frame())
      page_url <- res$url
      data <-
        page_url %>%
        parse_trading_view_news_url_safe(return_message = return_message)

      df <<-
        df %>%
        bind_rows(data)
    }
    failure <- function(msg) {
      data_frame()
    }
    urls %>%
      walk(function(x) {
        curl_fetch_multi(url = x, success, failure)
      })
    multi_run()
    closeAllConnections()
    df
  }


#' Tradingview tickers news
#'
#' Returns news data for specified tickers
#'
#' @param tickers
#' @param return_message
#' @param nest_data
#' @import dplyr tibble glue anytime tidyr curl jsonlite
#' @return
#' @export
#'
#' @examples
#' get_data_tickers_tradingview_news(tickers = c("VNO", "AVB", "PEI"), return_message = TRUE, nest_data = FALSE)

get_data_tickers_tradingview_news <-
  function(tickers = c("FB", "AAPL", "NFLX", "GOOG", "VNO", "EQR", "BXP"),
           return_message = TRUE,
           nest_data = FALSE) {
    urls <-
      glue::glue("https://news-headlines.tradingview.com/headlines/yahoo/symbol/{tickers}")

    all_data <-
      urls %>%
      parse_tradingview_news_urls(return_message = return_message)

    if (nest_data) {
      all_data <-
        all_data %>%
        tidyr::nest(-c(idTicker, urlJSON), .key = 'tickerNews')
    }

    all_data
  }