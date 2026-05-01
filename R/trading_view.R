
# ---- Internal TradingView helpers (added 2026-05-01 per LandMine sweep) ----

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

#' Retry an httr call with exponential backoff on 429/5xx + network errors
#' @keywords internal
.tv_with_retry <- function(call_fn, max_tries = 3, base_sleep = 0.5) {
  for (i in seq_len(max_tries)) {
    resp <- tryCatch(call_fn(), error = function(e) e)
    if (inherits(resp, "error")) {
      if (i == max_tries) stop("TradingView call failed after ", max_tries, " tries: ", conditionMessage(resp))
      Sys.sleep(base_sleep * (2 ^ (i - 1)))
      next
    }
    code <- httr::status_code(resp)
    if (code < 400) return(resp)
    if (code == 429 || code >= 500) {
      if (i == max_tries) stop("TradingView returned HTTP ", code, " after ", max_tries, " tries")
      Sys.sleep(base_sleep * (2 ^ (i - 1)))
      next
    }
    stop("TradingView returned HTTP ", code, ": ", httr::content(resp, as = "text", encoding = "UTF-8"))
  }
}

#' Parse JSON response and verify expected top-level keys exist
#' @keywords internal
.tv_safe_fromJSON <- function(resp, expect = NULL, context = "tradingview") {
  body_text <- httr::content(resp, as = "text", encoding = "UTF-8")
  if (!nzchar(body_text)) stop(context, ": empty response body")
  parsed <- tryCatch(jsonlite::fromJSON(body_text, simplifyDataFrame = TRUE),
                     error = function(e) stop(context, ": JSON parse failed: ", conditionMessage(e)))
  if (!is.null(expect)) {
    missing <- setdiff(expect, names(parsed))
    if (length(missing) > 0) {
      warning(context, ": response schema drift — missing keys: ",
              paste(missing, collapse = ", "), call. = FALSE)
    }
  }
  parsed
}

.get_dictionary_tradeview_types <- function() {
  tibble(
    type = c(
      "All",
      "stock",
      "futures",
      "forex",
      "cfd",
      "cryptocurrency",
      "Index",
      "Economy",
      "quandl"
    ),
    slugType = c(
      "",
      "stocks",
      "futures",
      "forex",
      "cfd",
      "bitcoin",
      "index",
      "economic",
      "quandl"
    )
  )
}
# dictionaries ------------------------------------------------------------

.get_tradeingview_chart_items <-
  function() {
    json_data <-
      "https://pine-facade.tradingview.com/pine-facade/list?filter=standard" %>%
      jsonlite::fromJSON(simplifyDataFrame = TRUE)

    data <-
      json_data[1:6] %>%
      tibble::as_tibble()

    data
  }

# events ------------------------------------------------------------------
.parse_result_number <-
  function(x) {
    x %>% stringi::stri_trans_general("Latin-ASCII") %>% readr::parse_number()
  }

.parse_result <-
  function(x) {
    if (x %>% is.na()) {
      return(NA)
    }
    result <-
      x %>% .parse_result_number()
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

.dictionary_market_event_names <-
  function(){
    tibble(nameTV = c("actual", "comment", "country", "currency", "date", "forecast",
                          "id", "importance", "indicator", "link", "period", "previous",
                          "scale", "source", "title", "unit"),
               nameActual = c("actualData", "commentRelease", "slugCountry", "slugCurrency", "datetimeRelease", "forecastData",
                              "idTradeview", "rankImportance", "nameIndicator", "urlIndicator", "periodData", "previousData",
                              "scaleData", "sourceData", "titleIndicator", "unitIndicator")

    )
  }

#' Get Market Events
#'
#' Returns a list of global financial
#' and economics events
#'
#' @param return_message if \code{TRUE} returns a message
#'
#' @return a \code{tibble} of market events
#' @export
#' @import dplyr jsonlite purrr anytime glue stringr
#' @examples
#' \dontrun{
#' tv_market_events(return_message = TRUE)
#' }
tv_market_events <-
  function(return_message = TRUE) {

    url <-
      glue("https://chartevents-reuters.tradingview.com/events?minImportance=0&from={Sys.Date()}T00:00:00.000Z&to={Sys.Date()+720}T13:00:00.000Z&currencies=USD,EUR,JPY,AUD,DEM,GBP,CAD,FRF,ITL,NZD,ESP,MXN,CHF,TRL,ZAR") %>%
      as.character()
    data <-
      url %>%
      jsonlite::fromJSON(simplifyDataFrame = TRUE) %>%
      as_tibble()

    data <- data$result %>% as_tibble()
    dict_names <- .dictionary_market_event_names()
    actual_names <-
      names(data) %>%
      map_chr(function(name){
        df_row <- dict_names %>% filter(nameTV == name)
        if (nrow(df_row) == 0) {
          glue::glue("Missing {name}")
          return(name)
        }

        df_row$nameActual
      })

    data <-
      data %>%
      purrr::set_names(actual_names) %>%
      mutate(
        hasData = !is.na(actualData),
        ratioActualForecast = case_when(hasData ~ actualData / forecastData,
                                        TRUE ~ NA_real_)
      )

    data <-
      data %>%
      mutate(
        datetimeRelease = anytime::anytime(datetimeRelease),
        dateRelease = datetimeRelease %>% anytime::anydate()
      ) %>%
      dplyr::select(
        dateRelease,
        datetimeRelease,
        slugCurrency,
        titleIndicator,
        nameIndicator,
        commentRelease,
        forecastData,
        actualData,
        previousData,
        everything()
      ) %>%
      arrange(dateRelease, desc(rankImportance))

    data <- data %>%
      tidyr::separate(
        titleIndicator,
        into = c("regionIndicator", "typeIndicator", "descriptionIndicator"),
        sep = "\\ - "
      ) %>%
      mutate(regionIndicator = regionIndicator %>% str_replace_all("\\-year", "\\ year")) %>%
      tidyr::separate(
        regionIndicator,
        into = c("regionIndicator", "groupIndicator"),
        sep = "\\ "
      ) %>%
      mutate(across(where(is.character),
                ~ifelse(. == "", NA, .) %>% str_trim()
                )) %>%
      suppressMessages() %>%
      suppressWarnings()

    data <-
      data %>%
      mutate(across(c('actualData', 'previousData', 'forecastData'),
                ~map_dbl(., function(x) {
                  .parse_result(x = x)
                }))) %>%
      suppressWarnings() %>%
      mutate(changeData = actualData - previousData)

    if (return_message) {
      glue::glue(
        "Returned {nrow(data)} events from {min(data$dateRelease)} to {max(data$dateRelease)}"
      ) %>%
        cat(fill = TRUE)
    }
    gc()

    janitor::clean_names(data)
  }


# search ------------------------------------------------------------------

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
.generate_slug <-
  function(value = "nyse", sep_pre = "&",parameter = "exchange", symbol = "=", sep_post = "") {
    if (value %>% is.null()) {
      return("")
    }
    glue::glue("{sep_pre}{parameter}{symbol}{value}{sep_post}") %>%
      as.character()
  }

.generate_ticker_estimates_url <-
  function(ticker = "cim", exchange = NULL) {
    slug_exchange <-
      .generate_slug(value = exchange)
    base <- "https://esd-feed.tradingview.com/estimates"

    glue::glue("{base}?symbol={ticker}{slug_exchange}") %>%
      as.character()
  }



# search ------------------------------------------------------------------
.generate_url_reference <-
  function() {
    user_agents <-
      c(
        "Mozilla/5.0 (Linux; U; en-US) AppleWebKit/528.5+ (KHTML, like Gecko, Safari/528.5+) Version/4.0 Kindle/3.0 (screen 600x800; rotate)",
        "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/42.0.2311.135 Safari/537.36 Edge/12.246",
        "Mozilla/5.0 (X11; CrOS x86_64 8172.45.0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/51.0.2704.64 Safari/537.36",
        "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_2) AppleWebKit/601.3.9 (KHTML, like Gecko) Version/9.0.2 Safari/601.3.9",
        "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.111 Safari/537.36"
      )


    user_agent <-
      user_agents[!user_agents %>% str_detect("bot|slurp")] %>%
      sample(1)

    tl_domain <-
      c('.com', '.gov', '.org', '.mil', '.co') %>%
      sample(1)

    word_length <-
      8:15

    words <-
      word_length %>% sample(1)

    domain_slug <-
      1:words %>%
      map_chr(function(x) {
        sample(letters, 1)
      }) %>%
      paste0(collapse = '')

    url <-
      list('http://', domain_slug, tl_domain) %>%
      purrr::reduce(paste0)
    df <-
      tibble(urlReferer = url,
                 userAgent = user_agent)
    return(df)
  }

get_tradeview_term <-
  function(term = "FB",
           exchange = NULL,
           type = NULL) {
    # Migrated 2026-05-01: legacy data.tradingview.com/search endpoint
    # is dead (Empty reply from server). Modern endpoint requires full
    # browser-shaped headers and returns a richer payload under `symbols`.
    url <- 'https://symbol-search.tradingview.com/symbol_search/v3/'
    query <- list(
      text = term,
      hl = '1',
      exchange = exchange %||% '',
      lang = 'en',
      search_type = type %||% 'undefined',
      domain = 'production'
    )
    resp <- .tv_with_retry(function() {
      httr::GET(
        url,
        query = query,
        httr::add_headers(
          `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/130.0.0.0 Safari/537.36',
          Accept = 'application/json',
          Origin = 'https://www.tradingview.com',
          Referer = 'https://www.tradingview.com/',
          `Sec-Fetch-Mode` = 'cors',
          `Sec-Fetch-Site` = 'same-site'
        )
      )
    })
    body <- .tv_safe_fromJSON(resp, expect = c('symbols'), context = 'get_tradeview_term')
    syms <- body$symbols
    if (is.null(syms) || (is.data.frame(syms) && nrow(syms) == 0)) {
      return(tibble::tibble(term_search = term))
    }
    syms %>%
      tibble::as_tibble() %>%
      janitor::clean_names() %>%
      mutate(across(where(is.character),
                    ~ str_replace_all(.x, "<em>|</em>", "") %>% str_trim())) %>%
      mutate(term_search = term) %>%
      dplyr::select(term_search, dplyr::any_of(c("symbol", "description", "type", "exchange",
                                                  "country", "currency_code", "isin", "cusip",
                                                  "cik_code", "source_id", "provider_id")),
                    dplyr::everything())
  }

# scan --------------------------------------------------------------------

# {https://scanner.tradingview.com/uk/scan}

.parse_region_security_url <-
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
      as_tibble() %>%
      dplyr::rename(idExchangeTicker = s) %>%
      tidyr::separate(
        idExchangeTicker,
        into = c('idExchange',
                 'idTickerClass'),
        sep = '\\:'
      ) %>%
      tidyr::separate(idTickerClass,
                      into = c('idTicker', 'typeSecurity')) %>%
      mutate(across(everything(), str_trim)) %>%
      mutate(
        regionSecurities = idRegion,
        urlJSON = url,
        typeSecurity = if_else(typeSecurity %>% is.na(), 'COMMON', typeSecurity)
      ) %>%
      suppressWarnings() %>%
      dplyr::select(regionSecurities, everything()) %>%
      arrange(idTicker)

    if (return_message) {
      glue::glue("Acquired {nrow(data)} listed securities in {idRegion %>% str_to_title()}") %>% cat(fill = TRUE)
    }

    data
  }

.parse_regions_security_urls <-
  function(urls,
           return_message = TRUE) {
    state <- new.env(parent = emptyenv())

    state$rows <- list()
    success <- function(res) {
      .parse_region_security_url_safe <-
        purrr::possibly(.parse_region_security_url, tibble())
      page_url <- res$url
      data <-
        page_url %>%
        .parse_region_security_url_safe(return_message = return_message)

      state$rows[[length(state$rows) + 1L]] <- data
    }
    failure <- function(msg) {
      tibble()
    }
    urls %>%
      walk(function(x) {
        curl_fetch_multi(url = x, success, failure)
      })
    multi_run()

    dplyr::bind_rows(state$rows)
  }


#' Tradingview regions traded securities
#'
#' Acquires ticker symbols for specified regions
#'
#' @param regions vector of regions \itemize{
#' \item america
#' \item uk
#' \item china
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
#' @param nest_data if \code{TRUE} nest data by region
#' @import jsonlite glue dplyr purrr tidyr stringr
#' @return a \code{tibble} of ticker data by region
#' @export
#'
#' @examples
#' \dontrun{
#' tv_regions_tickers(regions = "america", return_message = TRUE)
#' }
tv_regions_tickers <-
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
      .parse_regions_security_urls(return_message = return_message)

    if (nest_data) {
      all_data <-
        all_data %>%
        nest(dataTickers = -c(urlJSON, regionSecurities))
    }

    janitor::clean_names(all_data)
  }


.parse_metric_dictionary_url <-
  function(url = "https://scanner.tradingview.com/america/metainfo",
           return_message = TRUE) {
    idRegion <-
      url %>% str_replace_all("https://scanner.tradingview.com/|/metainfo", '')

    json_data <-
      url %>%
      jsonlite::fromJSON(flatten = TRUE)

    data <-
      json_data$fields %>%
      as_tibble()

    if (nrow(data) == 0 || ncol(data) == 0) {
      return(tibble())
    }

    legacy_names <- c('nameTW', 'typeField', 'fieldMembers', 'isHH', 'isRR')
    raw_names <- names(data)
    n_cols <- ncol(data)
    new_names <- character(n_cols)
    for (i in seq_len(n_cols)) {
      new_names[i] <- if (i <= length(legacy_names)) legacy_names[i] else raw_names[i]
    }
    data <- data %>% purrr::set_names(new_names)

    fc <- if (!is.null(json_data$financial_currency)) json_data$financial_currency else NA_character_

    data <-
      data %>%
      mutate(regionSecurities = idRegion,
             urlJSON = url,
             financialCurrency = fc) %>%
      dplyr::select(regionSecurities, financialCurrency, everything()) %>%
      tidyr::separate(nameTW,
               into = c('nameTW', 'baseTimeframe'),
               sep = '\\|',
               fill = "right",
               extra = "merge") %>%
      suppressWarnings()

    # Drop non-atomic columns (list-cols) — they break parquet write and
    # the catalog only needs scalar metadata (name + type + flags).
    list_cols <- names(data)[purrr::map_lgl(data, is.list)]
    if (length(list_cols) > 0) {
      data <- data %>% dplyr::select(-dplyr::any_of(list_cols))
    }

    if (return_message) {
      glue::glue("Acquired {nrow(data)} searchable metrics for {idRegion} securities") %>% cat(fill = TRUE)
    }

    data
  }

.parse_metric_dictionaries_url <-
  function(urls,
           return_message = TRUE) {
    state <- new.env(parent = emptyenv())

    state$rows <- list()
    success <- function(res) {
      .parse_metric_dictionary_url_safe <-
        purrr::possibly(.parse_metric_dictionary_url, tibble())
      page_url <- res$url
      data <-
        page_url %>%
        .parse_metric_dictionary_url_safe(return_message = return_message)

      state$rows[[length(state$rows) + 1L]] <- data
    }
    failure <- function(msg) {
      tibble()
    }
    urls %>%
      walk(function(x) {
        curl_fetch_multi(url = x, success, failure)
      })
    multi_run()

    dplyr::bind_rows(state$rows)
  }

#' Tradingview searchable metrics by region
#'
#' Get searchable metrics by region
#' @param regions vector of regions \itemize{
#' \item cfd
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
#' @param nest_data if \code{TRUE} nest data by region
#' @import jsonlite glue dplyr purrr tidyr stringr
#' @return a \code{tibble} of metrics data by region
#' @export
#'
#' @examples
#' \dontrun{
#' tv_regions_metrics(regions = "america", return_message = TRUE)
#' }
tv_regions_metrics <-
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
      .parse_metric_dictionaries_url(return_message = return_message)

    if (nest_data) {
      all_data <-
        all_data %>%
        nest(dataMetrics = -c(urlJSON, regionSecurities))
    }

    janitor::clean_names(all_data)
  }



# metric_query ------------------------------------------------------------

#' Default Bond query metric
#'
#' @param column_names if not \code{NULL} vector of column names
#'
#' @return a list representing a TradingView bond query
#' @export
#'
#' @examples
#' \dontrun{
#' generate_tv_bond_query()
#' }
generate_tv_bond_query <-
  function(column_names = c("country_code", "name", "coupon", "maturity_date", "close",
                            "change", "change_abs", "high", "low", "Recommend.All", "description",
                            "subtype", "pricescale", "minmov", "fractional", "minmove2",
                            "ask", "all_time_low", "sector", "country2", "bid", "country",
                            "current_session", "all_time_high", "rtc", "type", "expiration",
                            "exchange", "pre_change", "pre_change_abs", "change_from_open_abs",
                            "change_from_open", "post_change", "time", "open", "gap")
  ){
    data <- structure(list(structure(list(c("forex_priority", "sector", "description"
    ), c("nempty", "equal", "match"), c(NA, "bond", "YIELD$")), .Names = c("left",
                                                                           "operation", "right"), class = "data.frame", row.names = c(NA,
                                                                                                                                      3L)), structure(list(structure(list(list()), .Names = "types"),
                                                                                                                                                           list()), .Names = c("query", "tickers")), c("country_code",
                                                                                                                                                                                                       "name", "coupon", "maturity_date", "close", "change", "change_abs",
                                                                                                                                                                                                       "high", "low", "Recommend.All", "description", "name", "subtype",
                                                                                                                                                                                                       "pricescale", "minmov", "fractional", "minmove2"), structure(list(
                                                                                                                                                                                                         "forex_priority", "asc"), .Names = c("sortBy", "sortOrder"
                                                                                                                                                                                                         )), structure(list("en"), .Names = "lang"), c(0L, 30000L)), .Names = c("filter",
                                                                                                                                                                                                                                                                                "symbols", "columns", "sort", "options", "range"))
    if (length(column_names) > 0){
      data$columns <- column_names
    }
    data
  }

#' Generate tradeview metric query
#'
#' @param filter filter conditions as a tibble
#' @param symbols symbols query parameters as a list
#' @param metrics vector of metric column names to retrieve
#' @param sort sort parameters as a list
#' @param options options parameters as a list
#' @param range range vector for pagination
#'
#' @return a list representing a TradingView metric query
#' @export
#' @import reticulate magrittr glue dplyr
#' @examples
#' \dontrun{
#' tv_metric()
#' }
tv_metric <-
  function(filter = tibble(left = 'market_cap_basic',
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
      toJSON(auto_unbox = TRUE)

    data_query
  }


.parse_tv_metric_url <-
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
      as.character() %>%
      jsonlite::fromJSON(flatten = TRUE)


    json$close()
    data <-
      json_data$data %>% as_tibble() %>% unnest(cols = where(is.list))

    data <- data %>%
      purrr::set_names(c('idExchangeTicker', 'value')) %>%
      group_by(idExchangeTicker) %>%
      mutate(countItem = seq_len(n())) %>%
      ungroup()

    data <-
      data %>%
      tidyr::separate(idExchangeTicker,
                      into = c('idExchange',
                               'idTickerClass'),
                      sep = '\\:') %>%
      tidyr::separate(idTickerClass,
                      into = c('idTicker', 'typeSecurity')) %>%
      mutate(across(everything(), str_trim)) %>%
      mutate(idRegion,
             typeSecurity = if_else(typeSecurity %>% is.na(), 'COMMON', typeSecurity)) %>%
      suppressWarnings() %>%
      dplyr::select(idRegion, everything())

    if (return_message) {
      tickers <-
        data$idTicker %>% unique() %>% length() %>% formattable::comma(digits = 0)
      glue::glue(
        "Acquired {nrow(data) %>% formattable::comma(digits = 0)} listed metrics for {tickers} securities in {idRegion %>% str_to_title()}"
      ) %>% cat(fill = TRUE)
    }

    gc()
    data
  }

#' Generate default TV query metric
#'
#' @return a list representing a default TradingView query
#' @export
#'
#' @examples
#' \dontrun{
#' tv_metric_query()
#' }
tv_metric_query <-
  function() {
    list(
      filter = tibble(left = 'market_cap_basic',
                          operation = 'nempty'),
      symbols = list(query = list(types = c(
        'stock', 'fund', 'dr'
      ))),
      metrics =
        c(
          "subtype",
          "beta_5_year",
          "earnings_release_date",
          "earnings_per_share_forecast_next_fq",
          "operating_margin",
          "return_on_equity",
          "current_ratio",
          "debt_to_assets",
          "price_revenue_ttm",
          "amount_recent",
          "market_cap_basic",
          "ebitda",
          "fundamental_currency_code",
          "total_assets",
          "current_session",
          "earnings_per_share_fq",
          "earnings_per_share_forecast_fq",
          "earnings_release_next_time",
          "cash_ratio",
          "yield_upcoming",
          "sector",
          "basic_eps_net_income",
          "price_book_ratio",
          "quick_ratio",
          "net_debt",
          "total_shares_outstanding_fundamental",
          "enterprise_value_fq",
          "beta_3_year",
          "total_capital",
          "earnings_per_share_diluted_ttm",
          "last_annual_eps",
          "revenue_fq",
          "ex_dividend_date_recent",
          "price_earnings_ttm",
          "debt_to_equity",
          "pre_tax_margin",
          "debt_to_equity_fq",
          "number_of_employees",
          "total_current_assets",
          "last_annual_revenue",
          "revenue_forecast_fq",
          "industry",
          "return_on_assets",
          "return_of_invested_capital_percent_ttm",
          "return_on_invested_capital",
          "gross_profit",
          "dividends_paid",
          "preferred_dividends",
          "earnings_release_next_date",
          "dividends_yield",
          "price_sales_ratio",
          "yield_recent",
          "ex_dividend_date_upcoming",
          "total_shares_outstanding",
          "price_earnings_to_growth_ttm",
          "price_book_fq",
          "enterprise_value_ebitda_ttm",
          "rtc",
          "amount_upcoming",
          "average_volume",
          "revenue_per_employee",
          "after_tax_margin",
          "net_income",
          "earnings_release_time",
          "type",
          "dividends_per_share_fq",
          "payment_date_upcoming",
          "gross_margin_percent_ttm",
          "earnings_per_share_basic_ttm",
          "price_free_cash_flow_ttm",
          "long_term_capital",
          "total_debt",
          "country",
          "total_revenue",
          "gross_margin",
          "number_of_shareholders",
          "beta_1_year",
          "goodwill",
          "expected_annual_dividends",
          "revenue_forecast_next_fq",
          "payment_date_recent",
          "low",
          "volume",
          "pre_change_abs",
          "gap",
          "open",
          "volume",
          "pre_change_abs",
          "time",
          "change_from_open",
          "low",
          "high",
          "close",
          "volume",
          "change_abs",
          "open",
          "change_from_open",
          "change_abs",
          "time",
          "change",
          "pre_change_abs",
          "time",
          "gap",
          "high",
          "open",
          "change_from_open",
          "change_abs",
          "low",
          "close",
          "change",
          "change_abs",
          "close",
          "time",
          "change",
          "pre_change",
          "close",
          "high",
          "gap",
          "change",
          "open",
          "high",
          "pre_change",
          "pre_change",
          "pre_change_abs",
          "gap",
          "pre_change",
          "change_from_open",
          "low",
          "volume",
          "relative_volume",
          "type",
          "subtype",
          "eps_surprise_fq",
          "market_cap_calc",
          "exchange",
          "price_sales",
          "eps_surprise_percent_fq"
        )
      ,
      sort = list(sortBy = 'market_cap_basic',
                  sortOrder = 'desc'),
      options = list(lang = 'eng'),
      range = c(0, 15000000000000)
    )
  }
.tv_metric_data <-
  function(region = c( 'america'),
           query =
             list(
               filter = tibble(left = 'market_cap_basic',
                                   operation = 'nempty'),
               symbols = list(query = list(types = c(
                 'stock', 'fund', 'dr'
               ))),
               metrics =
                 c(
                   "subtype",
                   "name",
                   "beta_5_year",
                   "earnings_release_date",
                   "earnings_per_share_forecast_next_fq",
                   "operating_margin",
                   "return_on_equity",
                   "current_ratio",
                   "debt_to_assets",
                   "price_revenue_ttm",
                   "amount_recent",
                   "market_cap_basic",
                   "ebitda",
                   "fundamental_currency_code",
                   "total_assets",
                   "current_session",
                   "earnings_per_share_fq",
                   "earnings_per_share_forecast_fq",
                   "earnings_release_next_time",
                   "cash_ratio",
                   "yield_upcoming",
                   "sector",
                   "basic_eps_net_income",
                   "price_book_ratio",
                   "quick_ratio",
                   "net_debt",
                   "total_shares_outstanding_fundamental",
                   "enterprise_value_fq",
                   "beta_3_year",
                   "total_capital",
                   "earnings_per_share_diluted_ttm",
                   "last_annual_eps",
                   "revenue_fq",
                   "ex_dividend_date_recent",
                   "price_earnings_ttm",
                   "debt_to_equity",
                   "pre_tax_margin",
                   "debt_to_equity_fq",
                   "number_of_employees",
                   "total_current_assets",
                   "last_annual_revenue",
                   "revenue_forecast_fq",
                   "industry",
                   "return_on_assets",
                   "return_of_invested_capital_percent_ttm",
                   "return_on_invested_capital",
                   "gross_profit",
                   "dividends_paid",
                   "preferred_dividends",
                   "earnings_release_next_date",
                   "dividends_yield",
                   "price_sales_ratio",
                   "yield_recent",
                   "ex_dividend_date_upcoming",
                   "total_shares_outstanding",
                   "price_earnings_to_growth_ttm",
                   "price_book_fq",
                   "enterprise_value_ebitda_ttm",
                   "rtc",
                   "amount_upcoming",
                   "average_volume",
                   "revenue_per_employee",
                   "after_tax_margin",
                   "net_income",
                   "earnings_release_time",
                   "type",
                   "dividends_per_share_fq",
                   "payment_date_upcoming",
                   "gross_margin_percent_ttm",
                   "earnings_per_share_basic_ttm",
                   "price_free_cash_flow_ttm",
                   "long_term_capital",
                   "total_debt",
                   "country",
                   "total_revenue",
                   "gross_margin",
                   "number_of_shareholders",
                   "beta_1_year",
                   "goodwill",
                   "expected_annual_dividends",
                   "revenue_forecast_next_fq",
                   "payment_date_recent",
                   "low",
                   "volume",
                   "pre_change_abs",
                   "gap",
                   "open",
                   "volume",
                   "pre_change_abs",
                   "time",
                   "change_from_open",
                   "low",
                   "high",
                   "close",
                   "volume",
                   "change_abs",
                   "open",
                   "change_from_open",
                   "change_abs",
                   "time",
                   "change",
                   "pre_change_abs",
                   "time",
                   "gap",
                   "high",
                   "open",
                   "change_from_open",
                   "change_abs",
                   "low",
                   "close",
                   "change",
                   "change_abs",
                   "close",
                   "time",
                   "change",
                   "pre_change",
                   "close",
                   "high",
                   "gap",
                   "change",
                   "open",
                   "high",
                   "pre_change",
                   "pre_change",
                   "pre_change_abs",
                   "gap",
                   "pre_change",
                   "change_from_open",
                   "low",
                   "volume",
                   "relative_volume",
                   "type",
                   "subtype",
                   "eps_surprise_fq",
                   "market_cap_calc",
                   "exchange",
                   "price_sales",
                   "eps_surprise_percent_fq"
                 )
               ,
               sort = list(sortBy = 'market_cap_basic',
                           sortOrder = 'desc'),
               options = list(lang = 'eng'),
               range = c(0, 15000000000000)
             ),
           return_message = TRUE) {
    glue::glue("\n\nWARNING -- this function requires Python and the requests module!!!!\n\n") %>%
      cat(fill = TRUE)

    urls <-
      glue::glue("https://scanner.tradingview.com/{region}/scan")

    if (!region %>% str_to_lower() %>% str_detect("cfd")) {
      data_query <-
        tv_metric(
          filter = query$filter,
          symbols = query$symbols,
          metrics = query$metrics,
          sort = query$sort,
          options = query$options,
          range = query$range
        )

    } else {
      data_query <- toJSON(query, auto_unbox = TRUE)
    }

    data <-
      seq_along(urls) %>%
      future_map_dfr(function(x) {
        .parse_tv_metric_url(url = urls[x], data_query = data_query)
      }) %>%
      mutate(countItem = countItem %>% as.integer())

    if (!region %>% str_to_lower() %>% str_detect("cfd")) {
      metrics <-
        tibble(nameTW = c('name', query$metrics) %>% unique()) %>%
        mutate(countItem = seq_len(n()))

      data <-
        data %>%
        left_join(metrics) %>%
        suppressMessages()
    } else {

      metrics <-
        tibble(nameTW = c('name', query$columns) %>% unique()) %>%
        mutate(countItem = seq_len(n()))

      data <-
        data %>%
        left_join(metrics) %>%
        suppressMessages()

      data <- data %>% filter(value != idTicker)
    }

    if (!region %>% str_to_lower() %>% str_detect("cfd")) {
      data <-
        data %>%
        mutate(idTicker = ifelse(nameTW == "name", value, NA)) %>%
        fill(idTicker) %>%
        filter(!nameTW == 'name')
    }

    df_metrics <-
      tv_regions_metrics(regions = region)

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
      filter(!is.na(value))

    df_companies <-
      df_companies %>%
      pivot_wider(names_from = nameTW, values_from = value)

    df_values <-
      data %>%
      filter(!typeField %in% c(NA, 'text')) %>%
      dplyr::select(idExchange:value, nameTW) %>%
      mutate(value = value %>% readr::parse_number()) %>%
      pivot_wider(names_from = nameTW, values_from = value)

    data <-
      df_companies %>%
      left_join(df_values) %>%
      suppressMessages() %>%
      dplyr::select(which(colMeans(is.na(.)) < 1))

    data <-
      data %>%
      mutate(across(where(is.character),
                ~ifelse(. == "", NA, .) %>% str_trim() %>% str_to_upper()
                ))

    df_fields <-
      df_metrics %>%
      count(typeField)
    fields <- df_fields$typeField

    fields %>%
      walk(function(field){

        if (field == "text") {
          col_names <- df_metrics %>% filter(typeField == "text") %>% pull(nameTW)
          mutate_cols <-
            names(data)[names(data) %in% col_names]
          if (length(mutate_cols) > 0) {
            data <<-
              data %>%
              mutate(across(all_of(mutate_cols),
                        ~gsub("^\\s+|\\s+$", "", .) %>%
                            str_to_upper() %>% str_trim()
                        ))
          }
        }

        if (field == "number") {
          col_names <- df_metrics %>% filter(typeField == "number") %>% pull(nameTW)
          mutate_cols <- names(data)[names(data) %in% col_names]
          if (length(mutate_cols) > 0) {
            data <<-
              data %>%
              mutate(across(all_of(mutate_cols),
                        ~formattable::comma(., digits = 2)
                        ))
          }
        }

        if (field == "percent") {
          col_names <-
            df_metrics %>% filter(typeField == "percent") %>% pull(nameTW)
          mutate_cols <-
            names(data)[names(data) %in% col_names]
          if (length(mutate_cols) > 0) {
            data <<-
              data %>%
              mutate(across(all_of(mutate_cols),
                        ~formattable::percent(. / 100, digits = 2)
                        ))
          }
        }

        if (field == "price") {
          col_names <-
            df_metrics %>% filter(typeField == "price") %>% pull(nameTW)
          mutate_cols <-
            names(data)[names(data) %in% col_names]
          if (length(mutate_cols) > 0) {
            data <<-
              data %>%
              mutate(across(all_of(mutate_cols),
                        ~formattable::currency(., digits = 2)
                        ))
          }
        }
        if (field == "time") {
          col_names <-
            df_metrics %>% filter(typeField == "time") %>% pull(nameTW)
          mutate_cols <-
            names(data)[names(data) %in% col_names]
          if (length(mutate_cols) > 0) {
            data <<-
              data %>%
              mutate(across(all_of(mutate_cols),
                        ~as.POSIXct(., origin = "1970-01-01", tz = "UTC")
                        ))
          }
        }


      })

    data <-
      data %>%
      separate(idTicker, sep = "/", into = c("idTicker", "classSecurity")) %>%
      mutate(across(where(is.character),
                ~ifelse(. == "", NA, .) %>% str_trim() %>% str_to_upper()
                )) %>%
      suppressMessages() %>%
      suppressWarnings()


    data <-
      data %>%
      janitor::clean_names() %>%
      dplyr::select(which(colMeans(is.na(.)) < 1))

    if (region %>% str_to_lower() %>% str_detect("cfd") %>% sum(na.rm = TRUE) > 0) {
      data <-
        data %>%
        mutate(type_security = "BOND")

      if (data %>% tibble::has_name("country")){
        data <- data %>% rename(region= country)

      }

      if (data %>% tibble::has_name("descripiton")) {
        data %>%
          mutate(
            country_duration = description,
            country_duration = country_duration %>% gsub(pattern = "\\ GOVERNMENT BONDS ", replacement = "\\-", .)
          ) %>%
          separate(country_duration,
                   into = c("country", "duration_slug"),
                   sep = "\\-") %>%
          mutate(
            duration_slug = duration_slug %>% gsub("\\ YIELD", "", .),
            days_duration_bond =
              case_when(
                duration_slug == "1 YR" ~ 360,
                duration_slug == "2 YR" ~ 2 * 360,
                duration_slug == "3 YR" ~ 3 * 360,
                duration_slug == "5 YR" ~ 5 * 360,
                duration_slug == "10 YR" ~ 10 * 360,
                duration_slug == "15 YR" ~ 15 * 360,
                duration_slug == "30 YR" ~ 30 * 360,
                duration_slug == "20 YR" ~ 20 * 360,
                duration_slug == "3 MO" ~ 90,
                duration_slug == "6 MO" ~ 180 ,
                duration_slug == "25 YR" ~ 25 * 360,
                duration_slug == "50 YR" ~ 50 * 360,
                duration_slug ==  "40 YR" ~ 40 * 360,
                duration_slug == "1 MO" ~ 90
              )
          )
      }

    }

    data

  }

#' Tradeview region metrics
#'
#' Get data for trade view query
#'
#' @param regions vector of regions \itemize{
#' \item cfd
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
#' \item metrics - vector of parameters see \code{tv_regions_tickers} for options
#' \item sort - sort paramters
#' \item options- sort options
#' }
#' @param return_message if \code{TRUE} returns a message
#'
#' @return a \code{tibble} of metrics data
#' @export
#' @import reticulate dplyr purrr stringr glue
#'
#' @examples
#' \dontrun{
#' tv_metrics_data(regions = c('america'))
#' }
tv_metrics_data <-
  function(regions = c( 'america'),
           query =
             list(
               filter = tibble(left = 'market_cap_basic',
                                   operation = 'nempty'),
               symbols = list(query = list(types = c(
                 'stock', 'fund', 'dr'
               ))),
               metrics =
                 c(
                   "subtype",
                   "name",
                   "beta_5_year",
                   "earnings_release_date",
                   "earnings_per_share_forecast_next_fq",
                   "operating_margin",
                   "return_on_equity",
                   "current_ratio",
                   "debt_to_assets",
                   "price_revenue_ttm",
                   "amount_recent",
                   "market_cap_basic",
                   "ebitda",
                   "fundamental_currency_code",
                   "total_assets",
                   "current_session",
                   "earnings_per_share_fq",
                   "earnings_per_share_forecast_fq",
                   "earnings_release_next_time",
                   "cash_ratio",
                   "yield_upcoming",
                   "sector",
                   "basic_eps_net_income",
                   "price_book_ratio",
                   "quick_ratio",
                   "net_debt",
                   "total_shares_outstanding_fundamental",
                   "enterprise_value_fq",
                   "beta_3_year",
                   "total_capital",
                   "earnings_per_share_diluted_ttm",
                   "last_annual_eps",
                   "revenue_fq",
                   "ex_dividend_date_recent",
                   "price_earnings_ttm",
                   "debt_to_equity",
                   "pre_tax_margin",
                   "debt_to_equity_fq",
                   "number_of_employees",
                   "total_current_assets",
                   "last_annual_revenue",
                   "revenue_forecast_fq",
                   "industry",
                   "return_on_assets",
                   "return_of_invested_capital_percent_ttm",
                   "return_on_invested_capital",
                   "gross_profit",
                   "dividends_paid",
                   "preferred_dividends",
                   "earnings_release_next_date",
                   "dividends_yield",
                   "price_sales_ratio",
                   "yield_recent",
                   "ex_dividend_date_upcoming",
                   "total_shares_outstanding",
                   "price_earnings_to_growth_ttm",
                   "price_book_fq",
                   "enterprise_value_ebitda_ttm",
                   "rtc",
                   "amount_upcoming",
                   "average_volume",
                   "revenue_per_employee",
                   "after_tax_margin",
                   "net_income",
                   "earnings_release_time",
                   "type",
                   "dividends_per_share_fq",
                   "payment_date_upcoming",
                   "gross_margin_percent_ttm",
                   "earnings_per_share_basic_ttm",
                   "price_free_cash_flow_ttm",
                   "long_term_capital",
                   "total_debt",
                   "country",
                   "total_revenue",
                   "gross_margin",
                   "number_of_shareholders",
                   "beta_1_year",
                   "goodwill",
                   "expected_annual_dividends",
                   "revenue_forecast_next_fq",
                   "payment_date_recent",
                   "low",
                   "volume",
                   "pre_change_abs",
                   "gap",
                   "open",
                   "volume",
                   "pre_change_abs",
                   "time",
                   "change_from_open",
                   "low",
                   "high",
                   "close",
                   "volume",
                   "change_abs",
                   "open",
                   "change_from_open",
                   "change_abs",
                   "time",
                   "change",
                   "pre_change_abs",
                   "time",
                   "gap",
                   "high",
                   "open",
                   "change_from_open",
                   "change_abs",
                   "low",
                   "close",
                   "change",
                   "change_abs",
                   "close",
                   "time",
                   "change",
                   "pre_change",
                   "close",
                   "high",
                   "gap",
                   "change",
                   "open",
                   "high",
                   "pre_change",
                   "pre_change",
                   "pre_change_abs",
                   "gap",
                   "pre_change",
                   "change_from_open",
                   "low",
                   "volume",
                   "relative_volume",
                   "type",
                   "subtype",
                   "eps_surprise_fq",
                   "market_cap_calc",
                   "exchange",
                   "price_sales",
                   "eps_surprise_percent_fq"
                 )
               ,
               sort = list(sortBy = 'market_cap_basic',
                           sortOrder = 'desc'),
               options = list(lang = 'eng'),
               range = c(0, 15000000000000)
             ),
           return_message = TRUE) {
    .tv_metric_data_safe <-
      purrr::possibly(.tv_metric_data, tibble())
    regions %>%
      map_df(function(region){
        .tv_metric_data_safe(region = region, query = query, return_message = return_message)
      })
  }


# news --------------------------------------------------------------------

# https://news-headlines.tradingview.com/headlines/yahoo/symbol/FB/?locale=en

.get_ticker_tradingview_news <-
  function(ticker = "FB") {
    url <-
      glue::glue("https://news-headlines.tradingview.com/headlines/yahoo/symbol/{ticker}")

    data <-
      url %>%
      jsonlite::fromJSON(simplifyDataFrame = TRUE) %>%
      dplyr::as_tibble() %>%
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

.parse_trading_view_news_url <-
  function(url = "https://news-headlines.tradingview.com/v2/headlines?category=stock&client=web&lang=en&symbol=NASDAQ:AAPL",
           return_message = TRUE) {
    # Extract symbol from query string. Supports both legacy proSymbol= and v2 symbol=.
    # Base-R only — avoid latent urltools dep.
    q <- sub("^[^?]+\\?", "", url)
    parts <- strsplit(q, "&")[[1]]
    sym_param <- parts[grepl("^(symbol|proSymbol)=", parts)]
    raw_sym <- if (length(sym_param) > 0) {
      utils::URLdecode(sub("^(symbol|proSymbol)=", "", sym_param[1]))
    } else {
      NA_character_
    }
    # symbol may be EXCHANGE:TICKER — keep both
    if (!is.na(raw_sym) && grepl(":", raw_sym, fixed = TRUE)) {
      idExchange <- sub(":.*$", "", raw_sym)
      ticker <- sub("^[^:]+:", "", raw_sym)
    } else {
      idExchange <- NA_character_
      ticker <- raw_sym
    }

    if (return_message) {
      glue::glue("Acquiring Tradingview news for {raw_sym}") %>%
        cat(fill = TRUE)
    }

    json_data <-
      tryCatch(
        jsonlite::fromJSON(url, flatten = TRUE, simplifyDataFrame = TRUE),
        error = function(e) NULL
      )

    if (is.null(json_data) || is.null(json_data$items) || length(json_data$items) == 0) {
      return(tibble())
    }

    items <- json_data$items %>% as_tibble()

    # v2 schema: id, title, provider, sourceLogoId, published (unix), source, urgency,
    # relatedSymbols (list), storyPath
    data <- items %>%
      mutate(
        idTicker = ticker,
        idExchange = idExchange,
        symbolFull = raw_sym,
        datetimePublished = anytime::anytime(.data$published),
        titleArticle = .data$title,
        sourceArticle = .data$source,
        urlArticle = ifelse(
          !is.null(.data$storyPath),
          paste0("https://www.tradingview.com", .data$storyPath),
          NA_character_
        ),
        urlJSON = url
      ) %>%
      dplyr::select(
        idTicker, idExchange, symbolFull,
        datetimePublished, titleArticle, sourceArticle, urlArticle,
        idArticle = .data$id,
        providerArticle = .data$provider,
        urgencyArticle = .data$urgency,
        urlJSON
      )

    data
  }

.parse_tradingview_news_urls <-
  function(urls,
           return_message = TRUE) {
    state <- new.env(parent = emptyenv())

    state$rows <- list()
    success <- function(res) {
      .parse_trading_view_news_url_safe <-
        purrr::possibly(.parse_trading_view_news_url, tibble())
      page_url <- res$url
      data <-
        page_url %>%
        .parse_trading_view_news_url_safe(return_message = return_message)

      state$rows[[length(state$rows) + 1L]] <- data
    }
    failure <- function(msg) {
      tibble()
    }
    urls %>%
      walk(function(x) {
        curl_fetch_multi(url = x, success, failure)
      })
    multi_run()

    dplyr::bind_rows(state$rows)
  }


#' Tradingview tickers news
#'
#' Returns news data for specified tickers
#'
#' @param tickers vector of ticker symbols
#' @param return_message if \code{TRUE} returns a message
#' @param nest_data if \code{TRUE} nest data by ticker
#' @import dplyr tibble glue anytime tidyr curl jsonlite
#' @return a \code{tibble} of news data
#' @export
#'
#' @examples
#' \dontrun{
#' tv_tickers_news(tickers = c("VNO", "AVB", "PEI"), return_message = TRUE, nest_data = FALSE)
#' }

tv_tickers_news <-
  function(tickers = c("FB", "AAPL", "NFLX", "GOOG", "VNO", "EQR", "BXP"),
           default_exchange = "NASDAQ",
           return_message = TRUE,
           nest_data = FALSE) {
    # Accept either bare tickers ("AAPL") or EXCHANGE:TICKER ("NASDAQ:AAPL").
    # Bare tickers get prefixed with default_exchange for v2 endpoint compatibility.
    symbols <- ifelse(grepl(":", tickers, fixed = TRUE),
                      tickers,
                      paste0(default_exchange, ":", tickers))
    encoded <- utils::URLencode(symbols, reserved = TRUE)
    urls <-
      glue::glue("https://news-headlines.tradingview.com/v2/headlines?category=stock&client=web&lang=en&symbol={encoded}")

    all_data <-
      urls %>%
      .parse_tradingview_news_urls(return_message = return_message) %>%
      mutate(across(where(is.character),
                ~ifelse(. == "", NA, .) %>% str_trim()
                )) %>%
      suppressMessages() %>%
      suppressWarnings()

    if (nest_data && nrow(all_data) > 0) {
      all_data <-
        all_data %>%
        tidyr::nest(tickerNews = -c(idTicker, urlJSON))
    }

    janitor::clean_names(all_data)
  }