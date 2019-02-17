.extract_date_time <-
  function(x = "Mon Nov 12 10:42:35 UTC-05:00 2018") {
    total_chars <- x %>% nchar()
    year_time <-
      x %>% substr(total_chars -3 , total_chars)

    non_year <-
      x %>% substr(1, total_chars -4) %>% str_trim() %>% str_split("\\ UTC") %>%
      flatten_chr() %>% .[[1]]

    md <- non_year %>% substr(4, nchar(non_year)) %>% str_trim()

    x <-
      glue::glue("{year_time} {md}") %>% lubridate::ymd_hms()

    x
  }


.get_data_ticker_trade <-
  function(ticker = "bxp" , return_message = TRUE) {
  json_url <-
    stringr::str_c('http://dev.markitondemand.com/MODApis/Api/v2/Quote/json?symbol=', ticker)
  json_data <-
    json_url %>%
    jsonlite::fromJSON()

  if ("Message" %in% names(json_data)) {
    stop(str_c("No MARKIT ticker data for ", ticker))
  }
  df_ticker <-
    json_data %>%
    flatten_df() %>%
    purrr::set_names(
      c(
        'statusSearch',
        'nameCompany',
        'idTicker',
        'priceLast',
        'priceChange',
        'pctChange',
        'datetimeTradeLast',
        'dateMS',
        'amountMarketCapitalization',
        'countVolumeShares',
        'priceYearStart',
        'pctChangeYTD',
        'priceHigh',
        'priceLow',
        'priceOpen'
      )
    ) %>%
    dplyr::select(-c(dateMS, statusSearch)) %>%
    mutate(datetimeTradeLast = datetimeTradeLast %>% str_replace_all(" UTC-04:00 ", ' '))

  date_time_price <- df_ticker$datetimeTradeLast %>% .extract_date_time()

  df_ticker <- df_ticker %>%
    mutate(datetimeTradeLast = date_time_price) %>%
    mutate(countSharesOutstanding = amountMarketCapitalization / priceLast) %>%
    select(datetimeTradeLast, everything())

  if (return_message) {
    glue::glue("Parsed trades for {str_to_upper(ticker)}") %>% cat(fill = T)
  }

df_ticker
}


#' Tickers trades
#'
#' @param tickers
#' @param return_message
#'
#' @return
#' @export
#' @import dplyr curl jsonlite stringr dplyr purrr lubridate
#'
#' @examples
#' tickers_trades(tickers = c("VNO", "BXP", "FB"))
#'
tickers_trades <-
  function(tickers = c("PEI","bxp") , return_message = TRUE) {
    .get_data_ticker_trade_safe <-
      purrr::possibly(.get_data_ticker_trade, tibble())

    all_data <-
      tickers %>%
      future_map_dfr(function(x){
        .get_data_ticker_trade_safe(ticker = x, return_message = return_message)
      })

    all_data <-
      all_data %>%
      mutate_at(all_data %>% dplyr::select(dplyr::matches("^price")) %>% names(),
                funs(. %>% formattable::currency(digits = 2))) %>%
      mutate_at(all_data %>% dplyr::select(dplyr::matches("^pct")) %>% names(),
                funs((. / 100) %>% formattable::percent(digits = 3))) %>%
      mutate_at(all_data %>% dplyr::select(dplyr::matches("^count")) %>% names(),
                funs(. %>% formattable::comma(digits = 0))) %>%
      mutate_at(all_data %>% dplyr::select(dplyr::matches("^amount")) %>% names(),
                funs(. %>% formattable::currency(digits = 0))) %>%
      mutate(
        priceYearStart = priceLast / (1 + pctChangeYTD) ,
        amountMarketCapitalizationYearStart = amountMarketCapitalization / (1 + pctChangeYTD)
      ) %>%
      dplyr::select(datetimeTradeLast, everything()) %>%
      suppressWarnings()


    return(all_data)
  }


.companies_tickers <-
  function(company = "Netflix", return_message = TRUE) {
  json_url <-
    stringr::str_c("http://dev.markitondemand.com/MODApis/Api/v2/Lookup/json?input=", company)

  data <-
    json_url %>%
    jsonlite::fromJSON() %>%
    data.frame(stringsAsFactors = FALSE) %>%
    as_tibble() %>%
    purrr::set_names(c('idTicker', 'nameCompany', 'nameExchange')) %>%
    mutate(nameCompanySearch = company) %>%
    dplyr::select(nameCompanySearch, everything())

  if (return_message) {
    list("Found ", nrow(data) %>% formattable::comma(digits = 0), ' ticker(s) matching ', company) %>%
      purrr::reduce(paste0) %>%
      cat(fill = T)
  }
  return(data)
}

#' Companies ticker symbols
#'
#' Company ticker data
#'
#' @param companies vector of comapnies
#' @param include_trades if \code{TRUE} includes trade data
#' @references \href{http://markit.com}{MARKIT}
#' @return a \code{date_frame}
#' @export
#' @import dplyr curl jsonlite stringr dplyr purrr lubridate
#' @examples
companies_tickers <-
  function(companies = c("Vornado", "Snap"), include_trades = TRUE,
           return_message = TRUE) {
    .companies_tickers_safe <-
      purrr::possibly(.companies_tickers, tibble())

    all_data <-
      companies %>%
      future_map_dfr(function(x) {
        .companies_tickers_safe(company = x)
      })

    if (include_trades) {
      tickers <- all_data$idTicker %>% unique()
      tickers_trades_safe <-
        purrr::possibly(tickers_trades, tibble())
      df_trades <-
        tickers %>%
        tickers_trades(return_message = return_message) %>%
        suppressMessages()

      all_data <-
        all_data %>%
        left_join(df_trades) %>%
        suppressMessages()
    }

    return(all_data)
  }


# ocr ---------------------------------------------------------------------

function(url = "http://www.markit.com/Company/Files/DownloadFiles?CMSID=f8c7fb31019e421a8b5d3d0a9980211c") {

}
