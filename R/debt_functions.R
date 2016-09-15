get_setttlement_type_df <-
  function() {
    settlement_type_df <-
      data_frame(
        productName = c(
          "Eurodollar Futures",
          "30 Day Federal Funds Futures",
          "2-Year Eurodollar Bundle Futures",
          "3-Year Eurodollar Bundle Futures",
          "5-Year Eurodollar Bundle Futures"
        ),
        productCode = c("GE", "ZQ", "BU2", "BU3", "BU5"),
        idSettlementType = c(
          "2_PRIOR_LAST_WED",
          "LAST_BDAY_MONTH",
          '2_PRIOR_LAST_WED',
          '2_PRIOR_LAST_WED',
          '2_PRIOR_LAST_WED'
        )
      )
    return(settlement_type_df)
  }


parse_settlement_date <-
  function(date = "2016-09-01",
           settlement_id = '2_PRIOR_LAST_WED') {
    date <-
      date %>% ymd
    month_number <-
      date %>% month

    year_number <-
      date %>% year

    if (settlement_id == '2_PRIOR_LAST_WED') {
      date_third_weds <-
        RcppBDT::getNthDayOfWeek(third, Wed, month_number, year_number) %>% format %>% ymd

      settlement_date <-
        RQuantLib::adjust(calendar = "TARGET",
                          dates = date_third_weds - 2,
                          bdc = 0) %>%
        ymd
    }

    if (settlement_id == 'LAST_BDAY_MONTH') {
      settlement_date <-
        tis::lastBusinessDayOfMonth(date) %>% as.character() %>% ymd
    }
    settlement_date <-
      settlement_date %>% as.character()
    return(settlement_date)
  }


get_split_mixed_char_title <-
  function(x = 'namePersonActor') {
    full_title <-
      x %>%
      str_replace_all("([a-z])([A-Z])", "\\1SPLITHERE\\2") %>%
      str_split("SPLITHERE") %>%
      purrr::flatten_chr()

    return(x)

  }


is_all_cap_word <-
  function(x = "md") {
    if (x %>% nchar == 2) {
      title_word <-
        x %>% str_to_upper()
    } else {
      title_word <-
        x %>% str_to_title()
    }
    return(title_word)
  }

fix_names <-
  function(x = "idSettlementType") {
    keep_words <-
      c('has', 'is', 'url', 'pct', 'id')

    name_vec <-
      x %>% str_replace_all("([a-z])([A-Z])", "\\1SPLITHERE\\2") %>%
      str_split("SPLITHERE") %>%
      purrr::flatten_chr()
    if (name_vec %>% length == 1) {
      new_name <-
        name_vec[1] %>% str_to_lower()
    }

    if (name_vec %>% length == 2) {
      if (name_vec[1] %in% keep_words) {
        new_name <-
          name_vec[1] %>% str_to_lower() %>%
          paste0(name_vec[2] %>% is_all_cap_word())
      } else {
        new_name <-
          name_vec[2] %>% str_to_lower() %>%
          paste0(name_vec[1] %>% is_all_cap_word())
      }
    }
    if (name_vec %>% length > 2) {
      first_no <-
        name_vec %>% length
      if (name_vec[1] %in% keep_words) {
        first_word <-
          name_vec[1] %>% str_to_lower()

        other_words <-
          2:(first_no) %>%
          purrr::map(function(x)
            is_all_cap_word(name_vec[x])) %>%
          flatten_chr %>%
          paste0(collapse = '')

        new_name <-
          first_word %>%
          paste0(other_words)
      } else {
        first_word <-
          name_vec[first_no] %>% str_to_lower()

        other_words <-
          1:(first_no - 1) %>%
          purrr::map(function(x)
            is_all_cap_word(name_vec[x])) %>%
          flatten_chr %>%
          paste0(collapse = '')

        new_name <-
          first_word %>%
          paste0(other_words)
      }

    }
    return(new_name)
  }


# summary_data ------------------------------------------------------------

get_cme_floating_rate_summary_df <-
  function() {
    futures_df <-
      'http://www.cmegroup.com/CmeWS/mvc/Quotes/FrontMonths?productIds=1,305,7540,7542,7544&products_quaterly=0&venue=G&type=VOLUME&pageSize=50&_=1466183910143' %>%
      jsonlite::fromJSON(flatten = T) %>%
      as_data_frame

    futures_df <-
      futures_df %>%
      dplyr::select(-matches(c('priceChart'))) %>%
      mutate(
        urlContract = 'http://www.cmegroup.com' %>% paste0(uri),
        urlOption = 'http://www.cmegroup.com' %>% paste0(optionUri),
        expirationDate = expirationDate %>% ymd
      ) %>%
      dplyr::select(-(c(
        updated, uri, highLowLimits, optionUri, escapedQuoteCode
      ))) %>%
      dplyr::select(
        productId,
        expirationMonth,
        expirationDate,
        productName,
        productCode,
        last:low,
        volume,
        everything()
      )

    futures_df <-
      futures_df %>%
      mutate_each_(
        funs(as.numeric),
        c(
          "volume",
          "change",
          "last",
          "priorSettle",
          "open",
          "close",
          "high",
          "low",
          "highLimit",
          "lowLimit"
        )
      ) %>%
      suppressWarnings() %>%
      left_join(get_setttlement_type_df()) %>%
      suppressMessages()

    expiry_dates <-
      seq_len(futures_df %>% nrow) %>%
      purrr::map(function(x) {
        parse_settlement_date(date = futures_df$expirationDate[x],
                              settlement_id = futures_df$idSettlementType[x])
      }) %>%
      flatten_chr %>%
      ymd()

    names(futures_df)[6:13] <-
      names(futures_df)[6:13] %>%
      paste0('Value')

    new_names <-
      seq_len(names(futures_df) %>% length) %>%
      purrr::map(function(x) {
        fix_names(x = names(futures_df)[x])
      }) %>%
      flatten_chr

    names(futures_df) <-
      new_names

    rates_df <-
      futures_df %>%
      dplyr::select(keyMD, valueLast:limitLow) %>%
      dplyr::select(-c(valueChange))

    names(rates_df)[c(2:7)] <-
      names(rates_df)[c(2:7)] %>% str_replace_all('value', '') %>% str_to_lower %>% str_replace("priorsettle", 'priorSettle')

    numeric_vars <-
      c("last",
        "priorSettle",
        "open",
        "close",
        "high",
        "low",
        "high")

    rates_df <-
      rates_df %>%
      mutate_each_(funs(100 - .), vars = numeric_vars)

    names(rates_df)[2:length(names(rates_df))] <-
      names(rates_df)[2:length(names(rates_df))] %>% paste0('Rate')

    new_names_rates <-
      2:(names(rates_df) %>% length) %>%
      purrr::map(function(x) {
        fix_names(x = names(rates_df)[x])
      }) %>%
      flatten_chr

    names(rates_df)[2:length(names(rates_df))] <-
      new_names_rates

    rates_df <-
      rates_df %>%
      left_join(futures_df) %>%
      dplyr::select(idProduct,
                    monthExpiration,
                    dateExpiration,
                    nameProduct,
                    nameCode = code,
                    everything()) %>%
      mutate(dateExpiration = expiry_dates) %>%
      suppressMessages()
    return(rates_df)
  }


# rate_functions ----------------------------------------------------------


parse_futures_data <-
  function(url = 'http://www.cmegroup.com/trading/interest-rates/stir/eurodollar.html',
           return_wide = T) {
    options(digits = 5)
    page <-
      url %>%
      read_html()

    product_type <-
      page %>%
      html_nodes('#productName') %>%
      html_text %>%
      str_trim

    if (product_type %>% str_detect("Eurodollar")) {
      settlement_id <-
        "2_PRIOR_LAST_WED"
    }


    rate_data <-
      page %>% html_table(fill = T) %>%
      data.frame() %>%
      tbl_df

    rate_data <-
      rate_data %>%
      slice(-1) %>%
      dplyr::select(1:11) %>%
      dplyr::select(-c(Options, Charts))

    names(rate_data) <-
      c(
        'monthYearExpiry',
        'valueCurrent',
        'valueChange',
        'valuePrior',
        'valueOpen',
        'valueHigh',
        'valueLow',
        'valueVolume',
        'dateTimeData'
      )

    rate_data <-
      rate_data %>%
      mutate_each_(funs(parse_number),
                   vars =
                     rate_data %>% dplyr::select(matches("value")) %>% names) %>%
      dplyr::select(-dateTimeData) %>%
      mutate(dateTimeData = Sys.time()) %>%
      suppressWarnings()

    rate_data <-
      rate_data %>%
      separate(
        monthYearExpiry,
        into = c('month', 'year'),
        sep = '\\ ',
        remove = F
      ) %>%
      unite(periodExpiry, month, year, sep = ' 1 ') %>%
      mutate(periodExpiry = periodExpiry %>% mdy)

    rate_data <-
      rate_data %>%
      mutate(
        indexFuture = ifelse(valueCurrent %>% is.na, valuePrior, valueCurrent),
        rateFuture = 100 - indexFuture,
        rateFuturePrior = 100 - valuePrior
      ) %>%
      dplyr::select(monthYearExpiry, rateFuture, rateFuturePrior, everything())

    rate_data <-
      rate_data %>%
      mutate(idSettlementType = settlement_id)

    expiration_dates <-
      seq_len(rate_data %>% nrow) %>%
      purrr::map(function(x) {
        parse_settlement_date(date = rate_data$periodExpiry[x],
                              settlement_id = rate_data$idSettlementType[x])
      }) %>%
      flatten_chr %>%
      ymd


    rate_data <-
      rate_data %>%
      mutate(dateExpiry = expiration_dates) %>%
      dplyr::select(monthYearExpiry, dateExpiry, everything()) %>%
      dplyr::select(-c(periodExpiry))

    if (return_wide == F) {
      rate_data <-
        rate_data %>%
        gather(item,
               value,
               -c(monthYearExpiry, dateExpiry, dateTimeData),
               na.rm = T)
    }

    return(rate_data)
  }



#' Get current LIBOR data
#'
#' @param return_wide
#'
#' @return
#' @export
#' @importFrom magrittr %>%
#' @import dplyr tidyr xml2 rvest httr purrr lubridate formattable

get_data_libor_current <-
  function(return_wide = T) {
    url <-
      'http://www.wsj.com/mdc/public/page/2_3020-libor.html'

    page <-
      url %>%
      read_html

    valueCurrent <-
      page %>%
      html_nodes('.text+ .num') %>%
      html_text %>%
      readr::parse_number()

    valueLastWeek <-
      page %>%
      html_nodes('.num:nth-child(3)') %>%
      html_text %>%
      readr::parse_number()

    value52WeekHigh <-
      page %>%
      html_nodes('.num:nth-child(4)') %>%
      html_text %>%
      readr::parse_number()

    value52WeekLow <-
      page %>%
      html_nodes('.num:nth-child(5)') %>%
      html_text %>%
      readr::parse_number()

    libor_df <-
      data_frame(
        currencyRate = c(rep('USD', 7), rep('Euro', 7), rep('Pound', 7), rep('Yen', 7)),
        durationYield  =
          c(1, 7, 30, 60, 90, 180 , 360) %>% rep(4),
        valueCurrent,
        valueLastWeek,
        value52WeekHigh,
        value52WeekLow
      )

    libor_df <-
      libor_df %>%
      unite(nameLibor,
            currencyRate,
            durationYield,
            sep = '.',
            remove = F)

    if (return_wide == F) {
      libor_df <-
        libor_df %>%
        gather(period, value, -c(nameLibor, currencyRate, durationYield))
    }

    return(libor_df)
  }


parse_futures_data <-
  function(url = 'http://www.cmegroup.com/trading/interest-rates/stir/eurodollar.html',
           return_wide = T) {
    options(digits = 5)
    page <-
      url %>%
      read_html()

    product_type <-
      page %>%
      html_nodes('#productName') %>%
      html_text %>%
      str_trim

    if (product_type %>% str_detect("Eurodollar")) {
      settlement_id <-
        "2_PRIOR_LAST_WED"
    }


    rate_data <-
      page %>% html_table(fill = T) %>%
      data.frame() %>%
      tbl_df

    rate_data <-
      rate_data %>%
      slice(-1) %>%
      dplyr::select(1:11) %>%
      dplyr::select(-matches('Options|Charts|NA.'))

    names(rate_data) <-
      c(
        'monthYearExpiry',
        'valueCurrent',
        'valueChange',
        'valuePrior',
        'valueOpen',
        'valueHigh',
        'valueLow',
        'valueVolume',
        'dateTimeData'
      )

    rate_data <-
      rate_data %>%
      mutate_each_(funs(parse_number),
                   vars =
                     rate_data %>% dplyr::select(matches("value")) %>% names) %>%
      dplyr::select(-dateTimeData) %>%
      mutate(dateTimeData = Sys.time()) %>%
      suppressWarnings()

    rate_data <-
      rate_data %>%
      separate(
        monthYearExpiry,
        into = c('month', 'year'),
        sep = '\\ ',
        remove = F
      ) %>%
      unite(periodExpiry, month, year, sep = ' 1 ') %>%
      mutate(periodExpiry = periodExpiry %>% mdy)

    rate_data <-
      rate_data %>%
      mutate(
        indexFuture = ifelse(valueCurrent %>% is.na, valuePrior, valueCurrent),
        rateFuture = 100 - indexFuture,
        rateFuturePrior = 100 - valuePrior
      ) %>%
      dplyr::select(monthYearExpiry, rateFuture, rateFuturePrior, everything())

    rate_data <-
      rate_data %>%
      mutate(idSettlementType = settlement_id)

    expiration_dates <-
      seq_len(rate_data %>% nrow) %>%
      purrr::map(function(x) {
        parse_settlement_date(date = rate_data$periodExpiry[x],
                              settlement_id = rate_data$idSettlementType[x])
      }) %>%
      flatten_chr %>%
      ymd


    rate_data <-
      rate_data %>%
      mutate(dateExpiry = expiration_dates) %>%
      dplyr::select(monthYearExpiry, dateExpiry, everything()) %>%
      dplyr::select(-c(periodExpiry))

    if (return_wide == F) {
      rate_data <-
        rate_data %>%
        gather(item,
               value,
               -c(monthYearExpiry, dateExpiry, dateTimeData),
               na.rm = T)
    }

    return(rate_data)
  }

get_data_index_future <-
  function(futures_index_name = 'Eurodollars',
           return_wide = T) {
    options(scipen = 99999)
    futures_name_df <-
      data_frame(
        nameIndex = c('30 Day Fed Funds', 'Eurodollars', '1 Month Libor', 'Euribor'),
        slugPage = c(
          '30-day-federal-fund',
          'eurodollar',
          '1-month-libor',
          'euribor'
        ),
        urlPage = paste0(
          'http://www.cmegroup.com/trading/interest-rates/stir/',
          slugPage,
          '.html'
        )
      )
    if (!futures_index_name %in% futures_name_df$nameIndex) {
      stop("Index futures options are limited to:\n" %>% paste0(paste0(
        futures_name_df$nameIndex, collapse = '\n'
      )))
    }
    parse_futures_data_safe <-
      possibly(parse_futures_data, NULL)
    selection_df <-
      futures_name_df %>%
      dplyr::filter(nameIndex == futures_index_name)

    future_df <-
      selection_df$urlPage %>%
      map_df(function(x) {
        parse_futures_data_safe(url = x, return_wide = return_wide)
      }) %>%
      mutate(nameIndex = futures_index_name) %>%
      dplyr::select(nameIndex, everything())

    if (!return_wide) {
      future_df <-
        future_df %>%
        dplyr::select(-matches("idSettlementType")) %>%
        gather(item,
               data,
               -c(nameIndex, monthYearExpiry, dateExpiry))
    }

    return(future_df)


  }

#' Get futures data for specified index
#'
#' @param future_indicies
#' @param return_wide
#'
#' @return
#' @export
#' @import rvest purrr readr dplyr RcppBDT
#' @importFrom RcppBDT getNthDayOfWeek
#' @importFrom tis lastBusinessDayOfMonth
#' @importFrom RQuantLib adjust
#' @examples
get_data_futures_indicies <-
  function(future_indicies = c("30 Day Fed Funds", "Eurodollars", "1 Month Libor", "Euribor"),
           return_wide = T) {
    get_data_index_future_safe <-
      possibly(get_data_index_future, NULL)

    futures_df <-
      future_indicies %>%
      map_df(function(x) {
        get_data_index_future_safe(futures_index_name = x,
                              return_wide = return_wide)
      })

    return(futures_df)
  }

get_fred_index_symbol_df <-
  function() {
    symbolsFred <-
      c(
        'DGS30',
        'DGS10',
        'DGS5',
        'DGS2',
        'DAAA',
        'DBAA',
        'DFF',
        'DSWP10',
        'DSWP5',
        'DSWP2',
        'DPRIME',
        'DFII10',
        'DED1',
        'DED3',
        'DED6'
      )

    symbolsYahoo <-
      c(
        '^TYX',
        '^TNX',
        '^FVX',
        'DAAA',
        'DBAA',
        'DFF',
        'DSWP10',
        'DSWP5',
        'DSWP2',
        'DPRIME',
        'DFII10',
        'DED1',
        'DED3',
        'DED6'
      )
    index <-
      c(
        '30 Year Treasury',
        "10 Year Treasury",
        "5 Year Treasury",
        "AAA Bond Yield",
        'BAA Bond Yield',
        'Fed Funds',
        '10 Year Swap',
        '5 Year Swap',
        '2 Year Swap',
        'Prime',
        '10 Year Treasury Inflated',
        '1 Month LIBOR',
        '3 Month LIBOR',
        '6 Month LIBOR'
      )
    symbol_df <-
      data_frame(symbol = symbols,
                 index)

    return(symbol_df)
  }

#' Get FRED index time series data
#'
#' @param symbol
#' @param return_wide
#'
#' @return
#' @export
#' @importFrom quantmod getSymbols
#' @examples
get_data_index_symbol_time_series <-
  function(symbol = 'DGS10',
           return_wide = F) {
    time_series_data <-
      getSymbols(
        Symbols = symbol,
        warnings = F,
        src = 'FRED',
        auto.assign = F
      ) %>%
      as.data.frame

    ts_data <-
      data_frame(date = rownames(time_series_data) %>% as.Date %>% ymd,
                 value = time_series_data[, 1]) %>%
      mutate(symbol) %>%
      dplyr::select(symbol, date, value)

    if (return_wide == T) {
      ts_data <-
        ts_data %>%
        spread(symbol, value)
    }

    return(ts_data)
  }

#' Get current specified index value
#'
#' @param symbol
#' @param return_wide
#'
#' @return
#' @export
#' @importFrom quantmod getQuote
#' @examples get_data_index_symbol_current_value("^TNX")
get_data_index_symbol_current_value <-
  function(symbol = "^TNX",
           return_wide = F) {
    time_series_data <-
      getQuote(symbol, auto.assign = F)

    ts_data <-
      data_frame(symbol,
                 dateTimeData = time_series_data$`Trade Time` %>% ymd_hms(),
                 value = time_series_data$Last)

    if (return_wide == T) {
      ts_data <-
        ts_data %>%
        spread(symbol, value)
    }
    return(ts_data)
  }


get_data_monthly_periods <-
  function(start_date = "2016-06-01",
           term_years = 25,
           term_months = 0){

    periods <-
      term_years * 12 + term_months

    periods <-
      0:periods

    start_date <-
      start_date %>% lubridate::ymd %>% as.Date()

    get_end_of_period <-
      function(period = 0) {
        if (period == 0) {
          period_date <-
            start_date %m+% months(period) %>%
            as.character() %>%
            as.Date()
        }

        if (period == 1) {
          period_date <-
            start_date %m+% months(0) %>%
            timeDate::timeLastDayInMonth %>%
            as.character() %>%
            as.Date()
        }

        if (period > 1) {
          period_date <-
            start_date %m+% months(period - 1) %>%
            timeDate::timeLastDayInMonth %>%
            as.character() %>%
            as.Date()
        }
        period_df <-
          data_frame(idPeriod = period, datePeriod = period_date)
        return(period_df)

      }

    all_periods <-
      periods %>%
      purrr::map(function(x) {
        get_end_of_period(period = x)
      }) %>%
      compact %>%
      bind_rows()

    all_periods <-
      all_periods %>%
      mutate(yearPeriod = ifelse(idPeriod == 0, 0, (idPeriod %/% 12) + 1)) %>%
      dplyr::select(idPeriod, yearPeriod, everything())

    return(all_periods)
  }

#' Get's Loan Payment data
#'
#' @param loan_start_date
#' @param cash_flow_data
#' @param override_monthly_interest
#' @param amount_initial_draw
#' @param is_interest_only
#' @param interest_only_periods
#' @param interest_rate
#' @param is_actual_360
#' @param term_years
#' @param term_months
#' @param pct_loan_fee
#' @param balloon_year
#' @param interest_reserve_period
#' @param balloon_month
#' @param return_annual_summary
#'
#' @return
#' @export
#'
#' @examples
calculate_loan_payment <-
  function(loan_start_date = "2016-06-01",
           amount_initial_draw = 3000,
           is_interest_only = F,
           interest_only_periods = 24,
           interest_rate = 10,
           is_actual_360 = T,
           amortization_years = 10,
           amortization_months = 0,
           term_years = 10,
           term_months = 0,
           pct_loan_fee = 0,
           balloon_year = 10,
           override_monthly_interest = F,
           interest_reserve_period = 0,
           balloon_month = 0,
           return_annual_summary = F) {
    options(scipen = 99999)
    options(digits = 10)
    if (interest_rate > 0) {
      interest_rate <-
        interest_rate / 100
    }


    if (is_actual_360 == T) {
      daily_interest <-
        interest_rate / 360
      net_rate <-
        interest_rate / 360 * 365
    } else {
      daily_interest <-
        interest_rate / 365
      net_rate <-
        interest_rate
    }

    amortization_periods <-
      (amortization_years * 12) + amortization_months

    loan_periods <-
      (balloon_year * 12) + balloon_month

    loan_period_df <-
      get_monthly_period_df(start_date = loan_start_date,
                            term_years = term_years,
                            term_months = term_months)

    loan_period_df <-
      loan_period_df %>%
      mutate(
        isIO = ifelse(is_interest_only == T &
                        idPeriod <= interest_only_periods, T, F),
        isActiveLoan = ifelse(idPeriod <= loan_periods, T, F),
        amountInitialDraw = ifelse(idPeriod == 0, amount_initial_draw, 0),
        amountLoanFee = amountInitialDraw * pct_loan_fee
      )

    loan_period_df <-
      loan_period_df %>%
      dplyr::filter(isActiveLoan == T)

    periods <-
      loan_period_df$idPeriod
    all_payment_data <-
      data_frame()
    for (period in periods) {
      period_index <-
        period + 1

      datePeriod <-
        loan_period_df$datePeriod[period_index]

      drawInitial <-
        loan_period_df$amountInitialDraw[period_index]

      drawAdditional <-
        loan_period_df$amountAdditionalDraw[period_index]

      is_interest_only <-
        loan_period_df$isIO[period_index]

      periodFee <-
        loan_period_df$amountLoanFee[period_index]

      if (period == 0) {
        month_df <-
          data_frame(
            idPeriod = period,
            dateStartPeriod = datePeriod,
            dateEndPeriod = datePeriod,
            isIO = is_interest_only,
            balanceInitial = 0 %>% currency(digits = 2),
            amountInitialDraw = drawInitial %>% currency(digits = 2),
            amountAdditionalDraw = drawAdditional %>% currency(digits = 2),
            paymentInterest = 0 %>% currency(digits = 2),
            paymentPrincipal = 0 %>% currency(digits = 2),
            amountRepayment = 0 %>% currency(digits = 2),
            amountLoanFee = periodFee %>% currency(digits = 2)
          ) %>%
          mutate(balanceEnd = amountInitialDraw + amountAdditionalDraw)
      } else {
        initial_balance <-
          all_payment_data$balanceEnd[period_index - 1]

        total_balance <-
          initial_balance + drawInitial + drawAdditional

        balance_basis <-
          (all_payment_data$amountInitialDraw %>% sum()) +
          all_payment_data$amountAdditionalDraw %>% sum()

        start_month <-
          timeDate::timeFirstDayInMonth(datePeriod) %>% as.Date

        days <-
          (datePeriod  - start_month) + 1

        month_days <-
          as.numeric(days, units = 'days')

        if (override_monthly_interest == T) {
          monthly_interest <-
            net_rate / 12
        } else {
          monthly_interest <-
            daily_interest * month_days
        }

        paymentInterest <-
          monthly_interest * (total_balance)

        if (interest_reserve_period > 0 &
            period <= interest_reserve_period) {
          paymentInterest <-
            0
        }

        if (is_interest_only == T) {
          paymentTotal <-
            paymentInterest
        } else {
          paymentTotal <-
            pmt(
              r = monthly_interest,
              pv = balance_basis,
              fv = 0,
              n = amortization_periods
            )
        }

        paymentPrincipal <-
          abs(paymentTotal) - paymentInterest

        if (period == loan_periods) {
          amountRepayment <-
            initial_balance  + drawInitial + drawAdditional - paymentPrincipal
        } else {
          amountRepayment <-
            0
        }

        month_df <-
          data_frame(
            idPeriod = period,
            dateStartPeriod = start_month,
            dateEndPeriod = datePeriod,
            isIO = is_interest_only,
            balanceInitial = initial_balance %>% currency(digits = 2),
            amountInitialDraw = drawInitial %>% currency(digits = 2),
            amountAdditionalDraw = drawAdditional %>% currency(digits = 2),
            paymentInterest = -paymentInterest %>% currency(digits = 2),
            paymentPrincipal = -paymentPrincipal %>% currency(digits = 2),
            amountRepayment = -amountRepayment %>% currency(digits = 2),
            amountLoanFee = periodFee %>% currency(digits = 2)
          ) %>%
          mutate(
            balanceEnd =
              balanceInitial + amountInitialDraw + amountAdditionalDraw +
              paymentPrincipal + amountRepayment
          )

      }
      all_payment_data <-
        month_df %>%
        bind_rows(all_payment_data) %>%
        arrange((idPeriod))

    }

    all_payment_data <-
      all_payment_data %>%
      mutate(
        balanceInitial = balanceInitial %>% currency(digits = 2),
        amountInitialDraw = amountInitialDraw %>% currency(digits = 2),
        amountAdditionalDraw = amountAdditionalDraw %>% currency(digits = 2),
        paymentInterest = paymentInterest %>% currency(digits = 2),
        paymentPrincipal = paymentPrincipal %>% currency(digits = 2),
        amountRepayment = amountRepayment %>% currency(digits = 2),
        amountLoanFee = amountLoanFee %>% currency(digits = 2),
        balanceEnd = balanceEnd %>% currency(digits = 2)
      ) %>%
      mutate(yearPeriod = ifelse(idPeriod > 0,
                                 ((idPeriod - 1) %/% 12) + 1,
                                 0)) %>%
      dplyr::select(yearPeriod, everything())

    if (return_annual_summary == T) {
      all_payment_data <-
        all_payment_data %>%
        group_by(yearPeriod) %>%
        summarise(
          dateStartPeriod = min(dateStartPeriod),
          dateEndPeriod = max(dateEndPeriod),
          amountInitialDraw = sum(amountInitialDraw),
          amountAdditionalDraw = sum(amountAdditionalDraw),
          paymentInterest = sum(paymentInterest),
          paymentPrincipal = sum(paymentPrincipal),
          amountRepayment = sum(amountRepayment),
          amountLoanFee = sum(amountLoanFee),
          cfLoan = (
            amountInitialDraw + amountAdditionalDraw + paymentInterest + paymentPrincipal + amountRepayment + amountLoanFee
          ) %>% currency(digits = 2)
        ) %>%
        mutate(
          balanceEnd = cumsum(amountInitialDraw) + cumsum(paymentPrincipal) + cumsum(amountRepayment)
        ) %>%
        ungroup
    }

    return(all_payment_data)
  }
