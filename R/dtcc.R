# utilities ---------------------------------------------------------------

get_dtcc_name_df <-
  function() {
    dtcc_name_df <-
      data_frame(nameDTCC = c("DISSEMINATION_ID", "ORIGINAL_DISSEMINATION_ID", "ACTION",
                              "EXECUTION_TIMESTAMP", "CLEARED", "INDICATION_OF_COLLATERALIZATION",
                              "INDICATION_OF_END_USER_EXCEPTION", "INDICATION_OF_OTHER_PRICE_AFFECTING_TERM",
                              "BLOCK_TRADES_AND_LARGE_NOTIONAL_OFF-FACILITY_SWAPS", "EXECUTION_VENUE",
                              "EFFECTIVE_DATE", "END_DATE", "DAY_COUNT_CONVENTION", "SETTLEMENT_CURRENCY",
                              "ASSET_CLASS", "SUB-ASSET_CLASS_FOR_OTHER_COMMODITY", "TAXONOMY",
                              "PRICE_FORMING_CONTINUATION_DATA", "UNDERLYING_ASSET_1", "UNDERLYING_ASSET_2",
                              "PRICE_NOTATION_TYPE", "PRICE_NOTATION", "ADDITIONAL_PRICE_NOTATION_TYPE",
                              "ADDITIONAL_PRICE_NOTATION", "NOTIONAL_CURRENCY_1", "NOTIONAL_CURRENCY_2",
                              "ROUNDED_NOTIONAL_AMOUNT_1", "ROUNDED_NOTIONAL_AMOUNT_2", "PAYMENT_FREQUENCY_1",
                              "RESET_FREQUENCY_1", "OPTION_STRIKE_PRICE", "OPTION_TYPE", "OPTION_FAMILY",
                              "OPTION_CURRENCY", "OPTION_PREMIUM", "OPTION_LOCK_PERIOD", "OPTION_EXPIRATION_DATE",
                              "PRICE_NOTATION2_TYPE", "PRICE_NOTATION2", "PRICE_NOTATION3_TYPE",
                              "PRICE_NOTATION3", "urlData", "EMBEDED_OPTION", "PAYMENT_FREQUENCY_2",
                              "RESET_FREQUENCY_2"),
                 nameActual = c("idDissemination", "idDisseminationOriginal", "typeAction",
                                "dateTimeExecution", "isCleared", "idTypeCollateralization",
                                "hasEndUserAccepted", "hasOtherPriceAffectingTerm",
                                "hasBlockTradeLargeNotionalOffFacilitySwap", "typeExecutionVenue",
                                "dateEffective", "dateEnd", "idDayCount", "codeCurrencySettlement",
                                "idAssetClass", "idSubAssetClass", "descriptionTaxonomy",
                                "typePriceFormingContinuation", "nameUnderylingAsset1", "nameUnderylingAsset2",
                                "typePriceNotation", "priceNotation", "typePriceNotationAdditional",
                                "detailsPriceNotation", "codeCurrencyNotional1", "codeCurrencyNotional2",
                                "amountNotionalRounded1", "amountNotionalRounded2", "idPaymentFrequency1",
                                "idResetFrequency1", "priceOptionStrike", "typeOption", "familyOption",
                                "codeCurrencyOption", "amountOptionPremium", "dateOptionLockPeriod", "dateOptionExpiration",
                                "typePriceNotation2", "priceNotation2", "typePriceNotation3",
                                "priceNotation3", "urlData", "descriptionEmbeddedOption", "idPaymentFrequency2",
                                "idResetFrequency2"))
    return(dtcc_name_df)
  }

resolve_dtcc_name_df <-
  function(data) {
    options(scipen = 9999999)
    name_df <-
      get_dtcc_name_df() %>%
      mutate(idRow = 1:n())

    rf_names <-
      data %>% names()

    has_missing_names <-
      rf_names[!rf_names %in% name_df$nameDTCC] %>% length() > 0

    if (has_missing_names) {
      df_has <-
        data %>%
        select(one_of(rf_names[rf_names %in% name_df$nameDTCC]))

      has_names <-
        names(df_has) %>%
        map_chr(function(x){
          name_df %>%
            filter(nameDTCC == x) %>%
            filter(idRow == min(idRow)) %>%
            .$nameActual
        })

      df_has <-
        df_has %>%
        purrr::set_names(has_names)

      data <-
        df_has %>%
        bind_cols(data %>%
                    select(one_of(rf_names[!rf_names %in% name_df$nameDTCC])))
      return(data)
    }

    actual_names <-
      names(data) %>%
      map_chr(function(x){
        name_df %>%
          filter(nameDTCC == x) %>%
          filter(idRow == min(idRow)) %>%
          .$nameActual
      })

    data <-
      data %>%
      purrr::set_names(actual_names)

    has_notional <-
      data %>% select(matches("amountNotional")) %>% names() %>% length() > 0

    if (has_notional) {
      data <-
        data %>%
        mutate(isNotionalEstimate = amountNotionalRounded1 %>% str_detect('\\+'),
               amountNotionalRounded1 = amountNotionalRounded1 %>% readr::parse_number())
      if ('amountNotionalRounded2' %in% names(data)) {
        data <-
          data %>%
          mutate(isNotionalEstimate2 = amountNotionalRounded2 %>% str_detect('\\+'),
                 amountNotionalRounded2 = amountNotionalRounded2 %>% readr::parse_number())
      }
    }

    data <-
      data %>%
      mutate_at(data %>% select(matches("isCleared")) %>% names(),
                funs(ifelse(. == "C", TRUE, FALSE))) %>%
      mutate_at(data %>% select(matches("^has")) %>% names(),
                funs(ifelse(. == "Y", TRUE, FALSE)))

    data <-
      data %>%
      mutate_at(data %>% select(matches("^name|^description|^idDay|^type")) %>% names(),
                funs(. %>% str_to_upper())) %>%
      mutate_at(data %>% select(matches("^price|amountOptionPremium")) %>% names(),
                funs(. %>% readr::parse_number())) %>%
      mutate_at(data %>% select(matches("^dateOptionLockPeriod|dateOptionExpiration|^date")) %>% select(-matches("dateTime")) %>% names(),
                funs(. %>% lubridate::ymd())) %>%
      mutate_at(data %>% select(matches("^dateTime")) %>% names(),
                funs(. %>% lubridate::ymd_hms())) %>%
      mutate_at(data %>% select(matches("^details|idDisseminationOriginal")) %>% names(),
                funs(. %>% as.character()))
    return(data)

  }

# dtcc --------------------------------------------------------------------

# https://rtdata.dtcc.com/gtr/tracker.do

# https://kgc0418-tdw-data-0.s3.amazonaws.com/prices/COMMODITIES_SWAPS_PRICES.HTML?_=1483653682035

# https://rtdata.dtcc.com/gtr/dashboard.do

parse_most_recent_dtcc_url <-
  function(url = "https://kgc0418-tdw-data-0.s3.amazonaws.com/gtr/static/gtr/html/tracker.html") {

}

generate_dtcc_dump_urls <-
  function(date = "2016-01-07",
           products = NULL) {

    if (products %>% is_null()) {
      products <-
        c('COMMODITIES','credits', 'equities', 'forex', 'rates') %>%
        str_to_upper()
    }

    if (!products %>% is_null()) {
      actual_products <-
        c('COMMODITIES','credits', 'equities', 'forex', 'rates') %>%
        str_to_upper()

      wrong <-
        !products %>% str_to_upper() %in% actual_products %>% sum() == length(products)

      if (wrong) {
        stop(list("Financial Products can only be\n", paste0(actual_products, collapse = '\n')) %>% purrr::invoke(paste0,.))
      }
      products <-
        products %>%
        str_to_upper()
    }

    date_actual <-
      date %>% lubridate::ymd()


    date <-
      date_actual %>%
      str_replace_all("\\-", '\\_')

    urls <-
      list("https://kgc0418-tdw-data-0.s3.amazonaws.com/slices/CUMULATIVE_", products,"_", date,".zip") %>%
      purrr::invoke(paste0,.)

    url_df <-
      data_frame(dateData = date_actual,
                 urlData = urls)
      return(url_df)
  }

download_dtcc_url <-
  function(url = "https://kgc0418-tdw-data-0.s3.amazonaws.com/slices/CUMULATIVE_CREDITS_2017_01_06.zip",
           return_message = TRUE) {
    tmp <-
      tempfile()

    date_data <-
      url %>%
      str_replace_all("https://kgc0418-tdw-data-0.s3.amazonaws.com/slices/|.zip|CUMULATIVE_|COMMODITIES|CREDITS|EQUITIES|FOREX|RATES|\\_", '') %>%
      lubridate::ymd()

    type_item <-
      url %>%
      str_replace_all("https://kgc0418-tdw-data-0.s3.amazonaws.com/slices/CUMULATIVE_",'') %>%
      str_split('\\_') %>%
      flatten_chr() %>%
      .[[1]]

    url %>%
      curl::curl_download(url = ., tmp)

    con <-
      unzip(tmp)

    data <-
      con %>%
      readr::read_csv() %>%
      suppressWarnings() %>%
      suppressMessages() %>%
      select(which(colMeans(is.na(.)) < 1)) %>%
      as_data_frame()

    con %>%
      unlink()

    data <-
      data %>%
      resolve_dtcc_name_df() %>%
      mutate(typeFinancialProduct = type_item,
             dateData = date_data) %>%
      select(typeFinancialProduct,
             dateData, everything()) %>%
      select(which(colMeans(is.na(.)) < 1))

    if ('nameUnderylingAsset1' %in% names(data)) {
      data <-
        data %>%
        separate(nameUnderylingAsset1, into = c('descriptionUnderlyingAsset1', 'durationIndex', 'idSeriesUnderlyingAsset1'),
                 sep = '\\:', remove = FALSE) %>%
        mutate(idSeriesUnderlyingAsset1 = ifelse(idSeriesUnderlyingAsset1 %>% is.na(), durationIndex, idSeriesUnderlyingAsset1),
               descriptionUnderlyingAsset1 = ifelse(descriptionUnderlyingAsset1 == durationIndex, NA, descriptionUnderlyingAsset1),
               durationIndex = ifelse(durationIndex == idSeriesUnderlyingAsset1, NA, durationIndex)) %>%
        suppressWarnings()

      has_desc_df <-
        data %>%
        mutate(idRow = 1:n()) %>%
        filter(!descriptionUnderlyingAsset1 %>% is.na()) %>%
        select(idRow, descriptionUnderlyingAsset1) %>% nrow() > 0
      if (has_desc_df) {
      description_df <-
        data %>%
        mutate(idRow = 1:n()) %>%
        filter(!descriptionUnderlyingAsset1 %>% is.na()) %>%
        select(idRow, descriptionUnderlyingAsset1)

      desc_df <-
        1:nrow(description_df) %>%
        map_df(function(x){
          row_number <-
            description_df$idRow[[x]]

          if (description_df$descriptionUnderlyingAsset1[[x]] %>% str_count('\\.') == 0) {
            return(data_frame(idRow = row_number))
          }

          items <-
            description_df$descriptionUnderlyingAsset1[[x]] %>%
            str_split('\\.') %>%
            flatten_chr()

          items <-
            items[!items == 'NA']

          if (items %>% length() == 2) {
            df <-
              data_frame(idRow = row_number, item = c('idSubIndex', 'idSeries'),
                                            values = items) %>%
              spread(item, values) %>%
              mutate(idSeries = idSeries %>% as.numeric())
          }

          if (items %>% length() == 3) {
            df <-
              data_frame(idRow = row_number, item = c('idIndex','idSubIndex', 'idSeries'),
                                            values = items) %>%
              spread(item, values) %>%
              mutate(idSeries = idSeries %>% as.numeric())
          }

          if (items %>% length() == 4) {
            df <-
              data_frame(idRow = row_number, item = c('idIndex','idSubIndex', 'idSeries', 'idSubIndex1'),
                         values = items) %>%
              spread(item, values) %>%
              mutate(idSeries = idSeries %>% as.numeric())
          }
          return(df)
        }) %>%
        distinct()

      data <-
        data %>%
        mutate(idRow = 1:n()) %>%
        left_join(desc_df) %>%
        select(typeFinancialProduct:nameUnderylingAsset1, idIndex, idSubIndex, idSeries, matches("idSubIndex1"), everything()) %>%
        select(-idRow) %>%
        suppressMessages()
      }
    }

    if (return_message) {
      list("Parsed: ", url) %>%
        purrr::invoke(paste0, .) %>% message()
    }
    return(data)
  }

#' Get DTCC cleared trades by type and day
#'
#' @param products type of DTCC cleared financial product \code{NULL, COMMODITIES, CREDITS, EQUITIES, FOREX, RATES}
#' @param start_date date starting \code{"Y-M-D"}
#' @param end_date date ending \code{"Y-M-D"}
#' @param nest_data return a nested data frame \code{TRUE, FALSE}
#' @param return_message return a message \code{TRUE, FALSE}
#' @import curl dplyr purrr readr lubridate stringr tidyr
#' @return
#' @export
#'
#' @examples
#' get_data_dtcc_products_days(products = NULL, start_date = "2017-01-05", end_date = Sys.Date(), nest_data = TRUE, return_message = FALSE)
get_data_dtcc_products_days <-
  function(products = NULL,
           start_date = "2016-12-01",
           end_date = Sys.Date(),
           nest_data = TRUE,
           return_message = TRUE) {
    start_date <-
      start_date %>%
      readr::parse_date()

    end_date <-
      end_date %>%
      readr::parse_date()

    days <-
      seq(start_date, end_date, by = 1)

    df_date <-
      days %>%
      map_df(function(x) {
        generate_dtcc_dump_urls(date = x, products = products)
      })

    download_dtcc_url_safe <-
      purrr::possibly(download_dtcc_url, data_frame())

    all_df <-
      1:nrow(df_date) %>%
      map_df(function(x) {
        download_dtcc_url_safe(url = df_date$urlData[[x]], return_message = TRUE)
      })

    if (return_message) {
      list("Parsed ", all_df %>% nrow() %>% formattable::comma(digits = 0), ' DTCC cleared trades from ',
           all_df$dateData %>% min(na.rm = T), ' to ', all_df$dateData %>% max(na.rm = T)) %>%
        purrr::invoke(paste0,.) %>%
        message()
    }

    if (nest_data) {
      all_df <-
        all_df %>%
        nest(-c(dateData, typeFinancialProduct), .key = dataDTCC)
    }
    return(all_df)
  }

# most_recent -------------------------------------------------------------

# https://kgc0418-tdw-data-0.s3.amazonaws.com/gtr/static/gtr/html/tracker.html
get_data_dtcc_products_most_recent <-
  function() {

  }

get_data_dtcc_products_today <-
  function() {

  }