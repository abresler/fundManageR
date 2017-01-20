# utilities ---------------------------------------------------------------

get_dtcc_name_df <-
  function() {
    dtcc_name_df <-
      data_frame(
        nameDTCC = c(
          "DISSEMINATION_ID",
          "ORIGINAL_DISSEMINATION_ID",
          "ACTION",
          "EXECUTION_TIMESTAMP",
          "CLEARED",
          "INDICATION_OF_COLLATERALIZATION",
          "INDICATION_OF_END_USER_EXCEPTION",
          "INDICATION_OF_OTHER_PRICE_AFFECTING_TERM",
          "BLOCK_TRADES_AND_LARGE_NOTIONAL_OFF-FACILITY_SWAPS",
          "EXECUTION_VENUE",
          "EFFECTIVE_DATE",
          "END_DATE",
          "DAY_COUNT_CONVENTION",
          "SETTLEMENT_CURRENCY",
          "ASSET_CLASS",
          "SUB-ASSET_CLASS_FOR_OTHER_COMMODITY",
          "TAXONOMY",
          "PRICE_FORMING_CONTINUATION_DATA",
          "UNDERLYING_ASSET_1",
          "UNDERLYING_ASSET_2",
          "PRICE_NOTATION_TYPE",
          "PRICE_NOTATION",
          "ADDITIONAL_PRICE_NOTATION_TYPE",
          "ADDITIONAL_PRICE_NOTATION",
          "NOTIONAL_CURRENCY_1",
          "NOTIONAL_CURRENCY_2",
          "ROUNDED_NOTIONAL_AMOUNT_1",
          "ROUNDED_NOTIONAL_AMOUNT_2",
          "PAYMENT_FREQUENCY_1",
          "RESET_FREQUENCY_1",
          "OPTION_STRIKE_PRICE",
          "OPTION_TYPE",
          "OPTION_FAMILY",
          "OPTION_CURRENCY",
          "OPTION_PREMIUM",
          "OPTION_LOCK_PERIOD",
          "OPTION_EXPIRATION_DATE",
          "PRICE_NOTATION2_TYPE",
          "PRICE_NOTATION2",
          "PRICE_NOTATION3_TYPE",
          "PRICE_NOTATION3",
          "urlData",
          "EMBEDED_OPTION",
          "PAYMENT_FREQUENCY_2",
          "RESET_FREQUENCY_2",
          "Action",
          "UPI/Taxonomy",
          "PublicationTimestamp (UTC)",
          "ExecutionTimestamp (UTC)",
          "UnderlyingAsset 1",
          "UnderlyingAsset 2",
          "MaturityDate",
          "RoundedNotionalCurrency/Quantity",
          "NotionalQuantity UOM/Currency",
          "Clear",
          "PriceNotation",
          "PriceNotationType",
          "Bespoke(Y/N)",
          "OptionType",
          "ExerciseDate",
          "OptionLevel",
          "OptionPremium",
          "RoundedNotional(MMs)",
          "Curr",
          "Addl PriceNotationExists(Y/N)",
          "RoundedNotional 2/Units",
          "EffectiveDate",
          "RoundedNotional1(MMs)",
          "Curr1",
          "Curr2",
          "Exotic(Y/N)",
          "EmbeddedOption(Y/N)",
          "OptionFamily",
          "CLEARED_OR_UNCLEARED",
          "COLLATERALIZATION",
          "END_USER_EXCEPTION",
          "BESPOKE_SWAP",
          "BLOCK_TRADE",
          "ASSET_ID"
        ),
        nameActual = c(
          "idDissemination",
          "idDisseminationOriginal",
          "typeAction",
          "dateTimeExecution",
          "isCleared",
          "idTypeCollateralization",
          "hasEndUserAccepted",
          "hasOtherPriceAffectingTerm",
          "hasBlockTradeLargeNotionalOffFacilitySwap",
          "typeExecutionVenue",
          "dateEffective",
          "dateEnd",
          "idDayCount",
          "codeCurrencySettlement",
          "idAssetClass",
          "idSubAssetClass",
          "descriptionTaxonomy",
          "typePriceFormingContinuation",
          "nameUnderylingAsset1",
          "nameUnderylingAsset2",
          "typePriceNotation",
          "priceNotation",
          "typePriceNotationAdditional",
          "detailsPriceNotation",
          "codeCurrencyNotional1",
          "codeCurrencyNotional2",
          "amountNotionalRounded1",
          "amountNotionalRounded2",
          "idPaymentFrequency1",
          "idResetFrequency1",
          "priceOptionStrike",
          "typeOption",
          "familyOption",
          "codeCurrencyOption",
          "amountOptionPremium",
          "dateOptionLockPeriod",
          "dateOptionExpiration",
          "typePriceNotation2",
          "priceNotation2",
          "typePriceNotation3",
          "priceNotation3",
          "urlData",
          "descriptionEmbeddedOption",
          "idPaymentFrequency2",
          "idResetFrequency2",
          "typeAction",
          "descriptionTaxonomy",
          "dateTimePublication",
          "dateTimeExecution",
          "nameUnderylingAsset1",
          "nameUnderylingAsset2",
          "dateMaturity",
          "amountNotionalRounded1",
          "codeNotionalQuantity",
          "isCleared",
          "priceNotation",
          "typePriceNotation",
          "isBespoke",
          "typeOption",
          "dateExercise",
          "amountLevelOption",
          "amountOptionPremium",
          "amountNotionalRounded1",
          "codeCurrency",
          "hasAdditionalPriceNotation",
          "amountRoundedNotionalUnits",
          "dateEffective",
          "amountNotionalRounded2",
          "codeCurrency1",
          "codeCurrency2",
          "isExotic",
          "hasEmbeddedOption",
          "codeOptionFamily",
          "type",
          "idTypeCollateralization",
          "hasEndUserException",
          "isBespokeSwap",
          "isBlockTrade",
          "idAssetType"
        )
      )
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
        map_chr(function(x) {
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
      map_chr(function(x) {
        name_df %>%
          filter(nameDTCC == x) %>%
          filter(idRow == min(idRow)) %>%
          .$nameActual
      })

    data <-
      data %>%
      purrr::set_names(actual_names)

    has_notional <-
      data %>% select(matches("amountNotionalRounded1")) %>% names() %>% length() > 0

    if (has_notional) {
      data <-
        data %>%
        mutate(
          isNotionalEstimate = amountNotionalRounded1 %>% str_detect('\\+'),
          amountNotionalRounded1 = amountNotionalRounded1 %>% readr::parse_number()
        )
      if ('amountNotionalRounded2' %in% names(data)) {
        data <-
          data %>%
          mutate(
            isNotionalEstimate2 = amountNotionalRounded2 %>% str_detect('\\+'),
            amountNotionalRounded2 = amountNotionalRounded2 %>% readr::parse_number()
          )
      }
    }

    data <-
      data %>%
      mutate_at(data %>% select(matches("isCleared")) %>% names(),
                funs(ifelse(. == "C", TRUE, FALSE))) %>%
      mutate_at(data %>% select(matches("^has|^is")) %>% names(),
                funs(ifelse(. == "Y", TRUE, FALSE))) %>%
      mutate_at(data %>% select(matches("nameUnderylingAsset|^code|^type")) %>% names(),
                funs(ifelse(. == '', NA, .)))

    if ('amountLevelOption' %in% names(data)) {
      data <-
        data %>%
        mutate(amountLevelOption = amountLevelOption %>% as.character())
    }

    data <-
      data %>%
      mutate_at(data %>% select(matches("^name|^description|^idDay|^type")) %>% names(),
                funs(. %>% str_to_upper())) %>%
      mutate_at(data %>% select(matches("^price|amountOptionPremium")) %>% names(),
                funs(. %>% readr::parse_number())) %>%
      mutate_at(
        data %>% select(
          matches("^dateOptionLockPeriod|dateOptionExpiration|^date")
        ) %>% select(-matches("dateTime|dateMaturity|dateExercise")) %>% names(),
        funs(. %>% lubridate::ymd())
      ) %>%
      mutate_at(data %>% select(matches("dateMaturity|dateExercise")) %>% names(),
                funs(. %>% lubridate::mdy())) %>%
      mutate_at(data %>% select(matches("^dateTime")) %>% names(),
                funs(. %>% lubridate::ymd_hms())) %>%
      mutate_at(data %>% select(matches("^details|idDisseminationOriginal")) %>% names(),
                funs(. %>% as.character()))

    if ('amountLevelOption' %in% names(data)) {
      data <-
        data %>%
        mutate(amountLevelOption = amountLevelOption %>% as.character())
    }
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
           assets = NULL) {
    if (assets %>% is_null()) {
      assets <-
        c('COMMODITIES', 'credits', 'equities', 'forex', 'rates') %>%
        str_to_upper()
    }

    if (!assets %>% is_null()) {
      actual_assets <-
        c('COMMODITIES', 'credits', 'equities', 'forex', 'rates') %>%
        str_to_upper()

      wrong <-
        !assets %>% str_to_upper() %in% actual_assets %>% sum() == length(assets)

      if (wrong) {
        stop(list(
          "Financial assets can only be\n",
          paste0(actual_assets, collapse = '\n')
        ) %>% purrr::invoke(paste0, .))
      }
      assets <-
        assets %>%
        str_to_upper()
    }

    date_actual <-
      date %>% lubridate::ymd()


    date <-
      date_actual %>%
      str_replace_all("\\-", '\\_')

    urls <-
      list(
        "https://kgc0418-tdw-data-0.s3.amazonaws.com/slices/CUMULATIVE_",
        assets,
        "_",
        date,
        ".zip"
      ) %>%
      purrr::invoke(paste0, .)

    url_df <-
      data_frame(dateData = date_actual,
                 urlData = urls)
    return(url_df)
  }

parse_for_underlying_asset <-
  function(data) {
    if ('nameUnderylingAsset1' %in% names(data)) {
      data <-
        data %>%
        separate(
          nameUnderylingAsset1,
          into = c(
            'descriptionUnderlyingAsset1',
            'durationIndex',
            'idSeriesUnderlyingAsset1'
          ),
          sep = '\\:',
          remove = FALSE
        ) %>%
        mutate(
          idSeriesUnderlyingAsset1 = ifelse(
            idSeriesUnderlyingAsset1 %>% is.na(),
            durationIndex,
            idSeriesUnderlyingAsset1
          ),
          descriptionUnderlyingAsset1 = ifelse(
            descriptionUnderlyingAsset1 == durationIndex,
            NA,
            descriptionUnderlyingAsset1
          ),
          durationIndex = ifelse(
            durationIndex == idSeriesUnderlyingAsset1,
            NA,
            durationIndex
          )
        ) %>%
        suppressWarnings()

      has_desc_df <-
        data %>%
        mutate(idRow = 1:n()) %>%
        filter(!descriptionUnderlyingAsset1 %>% is.na()) %>%
        select(idRow, descriptionUnderlyingAsset1) %>% nrow() > 0
      if (has_desc_df) {
        description_df <-
          data %>%
          filter(!descriptionUnderlyingAsset1 %>% is.na()) %>%
          mutate(idRow = 1:n()) %>%
          select(idRow, descriptionUnderlyingAsset1)

        desc_df <-
          1:nrow(description_df) %>%
          map_df(function(x) {
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
            count_items <-
              items %>% length()
            if (count_items == 2) {
              df <-
                data_frame(
                  idRow = row_number,
                  item = c('idSubIndex', 'idSeries'),
                  values = items
                ) %>%
                spread(item, values) %>%
                mutate(idSeries = idSeries %>% as.numeric())
            }

            if (count_items == 3) {
              df <-
                data_frame(
                  idRow = row_number,
                  item = c('idIndex', 'idSubIndex', 'idSeries'),
                  values = items
                ) %>%
                spread(item, values) %>%
                mutate(idSeries = idSeries %>% as.numeric())
            }

            if (count_items == 4) {
              df <-
                data_frame(
                  idRow = row_number,
                  item = c('idIndex', 'idSubIndex', 'idSeries', 'idSubIndex1'),
                  values = items
                ) %>%
                spread(item, values) %>%
                mutate(idSeries = idSeries %>% as.numeric())
            }

            if (count_items == 5) {
              df <-
                data_frame(
                  idRow = row_number,
                  item = c(
                    'idIndex',
                    'idSubIndex',
                    'idSeries',
                    'idSubIndex1',
                    'idRating'
                  ),
                  values = items
                ) %>%
                spread(item, values) %>%
                mutate(idSeries = idSeries %>% as.numeric())
            }

            if (count_items == 6) {
              df <-
                data_frame(
                  idRow = row_number,
                  item = c(
                    'idIndex',
                    'idSubIndex',
                    'idSeries',
                    'idSeries1',
                    'idRating',
                    'idOther'
                  ),
                  values = items
                ) %>%
                spread(item, values) %>%
                mutate(idSeries = idSeries %>% as.numeric())
            }

            if (count_items == 7) {
              df <-
                data_frame(
                  idRow = row_number,
                  item = c(
                    'idIndex',
                    'idSubIndex',
                    'idSeries',
                    'idSeries1',
                    'idRating',
                    'idOther',
                    'idOther1'
                  ),
                  values = items
                ) %>%
                spread(item, values) %>%
                mutate(idSeries = idSeries %>% as.numeric())
            }

            if (count_items == 8) {
              df <-
                data_frame(
                  idRow = row_number,
                  item = c(
                    'idIndex',
                    'idSubIndex',
                    'idSeries',
                    'idSeries1',
                    'idRating',
                    'idOther',
                    'idOther1',
                    'idOther2'
                  ),
                  values = items
                ) %>%
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
          select(-idRow) %>%
          suppressMessages()
      }
      return(data)
    }
  }

resolve_taxonomy <-
  function(data) {
    has_taxonomy <-
      'descriptionTaxonomy' %in% names(data)
    if (has_taxonomy) {
      df_taxonomy <-
        data %>%
        filter(!descriptionTaxonomy %>% is.na()) %>%
        select(descriptionTaxonomy) %>%
        distinct() %>%
        arrange(descriptionTaxonomy)

      df_taxonomies <-
        1:nrow(df_taxonomy) %>%
        map_df(function(x) {
          tax <-
            df_taxonomy$descriptionTaxonomy[[x]]
          levels <-
            tax %>% str_count('\\:')
          if (levels == 0) {
            return(data_frame(descriptionTaxonomy = tax))
          }
          tax_items <-
            tax %>%
            str_split('\\:') %>%
            flatten_chr()
          asset <-
            tax_items[[1]] %>% str_to_upper()

          if (asset == 'COMMODITY') {
            items <-
              c(
                'typeFinancialProduct',
                'nameProduct',
                'nameSubProduct',
                'typeFuture',
                'methodDelivery'
              )

            df_long <-
              data_frame(value = tax_items, item = items[1:length(tax_items)]) %>%
              mutate(descriptionTaxonomy = tax)

            col_order <-
              c('descriptionTaxonomy', df_long$item)

            df <-
              df_long %>%
              spread(item, value) %>%
              select(one_of(col_order))

          }

          if (asset == "CREDIT") {
            items <-
              c(
                'typeFinancialProduct',
                'typeIndex',
                'nameIndexReference',
                'nameSubIndexReference'
              )

            df_long <-
              data_frame(value = tax_items, item = items[1:length(tax_items)]) %>%
              mutate(descriptionTaxonomy = tax)

            col_order <-
              c('descriptionTaxonomy', df_long$item)

            df <-
              df_long %>%
              spread(item, value) %>%
              select(one_of(col_order))

          }

          if (asset == "EQUITY") {
            items <-
              c(
                'typeFinancialProduct',
                'typeFuture',
                'nameIndexReference',
                'typeIndexReference'
              )

            df_long <-
              data_frame(value = tax_items, item = items[1:length(tax_items)]) %>%
              mutate(descriptionTaxonomy = tax)

            col_order <-
              c('descriptionTaxonomy', df_long$item)

            df <-
              df_long %>%
              spread(item, value) %>%
              select(one_of(col_order))
          }

          if (asset %in% c("FOREIGNEXCHANGE", "INTERESTRATE")) {
            items <-
              c(
                'typeFinancialProduct',
                'typeFuture',
                'nameIndexReference',
                'typeIndexReference'
              )

            df_long <-
              data_frame(value = tax_items, item = items[1:length(tax_items)]) %>%
              mutate(descriptionTaxonomy = tax)

            col_order <-
              c('descriptionTaxonomy', df_long$item)

            df <-
              df_long %>%
              spread(item, value) %>%
              select(one_of(col_order))
          }
          df <-
            df %>%
            mutate_all(as.character)
          return(df)
        })

      data <-
        data %>%
        left_join(df_taxonomies) %>%
        suppressMessages()

      return(data)
    }
  }

download_dtcc_url <-
  function(url = "https://kgc0418-tdw-data-0.s3.amazonaws.com/slices/CUMULATIVE_CREDITS_2017_01_06.zip",
           return_message = TRUE) {
    tmp <-
      tempfile()

    date_data <-
      url %>%
      str_replace_all(
        "https://kgc0418-tdw-data-0.s3.amazonaws.com/slices/|.zip|CUMULATIVE_|COMMODITIES|CREDITS|EQUITIES|FOREX|RATES|\\_",
        ''
      ) %>%
      lubridate::ymd()

    type_item <-
      url %>%
      str_replace_all("https://kgc0418-tdw-data-0.s3.amazonaws.com/slices/CUMULATIVE_",
                      '') %>%
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
      mutate(nameAsset = type_item,
             dateData = date_data) %>%
      select(nameAsset,
             dateData, everything()) %>%
      select(which(colMeans(is.na(.)) < 1))

    data <-
      data %>%
      parse_for_underlying_asset() %>%
      resolve_taxonomy()

    if (return_message) {
      list("Parsed: ", url) %>%
        purrr::invoke(paste0, .) %>% message()
    }
    return(data)
  }

get_data_dtcc_assets_days <-
  function(assets = NULL,
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
        generate_dtcc_dump_urls(date = x, assets = assets)
      })

    download_dtcc_url_safe <-
      purrr::possibly(download_dtcc_url, data_frame())

    all_df <-
      1:nrow(df_date) %>%
      map_df(function(x) {
        download_dtcc_url_safe(url = df_date$urlData[[x]], return_message = TRUE)
      })

    if (return_message) {
      list(
        "Parsed ",
        all_df %>% nrow() %>% formattable::comma(digits = 0),
        ' DTCC cleared trades from ',
        all_df$dateData %>% min(na.rm = T),
        ' to ',
        all_df$dateData %>% max(na.rm = T)
      ) %>%
        purrr::invoke(paste0, .) %>%
        message()
    }

    if (nest_data) {
      all_df <-
        all_df %>%
        nest(-c(dateData, nameAsset), .key = dataDTCC)
    }
    return(all_df)
  }

# most_recent -------------------------------------------------------------

# https://kgc0418-tdw-data-0.s3.amazonaws.com/gtr/static/gtr/html/tracker.html
get_dtcc_recent_schema_df <-
  function() {
    data_frame(
      idCSS = c(
        '#commoditiesSwapsGrid th',
        '#commoditiesOptionsGrid th',
        '#creditSwapsGrid th',
        '#creditOptionsGrid th',
        '#equitiesSwapsGrid th',
        '#forexSwapsGrid th',
        '#forexOptionsGrid th',
        '#rateSwapsGrid th',
        '#rateOptionsGrid th'
      ),
      nameAsset = c(
        'commodities',
        'commodities',
        'credits',
        'credits',
        'equities',
        'forex',
        'forex',
        'rates',
        'rates'
      ),
      urlData = c(
        "https://kgc0418-tdw-data-0.s3.amazonaws.com/prices/COMMODITIES_SWAPS_PRICES.HTML",
        "https://kgc0418-tdw-data-0.s3.amazonaws.com/prices/COMMODITIES_OPTIONS_PRICES.HTML",
        "https://kgc0418-tdw-data-0.s3.amazonaws.com/prices/CREDITS_SWAPS_PRICES.HTML",
        "https://kgc0418-tdw-data-0.s3.amazonaws.com/prices/CREDITS_OPTIONS_PRICES.HTML",
        "https://kgc0418-tdw-data-0.s3.amazonaws.com/prices/EQUITIES_SWAPS_PRICES.HTML",
        "https://kgc0418-tdw-data-0.s3.amazonaws.com/prices/FOREX_SWAPS_PRICES.HTML",
        "https://kgc0418-tdw-data-0.s3.amazonaws.com/prices/FOREX_OPTIONS_PRICES.HTML",
        "https://kgc0418-tdw-data-0.s3.amazonaws.com/prices/RATES_SWAPS_PRICES.HTML",
        "https://kgc0418-tdw-data-0.s3.amazonaws.com/prices/RATES_OPTIONS_PRICES.HTML"
      )
    )
  }

parse_most_recent_url <-
  function(url =  "https://kgc0418-tdw-data-0.s3.amazonaws.com/prices/COMMODITIES_SWAPS_PRICES.HTML",
           return_message = TRUE) {
    css_df <-
      get_dtcc_recent_schema_df()

    page <-
      "https://kgc0418-tdw-data-0.s3.amazonaws.com/gtr/static/gtr/html/tracker.html" %>%
      read_html()
    id_css <-
      css_df %>%
      filter(urlData == url) %>%
      .$idCSS

    names_dtcc <-
      page %>%
      html_nodes(id_css) %>%
      html_text()

    page <-
      url %>%
      read_html()

    df <-
      page %>%
      html_table(fill = F) %>%
      flatten_df() %>%
      purrr::set_names(names_dtcc)

    df <-
      df %>%
      resolve_dtcc_name_df() %>%
      mutate(urlData = url,
             datetimeData = Sys.time()) %>%
      inner_join(css_df %>% select(urlData, nameAsset)) %>%
      mutate(nameAsset = nameAsset %>% str_to_upper()) %>%
      select(datetimeData, nameAsset, everything()) %>%
      suppressMessages()


    if (return_message) {
      list("Parsed: ", url) %>%
        purrr::invoke(paste0, .) %>% message()
    }
    return(df)
  }

#' DTCC most recent trades by product
#'
#' This function returns information about the most recent
#' trades cleared by The Depository Trust & Clearing Corporation [DTCC]
#' for specified product type.
#'
#' @param assets type of DTCC cleared financial product \itemize{
#' \item \code{NULL}: returns all product types (default)
#' \item \code{COMMODITIES}: Commodities
#' \item \code{CREDITS}: Credit Default Swaps
#' \item \code{EQUITIES}: Equities
#' \item \code{FOREX}: Foreign Exchange
#' \item \code{RATES}: Interest Rates
#' }
#' @return nested \code{data_frame} or \code{data_frame} if \code{nest_data = FALSE}
#' @references \href{http://dtcc.com}{The Depository Trust & Clearing Corporation}
#' @param return_message \code{TRUE} return a message after data import
#' @param nest_data \code{TRUE} return nested data frame
#'
#' @return where \code{nest_data} is \code{TRUE} a nested data_frame by asset,
#' where \code{nest_data} is \code{FALSE} a data_frame
#' @export
#' @family DTCC
#' @family real-time data
#' @family transaction data
#' @import rvest httr dplyr stringr tidyr purrr
#' @examples
#' get_data_dtcc_most_recent_trades(assets = NULL, nest_data = FALSE)
#' get_data_dtcc_most_recent_trades(assets = c('credits', 'equities', 'rates))

get_data_dtcc_most_recent_trades <-
  function(assets = NULL,
           nest_data = TRUE,
           return_message = TRUE) {
    assets <-
      assets %>% str_to_upper()
    css_df <-
      get_dtcc_recent_schema_df()

    if (!assets %>% is_null()) {
      assets_options <-
        css_df$nameAsset %>% unique()
      if (assets %>% str_to_lower() %in% assets_options %>% sum() == 0) {
        stop(
          list(
            "Assets can only be:\n",
            assets_options %>% paste0(collapse = '\n')
          ) %>%
            purrr::invoke(paste0, .)
        )
      }
      css_df <-
        css_df %>%
        filter(nameAsset %in% assets)
    }
    parse_most_recent_url_safe <-
      purrr::possibly(parse_most_recent_url, data_frame())
    all_data <-
      css_df$urlData %>%
      map_df(function(x) {
        parse_most_recent_url(url = x, return_message = return_message)
      }) %>%
      select(which(colMeans(is.na(.)) < 1)) %>%
      suppressMessages() %>%
      suppressWarnings()

    all_data <-
      all_data %>%
      parse_for_underlying_asset() %>%
      suppressWarnings() %>%
      select(which(colMeans(is.na(.)) < 1))

    if ('amountLevelOption' %in% names(all_data)) {
      all_data <-
        all_data %>%
        mutate(amountLevelOption = amountLevelOption %>% readr::parse_number())
    }

    if (return_message) {
      list(
        "Parsed ",
        all_data %>% nrow() %>% formattable::comma(digits = 0),
        ' DTCC most recent cleared trades as of ',
        Sys.time()
      ) %>%
        purrr::invoke(paste0, .) %>%
        message()
    }


    if (nest_data) {
      all_data <-
        all_data %>%
        nest(-c(nameAsset, typeAction), .key = 'dataDTCC')
    }
    return(all_data)
  }



# today -------------------------------------------------------------------

get_c_url_data <-
  function(c_url =  "curl 'https://rtdata.dtcc.com/gtr/dailySearch.do?action=dailySearchNextPage&dailySearchCurrentPage=10&dailySearchHasMore=yes&dailySearchMaxDailyNumber=49369457&displayType=c' -H 'DNT: 1' -H 'Accept-Encoding: gzip, deflate, sdch, br' -H 'Accept-Language: en-US,en;q=0.8' -H 'Upgrade-Insecure-Requests: 1' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/56.0.2924.59 Safari/537.36' -H 'Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8' -H 'Cookie: JSESSIONID_TDW01_Cluster=0000w-S0Gm-OK-9X7LvjnOgRFHE:1a9spvpvu' -H 'Connection: keep-alive' --compressed") {
    clean_url <-
      c_url %>%
      curlconverter::straighten() %>%
      suppressMessages()

    res <-
      clean_url %>%
      make_req(add_clip = FALSE)

    dtcc_df <-
      res[[1]]() %>%
      content(as = "parsed") %>%
      as_data_frame() %>%
      suppressWarnings() %>%
      suppressMessages()

    if (dtcc_df %>% nrow() == 0) {
      return(data_frame())
    }

    dtcc_df <-
      dtcc_df %>%
      mutate_at(dtcc_df %>% select(
        matches(
          "PRICE_NOTATION2|PRICE_NOTATION3|OPTION_EXPIRATION_DATE|OPTION_LOCK_PERIOD|OPTION_PREMIUM|ADDITIONAL_PRICE_NOTATION|ROUNDED_NOTIONAL_AMOUNT_1|ROUNDED_NOTIONAL_AMOUNT_2|OPTION_STRIKE_PRICE|ORIGINAL_DISSEMINATION_ID"
        )
      ) %>% names(),
      funs(. %>% as.character()))

    return(dtcc_df)
  }

get_data_today <-
  function(dtcc_url = "https://rtdata.dtcc.com/gtr/dailySearch.do?action=dailySearchNextPage&dailySearchCurrentPage=1&dailySearchHasMore=yes&dailySearchMaxDailyNumber=993694579&displayType=c") {
    dtcc_url <-
      list(
        "curl '",
        dtcc_url,
        "' -H 'DNT: 1' -H 'Accept-Encoding: gzip, deflate, sdch, br' -H 'Accept-Language: en-US,en;q=0.8' -H 'Upgrade-Insecure-Requests: 1' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/56.0.2924.59 Safari/537.36' -H 'Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8' -H 'Cookie: JSESSIONID_TDW01_Cluster=0000w-S0Gm-OK-9X7LvjnOgRFHE:1a9spvpvu' -H 'Connection: keep-alive' --compressed"
      ) %>%
      purrr::invoke(paste0, .)
    get_c_url_data_safe <-
      purrr::possibly(get_c_url_data, data_frame())
    data <-
      dtcc_url %>%
      get_c_url_data_safe()
    return(data)
  }

get_data_dtcc_today <-
  function(assets = NULL,
           nest_data = TRUE,
           return_message = TRUE) {
    date_data <-
      Sys.Date() %>%
      str_split('\\-') %>%
      flatten_chr() %>% {
        list(.[2], .[3], .[1]) %>% purrr::invoke(paste, ., sep = "%2F")
      }
    nameAsset <-
      c('credits', 'commodities', 'equities', 'forex', 'rates')
    types <-
      c("CR", 'CO', 'EQ', 'FX', 'IR')
    urls <-
      list(
        'https://rtdata.dtcc.com/gtr/dailySearch.do?action=dailySearch&disseminationDateLow=',
        date_data,
        '&disseminationDateHigh=',
        date_data,
        '&assetClassification=',
        types,
        '&notionalRangeLow=0&notionalRangeHigh=50000000000000&disseminationHourLow=0&disseminationMinuteLow=0&disseminationHourHigh=23&disseminationMinuteHigh=59&currency=USD&displayType=c'
      ) %>%
      purrr::invoke(paste0, .)

    df_types <-
      data_frame(nameAsset, idAssetType = types,
                 urlData = urls) %>%
      mutate(nameAsset = nameAsset %>% str_to_upper())

    get_data_today_safe <-
      purrr::possibly(get_data_today, data_frame())

    if (!assets %>% is_null() | assets %>% length() > 0) {
      assets <-
        assets %>% str_to_upper()
      assets_options <-
        df_types$nameAsset %>% unique()
      if (assets %>% str_to_upper() %in% assets_options %>% sum() == 0) {
        stop(
          list(
            "Assets can only be:\n",
            assets_options %>% paste0(collapse = '\n')
          ) %>%
            purrr::invoke(paste0, .)
        )
      }
      df_types <-
        df_types %>%
        filter(nameAsset %in% assets)
    }

    urls <-
      df_types$urlData

    all_data <-
      urls %>%
      sort(decreasing = T) %>%
      map_df(function(x) {
        get_data_today(dtcc_url = x) %>%
          mutate(urlData = x)
      }) %>%
      mutate(dateData = Sys.Date()) %>%
      mutate_at(.cols = c('ORIGINAL_DISSEMINATION_ID'),
                funs(. %>% as.numeric()))

    if (all_data %>% nrow() == 0) {
      return(all_data)
    }

    all_data <-
      all_data %>%
      resolve_dtcc_name_df() %>%
      parse_for_underlying_asset() %>%
      resolve_taxonomy() %>%
      mutate(dateData = Sys.Date()) %>%
      left_join(df_types %>% select(-urlData)) %>%
      suppressWarnings() %>%
      mutate(nameAsset = nameAsset %>% str_to_upper()) %>%
      select(idAssetType, nameAsset, everything()) %>%
      suppressMessages()

    all_data <-
      all_data %>%
      mutate_at(
        all_data %>% select(matches("^priceNotation")) %>% names(),
        funs(. %>% as.character() %>% readr::parse_number())
      )
    if ('isCleared' %in% names(all_data)) {
      all_data <-
        all_data %>%
        mutate(isCleared = ifelse(isCleared == "C", TRUE, FALSE))
    }

    if (return_message) {
      list(
        "Parsed ",
        all_data %>% nrow() %>% formattable::comma(digits = 0),
        ' DTCC most recent cleared trades for ',
        Sys.Date()
      ) %>%
        purrr::invoke(paste0, .) %>%
        message()
    }
    if (nest_data) {
      all_data <-
        all_data %>%
        nest(-c(dateData, nameAsset), .key = 'dataDTCC')
    }
    return(all_data)
  }


# all ---------------------------------------------------------------------


#' DTCC trades by date and product
#'
#' This function returns information on derivatives trades cleared by the
#' The Depository Trust & Clearing Corporation [DTCC] for specified dates
#' and product types.
#'
#' @param assets type of DTCC cleared financial product \itemize{
#' \item \code{NULL}: returns all product types (default)
#' \item \code{COMMODITIES}: Commodities
#' \item \code{CREDITS}: Credit Default Swaps
#' \item \code{EQUITIES}: Equities
#' \item \code{FOREX}: Foreign Exchange
#' \item \code{RATES}: Interest Rates
#' }
#' @note Use \code{\link{get_data_dtcc_most_recent_trades}} for most recent trades
#' @references \href{http://dtcc.com}{The Depository Trust & Clearing Corporation}
#' @return where \code{nest_data} is \code{TRUE} a nested data_frame by asset and action
#' where \code{nest_data} is \code{FALSE} a data_frame
#' @param start_date date starting, must be in year-day-format
#' @param end_date date ending, must be in year-month-day format
#' @param include_today include today's trades
#' @return nested \code{data_frame} or \code{data_frame} if \code{nest_data = FALSE}
#' @references \href{http://dtcc.com}{The Depository Trust & Clearing Corporation}
#' @param return_message \code{TRUE} return a message after data import
#' @param nest_data \code{TRUE} return nested data frame
#' @return where \code{nest_data} is \code{TRUE} a nested data_frame by asset,
#' where \code{nest_data} is \code{FALSE} a data_frame
#' @export
#' @family DTCC
#' @family transaction data
#' @import curl dplyr purrr readr lubridate stringr tidyr curlconverter
#' @examples
#' \dontrun{
#' get_data_dtcc_trades()
#' get_data_dtcc_trades(start_date = "2017-01-16")
#' get_data_dtcc_trades(assets = c('credits', 'equities'), include_today = TRUE, start_date = '2017-01-10', end_date = Sys.Date(), nest_data = FALSE)
#' }

get_data_dtcc_trades <-
  function(assets = NULL,
           include_today = TRUE,
           start_date = NULL,
           end_date = NULL,
           nest_data = TRUE,
           return_message = TRUE) {
    all_data <-
      data_frame()

    if (!assets %>% is_null()) {
      assets <-
        assets %>% str_to_upper()
    }

    if (include_today) {
      today <-
        get_data_dtcc_today(assets = assets,
                            nest_data = FALSE,
                            return_message = return_message)
      if (today %>% nrow() > 0) {
        today <-
          today %>%
          resolve_dtcc_name_df() %>%
          select(which(colMeans(is.na(.)) < 1))

        today <-
          today %>%
          mutate_at(today %>% select(matches(
            "dateTime|idDisseminationOriginal|^date"
          )) %>% names(),
          funs(. %>% as.character())) %>%
          mutate_at(today %>% select(matches("^amount|priceOptionStrike")) %>% names(),
                    funs(. %>% as.numeric())) %>%
          suppressWarnings()

        all_data <-
          all_data %>%
          bind_rows(today)
      }
    }

    if (!start_date %>% is_null()) {
      if (end_date %>% is_null()) {
        end_date <-
          Sys.Date()
      }

      data <-
        get_data_dtcc_assets_days(
          assets = assets,
          start_date = start_date,
          end_date = end_date,
          nest_data = FALSE,
          return_message = return_message
        )

      data <-
        data %>%
        resolve_dtcc_name_df() %>%
        mutate_at(data %>% select(
          matches(
            "^dateTime|idDisseminationOriginal|^date|^priceNotation"
          )
        ) %>% names(),
        funs(. %>% as.character()))

      data <-
        data %>%
        mutate_at(
          data %>% select(matches("^priceNotation")) %>% names(),
          funs(. %>% as.character() %>% readr::parse_number())
        )



      if (data %>% nrow() > 0) {
        all_data <-
          all_data %>%
          bind_rows(data)
      }
    }

    all_data <-
      all_data %>%
      mutate_at(all_data %>% select(matches("^dateTime[A-Z]")) %>% names(),
                funs(. %>% lubridate::ymd_hms())) %>%
      mutate_at(
        all_data %>% select(matches("^date[A-Z]")) %>% select(-matches("^dateTime")) %>% names(),
        funs(. %>% lubridate::ymd())
      ) %>%
      mutate_at(all_data %>% select(matches("^idDissemination")) %>% names(),
                funs(. %>% as.character() %>% as.integer())) %>%
      select(-c(urlData, idAssetType)) %>%
      suppressMessages() %>%
      suppressWarnings()

    all_data <-
      all_data %>%
      arrange(desc(idDissemination))

    if (nest_data) {
      all_data <-
        all_data %>%
        nest(-c(dateData, nameAsset), .key = dataDTCC)
    }
    return(all_data)
  }