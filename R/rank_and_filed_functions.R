# dictionaries ------------------------------------------------------------

get_general_name_df <-
  function() {
    general_name_df <-
      data_frame(
        nameRF = c(
          "category",
          "cik",
          "city",
          "filing_count",
          "irs_number",
          "named",
          "phone",
          "state_address",
          "state_incorp",
          "street_1",
          "street_2",
          "type",
          "zip_code",
          "private_industry", "private_issuer", "private_type",
          'fund_type', 'is_owned', 'detail'

        ),
        nameActual = c(
          "typeCategory",
          "idCIK",
          "cityEntity",
          "countFilings",
          "idIRS",
          "nameEntity",
          "phoneEntity",
          "addressEntity",
          "stateIncorporation",
          "streetEntity1",
          "streetEntity2",
          "typeCompany",
          "zipCodeEntity",
          "industryEntity", "objectIssuer", "typeEntity",
          'typeFund', 'idCIKOwnedBy', 'detailRF'

        )
      )

    return(general_name_df)
  }

get_private_name_df <-
  function() {
    data_frame(nameRF =c("amended", "cik", "date", "exemption", "finders_fee", "industry",
                         "minimum", "nonaccredited", "num_invested", "option", "proceeds_used",
                         "sale_date", "sales_fee", "security", "total_offering", "total_remaining",
                         "total_sold", "debt", "equity", "foreign_solicit", 'fund_type', 'hedge_fund', 'other', 'total_clar'),
               nameActual = c("dateAmmended", "idCIK", "dateFiling", "idExemption", "amountFindersFee", "nameIndustry",
                              "amountMinimumInvestment", "countInvestorsNonAccredited", "countInvestors", "isOption", "amountProceedsUsed",
                              "dateSale", "amountSaleFee", "isSecurity", "amountOffered", "amountRemaining",
                              "amountSold", "isDebtSecurity", "isEquity", "isForeignSolicted", 'typeFund', 'isHedgeFund', 'detailOther', 'detailClarification'))
  }

get_form_d_category_df <-
  function(){
    category_df <-
      dplyr::data_frame(
        idIndustry = 1:35,
        nameIndustry = c("AGRICULTURE", "AIRLINES AND AIRPORTS", "BIOTECHNOLOGY", "BUSINESS SERVICES",
                         "COAL MINING", "COMMERCIAL REAL ESTATE", "COMMERCIAL BANKING",
                         "COMPUTERS", "CONSTRUCTION", "ELECTRIC UTILITIES", "ENERGY CONSERVATION",
                         "ENVIORNMENTAL SERVICES", "HEALTH INSURANCE", "HOSPITALS AND PHYSICIANS",
                         "INSURANCE", "INVESTING", "INVESTMENT BANKING", "LODGING AND CONVETION",
                         "MANUFACTURING", "OIL AND GAS", "OTHER", "OTHER BANKING AND FINANCIAL SERVICES",
                         "OTHER ENERGY", "OTHER HEALTH CARE", "OTHER REAL ESTATE", "OTHER TECHNOLOGY",
                         "OTHER TRAVEL", "PHARMACEUTICALS", "POOLED INVESTMENT FUND",
                         "REITS AND FINANCE", "RESIDENTIAL REAL ESTATE", "RESTAURANTS",
                         "RETAIL", "TELECOMMUNICATIONS", "TRAVEL AND TOURISM"),
        codeIndustryParent = c("OTHER", "TRAVEL", "HEALTH", "OTHER", "ENERGY", "REAL", "FINANCE",
                               "TECH", "REAL", "ENERGY", "ENERGY", "ENERGY", "HEALTH", "HEALTH",
                               "FINANCE", "FINANCE", "FINANCE", "TRAVEL", "OTHER", "ENERGY",
                               "OTHER", "FINANCE", "ENERGY", "HEALTH", "REAL", "TECH", "TRAVEL",
                               "HEALTH", "FINANCE", "REAL", "REAL", "OTHER", "OTHER", "TECH",
                               "TRAVEL"),
        nameIndustryParent = c("OTHER", "TRAVEL AND LEISURE", "HEALTHCARE", "OTHER", "ENERGY",
                               "REAL ESTATE", "FINANCIAL", "TECHNOLOGY", "REAL ESTATE", "ENERGY",
                               "ENERGY", "ENERGY", "HEALTHCARE", "HEALTHCARE", "FINANCIAL",
                               "FINANCIAL", "FINANCIAL", "TRAVEL AND LEISURE", "OTHER", "ENERGY",
                               "OTHER", "FINANCIAL", "ENERGY", "HEALTHCARE", "REAL ESTATE",
                               "TECHNOLOGY", "TRAVEL AND LEISURE", "HEALTHCARE", "FINANCIAL",
                               "REAL ESTATE", "REAL ESTATE", "OTHER", "OTHER", "TECHNOLOGY",
                               "TRAVEL AND LEISURE")
      )
    return(category_df)
  }


# utilities ---------------------------------------------------------------
get_cik_url_df <-
  function(cik = 1138621) {
    slugs <-
      c('general', 'filings', 'private', 'fundraising')

    url_json <-
      list('http://rankandfiled.com/data/filer/', cik, '/', slugs) %>%
      purrr::invoke(paste0, .)

    url_df <-
      dplyr::data_frame(
        nameTable = c('General', 'Filings', 'Private', 'Fundraising'),
        urlJSON = url_json
      )
    return(url_df)
  }

resolve_cik_url <-
  function(data) {
    if ('idCIK' %in% names(data)) {
      data <-
        data %>%
        mutate(urlCIKRankandFiled =
                 ifelse(
                   !idCIK %>% is.na(),
                   list('http://rankandfiled.com/#/filers/', idCIK, '/filings') %>% purrr::invoke(paste0, .),
                   NA
                 ))
      return(data)
    }
  }

clean_ticker <-
  function(data) {
    if ('idTicker' %in% names(data)) {
      data <-
        data %>%
        tidyr::separate(idTicker,
                        into = c('remove', 'idTicker'),
                        sep = 'f|c') %>%
        select(-remove) %>%
        mutate(idTicker = ifelse(idTicker == '', NA, idTicker) %>% stringr::str_trim())
      return(data)
    }
  }

clean_cik <-
  function(data) {
    if ('idCIK' %in% names(data)) {
      data %>%
        mutate(idCIK = idCIK %>% as.numeric())
    }
  }

clean_names <-
  function(data, upper_case = TRUE) {
    has_name_count <-
      data %>% names() %>% stringr::str_count('^name') %>% sum() > 0
    if (has_name_count) {
      if (upper_case) {
        data <-
          data %>%
          mutate_at(
            .cols =
              data %>% dplyr::select(matches("^name")) %>% names(),
            funs(. %>% stringr::str_to_upper())
          )
      } else {
        data <-
          data %>%
          mutate_at(
            .cols =
              data %>% dplyr::select(matches("^name")) %>% names(),
            funs(. %>% stringr::str_to_lower())
          )
      }
      return(data)
    }
  }

parse_rank_and_filed_data <-
  function(url = "http://rankandfiled.com/static/export/sic_naics.csv",
           column_names = c('idSIC',
                            'classificationSIC',
                            'idNAICS',
                            'classificationNAICS')) {
    data <-
      url %>%
      readr::read_csv(col_names = F) %>%
      suppressWarnings() %>%
      suppressMessages()

    data <-
      data %>%
      tidyr::separate(col = X1,
                      sep = '\\|',
                      into = column_names) %>%
      suppressWarnings() %>%
      suppressMessages() %>%
      slice(-1) %>%
      distinct()

    data
  }

print_message <-
  function(data, table_name = '13F Owned Companies') {
    list("You got ",
         data %>% nrow() %>% formattable::comma(digits = 0),
         ' ',
         table_name) %>%
      purrr::invoke(paste0, .) %>%
      message()
  }

resolve_name_df <-
  function(data) {
    name_df <-
      get_general_name_df() %>%
      bind_rows(get_private_name_df()) %>%
      distinct() %>%
      mutate(idRow = 1:n())

    rf_names <-
      data %>% names()

    has_missing_names <-
      rf_names[!rf_names %in% name_df$nameRF] %>% length() > 0

    if (has_missing_names) {
      df_has <-
        data %>%
        select(one_of(rf_names[rf_names %in% name_df$nameRF]))

      has_names <-
        names(df_has) %>%
        map_chr(function(x){
          name_df %>%
            filter(nameRF == x) %>%
            filter(idRow == min(idRow)) %>%
            .$nameActual
        })

      df_has <-
        df_has %>%
        purrr::set_names(has_names)

      data <-
        df_has %>%
        bind_cols(data %>%
                    select(one_of(rf_names[!rf_names %in% name_df$nameRF])))
      return(data)
    }

    actual_names <-
      names(data) %>%
      map_chr(function(x){
        name_df %>%
          filter(nameRF == x) %>%
          filter(idRow == min(idRow)) %>%
          .$nameActual
      })

    data <-
      data %>%
      purrr::set_names(actual_names)

    return(data)

  }


# industries --------------------------------------------------------------

#' Get SIC code Classifications
#'
#' @param filter_duplicates
#'
#' @return
#' @export
#' @import dplyr
#' @importFrom readr read_csv
#' @importFrom tidyr separate
#' @examples
get_data_sic_codes <-
  function(filter_duplicates = TRUE) {
    sic <-
      "http://rankandfiled.com/static/export/sic_naics.csv" %>%
      parse_rank_and_filed_data(column_names = c(
        'idSIC',
        'classificationSIC',
        'idNAICS',
        'classificationNAICS'
      ))

    sic <-
      sic %>%
      mutate_at(.cols = c('idSIC', 'idNAICS'),
                funs(. %>% as.numeric())) %>%
      mutate(
        classificationSIC = ifelse(classificationSIC == '', NA, classificationSIC),
        classificationNAICS = ifelse(classificationNAICS == '', NA, classificationNAICS)
      ) %>%
      arrange(idSIC)

    if (filter_duplicates) {
      sic <-
        sic %>%
        mutate(idRow = 1:n()) %>%
        group_by(idSIC, idNAICS) %>%
        filter(idRow == min(idRow)) %>%
        ungroup() %>%
        distinct() %>%
        select(-idRow)
    }
    return(sic)
  }


# file_codes --------------------------------------------------------------

#' Get SEC CODES
#'
#' @return
#' @export
#' @import dplyr purrr
#' @importFrom  tidyr separate
#' @importFrom readr read_csv
#' @examples
get_data_sec_file_codes <-
  function() {
    codes <-
      "http://rankandfiled.com/static/export/file_numbers.csv" %>%
      parse_rank_and_filed_data(column_names = c("idPrefixSEC", "typeFiling", "nameLawSEC")) %>%
      mutate(typeFiling = ifelse(typeFiling == '', NA, typeFiling))

    return(codes)
  }

# countries ---------------------------------------------------------------
get_rank_and_filed_countries <-
  function() {
    countries <-
      "http://rankandfiled.com/static/export/edgar_state_country.csv" %>%
      parse_rank_and_filed_data(column_names = c('codeLocation', 'nameLocation'))
    countries
  }


# cusips ------------------------------------------------------------------
#' Get CUSIP Data
#'
#' @param return_message
#'
#' @return
#' @export
#' @import dplyr purrr
#' @importFrom  tidyr separate
#' @importFrom readr read_csv
#' @examples
get_data_cusips <-
  function(return_message = TRUE) {
    cusips <-
      'http://rankandfiled.com/static/export/cusip_ticker.csv' %>%
      parse_rank_and_filed_data(column_names = c('nameIssuer', 'idTicker', 'idCUSIP', 'idCIK')) %>%
      mutate(
        nameIssuer = nameIssuer %>% stringr::str_to_upper(),
        idCIK = ifelse(idCIK == '', NA, idCIK),
        idCIK = idCIK %>% as.numeric(),
        idTicker = ifelse(idTicker == '', NA, idTicker)
      ) %>%
      suppressWarnings()

    cusips <-
      cusips %>%
      resolve_cik_url()

    if (return_message) {
      list(
        "You acquired data for ",
        cusips %>% nrow() %>% formattable::comma(digits = 0),
        " United States cusiped issuers"
      ) %>%
        purrr::invoke(paste0, .) %>% message()
    }
    return(cusips)
  }


# leis --------------------------------------------------------------------

#' Get US domiciled LEI issuers
#'
#' @param return_message
#'
#' @return
#' @export
#' @import dplyr purrr
#' @importFrom  tidyr separate
#' @importFrom readr read_csv
#' @examples
get_data_leis <-
  function(return_message = TRUE) {
    leis <-
      "http://rankandfiled.com/static/export/cik_lei.csv" %>%
      parse_rank_and_filed_data(column_names = c('idCIK',
                                                 'nameEntity', 'idLEI', 'typeEntity')) %>%
      mutate(nameEntity = nameEntity %>% str_to_upper(),
             idCIK = idCIK %>% as.numeric())

    leis <-
      leis %>%
      resolve_cik_url()

    if (return_message) {
      list(
        "You acquired data for ",
        leis %>% nrow() %>% formattable::comma(digits = 0),
        " United States legal entities"
      ) %>%
        purrr::invoke(paste0, .) %>% message()
    }
    return(leis)
  }

# public_companies --------------------------------------------------------

#' Get ticker information for United States traded public entities
#'
#' @param return_message
#'
#' @return
#' @export
#' @import dplyr purrr
#' @importFrom  tidyr separate
#' @importFrom readr read_csv
#' @importFrom formattable comma
#' @examples get_data_us_tickers(return_message = TRUE)
get_data_us_tickers <-
  function(return_message = TRUE) {
    data <-
      "http://rankandfiled.com/static/export/cik_ticker.csv" %>%
      parse_rank_and_filed_data(
        column_names = c(
          'idCIK',
          'idTicker',
          'nameCompany',
          'idExchange2',
          'idSIC',
          'codeLocationBusiness',
          'codeLocationIncorporation',
          'idIRS'
        )
      )

    data <-
      data %>%
      mutate_at(.cols = c('idCIK', 'idSIC', 'idSIC', 'idIRS'),
                funs(. %>% as.numeric()))

    data <-
      data %>%
      mutate(
        nameCompany = nameCompany %>% str_to_upper(),
        idExchange2 = ifelse(idExchange2 == '', NA, idExchange2),
        codeLocationBusiness = ifelse(codeLocationBusiness == '', NA, codeLocationBusiness),
        codeLocationIncorporation = ifelse(
          codeLocationIncorporation == '',
          NA,
          codeLocationIncorporation
        ),
        idIRS = ifelse(idIRS == '', NA, idIRS)
      ) %>%
      arrange(nameCompany) %>%
      left_join(data_frame(
        idExchange2 = c(
          NA,
          "NASDAQ",
          "OTC",
          "OTCBB",
          "NYSE",
          "NYSE MKT",
          "NYSE ARCA",
          "BATS"
        ),
        idExchange = c(NA, "NASDAQ", "OTC", "OTC", "NYSE", "NYSE", "NYSE",
                       "NYSE"),
        detailExchange = c(NA, NA, NA, "BB", NA, "MKT", "ARCA",
                           "BATS")

      )) %>%
      dplyr::select(-idExchange2) %>%
      dplyr::select(idCIK:nameCompany, idExchange, detailExchange, everything()) %>%
      suppressMessages()

    countries <-
      get_rank_and_filed_countries()


    data <-
      data %>%
      left_join(
        countries %>%
          dplyr::rename(
            codeLocationBusiness = codeLocation,
            nameLocationBusiness = nameLocation
          )
      ) %>%
      left_join(
        countries %>%
          dplyr::rename(
            codeLocationIncorporation = codeLocation,
            nameLocationIncorporation = nameLocation
          )
      ) %>%
      dplyr::select(
        idCIK:codeLocationBusiness,
        nameLocationIncorporation,
        codeLocationIncorporation,
        nameLocationIncorporation,
        everything()
      ) %>%
      suppressWarnings() %>%
      suppressMessages()


    data <-
      data %>%
      left_join(
        get_data_sic_codes(filter_duplicates = TRUE) %>%
          dplyr::select(idSIC, classificationSIC) %>%
          mutate(idRow = 1:n()) %>%
          group_by(idSIC) %>%
          filter(idRow == min(idRow)) %>%
          dplyr::select(-idRow)
      ) %>%
      distinct() %>%
      mutate(
        urlTickerRankandFiled =
          ifelse(
            !idExchange %>% is.na(),
            list('http://rankandfiled.com/#/public/', idTicker, '/filings') %>% purrr::invoke(paste0, .),
            NA
          ),
        urlCIKRankandFiled = list('http://rankandfiled.com/#/filers/', idCIK, '/filings') %>% purrr::invoke(paste0, .)
      ) %>%
      suppressMessages()

    if (return_message) {
      list(
        "You acquired data for ",
        data %>% nrow() %>% formattable::comma(digits = 0),
        " United States publically traded companies"
      ) %>%
        purrr::invoke(paste0, .) %>% message()
    }
    return(data)
  }



# private_offerings -------------------------------------------------------


# securities_offerings ----------------------------------------------------


# foia --------------------------------------------------------------------


# money_markets -----------------------------------------------------------



# insider_trading ---------------------------------------------------------

get_data_insider_trades_most_recent <-
  function()

    # debt_securities ---------------------------------------------------------
#' Get data money market owned securities
#'
#' @param return_message
#'
#' @return
#' @export
#' @import dplyr purrr
#' @importFrom  tidyr separate
#' @importFrom readr read_csv
#' @importFrom stringr str_to_upper
#' @importFrom lubridate ymd
#' @examples
get_data_mmf_owned_debt_securities <-
  function(return_message = TRUE) {
    debt <-
      "http://rankandfiled.com/static/export/mmf_cusips.csv" %>%
      parse_rank_and_filed_data(
        column_names = c(
          'idCIK',
          'nameIssuer',
          'idCUSIP',
          'nameSecurity',
          'categorySecurity',
          'dateMaturity',
          'ratingSecurity'
        )
      ) %>%
      mutate(
        nameIssuer = nameIssuer %>% stringr::str_to_upper(),
        nameSecurity = nameSecurity %>% stringr::str_to_upper(),
        idCIK = idCIK %>% as.numeric(),
        dateMaturity = dateMaturity %>% lubridate::ymd()
      )

    debt <-
      debt %>%
      resolve_cik_url()

    if (return_message) {
      debt %>%
        print_message(table_name = 'Money Market Owned Debt Securities')
    }
    return(debt)
  }


# 13fs --------------------------------------------------------------------
#' Get 13F owned compaines
#'
#' @param return_message
#'
#' @return
#' @import dplyr purrr
#' @importFrom  tidyr separate
#' @importFrom readr read_csv
#' @export
#'
#' @examples
get_data_sec_13F_companies <-
  function(return_message = TRUE) {
    data <-
      "http://rankandfiled.com/static/export/13f_cusips.csv" %>%
      parse_rank_and_filed_data(column_names = c('idCIK', 'nameIssuer', 'idCUSIP', 'classSecurity')) %>%
      mutate(
        nameIssuer = nameIssuer %>% stringr::str_to_upper(),
        idCIK = idCIK %>% as.numeric()
      ) %>%
      suppressMessages() %>%
      suppressWarnings()

    data <-
      data %>%
      mutate(
        isLetterOnly = idCUSIP %>% grepl("^[[:alpha:]]*$", .),
        idTicker = ifelse(isLetterOnly == TRUE,
                          idCUSIP, NA),
        idTicker = idTicker %>% str_replace_all('XXXXX|HEDGE', ''),
        idCUSIP = ifelse(isLetterOnly == TRUE, NA, idCUSIP)
      ) %>%
      select(-isLetterOnly)

    data <-
      data %>%
      resolve_cik_url()

    if (return_message) {
      data %>%
        print_message(table_name = '13F Owned Companies')
    }
    return(data)
  }

# mutual_funds ------------------------------------------------------------

#' Get Mutual Fund Data
#'
#' @param return_message
#' @import dplyr purrr
#' @importFrom  tidyr separate
#' @importFrom readr read_csv
#' @return
#' @export
#'
#' @examples
get_data_mutual_funds <-
  function(return_message = TRUE) {
    funds <-
      'http://rankandfiled.com/static/export/funds.csv' %>%
      parse_rank_and_filed_data(
        column_names = c(
          'idCIK',
          'nameEntitySECRegistration',
          'nameFundFamily',
          'idTicker' ,
          'nameFund',
          'typeFund'
        )
      ) %>%
      mutate(idCIK = idCIK %>% as.numeric()) %>%
      arrange(nameEntitySECRegistration)

    funds <-
      funds %>%
      resolve_cik_url()

    if (return_message) {
      funds %>%
        print_message(table_name = 'SEC Registered Mutual Funds')
    }
    return(funds)
  }



# securities_filings ------------------------------------------------------

#' Get Insider Trades from the Last Week
#'
#' @param return_message
#' @import dplyr purrr
#' @importFrom  tidyr separate
#' @importFrom readr read_csv
#' @return
#' @export
#'
#' @examples

get_data_recent_insider_trades <-
  function(return_message = TRUE) {
    options(scipen = 9999)
    json_data <-
      "http://rankandfiled.com/data/buy_sell" %>%
      fromJSON()
    insider_name_df <-
      data_frame(
        idInsiderTable = 1:5,
        nameRF = json_data$result %>% names(),
        typeInsider = c(rep('Company', 3), rep('Person', 2)),
        typeTransaction = c('Buyback', 'Buy', 'Sell', 'Buy', 'Sell'),
        nameTable = c(
          'Buyback',
          'Company Buy',
          'Company Sell',
          'Insider Buy',
          'Insider Sell'
        )
      )

    all_data <-
      1:(json_data$result %>% length()) %>%
      map_df(function(x) {
        table <-
          json_data$result[[x]]

        if (x %in% 1:3) {
          table_names <-
            c(
              'idCIK',
              'countSales',
              'amountSales',
              'countPurchases',
              'amountPurchases',
              'X3',
              'X4',
              'nameCompany',
              'idTicker'
            )

          df <-
            data_frame(X1 = table) %>%
            tidyr::separate(col = X1,
                            into = table_names,
                            sep = '\\*') %>%
            suppressWarnings()
        }

        if (x == 4) {
          table_names <-
            c(
              'idCIKPerson',
              'idCIK',
              'X1',
              'amountPurchases',
              'namePerson',
              'X2',
              'nameCompany',
              'idTicker'
            )

          df <-
            data_frame(X1 = table) %>%
            tidyr::separate(col = X1,
                            into = table_names,
                            sep = '\\*') %>%
            suppressWarnings()
        }

        if (x == 5) {
          table_names <-
            c(
              'idCIKPerson',
              'idCIK',
              'amountSales',
              'X1',
              'namePerson',
              'X2',
              'nameCompany',
              'idTicker'
            )

          df <-
            data_frame(X1 = table) %>%
            tidyr::separate(col = X1,
                            into = table_names,
                            sep = '\\*') %>%
            suppressWarnings()
        }

        df <-
          df %>%
          select(-matches("^X[1-9]")) %>%
          separate(idTicker,
                   into = c('replace', 'idTicker'),
                   sep = 'c|f') %>%
          select(-replace) %>%
          mutate(idTicker = ifelse(idTicker == '', NA, idTicker)) %>%
          mutate_at(.cols = df %>% select(matches("^count|^amount|idCIK")) %>% names,
                    funs(. %>% as.numeric())) %>%
          mutate_at(.cols = df %>% select(matches("^name")) %>% names,
                    funs(. %>% str_to_upper())) %>%
          mutate(idInsiderTable = x) %>%
          suppressWarnings()

        return(df)
      }) %>%
      left_join(insider_name_df) %>%
      select(
        nameTable,
        typeInsider,
        typeTransaction,
        idCIK,
        nameCompany,
        idTicker,
        idCIKPerson,
        namePerson,
        amountSales,
        countSales,
        amountPurchases,
        countPurchases
      ) %>%
      suppressMessages() %>%
      mutate(datetimeData = Sys.time())

    all_data <-
      all_data %>%
      mutate_at(
        .cols = all_data %>% select(matches("amount")) %>% names(),
        funs(. %>% as.numeric %>% formattable::currency(digits = 0))
      ) %>%
      mutate_at(
        .cols = all_data %>% select(matches("count")) %>% names(),
        funs(. %>% as.numeric %>% formattable::comma(digits = 0))
      ) %>%
      arrange(nameCompany, nameTable)

    if (return_message) {
      all_data %>%
        filter(typeInsider == 'Person') %>%
        filter(!namePerson %>% is.na()) %>%
        print_message(table_name = 'Insider Transactions, Last 7 Days')
    }

    all_data <-
      all_data %>%
      resolve_cik_url()

    return(all_data)

  }

#' Get Securities Filing Counts
#'
#' @param return_message
#' @import dplyr purrr
#' @importFrom  tidyr gather
#' @importFrom readr read_csv
#' @return
#' @export
#'
#' @examples
get_data_securities_filing_counts <-
  function(return_message = TRUE) {
    data_frame(
      idForm = c('D', 'W', 'S-1', 'S-3', 'S-4'),
      nameForm = c(
        "Exempt Offering",
        "Secondary Sale",
        "IPOs",
        "IPO Withdrawls",
        "Merger"
      )
    )

    filing_count_data <-
      "http://rankandfiled.com/data/registered_offerings" %>%
      jsonlite::fromJSON(simplifyDataFrame = TRUE) %>%
      flatten_df() %>%
      unnest() %>%
      mutate(idForm = c('D', 'W', 'S-1', 'S-3', 'S-4')) %>%
      select(idForm, everything()) %>%
      gather(yearFiling, countFilings, -idForm) %>%
      mutate(yearFiling = yearFiling %>% as.numeric()) %>%
      filter(countFilings > 0)

    if (return_message) {
      list(
        "There have been ",
        filing_count_data$countFilings %>% sum() %>% formattable::comma(digits = 0),
        " Securities filings from ",
        filing_count_data$yearFiling %>% min,
        ' to ',
        filing_count_data$yearFiling %>% max
      ) %>%
        purrr::invoke(paste0, .) %>%
        message()
    }

    return(filing_count_data)

  }

generate_securities_urls <-
  function() {
    count_df <-
      get_data_securities_filing_counts(return_message = FALSE) %>%
      mutate(lengthOut = (countFilings %/% 50) + 1)

    url_df <-
      1:nrow(count_df) %>%
      map_df(function(x) {
        row_df <-
          count_df %>%
          slice(x)
        url_data <-
          list(
            "http://rankandfiled.com/data/registered_selection?form=",
            row_df$idForm,
            '&year=',
            row_df$yearFiling,
            '&start=',
            seq(0, by = 50, length.out = row_df$lengthOut)
          ) %>%
          purrr::invoke(paste0, .)

        data_frame(
          idForm = row_df$idForm,
          yearFiling = row_df$yearFiling,
          urlData = url_data
        )
      })

    return(url_df)
  }

parse_securities_url <-
  function(url = "http://rankandfiled.com/data/registered_selection?form=S-4&year=1996&start=400",
           column_names =  c('idCIK', 'nameIssuer', 'idTicker', 'idForm'),
           return_message = TRUE) {
    has_data <-
      url %>%
      jsonlite::fromJSON(simplifyDataFrame = TRUE) %>%
      .$detail %>% nrow() > 0

    if (has_data) {
      data <-
      url %>%
      jsonlite::fromJSON(simplifyDataFrame = TRUE) %>%
      .$detail %>%
      data.frame(stringsAsFactors = FALSE) %>%
      as_data_frame() %>%
      purrr::set_names(c('dateFiling', 'offering')) %>%
      suppressWarnings() %>%
      suppressMessages()

    data <-
      data %>%
      mutate(dateFiling = dateFiling %>% lubridate::ymd()) %>%
      separate(
        offering,
        into = column_names,
        sep = '\\*'
      ) %>%
      clean_names() %>%
      clean_ticker() %>%
      clean_cik() %>%
      resolve_cik_url() %>%
      suppressWarnings() %>%
      suppressMessages()

    data <-
      data %>%
      mutate_at(.cols =
                  data %>% select(matches("^amount|^count|idIndustry")) %>% names(),
                .funs = as.numeric
      )

    if (return_message) {
      list("Parsed: ", url) %>%
        purrr::invoke(paste0, .) %>% message()
    }

    return(data)
    }
  }

#' Get Securities Offering data
#'
#' @param year_forms
#' @param forms
#' @param return_message
#'
#' @return
#' @export
#'
#' @examples
get_data_years_securities_offerings <-
  function(year_forms = NULL,
           forms = NULL,
           return_message = TRUE) {

    url_df <-
      generate_securities_urls()

    if ('forms' %>% exists() & !forms %>% purrr::is_null()) {
      url_df <-
        url_df %>%
        dplyr::filter(idForm %in% forms)

    }

    if ('year_forms' %>% exists() &
        !year_forms %>% purrr::is_null()) {
      url_df <-
        url_df %>%
        dplyr::filter(yearFiling %in% year_forms)
    }

    urls <-
      url_df$urlData

    parse_securities_url_safe <-
      purrr::possibly(parse_securities_url, NULL)

    all_data <-
      urls %>%
      map_df(function(x) {
        parse_securities_url_safe(
          url = x,
          column_names = c('idCIK', 'nameIssuer', 'idTicker', 'idForm'),
          return_message = TRUE
        )
      }) %>%
      arrange(dateFiling) %>%
      distinct()

    all_data <-
      all_data %>%
      left_join(data_frame(
        idForm =
          c("S-4", "S-3", "S-1", "W", "D"),
        typeForm =
          c(
            "Merger",
            "Secondary Offering",
            "IPO",
            "IPO Withdrawl",
            "Exempt Offering"
          )
      )) %>%
      select(dateFiling, typeForm, idForm, everything()) %>%
      suppressMessages()

    if (return_message) {
      list(
        "You acquired ",
        all_data %>% nrow() %>% formattable::comma(digits = 0),
        " Securities offerings from ",
        all_data$dateFiling %>% min,
        ' to ',
        all_data$dateFiling %>% max
      ) %>%
        purrr::invoke(paste0, .) %>%
        message()
    }

    return(all_data)

  }


# search_names ------------------------------------------------------------------

generate_search_name <-
  function(entity_name = "Rockwood Capital") {
    json_url <-
      list("http://rankandfiled.com/data/search?q=",
           entity_name %>% stringr::str_to_lower() %>% URLencode()) %>%
      purrr::invoke(paste0, .)

    return(json_url)
  }

parse_rf_search_name <-
  function(url = "http://rankandfiled.com/data/search?q=kav") {
    data <-
      url %>%
      jsonlite::fromJSON(simplifyDataFrame = TRUE) %>%
      .$results %>%
      data.frame(results = ., stringsAsFactors = FALSE) %>%
      as_data_frame()

    type_df <-
      data_frame(
        idTypeFiler = c('f', 'i', 'c'),
        typeFiler = c('Filer', 'Insider',
                          'Company'),
        slugFiler = c('filers', 'insiders', 'public')
      )

    data <-
      data %>%
      tidyr::separate(results,
                      sep = '\\*',
                      into = c('nameEntity', 'X2')) %>%
      mutate(nameEntity = nameEntity %>% stringr::str_to_upper()) %>%
      separate(X2,
               into = c('idCompanyType', 'idCIKTicker', 'idTypeFiler'),
               sep = '\\#') %>%
      mutate(idCIK = idCIKTicker %>% readr::parse_number()) %>%
      left_join(type_df) %>%
      suppressWarnings() %>%
      suppressMessages()

    data <-
      data %>%
      mutate(
        urlDataRF =
          list('http://rankandfiled.com/#/', slugFiler, '/', idCIKTicker, '/filings') %>% purrr::invoke(paste0, .)
      ) %>%
      select(-slugFiler)

    has_tickers <-
      data$nameEntity %>% stringr::str_count('\\[') %>% sum()

    if (has_tickers) {
      data <-
        data %>%
        separate(nameEntity,
                 into = c('nameEntity', 'idTicker'),
                 sep = '\\[') %>%
        mutate(
          nameEntity = nameEntity %>% stringr::str_trim(),
          idTicker = idTicker %>% str_replace('\\]', '') %>% stringr::str_trim(),
          nameCompany = ifelse(!idTicker %>% is.na(), nameEntity, NA) %>% str_to_upper()
        ) %>%
        select(nameCompany, everything()) %>%
        suppressWarnings()
    }

  has_individuals <-
    data$idTypeFiler %>% str_count(pattern = '\\i') %>% sum() > 0

  if (has_individuals) {
    data <-
      data %>%
      mutate(namePerson = ifelse(idTypeFiler == "i", nameEntity, NA)) %>%
      tidyr::separate(namePerson, into = c('namePerson', 'remove'), sep = '\\|') %>%
      select(-remove) %>%
      select(matches("name"), everything()) %>%
      suppressWarnings()

    people <-
      data$namePerson %>%
      map_chr(function(x){
        if (x %>% is.na()) {
          return(NA)
        }

        is_entity <-
          x %>% str_detect("LP|LLC|TRUST|REALTY|GP|LTD")

        if (is_entity) {
          return(x)
        }

        is_doctor <-
          x %>%
          substr(start = (x %>% nchar) -1, stop = x %>% nchar) == "DR"

        if (is_doctor) {
          person_name <-
            x %>%
            substr(1, x %>% nchar -2) %>%
            str_trim()

          words <-
            person_name %>%
            str_split('\\ ') %>% flatten_chr()

          if (words %>% length == 2){
            name_first <-
              words[[2]]

            name_last <-
              words[[1]]

            name_middle <-
              ''
          }

          if (words %>% length == 3){
            name_first <-
              words[[2]]

            name_last <-
              words[[1]]

            name_middle <-
              words[[3]]
          }

          person_name <-
            list("DR ",name_first, ' ', name_middle, ' ', name_last) %>%
            purrr::invoke(paste0, .) %>%
            stringr::str_trim()

          return(person_name)
        }
        words <-
          x %>% str_split('\\ ') %>%
          flatten_chr()

        if (words %>% length == 2) {
          return(list(words[[2]], ' ', words[[1]]) %>%
            purrr::invoke(paste0, .))
        }

        if (words %>% length == 3) {
          return(list(words[[2]], ' ', words[[3]], ' ', words[[1]]) %>%
                   purrr::invoke(paste0, .))
        }
      }) %>%
      str_trim()

    data %>%
      mutate(namePerson = people) %>%
      mutate(nameEntity = ifelse(idTypeFiler == "i", namePerson, nameEntity))
  }
  return(data)
  }

get_data_sec_entity <-
  function(entity_name = "Rockwood Capital",
           return_message = TRUE) {
    json_url <-
      entity_name %>%
      generate_search_name()

    data <-
      json_url %>%
      parse_rf_search_name() %>%
      mutate(nameEntitySearch = entity_name) %>%
      select(nameEntitySearch, matches("^name"), everything())

    if (return_message) {
      list("Returned ",
           data %>% nrow(),
           ' SEC registered entities matching the name ',
           entity_name) %>%
        purrr::invoke(paste0, .) %>% message()
    }

    return(data)
  }

#' Get SEC Filing Entities for Search Names
#'
#' @param entity_names
#' @param return_message
#'
#' @return
#' @export
#' @import purrr dplyr stringr tidyr
#' @importFrom jsonlite fromJSON
#'
#' @examples
get_data_sec_filing_entities <-
  function(entity_names = c('Rockwood Capital', 'Vornado', 'Two Sigma'),
           return_message = TRUE) {

    no_entry <-
      (entity_names %>% purrr::is_null() | !'entity_names' %>% exists())

    if (no_entry) {
      stop("Please enter a search name")
    }

    get_data_sec_entity_safe <-
      purrr::possibly(get_data_sec_entity, NULL)

    all_data <-
      entity_names %>%
      map_df(function(x){
        get_data_sec_entity_safe(entity_name = x, return_message = return_message)
      })

    return(all_data)
  }

# form_ds -----------------------------------------------------------------


#' Get Form D Filing Data
#'
#' @param industries
#' @param years_filings
#' @import dplyr purrr lubridate tidyr
#' @return
#' @export
#'
#' @examples
get_data_form_ds <-
  function(industries = NULL, years_filings = NULL, return_message = TRUE) {
  now_year <-
    Sys.Date() %>% lubridate::year()

  now_month <-
    Sys.Date() %>% lubridate::month()

  if (now_month %>% nchar == 1) {
    now_month <-
      "0" %>% paste0(., now_month)
  } else {
    now_month <-
      now_month %>%
      as.character()
  }

  now_period <-
    list(now_year, now_month) %>%
    purrr::invoke(paste0, .)

  years <-
    2009:(now_year)

  months_names <-
    c("01",
      "02",
      "03",
      "04",
      "05",
      "06",
      "07",
      "08",
      "09",
      "10",
      "11",
      "12")

  period_df <-
    tidyr::crossing(year = years, month = months_names) %>%
    tidyr::unite(namePeriod, year, month, sep = '', remove = FALSE) %>%
    mutate(idPeriod = 1:n())

  now <-
    period_df %>%
    filter(namePeriod == now_period) %>%
    .$idPeriod

  period_df <-
    period_df %>%
    filter(idPeriod <= now)

  if ('years_filings' %>% exists() & !years_filings %>% purrr::is_null()) {
    period_df <-
      period_df %>%
      filter(year %in% years_filings)
  }

  periods <-
    period_df$namePeriod

  category_df <-
    get_form_d_category_df()

  if ('industries' %>% exists() & !industries %>% purrr::is_null()) {
    industries <-
      industries %>% str_to_upper()
    industry_names <-
      c(
        "ENERGY",
        "FINANCIAL",
        "HEALTHCARE",
        "OTHER",
        "REAL ESTATE",
        "TECHNOLOGY",
        "TRAVEL AND LEISURE"
      )
    if (!industries %in% industry_names) {
      stop(
        list(
          "Sorry industries can only be\n",
          industry_names %>% paste0(collapse = '\n')
        ) %>%
          purrr::invoke(paste0, .)
      )
    }

    category_df <-
      category_df %>%
      filter(nameIndustryParent %in% industries)
  }

  category_ids <-
    category_df$idIndustry

  urls <-
    tidyr::crossing(namePeriod = periods, idCategory = category_ids) %>%
    mutate(urlJSON = list('http://rankandfiled.com/data/private_selection?month=', namePeriod, '&ind=', idCategory) %>% purrr::invoke(paste0,.)) %>%
    .$urlJSON

  parse_securities_url_safe <-
    purrr::possibly(parse_securities_url, NULL)

    all_data <-
      urls %>%
      map_df(function(x) {
        parse_securities_url_safe(
          url = x,
          column_names = c(
            'idIndustry',
            'nameIssuer',
            'idTicker',
            'idCIK',
            'amountSold',
            'amountOffered',
            'countInvestors',
            'countInvestorsNonAccredited',
            'amountInvestmentMinimum',
            'idExemption'
          )
        )
      }) %>%
      distinct()

    all_data <-
      all_data %>%
      mutate_at(.cols = all_data %>% select(matches("^amount")) %>% names,
                funs(. %>% formattable::currency(digits = 0))) %>%
      mutate_at(.cols = all_data %>% select(matches("^count")) %>% names,
                funs(. %>% formattable::comma(digits = 0))) %>%
      arrange(desc(dateFiling)) %>%
      left_join(category_df) %>%
      mutate(pctSold = (amountSold / amountOffered) %>% formattable::percent(digits = 2)) %>%
      select(
        dateFiling,
        nameIssuer,
        idIndustry,
        nameIndustry,
        codeIndustryParent,
        nameIndustryParent,
        everything()
      ) %>%
      select(dateFiling:amountOffered, pctSold, everything()) %>%
      suppressMessages()

    if (return_message) {
      list(
        "You acquired data from ",
        all_data %>% nrow() %>% formattable::comma(digits = 0),
        " Form D's totaling ",
        all_data$amountSold %>% sum(na.rm = T) %>% formattable::currency(digits = 0),
        ' in raised capital from ',
        all_data$dateFiling %>% min(),
        ' to ',
        all_data$dateFiling %>% max()
      ) %>%
        purrr::invoke(paste0, .) %>%
        message()
    }
    return(all_data)
  }




# filers ------------------------------------------------------------------

parse_json_general_filing <-
  function(url = "http://rankandfiled.com/data/filer/1138621/general",
           return_message = TRUE) {

    data <-
      url %>%
      fromJSON(simplifyDataFrame = TRUE) %>%
      data.frame(stringsAsFactors = FALSE) %>%
      as_data_frame()

    data <-
      data %>%
      resolve_name_df()

    cik <-
        url %>% str_replace_all('http://rankandfiled.com/data/filer/', '') %>%
        str_replace_all('\\/general', '') %>%
        as.numeric()

    data <-
        data %>%
        mutate(idCIK = cik)

    if (!'nameEntity' %in% names(data)) {
      if ('company' %in% names(data)) {
        company_name_df <-
          get_data_sec_filing_entities(entity_names = data$company)
        has_rows <-
          company_name_df %>% nrow > 0
        if (has_rows) {
        cn <-
          company_name_df$nameEntity[[1]] %>% str_split('\\|') %>%
          flatten_chr()
        if (cn %>% length > 1) {
          entity_name <-
            cn[[2]]
        } else {
          entity_name <-
            cn[[1]]
        }
        } else {
          entity_name <-
            get_data_us_tickers() %>%
            filter(idTicker == data$company) %>%
            .$nameCompany %>%
            .[[1]]

      }
        } else {
        entity_name <-
          NA
      }
      data <-
        data %>%
        mutate(nameEntity = entity_name) %>%
        select(-matches("company"))
    }

    data <-
      data %>%
      select(-matches("objectIssuer")) %>%
      mutate_at(.cols = data %>% select(matches("idCIK|idIRS")) %>% names(),
                as.numeric) %>%
      mutate(urlJSONGeneral = url,
             nameEntity = nameEntity %>% stringr::str_to_upper()) %>%
      select(nameEntity, idCIK, matches("typeCategory"), matches("typeCompany"), everything())

    if (return_message) {
      list("Parsed: ", url) %>%
        purrr::invoke(paste0, .) %>% message()
    }

    return(data)

  }

parse_json_filings <-
  function(url = "http://rankandfiled.com/data/filer/1138621/filings",
           return_message = TRUE) {

    cik <-
      url %>% str_replace_all('http://rankandfiled.com/data/filer/|/filings','') %>%
      as.numeric()

    json_data <-
      url %>%
      jsonlite::fromJSON(simplifyDataFrame = TRUE) %>%
      data.frame(stringsAsFactors = FALSE) %>%
      as_data_frame() %>%
      tidyr::separate(
        filings,
        sep = '\\*',
        into = c(
          'dateFiling',
          'codeFiling',
          'typeForm',
          'X1',
          'detailOffering',
          'slugSEC',
          'idSECSlug'
        )
      ) %>%
      mutate(
        dateFiling = dateFiling %>% as.numeric() %>% lubridate::ymd,
        idCIK = cik,
        urlJSONFilings = url,
        urlSEC = ifelse(
          slugSEC == "None",
          NA,
          list(
            "https://www.sec.gov/Archives/edgar/data/",
            idCIK,
            '/',
            slugSEC
          ) %>% purrr::invoke(paste0, .)
        )
      ) %>%
      select(-matches("^X")) %>%
      suppressMessages() %>%
      select(-slugSEC) %>%
      select(idCIK, dateFiling, everything())

    if (return_message) {
      list("Parsed: ", url) %>%
        purrr::invoke(paste0, .) %>% message()
    }

    return(json_data)
  }

parse_json_private <-
  function(url = "http://rankandfiled.com/data/filer/1503140/private",
           return_message = TRUE) {

    if (!url %>% httr::url_ok() %>% suppressWarnings()) {
      return(data_frame())
    }

    json_data <-
      url %>%
      fromJSON()

    options(scipen = 9999)

    status_df <-
      json_data$status_history %>% flatten_df() %>%
      mutate(date = date %>% lubridate::ymd())

    offering_history_class_df <-
      json_data$offering_history %>% map_df(class) %>%
      gather(column, type) %>%
      mutate(idName = 1:n())

    offering_data <-
      json_data$offering_history %>%
      select(offering_history_class_df %>%
               filter(!type == 'list') %>%
               .$idName)

    offering_data <-
      offering_data %>%
      as_data_frame() %>%
      mutate_all(funs(. %>% str_replace('\\|', '')))

    offering_data <-
      offering_data %>%
      resolve_name_df()

    if (offering_data %>% ncol >= 9) {
    offering_data <-
      offering_data %>%
      separate(dateAmmended, into = c('dateAmmended', 'dateAmmended1'), sep = '\\|') %>%
      separate(amountFindersFee, into = c('amountFindersFee', 'amountFindersFee1'), sep = '\\|') %>%
      separate(countInvestors, into = c('countInvestors', 'countInvestors1'), sep = '\\|') %>%
      separate(countInvestorsNonAccredited, into = c('countInvestorsNonAccredited', 'countInvestorsNonAccredited1'), sep = '\\|') %>%
      separate(amountOffered, into = c('amountOffered', 'amountOffered1'), sep = '\\|') %>%
      separate(amountRemaining, into = c('amountRemaining', 'amountRemaining1'), sep = '\\|') %>%
      separate(amountSold, into = c('amountSold', 'amountSold1'), sep = '\\|') %>%
      suppressWarnings()

    offering_data <-
      offering_data %>%
      mutate_at(.cols = offering_data %>% select(matches("^is")) %>% names,
                funs(. %>% as.logical())) %>%
      mutate_at(.cols = offering_data %>% select(matches("^amount|^count|^idCIK")) %>% names,
                funs(. %>% as.numeric())) %>%
      mutate_at(.cols = offering_data %>% select(matches("^date")) %>% names,
                funs(. %>% lubridate::ymd())) %>%
      mutate_at(.cols = offering_data %>% select(matches("^amount")) %>% names,
                funs(. %>% formattable::currency(digits = 0))) %>%
      mutate_at(.cols = offering_data %>% select(matches("^count")) %>% names,
                funs(. %>% formattable::comma(digits = 0))) %>%
      suppressWarnings()
    } else {
      offering_data <-
      offering_data %>%
      mutate_at(.cols = offering_data %>% select(matches("^amount|^count|^idCIK")) %>% names,
              funs(. %>% as.numeric())) %>%
      mutate_at(.cols = offering_data %>% select(matches("^date")) %>% names,
                funs(. %>% lubridate::ymd()))
    }

    has_relations <-
      '_related_people' %in% names(json_data$offering_history)

    if (has_relations) {
    relation_df <-
      1:(json_data$offering_history$amended %>% length()) %>%
      map_df(function(x){
        data_frame(nameRelation =
                     json_data$offering_history$`_related_people`[[x]] %>% mutate(name =
                                                                                    ifelse(name %>% substr(1,3) %>% str_detect('\\-'),
                                                                                           name %>% str_replace_all('\\-','') %>% str_trim,
                                                                                           name %>% str_trim)) %>%
                     tidyr::unite(nameRelation, name, relation, sep = '-') %>%
                     .$nameRelation %>% paste0(collapse = '&'))
      })
    offering_data <-
      offering_data %>%
      bind_cols(relation_df)
    }

    has_brokers <-
      '_brokers' %in% names(json_data$offering_history)

    if (has_brokers) {
      broker_df <-
      1:(json_data$offering_history$amended %>% length()) %>%
      map_df(function(x){
        empty_value <-
          json_data$offering_history$`_brokers`[[x]] %>%is_null()
        if (empty_value) {
          broker_crd <-
            NA
        } else {
          broker_crd <-
            json_data$offering_history$`_brokers`[[x]] %>%
            tidyr::unite(nameBrokerCRD, name, crd, sep = '&') %>%
            .$nameBrokerCRD %>%
            paste0(collapse = ' | ')
        }
        data_frame(nameBrokerCRD = broker_crd)
      })
      offering_data <-
        offering_data %>%
        bind_cols(broker_df)
    }

    if ('date' %in% names(status_df)) {
      initial_date <-
      status_df$date
    } else {
      initial_date <-
        NA
    }

    if ('entity_type' %in% names(status_df)) {
      typeEntity <-
      status_df$entity_type
    } else {
      typeEntity <-
        NA
    }

    if ('jurisdiction' %in% names(status_df)) {
      jurisdiction <-
        status_df$jurisdiction
    } else {
      jurisdiction <-
        NA
    }

    if ('over_five' %in% names(status_df)) {
      has_five <-
        status_df$over_five
    } else {
      has_five <-
        FALSE
    }
    offering_data <-
      offering_data %>%
      mutate(
        dateInitialFiling = initial_date,
        typeEntity = typeEntity,
        locationJurisdiction = jurisdiction,
        hasOverFileFilings = has_five,
        urlJSONFilings = url
      )

    if (return_message) {
      list("Parsed: ", url) %>%
        purrr::invoke(paste0, .) %>% message()
    }
    return(offering_data)
  }

parse_json_fundraising <-
  function(url = "http://rankandfiled.com/data/filer/1138621/fundraising", return_message = TRUE) {
    json_data <-
      url %>%
      jsonlite::fromJSON()

    fundraising_df <-
      json_data$results %>%
      as_data_frame() %>%
      purrr::set_names(c('idCIKs', 'nameCompanies', 'isCompany', 'namePerson', 'offeringsValues')) %>%
      mutate(idPerson = 1:n(),
             idCIK = url %>% str_replace_all('http://rankandfiled.com/data/filer/|/fundraising', '') %>% as.numeric(),
             namePerson = namePerson %>% str_replace_all('\\-','') %>% stringr::str_to_upper() %>% str_trim(),
             urlJSONFundraising = url) %>%
      suppressWarnings()

    company_name_df <-
      1:length(fundraising_df$nameCompanies) %>%
      map_df(function(x) {
        company_name_data <-
          fundraising_df$nameCompanies[[x]]

        company_name_data <-
          company_name_data %>%
          str_split('\\*') %>%
          flatten_chr() %>%
          str_to_upper()

        data_frame(value = company_name_data) %>%
          mutate(item = 'nameCompanyFundraisingRelated') %>%
          mutate(countRow = 1:n()) %>%
          mutate(
            countRow = countRow - 1,
            item = ifelse(countRow == 0, item, item %>% paste0(countRow)),
            idPerson = x
          ) %>%
          select(-countRow) %>%
          spread(item, value)
      })


    offering_value_df <-
      1:length(fundraising_df$offeringsValues) %>%
      map_df(function(x) {
        offering_value_data <-
          fundraising_df$offeringsValues[[x]]
        offering_value_data <-
          offering_value_data %>%
          str_split('\\*') %>%
          flatten_chr()

        data_frame(offering = offering_value_data) %>%
          tidyr::separate(
            offering,
            into = c(
              'idCIKRelatedCompanyFundraising',
              'idIndustryRelatedCompanyFundRaising',
              'amountRaisedRelatedCompanyFundRaising'
            ),
            sep = '\\|'
          ) %>%
          mutate(countRow = 1:n()) %>%
          gather(item, value, -countRow) %>%
          mutate(
            countRow = countRow - 1,
            value = value %>% as.numeric(),
            item = ifelse(countRow == 0, item, item %>% paste0(countRow)),
            idPerson = x
          ) %>%
          select(-countRow) %>%
          spread(item, value)
      })

    fundraising_df <-
      fundraising_df %>%
      left_join(company_name_df) %>%
      left_join(offering_value_df) %>%
      select(-c(idCIKs, nameCompanies, idPerson, offeringsValues)) %>%
      select(idCIK, namePerson, isCompany, everything()) %>%
      suppressMessages() %>%
      suppressWarnings()

    if (return_message) {
      list("Parsed: ", url) %>%
        purrr::invoke(paste0, .) %>% message()
    }
    return(fundraising_df)
  }

parse_cik_data <-
  function(cik = 1138621, return_message = TRUE) {
    url_df <-
      cik %>%
      get_cik_url_df()

  general_df <-
    url_df$urlJSON[[1]] %>%
      parse_json_general_filing(return_message = return_message)

  parse_json_filings_safe <-
    purrr::possibly(parse_json_filings, data_frame())

  filing_df <-
    url_df$urlJSON[[2]] %>%
    parse_json_filings_safe(return_message = return_message)

  has_rows  <-
    filing_df %>% nrow() > 0

  if (has_rows) {
  filing_df <-
    filing_df %>%
    left_join(
      general_df %>% select(idCIK, nameEntity)
    ) %>%
    select(nameEntity, idCIK, everything()) %>%
    suppressMessages()
  }

  parse_json_private_safe <-
    purrr::possibly(parse_json_private, data_frame())

  private_df <-
    url_df$urlJSON[[3]] %>%
    parse_json_private(return_message = return_message)

  has_rows  <-
    private_df %>% nrow() > 0

  if (has_rows) {
    private_df <-
    private_df %>%
    left_join(
      general_df %>% select(idCIK, nameEntity)
    ) %>%
    select(nameEntity, idCIK, everything()) %>%
    suppressMessages()
  }
  parse_json_fundraising_safe <-
    purrr::possibly(parse_json_fundraising, data_frame())

  fundraising_df <-
    url_df$urlJSON[[4]] %>%
    parse_json_fundraising_safe(return_message = return_message)

  has_rows  <-
    fundraising_df %>% nrow() > 0
  if (has_rows) {
    fundraising_df <-
    fundraising_df %>%
    left_join(
      general_df %>% select(idCIK, nameEntity)
    ) %>%
    select(nameEntity, idCIK, everything()) %>%
    suppressMessages()
  }
  nameEntity <-
    general_df$nameEntity

  all_data <-
    data_frame(
      idCIK = cik,
      nameEntity,
      nameTable = c('General', 'Filings', 'Private Offerings', 'Related Parties'),
      dataTable = list(general_df, filing_df, private_df, fundraising_df)
    )

  if (return_message) {
    list("\nParsed SEC Private Filing Data for CIK: ", cik, ' - ',nameEntity, "\n") %>%
      purrr::invoke(paste0, .) %>%
      message()
  }

  all_data <-
    all_data %>%
    mutate(countRows = dataTable %>% purrr::map_dbl(nrow)) %>%
    filter(countRows > 0 ) %>%
    select(-countRows) %>%
    suppressWarnings()

  return(all_data)
  }

#' Get SEC Private Filer data
#'
#' @param entity_names
#' @param ciks
#' @param tables
#' @param assign_to_environment
#' @param return_message
#' @import dplyr stringr tidyr purrr formattable
#' @importFrom jsonlite fromJSON
#' @importFrom httr url_ok
#' @return
#' @export
#'
#' @examples
get_data_sec_private_filers <-
  function(entity_names = NULL,
           ciks = NULL,
           tables = NULL,
           assign_to_environment = TRUE,
           return_message = TRUE) {

    has_entities <-
      (('entity_names' %>% exists()) & (!entity_names %>% purrr::is_null()))

    has_ciks <-
      (('ciks' %>% exists()) & (!ciks %>% purrr::is_null()))

    has_nothing <-
      ((!has_ciks) & (!has_entities))

    has_tables <-
      (!tables %>% purrr::is_null()) #(('tables' %>% exists()) |

    if (has_nothing) {
      stop("Please enter a CIK or entity name")
    }

    all_ciks <-
      c()

    if (has_entities) {
      search_df <-
        entity_names %>%
        get_data_sec_filing_entities(return_message = return_message)

      has_rows <-
        search_df %>% nrow() > 0

      if (has_rows) {
        search_ciks <-
          search_df %>%
          filter(idTypeFiler == 'f') %>%
          .$idCIK
        all_ciks <-
          all_ciks %>%
          append(search_ciks)
      }
    }

    if (has_ciks) {
      all_ciks <-
        all_ciks %>%
        append(ciks)
    }

    parse_cik_data_safe <-
      possibly(parse_cik_data, NULL)

    all_data <-
      all_ciks %>%
      sort() %>%
      map_df(function(x) {
        parse_cik_data_safe(cik = x, return_message = return_message)
      }) %>%
      mutate(urlRankAndFiled =
               list('http://rankandfiled.com/#/filers/', idCIK, '/filings') %>% purrr::invoke(paste0, .)) %>%
      select(idCIK, nameEntity, urlRankAndFiled, nameTable, dataTable) %>%
      suppressWarnings()

    if (has_tables) {
      table_options <-
        c("General", "Filings", "Related Parties", "Private Offerings"
        )
      table_names <-
        tables %>% str_to_lower() %>% paste0(collapse = "|")

      wrong_table <-
        table_options %>% str_to_lower() %>% str_count(table_names) %>% sum() == 0

      if (wrong_table) {
        stop("Sorry tables can only be:\n" %>% paste0(paste0(table_options, collapse = '\n')))
      }

      all_data <-
        all_data %>%
        mutate(table = nameTable %>% str_to_lower()) %>%
        filter(table %>% str_detect(table_names)) %>%
        select(-table)
    }

    missing_ciks <-
      all_ciks[!all_ciks %in% all_data$idCIK] %>% length() > 0

    if (missing_ciks) {
      list("Missing ", all_ciks[!all_ciks %in% all_data$idCIK] %>% paste(collapse = ', ')) %>%
        purrr::invoke(paste0, .) %>%
        message()
    }

    if (assign_to_environment) {
      table_name_df <-
        all_data %>%
        select(nameTable) %>%
        distinct() %>%
        mutate(nameDF =
                 list('data', nameTable %>% str_replace_all('\\ ','')) %>% purrr::invoke(paste0,.)
               )

      1:nrow(table_name_df) %>%
        walk(function(x){
          df_name <-
            table_name_df %>% slice(x) %>% .$nameDF

          df_data <-
            all_data %>%
            filter(nameTable == table_name_df$nameTable[[x]]) %>%
            select(dataTable) %>%
            unnest()

          df_data <-
            df_data %>%
            mutate_at(.cols = df_data %>% select(matches("^amount")) %>% names(),
                      funs(. %>% formattable::currency(digits = 0))) %>%
            mutate_at(.cols = df_data %>% select(matches("^count")) %>% names(),
                      funs(. %>% formattable::comma(digits = 0))) %>%
            select(
              idCIK,
              nameEntity,
              matches("^name"),
              matches("^type"),
              matches("^date"),
              matches("^is[A-Z]"),
              matches("^has[A-Z]"),
              matches("^amount"),
              matches("^count[A-Z]"),
              matches("^detail[A-Z]"),
              everything()
            ) %>%
            arrange(nameEntity, idCIK)


          assign(x = df_name, eval(df_data), env = .GlobalEnv)

        })
    }

    return(all_data)
  }


# filing_stream -----------------------------------------------------------

# http://rankandfiled.com/data/latest



# publics -----------------------------------------------------------------



# fund --------------------------------------------------------------------


