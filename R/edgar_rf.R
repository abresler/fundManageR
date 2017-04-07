# cunctions ---------------------------------------------------------------
drop_na_columns <-
  function(data) {
    data %>%
      select(which(colMeans(is.na(.)) < 1)) %>%
      suppressMessages() %>%
      suppressWarnings()
  }

tidy_column_formats <-
  function(data, drop_na_columns = TRUE) {
    data <-
      data %>%
      mutate_if(is_character,
                funs(ifelse(. == "N/A", NA, .))) %>%
      mutate_if(is_character,
                funs(ifelse(. == "", NA, .)))

    data <-
      data %>%
      mutate_at(data %>% dplyr::select(dplyr::matches("^idCRD|idCIK")) %>% names(),
                funs(. %>% as.numeric())) %>%
      mutate_at(data %>% dplyr::select(
        dplyr::matches(
          "^name[A-Z]|^details[A-Z]|^description[A-Z]|^city[A-Z]|^state[A-Z]|^country[A-Z]|^count[A-Z]|^street[A-Z]|^address[A-Z]"
        )
      ) %>% dplyr::select(-matches("nameElement")) %>% names(),
      funs(. %>% str_to_upper())) %>%
      mutate_at(
        data %>% dplyr::select(dplyr::matches("^amount")) %>% names(),
        funs(. %>% as.numeric() %>% formattable::currency(digits = 0))
      ) %>%
      mutate_at(data %>% dplyr::select(dplyr::matches("^is|^has")) %>% names(),
                funs(. %>% as.logical())) %>%
      mutate_at(
        data %>% dplyr::select(dplyr::matches("latitude|longitude")) %>% names(),
        funs(. %>% as.numeric() %>% formattable::digits(digits = 5))
      ) %>%
      mutate_at(
        data %>% dplyr::select(dplyr::matches("^price[A-Z]|pershare")) %>% select(-dplyr::matches("priceNotation")) %>% names(),
        funs(. %>% formattable::currency(digits = 3))
      ) %>%
      mutate_at(
        data %>% dplyr::select(dplyr::matches(
          "^count[A-Z]|^number[A-Z]|^year[A-Z]"
        )) %>% dplyr::select(-dplyr::matches("country|county")) %>% names(),
        funs(. %>% formattable::comma(digits = 0))
      ) %>%
      mutate_at(
        data %>% dplyr::select(dplyr::matches(
          "codeInterestAccrualMethod|codeOriginalInterestRateType|codeLienPositionSecuritization|codePaymentType|codePaymentFrequency|codeServicingAdvanceMethod|codePropertyStatus"
        )) %>% names(),
        funs(. %>% as.integer())
      ) %>%
      mutate_at(data %>% dplyr::select(dplyr::matches("^ratio|^multiple|^priceNotation|^value")) %>% names(),
                funs(. %>% formattable::comma(digits = 3))) %>%
      mutate_at(data %>% dplyr::select(dplyr::matches("^pct|^percent")) %>% names(),
                funs(. %>% formattable::percent(digits = 3))) %>%
      mutate_at(
        data %>% dplyr::select(dplyr::matches("^amountFact")) %>% names(),
        funs(. %>% as.numeric() %>% formattable::currency(digits = 3))
      ) %>%
      suppressWarnings()
    has_dates <-
      data %>% dplyr::select(dplyr::matches("^date")) %>% ncol() > 0

    if (has_dates) {
      data %>% dplyr::select(dplyr::matches("^date")) %>% map(class)
    }
    if (drop_na_columns ) {
      data <-
        data %>%
        drop_na_columns()
    }
    return(data)
  }

pad_cik <-
  function(cik = 886982) {
    cik_chars <-
      cik %>%
      nchar()

    zeros_needed <-
      10 - cik_chars
    zeros <-
      rep(0,zeros_needed) %>% as.character() %>% paste0(collapse = '')

    cik_code <-
      list(zeros, cik %>% as.character()) %>%
      purrr::reduce(paste0)

    return(cik_code)
  }

pad_sic <-
  function(sic = 800) {
    sic_chars <-
      sic %>%
      nchar()

    zeros_needed <-
      4 - sic_chars
    zeros <-
      rep(0,zeros_needed) %>% as.character() %>% paste0(collapse = '')

    sic_code <-
      list(zeros, sic %>% as.character()) %>%
      purrr::reduce(paste0)

    return(sic_code)
  }


resolve_legal_name <-
  function(data) {
    has_name <-
      'nameEntityLegal' %in% names(data)
    if (has_name) {
      data <-
        data %>%
        mutate(nameEntity = nameEntityLegal %>% str_to_upper() %>% str_replace_all('\\.|\\,', '') %>% str_trim()) %>%
        separate(nameEntity,
                 sep = '\\ /|\\/',
                 into = c('nameEntity', 'idLocationEntity')) %>%
        select(idCIK, nameEntity, everything()) %>%
        suppressWarnings() %>%
        suppressMessages()
      return(data)
    }
  }

resolve_names_to_upper <-
  function(data) {
    data <-
      data %>%
      mutate_at(
        data %>%
          keep(is_character) %>%
          select(-matches("url")) %>% names(),
        funs(. %>% str_to_upper())
      )

    return(data)
  }

remove_duplicate_columns <-
  function(data) {
    column_ids <-
      data_frame(name = names(data)) %>%
      mutate(idColumn = 1:n()) %>%
      group_by(name) %>%
      mutate(countCol = 1:n()) %>%
      filter(idColumn == min(idColumn)) %>%
      .$idColumn

    data <-
      data[, column_ids]
    return(data)
  }

find_target_filings <-
  function(data) {
    if ('idForm' %in% names(data)) {
      data <-
        data %>%
        mutate(is13FFiling = idForm %>% str_detect("13-F|13F-HR|13F-NT"),
               isFormD = ifelse(idForm %in% c("D", "D/A"), TRUE, FALSE),
               isForm3_4 = ifelse(idForm %in% c("3", "4"), TRUE, FALSE),
               hasAssetFile = idForm %>% str_detect("ABS-EE"),
               hasSmallOfferingData = idForm %>% str_detect("1-A|1-A/A"),
               hasSmallOfferingData = ifelse(idForm == "C", TRUE, hasSmallOfferingData))
    }
    return(data)
  }



# dictionaries ------------------------------------------------------------

get_filer_type_df <-
  function() {
    data_frame(
      idTypeFilerOwner = c(
        'insider',
        'private' ,
        'broker_dealer',
        'transfer_agent',
        'ia',
        'msd',
        'bank',
        'inv_co'
      ),
      typeFilerOwner = c(
        'Insider',
        'Private Placement',
        'Broker Dealer',
        'Transfer Agent',
        'Investment Advisor',
        'Bank',
        'Municipal Securities Dealer',
        'Investment Company'
      )
    ) %>%
      mutate_all(str_to_upper)
  }

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
          "private_industry",
          "private_issuer",
          "private_type",
          'fund_type',
          'is_owned',
          'detail',
          'yahoo',
          'ticker',
          'lei',
          'quarterly',
          'annual',
          'a_instant',
          'a_periodic',
          'q_instant',
          'q_periodic',
          'mda',
          'midas',
          'midased',
          'owned',
          "description",
          "filed",
          "filed_time",
          "filer_type",
          "form",
          "form_type",
          "id",
          "link_filing",
          "parsed",
          'investors',
          'sold',
          'total',
          "owned_category",
          "owned_cik",
          "owned_name",
          "owned_ticker",
          "code",
          "price",
          "traded",
          "trans",
          'bought',
          'dt',
          'ndt',
          "market_cap",
          "sector",
          "x_cat",
          "x_code",
          "ask",
          "bid",
          "book_val",
          "dps",
          "ebitda",
          "eps",
          "open",
          "pe_ratio",
          "prev_close",
          "short_ratio",
          "year_high",
          "year_low",
          "dt_first",
          "filer_name",
          "first_update",
          "last_update",
          "owned_company",
          'ndt_first',
          'etf',
          'filer',
          'ipo_year',
          'insider',
          'fund',
          'trades',
          "asset_backed",
          "business_dev",
          "closed_end_fund",
          "insurance",
          "money_management",
          "money_market_fund",
          "mutual_fund",
          "uit",
          "classes",
          "series_id",
          "series_name"

        ),
        nameActual = c(
          "typeCategory",
          "idCIK",
          "cityEntity",
          "countFilings",
          "idIRS",
          "nameEntity",
          "phoneEntity",
          "stateEntity",
          "stateIncorporation",
          "addressStreet1Entity",
          "addressStreet2Entity",
          "idTypeFiler",
          "zipcodeEntity",
          "industryEntity",
          "objectIssuer",
          "typeEntity",
          'typeFund',
          'idCIKOwnedBy',
          'idFormTypeOwnedBy',
          'objectYahoo',
          'idTicker',
          'idLEI',
          'dateisoQuarterly',
          'dateisoAnnual',
          'isAnnualInstantFiler',
          'isAnnualPeriodicFiler',
          'isQuarterlyInstantFiler',
          'isQuarterlyPeriodicFiler',
          'idMDA',
          'typeMidas',
          'idMidas',
          'detailsOwnedBy',
          "descriptionData",
          "dateFiled",
          "datetimeFiled",
          "typeFiler",
          "idForm",
          "typeForm",
          "idRF",
          "slugSEC",
          "isParsed",
          'countInvestors',
          'amountSold',
          'amountOffering',
          "typeCategoryOwned",
          "idCIKOwned",
          "nameEntityOwned",
          "idTickerOwned",
          "codeTransaction",
          "amountPrice",
          "idTicker",
          "countShares",
          'isBought',
          'dateOriginal',
          'dateSecond',
          "amountEquityMarketCap",
          "nameSector",
          "codeExchange",
          "idExchange",
          "priceAsk",
          "priceBid",
          "priceBookValue",
          "amountDPS",
          "amountEBITDA",
          "amountEPS",
          "priceOpen",
          "ratioPE",
          "priceClosePrevious",
          "ratioShort",
          "price52WeekHigh",
          "price52WeekLow",
          "dateFiledFirst",
          "nameFiler",
          "dateFileUpdated",
          "dateLastUpdated",
          "nameCompanyOwned",
          'dateFirstN',
          'isETF',
          'idCIK',
          'yearIPO',
          'idCIK',
          'idCIK',
          'countTrades',
          "isAssetBackedFund",
          "isBusinessDevelopmentCompany",
          "isClosedEndFund",
          "isInsuranceCompany",
          "isMoneyManager",
          "isMoneyMarketFund",
          "isMutualFund",
          "isUIT",
          "descriptionClasses",
          "idSeries",
          "nameSeries"
        )
      )

    return(general_name_df)
  }

get_private_name_df <-
  function() {
    data_frame(
      nameRF = c(
        "amended",
        "cik",
        "date",
        "exemption",
        "finders_fee",
        "industry",
        "minimum",
        "nonaccredited",
        "num_invested",
        "option",
        "proceeds_used",
        "sale_date",
        "sales_fee",
        "security",
        "total_offering",
        "total_remaining",
        "total_sold",
        "debt",
        "equity",
        "foreign_solicit",
        'fund_type',
        'hedge_fund',
        'other',
        'total_clar',
        'mineral',
        'combo',
        'subsidiary'
      ),
      nameActual = c(
        "dateAmmended",
        "idCIK",
        "dateFiling",
        "idExemption",
        "amountFindersFee",
        "nameIndustry",
        "amountMinimumInvestment",
        "countInvestorsNonAccredited",
        "countInvestors",
        "isOption",
        "amountProceedsUsed",
        "dateSale",
        "amountSaleFee",
        "isSecurity",
        "amountOffered",
        "amountRemaining",
        "amountSold",
        "isDebtSecurity",
        "isEquity",
        "isForeignSolicted",
        'typeFund',
        'isHedgeFund',
        'detailOther',
        'detailClarification',
        'isMineralCompany',
        'detailCombo',
        'objectSubsidiary'
      )
    )
  }

#' Form-D dictionary
#'
#' This function returns searchable
#' industries for parsed SEC Form-D
#' filings
#'
#' @return a \code{data_frame}
#' @export
#' @import dplyr
#' @examples
#' get_dictionary_form_d_categories()
get_dictionary_form_d_categories <-
  function() {
    category_df <-
      dplyr::data_frame(
        idIndustry = 1:35,
        nameIndustry = c(
          "AGRICULTURE",
          "AIRLINES AND AIRPORTS",
          "BIOTECHNOLOGY",
          "BUSINESS SERVICES",
          "COAL MINING",
          "COMMERCIAL REAL ESTATE",
          "COMMERCIAL BANKING",
          "COMPUTERS",
          "CONSTRUCTION",
          "ELECTRIC UTILITIES",
          "ENERGY CONSERVATION",
          "ENVIORNMENTAL SERVICES",
          "HEALTH INSURANCE",
          "HOSPITALS AND PHYSICIANS",
          "INSURANCE",
          "INVESTING",
          "INVESTMENT BANKING",
          "LODGING AND CONVETION",
          "MANUFACTURING",
          "OIL AND GAS",
          "OTHER",
          "OTHER BANKING AND FINANCIAL SERVICES",
          "OTHER ENERGY",
          "OTHER HEALTH CARE",
          "OTHER REAL ESTATE",
          "OTHER TECHNOLOGY",
          "OTHER TRAVEL",
          "PHARMACEUTICALS",
          "POOLED INVESTMENT FUND",
          "REITS AND FINANCE",
          "RESIDENTIAL REAL ESTATE",
          "RESTAURANTS",
          "RETAIL",
          "TELECOMMUNICATIONS",
          "TRAVEL AND TOURISM"
        ),
        codeIndustryParent = c(
          "OTHER",
          "TRAVEL",
          "HEALTH",
          "OTHER",
          "ENERGY",
          "REAL",
          "FINANCE",
          "TECH",
          "REAL",
          "ENERGY",
          "ENERGY",
          "ENERGY",
          "HEALTH",
          "HEALTH",
          "FINANCE",
          "FINANCE",
          "FINANCE",
          "TRAVEL",
          "OTHER",
          "ENERGY",
          "OTHER",
          "FINANCE",
          "ENERGY",
          "HEALTH",
          "REAL",
          "TECH",
          "TRAVEL",
          "HEALTH",
          "FINANCE",
          "REAL",
          "REAL",
          "OTHER",
          "OTHER",
          "TECH",
          "TRAVEL"
        ),
        nameIndustryParent = c(
          "OTHER",
          "TRAVEL AND LEISURE",
          "HEALTHCARE",
          "OTHER",
          "ENERGY",
          "REAL ESTATE",
          "FINANCIAL",
          "TECHNOLOGY",
          "REAL ESTATE",
          "ENERGY",
          "ENERGY",
          "ENERGY",
          "HEALTHCARE",
          "HEALTHCARE",
          "FINANCIAL",
          "FINANCIAL",
          "FINANCIAL",
          "TRAVEL AND LEISURE",
          "OTHER",
          "ENERGY",
          "OTHER",
          "FINANCIAL",
          "ENERGY",
          "HEALTHCARE",
          "REAL ESTATE",
          "TECHNOLOGY",
          "TRAVEL AND LEISURE",
          "HEALTHCARE",
          "FINANCIAL",
          "REAL ESTATE",
          "REAL ESTATE",
          "OTHER",
          "OTHER",
          "TECHNOLOGY",
          "TRAVEL AND LEISURE"
        )
      )
    return(category_df)
  }

get_insider_code_df <-
  function() {
    insider_df <-
      data_frame(
        idInsiderTransaction =
          c(
            "A",
            "C",
            "D",
            "F",
            "G",
            "H",
            "I",
            "J",
            "K",
            "L",
            "M",
            "NONE",
            "O",
            "P",
            "S",
            "U",
            "V",
            "W",
            "X",
            "Z"
          ),
        nameInsiderTransaction = c(
          "AWARD",
          "CONVEYANCE",
          "DISPOSITION TO ISSUER",
          "PAYMENT WITH SECURITIES",
          "GIFT",
          "EXPIRATION OF LONG DERIVATIVE POSITION",
          "DISCRETIONARY TRANSACTION",
          "OTHER",
          "EQUITY SWAP OR SIMILAR",
          "SMALL ACQUISITIONS",
          "EXEMPT",
          NA,
          "OTM EXERCISE",
          "PURCHASE",
          "SALE",
          "MERGER AND ACQUISITION",
          "REPORTED EARLY",
          "WILL OR LAWS OF DESCENT",
          "ITM OR ATM EXERCISE",
          "DEPOSIT INTO/WITHDRAWAL FROM VOTING TRUST"
        ),
        idTypeInsiderTransaction = c(
          "A",
          "D",
          "D",
          "D",
          "D",
          NA,
          NA,
          NA,
          NA,
          "A",
          "A",
          NA,
          "A",
          "A",
          "D",
          NA,
          NA,
          "D",
          "A",
          "D"
        )
      )
    return(insider_df)
  }

#' SEC filing code dictionary
#'
#' This function returns a
#' dictionary of SEC form filing types
#'
#' @return a \code{data_frame}
#' @export
#' @import dplyr stringr
#' @family SEC
#' @family dictionary
#'
#' @examples
#' get_dictionary_sec_filing_codes()
get_dictionary_sec_filing_codes <-
  function() {
    data_frame(
      idFormType = c(
        "1.01",
        "1.02",
        "1.03",
        "1.04",
        "2.01",
        "2.02",
        "2.03",
        "2.04",
        "2.05",
        "2.06",
        "3.01",
        "3.02",
        "3.03",
        "4.01",
        "4.02",
        "5.01",
        "5.02",
        "5.03",
        "5.04",
        "5.05",
        "5.06",
        "5.07",
        "5.08",
        "6.01",
        "6.02",
        "6.03",
        "6.04",
        "6.05",
        "7.01",
        "8.01",
        "9.01"
      ),
      nameFormType = c(
        "Entry into a Material Definitive Agreement",
        "Termination of a Material Definitive Agreement",
        "Bankruptcy or Receivership",
        "Mine Safety Ð Reporting of Shutdowns and Patterns of Violations",
        "Completion of Acquisition or Disposition of Assets",
        "Results of Operations and Financial Condition",
        "Creation of a Direct Financial Obligation or an Obligation under an Off-Balance Sheet Arrangement of a Registrant",
        "Triggering Events That Accelerate or Increase a Direct Financial Obligation or an Obligation under an Off-Balance Sheet Arrangement",
        "Costs Associated with Exit or Disposal Activities",
        "Material Impairments",
        "Notice of Delisting or Failure to Satisfy a Continued Listing Rule or Standard; Transfer of Listing",
        "Unregistered Sales of Equity Securities",
        "Material Modification to Rights of Security Holders",
        "Changes in Registrant's Certifying Accountant",
        "Non-Reliance on Previously Issued Financial Statements or a Related Audit Report or Completed Interim Review",
        "Changes in Control of Registrant",
        "Departure of Directors or Certain Officers; Election of Directors; Appointment of Certain Officers; Compensatory Arrangements of Certain Officers",
        "Amendments to Articles of Incorporation or Bylaws; Change in Fiscal Year",
        "Temporary Suspension of Trading Under Registrant's Employee Benefit Plans",
        "Amendments to the RegistrantÕs Code of Ethics, or Waiver of a Provision of the Code of Ethics",
        "Change in Shell Company Status",
        "Submission of Matters to a Vote of Security Holders",
        "Shareholder Director Nominations",
        "ABS Informational and Computational Material",
        "Change of Servicer or Trustee",
        "Change in Credit Enhancement or Other External Support",
        "Failure to Make a Required Distribution",
        "Securities Act Updating Disclosure",
        "Regulation FD Disclosure",
        "Other Events",
        "Financial Statements and Exhibits"
      ) %>% stringr::str_to_upper()
    )

  }

#' SEC form codes
#'
#' This function returns a
#' dictionary of SEC form codes
#'
#' @return a \code{data_frame}
#' @export
#' @family SEC
#' @family dictionary
#'
#' @examples
#' get_dictionary_sec_form_codes()
get_dictionary_sec_form_codes <-
  function() {
    data_frame(
      idForm = c(
        "R",
        "A",
        "Q",
        "CR",
        "REG",
        "REGX",
        "O",
        "P",
        "X",
        "W",
        "SEC",
        "PROXY",
        "CT",
        "IS",
        "CO",
        "T"
      ),
      nameForm = c(
        "Other Report",
        "Annual Report",
        "Quarterly Report",
        "Current Report",
        "Registration",
        "Private Offering",
        "Ownership",
        "Prospectus",
        "Exemption",
        "Withdrawal",
        "SEC Correspondence",
        "Proxy Statement",
        "Confidential Treatment",
        "Initial Statement",
        "Change in Ownership",
        "Trades"
      ) %>% stringr::str_to_upper()
    )
  }

get_company_type_df <-
  function() {
    data_frame(
      idCompanyType = c(
        "ic",
        "i",
        "ia",
        "bd",
        "m",
        "t",
        "b",
        "c",
        "p",
        "etf",
        "mmf",
        "mf",
        "uit",
        "cef"
      ),
      nameCompanyType = c(
        "Investment Company",
        "Insider",
        "Investment Adviser",
        "Broker-dealer",
        "Municipal Securities Dealer",
        "Transfer Agent",
        "Bank",
        "Company",
        "Private Issuer",
        "ETF",
        "Money Market Fund",
        "Mutual Fund",
        "UIT",
        "Closed-end Fund"
      )
    )
  }

#' SEC Rule dictionary
#'
#' This function retuns a
#' dictionary of SEC rules
#'
#' @return a \code{data_frame}
#' @export
#' @import dplyr stringr
#'
#' @examples
#' get_dictionary_sec_rules()
get_dictionary_sec_rules <-
  function() {
    data_frame(
      idRule = c(
        "06",
        "3C",
        "3C.7",
        "3C.1",
        "06b",
        "04",
        "46",
        "04.1",
        "04.2",
        "04.3",
        "05",
        "3C.6",
        "3C.5",
        "06c",
        "4a5",
        "3C.11",
        "3C.2",
        "3C.3",
        "3C.9",
        "3C.10",
        "3C.4",
        "3C.12",
        "3C.",
        "3C.14",
        "3"
      ),
      nameRule = c(
        "Rule 506",
        "Investment Company Act Section 3c",
        "Investment Company Act Section 3c",
        "Investment Company Act Section 3c",
        "Rule 506b",
        "Rule 504",
        "Rule 506c",
        "Rule 504b(1)(i)",
        "Rule 504b(1)(ii)",
        "Rule 504b(1)(iii)",
        "Rule 505",
        "Investment Company Act Section 3c",
        "Investment Company Act Section 3c",
        "Rule 506c",
        "Securities Act Section 4(a)(5)",
        "Investment Company Act Section 3c",
        "Investment Company Act Section 3c",
        "Investment Company Act Section 3c",
        "Investment Company Act Section 3c",
        "Investment Company Act Section 3c",
        "Investment Company Act Section 3c",
        "Investment Company Act Section 3c",
        "Investment Company Act Section 3c",
        "Investment Company Act Section 3c",
        "Investment Company Act Section 3c"
      )
    ) %>%
      mutate_all(str_to_upper)
  }

# utilities ---------------------------------------------------------------
separate_column <-
  function(data, column_name = "idExemption") {
    has_splits <-
      data %>%
      select(one_of(column_name)) %>%
      magrittr::extract2(1) %>%
      map_dbl(function(x) {
        x %>% str_count('\\|')
      }) %>%
      sum(na.rm = T) > 0

    if (!has_splits) {
      return(data)
    }

    max_split <-
      data %>%
      select(one_of(column_name)) %>%
      magrittr::extract2(1) %>%
      map_dbl(function(x) {
        x %>% str_count('\\|')
      }) %>%
      max()

    column_names <-
      0:max_split %>%
      map_chr(function(x) {
        if (x == 0) {
          return(column_name)
        }
        column_name %>% paste0(x)
      })

    data %>%
      separate_(column_name, into = column_names, sep = '\\|') %>%
      suppressWarnings() %>%
      suppressMessages()

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
            .vars =
              data %>% dplyr::select(matches("^name")) %>% names(),
            funs(. %>% stringr::str_to_upper())
          )
      } else {
        data <-
          data %>%
          mutate_at(
            .vars =
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
        map_chr(function(x) {
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

      data <-
        data %>%
        mutate_at(.vars =
                    data %>% select(
                      matches(
                        "idCIK|idMidas|idIRS|^count|^price|^amount|^ratio|^pct|idMDA|^dateiso|idRF|price|amount|^year"
                      )
                    ) %>% names,
                  funs(. %>% readr::parse_number())) %>%
        suppressWarnings()
      return(data)
    }

    actual_names <-
      names(data) %>%
      map_chr(function(x) {
        name_df %>%
          filter(nameRF == x) %>%
          filter(idRow == min(idRow)) %>%
          .$nameActual
      })

    data <-
      data %>%
      purrr::set_names(actual_names)

    data <-
      data %>%
      mutate_at(.vars =
                  data %>% select(
                    matches(
                      "idCIK|idMidas|idIRS|^count|^price|^amount|^ratio|^pct|idMDA|^dateiso|idRF|price|amount|^year"
                    )
                  ) %>% names,
                funs(. %>% readr::parse_number())) %>%
      suppressWarnings()

    return(data)

  }


# industries --------------------------------------------------------------

#' SIC Codes
#'
#' This function returns Standard Industrial Classification [SIC]
#' codes and the corresponding
#' North American Industry Classification System [NAICS] identification.
#'
#' @param filter_duplicates \code{TRUE} removes duplicate entries
#'
#' @return a \code{data_frame}
#' @export
#' @import dplyr
#' @importFrom readr read_csv
#' @importFrom tidyr separate
#' @family dictionary
#' @examples
#' get_data_sic_naics_codes(filter_duplicates = TRUE)
get_data_sic_naics_codes <-
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
      mutate_at(.vars = c('idSIC', 'idNAICS'),
                funs(. %>% as.numeric())) %>%
      mutate(
        classificationSIC = ifelse(classificationSIC == '', NA, classificationSIC),
        classificationNAICS = ifelse(classificationNAICS == '', NA, classificationNAICS)
      ) %>%
      arrange(idSIC) %>%
      mutate_if(is.character,
                stringr::str_to_upper)

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

#' SEC Prefix Laws
#'
#' This function returns prefix
#' descriptions of SEC rules and the
#' law that created the rule
#'
#' @return a  \code{data_frame}
#' @export
#' @import dplyr purrr
#' @importFrom  tidyr separate
#' @importFrom readr read_csv
#' @examples
#' @family dictionary
#' @family SEC
#' get_data_sec_rules()
get_data_sec_rules <-
  function() {
    codes <-
      "http://rankandfiled.com/static/export/file_numbers.csv" %>%
      parse_rank_and_filed_data(column_names = c("idPrefixSEC", "typeFiling", "nameLawSEC")) %>%
      mutate(typeFiling = ifelse(typeFiling == '', NA, typeFiling)) %>%
      mutate_all(str_to_upper)

    return(codes)
  }

# countries ---------------------------------------------------------------
#' Locations
#'
#' This function returns location codes
#' and the name the location
#'
#' @return a \code{data_frame}
#' @export
#' @import dplyr purrr
#' @importFrom  tidyr separate
#' @importFrom readr read_csv
#' @family dictionary
#' @examples
#' get_data_location_codes()
get_data_location_codes <-
  function() {
    countries <-
      "http://rankandfiled.com/static/export/edgar_state_country.csv" %>%
      parse_rank_and_filed_data(column_names = c('codeLocation', 'nameLocation'))
    countries
  }

# leis --------------------------------------------------------------------

#' LEIs
#'
#' This function returns \href{https://en.wikipedia.org/wiki/Legal_Entity_Identifier}{Legal Entity Identifier}s
#' for entities registered with the SEC
#'
#' @param return_message \code{TRUE} return a message after data import
#' @export
#' @import dplyr purrr formattable
#' @importFrom  tidyr separate
#' @importFrom readr read_csv
#' @family entity search
#' @family SEC
#' @family Rank and Filed
#' @examples
#' get_data_rf_leis()
get_data_rf_leis <-
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

#' Get United States publicly traded ticker data
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
get_data_rf_us_tickers <-
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
      mutate_at(.vars = c('idCIK', 'idSIC', 'idSIC', 'idIRS'),
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
      get_data_location_codes()


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
        get_data_sic_naics_codes(filter_duplicates = TRUE) %>%
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
      suppressMessages() %>%
      arrange(idTicker)

    if (return_message) {
      list(
        "You acquired data for ",
        data %>% nrow() %>% formattable::comma(digits = 0),
        " United States publicly traded companies"
      ) %>%
        purrr::invoke(paste0, .) %>% message()
    }
    return(data)
  }




# debt_securities ---------------------------------------------------------
#' Money Market Debt Securities
#'
#' This function returns
#' debt securities owned by money market funds.
#'
#' @param return_message \code{TRUE} return a message after data import
#'
#' @return a \code{data_frame}
#' @export
#' @import dplyr purrr
#' @importFrom  tidyr separate
#' @importFrom readr read_csv
#' @importFrom stringr str_to_upper
#' @importFrom lubridate ymd
#' @family SEC
#' @family Rank and Filed
#' @family securities search
#' @examples
#' get_data_mmf_owned_debt_securities(return_message = TRUE)
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
        dateMaturity = dateMaturity %>% lubridate::ymd(),
        categorySecurity = categorySecurity %>% stringr::str_to_upper(),
        ratingSecurity = ratingSecurity %>% str_to_upper()
      ) %>%
      select(categorySecurity, idCUSIP, nameSecurity, everything()) %>%
      filter(!idCUSIP %>% is.na())

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
#' 13F owned companies
#'
#' This function returns companies
#' reported in as owned by a 13-F filer.
#' @param return_message \code{TRUE} return a message after data import
#'
#'
#' @return a \code{data_frame}
#' @import dplyr purrr
#' @importFrom  tidyr separate
#' @importFrom readr read_csv
#' @export
#' @family entity search
#' @family SEC
#' @family Rank and Filed
#' @family securities search
#' @examples
#' get_data_sec_13F_companies()
get_data_rf_sec_13F_companies <-
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

# securities_filings ------------------------------------------------------

#' Insider trades, most recent
#'
#' This function returns the most recent insider
#' trades as filed with the SEC.
#'
#' @param return_message \code{TRUE} return a message after data import
#' @param nest_data \code{TRUE} return nested data frame
#' @return nested \code{data_frame} or \code{data_frame} if \code{nest_data = FALSE}
#' @import dplyr purrr tidyr readr lubridate stringr
#' @importFrom jsonlite fromJSON
#' @export
#' @family SEC
#' @family Rank and Filed
#' @family securities transaction
#' @examples
#' get_data_recent_insider_trades(nest_data = TRUE)

get_data_recent_insider_trades <-
  function(nest_data = FALSE,
           return_message = TRUE) {
    options(scipen = 9999)
    json_data <-
      "http://rankandfiled.com/data/buy_sell" %>%
      jsonlite::fromJSON()
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
          mutate_at(.vars = df %>% select(matches("^count|^amount|idCIK")) %>% names,
                    funs(. %>% as.numeric())) %>%
          mutate_at(.vars = df %>% select(matches("^name")) %>% names,
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
      suppressMessages()

    all_data <-
      all_data %>%
      mutate_at(
        .vars = all_data %>% select(matches("amount")) %>% names(),
        funs(. %>% as.numeric %>% formattable::currency(digits = 0))
      ) %>%
      mutate_at(
        .vars = all_data %>% select(matches("count")) %>% names(),
        funs(. %>% as.numeric %>% formattable::comma(digits = 0))
      ) %>%
      mutate_at(
        .vars = all_data %>% select(matches("name|type")) %>% names(),
        funs(. %>% stringr::str_to_upper())
      ) %>%
      arrange(nameCompany, nameTable)

    if (return_message) {
      list("You got ", all_data %>% nrow() %>% formattable::comma(digits = 0), ' Insider Transactions from the last 7 days') %>%
        purrr::reduce(paste0) %>%
        message()
    }

    all_data <-
      all_data %>%
      resolve_cik_url()

    if (nest_data) {
      all_data <-
        all_data %>%
        nest(-c(nameTable, typeInsider, typeTransaction), .key = dataInsiderTrades)
    }

    return(all_data)

  }

#' SEC Filing Counts
#'
#' This function returns counts of SEC filings
#' by the type of form by year since 1994
#'
#' @param return_message \code{TRUE} return a message after data import
#' @import dplyr purrr tidyr readr lubridate stringr formattable
#' @importFrom jsonlite fromJSON
#' @return a \code{data_frame}
#' @export
#' @family SEC
#' @family Rank and Fild
#'
#' @examples
#' get_data_sec_securities_filing_counts()
get_data_sec_securities_filing_counts <-
  function(return_message = TRUE) {
    data_frame(
      idForm = c('D', 'W', 'S-1', 'S-3', 'S-4'),
      nameForm = c(
        "Exempt Offering",
        "Secondary Sale",
        "IPOs",
        "IPO Withdrawls",
        "Merger"
      ) %>% stringr::str_to_upper()
    )

    filing_count_data <-
      "http://rankandfiled.com/data/registered_offerings" %>%
      jsonlite::fromJSON(simplifyDataFrame = TRUE) %>%
      flatten_df() %>%
      unnest() %>%
      mutate(idForm = c('D', 'W', 'S-1', 'S-3', 'S-4')) %>%
      select(idForm, everything()) %>%
      gather(yearFiling, countFilings, -idForm) %>%
      mutate(yearFiling = yearFiling %>% as.numeric(),
             countFilings = countFilings %>% formattable::comma(digits = 0)) %>%
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
      get_data_sec_securities_filing_counts(return_message = FALSE) %>%
      mutate(lengthOut = ceiling(countFilings/50) + 1)

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
        dplyr::as_data_frame() %>%
        purrr::set_names(c('dateFiling', 'offering')) %>%
        suppressWarnings() %>%
        suppressMessages()

      data <-
        data %>%
        mutate(dateFiling = dateFiling %>% lubridate::ymd()) %>%
        tidyr::separate(offering,
                        into = column_names,
                        sep = '\\*') %>%
        clean_names() %>%
        clean_ticker() %>%
        clean_cik() %>%
        resolve_cik_url() %>%
        suppressWarnings() %>%
        suppressMessages()

      data <-
        data %>%
        mutate_at(.vars =
                    data %>% select(matches("^amount|^count|idIndustry")) %>% names(),
                  funs(. %>% as.numeric()))

      if (return_message) {
        list("Parsed: ", url) %>%
          purrr::invoke(paste0, .) %>% message()
      }

      return(data)
    }
  }

#' Securities offerings
#'
#' This function returns securities offering
#' data by form type and year from 1994 onward.
#'
#' @param year_forms years to search starting in 1994, \code{NULL} returns all years
#' @param forms forms to use in search \itemize{
#' \item \code{NULL}: returns all (default)
#' \item \code{D}: Exempt offerings
#' \item \code{W}: Secondary sales
#' \item \code{S-1}: Initial public offerings
#' \item \code{S-4}: Initial public offering withdrawls
#' }
#' @param return_message \code{TRUE} return a message after data import
#' @param nest_data \code{TRUE} return nested data frame
#' @return where \code{nest_data} is \code{TRUE} a nested data_frame by asset,
#' where \code{nest_data} is \code{FALSE} a data_frame
#' @export
#' @family SEC
#' @family entity search
#' @family securities search
#' @family Rank and Filed
#' @examples
#' #' \dontrun{
#'
#' ## All Securities Filings
#' get_data_securities_offerings(year_forms = NULL, forms = NULL, return_message = TRUE, nest_data = TRUE)
#'
#' ## IPOs since 1999
#' get_data_securities_offerings(year_forms = 1999:2017, forms = "S-1", return_message = TRUE, nest_data = FALSE)
#' }
get_data_securities_offerings <-
  function(year_forms = NULL,
           forms = NULL,
           nest_data = FALSE,
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
          c("S-4", "S-3", "S-1", "W", "D", "F-4", "F-3", "F-1", "REGDEX"),
        typeForm =
          c(
            "Merger",
            "Secondary Offering",
            "IPO",
            "IPO Withdrawl",
            "Exempt Offering",
            "Foreign Issuer Merger", "Foreign Private Issuer", "Foreign Public Issuer", "Paper Filing"
          ) %>% str_to_upper()
      )) %>%
      mutate(yearFiling = dateFiling %>% lubridate::year()) %>%
      select(yearFiling, dateFiling, typeForm, idForm, everything()) %>%
      suppressMessages()

    if (return_message) {
      list(
        "You acquired ",
        all_data %>% nrow() %>% formattable::comma(digits = 0),
        " Securities offerings from ",
        all_data$dateFiling %>% min(),
        ' to ',
        all_data$dateFiling %>% max()
      ) %>%
        purrr::invoke(paste0, .) %>%
        message()
    }

    if (nest_data) {
      all_data <-
        all_data %>%
        nest(-c(idForm, typeForm), .key = dataOfferings)

    }

    return(all_data)

  }


# search_names ------------------------------------------------------------------

generate_search_name <-
  function(entity_name = "Rockwood Capital") {
    json_url <-
      list(
        "http://rankandfiled.com/data/search?q=",
        entity_name %>% stringr::str_to_lower() %>% URLencode()
      ) %>%
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
      separate(
        X2,
        into = c('idCompanyType', 'idCIKTicker', 'idTypeFiler'),
        sep = '\\#'
      ) %>%
      mutate(idCIK = idCIKTicker %>% readr::parse_number()) %>%
      left_join(type_df) %>%
      left_join(get_company_type_df()) %>%
      suppressWarnings() %>%
      suppressMessages()

    data <-
      data %>%
      mutate(
        urlDataRF =
          list(
            'http://rankandfiled.com/#/',
            slugFiler,
            '/',
            idCIKTicker,
            '/filings'
          ) %>% purrr::invoke(paste0, .)
      ) %>%
      select(-slugFiler)

    has_tickers <-
      data$nameEntity %>% stringr::str_count('\\[') %>% sum() > 0

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

    return(data)
  }

get_data_sec_entity <-
  function(entity_name = "Rockwood Capital",
           return_message = TRUE) {
    json_url <-
      entity_name %>%
      generate_search_name()
    parse_rf_search_name_safe <-
      possibly(parse_rf_search_name, data_frame())

    data <-
      json_url %>%
      parse_rf_search_name_safe() %>%
      mutate(nameEntitySearch = entity_name) %>%
      select(nameEntitySearch, matches("^name"), everything())

    if (data %>% nrow() == 0) {
      return(data_frame())
    }

    has_names <-
      data$nameEntity %>% str_detect(pattern = '\\|') %>% sum() > 0


    if (has_names) {
      data <-
        data %>%
        separate(nameEntity, into = c('nameEntity', 'namePerson'), sep = '\\|') %>%
        suppressMessages() %>%
        suppressWarnings()
    }

    if ('nameCompany' %in% names(data)) {
      data <-
        data %>%
        mutate(namePerson = ifelse(namePerson %>% is.na(), nameCompany, namePerson)) %>%
        select(-nameCompany) %>%
        dplyr::rename(nameCompany = namePerson)
    }


    if (return_message) {
      list(
        "Returned ",
        data %>% nrow(),
        ' SEC registered entities matching the name ',
        entity_name
      ) %>%
        purrr::invoke(paste0, .) %>% message()
    }

    return(data)
  }

#' SEC filing entity metdata
#' via Rank and Filed
#'
#' This function returns metadata for
#' any SEC filing entity that matching user
#' inputs
#'
#' @param entity_names vector of names to search
#' @param return_message return a message \code{TRUE, FALSE}
#'
#' @return a \code{data_frame}
#' @export
#' @import purrr dplyr stringr tidyr formattable
#' @importFrom jsonlite fromJSON
#' @family SEC
#' @family Rank and Filed
#' @family entity search
#' @examples
#' get_data_sec_filing_entities(entity_names = c('Rockwood Capital', 'Vornado', 'Two Sigma'))
get_data_sec_filing_entities <-
  function(entity_names = c('Rockwood Capital', 'Vornado', 'Two Sigma'),
           return_message = TRUE) {
    no_entry <-
      (entity_names %>% purrr::is_null() |
         !'entity_names' %>% exists())

    if (no_entry) {
      stop("Please enter a search name")
    }

    get_data_sec_entity_safe <-
      purrr::possibly(get_data_sec_entity, NULL)

    all_data <-
      entity_names %>%
      map_df(function(x) {
        get_data_sec_entity_safe(entity_name = x, return_message = return_message)
      })

    has_double_entities <-
      all_data$nameEntity %>%
      map_dbl(function(x) {
        x %>% str_count('\\|')
      }) %>%
      sum(na.rm = T) > 0

    if (has_double_entities) {
      all_data <-
        all_data %>%
        mutate(idRow = 1:n())

      entites_df <-
        1:nrow(all_data) %>%
        map_df(function(x) {
          entity <-
            all_data$nameEntity[[x]]

          has_double <-
            entity %>% str_detect("|")
          if (!has_double) {
            return(data_frame(idRow = x, nameEntity = entity))
          }
          entities <-
            entity %>%
            str_split('\\|') %>%
            flatten_chr()

          if (entities %>% length() == 2) {
            entities <-
              c(entities[[2]], entities[[1]])
          }

          entity_df <-
            data_frame(idRow = x,
                       item = 'nameEntity',
                       value = entities) %>%
            mutate(countItems = 1:n() - 1,
                   item = ifelse(countItems == 0, item, paste0(item, countItems))) %>%
            select(-countItems) %>%
            spread(item, value)

          return(entity_df)
        })

      all_data <-
        all_data %>%
        select(-nameEntity) %>%
        left_join(entites_df) %>%
        select(nameEntitySearch, matches("nameEntity"), everything()) %>%
        select(-idRow) %>%
        suppressMessages()
    }

    return(all_data)
  }

# form_ds -----------------------------------------------------------------

#' SEC Form-D's
#'
#' This function returns data for SEC Form D's by
#' specified industry
#'
#' \code{get_data_sec_form_ds()} queries all SEC filed form-d's since 2009 and returns the associated data.
#' the default parameters search every industry and year which you
#' can change by modifying the parameters
#'
#' @param industries industries to search options: \itemize{
#' \item \code{NULL: returns all industries(default)
#' \item \code{ENERGY}: energy sector
#' \item \code{FINANCIAL}: financial sector
#' \item \code{HEALTHCARE}: health-care sector
#' \item \code{OTHER}: all other sectors
#' \item \code{REAL ESTATE}: real estate sector
#' \item \code{TECHNOLOGY}: technology sector
#' \item \code{TRAVEL AND LEISURE}: travel and leisure sector
#' }
#' @param form_years years to search options are \itemize{
#' \item \code{NULL}: all years (default)
#' \item \code{2009-present}: search years
#' }
#' @param months months to search \itemize{
#' \item \code{NULL}: all months (default)
#' \code{1:12}: numeric month
#' @param return_message \code{TRUE} return a message after data import
#' @param nest_data \code{TRUE} return nested data frame
#' @import purrr dplyr stringr tidyr formattable lubridate
#' @importFrom jsonlite fromJSON
#' @return where \code{nest_data} is \code{TRUE} a nested data_frame by asset,
#' where \code{nest_data} is \code{FALSE} a data_frame

#' @export
#'
#' @examples
#' \dontrun{
#' get_data_sec_form_ds()
#' get_data_sec_form_ds(form_years = 2016:2017, industries = c("Real Estate", "Technology", "Other"))
#' }
get_data_sec_form_ds <-
  function(industries = NULL,
           form_years = NULL,
           months = NULL,
           nest_data = FALSE,
           return_message = TRUE) {
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
      tidyr::crossing(year = form_years, month = months_names) %>%
      tidyr::unite(namePeriod, year, month, sep = '', remove = FALSE) %>%
      mutate(
        idPeriod = 1:n(),
        codeMonth = month,
        month = month %>% as.numeric()
      )

    now <-
      period_df %>%
      filter(namePeriod == now_period) %>%
      .$idPeriod

    period_df <-
      period_df %>%
      filter(idPeriod <= now)

    if ('form_years' %>% exists() &
        !form_years %>% purrr::is_null()) {
      period_df <-
        period_df %>%
        filter(year %in% form_years)
    }

    if ('months' %>% exists() & !months %>% purrr::is_null()) {
      period_df <-
        period_df %>%
        filter(month %in% months)
    }

    periods <-
      period_df$namePeriod

    category_df <-
      get_dictionary_form_d_categories()

    if ('industries' %>% exists()) {
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
      if (industries %in% industry_names %>% sum(na.rm = T) == 0) {
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
      mutate(
        urlJSON = list(
          'http://rankandfiled.com/data/private_selection?month=',
          namePeriod,
          '&ind=',
          idCategory
        ) %>% purrr::invoke(paste0, .)
      ) %>%
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
      mutate_at(.vars = all_data %>% select(matches("^amount")) %>% names,
                funs(. %>% formattable::currency(digits = 0))) %>%
      mutate_at(.vars = all_data %>% select(matches("^count")) %>% names,
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

    if (nest_data) {
      all_data <-
        all_data %>%
        nest(-nameIndustryParent, .key = dataFormDs)
    }
    return(all_data)
  }




# filers ------------------------------------------------------------------
get_cik_url_df <-
  function(cik = 1138621) {
    slugs <-
      c(
        'general',
        'filings',
        'private',
        'fundraising',
        'traders',
        'clevel',
        'mda',
        'owners',
        'subsidiaries'
      )

    url_json <-
      list('http://rankandfiled.com/data/filer/', cik, '/', slugs) %>%
      purrr::invoke(paste0, .)

    url_df <-
      dplyr::data_frame(
        nameTable = c(
          'General',
          'Filings',
          'Private',
          'Fundraising',
          'Traders',
          'C Level',
          'MDA',
          'Owners',
          'Subsidiaries'
        ),
        urlJSON = url_json
      )
    return(url_df)
  }

parse_json_general_filing <-
  function(url = "http://rankandfiled.com/data/filer/1468327/general",
           nest_data = TRUE,
           return_message = TRUE) {
    if (!url %>% httr::url_ok() %>% suppressWarnings()) {
      return(data_frame())
    }

    data <-
      url %>%
      jsonlite::fromJSON(simplifyDataFrame = TRUE) %>%
      data.frame(stringsAsFactors = FALSE) %>%
      as_data_frame()

    is_company <-
      'company' %in% names(data)

    is_insider <-
      'insider' %in% names(data)

    is_fund <-
      'fund' %in% names(data)

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
      if (is_company) {
        ticker <-
          data$company

        company_name_df <-
          ticker %>%
          parse_company_general() %>%
          suppressWarnings()

        has_rows <-
          company_name_df %>% nrow > 0
        if (has_rows) {
          return(company_name_df)
        } else {
          entity_name <-
            NA
        }
      }
      if (is_insider) {
        insider_df <-
          parse_json_general_insider(cik = cik, return_message = return_message) %>%
          mutate(nameEntity = nameEntity %>% str_to_upper())
        return(insider_df)
      }
      if (is_fund) {
        fund_df <-
          parse_json_fund_general(cik = cik, return_message = return_message) %>%
          mutate(nameEntity = nameEntity %>% str_to_upper())
        return(fund_df)
      }
      data <-
        data %>%
        mutate(nameEntity = entity_name,
               idTicker = ticker) %>%
        select(-matches("company"))
    }

    data <-
      data %>%
      select(-matches("object")) %>%
      mutate_at(.vars = data %>% select(matches("idCIK|idIRS")) %>% names(),
                as.numeric) %>%
      mutate(urlJSONGeneral = url,
             nameEntity = nameEntity %>% stringr::str_to_upper())
    has_address <-
      names(data) %in% c('addressStreet1Entity',
                         'stateEntity',
                         'cityEntity',
                         'zipcodeEntity') %>% sum() == 4
    if (has_address) {
      data <-
        data %>%
        mutate(
          addressEntity = list(
            addressStreet1Entity,
            ' ',
            cityEntity,
            ' ',
            stateEntity,
            ', ',
            zipcodeEntity
          ) %>% purrr::invoke(paste0, .)
        ) %>%
        select(idCIK, matches("nameEntity"), addressEntity, everything())
    }

    if ('detailsOwnedBy' %in% names(data)) {
      data <-
        data %>%
        dplyr::rename(detailsOwns = detailsOwnedBy)
    }

    if ('detailsOwns' %in% names(data)) {
      detail_df <-
        1:length(data$detailsOwns) %>%
        map_df(function(x) {
          detail_value <-
            data$detailsOwns[[x]]

          if (detail_value %>% is.na()) {
            df <-
              data_frame(idRow = x, nameCompanyOwns = NA)

            if (nest_data) {
              df <-
                df %>%
                nest(-idRow, .key = dataCompaniesOwns)
            }

            return(df)
          }

          values <-
            detail_value %>% str_replace('\\|', '') %>%
            str_split('\\|') %>%
            flatten_chr()

          df_data <-
            data_frame(value = values) %>%
            tidyr::separate(value,
                            into = c('idTickerOwns', 'other'),
                            sep = '\\:') %>%
            tidyr::separate(other,
                            into = c('nameCompanyOwns', 'other'),
                            sep = '\\_') %>%
            tidyr::separate(other,
                            into = c('roleOwner', 'dateOwner'),
                            sep = '\\#') %>%
            mutate(nameCompanyOwns = nameCompanyOwns %>% str_to_upper(),
                   idRow = x) %>%
            gather(item, value, -idRow, na.rm = TRUE) %>%
            group_by(item) %>%
            mutate(count = 1:n() - 1) %>%
            ungroup() %>%
            arrange((count)) %>%
            mutate(item = ifelse(count == 0, item, paste0(item, count))) %>%
            select(-count)

          column_order <-
            c('idRow', df_data$item)

          df_data <-
            df_data %>%
            spread(item, value) %>%
            select(one_of(column_order))
        }) %>%
        suppressWarnings()

      detail_df <-
        detail_df %>%
        mutate_at(.vars = detail_df %>% select(matches("date")) %>% names(),
                  funs(. %>% ymd())) %>%
        suppressWarnings()

      if (nest_data) {
        detail_df <-
          detail_df %>%
          nest(-idRow, .key = dataCompaniesOwns)
      }

      data <-
        data %>%
        mutate(idRow = 1:n()) %>%
        select(-detailsOwns) %>%
        left_join(detail_df) %>%
        select(-idRow) %>%
        suppressMessages()
    }

    data <-
      data %>%
      select(
        nameEntity,
        idCIK,
        matches("typeCategory"),
        matches("idtypeCompany"),
        everything()
      )

    if (return_message) {
      list("Parsed: ", url) %>%
        purrr::invoke(paste0, .) %>% message()
    }

    return(data)

  }

parse_json_filings <-
  function(url = "http://rankandfiled.com/data/filer/1138621/filings",
           return_message = TRUE) {
    if (!url %>% httr::url_ok() %>% suppressWarnings()) {
      return(data_frame())
    }

    cik <-
      url %>% str_replace_all('http://rankandfiled.com/data/filer/|/filings', '') %>%
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
          'baseIndex',
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
        ),
        pageSlug = idSECSlug %>% str_replace_all('\\-',''),
        urlSECFilingDirectory = ifelse(
          idSECSlug %>% str_detect('\\-'),
          list(
            "https://www.sec.gov/Archives/edgar/data/",
            idCIK,
            '/',
            pageSlug,
            '/',
            idSECSlug,
            '-index.htm'
          ) %>% purrr::reduce(paste0),
          NA
        )
      ) %>%
      select(-matches("^X")) %>%
      suppressMessages() %>%
      select(-c(slugSEC, pageSlug)) %>%
      select(idCIK, dateFiling, everything())

    if (return_message) {
      list("Parsed: ", url) %>%
        purrr::invoke(paste0, .) %>% message()
    }

    return(json_data)
  }

parse_json_private <-
  function(url = "http://rankandfiled.com/data/filer/1438171/private",
           nest_data = TRUE,
           return_message = TRUE) {
    if (!url %>% httr::url_ok() %>% suppressWarnings()) {
      return(data_frame())
    }

    json_data <-
      url %>%
      jsonlite::fromJSON()

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
      resolve_name_df() %>%
      resolve_names_to_upper()

    if (offering_data %>% ncol >= 9) {
      offering_data <-
        offering_data %>%
        separate_column(column_name = 'idExemption') %>%
        separate_column(column_name = 'dateAmmended') %>%
        separate_column(column_name = 'amountFindersFee') %>%
        separate_column(column_name = 'countInvestors') %>%
        separate_column(column_name = 'countInvestorsNonAccredited') %>%
        separate_column(column_name = 'amountOffered') %>%
        separate_column(column_name = 'amountRemaining') %>%
        separate_column(column_name = 'amountSold')


      offering_data <-
        offering_data %>%
        mutate_at(.vars = offering_data %>% select(matches("^is")) %>% names,
                  funs(. %>% as.logical())) %>%
        mutate_at(.vars = offering_data %>% select(matches("^amount|^count|^idCIK")) %>% names,
                  funs(. %>% as.numeric())) %>%
        mutate_at(.vars = offering_data %>% select(matches("^date")) %>% names,
                  funs(. %>% lubridate::ymd())) %>%
        mutate_at(.vars = offering_data %>% select(matches("^amount")) %>% names,
                  funs(. %>% formattable::currency(digits = 0))) %>%
        mutate_at(.vars = offering_data %>% select(matches("^count")) %>% names,
                  funs(. %>% formattable::comma(digits = 0))) %>%
        suppressWarnings()
    } else {
      offering_data <-
        offering_data %>%
        mutate_at(.vars = offering_data %>% select(matches("^amount|^count|^idCIK")) %>% names,
                  funs(. %>% as.numeric())) %>%
        mutate_at(.vars = offering_data %>% select(matches("^date")) %>% names,
                  funs(. %>% lubridate::ymd()))
    }

    has_relations <-
      '_related_people' %in% names(json_data$offering_history)

    if (has_relations) {
      relation_df <-
        1:(json_data$offering_history$amended %>% length()) %>%
        map_df(function(x) {
          if (!json_data$offering_history$`_related_people`[[x]] %>% purrr::is_null()) {
            relation_data <-
              json_data$offering_history$`_related_people`[[x]] %>% mutate(
                name =
                  ifelse(
                    name %>% substr(1, 3) %>% str_detect('\\-'),
                    name %>% str_replace_all('\\-', '') %>% str_trim,
                    name %>% str_trim
                  )
              ) %>%
              tidyr::unite(nameRelation, name, relation, sep = '-') %>%
              .$nameRelation %>% paste0(collapse = '&')
          } else {
            relation_data <-
              NA
          }
          data_frame(nameRelation = relation_data)
        }) %>%
        resolve_names_to_upper()

      relation_df <-
        1:nrow(relation_df) %>%
        map_df(function(x) {
          person_title <-
            relation_df$nameRelation[[x]] %>%
            str_split('\\&') %>%
            flatten_chr() %>%
            str_to_upper() %>%
            str_trim()

          df <-
            data_frame(idRow = x, person_title) %>%
            tidyr::separate(
              person_title,
              sep = '\\-',
              into = c('nameRelatedParty', 'titleRelatedParty')
            ) %>%
            mutate(countItem = 1:n() - 1) %>%
            gather(item, value, -c(idRow, countItem)) %>%
            arrange(countItem)

          df <-
            df %>%
            mutate(item = ifelse(countItem == 0, item, item %>% paste0(countItem))) %>%
            select(-countItem)
          column_order <-
            c('idRow', df$item)

          df <-
            df %>%
            spread(item, value) %>%
            select(one_of(column_order))

          if (nest_data) {
            df <-
              df %>%
              nest(-idRow, .key = dataRelations)
          }
          return(df)
        })

      offering_data <-
        offering_data %>%
        mutate(idRow = 1:n()) %>%
        left_join(relation_df) %>%
        suppressMessages() %>%
        select(-idRow)
    }

    has_brokers <-
      '_brokers' %in% names(json_data$offering_history)

    if (has_brokers) {
      broker_df <-
        1:(json_data$offering_history$amended %>% length()) %>%
        map_df(function(x) {
          empty_value <-
            json_data$offering_history$`_brokers`[[x]] %>% is_null()
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
        }) %>%
        resolve_names_to_upper()

      broker_df <-
        1:nrow(broker_df) %>%
        map_df(function(x) {
          broker_crd <-
            broker_df$nameBrokerCRD[[x]] %>%
            str_split('\\|') %>%
            flatten_chr() %>%
            str_to_upper() %>%
            str_trim()

          if (broker_crd %>% is.na() %>% sum() > 0) {
            df <-
              data_frame(
                idRow = x,
                nameBroker = "NONE",
                idCRDBroker = NA
              )
            if (nest_data) {
              df <-
                df %>%
                nest(-idRow, .key = dataBrokers)
            }
            return(data_frame())
          }

          df <-
            data_frame(idRow = x, broker_crd) %>%
            tidyr::separate(broker_crd,
                            sep = '\\&',
                            into = c('nameBroker', 'idCRDBroker')) %>%
            mutate(countItem = 1:n() - 1) %>%
            gather(item, value, -c(idRow, countItem)) %>%
            arrange(countItem) %>%
            mutate(item = ifelse(countItem == 0, item, item %>% paste0(countItem))) %>%
            select(-countItem)

          column_order <-
            c('idRow', df$item)

          df <-
            df %>%
            spread(item, value) %>%
            select(one_of(column_order))

          df <-
            df %>%
            mutate_at(df %>% select(matches("idCRD")) %>% names(),
                      funs(. %>% as.numeric())) %>%
            resolve_names_to_upper()

          if (nest_data) {
            df <-
              df %>%
              nest(-idRow, .key = dataBrokers)
          }
          return(df)
        })

      offering_data <-
        offering_data %>%
        mutate(idRow = 1:n()) %>%
        left_join(broker_df) %>%
        suppressMessages() %>%
        select(-idRow)
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
        hasOver5FileFilings = has_five,
        urlJSONFilings = url
      ) %>%
      select(
        idCIK,
        dateInitialFiling,
        typeEntity,
        locationJurisdiction,
        hasOver5FileFilings,
        matches("nameIndustry"),
        matches("typeFund"),
        matches("^is"),
        matches("^amount"),
        matches("^count"),
        everything()
      ) %>%
      resolve_names_to_upper() %>%
      select(which(colMeans(is.na(.)) < 1))

    if (return_message) {
      list("Parsed: ", url) %>%
        purrr::invoke(paste0, .) %>% message()
    }
    return(offering_data)
  }

parse_json_fundraising <-
  function(url = "http://rankandfiled.com/data/filer/1138621/fundraising",
           nest_data = TRUE,
           return_message = TRUE) {
    if (!url %>% httr::url_ok() %>% suppressWarnings()) {
      return(data_frame())
    }

    json_data <-
      url %>%
      jsonlite::fromJSON()

    fundraising_df <-
      json_data$results %>%
      as_data_frame() %>%
      purrr::set_names(c(
        'idCIKs',
        'nameCompanies',
        'isCIKFiler',
        'namePerson',
        'offeringsValues'
      )) %>%
      mutate(
        idPerson = 1:n(),
        idCIK = url %>% str_replace_all('http://rankandfiled.com/data/filer/|/fundraising', '') %>% as.numeric(),
        namePerson = namePerson %>% str_replace_all('\\-', '') %>% stringr::str_to_upper() %>% str_trim(),
        urlJSONFundraising = url
      ) %>%
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

        df <-
          data_frame(value = company_name_data) %>%
          mutate(item = 'nameCompanyFundraisingRelated') %>%
          mutate(countRow = 1:n()) %>%
          mutate(
            countRow = countRow - 1,
            item = ifelse(countRow == 0, item, item %>% paste0(countRow)),
            idPerson = x
          ) %>%
          select(-countRow)

        col_order <-
          c('idPerson', df$item)

        df <-
          df %>%
          spread(item, value) %>%
          select(one_of(col_order)) %>%
          resolve_names_to_upper()

        if (nest_data) {
          df <-
            df %>%
            nest(-idPerson, .key = dataCompaniesRelated)
        }
        return(df)
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

        df <-
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
          select(-countRow)

        col_order <-
          c('idPerson', df$item)

        df <-
          df %>%
          spread(item, value) %>%
          select(one_of(col_order)) %>%
          resolve_names_to_upper()

        if (nest_data) {
          df <-
            df %>%
            nest(-idPerson, .key = dataOfferingValues)
        }
        return(df)
      })

    fundraising_df <-
      fundraising_df %>%
      left_join(company_name_df) %>%
      left_join(offering_value_df) %>%
      select(-c(idCIKs, nameCompanies, idPerson, offeringsValues)) %>%
      select(idCIK, namePerson, isCIKFiler, everything()) %>%
      suppressMessages() %>%
      suppressWarnings()

    if (return_message) {
      list("Parsed: ", url) %>%
        purrr::invoke(paste0, .) %>% message()
    }
    return(fundraising_df)
  }

parse_json_traders <-
  function(url = "http://rankandfiled.com/data/filer/1326801/traders",
           return_message = TRUE) {
    if (!url %>% httr::url_ok() %>% suppressWarnings()) {
      return(data_frame())
    }

    json_data <-
      url %>%
      jsonlite::fromJSON()

    options(scipen = 9999)
    cik <-
      url %>% str_replace_all('http://rankandfiled.com/data/filer/|/traders', '') %>%
      as.numeric()
    traders <-
      json_data$owners$count

    df <-
      json_data$owners$owners %>%
      as_data_frame() %>%
      purrr::set_names(c('nameEntityTrader', 'idCIKTrader', 'titleEntityTrader')) %>%
      mutate(
        nameEntityTrader = nameEntityTrader %>% str_to_upper(),
        idCIKTrader = idCIKTrader %>% as.numeric(),
        idCIK = cik
      ) %>%
      select(idCIK, everything()) %>%
      mutate(countTraders = traders) %>%
      resolve_names_to_upper()

    if (return_message) {
      list("Parsed: ", url) %>%
        purrr::invoke(paste0, .) %>% message()
    }

    return(df)
  }

parse_json_clevel <-
  function(url = "http://rankandfiled.com/data/filer/1326801/clevel",
           return_message = TRUE) {
    if (!url %>% httr::url_ok() %>% suppressWarnings()) {
      return(data_frame())
    }

    json_data <-
      url %>%
      jsonlite::fromJSON()

    options(scipen = 9999)
    cik <-
      url %>% str_replace_all('http://rankandfiled.com/data/filer/|/clevel', '') %>%
      as.numeric()

    clevel_df <-
      json_data$clevel %>%
      as_data_frame() %>%
      tidyr::separate(
        value,
        into = c(
          "idCIKCSuite",
          "nameEntityCSuite",
          "dateStartCSuite",
          "dateEndCSuite",
          "nameCSuiteRole",
          'codeCSuiteRole'
        ),
        sep = '\\*'
      ) %>%
      mutate(
        idCIKCSuite = idCIKCSuite %>% as.numeric(),
        nameEntityCSuite = nameEntityCSuite %>% str_to_upper(),
        idCIK = cik,
        dateStartCSuite = dateStartCSuite %>% lubridate::ymd(),
        dateEndCSuite = dateEndCSuite %>% lubridate::ymd()
      ) %>%
      select(idCIK,
             idCIKCSuite,
             nameEntityCSuite,
             codeCSuiteRole,
             everything()) %>%
      mutate(isActiveCSuite = ifelse(dateEndCSuite %>% is.na(), TRUE, FALSE)) %>%
      resolve_names_to_upper()

    if (return_message) {
      list("Parsed: ", url) %>%
        purrr::invoke(paste0, .) %>% message()
    }

    return(clevel_df)
  }

parse_json_mda <-
  function(url = "http://rankandfiled.com/data/filer/1326801/mda",
           return_message = TRUE) {
    if (!url %>% httr::url_ok() %>% suppressWarnings()) {
      return(data_frame())
    }

    json_data <-
      url %>%
      jsonlite::fromJSON()

    options(scipen = 9999)

    cik <-
      url %>% str_replace_all('http://rankandfiled.com/data/filer/|/mda', '') %>%
      as.numeric()

    data <-
      json_data$results$matrix %>%
      as_data_frame()

    names(data) <-
      json_data$results$dates %>% lubridate::ymd()
    words <-
      json_data$results$words

    data <-
      data %>%
      mutate(nameWord = words) %>%
      gather(date10K, countWord, -nameWord) %>%
      mutate(date10K = date10K %>% lubridate::ymd(),
             idCIK = cik) %>%
      select(idCIK, date10K, nameWord, countWord) %>%
      arrange(desc(date10K)) %>%
      resolve_names_to_upper()

    if (return_message) {
      list("Parsed: ", url) %>%
        purrr::invoke(paste0, .) %>% message()
    }

    return(data)
  }

parse_json_owners <-
  function(url = "http://rankandfiled.com/data/filer/1326801/owners",
           nest_data = TRUE,
           return_message = TRUE) {
    if (!url %>% httr::url_ok() %>% suppressWarnings()) {
      return(data_frame())
    }

    json_data <-
      url %>%
      jsonlite::fromJSON()

    cik <-
      url %>% str_replace_all('http://rankandfiled.com/data/filer/|/owners', '') %>%
      as.numeric()

    general_df <-
      data_frame(idCIK = cik,
                 idCIKOwned = json_data$insiders$cik %>% as.numeric())

    has_filer <-
      'filer' %in% names(json_data$insiders)

    if (has_filer) {
      filing_df <-
        json_data$insiders$filer %>%
        as_data_frame()

      filing_df <-
        filing_df %>%
        resolve_name_df()

      filing_df <-
        filing_df %>%
        mutate_at(.vars = filing_df %>% select(matches("nameEntity")) %>% names(),
                  funs(. %>% str_to_upper())) %>%
        resolve_names_to_upper()

      if ('name' %in% names(filing_df)) {
        filing_df <-
          filing_df %>%
          select(-name)
      }

      if ('sic' %in% names(filing_df)) {
        filing_df <-
          filing_df %>%
          dplyr::rename(idSIC = sic) %>%
          mutate(idSIC = idSIC %>% as.numeric())
      }

      names(filing_df) <-
        names(filing_df) %>% str_replace('OwnedBy', '') %>%
        paste0('Owner')
      if ('detailsOwner' %in% names(filing_df)) {
        detail_df <-
          1:length(filing_df$detailsOwner) %>%
          map_df(function(x) {
            detail_value <-
              filing_df$detailsOwner[[x]]

            if (detail_value %>% is.na()) {
              df <-
                data_frame(idRow = x, nameCompanyOwned = NA)

              if (nest_data) {
                df <-
                  df %>%
                  nest(-idRow, .key = dataCompaniesOwned)
              }
              return(df)
            }

            values <-
              detail_value %>% str_replace('\\|', '') %>%
              str_split('\\|') %>%
              flatten_chr()

            df_data <-
              data_frame(value = values) %>%
              tidyr::separate(value,
                              into = c('idTickerOwned', 'other'),
                              sep = '\\:') %>%
              tidyr::separate(other,
                              into = c('nameCompanyOwned', 'other'),
                              sep = '\\_') %>%
              tidyr::separate(other,
                              into = c('roleOwned', 'dateOwned'),
                              sep = '\\#') %>%
              mutate(nameCompanyOwned = nameCompanyOwned %>% str_to_upper(),
                     idRow = x) %>%
              gather(item, value, -idRow, na.rm = TRUE) %>%
              group_by(item) %>%
              mutate(count = 1:n() - 1) %>%
              ungroup() %>%
              arrange((count)) %>%
              mutate(item = ifelse(count == 0, item, paste0(item, count))) %>%
              select(-count)

            column_order <-
              c('idRow', df_data$item)

            df_data <-
              df_data %>%
              spread(item, value) %>%
              select(one_of(column_order)) %>%
              resolve_names_to_upper()

            if (nest_data) {
              df_data <-
                df_data %>%
                nest(-idRow, .key = dataCompaniesOwned)
            }

            return(df_data)
          }) %>%
          suppressWarnings()

        detail_df <-
          detail_df %>%
          mutate_at(.vars = detail_df %>% select(matches("date")) %>% names(),
                    funs(. %>% ymd())) %>%
          suppressWarnings()

        filing_df <-
          filing_df %>%
          mutate(idRow = 1:n()) %>%
          select(-detailsOwner) %>%
          left_join(detail_df) %>%
          select(-idRow) %>%
          suppressMessages()
      }

      general_df <-
        general_df %>%
        bind_cols(filing_df)
    }

    has_companies <-
      'companies' %in% names(json_data$insiders)

    if (has_companies) {
      company_df <-
        1:nrow(general_df) %>%
        map_df(function(x) {
          has_no_data <-
            json_data$insiders$companies[[x]] %>%
            nrow() == 0

          if (has_no_data) {
            df <-
              data_frame(idRow = x, nameFiler = NA)
            if (nest_data) {
              df <-
                df %>%
                nest(idRow, .key = dataInsiderCompaniesOwned)
            }
          }

          company_df <-
            json_data$insiders$companies[[x]] %>%
            as_data_frame() %>%
            resolve_name_df() %>%
            mutate(idRow = x) %>%
            mutate(nameFiler = nameFiler %>% str_to_upper())

          if ('sic' %in% names(company_df)) {
            company_df <-
              company_df %>%
              dplyr::rename(idSICCompanyOwned = sic) %>%
              mutate(idSICCompanyOwned = idSICCompanyOwned %>% as.numeric())
          }

          df_data <-
            company_df %>%
            gather(item, value, -c(nameFiler, idRow)) %>%
            group_by(item) %>%
            mutate(count = 1:n() - 1) %>%
            ungroup() %>%
            arrange((count)) %>%
            mutate(item = ifelse(count == 0, item, paste0(item, count))) %>%
            select(-count)

          column_order <-
            c('idRow', 'nameFiler', df_data$item)

          df_data <-
            df_data %>%
            spread(item, value) %>%
            select(one_of(column_order)) %>%
            resolve_names_to_upper()

          if (nest_data) {
            df_data <-
              df_data %>%
              nest(-idRow, .key = dataInsiderCompaniesOwned)
          }
          return(df_data)
        })

      company_df <-
        company_df %>%
        mutate_at(.vars =
                    company_df %>% select(matches("date")) %>% names(),
                  funs(. %>% lubridate::ymd())) %>%
        mutate_at(.vars =
                    company_df %>% select(matches("idCIK")) %>% names(),
                  .funs = as.numeric) %>%
        mutate_at(
          .vars =
            company_df %>% select(matches("nameCompany")) %>% names(),
          .funs = stringr::str_to_upper
        )

      general_df <-
        general_df %>%
        mutate(idRow = 1:n()) %>%
        left_join(company_df %>% select(-matches("idCIKOwned"))) %>%
        select(-idRow) %>%
        suppressMessages()
    }

    if (return_message) {
      list("Parsed: ", url) %>%
        purrr::invoke(paste0, .) %>% message()
    }

    general_df <-
      general_df %>%
      select(idCIK,
             idCIKOwned,
             nameEntityOwner,
             matches("nameFiler"),
             everything()) %>%
      resolve_names_to_upper()

    return(general_df)
  }

parse_json_public_filers <-
  function(url = "http://rankandfiled.com/data/filer/1680780/all?start=0",
           return_message = TRUE) {
    if (!url %>% httr::url_ok() %>% suppressWarnings()) {
      return(data_frame())
    }

    json_data <-
      url %>%
      jsonlite::fromJSON()

    options(scipen = 9999)

    cik <-
      url %>% str_replace_all('http://rankandfiled.com/data/filer/|/all', '') %>%
      str_split('\\?') %>%
      flatten_chr() %>%
      .[[1]] %>%
      as.numeric()

    filing_df <-
      json_data$filings %>%
      as_data_frame()

    filing_df <-
      filing_df %>%
      separate(
        value,
        into = c(
          "idRF",
          "idForm",
          "detailForm",
          "typeReport",
          "typeFiling",
          "slugSEC",
          "idSECSlug",
          "dateFiling",
          "X9"
        ),
        sep = '\\*'
      ) %>%
      select(-matches("X")) %>%
      suppressMessages() %>%
      suppressWarnings()

    filing_df <-
      filing_df %>%
      mutate(
        idCIK = cik,
        pageSlug = idSECSlug %>% str_replace_all('\\-',''),
        urlSECFilingDirectory = ifelse(
          idSECSlug %>% str_detect('\\-'),
          list(
            "https://www.sec.gov/Archives/edgar/data/",
            idCIK,
            '/',
            pageSlug,
            '/',
            idSECSlug,
            '-index.htm'
          ) %>% purrr::reduce(paste0),
          NA
        ),
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
      select(-pageSlug) %>%
      suppressWarnings()

    filing_df <-
      filing_df %>%
      mutate(
        typeFiling = typeFiling %>% str_to_upper(),
        dateFiling = dateFiling %>% as.numeric() %>% lubridate::ymd(),
        detailForm = ifelse(detailForm == '', NA, detailForm),
        typeReport = ifelse(typeReport == '', NA, typeReport),
        is13FFiling = (urlSEC %>% str_detect("xslForm13F")) &
          (typeFiling == "HOLDINGS")
      ) %>%
      tidyr::fill(dateFiling) %>%
      tidyr::fill(detailForm) %>%
      select(-slugSEC) %>%
      left_join(get_dictionary_sec_form_codes()) %>%
      tidyr::fill(nameForm) %>%
      select(idCIK, idRF, idForm, nameForm, everything()) %>%
      suppressMessages() %>%
      suppressWarnings() %>%
      resolve_names_to_upper()



    if (return_message) {
      list("Parsed: ", url) %>%
        purrr::invoke(paste0, .) %>% message()
    }
    return(filing_df)
  }

parse_json_subsidiaries <-
  function(url = "http://rankandfiled.com/data/filer/34088/subsidiaries",
           return_message = TRUE) {
    if (!url %>% httr::url_ok() %>% suppressWarnings()) {
      return(data_frame())
    }
    options(scipen = 9999)

    name_df <-
      data_frame(
        nameRF = c(
          "cik",
          "country",
          "first_filed",
          "last_filed",
          "name",
          "percent"
        ),
        nameActual = c(
          'idCIK',
          'locationOrganizationSubsidiary',
          'dateFirstFiled',
          'dateLastFiled',
          'nameSubsidiary',
          'pctSubsidiaryOwned'
        )
      ) %>%
      mutate(idRow = 1:n())

    data <-
      url %>%
      jsonlite::fromJSON() %>%
      .$subsidiaries %>%
      as_data_frame()

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
        map_chr(function(x) {
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

      data <-
        data %>%
        mutate_at(.vars =
                    data %>% select(
                      matches(
                        "idCIK|idMidas|idIRS|^count|^price|^amount|^ratio|^pct|idMDA|^dateiso|idRF|price|amount|^year"
                      )
                    ) %>% names,
                  funs(. %>% readr::parse_number())) %>%
        suppressWarnings()
      return(data)
    }

    actual_names <-
      names(data) %>%
      map_chr(function(x) {
        name_df %>%
          filter(nameRF == x) %>%
          filter(idRow == min(idRow)) %>%
          .$nameActual
      })

    data <-
      data %>%
      purrr::set_names(actual_names)

    data <-
      data %>%
      mutate(
        idCIK = idCIK %>% as.numeric(),
        nameSubsidiary = nameSubsidiary %>% str_to_upper(),
        locationOrganizationSubsidiary = locationOrganizationSubsidiary %>% str_to_upper()
      )
    has_pct <-
      'pctSubsidiaryOwned' %in% names(data)
    if (has_pct) {
      data <-
        data %>%
        mutate(
          pctSubsidiaryOwned = pctSubsidiaryOwned %>% as.numeric(),
          pctSubsidiaryOwned = pctSubsidiaryOwned / 100
        )
    }

    data <-
      data %>%
      mutate_at(.vars = data %>% select(matches("date")) %>% names(),
                funs(. %>% lubridate::ymd()))

    data <-
      data %>%
      filter(!locationOrganizationSubsidiary %>% is.na()) %>%
      resolve_names_to_upper()

    if (return_message) {
      list("Parsed: ", url) %>%
        purrr::invoke(paste0, .) %>% message()
    }
    return(data)
  }

parse_cik_filings <-
  function(cik = 1527559,
           return_message = TRUE) {
    general_url <-
      list('http://rankandfiled.com/data/filer/', cik, '/general') %>%
      purrr::invoke(paste0, .)

    data_js <-
      general_url %>% jsonlite::fromJSON() %>% data.frame(stringsAsFactors = FALSE)

    is_public_company <-
      'company' %in% (data_js %>% names())

    is_insider <-
      'insider' %in% (data_js %>% names())
    if (is_public_company) {
      company_df <-
        general_url %>% jsonlite::fromJSON() %>% data.frame(stringsAsFactors = FALSE) %>%
        as_data_frame()

      general_df <-
        parse_company_general(ticker = company_df$company)
    }

    if (is_insider) {
      general_df <-
        parse_json_general_insider(cik = cik)
    }

    is_private_filer <-
      (!is_public_company) & (!is_insider)
    if (is_private_filer) {
      general_df <-
        general_url %>%
        parse_json_general_filing()
    }

    filing_pages <-
      general_df$countFilings %/% 50
    if (filing_pages > 0) {
      filing_urls <-
        list(
          'http://rankandfiled.com/data/filer/',
          cik,
          '/all?start=',
          seq(0, by = 50, length.out = filing_pages)
        ) %>%
        purrr::invoke(paste0, .)
    }

    if (filing_pages == 0) {
      filing_urls <-
        list('http://rankandfiled.com/data/filer/',
             cik,
             '/all?start=0') %>%
        purrr::invoke(paste0, .)
    }

    parse_json_public_filers_safe <-
      purrr::possibly(parse_json_public_filers, NULL)

    all_filings <-
      filing_urls %>%
      map_df(function(x) {
        parse_json_public_filers_safe(url = x, return_message = return_message)
      }) %>%
      distinct() %>%
      suppressWarnings()

    entity <-
      general_df$nameEntity %>%
      str_to_upper()

    all_filings <-
      all_filings %>%
      mutate(nameEntity = entity) %>%
      select(idCIK, nameEntity, dateFiling,
             matches("idRF"),
             everything())

    if ('typeReport' %in% names(all_filings)) {
      report_dict_df <-
        get_dictionary_sec_filing_codes()

      report_df <-
        all_filings %>%
        mutate(idRow = 1:n()) %>%
        select(typeReport, idRow) %>%
        filter(!typeReport %>% is.na())

      report_df <-
        1:nrow(report_df) %>%
        map_df(function(x) {
          is_none <-
            report_df$typeReport[[x]] == 'None'

          if (is_none) {
            return(data_frame(
              idRow = report_df$idRow[[x]],
              idFormType = 'None',
              nameFormType = NA
            ))
          }

          row_df <-
            report_df %>%
            slice(x)
          reports <-
            row_df$typeReport %>%
            str_split('\\|') %>%
            flatten_chr()

          item_df <-
            data_frame(idFormType = reports, idRow = row_df$idRow) %>%
            left_join(report_dict_df) %>%
            gather(item, value, -idRow) %>%
            group_by(item) %>%
            mutate(countItems = 1:n() - 1) %>%
            ungroup() %>%
            mutate(item = ifelse(countItems == 0, item, paste0(item, countItems))) %>%
            arrange(countItems) %>%
            select(-countItems) %>%
            suppressMessages()

          col_order <-
            c('idRow', item_df$item)

          item_df <-
            item_df %>%
            spread(item, value) %>%
            select(one_of(col_order))
          return(item_df)

        })

      all_filings <-
        all_filings %>%
        mutate(idRow = 1:n()) %>%
        dplyr::rename(typesReport = typeReport) %>%
        left_join(report_df) %>%
        suppressMessages() %>%
        select(-idRow)
    }


    if (return_message) {
      list(
        "Parsed ",
        all_filings %>% nrow() %>% formattable::comma(digits = 0),
        ' SEC Filings for ',
        entity
      ) %>%
        purrr::invoke(paste0, .) %>%
        message()
    }
    all_filings <-
      all_filings %>%
      resolve_names_to_upper()
    return(all_filings)
  }

parse_cik_data <-
  function(cik = 1326801,
           nest_data = TRUE,
           tables = NULL,
           return_message = TRUE) {
    url_df <-
      cik %>%
      get_cik_url_df()

    table_options <-
      c(
        'General',
        'CIK Filings',
        'Filings',
        'Private Offerings',
        'Related Parties',
        'Traders',
        'C Level',
        'MDA',
        'Owners',
        'Insider Trades',
        'Trades',
        'Subsidiaries'
      )

    null_tables <-
      tables %>% purrr::is_null()
    if (null_tables) {
      tables <-
        c(
          'General',
          'CIK Filings',
          'Filings',
          'Private Offerings',
          'Related Parties',
          'Traders',
          'C Level',
          'MDA',
          'Owners',
          'Insider Trades',
          'Trades',
          'Subsidiaries'
        )
    }
    missing_tables <-
      (tables %>% str_to_upper()) %in% (table_options %>% str_to_upper()) %>% sum() == 0
    if (missing_tables) {
      stop(list(
        "Sorry Tables Can Only Be:",
        '\n',
        paste0(table_options, collapse = '\n')
      ) %>%
        purrr::invoke(paste0, .))
    }

    table_options <-
      table_options %>% str_to_upper()

    tables <-
      tables %>% str_to_upper()

    if (!'GENERAL' %in% tables) {
      tables <-
        tables %>%
        append('GENERAL')
    }

    has_general <-
      'general' %>% str_to_upper() %>% str_detect(tables) %>% sum() > 0

    has_filings <-
      'filings' %>% str_to_upper() %>% str_detect(tables) %>% sum() > 0

    has_cik_filings <-
      'cik filings' %>% str_to_upper() %>% str_detect(tables) %>% sum() > 0

    has_private <-
      'private offerings' %>% str_to_upper() %>% str_detect(tables) %>% sum() > 0

    has_related <-
      'related parties' %>% str_to_upper() %>% str_detect(tables) %>% sum() > 0

    has_traders <-
      'traders' %>% str_to_upper() %>% str_detect(tables) %>% sum() > 0

    has_clevel <-
      'c level' %>% str_to_upper() %>% str_detect(tables) %>% sum() > 0

    has_mda <-
      'mda' %>% str_to_upper() %>% str_detect(tables) %>% sum() > 0

    has_owners <-
      'owners' %>% str_to_upper() %>% str_detect(tables) %>% sum() > 0

    has_insider_trades <-
      'insider trades' %>% str_to_upper() %>% str_detect(tables) %>% sum() > 0

    has_subs <-
      'subsidiaries' %>% str_to_upper() %>% str_detect(tables) %>% sum() > 0

    if (has_general) {
      parse_json_general_filing_safe <-
        purrr::possibly(parse_json_general_filing, data_frame())
      general_df <-
        url_df$urlJSON[[1]] %>%
        parse_json_general_filing(nest_data = nest_data,
                                       return_message = return_message) %>%
        mutate(nameEntity = nameEntity %>% str_to_upper())

      if (general_df %>% nrow() == 0) {
        general_df <-
          data_frame(idCIK = cik,
                     nameEntity = NA)
      }
    } else {
      general_df <-
        data_frame(idCIK = cik)
    }


    if (has_filings) {
      parse_json_filings_safe <-
        purrr::possibly(parse_json_filings, data_frame())

      filing_df <-
        url_df$urlJSON[[2]] %>%
        parse_json_filings_safe(return_message = return_message) %>%
        mutate_if(is_character,
                  str_to_upper)

      has_rows  <-
        filing_df %>% nrow() > 0

      if (has_rows) {
        filing_df <-
          filing_df %>%
          left_join(general_df %>% select(idCIK, nameEntity)) %>%
          select(nameEntity, idCIK, everything()) %>%
          suppressMessages()
      }
    } else {
      filing_df <-
        data_frame(idCIK = cik)
    }


    if (has_private) {
      parse_json_private_safe <-
        purrr::possibly(parse_json_private, data_frame())

      private_df <-
        url_df$urlJSON[[3]] %>%
        parse_json_private_safe(nest_data = nest_data,
                                return_message = return_message)

      has_rows  <-
        private_df %>% nrow() > 0

      if (has_rows) {
        private_df <-
          private_df %>%
          left_join(general_df %>% select(idCIK, nameEntity)) %>%
          select(nameEntity, idCIK, everything()) %>%
          suppressMessages()
      }
    } else {
      private_df <-
        data_frame(idCIK = cik)
    }


    if (has_related) {
      parse_json_fundraising_safe <-
        purrr::possibly(parse_json_fundraising, data_frame())

      fundraising_df <-
        url_df$urlJSON[[4]] %>%
        parse_json_fundraising_safe(nest_data = nest_data,
                                    return_message = return_message)

      has_rows  <-
        fundraising_df %>% nrow() > 0
      if (has_rows) {
        fundraising_df <-
          fundraising_df %>%
          left_join(general_df %>% select(idCIK, nameEntity)) %>%
          select(nameEntity, idCIK, everything()) %>%
          suppressMessages()
      }
    } else {
      fundraising_df <-
        data_frame(idCIK = cik)
    }

    if (has_traders) {
      parse_json_traders_safe <-
        purrr::possibly(parse_json_traders, data_frame())

      traders_df <-
        url_df$urlJSON[[5]] %>%
        parse_json_traders_safe(return_message = return_message)

      has_rows  <-
        traders_df %>% nrow() > 0
      if (has_rows) {
        traders_df <-
          traders_df %>%
          left_join(general_df %>% select(idCIK, nameEntity)) %>%
          select(nameEntity, idCIK, everything()) %>%
          suppressMessages()
      }
    } else {
      traders_df <-
        data_frame(idCIK = cik)
    }


    if (has_clevel) {
      parse_json_clevel_safe <-
        purrr::possibly(parse_json_clevel, data_frame())

      clevel_df <-
        url_df$urlJSON[[6]] %>%
        parse_json_clevel_safe(return_message = return_message)

      has_rows  <-
        clevel_df %>% nrow() > 0
      if (has_rows) {
        clevel_df <-
          clevel_df %>%
          left_join(general_df %>% select(idCIK, nameEntity)) %>%
          select(nameEntity, idCIK, everything()) %>%
          suppressMessages()
      }
    } else {
      clevel_df <-
        data_frame(idCIK = cik)
    }

    if (has_mda) {
      parse_json_mda_safe <-
        purrr::possibly(parse_json_mda, data_frame())

      mda_df <-
        url_df$urlJSON[[7]] %>%
        parse_json_mda_safe(return_message = return_message)

      has_rows  <-
        mda_df %>% nrow() > 0

      if (has_rows) {
        mda_df <-
          mda_df %>%
          left_join(general_df %>% select(idCIK, nameEntity)) %>%
          select(nameEntity, idCIK, everything()) %>%
          suppressMessages()
      }
    } else {
      mda_df <-
        data_frame(idCIK = cik)
    }

    if (has_owners) {
      parse_json_owners_safe <-
        purrr::possibly(parse_json_owners, data_frame())

      owners_df <-
        url_df$urlJSON[[8]] %>%
        parse_json_owners_safe(nest_data = nest_data,
                               return_message = return_message)

      if ('idTypeFilerOwner' %in% names(owners_df)) {
        owners_df <-
          owners_df %>%
          left_join(get_filer_type_df()) %>%
          select(idCIK:nameEntityOwner, typeFilerOwner, everything()) %>%
          suppressMessages()
      }

      has_rows  <-
        owners_df %>% nrow() > 0
      if (has_rows) {
        owners_df <-
          owners_df %>%
          left_join(general_df %>% select(idCIK, nameEntity)) %>%
          select(nameEntity, idCIK, everything()) %>%
          select(-matches("dateiso")) %>%
          suppressMessages()
      }
    } else {
      owners_df <-
        data_frame(idCIK = cik)
    }

    if (has_cik_filings) {
      parse_cik_filings_safe <-
        purrr::possibly(parse_cik_filings, data_frame())

      cik_filing_df <-
        parse_cik_filings_safe(cik = cik, return_message = return_message)
    } else {
      cik_filing_df <-
        data_frame(idCIK = cik)
    }

    if (has_insider_trades) {
      parse_insider_trades_safe <-
        purrr::possibly(parse_insider_trades, data_frame())

      insider_trade_df <-
        parse_insider_trades_safe(cik = cik,
                                  nest_data = nest_data,
                                  return_message = return_message)

      has_rows  <-
        insider_trade_df %>% nrow() > 0

      if (has_rows) {
        insider_trade_df <-
          insider_trade_df %>%
          left_join(general_df %>% select(idCIK, nameEntity)) %>%
          select(nameEntity, idCIK, everything()) %>%
          suppressMessages()
      }
    } else {
      insider_trade_df <-
        data_frame(idCIK = cik)
    }

    if (has_subs) {
      parse_json_subsidiaries_safe <-
        purrr::possibly(parse_json_subsidiaries, data_frame())

      sub_df <-
        url_df$urlJSON[[9]] %>%
        parse_json_subsidiaries(return_message = return_message)

      has_rows  <-
        sub_df %>% nrow() > 0
      if (has_rows) {
        sub_df <-
          sub_df %>%
          left_join(general_df %>% select(idCIK, nameEntity)) %>%
          select(nameEntity, idCIK, everything()) %>%
          suppressMessages()
      }
    } else {
      sub_df <-
        data_frame(idCIK = cik)
    }

    if ('nameEntity' %in% names(general_df)) {
      nameEntity <-
        general_df$nameEntity %>%
        str_to_upper()
    } else {
      nameEntity <-
        NA
    }


    all_data <-
      data_frame(
        idCIK = cik,
        nameEntity,
        nameTable = c(
          'General',
          'CIK Filings',
          'Filings',
          'Private Offerings',
          'Related Parties',
          'Traders',
          'C Level',
          'MDA',
          'Owners',
          'Insider Trades',
          'Subsidiaries'
        ),
        dataTable = list(
          general_df,
          cik_filing_df,
          filing_df,
          private_df,
          fundraising_df,
          traders_df,
          clevel_df,
          mda_df,
          owners_df,
          insider_trade_df,
          sub_df
        )
      )

    if (return_message) {
      list("\nParsed SEC Private Filing Data for CIK: ",
           cik,
           ' - ',
           nameEntity,
           "\n") %>%
        purrr::invoke(paste0, .) %>%
        message()
    }

    all_data <-
      all_data %>%
      mutate(countCols = dataTable %>% purrr::map_dbl(ncol)) %>%
      filter(countCols > 1) %>%
      suppressWarnings() %>%
      select(-matches("countCols"))

    return(all_data)
  }

#' SEC filer
#'
#' This is function imports data
#' for a specified SEC filing entity.  An
#' SEC filing entity can be a person, public company or
#' private filer.  This function requires that the entity has a
#' Central Index Key [CIK].
#'
#' The function acquires information for the
#' specified tables and will auto parse forms if the filer has them and
#' the user activates these parameters.
#'
#' @param entity_names vector names to search
#' @param tickers character vector of ticker symbols to search
#' @param ciks numeric vector of CIKs
#' @param tables tables to include if they exist \itemize{
#' \code{NULL, General, CIK Filings, Filings, Private Offerings, Related Parties, Traders, C Level, MDA, Owners, Insider Trades, Trades}
#' \code{NULL}: selects all tables
#' \item \code{General}: general information about the filer
#' \item \code{CIK Filings}: summarised filings for a CIK
#' \item \code{Filings}: summarised filings for an entity, slightly different than \code{CIK Filings}
#' \item \code{Private Offerings}: parses any private offerings
#' \item \code{Related Parties}:  parses any related parties [people]
#' \item \code{Traders}: parses major traders
#' \item \code{C Level}: parses information about executives
#' \item \code{MDA}: parses text from company 10-K Management Discussion and Analysis [MDA] section
#' \item \code{Owners}: parses information about major owners
#' \item \code{Insider Trades}: parses insider trade information
#' \item \code{Trades}: parses all trade information
#' }
#' @param parse_all_filing_url_data \code{TRUE} parses every SEC fling link
#' @param parse_xbrl \code{TRUE} parse XBRL for public companies, data starts in 2009
#' @param parse_subsidiaries \code{TRUE} parse all filer subsidiaries (default)
#' @param parse_13F \code{TRUE} parse \href{https://en.wikipedia.org/wiki/Form_13F}{13F's} for institutional managers
#' @param parse_asset_files \code{TRUE} parses ABS XML for \href{https://www.sec.gov/info/edgar/specifications/absxml.htm}{ABS Asset Data}
#' filing entities (default)
#' @param parse_small_offerings \code{TRUE} parses \href{https://www.sec.gov/info/smallbus/secg/rccomplianceguide-051316.htm}{Regulation CrowdFunding}
#' Form 1-A data if any exists for a filer (default)
#' @param nest_data return a nested data frame \code{TRUE, FALSE}
#' @param assign_to_environment \code{true} assigns individual data frames to your environment
#' @param return_message \code{TRUE} return a message after data import
#' @import dplyr tidyr purrr stringr formattable readr lubridate XBRL curl jsonlite lazyeval
#' @importFrom jsonlite fromJSON
#' @export
#' @return where \code{nest_data} is \code{TRUE} a nested data_frame by asset,
#' where \code{nest_data} is \code{FALSE} a data_frame
#' @family SEC
#' @family Rank and Filed
#' @family XBRL
#' @family entity search
#' @family fund search
#' @export
#'
#' @examples
#' \dontrun{
#' get_data_sec_filer(entity_names = 'HLT Holdco', tickers = c('FB'),
#' nest_data = TRUE, parse_subsidiaries = TRUE, parse_all_filing_url_data = TRUE,
#' parse_13F = TRUE, assign_to_environment = TRUE,
#' return_message = TRUE)
#'
#' ## Small Asset Filer Example
#'
#' ## ABS Example
#'
#' #XBRL Example
#'
#'}
get_data_sec_filer <-
  function(entity_names = NULL,
           tickers = NULL,
           ciks = NULL,
           tables = NULL,
           nest_data = FALSE,
           parse_all_filing_url_data = FALSE,
           parse_xbrl = FALSE,
           parse_subsidiaries = FALSE,
           parse_13F = FALSE,
           parse_asset_files = FALSE,
           parse_small_offerings = FALSE,
           parse_complete_text_filings = FALSE,
           parse_form_d = FALSE,
           parse_form_3_4s = FALSE,
           assign_to_environment = TRUE,
           return_message = TRUE) {
    has_entities <-
      (('entity_names' %>% exists()) &
         (!entity_names %>% purrr::is_null()))

    has_ciks <-
      (('ciks' %>% exists()) & (!ciks %>% purrr::is_null()))

    has_tickers <-
      (('tickers' %>% exists()) & (!tickers %>% purrr::is_null()))

    has_nothing <-
      ((!has_ciks) & (!has_entities) & (!has_tickers))

    has_tables <-
      (!tables %>% purrr::is_null()) #(('tables' %>% exists()) |

    if (has_nothing) {
      stop("Please enter a CIK, ticker, or an entity name")
    }

    all_ciks <-
      c()

    if (has_entities) {
      get_data_sec_filing_entities_safe <-
        purrr::possibly(get_data_sec_filing_entities, data_frame())

      search_df <-
        entity_names %>%
        get_data_sec_filing_entities_safe(return_message = return_message)

      has_rows <-
        search_df %>% nrow() > 0

      if (has_rows) {
        search_ciks <-
          search_df %>%
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

    if (all_ciks %>% length() > 0) {
      all_data <-
        all_ciks %>%
        sort() %>%
        map_df(function(x) {
          parse_cik_data_safe(
            tables = tables,
            nest_data = nest_data,
            cik = x,
            return_message = return_message
          )
        }) %>%
        mutate(
          urlRankAndFiled =
            list('http://rankandfiled.com/#/filers/', idCIK, '/filings') %>% purrr::invoke(paste0, .)
        ) %>%
        select(idCIK, nameEntity, urlRankAndFiled, nameTable, dataTable) %>%
        distinct() %>%
        suppressWarnings()
    }

    if (has_tickers) {
      parse_ticker_data_safe <-
        purrr::possibly(parse_ticker_data, data_frame())

      table_exists <-
        'all_data' %>% exists()

      if (table_exists) {
        all_ticker_data <-
          tickers %>%
          map_df(function(x) {
            parse_ticker_data(
              ticker = x,
              nest_data = nest_data,
              tables = tables,
              return_message = return_message
            )
          }) %>%
          suppressWarnings()

        all_data <-
          all_data %>%
          bind_rows(all_ticker_data)
      } else {
        all_data <-
          tickers %>%
          map_df(function(x) {
            parse_ticker_data_safe(ticker = x,
                                   tables = tables,
                                   return_message = return_message)
          }) %>%
          suppressWarnings()
      }
    }

    if (has_tables) {
      table_options <-
        c(
          'General',
          'CIK Filings',
          'Filings',
          'Private Offerings',
          'Related Parties',
          'Traders',
          'C Level',
          'MDA',
          'Owners',
          'Insider Trades',
          'Trades'
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

    if (!'all_data' %>% exists()) {
      return(data_frame())
    }

    missing_ciks <-
      all_ciks[!all_ciks %in% all_data$idCIK] %>% length() > 0

    if (missing_ciks) {
      list("Missing ", all_ciks[!all_ciks %in% all_data$idCIK] %>% paste(collapse = ', ')) %>%
        purrr::invoke(paste0, .) %>%
        message()
    }

    all_data <-
      all_data %>%
      select(-matches("urlRankAndFiled"))

    has_filings <-
      c('CIK Filings', 'Filings') %in% all_data$nameTable %>% sum() > 0

    if (has_filings) {
        filing_df <-
          all_data %>%
          filter(nameTable %in% c('Filings', 'CIK Filings')) %>%
          select(dataTable) %>%
          unnest() %>%
          distinct()


        filing_df <-
          filing_df %>%
          mutate_at(filing_df %>% select(matches("^url")) %>% names(),
                    funs(. %>% str_to_lower()))


        filing_df <-
          filing_df %>%
          mutate_at(filing_df %>% select(matches("url^[A-Z]")) %>% names(),
                    funs(. %>% str_replace_all('archives', 'Archives')))

        filing_df <-
          filing_df %>%
          mutate(urlSECFilingDirectory = urlSECFilingDirectory %>% gsub('archives', 'Archives',.),
                 urlSEC = urlSEC %>% gsub('archives', 'Archives',.))

        has_subsidiaries <-
          (filing_df %>%
             filter(typeFiling == "SUBSIDIARIES OF THE REGISTRANT") %>%
             nrow() > 0) & (parse_subsidiaries)

          if (has_subsidiaries) {
            parse_sec_subsidiary_url_safe <-
              purrr::possibly(parse_sec_subsidiary_url, data_frame())

            has_list <-
              filing_df %>%
              filter(typeFiling == "LIST OF SUBSIDIARIES") %>%
              nrow() > 0

            sub_url_df <-
              filing_df %>%
              filter(
                typeFiling %in% c(
                  "SUBSIDIARIES OF THE REGISTRANT",
                  "SUBSIDIARIES OF HOLDING COMPANY"
                )
              ) %>%
              select(dateFiling, nameEntity, urlSEC) %>%
              distinct()

            if (has_list) {
              sub_url_list_df <-
                filing_df %>%
                filter(
                  typeFiling %>% str_detect(
                    "LIST OF SUBSIDIARIES|LIST OF SIGNIFICANT SUBSIDIARIES|LIST OF SIGNIFCANT"
                  )
                ) %>%
                select(dateFiling, nameEntity, urlSEC) %>%
                distinct()

              if ('sub_url_df' %>% exists()) {
                sub_url_df <-
                  sub_url_list_df %>%
                  bind_rows(sub_url_df)
              } else {
                sub_url_df <-
                  sub_url_list_df
              }
            }

            sub_df <-
              sub_url_df %>%
              arrange(dateFiling) %>%
              .$urlSEC %>%
              map_df(function(x) {
                parse_sec_subsidiary_url_safe(url = x, return_message = return_message)
              }) %>%
              suppressWarnings()

            if (sub_df %>% nrow() > 0) {
              sub_df <-
                sub_df %>%
                select(-matches("X|date")) %>%
                filter(
                  !nameSubsidiary %in% c(
                    '(I)',
                    '(II)',
                    '(III)',
                    '(IV)',
                    '(V)',
                    '(VI)',
                    '(VII)',
                    '(VIII)',
                    '(IX)',
                    '(X)',
                    'PART A'
                  )
                ) %>%
                left_join(sub_url_df) %>%
                select(idCIK, dateFiling, everything()) %>%
                suppressMessages() %>%
                distinct()

              active_date_df <-
                sub_df %>%
                group_by(nameSubsidiary) %>%
                summarise(
                  dateFirstFiled = min(dateFiling, na.rm = TRUE),
                  dateLastFiled = max(dateFiling, na.rm = TRUE),
                  isActiveSubsidiary = ifelse(
                    dateLastFiled == sub_df$dateFiling %>% max(na.rm = TRUE),
                    TRUE,
                    FALSE
                  )
                ) %>%
                ungroup()

              sub_df <-
                sub_df %>%
                left_join(active_date_df) %>%
                left_join(sub_url_df) %>%
                suppressMessages()

              sub_df <-
                sub_df %>%
                mutate(nameSubsidiaryRF = nameSubsidiary %>% str_replace_all('\\,|\\.', '')) %>%
                select(idCIK, nameEntity, dateFiling, everything()) %>%
                suppressMessages()

              has_sub_df <-
                'Subsidiaries' %in% all_data$nameTable

              if (has_sub_df) {
                ad_sub_df <-
                  all_data %>%
                  filter(nameTable == 'Subsidiaries') %>%
                  select(dataTable) %>%
                  unnest()

                if ('pctSubsidiaryOwned' %in% names(ad_sub_df)) {
                  sub_df <-
                    sub_df %>%
                    left_join(
                      ad_sub_df %>%
                        select(nameSubsidiaryRF = nameSubsidiary, pctSubsidiaryOwned) %>%
                        distinct()
                    ) %>%
                    suppressMessages() %>%
                    select(-nameSubsidiaryRF)
                }

                if (nest_data) {
                  sub_df <-
                    sub_df %>%
                    nest(-c(dateFiling, idCIK, nameEntity), .key = dataSubsidiaries)
                }
                a_sub_df <-
                  sub_df %>%
                  group_by(idCIK, nameEntity) %>%
                  nest(-c(idCIK, nameEntity), .key = dataTable) %>%
                  ungroup() %>%
                  mutate(nameTable = 'Subsidiaries')

                all_data <-
                  all_data %>%
                  filter(!nameTable == 'Subsidiaries') %>%
                  bind_rows(a_sub_df)

              } else {
                if (nest_data) {
                  sub_df <-
                    sub_df %>%
                    nest(-c(dateFiling, idCIK, nameEntity), .key = dataSubsidiaries)
                }
                a_sub_df <-
                  sub_df %>%
                  group_by(idCIK, nameEntity) %>%
                  nest(-c(idCIK, nameEntity), .key = dataTable) %>%
                  ungroup() %>%
                  mutate(nameTable = 'Subsidiaries')

                all_data <-
                  all_data %>%
                  filter(!nameTable == 'Subsidiaries') %>%
                  bind_rows(a_sub_df)
              }
            }
          }
        parse_for_tables_rf_safe <-
          purrr::possibly(parse_for_tables_rf, data_frame())
        tables_edgar <-
          parse_for_tables_rf_safe(
            filing_df = filing_df,
            parse_complete_text_filings = parse_complete_text_filings,
            parse_form_d = parse_form_d,
            parse_13F = parse_13F,
            parse_small_offerings = parse_small_offerings,
            parse_form_3_4s = parse_form_3_4s,
            parse_asset_files = parse_asset_files,
            parse_xbrl = parse_xbrl
          )
        has_edgar_tables <-
          tables_edgar %>% nrow() > 0
        if (has_edgar_tables) {

          all_data <-
          all_data %>%
          nest(-nameTable, .key = dataTable) %>%
          bind_rows(tables_edgar)

        }
      }

    if (assign_to_environment) {
      table_name_df <-
        all_data %>%
        select(nameTable) %>%
        distinct() %>%
        mutate(
          nameDF =
            list('dataFiler', nameTable %>% str_replace_all('\\ ', '')) %>% purrr::invoke(paste0, .)
        )

      1:nrow(table_name_df) %>%
        walk(function(x) {
          df_name <-
            table_name_df %>% slice(x) %>% .$nameDF
          df_name %>% message()
          df_data <-
            all_data %>%
            filter(nameTable == table_name_df$nameTable[[x]]) %>%
            select(matches(c('idCIK|nameEntity|dataTable'))) %>%
            unnest() %>%
            suppressWarnings() %>%
            remove_duplicate_columns()

          has_unnest2 <-
            names(df_data) %>% str_detect('data') %>% sum(na.rm = TRUE) > 1

          if (has_unnest2) {
            base_names <-
              df_data %>% remove_duplicate_columns() %>% dplyr::select(-matches("data")) %>% names()

            df_data_names <-
              names(df_data)[names(df_data) %>% str_detect('data')]

            for (x in 1:length(df_data_names)) {
              df_data_name <-
                df_data_names[[x]]
              table <-
                df_data %>%
                select(one_of(c(base_names, df_data_name))) %>%
                remove_duplicate_columns()
              is_null_col <-
                table[,df_data_name] %>% magrittr::extract2(1) %>% map_lgl(is_null)

              table <-
                table %>%
                mutate(is_null_col) %>%
                filter(!is_null_col) %>%
                unnest() %>%
                remove_duplicate_columns() %>%
                select(which(colMeans(is.na(.)) < 1)) %>%
                # tidy_column_formats() %>%
                select(-matches('is_null_col')) %>%
                distinct()

              df_table_name <-
                list(df_name, df_data_name %>% str_replace_all('data', '')) %>% purrr::reduce(paste0)
              assign(x = df_table_name,
                     eval(table),
                     envir = .GlobalEnv)
            }

          } else {
            has_unnest <-
              df_data %>% names() %>% str_detect('data') %>% sum(na.rm = TRUE) > 0
            if (has_unnest) {
              if (df_name %>% str_detect("General")) {
                table <-
                  df_data %>%
                  remove_duplicate_columns() %>%
                  select(-matches("data")) %>%
                  # tidy_column_formats() %>%
                  select(which(colMeans(is.na(.)) < 1)) %>%
                  distinct()
                assign(x = df_name,
                       eval(table),
                       envir = .GlobalEnv)
              }
              if (df_name %in% 'dataFilerTextFilings') {
                table <-
                  df_data %>%
                  unnest() %>%
                  select(which(colMeans(is.na(.)) < 1)) %>%
                  tidy_column_formats() %>%
                  distinct()
                assign(x = df_name,
                       eval(table),
                       envir = .GlobalEnv)
              }
              if (df_name %in% 'dataFilerFilingDirectories') {
                table <-
                  df_data %>%
                  select(-matches('data')) %>%
                  filter(!idCIK %>% is.na()) %>%
                  select(which(colMeans(is.na(.)) < 1)) %>%
                  # tidy_column_formats() %>%
                  distinct()
                assign(x = df_name,
                       eval(table),
                       envir = .GlobalEnv)
                }

              other <-
                (!df_name %>% str_detect("General")) & (!df_name %in% c('dataFilerFilingDirectories', 'dataFilerTextFilings'))

              if (other) {
                df_data <-
                  df_data %>%
                  remove_duplicate_columns() %>%
                  # select(matches("data")) %>%
                  unnest()

                select_cols <-
                  data_frame(nameData = names(df_data)) %>%
                  mutate(idColumn = 1:n()) %>%
                  group_by(nameData) %>%
                  mutate(countColumn = 1:n()) %>%
                  ungroup() %>%
                  filter(countColumn == min(countColumn)) %>%
                  .$idColumn

                df_data <-
                  df_data[, select_cols]

                table <-
                  df_data %>%
                  select(which(colMeans(is.na(.)) < 1)) %>%
                  # tidy_column_formats() %>%
                  distinct()
                assign(x = df_name,
                       eval(table),
                       envir = .GlobalEnv)
              }
            } else {
              table <-
                df_data %>%
                select(which(colMeans(is.na(.)) < 1)) %>%
                # tidy_column_formats() %>%
                distinct()
              assign(x = df_name,
                     eval(table),
                     envir = .GlobalEnv)
            }
          }
        })
    }

    return(all_data)
}


# insider -----------------------------------------------------------------

parse_json_general_insider <-
  function(cik = 1354879,
           nest_data = TRUE,
           return_message = TRUE) {
    url <-
      list('http://rankandfiled.com/data/insider/', cik, '/general') %>%
      purrr::invoke(paste0, .)
    if (!url %>% httr::url_ok() %>% suppressWarnings()) {
      return(data_frame())
    }

    data <-
      url %>%
      jsonlite::fromJSON() %>%
      .[['insider']]

    general_cols <-
      data %>% map_df(class) %>%
      gather(item, value) %>%
      filter(!value %>% str_detect(c('list', 'data.frame'))) %>%
      .$item %>%
      suppressWarnings()

    general_df <-
      data %>%
      data.frame(stringsAsFactors = FALSE) %>%
      dplyr::select(one_of(general_cols)) %>%
      resolve_name_df() %>%
      distinct()

    has_filer <-
      'filer' %in% names(data)

    if (has_filer) {
      filing_df <-
        data$filer %>%
        flatten_df() %>%
        resolve_name_df()

      if ('name' %in% names(filing_df)) {
        filing_df <-
          filing_df %>%
          select(-name)
      }

      if ('detailsOwnedBy' %in% names(filing_df)) {
        filing_df <-
          filing_df %>%
          dplyr::rename(detailsOwns = detailsOwnedBy)
      }

      if ('detailsOwns' %in% names(filing_df)) {
        detail_df <-
          1:length(filing_df$detailsOwns) %>%
          map_df(function(x) {
            detail_value <-
              filing_df$detailsOwns[[x]]

            if (detail_value %>% is.na()) {
              df <-
                data_frame(idRow = x, nameCompanyOwns = NA)
              if (nest_data) {
                df <-
                  df %>%
                  nest(-idRow, .key = dataInsiderCompanies)
              }
              return(df)
            }

            values <-
              detail_value %>% str_replace('\\|', '') %>%
              str_split('\\|') %>%
              flatten_chr()

            df_data <-
              data_frame(value = values) %>%
              tidyr::separate(value,
                              into = c('idTickerOwns', 'other'),
                              sep = '\\:') %>%
              tidyr::separate(other,
                              into = c('nameCompanyOwns', 'other'),
                              sep = '\\_') %>%
              tidyr::separate(other,
                              into = c('roleOwner', 'dateOwner'),
                              sep = '\\#') %>%
              mutate(nameCompanyOwns = nameCompanyOwns %>% str_to_upper(),
                     idRow = x) %>%
              gather(item, value, -idRow, na.rm = TRUE) %>%
              group_by(item) %>%
              mutate(count = 1:n() - 1) %>%
              ungroup() %>%
              arrange((count)) %>%
              mutate(item = ifelse(count == 0, item, paste0(item, count))) %>%
              select(-count)

            column_order <-
              c('idRow', df_data$item)

            df_data <-
              df_data %>%
              spread(item, value) %>%
              select(one_of(column_order))

            if (nest_data) {
              df_data <-
                df_data %>%
                nest(-idRow, .key = dataInsiderCompanies)
            }

            return(df_data)
          }) %>%
          suppressWarnings()

        detail_df <-
          detail_df %>%
          mutate_at(.vars = detail_df %>% select(matches("date")) %>% names(),
                    funs(. %>% ymd())) %>%
          suppressWarnings()

        filing_df <-
          filing_df %>%
          mutate(idRow = 1:n()) %>%
          select(-detailsOwns) %>%
          left_join(detail_df) %>%
          select(-idRow) %>%
          suppressMessages()
      }

      general_df <-
        general_df %>%
        left_join(filing_df) %>%
        suppressMessages()
    }

    has_companies <-
      'companies' %in% names(data)

    if (has_companies) {
      companies_df <-
        data$companies %>%
        as_data_frame() %>%
        resolve_name_df()

      company_name_df <-
        companies_df %>%
        select(-matches("status_history")) %>%
        gather(item, value, -c(idCIK, nameFiler)) %>%
        group_by(item) %>%
        mutate(countItem = 1:n() - 1) %>%
        ungroup() %>%
        mutate(item = ifelse(countItem == 0, item, item %>% paste0(countItem))) %>%
        select(-countItem) %>%
        suppressWarnings() %>%
        suppressMessages()

      col_order <-
        c('idCIK', 'nameFiler', company_name_df$item)

      company_name_df <-
        company_name_df %>%
        spread(item, value) %>%
        select(one_of(col_order))

      company_name_df <-
        company_name_df %>%
        mutate_at(company_name_df %>% select(matches("idCIK")) %>% names(),
                  funs(. %>% as.numeric()))

      companies_df <-
        companies_df %>%
        mutate(idRow = 1:n())

      if ('status_history' %in% names(companies_df)) {
        status_df <-
          1:length(companies_df$status_history) %>%
          map_df(function(x) {
            df <-
              companies_df$status_history[[x]] %>%
              as_data_frame() %>%
              mutate(idRow = x) %>%
              select(-matches("other|pair_id")) %>%
              gather(item, value, -idRow) %>%
              left_join(data_frame(
                item = c('date', 'officer', 'title', 'ten_percent', 'director'),
                nameItem = c(
                  'dateAppointment',
                  'isOfficer',
                  'titleOfficer',
                  'is10PercentOwner',
                  'isDirector'
                )
              )) %>%
              select(-item) %>%
              group_by(nameItem) %>%
              mutate(countItem = 1:n() - 1) %>%
              ungroup() %>%
              mutate(item = ifelse(countItem == 0, nameItem, nameItem %>% paste0(countItem))) %>%
              select(idRow, item, value) %>%
              spread(item, value) %>%
              suppressMessages() %>%
              suppressWarnings()
            return(df)
          })

        status_df <-
          status_df %>%
          mutate_at(status_df %>% select(matches("date")) %>% names(),
                    funs(. %>% lubridate::ymd())) %>%
          mutate_at(status_df %>% select(matches("is")) %>% names(),
                    funs(. %>% as.logical())) %>%
          mutate_at(status_df %>% select(matches("date")) %>% names(),
                    funs(. %>% as.character()))

        companies_df <-
          companies_df %>%
          select(-matches("status")) %>%
          left_join(status_df) %>%
          suppressWarnings() %>%
          suppressMessages() %>%
          gather(item, value, -c(idCIK, nameFiler, idRow)) %>%
          group_by(item, idRow) %>%
          mutate(countItem = 1:n() - 1) %>%
          ungroup() %>%
          mutate(item = ifelse(countItem == 0, item, item %>% paste0(countItem))) %>%
          select(-countItem) %>%
          suppressWarnings()

        col_order <-
          c('idCIK', 'nameFiler', companies_df$item)

        companies_df <-
          companies_df %>%
          spread(item, value) %>%
          select(one_of(col_order)) %>%
          suppressWarnings()

        companies_df <-
          companies_df %>%
          mutate_at(status_df %>% select(matches("date")) %>% names(),
                    funs(. %>% lubridate::ymd())) %>%
          mutate_at(status_df %>% select(matches("^is|^has")) %>% names(),
                    funs(. %>% as.logical()))

      } else {
        companies_df <-
          company_name_df
      }

      if (nest_data) {
        companies_df <-
          companies_df %>%
          mutate(idRow = 1:n()) %>%
          nest(-c(idRow, idCIK), .key = dataDetailsCompaniesOwned)
      }

      general_df <-
        general_df %>%
        mutate(idRow = 1:n()) %>%
        left_join(companies_df) %>%
        select(-idRow) %>%
        suppressMessages()

    }

    general_df <-
      general_df %>%
      mutate(urlJSONGeneral = url)

    if ('typeCompany' %in% names(general_df)) {
      general_df <-
        general_df %>%
        dplyr::rename(typeFiler = typeCompany)
    }

    if (return_message) {
      list("Parsed: ", url) %>%
        purrr::invoke(paste0, .) %>% message()
    }

    return(general_df)


  }

parse_insider_trade_json_url <-
  function(url = "http://rankandfiled.com/data/insider/1070844/trades?start=0",
           return_message = TRUE) {
    if (!url %>% httr::url_ok() %>% suppressWarnings()) {
      return(data_frame())
    }

    json_data <-
      url %>%
      jsonlite::fromJSON()

    options(scipen = 9999)

    cik <-
      url %>% str_replace_all('http://rankandfiled.com/data/insider/|/trades', '') %>%
      str_split('\\?') %>%
      flatten_chr() %>%
      .[[1]] %>%
      as.numeric()

    trade_df <-
      json_data$trades %>%
      as_data_frame() %>%
      dplyr::rename(dateTrade = date) %>%
      mutate(dateTrade = dateTrade %>% lubridate::ymd())

    count_columns <-
      trade_df$trade %>%
      map_dbl(function(x) {
        x %>%
          str_count('\\*')
      }) %>%
      max() + 1

    column_names <-
      list("X", 1:count_columns) %>%
      purrr::invoke(paste0, .)

    trade_df <-
      trade_df %>%
      separate(trade, column_names, sep = '\\*') %>%
      suppressWarnings()

    trade_df_names <-
      c(
        'dateTrade',
        "idCIK",
        "idCIKOwns",
        "idInsiderType",
        "countSharesOwned",
        "descriptionOption",
        "idTypeInsiderTransaction",
        "amountPrice",
        "countShares",
        "idInsiderTransaction",
        "X10",
        "detailOwnershipIndirect",
        "priceExcercised",
        "dateOptionExcercisable",
        "dateOptionExpiry",
        "countSharesOptions",
        "typeSecurityOption",
        "X17"
      )

    trade_df <-
      trade_df %>%
      purrr::set_names((trade_df_names)[1:ncol(trade_df)]) %>%
      select(-matches("X"))

    trade_df <-
      trade_df %>%
      mutate_at(.vars =
                  trade_df %>% select(matches("date")) %>% names(),
                .funs = lubridate::ymd) %>%
      mutate_at(.vars =
                  trade_df %>% select(matches("idCIK|count|amount|price")) %>% names(),
                .funs = readr::parse_number) %>%
      left_join(data_frame(
        idInsiderType = c("D", "ND"),
        typeInsider = c("Director", "Non-Director")
      )) %>%
      left_join(get_insider_code_df()) %>%
      left_join(
        data_frame(
          idTypeInsiderTransaction = c("A", "D", "None"),
          typeInsiderTransaction = c('Purchase', 'Sale', 'None'),
          isBought = c(TRUE, FALSE, NA)
        )
      ) %>%
      suppressMessages() %>%
      suppressWarnings()

    trade_df <-
      trade_df %>%
      mutate(
        countShares = ifelse(isBought == T, countShares, -countShares),
        amountTransaction = countShares * amountPrice,
        urlJSON = url
      )


    if (return_message) {
      list("Parsed: ", url) %>%
        purrr::invoke(paste0, .) %>% message()
    }

    return(trade_df)

  }

parse_insider_trades <-
  function(cik = 1070844,
           nest_data = TRUE,
           return_message = TRUE) {
    url_general <-
      list('http://rankandfiled.com/data/insider/', cik, '/general') %>%
      purrr::invoke(paste0, .)

    general_df <-
      parse_json_general_insider(cik = cik,
                                 nest_data = nest_data,
                                 return_message = TRUE)

    cik <-
      general_df$idCIK

    insider <-
      general_df$nameEntity %>%
      str_to_upper()

    count_trades <-
      general_df$countTrades %/% 50

    trade_urls <-
      list(
        'http://rankandfiled.com/data/insider/',
        cik,
        '/trades?start=',
        seq(0, by = 50, length.out = count_trades)
      ) %>%
      purrr::invoke(paste0, .)

    parse_insider_trade_json_url_safe <-
      purrr::possibly(parse_insider_trade_json_url, data_frame())

    all_data <-
      trade_urls %>%
      purrr::map_df(function(x) {
        parse_insider_trade_json_url(url = x, return_message = return_message)
      }) %>%
      distinct()

    ciks_owned <-
      all_data$idCIKOwns %>% unique()

    company_urls_general <-
      list('http://rankandfiled.com/data/filer/',
           ciks_owned,
           '/general') %>%
      purrr::invoke(paste0, .)

    owned_company_df <-
      company_urls_general %>%
      map_df(function(x) {
        parse_json_general_filing(url = x,
                                  return_message = TRUE,
                                  nest_data = nest_data)
      })

    owned_df <-
      owned_company_df %>%
      select(matches('idCIK|nameEntity|idTicker')) %>%
      select(-matches("idCIKOwnedBy"))

    names(owned_df) <-
      names(owned_df) %>% paste0('Owns')

    all_data <-
      all_data %>%
      mutate(nameInsider = insider) %>%
      left_join(owned_df) %>%
      select(
        dateTrade,
        nameInsider,
        idCIK,
        nameEntityOwns,
        matches('idCIKOwns|idTickerOwns'),
        everything()
      ) %>%
      suppressWarnings() %>%
      suppressMessages()

    all_data <-
      all_data %>%
      mutate_at(.vars = all_data %>% select(matches("amount|price")) %>% names(),
                funs(. %>% formattable::currency(digits = 2))) %>%
      mutate_at(.vars = all_data %>% select(matches("count")) %>% names(),
                funs(. %>% formattable::comma(digits = 0)))

    if (return_message) {
      list(
        "Parsed ",
        all_data %>% nrow() %>% formattable::comma(digits = 0),
        ' insider transactions for ',
        insider
      ) %>%
        purrr::invoke(paste0, .) %>%
        message()
    }
    return(all_data)
  }

parse_insider_filings <-
  function(cik = 1070844,
           nest_data = TRUE,
           return_message = TRUE) {
    general_df <-
      parse_json_general_insider(cik = cik,
                                 nest_data = nest_data,
                                 return_messag = TRUE)

    cik <-
      general_df$idCIK

    insider <-
      general_df$nameEntity %>%
      str_to_upper()

    count_filings <-
      general_df$countFilings %/% 50

    filing_urls <-
      list(
        'http://rankandfiled.com/data/filer/',
        cik,
        '/all?start=',
        seq(0, by = 50, length.out = count_filings)
      ) %>%
      purrr::invoke(paste0, .)

    parse_json_public_filers_safe <-
      purrr::possibly(parse_json_public_filers, NULL)

    all_filings <-
      filing_urls %>%
      map_df(function(x) {
        parse_json_public_filers_safe(url = x, return_message = return_message)
      }) %>%
      distinct() %>%
      suppressWarnings() %>%
      mutate(nameInsider = insider) %>%
      select(idCIK, nameInsider, everything())

    if (return_message) {
      list("Parsed ", all_filings %>% nrow(), ' SEC Filings for ', insider) %>%
        purrr::invoke(paste0, .) %>%
        message()
    }

    return(all_filings)

  }

# funds -------------------------------------------------------------------
generate_fund_general_url <-
  function(cik = 1034621) {
    list('http://rankandfiled.com/data/fund/', cik, '/general') %>%
      purrr::invoke(paste0, .)
  }

parse_json_fund_general <-
  function(cik = 1034621,
           return_message = TRUE) {
    url <-
      cik %>%
      generate_fund_general_url()

    if (!url %>% httr::url_ok() %>% suppressWarnings()) {
      return(data_frame())
    }

    json_data <-
      url %>%
      jsonlite::fromJSON()

    general_cols <-
      json_data %>% map_df(class) %>%
      gather(item, value) %>%
      filter(!value %in% (c('list', 'data.frame'))) %>%
      .$item %>%
      suppressWarnings()

    general_df <-
      json_data %>%
      data.frame(stringsAsFactors = FALSE) %>%
      dplyr::select(one_of(general_cols)) %>%
      resolve_name_df() %>%
      distinct() %>%
      select(-matches("descriptionClasses"))

    has_funds <-
      'funds' %in% names(json_data)

    if (has_funds) {
      general_df <-
        general_df %>%
        left_join(json_data$funds %>%
                    resolve_name_df() %>%
                    mutate(idCIK = cik)) %>%
        suppressMessages()
    }

    has_filer <-
      'filer' %in% names(json_data)

    if (has_filer) {
      filer_df <-
        json_data$filer %>%
        as_data_frame()

      filer_df <-
        filer_df %>%
        resolve_name_df()
      if (!'idCIK' %in% names(filer_df)) {
        filer_df <-
          filer_df %>%
          mutate(idCIK = cik)
      }

      if ('name' %in% names(filer_df)) {
        filer_df <-
          filer_df %>%
          mutate(nameEntity = nameEntity %>% stringr::str_to_upper()) %>%
          select(-name)
      }
      filer_df <-
        filer_df %>%
        mutate_at(filer_df %>% select(matches("idRF|idCIK")) %>% names(),
                  funs(. %>% as.numeric()))

      merge_cols <-
        c('idCIKFiler', 'idRow', names(filer_df)[!names(filer_df) %in% names(general_df)])

      general_df <-
        general_df %>%
        mutate(idRow = 1:n()) %>%
        left_join(
          filer_df %>%
            mutate(idRow = 1:n()) %>%
            dplyr::rename(idCIKFiler = idCIK) %>%
            select(one_of(merge_cols))
        ) %>%
        select(-matches("^object|idRow")) %>%
        distinct() %>%
        suppressMessages()

    }

    general_df <-
      general_df %>%
      select(idCIK,
             nameEntity,
             matches("name"),
             matches("id"),
             everything())

    if (return_message) {
      list("Parsed: ", url) %>%
        purrr::invoke(paste0, .) %>% message()
    }

    return(general_df)

  }


# for_table ---------------------------------------------------------------

parse_for_tables_rf <-
  function(filing_df,
           parse_complete_text_filings = TRUE,
           parse_form_d = TRUE,
           parse_13F = TRUE,
           parse_small_offerings = TRUE,
           parse_form_3_4s = TRUE,
           parse_asset_files = TRUE,
           parse_xbrl = TRUE,
           nest_data = TRUE,
           return_message = TRUE) {
    all_tables <-
      data_frame()
    parse_all_filings <-
      c(
        parse_complete_text_filings,
        parse_form_d,
        parse_13F,
        parse_small_offerings,
        parse_form_3_4s,
        parse_asset_files,
        parse_xbrl
      ) %>%
      sum() > 0

    parse_form_data_safe <-
      purrr::possibly(parse_form_data, data_frame())

    if (parse_all_filings) {
      if (!'typeFile' %in% names(filing_df)) {
        filing_df <-
          filing_df %>%
          mutate(typeFile = ifelse(urlSECFilingDirectory %>% str_detect('htm'),
                                   'html', NA))
      }

      search_df <-
        filing_df %>%
        select(dateFiling,
               matches("typeFile"),
               matches("idForm"),
               urlSECFilingDirectory) %>%
        distinct() %>%
        filter(!urlSECFilingDirectory %>% is.na()) %>%
        distinct()

      df_all_filing_urls <-
        search_df$urlSECFilingDirectory %>%
        unique() %>%
        map_df(function(x){
          parse_sec_filing_index(urls = x)
        })

      df_all_filing_urls <-
        df_all_filing_urls %>%
        mutate(isForm3_4 = ifelse(typeForm %in% c("3", "4") &
                                    typeFile == "xml", TRUE, FALSE))
      df_urls <-
        df_all_filing_urls %>%
        mutate(nameTable = 'Filing Directories') %>%
        nest(-nameTable, .key = dataTable)

      all_tables <-
        all_tables %>%
        bind_rows(df_urls)

      if (parse_complete_text_filings) {
        if (!'urlTextFilingFull' %in% names(df_all_filing_urls)) {
          df_all_filing_urls <-
            df_all_filing_urls %>%
            mutate(urlTextFilingFull = urlSECFilingDirectory %>% str_replace_all("-index.htm", ".txt"))
        }
        urls <-
          df_all_filing_urls$urlTextFilingFull %>%
          unique()
        get_data_sec_complete_filings_safe <-
          purrr::possibly(get_data_sec_complete_filings, data_frame())
        all_text_df <-
          get_data_sec_complete_filings(urls = urls)

        all_tables <-
          all_tables %>%
          bind_rows(data_frame(
            nameTable = 'Text Filings',
            dataTable = list(all_text_df %>% nest(-c(idCIK), .key = dataFilings))
          ))
      }

      if (parse_form_d) {
        df_form_ds <-
          df_all_filing_urls %>%
          parse_form_data_safe(filter_parameter = 'isFormD')

        all_tables <-
          all_tables %>%
          bind_rows(data_frame(
            nameTable = 'FormDs',
            dataTable = list(df_form_ds)
          ))
      }

      if (parse_13F) {
        df_13F <-
          df_all_filing_urls %>%
          parse_form_data_safe(filter_parameter = 'is13FFiling')

        all_tables <-
          all_tables %>%
          bind_rows(data_frame(nameTable = '13Fs', dataTable = list(df_13F)))
      }

      if (parse_small_offerings) {
        df_small_offerings <-
          df_all_filing_urls %>%
          parse_form_data_safe(filter_parameter = 'hasSmallOfferingData')
        all_tables <-
          all_tables %>%
          bind_rows(data_frame(
            nameTable = 'Small Offerings',
            dataTable = list(df_small_offerings)
          ))
      }

      if (parse_form_3_4s) {
        df_form3_4 <-
          df_all_filing_urls %>%
          parse_form_data_safe(filter_parameter = 'isForm3_4')

        all_tables <-
          all_tables %>%
          bind_rows(data_frame(
            nameTable = 'Form 3 and 4',
            dataTable = list(df_form3_4)
          ))
      }

      if (parse_asset_files) {
        df_assets <-
          df_all_filing_urls %>%
          parse_form_data_safe(filter_parameter = 'hasAssetFile')

        all_tables <-
          all_tables %>%
          bind_rows(data_frame(nameTable = 'Asset Data', dataTable = list(df_assets)))
      }

      if (parse_xbrl) {
        df_xbrl <-
          df_all_filing_urls %>%
          parse_form_data_safe(filter_parameter = 'isXBRLInstanceFile')

        all_tables <-
          all_tables %>%
          bind_rows(data_frame(nameTable = 'XBRL', dataTable = list(df_xbrl)))
      }

    }

    all_tables <-
      all_tables %>%
      mutate(countCols = dataTable %>% map_dbl(ncol)) %>%
      filter(countCols > 0) %>%
      select(-countCols)

    return(all_tables)
  }


# filing_stream -----------------------------------------------------------

get_most_recent_rf_id <-
  function(url = "http://rankandfiled.com/data/latest") {
    json_data <-
      url %>%
      jsonlite::fromJSON()

    json_data$filings$id %>% as.numeric() %>% max()
  }


parse_filing_stream <-
  function(url = "http://rankandfiled.com/data/latest?group=ALL&filer=All",
           nest_data = TRUE,
           return_message = TRUE) {
    json_data <-
      url %>%
      jsonlite::fromJSON()

    options(scipen = 9999)

    filing_class_df <-
      json_data$filings %>% map_df(class) %>%
      gather(column, type) %>%
      mutate(idName = 1:n())

    general_df <-
      json_data$filings %>%
      select(filing_class_df %>%
               filter(!type %in% c('list', 'data.frame')) %>%
               .$idName)

    general_df <-
      general_df %>%
      as_data_frame() %>%
      mutate_all(funs(. %>% str_replace('\\|', '')))

    general_df <-
      general_df %>%
      resolve_name_df() %>%
      distinct()

    general_df <-
      general_df %>%
      mutate_at(general_df %>% select(matches("^datetime[A-Z]")) %>% names(),
                funs(. %>% lubridate::ymd_hms())) %>%
      mutate_at(general_df %>% select(matches("dateFiled")) %>% names(),
                funs(. %>% lubridate::ymd())) %>%
      mutate_at(general_df %>% select(matches("idRF|idCIK")) %>% names(),
                funs(. %>% as.numeric())) %>%
      mutate_at(general_df %>% select(matches("^is|^has")) %>% names(),
                funs(. %>% as.logical())) %>%
      mutate_at(general_df %>% select(matches("^description|^type")) %>% names(),
                funs(. %>% stringr::str_to_upper())) %>%
      mutate(urlSEC = ifelse(
        slugSEC == "None",
        NA,
        list(
          "https://www.sec.gov/Archives/edgar/data/",
          idCIK,
          '/',
          slugSEC
        ) %>% purrr::invoke(paste0, .)
      ))

    if ('idFormType' %in% names(general_df)) {
      general_df %>%
        left_join(get_dictionary_sec_filing_codes()) %>%
        suppressMessages()
    }

    has_filer <-
      'filer' %in% names(json_data$filings)

    if (has_filer) {
      filer_df <-
        json_data$filings$filer %>%
        as_data_frame()

      filer_df <-
        filer_df %>%
        resolve_name_df()

      if ('name' %in% names(filer_df)) {
        filer_df <-
          filer_df %>%
          dplyr::rename(nameLegal = name) %>%
          mutate(nameEntity = nameEntity %>% stringr::str_to_upper())
      }
      filer_df <-
        filer_df %>%
        mutate_at(filer_df %>% select(matches("idRF|idCIK")) %>% names(),
                  funs(. %>% as.numeric())) %>%
        mutate_at(filer_df %>% select(matches("^name|^industry|^typeFund|^details")) %>% names(),
                  funs(. %>% stringr::str_to_upper()))

      if ('detailsOwnedBy' %in% names(filer_df)) {
        filer_df <-
          filer_df %>%
          dplyr::rename(detailsOwns = detailsOwnedBy)

        filer_df <-
          filer_df %>%
          mutate(idRow = 1:n(),
                 detailsOwns = detailsOwns %>% str_replace("\\|", ''))

        owns_df <-
          1:nrow(filer_df) %>%
          map_df(function(x) {
            owns <-
              filer_df$detailsOwns[[x]] %>%
              str_split("\\|") %>%
              flatten_chr()

            df <-
              data_frame(idRow = x, owns) %>%
              tidyr::separate(owns,
                              into = c('idTickerOwns', 'owns'),
                              sep = '\\:') %>%
              tidyr::separate(owns,
                              into = c('nameCompanyOwns', 'owns'),
                              sep = '\\_') %>%
              tidyr::separate(
                owns,
                into = c('typeOwnerOwns', 'dateOwnershipOwns'),
                sep = '\\#'
              ) %>%
              mutate(countItem = 1:n() - 1) %>%
              mutate(
                nameCompanyOwns = nameCompanyOwns %>% str_to_upper(),
                idTickerOwns = idTickerOwns %>% str_to_upper()
              ) %>%
              gather(item, value, -c(idRow, countItem)) %>%
              mutate(value = ifelse(value == '', NA, value)) %>%
              mutate(item = ifelse(countItem == 0, item, item %>% paste0(countItem))) %>%
              arrange(countItem) %>%
              select(-countItem)

            col_order <-
              c('idRow', df$item)
            df <-
              df %>%
              spread(item, value) %>%
              select(one_of(col_order))

            df <-
              df %>%
              mutate_at(df %>% select(matches("date")) %>% names(),
                        funs(. %>% lubridate::ymd()))

            if (nest_data) {
              df <-
                df %>%
                nest(-idRow, .key = dataCompaniesOwns)
            }
            return(df)
          }) %>%
          suppressWarnings()

        filer_df <-
          filer_df %>%
          left_join(owns_df) %>%
          select(-idRow) %>%
          suppressMessages()
      }


      general_df <-
        general_df %>%
        mutate(idRow = 1:n()) %>%
        left_join(
          filer_df %>%
            mutate(idRow = 1:n()) %>%
            dplyr::rename(idCIKFiler = idCIK) %>%
            select(one_of(
              c('idCIKFiler', 'idRow'), names(filer_df)[!names(filer_df) %in% names(general_df)]
            ))
        ) %>%
        select(-matches("^object|idRow")) %>%
        distinct() %>%
        suppressMessages()

    }

    has_offerings <-
      'offerings' %in% names(json_data$filings)

    if (has_offerings) {
      general_df <-
        general_df %>%
        mutate(idRow = 1:n())

      offering_df <-
        1:nrow(general_df) %>%
        map_df(function(x) {
          offering <-
            json_data$filings$offerings[[x]]

          has_no_data <-
            offering %>% is_null()

          if (has_no_data) {
            return(data_frame(idRow = x))
          }
          has_no_rows <-
            offering %>% nrow() == 0
          if (has_no_rows) {
            return(data_frame(idRow = x))
          }

          offering_long <-
            offering %>% resolve_name_df() %>%
            mutate(idRow = x) %>%
            gather(item, value, -idRow) %>%
            group_by(item) %>%
            mutate(countItem = 1:n() - 1) %>%
            ungroup() %>%
            mutate(item = ifelse(countItem == 0, item, item %>% paste0(countItem))) %>%
            arrange(countItem) %>%
            select(-countItem)

          col_order <-
            offering_long$item

          offering <-
            offering_long %>%
            spread(item, value) %>%
            select(one_of(c('idRow', col_order)))

          offering <-
            offering %>%
            mutate_at(offering %>% select(matches("^count[A-Z]|^amount")) %>% names(),
                      funs(. %>% as.numeric())) %>%
            mutate_at(offering %>% select(matches("^date")) %>% names(),
                      funs(. %>% lubridate::ymd()))

          if (nest_data) {
            offering <-
              offering %>%
              nest(-idRow, .key = dataOfferings)
          }
          return(offering)
        }) %>%
        select(idRow, everything())

      offering_df <-
        offering_df %>%
        mutate_at(matches("^nameIndustry"),
                  funs(. %>% str_to_upper()))

      general_df <-
        general_df %>%
        mutate(idRow = 1:n()) %>%
        select(-matches("nameIndustry")) %>%
        left_join(offering_df) %>%
        select(-idRow) %>%
        suppressWarnings() %>%
        suppressMessages()

      if ('nameIndustry' %in% names(general_df)) {
        general_df <-
          general_df %>%
          dplyr::rename(nameIndustryOffering = nameIndustry)
      }
    }

    has_trades <-
      'trades' %in% names(json_data$filings)

    if (has_trades) {
      general_df <-
        general_df %>%
        mutate(idRow = 1:n())

      trade_df <-
        1:nrow(general_df) %>%
        map_df(function(x) {
          trades <-
            json_data$filings$trades[[x]]

          has_no_data <-
            trades %>% is_null()

          if (has_no_data) {
            return(data_frame(idRow = x))
          }
          has_no_rows <-
            trades %>% nrow() == 0
          if (has_no_rows) {
            return(data_frame(idRow = x))
          }

          trades <-
            trades %>% resolve_name_df() %>%
            mutate(idRow = x) %>%
            dplyr::rename(idInsiderTransaction = codeTransaction)

          trades <-
            trades %>%
            mutate_at(.vars = trades %>% select(matches("amount|count")) %>% names,
                      funs(. %>% as.numeric())) %>%
            left_join(get_insider_code_df()) %>%
            suppressWarnings() %>%
            suppressMessages()

          if ('amountPrice' %in% names(trades)) {
            if (!'isBought' %in% names(trades)) {
              trades <-
                trades %>%
                mutate(isBought = FALSE)
            }

            trades <-
              trades %>%
              mutate(
                isBought = ifelse(isBought %>% is.na(), FALSE, TRUE),
                countShares = ifelse(isBought == T, countShares, -countShares),
                amountTransaction = countShares * amountPrice
              )
          }

          trades_long <-
            trades %>%
            gather(item, value, -idRow) %>%
            group_by(item) %>%
            mutate(countItem = 1:n() - 1) %>%
            ungroup %>%
            mutate(item = ifelse(countItem == 0, item, item %>% paste0(countItem))) %>%
            arrange(countItem) %>%
            select(-countItem)

          col_order <-
            trades_long$item

          trades <-
            trades_long %>%
            spread(item, value) %>%
            select(one_of(c('idRow', col_order)))

          trades <-
            trades %>%
            mutate_at(trades %>% select(matches("^count[A-Z]|^amount")) %>% names(),
                      funs(. %>% as.numeric())) %>%
            mutate_at(trades %>% select(matches("^date")) %>% names(),
                      funs(. %>% lubridate::ymd()))

          if (nest_data) {
            trades <-
              trades %>%
              nest(-idRow, .key = dataTrades)
          }
          return(trades)
        }) %>%
        select(idRow, everything())

      names(trade_df)[names(trade_df) %>% str_detect('dateFiling')] <-
        trade_df %>% select(matches("dateFiling")) %>% names() %>%
        str_replace_all("dateFiling", 'dateFilingInsider')


      general_df <-
        general_df %>%
        mutate(idRow = 1:n()) %>%
        select(-matches("nameIndustry")) %>%
        left_join(trade_df %>% select(-matches("idTicker")), by = 'idRow') %>%
        select(-idRow) %>%
        suppressWarnings() %>%
        suppressMessages()
    }

    general_df <-
      general_df %>%
      mutate_at(.vars = general_df %>% select(matches("nameEntity")) %>% names(),
                funs(. %>% str_to_upper())) %>%
      suppressWarnings() %>%
      ungroup() %>%
      select(-matches("^object[A-Z]|^slug|dateiso")) %>%
      select(idCIK,
             nameEntity,
             matches("name"),
             matches("id"),
             everything())


    if (return_message) {
      list("Parsed: ", url) %>%
        purrr::invoke(paste0, .) %>% message()
    }
    return(general_df)
  }


get_data_sec_filing_stream <-
  function(filers = 'All',
           filing_name = 'Registrations',
           nest_data = TRUE,
           return_message = TRUE) {
    both_all <-
      filers == 'All' & filing_name == 'All'

    if (both_all) {
      rf_id <-
        get_most_recent_rf_id()

      start <-
        rf_id - 3500

      rf_ds <-
        seq(start, rf_id, by = 30)

      urls <-
        list('http://rankandfiled.com/data/latest?id=',
             rf_ds) %>%
        purrr::invoke(paste0, .)

      parse_filing_stream_safe <-
        purrr::possibly(parse_filing_stream, data_frame())

      data <-
        urls %>%
        map_df(function(x) {
          parse_filing_stream_safe(url = x, nest_data = nest_data)
        }) %>%
        distinct() %>%
        select(
          idRF,
          idCIK,
          matches("nameEntity"),
          matches("idTicker"),
          matches("dateFiled"),
          matches("datetimeFiled"),
          matches("^name"),
          matches("^date"),
          matches("^id"),
          matches("^type"),
          matches("^description"),
          everything()
        ) %>%
        mutate(
          urlRankAndFiled = list('http://rankandfiled.com/#/filers/', idCIK, '/filings') %>%
            purrr::invoke(paste0, .)
        )


    } else {
      filer_names <-
        c('All',
          'Corporate Insider',
          'Companies',
          'Investment Company') %>%
        str_to_upper()

      filing_names <-
        c(
          'Annual Reports',
          'Quarterly Reports',
          'Current Reports',
          'Other Reports',
          'Registrations',
          'Private Offerings',
          'Ownership',
          'Prospectuses',
          'Exemptions',
          'Withdrawals',
          'Correspondence',
          'Proxy Statements',
          'Confidential',
          'All'
        ) %>% str_to_upper()

      no_filers <-
        !filers %>% str_to_upper() %in% filer_names

      if (no_filers) {
        stop(
          list(
            "Filers can only be:\n",
            filer_names %>%  stringr::str_to_title() %>% paste0(collapse = '\n')
          ) %>%
            purrr::invoke(paste0, .)
        )
      }
      no_filing_names <-
        !filing_name %>% str_to_upper() %in% filing_names

      if (no_filing_names) {
        stop(
          list(
            "Filing names can only be:\n",
            filing_names %>%  stringr::str_to_title() %>% paste0(collapse = '\n')
          ) %>%
            purrr::invoke(paste0, .)
        )
      }

      filer_type_df <-
        data_frame(
          codeFiler = c('All', 'insider', 'company', 'inv_co'),
          nameFiler = filer_names
        )

      slug_filer <-
        filer_type_df %>%
        filter(nameFiler == filers %>% str_to_upper()) %>%
        .$codeFiler

      filing_name_df <-
        data_frame(
          codeFiling  = c(
            "A",
            "Q",
            "CR",
            "R",
            "REG",
            "REGX",
            "O",
            "P",
            "X",
            "W",
            "SEC",
            "PROXY",
            "CT",
            "ALL"
          ),
          nameFiling = filing_names
        )

      slug_type <-
        filing_name_df %>%
        filter(nameFiling == filing_name %>% str_to_upper()) %>%
        .$codeFiling

      mr_id <-
        get_most_recent_rf_id()

      url_json <-
        list(
          'http://rankandfiled.com/data/latest?id=',
          mr_id,
          '&group=',
          slug_type,
          '&filer=',
          slug_filer
        ) %>%
        purrr::invoke(paste0, .)

      data <-
        url_json %>%
        parse_filing_stream() %>%
        select(
          idRF,
          idCIK,
          matches("nameEntity"),
          matches("idTicker"),
          matches("dateFiled"),
          matches("datetimeFiled"),
          matches("^name"),
          matches("^date"),
          matches("^id"),
          matches("^type"),
          matches("^description"),
          everything()
        ) %>%
        mutate(
          urlRankAndFiled = list('http://rankandfiled.com/#/filers/', idCIK, '/filings') %>%
            purrr::invoke(paste0, .)
        )
    }

    if (return_message) {
      list("\nParsed Most Recent filings for ",
           filers,
           ' Filers\n',
           filing_name,
           ' Form Type\n') %>%
        purrr::invoke(paste0, .) %>%
        message()
    }
    return(data)
  }


#' SEC filing stream
#'
#' This function returns the most recent SEC filings
#' for specified filer type by filing type.
#'
#' @param filers type of filer \itemize{
#' \item \code{All}: all filer types (default)
#' \item \code{Corporate Insider}: corporate insiders
#' \item \code{Companies}:
#' \itm \code{Investment Company} acquires investment compan
#' @param filing_names type of filing \itemize{
#' \item \code{Annual Reports}: annual report
#' \item \code{Quarterly Reports}: quarterly report
#' \item \code{Current Reports}: current report
#' \item \code{Other Reports}: other reports
#' \item \code{Registrations}: securities registration
#' \item \code{Private Offerings}: private securities offerings
#' \item \code{Ownership}: ownership
#' \item \code{Prospectuses}: securities prospectus
#' \item \code{Exemptions}: exempt securities
#' \item \code{Withdrawals}: securities withdrawls
#' \item \code{Correspondence}: SEC correspondence
#' \item \code{Proxy Statements}:  proxy issuances
#' \item \code{Confidential}: confidential information
#' }
#' @param return_message return a message
#' @import dplyr tidyr purrr stringr formattable readr lubridate
#' @importFrom jsonlite fromJSON
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' get_data_sec_filing_streams(filers = 'All', filing_names = 'Annual Reports')
#' }
#'
get_data_sec_filing_streams_rf <-
  function(filers = c('All', 'Corporate Insider', 'Companies', 'Investment Company'),
           filing_names = c(
             'All',
             'Annual Reports',
             'Quarterly Reports',
             'Current Reports',
             'Other Reports',
             'Registrations',
             'Private Offerings',
             'Ownership',
             'Prospectuses',
             'Exemptions',
             'Withdrawals',
             'Correspondence',
             'Proxy Statements',
             'Confidential'
           ),
           nest_data = TRUE,
           return_message = TRUE) {
    type_df <-
      expand.grid(
        nameFiler = filers,
        nameFiling = filing_names,
        stringsAsFactors = FALSE
      ) %>%
      as_data_frame()

    get_data_sec_filing_stream_safe <-
      purrr::possibly(get_data_sec_filing_stream, NULL)

    all_data <-
      1:nrow(type_df) %>%
      map_df(function(x) {
        get_data_sec_filing_stream_safe(
          filers = type_df$nameFiler[[x]],
          filing_name = type_df$nameFiling[[x]],
          nest_data = nest_data,
          return_message = return_message
        )
      }) %>%
      distinct() %>%
      mutate(idRow = 1:n()) %>%
      group_by(idRF) %>%
      filter(idRow == min(idRow)) %>%
      ungroup() %>%
      select(-idRow)

    all_data <-
      all_data %>%
      select(-matches("dateiso")) %>%
      mutate_at(all_data %>% select(matches("^name|^description|^industry|^typeEntity")) %>% names(),
                funs(. %>% stringr::str_to_upper()))

    return(all_data)
  }


# publics -----------------------------------------------------------------
generate_ticker_general_url <-
  function(ticker = "FB") {
    list("http://rankandfiled.com/data/company/",
         ticker,
         '/general') %>%
      purrr::invoke(paste0, .)
  }


parse_json_public_general <-
  function(url = "http://rankandfiled.com/data/company/BX/general",
           nest_data = TRUE,
           return_message = TRUE) {
    if (!url %>% httr::url_ok() %>% suppressWarnings()) {
      return(data_frame())
    }

    json_data <-
      url %>%
      jsonlite::fromJSON()

    ticker <-
      url %>% str_replace("http://rankandfiled.com/data/company/", '') %>%
      str_split('\\/') %>% flatten_chr() %>%
      .[[1]]

    general_class_df <-
      json_data %>% map_df(class) %>%
      gather(column, type) %>%
      mutate(idName = 1:n())

    general_df <-
      json_data[general_class_df %>%
                  filter(!type %in% c('list', 'data.frame')) %>%
                  .$idName] %>%
      flatten_df() %>%
      resolve_name_df() %>%
      select(-matches("idTicker")) %>%
      mutate(idTicker = ticker)

    has_market <-
      'market' %in% names(json_data)

    if (has_market) {
      general_df <-
        general_df %>%
        bind_cols(json_data$market %>%
                    flatten_df() %>%
                    resolve_name_df() %>%
                    select(-matches("codeExchange")))

      if ('amountEquityMarketCap' %in% names(general_df)) {
        general_df <-
          general_df %>%
          mutate(amountEquityMarketCap = amountEquityMarketCap %>% formattable::currency(digits = 0))
      }

      if ('nameIndustry' %in% names(general_df)) {
        has_semi <-
          general_df$nameIndustry %>% str_detect('\\:')
        if (has_semi) {
          general_df <-
            general_df %>%
            tidyr::separate(
              nameIndustry,
              into = c('nameIndustry', 'nameSubIndustry'),
              sep = '\\: '
            ) %>%
            suppressWarnings()
        }
      }

      general_df <-
        general_df %>%
        mutate_if(is_character,
                  str_to_upper)
    }

    has_snap_shot <-
      'snapshot' %in% names(json_data)

    if (has_snap_shot) {
      snap_shot_df <-
        json_data$snapshot %>%
        as_data_frame()

      if ('ebitda' %in% names(snap_shot_df)) {
        snap_shot_df <-
          snap_shot_df %>%
          mutate(digitEBITDA = ebitda %>% substr(
            start = (ebitda %>% nchar()) ,
            stop = ebitda %>% nchar()
          ))
      }

      snap_shot_df <-
        snap_shot_df %>%
        resolve_name_df() %>%
        select(-matches("amountEquityMarketCap"))

      if ('digitEBITDA' %in% names(snap_shot_df)) {
        snap_shot_df <-
          snap_shot_df %>%
          mutate(
            amountEBITDA = ifelse(
              digitEBITDA == "B",
              amountEBITDA * 1000000000,
              amountEBITDA * 1000000
            )
          ) %>%
          select(-digitEBITDA)
      }

      snap_shot_df <-
        snap_shot_df %>%
        mutate_at(.vars = snap_shot_df %>% select(matches("price|amount")) %>% names,
                  funs(. %>% currency(digits = 2))) %>%
        mutate_at(.vars = snap_shot_df %>% select(matches("amountEBITDA")) %>% names,
                  funs(. %>% currency(digits = 0)))

      general_df <-
        general_df %>%
        bind_cols(snap_shot_df)
    }

    has_filer <-
      ((
        'filer' %in% names(json_data) &
          json_data$filer %>% as_data_frame() %>% ncol > 2
      ))

    if (has_filer) {
      filer_df <-
        json_data$filer %>%
        as_data_frame()

      filer_df <-
        filer_df %>%
        resolve_name_df()

      if ('detailsOwnedBy' %in% names(filer_df)) {
        filer_df <-
          filer_df %>%
          dplyr::rename(detailsOwns = detailsOwnedBy)

        filer_df <-
          filer_df %>%
          mutate(idRow = 1:n(),
                 detailsOwns = detailsOwns %>% str_replace("\\|", ''))

        owns_df <-
          1:nrow(filer_df) %>%
          map_df(function(x) {
            owns <-
              filer_df$detailsOwns[[x]] %>%
              str_split("\\|") %>%
              flatten_chr()

            df <-
              data_frame(idRow = x, owns) %>%
              tidyr::separate(owns,
                              into = c('idTickerOwns', 'owns'),
                              sep = '\\:') %>%
              tidyr::separate(owns,
                              into = c('nameCompanyOwns', 'owns'),
                              sep = '\\_') %>%
              tidyr::separate(
                owns,
                into = c('typeOwnerOwns', 'dateOwnershipOwns'),
                sep = '\\#'
              ) %>%
              mutate(countItem = 1:n() - 1) %>%
              mutate(
                nameCompanyOwns = nameCompanyOwns %>% str_to_upper(),
                idTickerOwns = idTickerOwns %>% str_to_upper()
              ) %>%
              gather(item, value, -c(idRow, countItem)) %>%
              mutate(value = ifelse(value == '', NA, value)) %>%
              mutate(item = ifelse(countItem == 0, item, item %>% paste0(countItem))) %>%
              arrange(countItem) %>%
              select(-countItem)

            col_order <-
              c('idRow', df$item)
            df <-
              df %>%
              spread(item, value) %>%
              select(one_of(col_order))

            df <-
              df %>%
              mutate_at(df %>% select(matches("date")) %>% names(),
                        funs(. %>% lubridate::ymd()))

            if (nest_data) {
              df <-
                df %>%
                nest(-idRow, .key = dataCompaniesOwns)
            }
            return(df)
          }) %>%
          suppressWarnings()

        filer_df <-
          filer_df %>%
          left_join(owns_df) %>%
          select(-idRow) %>%
          suppressMessages()
      }

      filer_df <-
        filer_df %>%
        mutate_at(filer_df %>% select(matches("nameEntity")) %>% names(),
                  funs(. %>% stringr::str_to_upper())) %>%
        select(
          matches("nameEntity"),
          matches("^id"),
          matches("industry"),
          matches("name"),
          matches("type"),
          everything()
        ) %>%
        select(-matches("object"))

      if ('addressStreet1Entity' %in% names(filer_df)) {
        filer_df <-
          filer_df %>%
          mutate(
            addressEntity = list(
              addressStreet1Entity,
              ' ',
              cityEntity,
              ' ',
              stateEntity,
              ', ',
              zipcodeEntity
            ) %>% purrr::invoke(paste0, .)
          ) %>%
          select(nameEntity, addressEntity, everything())
      }

      filer_cols <-
        names(filer_df)[!names(filer_df) %in% names(general_df)]

      general_df <-
        general_df %>%
        bind_cols(filer_df %>% select(one_of(filer_cols))) %>%
        select(idCIK, matches("nameEntity"), everything()) %>%
        select(-matches("detailsOwns"))
    }

    if ('detailsOwns' %in% names(general_df)) {
      detail_df <-
        1:length(general_df$detailsOwns) %>%
        map_df(function(x) {
          detail_value <-
            general_df$detailsOwns[[x]]

          if (detail_value %>% is.na()) {
            df <-
              data_frame(idRow = x, nameCompanyOwns = NA)
            if (nest_data) {
              df <-
                df %>%
                nest(-idRow, .key = dataCompaniesOwns)
            }
            return(df)
          }

          values <-
            detail_value %>% str_replace('\\|', '') %>%
            str_split('\\|') %>%
            flatten_chr()

          df_data <-
            data_frame(value = values) %>%
            tidyr::separate(value,
                            into = c('idTickerOwns', 'other'),
                            sep = '\\:') %>%
            tidyr::separate(other,
                            into = c('nameCompanyOwns', 'other'),
                            sep = '\\_') %>%
            tidyr::separate(other,
                            into = c('roleOwner', 'dateOwner'),
                            sep = '\\#') %>%
            mutate(nameCompanyOwns = nameCompanyOwns %>% str_to_upper(),
                   idRow = x) %>%
            gather(item, value, -idRow, na.rm = TRUE) %>%
            group_by(item) %>%
            mutate(value = ifelse(value == '', NA, value),
                   count = 1:n() - 1) %>%
            ungroup() %>%
            arrange((count)) %>%
            mutate(item = ifelse(count == 0, item, paste0(item, count))) %>%
            select(-count)

          column_order <-
            c('idRow', df_data$item)

          df_data <-
            df_data %>%
            spread(item, value) %>%
            select(one_of(column_order))

          if (nest_data) {
            df_data <-
              df_data %>%
              nest(-idRow, .key = dataCompaniesOwns)
          }
          return(df_data)
        }) %>%
        suppressWarnings()

      detail_df <-
        detail_df %>%
        mutate_at(.vars = detail_df %>% select(matches("date")) %>% names(),
                  funs(. %>% ymd())) %>%
        suppressWarnings()

      general_df <-
        general_df %>%
        mutate(idRow = 1:n()) %>%
        select(-detailsOwns) %>%
        left_join(detail_df) %>%
        select(-idRow) %>%
        suppressMessages()
    }

    general_df <-
      general_df %>%
      mutate(
        urlTickerRankandFiled = list('http://rankandfiled.com/#/public/', idTicker, '/filings') %>% purrr::invoke(paste0, .),
        urlJSON = url
      )

    if (return_message) {
      list("Parsed: ", url) %>%
        purrr::invoke(paste0, .) %>% message()
    }

    return(general_df)
  }

parse_company_general <-
  function(ticker = "FB",
           nest_data = TRUE,
           return_message = TRUE) {
    data <-
      generate_ticker_general_url(ticker = ticker) %>%
      parse_json_public_general(nest_data = nest_data,
                                return_message = return_message)
    if ('nameEntity' %in% names(data)) {
      data <-
        data %>%
        mutate(nameCompany = nameEntity) %>%
        select(idCIK, idTicker, nameEntity, nameCompany, everything()) %>%
        select(-matches("dateiso"))
    } else {
      df_name <-
        list('http://rankandfiled.com/data/filer/',
             data$idCIK,
             '/general') %>%
        purrr::invoke(paste0, .) %>%
        parse_json_general_filing()

      entity <-
        df_name$nameEntity

      data <-
        data %>%
        mutate(nameEntity = entity,
               nameCompany = nameEntity) %>%
        select(idCIK, idTicker, nameEntity, nameCompany, everything()) %>%
        select(-matches("dateiso"))
    }

    data <-
      data %>%
      resolve_names_to_upper()
    return(data)
  }

parse_json_trades <-
  function(url = "http://rankandfiled.com/data/filer/1326801/trades?start=0",
           return_message = TRUE) {
    if (!url %>% httr::url_ok() %>% suppressWarnings()) {
      return(data_frame())
    }

    json_data <-
      url %>%
      jsonlite::fromJSON()

    options(scipen = 9999)

    cik <-
      url %>% str_replace_all('http://rankandfiled.com/data/filer/|/trades', '') %>%
      str_split('\\?') %>%
      flatten_chr() %>%
      .[[1]] %>%
      as.numeric()

    trade_df <-
      json_data$trades %>%
      as_data_frame() %>%
      dplyr::rename(dateTrade = date) %>%
      mutate(dateTrade = dateTrade %>% lubridate::ymd())

    trade_df <-
      trade_df %>%
      separate(
        trade,
        into = c(
          "idCIKOwner",
          "idCIK",
          "idInsiderType",
          "countSharesOwned",
          "descriptionOption",
          "idTypeInsiderTransaction",
          "amountPrice",
          "countShares",
          "idInsiderTransaction",
          "X10",
          "detailOwnershipIndirect",
          "priceExcercised",
          "dateOptionExcercisable",
          "dateOptionExpiry",
          "countSharesOptions",
          "typeSecurityOption",
          "X17"
        ),
        sep = '\\*'
      ) %>%
      suppressWarnings() %>%
      select(-matches("X"))

    trade_df <-
      trade_df %>%
      mutate_at(.vars =
                  trade_df %>% select(matches("date")) %>% names(),
                .funs = lubridate::ymd) %>%
      mutate_at(.vars =
                  trade_df %>% select(matches("idCIK|count|amount|price")) %>% names(),
                .funs = readr::parse_number) %>%
      left_join(data_frame(
        idInsiderType = c("D", "ND"),
        typeInsider = c("Director", "Non-Director")
      )) %>%
      left_join(get_insider_code_df()) %>%
      left_join(
        data_frame(
          idTypeInsiderTransaction = c("A", "D", "None"),
          typeInsiderTransaction = c('Purchase', 'Sale', 'None'),
          isBought = c(TRUE, FALSE, NA)
        )
      ) %>%
      suppressMessages()

    trade_df <-
      trade_df %>%
      mutate(
        countShares = ifelse(isBought == T, countShares, -countShares),
        amountTransaction = countShares * amountPrice,
        urlJSON = url
      )

    has_indirect_owner <-
      trade_df$detailOwnershipIndirect %>% str_count("By") %>% sum() > 0

    if (has_indirect_owner) {
      trade_df <-
        trade_df %>%
        tidyr::separate(
          detailOwnershipIndirect,
          into = c('remove', "nameOwnerIndirect"),
          remove = FALSE,
          sep = 'By '
        ) %>%
        mutate(nameOwnerIndirect = nameOwnerIndirect %>% str_trim() %>% str_to_upper()) %>%
        select(-remove) %>%
        suppressWarnings()
    }

    if (return_message) {
      list("Parsed: ", url) %>%
        purrr::invoke(paste0, .) %>% message()
    }
    return(trade_df)
  }

parse_trades <-
  function(ticker = "FB",
           nest_data = TRUE,
           return_message = TRUE) {
    general_df <-
      parse_company_general(ticker = ticker, nest_data)

    cik <-
      general_df$idCIK

    trader_url <-
      list("http://rankandfiled.com/data/filer/", cik, '/traders') %>%
      purrr::invoke(paste0, .)

    count_trades <-
      parse_json_traders(url = trader_url) %>%
      .$countTraders %>% unique() %/% 50

    trade_urls <-
      list(
        'http://rankandfiled.com/data/filer/',
        cik,
        '/trades?start=',
        seq(0, by = 50, length.out = count_trades)
      ) %>%
      purrr::invoke(paste0, .)

    parse_json_trades_safe <-
      purrr::possibly(parse_json_trades, NULL)

    all_trades <-
      trade_urls %>%
      map_df(function(x) {
        parse_json_trades_safe(url = x, return_message = return_message)
      }) %>%
      distinct() %>%
      suppressWarnings()

    owners_df <-
      list("http://rankandfiled.com/data/filer/", cik, '/owners') %>%
      purrr::invoke(paste0, .) %>%
      parse_json_owners(nest_data = nest_data)

    entity <-
      general_df$nameEntity

    all_trades <-
      all_trades %>%
      left_join(owners_df %>%
                  select(idCIKOwner = idCIKOwned, nameEntityOwner) %>%
                  distinct()) %>%
      suppressMessages()

    entity_name <-
      general_df$nameEntity

    all_trades <-
      all_trades %>%
      mutate(nameEntity = entity_name,
             idTicker = ticker) %>%
      select(idCIK,
             nameEntity,
             idTicker,
             dateTrade,
             idCIKOwner,
             nameEntityOwner,
             everything()) %>%
      suppressWarnings() %>%
      suppressMessages()

    all_trades <-
      all_trades %>%
      mutate_at(.vars = all_trades %>% select(matches("count")) %>% names,
                funs(. %>% formattable::comma(digits = 0))) %>%
      mutate_at(.vars = all_trades %>% select(matches("amount|price")) %>% names,
                funs(. %>% formattable::currency(digits = 2))) %>%
      select(idCIK:countShares, amountTransaction, everything()) %>%
      resolve_names_to_upper()

    if (return_message) {
      list("Parsed ", all_trades %>% nrow(), ' trades for ', entity) %>%
        purrr::invoke(paste0, .) %>%
        message()
    }

    return(all_trades)

  }

parse_public_filings <-
  function(ticker = "FB",
           return_message = TRUE) {
    general_df <-
      parse_company_general(ticker = ticker)

    cik <-
      general_df$idCIK

    filing_pages <-
      general_df$countFilings %/% 50

    filing_urls <-
      list(
        'http://rankandfiled.com/data/filer/',
        cik,
        '/all?start=',
        seq(0, by = 50, length.out = filing_pages)
      ) %>%
      purrr::invoke(paste0, .)

    parse_json_public_filers_safe <-
      purrr::possibly(parse_json_public_filers, NULL)

    all_filings <-
      filing_urls %>%
      map_df(function(x) {
        parse_json_public_filers_safe(url = x, return_message = return_message)
      }) %>%
      distinct() %>%
      suppressWarnings()

    entity <-
      general_df$nameEntity

    all_filings <-
      all_filings %>%
      mutate(idTicker = ticker,
             nameCompany = entity,
             nameEntity = entity) %>%
      select(idCIK,
             idTicker,
             nameEntity,
             nameCompany,
             dateFiling,
             idRF,
             everything())

    if (return_message) {
      list("Parsed ", all_filings %>% nrow(), ' SEC Filings for ', entity) %>%
        purrr::invoke(paste0, .) %>%
        message()
    }
    return(all_filings)
  }

parse_ticker_data <-
  function(ticker = "FB",
           nest_data = TRUE,
           tables = NULL,
           return_message = TRUE) {
    if (tables %>% is_null()) {
      tables <-
        c(
          'General',
          'CIK Filings',
          'Filings',
          'Private Offerings',
          'Related Parties',
          'Traders',
          'C Level',
          'MDA',
          'Owners',
          'Insider Trades',
          'Trades',
          'Subsidiaries'
        )
    }
    ticker <-
      ticker %>% str_to_upper()

    parse_company_general_safe <-
      purrr::possibly(parse_company_general, NULL)

    parse_trades_safe <-
      purrr::possibly(parse_trades, NULL)

    parse_public_filings_safe <-
      purrr::possibly(parse_public_filings, NULL)

    general <-
      parse_company_general_safe(ticker = ticker,
                                 nest_data = nest_data,
                                 return_message = return_message) %>%
      suppressWarnings()

    has_trades <-
      "TRADES" %>% str_detect(tables %>% str_to_upper()) %>% sum() > 0

    if (has_trades) {
      trades <-
        parse_trades_safe(ticker = ticker,
                          nest_data = nest_data,
                          return_message = return_message) %>%
        suppressWarnings()
    } else {
      trades <-
        data_frame(idTicker = ticker)
    }

    cik_data <-
      general$idCIK %>%
      parse_cik_data(tables = tables,
                     nest_data = nest_data,
                     return_message = return_message)

    if ('General' %in% cik_data$nameTable) {
      cik_data <-
        cik_data %>%
        filter(!nameTable == 'General')
    }
    all_data <-
      data_frame(
        nameEntity = general$nameEntity,
        idCIK = general$idCIK,
        nameTable = c('Company Profile', 'Insider Trades'),
        dataTable = list(general, trades)
      ) %>%
      bind_rows(cik_data) %>%
      mutate(countCols = dataTable %>% map_dbl(ncol)) %>%
      filter(countCols > 1) %>%
      select(-countCols)

    return(all_data)

    if (return_message) {
      list("Acquired all data for ", all_data$nameEntity %>% unique()) %>%
        purrr::invoke(paste0, .) %>%
        message()
    }

  }

#' US Public Company snapshot
#'
#' This function returns snapshot details
#' of X public companies.  Information includes
#' corporate metadata, valuation metrics, and more.
#'
#' @param merge_type how to merge general information for public companies \itemize{
#' \item \code{NULL} and \code{MATCH}: only acquires metadata for unmatched batch import companies (default)
#' #' \item \code{ALL}: returns general information for all companies
#' }
#' @param return_message \code{TRUE} return a message after data import
#'
#' @return a \code{data_frame}
#' @export
#' @import dplyr tidyr purrr stringr formattable readr lubridate
#' @importFrom jsonlite fromJSON
#' @family SEC
#' @family real-time data
#' @family Rank and Filed
#' @family entity search
#' @examples
#' \dontrun{
#' get_data_us_public_companies(merge_type = NULL)
#'
#' }
get_data_us_public_companies <-
  function(merge_type = NULL,
           return_message = TRUE) {
    no_merge <-
      (!'merge_type' %>% exists()) |
      (merge_type %>% purrr::is_null())

    if (no_merge) {
      merge_type <-
        'MATCH'
    }

    json_data <-
      "http://rankandfiled.com/data/public_companies" %>%
      jsonlite::fromJSON()

    company_data <-
      data_frame(df = json_data$result$data %>%
                   str_split(pattern = '\\|') %>%
                   flatten_chr()) %>%
      tidyr::separate(
        df,
        sep = '\\*',
        into = c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9")
      ) %>%
      mutate_at(.vars = c("X5", "X6", "X7", "X8", "X9"),
                .funs = readr::parse_number) %>%
      purrr::set_names(
        c(
          'idTicker',
          'idExchange',
          'codeLocationBusiness',
          'codeLocationIncorporation',
          'idSector',
          'amountEquityMarketCap',
          'priceOpen',
          'price52WeekLow',
          'price52WeekHigh'
        )
      ) %>%
      left_join(data_frame(
        idSector = 1:12,
        nameSector = c(
          'Finance',
          'Capital Goods',
          'Technology',
          'Transportation',
          'Consumer Services',
          'Health Care',
          'Consumer Durables',
          'Public Utilities',
          'Miscellaneous',
          'Basic Industries',
          'Energy',
          'Consumer Non Durables'
        )
      )) %>%
      suppressMessages() %>%
      left_join(data_frame(
        idExchange = c('N', 'Q', 'A'),
        nameExchange = c('NYSE', 'NASDAQ', 'NYSE ARCA')
      )) %>%
      mutate(
        amountEquityMarketCap = ifelse(
          idTicker == 'BRK-A',
          amountEquityMarketCap * 100000000000,
          amountEquityMarketCap
        ),
        codeLocationBusiness = ifelse(
          codeLocationBusiness == '',
          codeLocationIncorporation,
          codeLocationBusiness
        ),
        codeLocationIncorporation = ifelse(codeLocationIncorporation == '',
                                           NA,
                                           codeLocationIncorporation),
        countSharesOutstanding = ifelse(priceOpen > 0,
                                        ((
                                          amountEquityMarketCap / priceOpen
                                        )),
                                        NA),
        pct52WeekHigh = ifelse(priceOpen > 0,
                               ((
                                 priceOpen / price52WeekHigh
                               )),
                               NA),
        pct52WeekLow = ifelse(priceOpen > 0,
                              ((
                                priceOpen / price52WeekLow
                              )),
                              NA),
        amountEquityMarketCap = (amountEquityMarketCap),
        urlTickerRankandFiled = list('http://rankandfiled.com/#/public/', idTicker, '/filings') %>% purrr::invoke(paste0, .)
      ) %>%
      select(idTicker:idSector, nameSector, everything()) %>%
      suppressMessages()

    countries <-
      get_data_location_codes()

    company_data <-
      company_data %>%
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
      suppressMessages()

    company_data <-
      company_data %>%
      filter(priceOpen > 0) %>%
      filter(!priceOpen %>% is.na()) %>%
      group_by(idTicker, nameSector) %>%
      filter(amountEquityMarketCap == max(amountEquityMarketCap, na.rm = TRUE)) %>%
      ungroup() %>%
      arrange(idTicker)

    ticker_count_df <-
      company_data %>%
      count(idTicker, sort = TRUE)

    fine_tickers <-
      ticker_count_df %>% filter(n < 2) %>% .$idTicker

    fine_df <-
      company_data %>%
      filter(idTicker %in% (fine_tickers))

    dup_count_df <-
      ticker_count_df %>% filter(n > 1)

    dup_df <-
      company_data %>%
      filter(idTicker %in% dup_count_df$idTicker) %>%
      arrange(idTicker)

    dup_general_df <-
      dup_count_df$idTicker %>%
      map_df(function(x) {
        parse_company_general(ticker = x)
      }) %>%
      arrange(idTicker)

    dup_df <-
      dup_general_df %>%
      select(idTicker, nameLocationBusiness = stateEntity, nameSector) %>%
      left_join(countries %>% dplyr::rename(nameLocationBusiness = nameLocation)) %>%
      dplyr::rename(codeLocationBusiness = codeLocation) %>%
      left_join(dup_df) %>%
      suppressMessages()

    company_data <-
      fine_df %>%
      bind_rows(dup_df) %>%
      arrange(idTicker)

    is_merge_all <-
      merge_type %>% str_to_upper() == 'ALL'

    is_match <-
      merge_type %>% str_to_upper() == 'MATCH'

    if (is_merge_all) {
      general_data <-
        company_data$idTicker %>%
        unique() %>%
        map_df(function(x) {
          parse_company_general(ticker = x, return_message = return_message)
        }) %>%
        suppressWarnings()

      company_data <-
        company_data %>%
        inner_join(general_data %>%
                     select(-one_of(
                       c(
                         "idExchange",
                         "nameSector",
                         "amountEquityMarketCap",
                         "priceOpen",
                         "price52WeekLow",
                         "price52WeekHigh",
                         "urlTickerRankandFiled"
                       )
                     ))) %>%
        dplyr::rename(nameCompany = nameEntity) %>%
        select(idTicker,
               nameCompany,
               idCIK,
               idSector,
               nameSector,
               nameExchange,
               everything())

      if (return_message) {
        list(
          "Acquired data for ",
          company_data %>% nrow() %>% formattable::comma(digits = 0),
          ' US stocks with a combined market capitalization of ',
          company_data$amountEquityMarketCap %>% sum(na.rm = TRUE) %>% formattable::currency(digits = 0)
        ) %>%
          purrr::invoke(paste0, .) %>%
          message()
      }

      return(company_data)
    }

    if (is_match) {
      all_tickers <-
        get_data_rf_us_tickers()

      company_data <-
        company_data %>%
        left_join(
          all_tickers %>%
            filter(!urlTickerRankandFiled %>% is.na()) %>%
            select(
              idTicker,
              idCIK,
              nameCompany,
              codeLocationBusiness,
              idSIC,
              classificationSIC
            )
        ) %>%
        suppressMessages()

      count_df <-
        company_data %>%
        count(idTicker, sort = TRUE)

      dup_tickers <-
        count_df %>%
        filter(n > 1) %>%
        .$idTicker %>%
        unique()

      fine_df <-
        company_data %>%
        filter(!idTicker %in% dup_tickers)

      dup_df <-
        company_data %>%
        filter(idTicker %in% dup_tickers)
      parse_company_general_safe <-
        purrr::possibly(parse_company_general, data_frame)
      dup_general_df <-
        dup_tickers %>%
        map_df(function(x) {
          parse_company_general(ticker = x)
        }) %>%
        arrange(idTicker) %>%
        suppressWarnings()

      dup_df <-
        dup_df %>%
        select(-c(nameCompany, idCIK, idSIC, classificationSIC)) %>%
        distinct() %>%
        left_join(dup_general_df %>%
                    select(idTicker, idCIK, nameCompany = nameEntity)) %>%
        left_join(all_tickers %>%
                    select(idCIK, idSIC, classificationSIC)) %>%
        suppressWarnings() %>%
        suppressMessages()

      company_data <-
        fine_df %>%
        bind_rows(dup_df) %>%
        distinct()

      match_df <-
        company_data %>%
        filter(!nameCompany %>% is.na())

      missing_name_df <-
        company_data %>%
        filter(nameCompany %>% is.na()) %>%
        select(-c(nameCompany, idCIK)) %>%
        inner_join(all_tickers %>% select(idTicker, nameCompany, idCIK)) %>%
        suppressMessages()

      count_df <-
        missing_name_df %>%
        count(idTicker, sort = TRUE)

      dup_tickers <-
        count_df %>%
        filter(n > 1) %>%
        .$idTicker %>%
        unique()

      fine_df <-
        missing_name_df %>%
        filter(!idTicker %in% dup_tickers)

      dup_df <-
        missing_name_df %>%
        filter(idTicker %in% dup_tickers)

      dup_general_df <-
        dup_tickers %>%
        map_df(function(x) {
          parse_company_general(ticker = x)
        }) %>%
        arrange(idTicker) %>%
        suppressWarnings()

      dup_df <-
        dup_df %>%
        select(-c(nameCompany, idCIK)) %>%
        distinct() %>%
        left_join(dup_general_df %>% select(idTicker, nameCompany = nameEntity, idCIK)) %>%
        suppressMessages()

      missing_name_df <-
        fine_df %>%
        bind_rows(dup_df) %>%
        select(-c(idSIC, classificationSIC)) %>%
        left_join(all_tickers %>%
                    select(idCIK, idSIC, classificationSIC)) %>%
        suppressMessages()

      company_data <-
        match_df %>%
        bind_rows(missing_name_df) %>%
        arrange(desc(amountEquityMarketCap)) %>%
        select(
          idCIK,
          idTicker,
          nameCompany,
          idExchange,
          idSector,
          nameSector,
          idSIC,
          classificationSIC,
          everything()
        )

      company_data <-
        company_data %>%
        mutate_at(company_data %>% select(matches("price")) %>% names(),
                  funs(. %>% formattable::currency(digits = 2))) %>%
        mutate_at(company_data %>% select(matches("amount")) %>% names(),
                  funs(. %>% formattable::currency(digits = 0))) %>%
        mutate_at(company_data %>% select(matches("pct")) %>% names(),
                  funs(. %>% formattable::percent(digits = 2)))
      if (return_message) {
        list(
          "Acquired data for ",
          company_data %>% nrow() %>% formattable::comma(digits = 0),
          ' US Stocks with a combined market capitalization of ',
          company_data$amountEquityMarketCap %>% sum(na.rm = TRUE) %>% formattable::currency(digits = 0)
        ) %>%
          purrr::invoke(paste0, .) %>%
          message()
      }

      company_data <-
        company_data %>%
        resolve_names_to_upper()

      return(company_data)

    }
  }

# SEC - Subsidiary --------------------------------------------------------

parse_sec_url_for_cik <-
  function(url) {
    url %>%
      str_replace_all("https://www.sec.gov/Archives/edgar/data/", '') %>%
      str_split('\\/') %>%
      flatten_chr() %>%
      .[[1]] %>%
      as.numeric()
  }

get_loc_df <-
  function() {
    data_frame(
      nameLocation = c(
        "AFGHANISTAN",
        "ALAND ISLANDS",
        "ALBANIA",
        "ALGERIA",
        "AMERICAN SAMOA",
        "ANDORRA",
        "ANGOLA",
        "ANGUILLA",
        "ANTARCTICA",
        "ANTIGUA AND BARBUDA",
        "ARGENTINA",
        "ARMENIA",
        "ARUBA",
        "AUSTRALIA",
        "AUSTRIA",
        "AUSTRIA-HUNGARY",
        "AZERBAIJAN",
        "BADEN",
        "BAHAMAS",
        "BAHRAIN",
        "BANGLADESH",
        "BARBADOS",
        "BAVARIA",
        "BELARUS",
        "BELGIUM",
        "BELIZE",
        "BENIN",
        "BERMUDA",
        "BHUTAN",
        "BOLIVIA, PLURINATIONAL STATE OF",
        "BONAIRE, SINT EUSTATIUS AND SABA",
        "BOSNIA AND HERZEGOVINA",
        "BOTSWANA",
        "BOUVET ISLAND",
        "BRAZIL",
        "BRITISH INDIAN OCEAN TERRITORY",
        "BRUNEI DARUSSALAM",
        "BULGARIA",
        "BURKINA FASO",
        "BURUNDI",
        "CAMBODIA",
        "CAMEROON",
        "CANADA",
        "CABO VERDE",
        "CAYMAN ISLANDS",
        "CENTRAL AFRICAN REPUBLIC",
        "CHAD",
        "CHILE",
        "CHINA",
        "CHRISTMAS ISLAND",
        "COCOS (KEELING) ISLANDS",
        "COLOMBIA",
        "COMOROS",
        "CONGO, THE DEMOCRATIC REPUBLIC OF THE",
        "CONGO",
        "COOK ISLANDS",
        "COSTA RICA",
        "COTE D'IVOIRE",
        "CROATIA",
        "CUBA",
        "CURACAO",
        "CYPRUS",
        "CZECH REPUBLIC",
        "CZECHOSLOVAKIA",
        "DENMARK",
        "DJIBOUTI",
        "DOMINICA",
        "DOMINICAN REPUBLIC",
        "ECUADOR",
        "EGYPT",
        "EL SALVADOR",
        "EQUATORIAL GUINEA",
        "ERITREA",
        "ESTONIA",
        "ETHIOPIA",
        "FALKLAND ISLANDS (MALVINAS)",
        "FAROE ISLANDS",
        "FIJI",
        "FINLAND",
        "FRANCE",
        "FRENCH GUIANA",
        "FRENCH POLYNESIA",
        "FRENCH SOUTHERN TERRITORIES",
        "GABON",
        "GAMBIA",
        "GEORGIA",
        "GERMAN DEMOCRATIC REPUBLIC",
        "FEDERAL REPUBLIC OF GERMANY",
        "GERMANY",
        "GHANA",
        "GIBRALTAR",
        "GREECE",
        "GREENLAND",
        "GRENADA",
        "GUADELOUPE",
        "GUAM",
        "GUATEMALA",
        "GUERNSEY",
        "GUINEA",
        "GUINEA-BISSAU",
        "GUYANA",
        "HAITI",
        "HANOVER",
        "HEARD ISLAND AND MCDONALD ISLANDS",
        "HESSE ELECTORAL",
        "HESSE GRAND DUCAL",
        "HOLY SEE (VATICAN CITY STATE)",
        "HONDURAS",
        "HONG KONG",
        "HUNGARY",
        "ICELAND",
        "INDIA",
        "INDONESIA",
        "IRAN, ISLAMIC REPUBLIC OF",
        "IRAQ",
        "IRELAND",
        "ISLE OF MAN",
        "ISRAEL",
        "ITALY",
        "JAMAICA",
        "JAPAN",
        "JERSEY",
        "JORDAN",
        "KAZAKHSTAN",
        "KENYA",
        "KIRIBATI",
        "KOREA",
        "KOREA, DEMOCRATIC PEOPLE'S REPUBLIC OF",
        "KOREA, REPUBLIC OF",
        "KOSOVO",
        "KUWAIT",
        "KYRGYZSTAN",
        "LAO PEOPLE'S DEMOCRATIC REPUBLIC",
        "LATVIA",
        "LEBANON",
        "LESOTHO",
        "LIBERIA",
        "LIBYA",
        "LIECHTENSTEIN",
        "LITHUANIA",
        "LUXEMBOURG",
        "MACAO",
        "MACEDONIA, THE FORMER YUGOSLAV REPUBLIC OF",
        "MADAGASCAR",
        "MALAWI",
        "MALAYSIA",
        "MALDIVES",
        "MALI",
        "MALTA",
        "MARSHALL ISLANDS",
        "MARTINIQUE",
        "MAURITANIA",
        "MAURITIUS",
        "MAYOTTE",
        "MECKLENBURG SCHWERIN",
        "MEXICO",
        "MICRONESIA, FEDERATED STATES OF",
        "MODENA",
        "MOLDOVA, REPUBLIC OF",
        "MONACO",
        "MONGOLIA",
        "MONTENEGRO",
        "MONTSERRAT",
        "MOROCCO",
        "MOZAMBIQUE",
        "MYANMAR",
        "NAMIBIA",
        "NAURU",
        "NEPAL",
        "NETHERLANDS",
        "NETHERLANDS ANTILLES",
        "NEW CALEDONIA",
        "NEW ZEALAND",
        "NICARAGUA",
        "NIGER",
        "NIGERIA",
        "NIUE",
        "NORFOLK ISLAND",
        "NORTHERN MARIANA ISLANDS",
        "NORWAY",
        "OMAN",
        "PAKISTAN",
        "PALAU",
        "PALESTINE, STATE OF",
        "PANAMA",
        "PAPUA NEW GUINEA",
        "PARAGUAY",
        "PARMA",
        "PERU",
        "PHILIPPINES",
        "PITCAIRN",
        "POLAND",
        "PORTUGAL",
        "PUERTO RICO",
        "QATAR",
        "REPUBLIC OF VIETNAM",
        "REUNION",
        "ROMANIA",
        "RUSSIAN FEDERATION",
        "RWANDA",
        "SAINT BARTHELEMY",
        "SAINT HELENA, ASCENSION AND TRISTAN DA CUNHA",
        "SAINT KITTS AND NEVIS",
        "SAINT LUCIA",
        "SAINT MARTIN (FRENCH PART)",
        "SAINT PIERRE AND MIQUELON",
        "SAINT VINCENT AND THE GRENADINES",
        "SAMOA",
        "SAN MARINO",
        "SAO TOME AND PRINCIPE",
        "SAUDI ARABIA",
        "SAXONY",
        "SENEGAL",
        "SERBIA",
        "SEYCHELLES",
        "SIERRA LEONE",
        "SINGAPORE",
        "SINT MAARTEN (DUTCH PART)",
        "SLOVAKIA",
        "SLOVENIA",
        "SOLOMON ISLANDS",
        "SOMALIA",
        "SOUTH AFRICA",
        "SOUTH GEORGIA AND THE SOUTH SANDWICH ISLANDS",
        "SOUTH SUDAN",
        "SPAIN",
        "SRI LANKA",
        "SUDAN",
        "SURINAME",
        "SVALBARD AND JAN MAYEN",
        "SWAZILAND",
        "SWEDEN",
        "SWITZERLAND",
        "SYRIAN ARAB REPUBLIC",
        "TAIWAN, PROVINCE OF CHINA",
        "TAJIKISTAN",
        "TANZANIA, UNITED REPUBLIC OF",
        "THAILAND",
        "TIMOR-LESTE",
        "TOGO",
        "TOKELAU",
        "TONGA",
        "TRINIDAD AND TOBAGO",
        "TUNISIA",
        "TURKEY",
        "TURKMENISTAN",
        "TURKS AND CAICOS ISLANDS",
        "TUSCANY",
        "TUVALU",
        "TWO SICILIES",
        "UGANDA",
        "UKRAINE",
        "UNITED ARAB EMIRATES",
        "UNITED KINGDOM",
        "UNITED STATES",
        "UNITED STATES MINOR OUTLYING ISLANDS",
        "URUGUAY",
        "UZBEKISTAN",
        "VANUATU",
        "VENEZUELA, BOLIVARIAN REPUBLIC OF",
        "VIET NAM",
        "VIRGIN ISLANDS, BRITISH",
        "VIRGIN ISLANDS, U.S.",
        "WALLIS AND FUTUNA",
        "WESTERN SAHARA",
        "WUERTTEMBURG",
        "YEMEN",
        "YEMEN ARAB REPUBLIC",
        "YEMEN PEOPLE'S REPUBLIC",
        "YUGOSLAVIA",
        "ZAMBIA",
        "ZANZIBAR",
        "ZIMBABWE",
        "ALABAMA",
        "ALASKA",
        "ARIZONA",
        "ARKANSAS",
        "CALIFORNIA",
        "COLORADO",
        "CONNECTICUT",
        "DELAWARE",
        "FLORIDA",
        "GEORGIA",
        "HAWAII",
        "IDAHO",
        "ILLINOIS",
        "INDIANA",
        "IOWA",
        "KANSAS",
        "KENTUCKY",
        "LOUISIANA",
        "MAINE",
        "MARYLAND",
        "MASSACHUSETTS",
        "MICHIGAN",
        "MINNESOTA",
        "MISSISSIPPI",
        "MISSOURI",
        "MONTANA",
        "NEBRASKA",
        "NEVADA",
        "NEW HAMPSHIRE",
        "NEW JERSEY",
        "NEW MEXICO",
        "NEW YORK",
        "NORTH CAROLINA",
        "NORTH DAKOTA",
        "OHIO",
        "OKLAHOMA",
        "OREGON",
        "PENNSYLVANIA",
        "RHODE ISLAND",
        "SOUTH CAROLINA",
        "SOUTH DAKOTA",
        "TENNESSEE",
        "TEXAS",
        "UTAH",
        "VERMONT",
        "VIRGINIA",
        "WASHINGTON",
        "WEST VIRGINIA",
        "WISCONSIN",
        "WYOMING",
        "DISTRICT OF COLUMBIA",
        "ENGLAND",
        "BRITISH VIRGIN ISLANDS",
        "NETHERLAND ANTILLES",
        "RUSSIA",
        "SOUTH KOREA",
        'TAIWAN',
        "VENEZUELA",
        'CHANNEL ISLANDS'
      )
    )
  }

parse_page_sub_multi_item_html <-
  function(page) {
    locations <-
      get_loc_df() %>%
      .$nameLocation
    subsidiaries <-
      page %>%
      html_nodes('td div') %>%
      html_text() %>%
      str_replace_all('\u0095 |\u0096|\u0095\n', '') %>%
      str_trim()

    subsidiaries <-
      subsidiaries[!subsidiaries == '']

    data_nodes <-
      page %>%
      html_nodes('td') %>%
      html_text() %>%
      str_replace_all('\u0095 |\u0096|\u0095\n', '') %>%
      str_trim() %>%
      str_to_upper()

    data_nodes <-
      data_nodes[!data_nodes == '']

    location_items <-
      data_nodes[data_nodes %in% locations]

    pct_vals <-
      data_frame(value = data_nodes) %>%
      filter(!value %>% str_detect("\\([(1-9)]\\)")) %>%
      mutate(pctSubsidiaryOwned = value %>% as.numeric()) %>%
      filter(!pctSubsidiaryOwned %>% is.na()) %>%
      slice(1:length(subsidiaries)) %>%
      .$pctSubsidiaryOwned / 100 %>%
      suppressWarnings() %>%
      suppressMessages()

    all_data <-
      data_frame(
        nameSubsidiary = subsidiaries,
        nameLocationSubsidiary = location_items,
        pctSubsidiaryOwned = pct_vals
      ) %>%
      mutate(nameSubsidiary = nameSubsidiary %>% str_to_upper())

    return(all_data)
  }

parse_page_subsidiary_table_html <-
  function(page,
           numbers = 1:10,
           hit_terms = c(
             "Organized",
             "STATE OR|STATE OF|JURISDICTION OF|JURISDICTION OF INCORPORATION OR ORGANIZATION|JURISDICTION|JURISDICTION OF INCORPORATION OR\nORGANIZATION",
             "NAME|ORGANIZED UNDER THE LAWS OF",
             'STATE OF ORGANIZATION',
             'STATE OR COUNTRY OF ORGANIZATION',
             'NAME OF SUBSIDIARY',
             'NAME',
             'ENTITY NAME',
             'the laws of',
             'Percentage of voting',
             'securities owned by',
             'immediate parent',
             'CERTAIN INTERMEDIARY SUBSIDIARIES',
             'Note:',
             'Organized',
             'Under the',
             'Laws of',
             'OWNED BY',
             'IMMEDIATE',
             'PARENT',
             "OWNS",
             "CERTAIN INTERMEDIARY SUBSIDIARIES",
             'PERCENTAGE',
             'OF VOTING',
             'SECURITIES'
           )) {
    is_ib1 <-
      page %>%
      html_nodes('b font') %>%
      html_text() %>% length() > 0

    if (is_ib1) {
      items_bold <-
        page %>%
        html_nodes('b font') %>%
        html_text() %>%
        str_to_upper() %>%
        str_replace_all('\n', ' ') %>%
        str_split('\\–') %>%
        flatten_chr() %>%
        str_trim()
    } else {
      items_bold <-
        page %>%
        html_nodes('b') %>%
        html_text() %>%
        str_to_upper() %>%
        str_replace_all('\n', ' ') %>%
        str_split('\\–') %>%
        flatten_chr() %>%
        str_trim() %>%
        unique()
    }

    has_date <-
      items_bold %>% grep(month.name %>% str_to_upper() %>% paste(collapse = '|'), .) %>% length > 0

    if (has_date) {
      date_data <-
        items_bold[items_bold %>% grep(month.name %>% str_to_upper() %>% paste(collapse = '|'), .)] %>%
        lubridate::mdy()
    } else {
      date_data <-
        NA
    }

    hit_terms <-
      hit_terms %>%
      append(items_bold) %>%
      str_to_upper() %>%
      unique() %>%
      append(list('(', letters, ')') %>%
               purrr::invoke(paste0, .)) %>%
      paste0(collapse = '|')


    hit_terms_in <-
      hit_terms %>% str_split('\\|') %>%
      flatten_chr()

    locations <-
      get_loc_df() %>%
      .$nameLocation

    all_data <-
      numbers %>%
      map_df(function(x) {
        css_selector <-
          paste0('td:nth-child(', x, ')')
        has_length <-
          page %>%
          html_nodes(css_selector) %>% length() > 0
        if (has_length) {
          item <-
            paste0("X" , x)

          value <-
            page %>%
            html_nodes(css_selector) %>%
            html_text() %>%
            str_trim()
          data_frame(item, value)
        }
      }) %>%
      mutate(
        value = value %>% str_to_upper() %>% str_replace_all('\n  ', ' ') %>% str_replace_all('\u0096 ', '')
      ) %>%
      filter(!value == '')

    has_loc_key <-
      all_data %>%
      filter(value %in% locations) %>%
      nrow() > 0

    if (has_loc_key) {
      loc_cols <-
        all_data %>%
        filter(value %in% locations) %>%
        .$item %>%
        unique()
      if (loc_cols %>% length == 1) {
        loc_col <-
          loc_cols[[1]]
      }
    }

    has_pct <-
      all_data %>%
      filter(value %>% str_detect("PERCENT")) %>%
      .$item %>% unique() %>% length() > 0

    if (has_pct) {
      pct_col <-
        all_data %>%
        filter(value %>% str_detect("PERCENT")) %>%
        .$item %>% unique()
    } else {
      pct_col <-
        NA
    }

    is_whack <-
      pct_col[[1]] %in% loc_cols

    if (is_whack) {
      all_data <-
        page %>%
        parse_page_sub_multi_item_html() %>%
        mutate(dateSubsidiaryAsOf = date_data)

      return(all_data)
    }

    all_data <-
      all_data %>%
      filter(!value %in% items_bold) %>%
      filter(!value %>% str_detect(paste0(items_bold %>% unique(), collapse = '|'))) %>%
      filter(!value %in% hit_terms_in) %>%
      filter(!value %>% str_detect(hit_terms))

    count_df <-
      all_data %>% count(item, sort = T) %>%
      arrange(item) %>%
      spread(item, n)

    off_one <-
      (count_df[, 2] %>% extract2(1)) - (count_df[, 1] %>% extract2(1)) == 1

    min_item <-
      count_df %>% gather(item, value) %>% filter(value == min(value)) %>% .$item

    change_pct <-
      has_pct & (pct_col == min_item) %>% sum() > 0

    if (change_pct) {
      pct_col <-
        names(count_df)[[3]]
    }

    if (off_one) {
      df <-
        all_data$item %>% unique() %>%
        map_df(function(x) {
          has_data <-
            all_data %>%
            filter(item == x) %>%
            filter(!value %>% is.na()) %>%
            filter(!value == '') %>%
            nrow()

          if (has_data) {
            all_data %>%
              filter(item == x) %>%
              filter(!value %>% is.na()) %>%
              filter(!value == '') %>%
              filter(!value %>% str_detect(hit_terms)) %>%
              mutate(idSubsidiary = 1:n())
          }
        }) %>%
        filter(!value %>% str_detect(hit_terms)) %>%
        spread(item, value)

      if (change_pct) {
        df <-
          df %>%
          select(-one_of(min_item))
      }
    }

    if (!off_one) {
      has_property <-
        items_bold %>% str_detect('PROPERTY') %>% sum() > 0
      if (has_property) {
        tables <-
          page %>%
          html_table(fill = T)
        df <-
          1:length(tables) %>%
          map_df(function(x) {
            table_df <-
              tables[[x]] %>%
              data.frame(stringsAsFactors = FALSE) %>%
              as_data_frame()

            column_df <-
              table_df %>% slice(1) %>%
              gather(column, value) %>%
              mutate(idColumn = 1:n()) %>%
              filter(!value %>% is.na()) %>%
              left_join(data_frame(
                value = c(
                  "PROPERTY",
                  "ENTITIES",
                  "STATE OF FORMATION",
                  "DATE OF FORMATION",
                  " ",
                  'General Information:'
                ),
                nameItem = c(
                  'nameProperty',
                  'nameSubsidiary',
                  'locationOrganizationSubsidiary',
                  'dateSubsidiaryFormed',
                  'locationOrganizationSubsidiary',
                  'nameSubsidiary'
                )
              )) %>%
              suppressMessages()
            two_col <-
              column_df %>% nrow() == 2
            if (two_col) {
              column_df$nameItem[[2]] <-
                'locationOrganizationSubsidiary'
            }

            columns_keep <-
              column_df$idColumn

            table_df <-
              table_df <-
              table_df %>%
              select(columns_keep) %>%
              slice(-1) %>%
              purrr::set_names(column_df$nameItem)

            table_df <-
              table_df %>%
              mutate_all(funs(. %>% str_trim() %>% str_to_upper())) %>%
              mutate(nameSubsidiary = ifelse(nameSubsidiary == '', NA, nameSubsidiary)) %>%
              filter(!nameSubsidiary %>% is.na())


            if (two_col) {
              table_df <-
                table_df %>%
                tidyr::separate(
                  locationOrganizationSubsidiary,
                  into = c(
                    'locationOrganizationSubsidiary',
                    'dateSubsidiaryFormed'
                  ),
                  sep = 'FORMED'
                ) %>%
                suppressWarnings() %>%
                mutate(locationOrganizationSubsidiary = locationOrganizationSubsidiary %>% str_replace_all('\\,', '')) %>%
                mutate_all(funs(. %>% str_replace('\n', '') %>% str_trim()))
            }


            if ('nameProperty' %in% names(table_df)) {
              table_df <-
                table_df %>%
                mutate(nameProperty = ifelse(nameProperty == '', NA, nameProperty)) %>%
                mutate_all(funs(. %>% str_replace('\n|\n  |\n  ', '') %>% str_trim())) %>%
                mutate_all(funs(. %>% str_replace('\n', '') %>% str_trim())) %>%
                mutate_all(funs(. %>% str_replace('  ', ' ') %>% str_trim())) %>%
                fill(nameProperty)

            }

            return(table_df)
          })

        if ('dateSubsidiaryFormed' %in% names(df)) {
          df <-
            df %>%
            mutate(dateSubsidiaryFormed = dateSubsidiaryFormed %>% lubridate::mdy())
        }

        df <-
          df %>%
          mutate(idCIK = cik, urlSEC = url) %>%
          select(idCIK, nameSubsidiary, everything()) %>%
          mutate(
            locationOrganizationSubsidiary = locationOrganizationSubsidiary %>% str_replace_all(
              'A |LIMITED LIABILITY COMPANY|CORPORATION|LIMITED PARTNERSHIP'
            ) %>% str_trim()
          )

        return(df)
      }
      if (!has_property) {
        df <-
          all_data %>%
          mutate(value = ifelse(value == '', NA, value)) %>%
          filter(!value %>% is.na()) %>%
          group_by(item) %>%
          mutate(idSubsidiary = 1:n()) %>%
          spread(item, value) %>%
          filter(!X1 == '') %>%
          mutate(idSubsidiary = 1:n()) %>%
          gather(item, value, -c(X1, idSubsidiary)) %>%
          ungroup() %>%
          filter(!value %>% str_detect(hit_terms)) %>%
          spread(item, value)
      }

    }

    df <-
      df %>%
      dplyr::rename(nameSubsidiary = X1) %>%
      tidyr::separate(nameSubsidiary,
                      sep = '\\(',
                      into = c('nameSubsidiary', 'remove')) %>%
      select(-matches("remove")) %>%
      mutate(nameSubsidiary = nameSubsidiary %>% str_trim()) %>%
      suppressWarnings() %>%
      select(-matches("idSubsidiary"))

    if (has_pct) {
      names(df)[names(df) %>% grep(pct_col, .)] <-
        'pctSubsidiaryOwned'

      df <-
        df %>%
        mutate_at(df %>% select(matches('pct')) %>% names(),
                  funs(. %>% as.numeric() / 100)) %>%
        suppressWarnings()
    }

    if (has_loc_key) {
      names(df)[names(df) %>% grep(loc_col, .)] <-
        'locationOrganizationSubsidiary'
    }

    df <-
      df %>%
      select(-matches("X"))

    return(df)
  }

parse_sec_subsidiary_url_html <-
  function(url = "https://www.sec.gov/Archives/edgar/data/34088/000003408816000065/xomexhibit21.htm",
           return_message = TRUE) {
    cik <-
      url %>%
      parse_sec_url_for_cik()

    page <-
      url %>%
      read_html()

    is_zero <-
      page %>%
      html_nodes(paste0('td:nth-child(', 1, ')')) %>%
      length() == 0
    locations <-
      get_loc_df() %>%
      .$nameLocation

    if (is_zero) {
      data <-
        page %>%
        html_nodes('font') %>%
        html_text() %>%
        str_replace_all('\\ ', ' ')

      data <-
        data[!data == '']


      is_parenth <-
        data %>% str_detect('\\(') %>% sum() / length(data) > .25

      if (is_parenth) {
        data <-
          data[data %>% str_detect('\\(')]

        df <-
          data_frame(data) %>%
          separate(
            data,
            sep = '\\(',
            into = c('nameSubsidiary', 'locationOrganizationSubsidiary')
          ) %>%
          separate(
            locationOrganizationSubsidiary,
            sep = '\\)',
            into = c('locationOrganizationSubsidiary', 'remove')
          ) %>%
          select(-remove) %>%
          mutate_all(funs(. %>% str_trim() %>% str_to_upper())) %>%
          mutate(idCIK = cik, urlSEC = url) %>%
          select(-matches("idSubsidiary"))

        if (return_message) {
          list("Parsed: ", url) %>%
            purrr::invoke(paste0, .) %>% message()
        }

        return(df)
      }

      is_nested <-
        page %>%
        html_nodes('b font') %>%
        html_text() %>% length() > 2

      if (is_nested) {
        locations_raw <-
          page %>%
          html_nodes('b font') %>%
          html_text() %>%
          str_replace_all('\\:', '') %>%
          str_to_upper()

        locations <-
          locations_raw[!locations_raw %>% str_detect('EXHIBIT|SUBSIDIARY|SUBSIDIARIES')]

        data <-
          data[data %>% nchar() > 3] %>% str_to_upper()

        df <-
          data_frame(nameSubsidiary = data) %>%
          mutate(idRow = 1:n())

        loc_df <-
          data_frame(nameSubsidiary = locations) %>%
          inner_join(df %>% select(idRow, nameSubsidiary)) %>%
          mutate(idRow = idRow + 1) %>%
          select(locationOrganizationSubsidiary = nameSubsidiary, idRow) %>%
          suppressMessages()

        df <-
          df %>%
          filter(!nameSubsidiary %>% str_detect('SUBSIDIARY|SUBSIDIARIES')) %>%
          filter(!nameSubsidiary %>% str_detect(paste0(locations_raw, collapse = '|'))) %>%
          suppressWarnings()

        df <-
          df %>%
          left_join(loc_df) %>%
          fill(locationOrganizationSubsidiary) %>%
          mutate(urlSEC = url, idCIK = cik) %>%
          select(idCIK,
                 nameSubsidiary,
                 locationOrganizationSubsidiary,
                 everything()) %>%
          select(-idRow) %>%
          suppressMessages() %>%
          select(-matches("idSubsidiary"))
        if (return_message) {
          list("Parsed: ", url) %>%
            purrr::invoke(paste0, .) %>% message()
        }

        return(df)
      }
    }

    is_font_table <-
      page %>%
      html_nodes('b') %>%
      html_text() %>% length() == 0

    if (is_font_table) {
      all_data <-
        1:10 %>%
        map_df(function(x) {
          css_selector <-
            paste0('td:nth-child(', x, ')')
          has_length <-
            page %>%
            html_nodes(css_selector) %>% length() > 0
          if (has_length) {
            item <-
              paste0("X" , x)

            value <-
              page %>%
              html_nodes(css_selector) %>%
              html_text() %>%
              str_trim()
            data_frame(item, value)
          }
        }) %>%
        mutate(
          value = value %>% str_to_upper() %>% str_replace_all('\n  ', ' ') %>% str_replace_all('\u0096 ', '')
        ) %>%
        filter(!value == '')


      has_loc_key <-
        all_data %>%
        filter(value %in% locations) %>%
        nrow() > 0

      if (has_loc_key) {
        loc_col <-
          all_data %>%
          filter(value %in% locations) %>%
          .$item %>%
          unique()
      }

      hit_terms_in <-
        c(
          "Organized",
          "STATE OR|STATE OF|JURISDICTION OF|JURISDICTION OF INCORPORATION OR ORGANIZATION|JURISDICTION|JURISDICTION OF INCORPORATION OR\nORGANIZATION",
          "NAME|ORGANIZED UNDER THE LAWS OF",
          'STATE OF ORGANIZATION',
          'STATE OR COUNTRY OF ORGANIZATION',
          'NAME OF SUBSIDIARY',
          'NAME',
          'ENTITY NAME',
          'the laws of',
          'Percentage of voting',
          'securities owned by',
          'immediate parent',
          'CERTAIN INTERMEDIARY SUBSIDIARIES',
          'PERCENT OWNED'
        )
      hit_terms <-
        hit_terms %>%
        str_to_upper() %>%
        paste0(collapse = '|')

      hit_terms_in <-
        hit_terms %>% str_split('\\|') %>%
        flatten_chr()

      has_pct_col <-
        all_data %>%
        filter(value %in% "100") %>%
        nrow() > 0 |
        (all_data %>% filter(value %>% str_detect('PERCENT')) %>% nrow() > 0)

      if (has_pct_col) {
        pct_col <-
          all_data %>%
          filter((value %in% "100") |
                   (value %>% str_detect("PERCENT"))) %>%
          .$item %>%
          unique() %>%
          .[[1]]
      }

      all_data <-
        all_data %>%
        filter(!value %in% hit_terms_in) %>%
        filter(!value %>% str_detect(hit_terms)) %>%
        filter(!value == '') %>%
        mutate(valueNC = value %>% nchar()) %>%
        filter(!value %>% str_detect("PERCENT"))

      if (!has_pct_col) {
        all_data <-
          all_data %>%
          filter(valueNC > 3)
      }
      all_data <-
        all_data %>%
        select(-valueNC) %>%
        group_by(item) %>%
        mutate(idSubsidiary = 1:n()) %>%
        spread(item, value) %>%
        ungroup() %>%
        dplyr::rename(nameSubsidiary = X1)

      if (has_loc_key) {
        names(all_data)[names(all_data) %in% loc_col] <-
          'locationOrganizationSubsidiary'
      }

      if (has_pct_col) {
        names(all_data)[names(all_data) %in% pct_col] <-
          'pctSubsidiaryOwned'

        all_data <-
          all_data %>%
          mutate(pctSubsidiaryOwned = pctSubsidiaryOwned %>% as.numeric() / 100)
      }

      all_data <-
        all_data %>%
        mutate(idCIK = cik,
               dateSubsidiaryAsOf = NA,
               urlSEC = url) %>%
        select(-matches("idSubsidiary|^X"))

      if (return_message) {
        list("Parsed: ", url) %>%
          purrr::invoke(paste0, .) %>% message()
      }

      return(all_data)

    }

    df <-
      page %>%
      parse_page_subsidiary_table_html() %>%
      suppressWarnings()

    df <-
      df %>%
      filter(!nameSubsidiary == '') %>%
      mutate(idCIK = cik, urlSEC = url) %>%
      select(-matches("idSubsidiary")) %>%
      select(idCIK, everything())

    if (return_message) {
      list("Parsed: ", url) %>%
        purrr::invoke(paste0, .) %>% message()
    }

    return(df %>% select(-matches("idSubsidiary")))

  }

# url = 'https://www.sec.gov/Archives/edgar/data/19617/000095012301002499/y46253ex21-1.txt'
parse_sec_subsidiary_url_text <-
  function(url = "https://www.sec.gov/Archives/edgar/data/899689/000104746903007996/a2104897zex-21.txt",
           return_message = TRUE) {
    cik <-
      url %>%
      parse_sec_url_for_cik()
    data <-
      url %>%
      read_lines()

    data <-
      data[!data == '']
    has_s <-
      data %>% str_detect("<S>") %>% sum() > 0

    if (has_s) {
      data <-
        data[(data %>% grep("<S>", .) %>% .[[1]] + 1):length(data)]
    }

    data <-
      data[!data %>% str_detect("STATE OF|NAME OF|---|NAME OF SUBSIDIARY|ORGANIZED UNDER|THE LAWS OF|<")]

    data <-
      data[data %>% nchar() > 3]

    df <-
      1:length(data) %>%
      map_df(function(x) {
        item <-
          data[[x]]

        items <-
          item %>%
          str_replace_all('\\   ', '\\:') %>%
          str_split('\\:') %>%
          flatten_chr() %>%
          str_trim() %>%
          str_to_upper()

        items <-
          items[!items == '']

        if (items %>% length() == 1) {
          return(data_frame())
        }

        two_items <-
          items %>% length() == 2
        if (two_items) {
          table_data <-
            data_frame(
              idSubsidiary = x,
              nameSubsidiary = items[[1]],
              locationOrganizationSubsidiary = items[[2]]
            )
        }
        three_items <-
          items %>% length() == 3
        if (three_items) {
          table_data <-
            data_frame(
              idSubsidiary = x,
              nameSubsidiary = items[[1]],
              locationOrganizationSubsidiary = items[[2]],
              pctSubsidiaryOwned = items[[3]] %>% as.numeric() / 100
            )
        }

        table_data <-
          table_data %>%
          mutate(
            isChildSubsidiary = ifelse(nameSubsidiary %>% substr(1, 1) == "-", TRUE, FALSE),
            nameSubsidiary = nameSubsidiary %>% str_replace('\\-', '') %>% str_trim()
          )
        return(table_data)
      }) %>%
      mutate(idCIK = cik, urlSEC = url) %>%
      select(-matches("idSubsidiary")) %>%
      select(idCIK,
             nameSubsidiary,
             locationOrganizationSubsidiary,
             everything()) %>%
      filter(!nameSubsidiary %in% c('NAME', 'ORGANIZED UNDER'))

    df <-
      df %>%
      filter(!nameSubsidiary == '')

    if (return_message) {
      list("Parsed: ", url) %>%
        purrr::invoke(paste0, .) %>% message()
    }

    return(df)

  }

parse_sec_subsidiary_url  <-
  function(url = "https://www.sec.gov/Archives/edgar/data/34088/000003408816000065/xomexhibit21.htm",
           return_message = TRUE)  {
    is_text <-
      url %>%
      str_detect("txt")

    is_html <-
      url %>%
      str_detect("html|htm")
    parse_sec_subsidiary_url_text_safe <-
      purrr::possibly(parse_sec_subsidiary_url_text, data_frame())

    parse_sec_subsidiary_url_html_safe <-
      purrr::possibly(parse_sec_subsidiary_url_html, data_frame())

    if (is_text) {
      data <-
        url %>%
        parse_sec_subsidiary_url_text_safe()
    }

    if (is_html) {
      data <-
        url %>%
        parse_sec_subsidiary_url_html_safe()
    }
    return(data)
  }


# form_parsing ------------------------------------------------------------


parse_full_form_names <-
  function(sec_names) {
    df_names <-
      1:length(sec_names) %>%
      map_df(function(x) {
        sec_name <-
          sec_names[[x]]

        name_pieces <-
          sec_name %>% str_replace_all('\\.value|\\.item', '')

        pieces <-
          name_pieces %>%
          str_split('\\.') %>%
          flatten_chr()

        pieces_no_num <-
          pieces[!pieces %>% str_detect("[0-9]")]
        peice_length <-
          pieces_no_num %>% length()

        is_street <-
          pieces %>% str_detect("street1|street2|Street1|Street2") %>% sum(na.rm = T) > 0

        name_item <-
          pieces_no_num[length(pieces_no_num)]

        if (sec_name %>% str_detect('filingManager')) {
          name_item <-
            pieces %>% paste0(collapse = '')

          df <-
            data_frame(nameSECFull = sec_name,
                       nameSEC = name_item)
          return(df)
        }

        if (is_street) {
          name_item <-
            pieces[pieces %>% str_detect("street1|street2|Street1|Street2")]
        }

        is_sig <-
          name_pieces %>% str_detect('signature') & peice_length == 1

        is_footnote <-
          sec_name %>% str_detect('footnote')

        is_issuer <-
          sec_name %>% str_detect('\\issuer.[A-Z]')

        is_federal <-
          sec_name %>% str_detect(pattern = "federalExemptionsExclusions")

        if (is_federal) {
          df <-
            data_frame(
              nameSECFull = sec_name,
              nameTable = pieces[[1]],
              nameSEC = name_item
            )

          return(df)
        }

        if (is_issuer) {

          items <-
            sec_name %>% str_split('\\.') %>% flatten_chr()

          countItem <-
            pieces[2] %>% readr::parse_number() %>% suppressWarnings()

          name_item <-
            items[length(items)]

          df <-
            data_frame(
              nameSECFull = sec_name,
              nameTable = 'issuer',
              countItem,
              nameSEC = name_item
            )
          return(df)
        }

        if (is_footnote) {
          if (pieces %>% length() == 1) {
            countItem <-
              0
            item <-
              pieces[[1]]
          } else {
            item <-
              pieces[[1]]
            countItem <-
              pieces[2] %>% readr::parse_number() %>% suppressWarnings()
          }
          return(data_frame(nameTable = 'footnotes', nameSECFull = sec_name, nameSEC = item, countItem))
        }

        if (is_sig) {
          df <-
            data_frame(nameTable = 'signatures', nameSECFull = sec_name, nameSEC = name_item)
          return(df)
        }

        if (peice_length == 1) {
          df <-
            data_frame(nameSECFull = sec_name, nameSEC = name_item)
          return(df)
        }

        piece_count <-
          length(pieces)

        if (piece_count == 1) {
          df <-
            data_frame(nameSECFull = sec_name, nameSEC = sec_name)
          return(df)
        }

        if (piece_count == 2 &!is_footnote) {


          df <-
            data_frame(nameSECFull = sec_name,
                       nameTable = pieces[[1]] ,
                       nameSEC = name_item)

          return(df)
        }

        if (piece_count > 2) {
          countItem <-
            pieces[2] %>% readr::parse_number() %>% suppressWarnings()

          df <-
            data_frame(
              nameSECFull = sec_name,
              nameTable = pieces[[1]] ,
              countItem,
              nameSEC = name_item
            )

          return(df)
        }

      }) %>%
      filter(!nameSEC == '')

    df_dictionary <-
      sec_form_title_df()

    has_missing_names <-
      df_names$nameSEC[!df_names$nameSEC %in% df_dictionary$nameSEC] %>%
      length() > 0
    if (has_missing_names) {
      missing <-
        df_names$nameSEC[!df_names$nameSEC %in% df_dictionary$nameSEC] %>%
        unique()

      missing_names <-
        missing %>%
        paste0(collapse = '\n')
      stop(list("Missing:\n", missing_names) %>%
             purrr::reduce(paste0))
    }

    df_names <-
      df_names %>%
      left_join(df_dictionary) %>%
      suppressWarnings() %>%
      suppressMessages()

    if (!'nameTable' %in% names(df_names)) {
      df_names <-
        df_names %>%
        mutate(nameTable = 'asset')
    }

    df_names <-
      df_names %>%
      select(nameTable, nameSECFull, nameSEC, nameActual, everything()) %>%
      mutate(nameTable = nameTable %>% str_replace('Id',''),
             nameTable = ifelse(nameTable %in% c('issuerCredentials','securitiesIssued'), NA, nameTable)) %>%
      suppressWarnings() %>%
      suppressMessages()
  }

parse_xml_tables <-
  function(url = "https://www.sec.gov/Archives/edgar/data/61004/000114036117000046/doc1.xml"){
    page <-
      url %>%
      xml2::read_xml()

    tables <-
      page %>%
      xml_contents() %>%
      xml_name() %>%
      unique()

    data <-
      1:length(tables) %>%
      map_df(function(x){
        table <-
          tables[[x]]

        if (table %in% c('headerData', 'formData')) {
          form_tables <-
            page %>% xml_contents() %>% xml_name()

          table_loc <- table %>% grep(form_tables)
          xml_nodes <-
            page %>%
            xml_contents() %>% .[[table_loc]]
        }

        if (table %in% c('infoTable' , 'assets')) {
          xml_nodes <-
            page %>%
            xml_contents()
        }

        if (table == 'comment') {
          value <-
            page %>% xml_contents() %>% xml_text()

          df <-
            data_frame(idTable = x, nameSECFull = table, value)
          return(df)

        }

        tables_special <- c('headerData', 'formData', 'infoTable', 'assets')

        if (!table %in% tables_special) {

          value_search <-
            list('//', table) %>% purrr::reduce(paste0)

          xml_nodes <-
            page %>%
            xml_contents() %>%
            xml_find_all(value_search)
        }
        if (xml_nodes %>% length() > 100) {
          list("Be patient there are ", xml_nodes %>% length() %>% formattable::comma(digits = 0), ' nodes to parse') %>%
            purrr::reduce(paste0) %>% message()
        }
        value_list <-
          xml_nodes %>% as_list()

        value_list <-
          value_list[value_list %>% map(length) %>% flatten_dbl() > 0]

        json_data <-
          value_list %>%
          jsonlite::toJSON(force = FALSE,dataframe = 'values') %>%
          jsonlite::fromJSON(simplifyDataFrame = TRUE,flatten = TRUE)

        wrong_output <-
          json_data %>% class() == 'array'

        if (wrong_output) {
          item <-
            xml_nodes %>% xml_name()
          value <-
            xml_nodes %>% xml_text()
          json_data <-
            data_frame(item, value) %>%
            spread(item, value)
        }

        if (json_data %>% length() == 0) {
          return(data_frame())
        }
        if ('summaryInfo' %in% names(json_data)) {
          json_data <-
            1:length(json_data) %>% map(
              function(x){
                js_d <- json_data[x]
                if ('summaryInfo' %in% names(js_d)) {
                  if (js_d$summaryInfo$clarificationResponses %>% length() == 0) {
                    js_d$summaryInfo$clarificationResponses <-
                      NULL
                  }
                }
                return(js_d)
              }) %>%
            flatten()

          json_data <-
            json_data[json_data %>% map(function(x){data.frame(x, stringsAsFactors = F)} %>% nrow()) > 0]
        }

        json_data <-
          json_data %>%
          data.frame(stringsAsFactors = FALSE) %>%
          as_data_frame() %>%
          mutate_all(as.character) %>%
          mutate(idTable = x) %>%
          gather(nameSECFull, value, -idTable) %>%
          arrange(idTable)
        return(json_data)
      })

    data <-
      data %>%
      mutate(isList = value %>% str_detect('list')) %>%
      filter(!isList) %>%
      select(-isList) %>%
      mutate(
        nameSECFull = nameSECFull %>% str_replace_all(
          "filerInfo.flags.|filerInfo.filer.|coverPage.|.filer.|\\flags.|filer.credentials.",
          ''
        ),
        nameSECFull = nameSECFull %>% str_replace_all('filerInfo.|issuerCredentials.', '')
      )

    closeAllConnections()
    rm(tables)
    rm(page)
    rm(url)
    return(data)
  }

parse_sec_form <-
  function(url = "https://www.sec.gov/Archives/edgar/data/61004/000114036117000046/doc1.xml",
           return_message = TRUE) {
    data <-
      parse_xml_tables(url = url)

    if (!'nameSECFull' %in% names(data)) {
      data <-
        data %>%
        mutate(nameSECFull = nameSEC)
    }

    cik <-
      url %>% str_replace_all('https://www.sec.gov/Archives/edgar/data/', '') %>% str_split('/') %>% flatten_chr() %>% .[[1]] %>% readr::parse_number() %>% suppressMessages()

    df_title <-
      sec_form_title_df()

    is_13FInfo <-
      url %>% str_detect('form13fInfoTable.xml|infotable.xml')
    sec_names <-
      data$nameSECFull %>% unique()

    df_names <-
      parse_full_form_names(sec_names = sec_names)

    df_names <-
      df_names %>%
      mutate(nameTable = ifelse(
        nameSECFull %>% str_detect("issuerAddress"),
        "issuerAddress",
        nameTable),
        nameTable =  ifelse(
          nameSECFull %>% str_detect("reportingOwner"),
          "reportingOwner",
          nameTable)
      ) %>%
      mutate(nameTable = ifelse(nameSECFull %>% str_detect("issuerInfo."), 'issuerInfo', nameTable),
             nameTable = ifelse(nameSECFull %>% str_detect("securitiesIssued."), 'securitiesIssued', nameTable),
             nameTable = ifelse(nameSECFull %>% str_detect("summaryInfo."), 'summaryInfo', nameTable),
             nameTable = ifelse(nameSECFull %>% str_detect("^comment[A-Z]"), 'Comments', nameTable)
      )

    if (is_13FInfo) {
      df_names <-
        df_names %>%
        mutate(nameTable = 'holdingsInformation')
    }
    if (!'nameSEC' %in% names(data)) {
      data <- data %>%
        mutate(nameSEC = nameSECFull)
    }
    data <-
      data %>%
      select(-nameSEC) %>%
      left_join(df_names) %>%
      mutate(nameActual = ifelse(nameSECFull == "X.1.A.A.", 'idForm', nameActual)) %>%
      suppressMessages()

    if ('countItem' %in% names(data)) {
      data <-
        data %>%
        select(nameTable, countItem, nameSECFull, nameActual, everything()) %>%
        mutate(countItem = countItem - 1) %>%
        suppressMessages()
    }

    if ('property' %in% data$nameTable) {
      data <-
        data %>%
        mutate(nameTable = ifelse(nameTable %>% is.na(), 'Asset', nameTable))
    }

    has_metadata <-
      data %>%
      filter(nameTable %>% is.na()) %>% nrow() > 0

    if (has_metadata) {
      df_metadata <-
        data %>%
        filter(nameTable %>% is.na()) %>%
        select(nameActual, value) %>%
        group_by(nameActual) %>%
        mutate(countItem = 1:n() - 1) %>%
        arrange(countItem) %>%
        ungroup() %>%
        filter(!nameActual %>% str_detect('idCCC')) %>%
        mutate(nameActual = ifelse(countItem == 0, nameActual, nameActual %>% paste0(countItem))) %>%
        select(-countItem)

      col_order <-
        df_metadata$nameActual

      df_metadata <-
        df_metadata %>%
        spread(nameActual, value) %>%
        select(one_of(col_order)) %>%
        mutate(urlSECFiling = url) %>%
        resolve_form_columns()
    } else {
      df_metadata <-
        data_frame(idCIKFiler = cik,
                   urlSECFiling = url)
    }

    tables <-
      data %>%
      filter(!nameTable %>% is.na()) %>%
      .$nameTable %>%
      unique()

    data <-
      1:length(tables) %>%
      map(function(x) {
        table <-
          tables[[x]]
        table_name <-
          list('data',
               table %>% substr(1, 1) %>% str_to_upper(),
               table %>% substr(2, nchar(table))) %>%
          purrr::reduce(paste0)

        table_df <-
          data %>%
          filter(nameTable == table) %>%
          select(matches("countItem"), nameActual, value) %>%
          select(which(colMeans(is.na(.)) < 1)) %>%
          group_by(nameActual) %>%
          mutate(countItem = 1:n() - 1) %>%
          ungroup()

        has_counts <-
          table_df$countItem %>% max(na.rm = TRUE) > 0

        if (has_counts) {
          table_df <-
            table_df %>%
            arrange(countItem)

          col_order <- c('countItem', table_df$nameActual)

          table_df <-
            table_df %>%
            spread(nameActual, value) %>%
            select(one_of(col_order)) %>%
            mutate(urlSECFiling = url) %>%
            resolve_form_columns()

          table_df <-
            table_df %>%
            nest(-urlSECFiling, .key = data)
        } else {
          table_df <-
            table_df %>%
            select(-countItem)
          col_order <- c(table_df$nameActual)

          table_df <-
            table_df %>%
            spread(nameActual, value) %>%
            select(one_of(col_order)) %>%
            resolve_form_columns() %>%
            mutate(urlSECFiling = url)

          table_df <-
            table_df %>%
            nest(-urlSECFiling, .key = data)
        }
        names(table_df)[[2]] <-
          table_name

        df_metadata <-
          df_metadata %>%
          left_join(table_df) %>%
          suppressMessages()

      }) %>%
      reduce(left_join) %>%
      suppressMessages()

    ## maybe add IDCK

    if (return_message) {
      list("Parsed: ", url) %>%
        purrr::invoke(paste0, .) %>% message()
    }
    closeAllConnections()
    rm(df_metadata)
    return(data)
  }

parse_form_data <-
  function(all_filings, filter_parameter = 'isXBRLInstanceFile', return_message = TRUE) {
    df_search <-
      all_filings %>%
      filter_(.dots = filter_parameter)

    if (filter_parameter == 'isXBRLInstanceFile') {
      if (df_search %>% nrow() == 0) {
        return(data_frame())
      }
      parse_xbrl_filer_url_safe <-
        purrr::possibly(parse_xbrl_filer_url, data_frame())
      all_data <-
        df_search$urlSECFiling %>%
        unique() %>%
        map_df(function(x) {
          parse_xbrl_filer_url(url = x, return_message = return_message)
        })
      all_data <-
        all_data %>%
        select(-matches("idCIK1|nameFiler1")) %>%
        left_join(df_search %>% select(idForm, idAccession, nameFile, dateFiling, urlSECFiling)) %>%
        select(
          matches("idCIK"),
          matches("name[Entity]|name[Filer]"),
          dateFiling,
          idForm,
          idAccession,
          nameFile,
          everything()
        ) %>%
        suppressMessages()

      return(all_data)
    }

    if (filter_parameter == 'isFormD') {
      if ('idForm' %in% names(df_search)){
        df_search <-
          df_search %>%
          filter(!idForm %>% str_detect("10"))
      }
    }
    if (df_search %>% nrow() == 0) {
      return(data_frame())
    }
    all_data <-
      df_search$urlSECFiling %>%
      unique() %>%
      map_df(function(x) {
        parse_sec_form(url = x, return_message = return_message)
      })

    all_data <-
      all_data %>%
      select(-matches("idCIK1|nameFiler1")) %>%
      left_join(df_search %>% select(matches("idForm"), matches("idAccession"), matches("nameFile"), matches("dateFiling"), urlSECFiling)) %>%
      select(
        matches("idCIK"),
        matches("name[Entity]|name[Filer]"),
        dateFiling,
        matches("idForm"),
        matches("idAccession"),
        matches("nameFile"),
        everything()
      ) %>%
      suppressMessages()

    if (filter_parameter == 'hasAssetFile') {
      if('dataComments' %in% names(all_data)) {
        df_comments <-
          all_data %>%
          select(idCIKFiler, idAccession, dataComments) %>%
          mutate(isNULL = dataComments %>% map_lgl(is_null)) %>%
          filter(!isNULL) %>%
          distinct() %>%
          select(-isNULL)

        all_data <-
          all_data %>%
          select(-dataComments) %>%
          mutate(isNULL = dataAsset %>% map_lgl(is_null)) %>%
          filter(!isNULL) %>%
          filter(!nameFile == "ASSET RELATED DOCUMENT") %>%
          distinct() %>%
          select(-isNULL) %>%
          left_join(df_comments) %>%
          suppressMessages()
      }
    }
    return(all_data)
  }


# XBRL Finder -------------------------------------------------------------
parse_xbrl_filer_url <-
  function(url = "https://www.sec.gov/Archives/edgar/data/1037540/000165642316000023/bxp-20160930.xml",
           return_message = TRUE) {
    options(stringsAsFactors = FALSE, scipen = 999999)
    cik <-
      url %>%
      str_split('data/') %>%
      flatten_chr() %>%
      .[[2]] %>%
      str_split('/') %>%
      flatten_chr() %>%
      .[[1]] %>%
      as.numeric()
    td <-
      tempdir()
    tf <-
      tempfile(tmpdir = td, fileext = ".xml")

    url %>%
      curl::curl_download(destfile = tf)

    doc <-
      tf %>%
      XBRL::xbrlParse()


    ## Get a data frame with facts:
    df_fct <-
      XBRL::xbrlProcessFacts(doc) %>%
      as_data_frame()

    df_fct <-
      df_fct %>%
      mutate(
        isNumber = ifelse(!fact %>% readr::parse_number() %>% is.na(), TRUE, FALSE),
        amountFact = ifelse(isNumber == TRUE, fact %>% readr::parse_number(), NA)
      ) %>%
      separate(elementId,
               c('codeElement', 'nameElement'),
               sep = '\\_',
               remove = FALSE) %>%
      suppressWarnings()
    ## Get a data frame with contexts:
    df_cts <-
      XBRL::xbrlProcessContexts(doc) %>%
      as_data_frame()
    ## Get a data frame with units:
    df_unt <-
      XBRL::xbrlProcessUnits(doc) %>%
      as_data_frame()

    df_sch <-
      XBRL::xbrlGetSchemaName(doc) %>%
      as_data_frame()

    df_footnotes <-
      XBRL::xbrlProcessFootnotes(doc) %>%
      as_data_frame()


    ## Free the external memory used:
    XBRL::xbrlFree(doc)
    url_xsd <-
      url %>% str_replace(".xml", ".xsd")
    url_xsd %>%
      curl_download(destfile = tf)

    ## Parse the schema file:
    docS <-
      tf %>%
      XBRL::xbrlParse()
    ## Get roles:
    df_rls <-
      docS %>%
      XBRL::xbrlProcessRoles() %>%
      as_data_frame()

    ## calculation
    url_cal <-
      url %>% str_replace(".xml", "_cal.xml")
    if (httr::url_ok(url_cal) %>% suppressWarnings()){
      url_cal %>%
        curl_download(destfile = tf)

      docS <-
        tf %>%
        XBRL::xbrlParse()

      df_calcs <-
        docS %>%
        XBRL::xbrlProcessArcs(arcType = 'calculation') %>%
        as_data_frame()
    } else {
      df_calcs <-
        data_frame()
    }

    ## definition
    url_def <-
      url %>% str_replace(".xml", "_def.xml")

    url_def %>%
      curl_download(destfile = tf)

    docS <-
      tf %>%
      XBRL::xbrlParse()

    df_defs <-
      docS %>%
      XBRL::xbrlProcessArcs(arcType = 'definition') %>%
      as_data_frame()

    ## labels
    url_lab <-
      url %>% str_replace(".xml", "_lab.xml")

    url_lab %>%
      curl_download(destfile = tf)

    docS <-
      tf %>%
      XBRL::xbrlParse()

    df_labels <-
      docS %>%
      XBRL::xbrlProcessLabels() %>%
      as_data_frame()

    ## presentation
    url_pre <-
      url %>% str_replace(".xml", "_pre.xml")

    url_pre %>%
      curl_download(destfile = tf)

    docS <-
      tf %>%
      XBRL::xbrlParse()

    ## Free the external memory used:
    tf %>%
      unlink()
    data <-
      data_frame(
        idCIK = cik,
        urlSECFiling = url,
        dataFacts = list(df_fct),
        dataContexts = list(df_cts),
        dataUnits = list(df_unt),
        dataFootnotes = list(df_footnotes),
        dataRoles = list(df_rls),
        dataCalculations = list(df_calcs) ,
        dataDefinitions = list(df_defs),
        dataLabel = list(df_labels)
      )
    td %>% unlink()
    tf %>% unlink()
    if (return_message) {
      list("Parsed: ", url) %>%
        purrr::invoke(paste0, .) %>% message()
    }
    return(data)
  }

# dictionaries ------------------------------------------------------------
sec_form_title_df <-
  function() {
    data_frame(
      nameSEC = c(
        "conversionOrExercisePrice",
        "deemedExecutionDate",
        "directOrIndirectOwnership",
        "documentType",
        "equitySwapInvolved",
        "exerciseDate",
        "expirationDate",
        "footnote",
        "isDirector",
        "isOfficer",
        "isOther",
        "issuerCik",
        "issuerName",
        "issuerTradingSymbol",
        "isTenPercentOwner",
        "natureOfOwnership",
        "noSecuritiesOwned",
        "notSubjectToSection16",
        "officerTitle",
        "otherText",
        "periodOfReport",
        "postTransactionAmountsOwnedFollowingTransaction",
        "remarks",
        "rptOwnerCik",
        "rptOwnerCity",
        "rptOwnerName",
        "rptOwnerState",
        "rptOwnerStateDescription",
        "rptOwnerStreet1",
        "rptOwnerStreet2",
        "rptOwnerZipCode",
        "schemaVersion",
        "securityTitle",
        "sharesOwnedFollowingTransaction",
        "signatureDate",
        "signatureName",
        "transactionAcquiredDisposedCode",
        "transactionCode",
        "transactionDate",
        "transactionFormType",
        "transactionPricePerShare",
        "transactionShares",
        "transactionTimeliness",
        "transactionTotalValue",
        "underlyingSecurityShares",
        "underlyingSecurityTitle",
        "clarificationOfResponse", "isBusinessCombinationTransaction",
        "cik", "moreThanOneYear", "previousName", "edgarPreviousNameList",
        "entityName", "entityType", "entityTypeOtherDesc", "federalExemptionsExclusions",
        "industryGroupType", "investmentFundType", "investmentFundInfo",
        "hasNonAccreditedInvestors", "numberNonAccreditedInvestors",
        "totalNumberAlreadyInvested", "city", "stateOrCountry", "stateOrCountryDescription",
        "street1", "street2", "zipCode", "issuerPhoneNumber", "issuerPreviousNameList",
        "jurisdictionOfInc", "overFiveYears", "yearOfInc", "withinFiveYears",
        "yetToBeFormed", "aggregateNetAssetValueRange", "revenueRange",
        "minimumInvestmentAccepted", "totalAmountSold", "totalOfferingAmount",
        "totalRemaining", "firstName", "lastName", "middleName", "relationship",
        "relationshipClarification", "dollarAmount", "isEstimate", "associatedBDCRDNumber",
        "associatedBDName", "foreignSolicitation", "recipientCRDNumber",
        "recipientName", "description", "state", "statesOfSolicitationList",
        "authorizedRepresentative", "nameOfSigner", "signatureTitle",
        "submissionType", "testOrLive", "dateOfFirstSale", "yetToOccur",
        "isAmendment", "descriptionOfOtherType", "isDebtType", "isEquityType",
        "isMineralPropertyType", "isOptionToAcquireType", "isOtherType",
        "isPooledInvestmentFundType", "isSecurityToBeAcquiredType", "isTenantInCommonType",
        'notSubjectToSection16', 'rptOwnerStreet1', 'rptOwnerStreet2',

        "liveTestFlag", "confirmingCopyFlag", "returnCopyFlag", "overrideInternetFlag",
        "ccc", "reportCalendarOrQuarter", "filingManagername", "filingManageraddressstreet1",
        "filingManageraddressstreet2", "filingManageraddresscity", "filingManageraddressstateOrCountry",
        'filingManagerstateOrCountryDescription',
        "filingManageraddresszipCode", "reportType", "form13FFileNumber",
        "provideInfoForInstruction5", "name", "title", "phone", "signature",
        "otherIncludedManagersCount", "tableEntryTotal", "tableValueTotal",
        "isConfidentialOmitted",
        "nameOfIssuer", "titleOfClass", "cusip", "value", "investmentDiscretion",
        "otherManager", "putCall", "sshPrnamt", "sshPrnamtType", "Sole",
        "Shared", "None",

        "offeringFileNumber", "sinceLastFiling", "jurisdictionOrganization",
        "yearIncorporation", "sicCode", "irsNum", "fullTimeEmployees",
        "partTimeEmployees", "phoneNumber", "connectionName", "industryGroup",
        "cashEquivalents", "investmentSecurities", "accountsReceivable",
        "propertyPlantEquipment", "totalAssets", "accountsPayable", "longTermDebt",
        "totalLiabilities", "totalStockholderEquity", "totalLiabilitiesAndEquity",
        "totalRevenues", "costAndExpensesApplToRevenues", "depreciationAndAmortization",
        "netIncome", "earningsPerShareBasic", "earningsPerShareDiluted",
        "nameAuditor", "commonEquityClassName", "outstandingCommonEquity",
        "commonCusipEquity", "publiclyTradedCommonEquity", "preferredEquityClassName",
        "outstandingPreferredEquity", "preferredCusipEquity", "publiclyTradedPreferredEquity",
        "debtSecuritiesClassName", "outstandingDebtSecurities", "cusipDebtSecurities",
        "publiclyTradedDebtSecurities", "certifyIfTrue", "certifyIfNotDisqualified",
        "summaryInfo", "financialStatementAuditStatus", "securitiesOfferedTypes",
        "offerDelayedContinuousFlag", "offeringYearFlag", "offeringAfterQualifFlag",
        "offeringBestEffortsFlag", "solicitationProposedOfferingFlag",
        "resaleSecuritiesAffiliatesFlag", "securitiesOffered", "outstandingSecurities",
        "pricePerSecurity", "issuerAggregateOffering", "securityHolderAggegate",
        "qualificationOfferingAggregate", "concurrentOfferingAggregate",
        "totalAggregateOffering", "underwritersServiceProviderName",
        "underwritersFees", "auditorServiceProviderName", "auditorFees",
        "legalServiceProviderName", "legalFees", "promotersServiceProviderName",
        "promotersFees", "brokerDealerCrdNumber", "estimatedNetAmount",
        "clarificationResponses", "jurisdictionsOfSecOfferedSame", "issueJuridicationSecuritiesOffering",
        "dealersJuridicationSecuritiesOffering", "securitiesIssuerName",
        "securitiesIssuerTitle", "securitiesIssuedTotalAmount", "securitiesPrincipalHolderAmount",
        "securitiesIssuedAggregateAmount", "securitiesActExcemption",
        "certifyIfBadActor", "salesCommissionsServiceProviderName",
        "salesCommissionsServiceProviderFees", "jurisdictionsOfSecOfferedNone",
        "ifUnregsiteredNone", "blueSkyServiceProviderName", "blueSkyFees",
        'indicateTier1Tier2Offering', 'X.1.A.A.', 'X.1.A.A.', 'aggregateConsiderationBasis',
        'findersFeesServiceProviderName' , 'finderFeesFee',
        'loans', 'propertyAndEquipment', 'deposits', 'totalInterestIncome',
        'totalInterestExpenses', 'securitiesOfferedOtherDesc', 'comment',
        "assetTypeNumber",
        "assetNumber",
        "assetGroupNumber",
        "reportPeriodBeginningDate",
        "reportPeriodEndDate",
        "issuerName",
        "originalIssuanceDate",
        "originalSecurityAmount",
        "originalSecurityTermNumber",
        "securityMaturityDate",
        "originalAmortizationTermNumber",
        "originalInterestRatePercentage",
        "accrualTypeCode",
        "interestRateTypeCode",
        "originalInterestOnlyTermNumber",
        "firstPaymentDate",
        "underwritingIndicator",
        "securityTitleName",
        "denominationNumber",
        "currencyName",
        "trusteeName",
        "secFileNumber",
        "cik",
        "callableIndicator",
        "paymentFrequencyCode",
        "zeroCouponIndicator",
        "assetAddedIndicator",
        "assetModifiedIndicator",
        "reportPeriodBeginningAssetBalanceAmount",
        "reportPeriodBeginningScheduledAssetBalanceAmount",
        "reportPeriodScheduledPaymentAmount",
        "reportPeriodInterestRatePercentage",
        "totalActualPaidAmount",
        "actualInterestCollectionPercentage",
        "actualPrincipalCollectedAmount",
        "actualOtherCollectionAmount",
        "otherPrincipalAdjustmentAmount",
        "otherInterestAdjustmentAmount",
        "scheduledInterestAmount",
        "scheduledPrincipalAmount",
        "endReportingPeriodActualBalanceAmount",
        "endReportingPeriodScheduledBalanceAmount",
        "servicingFeePercentage",
        "servicingFlatFeeAmount",
        "zeroBalanceCode",
        "zeroBalanceEffectiveDate",
        "remainingTermToMaturityNumber",
        "currentDelinquentStatusNumber",
        "paymentPastDueDaysNumber",
        "paymentPastDueNumber",
        "nextReportPeriodPaymentDueAmount",
        "nextDueDate",
        "primaryLoanServicerName",
        "mostRecentServicingTransferReceivedDate",
        "assetSubjectToDemandIndicator",
        "statusAssetSubjectToDemandCode",
        "repurchaseAmount",
        "demandResolutionDate",
        "repurchaserName",
        "repurchaseReplacementReasonCode",
        "reportPeriodBeginDate",
        "originalLoanPurposeCode",
        "originatorName",
        "originalLoanAmount",
        "originalLoanMaturityDate",
        "originalInterestRateTypeCode",
        "originalLienPositionCode",
        "mostRecentJuniorLoanBalanceAmount",
        "mostRecentJuniorLoanBalanceDate",
        "mostRecentSeniorLoanAmount",
        "mostRecentSeniorLoanAmountDate",
        "loanTypeMostSeniorLienCode",
        "mostSeniorLienHybridPeriodNumber",
        "mostSeniorLienNegativeAmortizationLimitPercentage",
        "mostSeniorLienOriginationDate",
        "prepaymentPenaltyIndicator",
        "negativeAmortizationIndicator",
        "modificationIndicator",
        "modificationNumber",
        "mortgageInsuranceRequirementIndicator",
        "balloonIndicator",
        "coveredHighCostCode",
        "servicerHazardInsuranceCode",
        "refinanceCashOutAmount",
        "totalOriginationDiscountAmount",
        "brokerIndicator",
        "channelCode",
        "nationalMortgageLicenseSystemCompanyNumber",
        "buyDownNumber",
        "loanDelinquencyAdvanceNumber",
        "originationARMIndexCode",
        "armMarginPercentage",
        "fullyIndexedRatePercentage",
        "initialFixedRatePeriodHybridARMNumber",
        "initialInterestRateDecreasePercentage",
        "initialInterestRateIncreasePercentage",
        "indexLookbackNumber",
        "subsequentInterestRateResetNumber",
        "lifetimeRateCeilingPercentage",
        "lifetimeRateFloorPercentage",
        "subsequentInterestRateDecreasePercentage",
        "subsequentInterestRateIncreasePercentage",
        "subsequentPaymentResetNumber",
        "armRoundCode",
        "armRoundPercentage",
        "optionArmIndicator",
        "paymentMethodAfterRecastCode",
        "initialMinimumPaymentAmount",
        "convertibleIndicator",
        "HELOCIndicator",
        "HELOCDrawNumber",
        "prepaymentPenaltyCalculationCode",
        "prepaymentPenaltyTypeCode",
        "prepaymentPenaltyTotalTermNumber",
        "prepaymentPenaltyHardTermNumber",
        "negativeAmortizationLimitAmount",
        "negativeAmortizationInitialRecastNumber",
        "negativeAmortizationSubsequentRecastNumber",
        "negativeAmortizationBalanceAmount",
        "initialFixedPaymentNumber",
        "initialPaymentCapPercentage",
        "subsequentPaymentCapPercentage",
        "initialMinimumPaymentResetNumber",
        "subsequentMinimumPaymentResetNumber",
        "minimumPaymentAmount",
        "geographicalLocation",
        "occupancyStatusCode",
        "mostRecentOccupancyStatusCode",
        "propertyTypeCode",
        "mostRecentPropertyValueAmount",
        "mostRecentPropertyValueTypeCode",
        "mostRecentPropertyValueDate",
        "mostRecentAVMModelCode",
        "mostRecentAVMConfidenceNumber",
        "originalCLTVPercentage",
        "originalLTVPercentage",
        "originalObligorNumber",
        "originalObligorCreditScoreNumber",
        "originalObligorCreditScoreType",
        "mostRecentObligorCreditScoreNumber",
        "mostRecentObligorCreditScoreType",
        "mostRecentObligorCreditScoreDate",
        "obligorIncomeVerificationLevelCode",
        "IRSForm4506TIndicator",
        "originatorFrontEndDTIPercentage",
        "originatorBackEndDTIPercentage",
        "obligorEmploymentVerificationCode",
        "obligorEmploymentLengthCode",
        "obligorAssetVerificationCode",
        "originalPledgedAssetsAmount",
        "qualificationMethodCode",
        "mortgageInsuranceCompanyName",
        "mortgageInsuranceCoveragePercentage",
        "poolInsuranceCompanyName",
        "poolInsuranceStopLossPercentage",
        "mortgageInsuranceCoverageTypeCode",
        "modificationIndicatorReportingPeriod",
        "nextPaymentDueDate",
        "advancingMethodCode",
        "servicingAdvanceMethodologyCode",
        "stopPrincipalInterestAdvancingDate",
        "reportingPeriodBeginningLoanBalanceAmount",
        "reportingPeriodBeginningScheduledLoanBalanceAmount",
        "nextReportingPeriodPaymentDueAmount",
        "reportingPeriodInterestRatePercentage",
        "nextInterestRatePercentage",
        "otherAssessedUncollectedServicerFeeamount",
        "otherServicingFeeRetainedByServicerAmount",
        "reportingPeriodEndActualBalanceAmount",
        "reportingPeriodEndScheduledBalanceAmount",
        "reportingPeriodScheduledPaymentAmount",
        "actualInterestCollectedAmount",
        "actualOtherCollectedAmount",
        "paidThroughDate",
        "interestPaidThroughDate",
        "paidFullAmount",
        "servicerAdvancedPrincipalAmount",
        "servicerAdvancedRepaidPrincipalAmount",
        "servicerAdvancedCumulativePrincipalAmount",
        "servicerAdvanceInterestAmount",
        "servicerAdvanceRepaidInterestAmount",
        "servicerAdvanceCumulativeInterestAmount",
        "servicerAdvanceTaxesInsuranceAmount",
        "servicerAdvanceRepaidTaxesInsuranceAmount",
        "servicerAdvanceCumulativeTaxesInsuranceAmount",
        "servicerAdvanceCorporateAmount",
        "servicerAdvanceRepaidCorporateAmount",
        "servicerAdvanceCumulativeCorporateAmount",
        "mostRecentTwelveMonthHistoryCode",
        "nextResetRatePercentage",
        "nextPaymentChangeDate",
        "nextInterestRateChangeDate",
        "nextResetPaymentAmount",
        "exercisedArmConversionOptionIndicator",
        "primaryServicerName",
        "masterServicerName",
        "specialServicerName",
        "subServicerName",
        "assetSubjectDemandIndicator",
        "assetSubjectDemandStatusCode",
        "repurchaseReplacementCode",
        "chargeOffPrincipalAmount",
        "chargeOffInterestAmount",
        "lossMitigationTypeCode",
        "mostRecentLoanModificationEventCode",
        "mostRecentLoanModificationEffectiveDate",
        "postModificationMaturityDate",
        "postModificationInterestRateTypeCode",
        "postModificationAmortizationTypeCode",
        "postModificationInterestPercentage",
        "postModificationFirstPaymentDate",
        "postModificationLoanBalanceAmount",
        "postModificationPrincipalInterestPaymentAmount",
        "totalCapAmount",
        "incomeVerificationIndicatorAtModification",
        "modificationFrontEndDebtToIncomePercentage",
        "modificationBackEndDebtToIncomePercentage",
        "totalDeferredAmount",
        "forgivenPrincipalCumulativeAmount",
        "forgivenPrincipalReportingPeriodAmount",
        "forgivenInterestCumulativeAmount",
        "forgivenInterestReportingPeriodAmount",
        "actualEndingBalanceTotalDebtAmount",
        "scheduledEndingBalanceTotalDebtAmount",
        "postModificationARMCode",
        "postModificationARMIndexCode",
        "postModificationMarginPercentage",
        "postModificationInterestResetNumber",
        "postModificationNextResetDate",
        "postModificationIndexLookbackNumber",
        "postModificationARMRoundingCode",
        "postModificationARMRoundingPercentage",
        "postModificationInitialMinimumPayment",
        "postModificationNextPaymentAdjustmentDate",
        "postModificationARMPaymentRecastFrequency",
        "postModificationLifetimeFloorPercentage",
        "postModificationLifetimeCeilingPercentage",
        "postModificationInitialInterestRateIncreasePercentage",
        "postModificationInitialInterestRateDecreasePercentage",
        "postModificationSubsequentInterestIncreasePercentage",
        "postModificationSubsequentInterestRateDecreasePercentage",
        "postModificationPaymentCapPercentage",
        "postModificationPaymentMethodAfterRecastCode",
        "postModificationARMInterestRateTeaserNumber",
        "postModificationARMPaymentTeaserNumber",
        "postModificationARMNegativeAmortizationIndicator",
        "postModificationARMNegativeAmortizationCapPercentage",
        "postModificationInterestOnlyTermNumber",
        "postModificationInterestOnlyLastPaymentDate",
        "postModificationBalloonAmount",
        "postModificationInterestRateStepIndicator",
        "postModificationStepInterestPercentage",
        "postModificationStepDate",
        "postModificationStepPrincipalInterestPaymentAmount",
        "postModificationStepNumber",
        "postModificationMaximumFutureStepAgreementPercentage",
        "postModificationMaximumStepAgreementRateDate",
        "nonInterestBearingDeferredPrincipalCumulativeAmount",
        "nonInterestBearingDeferredPrincipalReportingPeriodAmount",
        "recoveryDeferredPrincipalReportingPeriodAmount",
        "nonInterestBearingDeferredPaidFullAmount",
        "nonInterestBearingDeferredInterestFeeReportingPeriodAmount",
        "nonInterestBearingDeferredInterestFeeCumulativeAmount",
        "recoveryDeferredInterestFeeReportingPeriodAmount",
        "mostRecentForbearancePlanOrTrialModificationStartDate",
        "mostRecentForbearancePlanOrTrialModificationScheduledEndDate",
        "mostRecentTrialModificationViolatedDate",
        "mostRecentRepaymentPlanStartDate",
        "mostRecentRepaymentPlanScheduledEndDate",
        "mostRecentRepaymentPlanViolatedDate",
        "shortSaleAcceptedOfferAmount",
        "mostRecentLossMitigationExitDate",
        "mostRecentLossMitigationExitCode",
        "attorneyReferralDate",
        "foreclosureDelayReasonCode",
        "foreclosureExitDate",
        "foreclosureExitReasonCode",
        "noticeOfIntentDate",
        "mostRecentAcceptedREOOfferAmount",
        "mostRecentAcceptedREOOfferDate",
        "grossLiquidationProceedsAmount",
        "netSalesProceedsAmount",
        "reportingPeriodLossPassedToIssuingEntityAmount",
        "cumulativeTotalLossPassedToIssuingEntityAmount",
        "subsequentRecoveryAmount",
        "evictionIndicator",
        "reoExitDate",
        "reoExitReasonCode",
        "UPBLiquidationAmount",
        "servicingFeesClaimedAmount",
        "servicerAdvanceReimbursedPrincipalAmount",
        "servicerAdvanceReimbursedInterestAmount",
        "servicerAdvanceReimbursedTaxesInsuranceAmount",
        "servicerAdvanceReimbursedCorporateAmount",
        "REOManagementFeesAmount",
        "cashKeyDeedAmount",
        "performanceIncentiveFeesAmount",
        "mortgageInsuranceClaimFiledDate",
        "mortgageInsuranceClaimAmount",
        "mortgageInsuranceClaimPaidDate",
        "mortgageInsuranceClaimPaidAmount",
        "mortgageInsuranceClaimDeniedRescindedDate",
        "marketableTitleTransferDate",
        "nonPayStatusCode",
        "reportingActionCode",
        "GroupID",
        "reportingPeriodBeginningDate",
        "reportingPeriodEndDate",
        "originationDate",
        "originalTermLoanNumber",
        "maturityDate",
        "interestRateSecuritizationPercentage",
        "interestAccrualMethodCode",
        "firstLoanPaymentDueDate",
        "lienPositionSecuritizationCode",
        "loanStructureCode",
        "paymentTypeCode",
        "periodicPrincipalAndInterestPaymentSecuritizationAmount",
        "scheduledPrincipalBalanceSecuritizationAmount",
        "NumberPropertiesSecuritization",
        "NumberProperties",
        "graceDaysAllowedNumber",
        "interestOnlyIndicator",
        "prepaymentPremiumIndicator",
        "modifiedIndicator",
        "armIndexCode",
        "firstRateAdjustmentDate",
        "firstPaymentAdjustmentDate",
        "armMarginNumber",
        "lifetimeRateCapPercentage",
        "periodicRateIncreaseLimitPercentage",
        "periodicRateDecreaseLimitPercentage",
        "periodicPaymentAdjustmentMaximumAmount",
        "periodicPaymentAdjustmentMaximumPercent",
        "rateResetFrequencyCode",
        "paymentResetFrequencyCode",
        "indexLookbackDaysNumber",
        "prepaymentLockOutEndDate",
        "yieldMaintenanceEndDate",
        "prepaymentPremiumsEndDate",
        "maximumNegativeAmortizationAllowedPercentage",
        "maximumNegativeAmortizationAllowedAmount",
        "negativeAmortizationDeferredInterestCapAmount",
        "deferredInterestCumulativeAmount",
        "deferredInterestCollectedAmount",
        "property",
        "reportPeriodModificationIndicator",
        "reportPeriodBeginningScheduleLoanBalanceAmount",
        "totalScheduledPrincipalInterestDueAmount",
        "servicerTrusteeFeeRatePercentage",
        "unscheduledPrincipalCollectedAmount",
        "reportPeriodEndActualBalanceAmount",
        "reportPeriodEndScheduledLoanBalanceAmount",
        "hyperAmortizingDate",
        "servicingAdvanceMethodCode",
        "nonRecoverabilityIndicator",
        "totalPrincipalInterestAdvancedOutstandingAmount",
        "totalTaxesInsuranceAdvancesOutstandingAmount",
        "otherExpensesAdvancedOutstandingAmount",
        "paymentStatusLoanCode",
        "armIndexRatePercentage",
        "nextInterestRateChangeAdjustmentDate",
        "nextPaymentAdjustmentDate",
        "mostRecentSpecialServicerTransferDate",
        "mostRecentMasterServicerReturnDate",
        "realizedLossToTrustAmount",
        "liquidationPrepaymentCode",
        "liquidationPrepaymentDate",
        "prepaymentPremiumYieldMaintenanceReceivedAmount",
        "workoutStrategyCode",
        "lastModificationDate",
        "modificationCode",
        "postModificationPaymentAmount",
        "postModificationAmortizationPeriodAmount",
        "propertyName",
        "propertyAddress",
        "propertyCity",
        "propertyState",
        "propertyZip",
        "propertyCounty",
        "netRentableSquareFeetNumber",
        "netRentableSquareFeetSecuritizationNumber",
        "unitsBedsRoomsNumber",
        "unitsBedsRoomsSecuritizationNumber",
        "yearBuiltNumber",
        "yearLastRenovated",
        "valuationSecuritizationAmount",
        "valuationSourceSecuritizationCode",
        "valuationSecuritizationDate",
        "mostRecentValuationAmount",
        "mostRecentValuationDate",
        "mostRecentValuationSourceCode",
        "physicalOccupancySecuritizationPercentage",
        "mostRecentPhysicalOccupancyPercentage",
        "propertyStatusCode",
        "defeasanceOptionStartDate",
        "DefeasedStatusCode",
        "largestTenant",
        "squareFeetLargestTenantNumber",
        "leaseExpirationLargestTenantDate",
        "secondLargestTenant",
        "squareFeetSecondLargestTenantNumber",
        "leaseExpirationSecondLargestTenantDate",
        "thirdLargestTenant",
        "squareFeetThirdLargestTenantNumber",
        "leaseExpirationThirdLargestTenantDate",
        "financialsSecuritizationDate",
        "mostRecentFinancialsStartDate",
        "mostRecentFinancialsEndDate",
        "revenueSecuritizationAmount",
        "mostRecentRevenueAmount",
        "operatingExpensesSecuritizationAmount",
        "operatingExpensesAmount",
        "netOperatingIncomeSecuritizationAmount",
        "mostRecentNetOperatingIncomeAmount",
        "netCashFlowFlowSecuritizationAmount",
        "mostRecentNetCashFlowAmount",
        "netOperatingIncomeNetCashFlowSecuritizationCode",
        "netOperatingIncomeNetCashFlowCode",
        "mostRecentDebtServiceAmount",
        "debtServiceCoverageNetOperatingIncomeSecuritizationPercentage",
        "mostRecentDebtServiceCoverageNetOperatingIncomePercentage",
        "debtServiceCoverageNetCashFlowSecuritizationPercentage",
        "mostRecentDebtServiceCoverageNetCashFlowpercentage",
        "debtServiceCoverageSecuritizationCode",
        "mostRecentDebtServiceCoverageCode",
        "mostRecentAnnualLeaseRolloverReviewDate",
        "reportingPeriodEndingDate",
        "originalLoanTerm",
        "loanMaturityDate",
        "interestCalculationTypeCode",
        "originalFirstPaymentDate",
        "gracePeriodNumber",
        "subvented",
        "vehicleManufacturerName",
        "vehicleModelName",
        "vehicleNewUsedCode",
        "vehicleModelYear",
        "vehicleTypeCode",
        "vehicleValueAmount",
        "vehicleValueSourceCode",
        "obligorCreditScoreType",
        "obligorCreditScore",
        "coObligorIndicator",
        "paymentToIncomePercentage",
        "obligorGeographicLocation",
        "reportingPeriodModificationIndicator",
        "nextReportingPeriodPaymentAmountDue",
        "otherServicerFeeRetainedByServicer",
        "otherAssessedUncollectedServicerFeeAmount",
        "reportingPeriodActualEndBalanceAmount",
        "totalActualAmountPaid",
        "servicerAdvancedAmount",
        "currentDelinquencyStatus",
        "chargedoffPrincipalAmount",
        "recoveredAmount",
        "modificationTypeCode",
        "paymentExtendedNumber",
        "repossessedIndicator",
        "repossessedProceedsAmount",
        "reportingPeriodBeginDate",
        "acquisitionCost",
        "originalLeaseTermNumber",
        "scheduledTerminationDate",
        "gracePeriod",
        "baseResidualValue",
        "baseResidualSourceCode",
        "contractResidualValue",
        "lesseeCreditScoreType",
        "lesseeCreditScore",
        "lesseeIncomeVerificationLevelCode",
        "lesseeEmploymentVerificationCode",
        "coLesseePresentIndicator",
        "lesseeGeographicLocation",
        "remainingTermNumber",
        "reportingPeriodSecuritizationValueAmount",
        "securitizationDiscountRate",
        "otherLeaseLevelServicingFeesRetainedAmount",
        "reportingPeriodEndingActualBalanceAmount",
        "reportingPeriodEndActualSecuritizationAmount",
        "primaryLeaseServicerName",
        "DemandResolutionDate",
        "repurchaseOrReplacementReasonCode",
        "chargedOffAmount",
        "leaseExtended",
        "terminationIndicator",
        "excessFeeAmount",
        "liquidationProceedsAmount",
        "commentNumber", "commentColumn", "commentDescription",
        'previousAccessionNumber', 'itemNumber', 'fieldName', 'notes'
      ),
      nameActual = c(
        "priceExerciseConversion",
        "dateDeemedExecution",
        "codeOwnershipDirectIndirect",
        "idDocument",
        "isEquitySwapInvolved",
        "dateExercised",
        "dateExpiration",
        "descriptionFootnote",
        "isDirector",
        "isOfficer",
        "isOther",
        "idCIKIssuer",
        "nameIssuer",
        "idTickerIssuer",
        "isTenPercentOwner",
        "descriptionNatureOfOwnership",
        "isNoSecuritiesOwned",
        "isNotSubjectToSection16",
        "titleOfficer",
        "descriptionOtherText",
        "dateReport",
        "countSharesOwnedPostTransaction",
        "descriptionRemarks",
        "idCIKOwner",
        "cityOwenr",
        "nameOwner",
        "stateOwner",
        "descriptionStateOwner",
        "addressStreet1Owner",
        "addressStreet2Owner",
        "zipcodeOwner",
        "idSchema",
        "titleSecurity",
        "countSharesOwnedPostTransaction",
        "dateSignature",
        "nameSignature",
        "codeTransactionAcquiredDisposed",
        "codeTransaction",
        "dateTransaction",
        "idFormTransaction",
        "pricePerShareTransaction",
        "countSharesTransaction",
        "idCodeTimelinessTransaction",
        "amountTransaction",
        "countSharesUnderlying",
        "titleSecurityUnderlying",
        "descriptionResponse", "isBusinessCombinationTransaction",
        "idCIK", "isMoreThanOneYear", "nameEntityPrevius", "listNameEntityPreviousEDGAR",
        "nameEntity", "typeEntity", "descriptionEntityTypeOther", "idFederalExemptionsExclusions",
        "typeIndustryGroup", "typeInvestmentFund", "descriptionInvestmentFund",
        "hasNonAccreditedInvestors", "countInvestorsNonAccredited",
        "countInvestorsActive", "cityEntity", "stateEntity", "descriptionStateEntity",
        "addressStreet1Entity", "addressStreet2Entity", "zipcodeEntity", "phoneNumberEntity", "listIssuerPreviousName",
        "jurisdictionOfInc", "isOverFiveYearsOld", "hasYearOfInc", "isFormedWithinFiveYears",
        "isYetToBeFormed", "rangeAgregateNetAssetValue", "rangeRevenue",
        "amountInvestmentMinimum", "amountSoldTotal", "amountOfferingTotal",
        "amountRemaining", "nameFirst", "nameLast", "nameMiddle", "relationshipEntity",
        "descriptionRelationship", "amountDollars", "isEstimate", "idCRDBroker",
        "nameBroker", "isForeignSolicitation", "idCRDRecipient",
        "nameRecipient", "stateDescription", "state", "listStatesSolicitation",
        "isAuthorizedRepresentative", "nameSignatory", "titleSignatory",
        "idForm", "codeTestOrLive", "dateFirstSale", "isYetToOccur",
        "isAmendment", "descriptionOtherType", "isDebtType", "isEquityType",
        "isMineralPropertyType", "isOptionToAcquireType", "isOtherType",
        "isPooledInvestmentFundType", "isSecurityToBeAcquiredType", "isTenantInCommonType",
        'isNotSubjectToSection16', 'addressStreet1Owner', 'addressStreet2Owner',
        "isLiveTestFlag", "isConfirmingCopyFlag", "isReturnCopyFlag", "isOverrideInternetFlag",
        "idCCC", "dateReportCalendarOrQuarter", "nameFilingManager", "addressStreet1FilingManager",
        "addressStreet2FilingManager", "cityFilingManager", "stateFilingManager",
        'descriptionStateFilingManager',
        "zipcodeFilingManager", "typeReport", "idSEC",
        "codeProvideInfoForInstruction5", "nameEntity", "titleEntity", "phoneEntity", "signatureEntity",
        "countOtherIncludedManagers", "countTableEntries", "amountValueHoldings",
        "isConfidentialOmitted", "nameIssuer", "classSecurities", "idCUSIP", "valueSecurities", "typeInvestmentDiscretion",
        "descriptionOtherManager", "codePutCall", "countSharesPrincipal", "codeSharesPrincipal", "countSharesVotingSole",
        "countSharesVotingShared", "countSharesVotingNone",

        "idSEC", "isSinceLastFiling", "codeJurisdictionOrganization",
        "yearIncorporation", "idSIC", "idIRS", "countEmployeesFullTime",
        "countEmployeesPartTime", "phoneEntity", "nameConnection", "nameIndustry",
        "amountCashEquivalents", "amountInvestmentSecurities", "amountAccountsReceivable",
        "amountPropertyPlantEquipment", "amountAssetsTotal", "amountAccountsPayable", "amountLongTermDebt",
        "amountLiabilitiesTotal", "amountStockholderEquityTotal", "amountLiabilitiesAndEquityTotal",
        "amountRevenuesTotal", "amountCostAndExpensesOfRevenue", "amountDepreciationAndAmortization",
        "amountNetIncome", "pershareEarningsBasic", "pershareEarningsDiluted",
        "nameAuditor", "nameCommonEquityClass", "amountCommonEquityOutstanding",
        "idCUSIPCommonEquity", "isCommonEquityPublic", "namePreferredEquityClass",
        "amountPreferredEquityOutstanding", "idCusipPreferrdEquity", "isdPreferredEquityPublic",
        "nameDebtSecuritiesClass", "amountOutstandingDebtSecurities", "idCUSIPDebtSecurities",
        "isDebtSecuritiesPublic", "isCertifyIfTrue", "isCertifyIfNotDisqualified",
        "codeTier1Tier2Offering", "codeFinancialStatementAuditStatus", "codeSecuritiesOfferedTypes",
        "codeOfferDelayedContinuous", "codeOfferingYearFlag", "codeOfferingAfterQualifFlag",
        "codeOfferingBestEffortsFlag", "codeSolicitationProposedOfferingFlag",
        "codeResaleSecuritiesAffiliates", "countSecuritiesOffered", "countSecuritiesOutstanding",
        "persharePrice", "amountOfferingIssuer", "amountOfferingExistingShareholdersSelling",
        "amountOfferingSold12MonthQualifiedOffering", "amountOfferingSoldConcurrent",
        "amountOfferingTotal", "nameUnderwritr",
        "amountUnderwritersFees", "nameAuditor", "amountAuditorFees",
        "nameLegal", "amountLegalFees", "namePromoter",
        "amountPromotersFees", "idCRDBroker", "amountOfferringProceedsNet",
        "descriptionResponse", "isJurisdictionsOfSecOfferedSame", "locatonJuridicationSecuritiesOffering",
        "locationDealersJuridicationSecuritiesOffering", "nameSecuritiesIssuer",
        "titleSecuritiesOffered", "amountSecuritiesIssued", "amountSecuritiesPrincipalHolder",
        "amountSecuritiesIssuedTotal", "nameSecuritiesActExemption",
        "isBadActor", "nameSalesCommissionsServiceProvider",
        "amountSalesCommissionsFees", "isJurisdictionsSecuritiesOfferingNone",
        "isUnRegisteredNone",
        "nameBlueSkyServiceProvider", "amountBlueSkyFees",
        'isTier1Tier2Offering', 'idForm', 'idForm', 'amountOfferingConsiderationBasis',
        'nameFindersFeeProvider' , 'amountFindersFee',
        'amountLoans', 'amountPropertyAndEquipment', 'amountDeposits', 'amountInterestIncomeTotal',
        'amountInterestExpenseTotal', 'descriptionOtherSecuritiesOffered',
        'commentFiling',
        "numberAssetType",
        "numberAsset",
        "numberAssetGroup",
        "dateReportPeriodBeginning",
        "dateReportPeriodEnd",
        "nameIssuer",
        "dateOriginalIssuance",
        "amountOriginalSecurity",
        "numberOriginalSecurityTerm",
        "dateSecurityMaturity",
        "numberOriginalAmortizationTerm",
        "percentageOriginalInterestRate",
        "codeAccrualType",
        "codeInterestRateType",
        "numberOriginalInterestOnlyTerm",
        "dateFirstPayment",
        "hasUnderwriting",
        "nameSecurityTitle",
        "numberDenomination",
        "nameCurrency",
        "nameTrustee",
        "numberSecFile",
        "idCIK",
        "hasCallable",
        "codePaymentFrequency",
        "hasZeroCoupon",
        "hasAssetAdded",
        "hasAssetModified",
        "amountReportPeriodBeginningAssetBalance",
        "amountReportPeriodBeginningScheduledAssetBalance",
        "amountReportPeriodScheduledPayment",
        "percentageReportPeriodInterestRate",
        "amountTotalActualPaid",
        "percentageActualInterestCollection",
        "amountActualPrincipalCollected",
        "amountActualOtherCollection",
        "amountOtherPrincipalAdjustment",
        "amountOtherInterestAdjustment",
        "amountScheduledInterest",
        "amountScheduledPrincipal",
        "amountEndReportingPeriodActualBalance",
        "amountEndReportingPeriodScheduledBalance",
        "percentageServicingFee",
        "amountServicingFlatFee",
        "codeZeroBalance",
        "dateZeroBalanceEffective",
        "numberRemainingTermToMaturity",
        "numberCurrentDelinquentStatus",
        "numberPaymentPastDueDays",
        "numberPaymentPastDue",
        "amountNextReportPeriodPaymentDue",
        "dateNextDue",
        "namePrimaryLoanServicer",
        "dateMostRecentServicingTransferReceived",
        "hasAssetSubjectToDemand",
        "codeStatusAssetSubjectToDemand",
        "amountRepurchase",
        "dateDemandResolution",
        "nameRepurchaser",
        "codeRepurchaseReplacementReason",
        "dateReportPeriodBegin",
        "codeOriginalLoanPurpose",
        "nameOriginator",
        "amountOriginalLoan",
        "dateOriginalLoanMaturity",
        "codeOriginalInterestRateType",
        "codeOriginalLienPosition",
        "amountMostRecentJuniorLoanBalance",
        "dateMostRecentJuniorLoanBalance",
        "amountMostRecentSeniorLoan",
        "dateMostRecentSeniorLoanAmount",
        "codeLoanTypeMostSeniorLien",
        "numberMostSeniorLienHybridPeriod",
        "percentageMostSeniorLienNegativeAmortizationLimit",
        "dateMostSeniorLienOrigination",
        "hasPrepaymentPenalty",
        "hasNegativeAmortization",
        "hasModification",
        "numberModification",
        "hasMortgageInsuranceRequirement",
        "hasBalloon",
        "codeCoveredHighCost",
        "codeServicerHazardInsurance",
        "amountRefinanceCashOut",
        "amountTotalOriginationDiscount",
        "hasBroker",
        "codeChannel",
        "numberNationalMortgageLicenseSystemCompany",
        "numberBuyDown",
        "numberLoanDelinquencyAdvance",
        "codeOriginationARMIndex",
        "percentageArmMargin",
        "percentageFullyIndexedRate",
        "numberInitialFixedRatePeriodHybridARM",
        "percentageInitialInterestRateDecrease",
        "percentageInitialInterestRateIncrease",
        "numberIndexLookback",
        "numberSubsequentInterestRateReset",
        "percentageLifetimeRateCeiling",
        "percentageLifetimeRateFloor",
        "percentageSubsequentInterestRateDecrease",
        "percentageSubsequentInterestRateIncrease",
        "numberSubsequentPaymentReset",
        "codeArmRound",
        "percentageArmRound",
        "hasOptionArm",
        "codePaymentMethodAfterRecast",
        "amountInitialMinimumPayment",
        "hasConvertible",
        "hasHELOC",
        "numberHELOCDraw",
        "codePrepaymentPenaltyCalculation",
        "codePrepaymentPenaltyType",
        "numberPrepaymentPenaltyTotalTerm",
        "numberPrepaymentPenaltyHardTerm",
        "amountNegativeAmortizationLimit",
        "numberNegativeAmortizationInitialRecast",
        "numberNegativeAmortizationSubsequentRecast",
        "amountNegativeAmortizationBalance",
        "numberInitialFixedPayment",
        "percentageInitialPaymentCap",
        "percentageSubsequentPaymentCap",
        "numberInitialMinimumPaymentReset",
        "numberSubsequentMinimumPaymentReset",
        "amountMinimumPayment",
        "locationGeographical",
        "codeOccupancyStatus",
        "codeMostRecentOccupancyStatus",
        "codePropertyType",
        "amountMostRecentPropertyValue",
        "codeMostRecentPropertyValueType",
        "dateMostRecentPropertyValue",
        "codeMostRecentAVMModel",
        "numberMostRecentAVMConfidence",
        "percentageOriginalCLTV",
        "percentageOriginalLTV",
        "numberOriginalObligor",
        "numberOriginalObligorCreditScore",
        "typeOriginalObligorCreditScore",
        "numberMostRecentObligorCreditScore",
        "typeMostRecentObligorCreditScore",
        "dateMostRecentObligorCreditScore",
        "codeObligorIncomeVerificationLevel",
        "hasIRSForm4506T",
        "percentageOriginatorFrontEndDTI",
        "percentageOriginatorBackEndDTI",
        "codeObligorEmploymentVerification",
        "codeObligorEmploymentLength",
        "codeObligorAssetVerification",
        "amountOriginalPledgedAssets",
        "codeQualificationMethod",
        "nameMortgageInsuranceCompany",
        "percentageMortgageInsuranceCoverage",
        "namePoolInsuranceCompany",
        "percentagePoolInsuranceStopLoss",
        "codeMortgageInsuranceCoverageType",
        "periodModificationHasReporting",
        "dateNextPaymentDue",
        "codeAdvancingMethod",
        "codeServicingAdvanceMethodology",
        "dateStopPrincipalInterestAdvancing",
        "amountReportingPeriodBeginningLoanBalance",
        "amountReportingPeriodBeginningScheduledLoanBalance",
        "amountNextReportingPeriodPaymentDue",
        "percentageReportingPeriodInterestRate",
        "percentageNextInterestRate",
        "feeamountOtherAssessedUncollectedServicer",
        "amountOtherServicingFeeRetainedByServicer",
        "amountReportingPeriodEndActualBalance",
        "amountReportingPeriodEndScheduledBalance",
        "amountReportingPeriodScheduledPayment",
        "amountActualInterestCollected",
        "amountActualOtherCollected",
        "datePaidThrough",
        "dateInterestPaidThrough",
        "amountPaidFull",
        "amountServicerAdvancedPrincipal",
        "amountServicerAdvancedRepaidPrincipal",
        "amountServicerAdvancedCumulativePrincipal",
        "amountServicerAdvanceInterest",
        "amountServicerAdvanceRepaidInterest",
        "amountServicerAdvanceCumulativeInterest",
        "amountServicerAdvanceTaxesInsurance",
        "amountServicerAdvanceRepaidTaxesInsurance",
        "amountServicerAdvanceCumulativeTaxesInsurance",
        "amountServicerAdvanceCorporate",
        "amountServicerAdvanceRepaidCorporate",
        "amountServicerAdvanceCumulativeCorporate",
        "codeMostRecentTwelveMonthHistory",
        "percentageNextResetRate",
        "dateNextPaymentChange",
        "dateNextInterestRateChange",
        "amountNextResetPayment",
        "hasExercisedArmConversionOption",
        "namePrimaryServicer",
        "nameMasterServicer",
        "nameSpecialServicer",
        "nameSubServicer",
        "hasAssetSubjectDemand",
        "codeAssetSubjectDemandStatus",
        "codeRepurchaseReplacement",
        "amountChargeOffPrincipal",
        "amountChargeOffInterest",
        "codeLossMitigationType",
        "codeMostRecentLoanModificationEvent",
        "dateMostRecentLoanModificationEffective",
        "datePostModificationMaturity",
        "codePostModificationInterestRateType",
        "codePostModificationAmortizationType",
        "percentagePostModificationInterest",
        "datePostModificationFirstPayment",
        "amountPostModificationLoanBalance",
        "amountPostModificationPrincipalInterestPayment",
        "amountTotalCap",
        "modificationIncomeVerificationHasAt",
        "percentageModificationFrontEndDebtToIncome",
        "percentageModificationBackEndDebtToIncome",
        "amountTotalDeferred",
        "amountForgivenPrincipalCumulative",
        "amountForgivenPrincipalReportingPeriod",
        "amountForgivenInterestCumulative",
        "amountForgivenInterestReportingPeriod",
        "amountActualEndingBalanceTotalDebt",
        "amountScheduledEndingBalanceTotalDebt",
        "codePostModificationARM",
        "codePostModificationARMIndex",
        "percentagePostModificationMargin",
        "numberPostModificationInterestReset",
        "datePostModificationNextReset",
        "numberPostModificationIndexLookback",
        "codePostModificationARMRounding",
        "percentagePostModificationARMRounding",
        "paymentPostModificationInitialMinimum",
        "datePostModificationNextPaymentAdjustment",
        "frequencyPostModificationARMPaymentRecast",
        "percentagePostModificationLifetimeFloor",
        "percentagePostModificationLifetimeCeiling",
        "percentagePostModificationInitialInterestRateIncrease",
        "percentagePostModificationInitialInterestRateDecrease",
        "percentagePostModificationSubsequentInterestIncrease",
        "percentagePostModificationSubsequentInterestRateDecrease",
        "percentagePostModificationPaymentCap",
        "codePostModificationPaymentMethodAfterRecast",
        "numberPostModificationARMInterestRateTeaser",
        "numberPostModificationARMPaymentTeaser",
        "hasPostModificationARMNegativeAmortization",
        "percentagePostModificationARMNegativeAmortizationCap",
        "numberPostModificationInterestOnlyTerm",
        "datePostModificationInterestOnlyLastPayment",
        "amountPostModificationBalloon",
        "hasPostModificationInterestRateStep",
        "percentagePostModificationStepInterest",
        "datePostModificationStep",
        "amountPostModificationStepPrincipalInterestPayment",
        "numberPostModificationStep",
        "percentagePostModificationMaximumFutureStepAgreement",
        "datePostModificationMaximumStepAgreementRate",
        "amountNonInterestBearingDeferredPrincipalCumulative",
        "amountNonInterestBearingDeferredPrincipalReportingPeriod",
        "amountRecoveryDeferredPrincipalReportingPeriod",
        "amountNonInterestBearingDeferredPaidFull",
        "amountNonInterestBearingDeferredInterestFeeReportingPeriod",
        "amountNonInterestBearingDeferredInterestFeeCumulative",
        "amountRecoveryDeferredInterestFeeReportingPeriod",
        "dateMostRecentForbearancePlanOrTrialModificationStart",
        "dateMostRecentForbearancePlanOrTrialModificationScheduledEnd",
        "dateMostRecentTrialModificationViolated",
        "dateMostRecentRepaymentPlanStart",
        "dateMostRecentRepaymentPlanScheduledEnd",
        "dateMostRecentRepaymentPlanViolated",
        "amountShortSaleAcceptedOffer",
        "dateMostRecentLossMitigationExit",
        "codeMostRecentLossMitigationExit",
        "dateAttorneyReferral",
        "codeForeclosureDelayReason",
        "dateForeclosureExit",
        "codeForeclosureExitReason",
        "dateNoticeOfIntent",
        "amountMostRecentAcceptedREOOffer",
        "dateMostRecentAcceptedREOOffer",
        "amountGrossLiquidationProceeds",
        "amountNetSalesProceeds",
        "amountReportingPeriodLossPassedToIssuingEntity",
        "amountCumulativeTotalLossPassedToIssuingEntity",
        "amountSubsequentRecovery",
        "hasEviction",
        "dateReoExit",
        "codeReoExitReason",
        "amountUPBLiquidation",
        "amountServicingFeesClaimed",
        "amountServicerAdvanceReimbursedPrincipal",
        "amountServicerAdvanceReimbursedInterest",
        "amountServicerAdvanceReimbursedTaxesInsurance",
        "amountServicerAdvanceReimbursedCorporate",
        "amountREOManagementFees",
        "amountCashKeyDeed",
        "amountPerformanceIncentiveFees",
        "dateMortgageInsuranceClaimFiled",
        "amountMortgageInsuranceClaim",
        "dateMortgageInsuranceClaimPaid",
        "amountMortgageInsuranceClaimPaid",
        "dateMortgageInsuranceClaimDeniedRescinded",
        "dateMarketableTitleTransfer",
        "codeNonPayStatus",
        "codeReportingAction",
        "idGroup",
        "dateReportingPeriodBeginning",
        "dateReportingPeriodEnd",
        "dateOrigination",
        "numberOriginalTermLoan",
        "dateMaturity",
        "percentageInterestRateSecuritization",
        "codeInterestAccrualMethod",
        "dateFirstLoanPaymentDue",
        "codeLienPositionSecuritization",
        "codeLoanStructure",
        "codePaymentType",
        "amountPeriodicPrincipalAndInterestPaymentSecuritization",
        "amountScheduledPrincipalBalanceSecuritization",
        "securitizationNumberProperties",
        "propertiesNumber",
        "numberGraceDaysAllowed",
        "hasInterestOnly",
        "hasPrepaymentPremium",
        "hasModified",
        "codeArmIndex",
        "dateFirstRateAdjustment",
        "dateFirstPaymentAdjustment",
        "numberArmMargin",
        "percentageLifetimeRateCap",
        "percentagePeriodicRateIncreaseLimit",
        "percentagePeriodicRateDecreaseLimit",
        "amountPeriodicPaymentAdjustmentMaximum",
        "percentPeriodicPaymentAdjustmentMaximum",
        "codeRateResetFrequency",
        "codePaymentResetFrequency",
        "numberIndexLookbackDays",
        "datePrepaymentLockOutEnd",
        "dateYieldMaintenanceEnd",
        "datePrepaymentPremiumsEnd",
        "percentageMaximumNegativeAmortizationAllowed",
        "amountMaximumNegativeAmortizationAllowed",
        "amountNegativeAmortizationDeferredInterestCap",
        "amountDeferredInterestCumulative",
        "amountDeferredInterestCollected",
        "propertyProperty",
        "hasReportPeriodModification",
        "amountReportPeriodBeginningScheduleLoanBalance",
        "amountTotalScheduledPrincipalInterestDue",
        "percentageServicerTrusteeFeeRate",
        "amountUnscheduledPrincipalCollected",
        "amountReportPeriodEndActualBalance",
        "amountReportPeriodEndScheduledLoanBalance",
        "dateHyperAmortizing",
        "codeServicingAdvanceMethod",
        "hasNonRecoverability",
        "amountTotalPrincipalInterestAdvancedOutstanding",
        "amountTotalTaxesInsuranceAdvancesOutstanding",
        "amountOtherExpensesAdvancedOutstanding",
        "codePaymentStatusLoan",
        "percentageArmIndexRate",
        "dateNextInterestRateChangeAdjustment",
        "dateNextPaymentAdjustment",
        "dateMostRecentSpecialServicerTransfer",
        "dateMostRecentMasterServicerReturn",
        "amountRealizedLossToTrust",
        "codeLiquidationPrepayment",
        "dateLiquidationPrepayment",
        "amountPrepaymentPremiumYieldMaintenanceReceived",
        "codeWorkoutStrategy",
        "dateLastModification",
        "codeModification",
        "amountPostModificationPayment",
        "amountPostModificationAmortizationPeriod",
        "nameProperty",
        "addressProperty",
        "cityProperty",
        "stateProperty",
        "zipcodeProperty",
        "countyProperty",
        "numberNetRentableSquareFeet",
        "numberNetRentableSquareFeetSecuritization",
        "numberUnitsBedsRooms",
        "numberUnitsBedsRoomsSecuritization",
        "yearBuilt",
        "yearLastRenovated",
        "amountValuationSecuritization",
        "codeValuationSourceSecuritization",
        "dateValuationSecuritization",
        "amountMostRecentValuation",
        "dateMostRecentValuation",
        "codeMostRecentValuationSource",
        "percentagePhysicalOccupancySecuritization",
        "percentageMostRecentPhysicalOccupancy",
        "codePropertyStatus",
        "dateDefeasanceOptionStart",
        "codeDefeasedStatus",
        "tenantLargest",
        "numberSquareFeetLargestTenant",
        "dateLeaseExpirationLargestTenant",
        "tenantSecondLargest",
        "numberSquareFeetSecondLargestTenant",
        "dateLeaseExpirationSecondLargestTenant",
        "tenantThirdLargest",
        "numberSquareFeetThirdLargestTenant",
        "dateLeaseExpirationThirdLargestTenant",
        "dateFinancialsSecuritization",
        "dateMostRecentFinancialsStart",
        "dateMostRecentFinancialsEnd",
        "amountRevenueSecuritization",
        "amountMostRecentRevenue",
        "amountOperatingExpensesSecuritization",
        "amountOperatingExpenses",
        "amountNetOperatingIncomeSecuritization",
        "amountMostRecentNetOperatingIncome",
        "amountNetCashFlowFlowSecuritization",
        "amountMostRecentNetCashFlow",
        "codeNetOperatingIncomeNetCashFlowSecuritization",
        "codeNetOperatingIncomeNetCashFlow",
        "amountMostRecentDebtService",
        "percentageDebtServiceCoverageNetOperatingIncomeSecuritization",
        "percentageMostRecentDebtServiceCoverageNetOperatingIncome",
        "percentageDebtServiceCoverageNetCashFlowSecuritization",
        "percentageMostRecentDebtServiceCoverageNetCash",
        "codeDebtServiceCoverageSecuritization",
        "codeMostRecentDebtServiceCoverage",
        "dateMostRecentAnnualLeaseRolloverReview",
        "dateReportingPeriodEnding",
        "termOriginalLoan",
        "dateLoanMaturity",
        "codeInterestCalculationType",
        "dateOriginalFirstPayment",
        "numberGracePeriod",
        "subventedSubvented",
        "nameVehicleManufacturer",
        "nameVehicleModel",
        "codeVehicleNewUsed",
        "yearVehicleModel",
        "codeVehicleType",
        "amountVehicleValue",
        "codeVehicleValueSource",
        "typeObligorCreditScore",
        "scoreObligorCredit",
        "hasCoObligor",
        "percentagePaymentToIncome",
        "locationObligorGeographic",
        "hasReportingPeriodModification",
        "amountPaymentDueNextReportingPeriod",
        "servicerOtherServicerFeeRetainedBy",
        "amountOtherAssessedUncollectedServicerFee",
        "amountReportingPeriodActualEndBalance",
        "amountPaidTotalActual",
        "amountServicerAdvanced",
        "isDelinquent",
        "amountChargedoffPrincipal",
        "amountRecovered",
        "codeModificationType",
        "numberPaymentExtended",
        "hasRepossessed",
        "amountRepossessedProceeds",
        "dateReportingPeriodBegin",
        "costAcquisition",
        "numberOriginalLeaseTerm",
        "dateScheduledTermination",
        "periodGrace",
        "valueBaseResidual",
        "codeBaseResidualSource",
        "valueContractResidual",
        "typeLesseeCreditScore",
        "scoreLesseeCredit",
        "codeLesseeIncomeVerificationLevel",
        "codeLesseeEmploymentVerification",
        "hasCoLesseePresent",
        "locationLesseeGeographic",
        "numberRemainingTerm",
        "amountReportingPeriodSecuritizationValue",
        "rateSecuritizationDiscount",
        "amountOtherLeaseLevelServicingFeesRetained",
        "amountReportingPeriodEndingActualBalance",
        "amountReportingPeriodEndActualSecuritization",
        "namePrimaryLeaseServicer",
        "dateDemandResolution",
        "codeRepurchaseOrReplacementReason",
        "amountChargedOff",
        "extendedLease",
        "hasTermination",
        "amountExcessFee",
        "amountLiquidationProceeds",
        "detailNumberComment", "columnComment", "descriptionComment",
        'idAccessionPrevious',
        'numberItem', 'nameField', 'descriptionNotes'
      )
    )}

get_filer_type_df <-
  function() {
    data_frame(
      idTypeFilerOwner = c(
        'insider',
        'private' ,
        'broker_dealer',
        'transfer_agent',
        'ia',
        'msd',
        'bank',
        'inv_co'
      ),
      typeFilerOwner = c(
        'Insider',
        'Private Placement',
        'Broker Dealer',
        'Transfer Agent',
        'Investment Advisor',
        'Bank',
        'Municipal Securities Dealer',
        'Investment Company'
      )
    ) %>%
      mutate_all(str_to_upper)
  }

#' Form-D dictionary
#'
#' This function returns searchable
#' industries for parsed SEC Form-D
#' filings
#'
#' @return a \code{data_frame}
#' @export
#' @import dplyr
#' @examples
#' get_dictionary_form_d_categories()
get_dictionary_form_d_categories <-
  function() {
    category_df <-
      dplyr::data_frame(
        idIndustry = 1:35,
        nameIndustry = c(
          "AGRICULTURE",
          "AIRLINES AND AIRPORTS",
          "BIOTECHNOLOGY",
          "BUSINESS SERVICES",
          "COAL MINING",
          "COMMERCIAL REAL ESTATE",
          "COMMERCIAL BANKING",
          "COMPUTERS",
          "CONSTRUCTION",
          "ELECTRIC UTILITIES",
          "ENERGY CONSERVATION",
          "ENVIORNMENTAL SERVICES",
          "HEALTH INSURANCE",
          "HOSPITALS AND PHYSICIANS",
          "INSURANCE",
          "INVESTING",
          "INVESTMENT BANKING",
          "LODGING AND CONVETION",
          "MANUFACTURING",
          "OIL AND GAS",
          "OTHER",
          "OTHER BANKING AND FINANCIAL SERVICES",
          "OTHER ENERGY",
          "OTHER HEALTH CARE",
          "OTHER REAL ESTATE",
          "OTHER TECHNOLOGY",
          "OTHER TRAVEL",
          "PHARMACEUTICALS",
          "POOLED INVESTMENT FUND",
          "REITS AND FINANCE",
          "RESIDENTIAL REAL ESTATE",
          "RESTAURANTS",
          "RETAIL",
          "TELECOMMUNICATIONS",
          "TRAVEL AND TOURISM"
        ),
        codeIndustryParent = c(
          "OTHER",
          "TRAVEL",
          "HEALTH",
          "OTHER",
          "ENERGY",
          "REAL",
          "FINANCE",
          "TECH",
          "REAL",
          "ENERGY",
          "ENERGY",
          "ENERGY",
          "HEALTH",
          "HEALTH",
          "FINANCE",
          "FINANCE",
          "FINANCE",
          "TRAVEL",
          "OTHER",
          "ENERGY",
          "OTHER",
          "FINANCE",
          "ENERGY",
          "HEALTH",
          "REAL",
          "TECH",
          "TRAVEL",
          "HEALTH",
          "FINANCE",
          "REAL",
          "REAL",
          "OTHER",
          "OTHER",
          "TECH",
          "TRAVEL"
        ),
        nameIndustryParent = c(
          "OTHER",
          "TRAVEL AND LEISURE",
          "HEALTHCARE",
          "OTHER",
          "ENERGY",
          "REAL ESTATE",
          "FINANCIAL",
          "TECHNOLOGY",
          "REAL ESTATE",
          "ENERGY",
          "ENERGY",
          "ENERGY",
          "HEALTHCARE",
          "HEALTHCARE",
          "FINANCIAL",
          "FINANCIAL",
          "FINANCIAL",
          "TRAVEL AND LEISURE",
          "OTHER",
          "ENERGY",
          "OTHER",
          "FINANCIAL",
          "ENERGY",
          "HEALTHCARE",
          "REAL ESTATE",
          "TECHNOLOGY",
          "TRAVEL AND LEISURE",
          "HEALTHCARE",
          "FINANCIAL",
          "REAL ESTATE",
          "REAL ESTATE",
          "OTHER",
          "OTHER",
          "TECHNOLOGY",
          "TRAVEL AND LEISURE"
        )
      )
    return(category_df)
  }

get_insider_code_df <-
  function() {
    insider_df <-
      data_frame(
        idInsiderTransaction =
          c(
            "A",
            "C",
            "D",
            "F",
            "G",
            "H",
            "I",
            "J",
            "K",
            "L",
            "M",
            "NONE",
            "O",
            "P",
            "S",
            "U",
            "V",
            "W",
            "X",
            "Z"
          ),
        nameInsiderTransaction = c(
          "AWARD",
          "CONVEYANCE",
          "DISPOSITION TO ISSUER",
          "PAYMENT WITH SECURITIES",
          "GIFT",
          "EXPIRATION OF LONG DERIVATIVE POSITION",
          "DISCRETIONARY TRANSACTION",
          "OTHER",
          "EQUITY SWAP OR SIMILAR",
          "SMALL ACQUISITIONS",
          "EXEMPT",
          NA,
          "OTM EXERCISE",
          "PURCHASE",
          "SALE",
          "MERGER AND ACQUISITION",
          "REPORTED EARLY",
          "WILL OR LAWS OF DESCENT",
          "ITM OR ATM EXERCISE",
          "DEPOSIT INTO/WITHDRAWAL FROM VOTING TRUST"
        ),
        idTypeInsiderTransaction = c(
          "A",
          "D",
          "D",
          "D",
          "D",
          NA,
          NA,
          NA,
          NA,
          "A",
          "A",
          NA,
          "A",
          "A",
          "D",
          NA,
          NA,
          "D",
          "A",
          "D"
        )
      )
    return(insider_df)
  }

#' SEC filing code dictionary
#'
#' This function returns a
#' dictionary of SEC form filing types
#'
#' @return a \code{data_frame}
#' @export
#' @import dplyr stringr
#' @family SEC
#' @family dictionary
#'
#' @examples
#' get_dictionary_sec_filing_codes()
get_dictionary_sec_filing_codes <-
  function() {
    data_frame(
      idFormType = c(
        "1.01",
        "1.02",
        "1.03",
        "1.04",
        "2.01",
        "2.02",
        "2.03",
        "2.04",
        "2.05",
        "2.06",
        "3.01",
        "3.02",
        "3.03",
        "4.01",
        "4.02",
        "5.01",
        "5.02",
        "5.03",
        "5.04",
        "5.05",
        "5.06",
        "5.07",
        "5.08",
        "6.01",
        "6.02",
        "6.03",
        "6.04",
        "6.05",
        "7.01",
        "8.01",
        "9.01"
      ),
      nameFormType = c(
        "Entry into a Material Definitive Agreement",
        "Termination of a Material Definitive Agreement",
        "Bankruptcy or Receivership",
        "Mine Safety Ð Reporting of Shutdowns and Patterns of Violations",
        "Completion of Acquisition or Disposition of Assets",
        "Results of Operations and Financial Condition",
        "Creation of a Direct Financial Obligation or an Obligation under an Off-Balance Sheet Arrangement of a Registrant",
        "Triggering Events That Accelerate or Increase a Direct Financial Obligation or an Obligation under an Off-Balance Sheet Arrangement",
        "Costs Associated with Exit or Disposal Activities",
        "Material Impairments",
        "Notice of Delisting or Failure to Satisfy a Continued Listing Rule or Standard; Transfer of Listing",
        "Unregistered Sales of Equity Securities",
        "Material Modification to Rights of Security Holders",
        "Changes in Registrant's Certifying Accountant",
        "Non-Reliance on Previously Issued Financial Statements or a Related Audit Report or Completed Interim Review",
        "Changes in Control of Registrant",
        "Departure of Directors or Certain Officers; Election of Directors; Appointment of Certain Officers; Compensatory Arrangements of Certain Officers",
        "Amendments to Articles of Incorporation or Bylaws; Change in Fiscal Year",
        "Temporary Suspension of Trading Under Registrant's Employee Benefit Plans",
        "Amendments to the Registrant's Code of Ethics, or Waiver of a Provision of the Code of Ethics",
        "Change in Shell Company Status",
        "Submission of Matters to a Vote of Security Holders",
        "Shareholder Director Nominations",
        "ABS Informational and Computational Material",
        "Change of Servicer or Trustee",
        "Change in Credit Enhancement or Other External Support",
        "Failure to Make a Required Distribution",
        "Securities Act Updating Disclosure",
        "Regulation FD Disclosure",
        "Other Events",
        "Financial Statements and Exhibits"
      ) %>% stringr::str_to_upper()
    )

  }

#' SEC form codes
#'
#' This function returns a
#' dictionary of SEC form codes
#'
#' @return a \code{data_frame}
#' @export
#' @family SEC
#' @family dictionary
#'
#' @examples
#' get_dictionary_sec_form_codes()
get_dictionary_sec_form_codes <-
  function() {
    data_frame(
      idForm = c(
        "R",
        "A",
        "Q",
        "CR",
        "REG",
        "REGX",
        "O",
        "P",
        "X",
        "W",
        "SEC",
        "PROXY",
        "CT",
        "IS",
        "CO",
        "T"
      ),
      nameForm = c(
        "Other Report",
        "Annual Report",
        "Quarterly Report",
        "Current Report",
        "Registration",
        "Private Offering",
        "Ownership",
        "Prospectus",
        "Exemption",
        "Withdrawal",
        "SEC Correspondence",
        "Proxy Statement",
        "Confidential Treatment",
        "Initial Statement",
        "Change in Ownership",
        "Trades"
      ) %>% stringr::str_to_upper()
    )
  }

get_company_type_df <-
  function() {
    data_frame(
      idCompanyType = c(
        "ic",
        "i",
        "ia",
        "bd",
        "m",
        "t",
        "b",
        "c",
        "p",
        "etf",
        "mmf",
        "mf",
        "uit",
        "cef"
      ),
      nameCompanyType = c(
        "Investment Company",
        "Insider",
        "Investment Adviser",
        "Broker-dealer",
        "Municipal Securities Dealer",
        "Transfer Agent",
        "Bank",
        "Company",
        "Private Issuer",
        "ETF",
        "Money Market Fund",
        "Mutual Fund",
        "UIT",
        "Closed-end Fund"
      )
    )
  }

#' SEC Rule dictionary
#'
#' This function retuns a
#' dictionary of SEC rules
#'
#' @return a \code{data_frame}
#' @export
#' @import dplyr stringr
#'
#' @examples
#' get_dictionary_sec_rules()
get_dictionary_sec_rules <-
  function() {
    data_frame(
      idRule = c(
        "06",
        "3C",
        "3C.7",
        "3C.1",
        "06b",
        "04",
        "46",
        "04.1",
        "04.2",
        "04.3",
        "05",
        "3C.6",
        "3C.5",
        "06c",
        "4a5",
        "3C.11",
        "3C.2",
        "3C.3",
        "3C.9",
        "3C.10",
        "3C.4",
        "3C.12",
        "3C.",
        "3C.14",
        "3"
      ),
      nameRule = c(
        "Rule 506",
        "Investment Company Act Section 3c",
        "Investment Company Act Section 3c",
        "Investment Company Act Section 3c",
        "Rule 506b",
        "Rule 504",
        "Rule 506c",
        "Rule 504b(1)(i)",
        "Rule 504b(1)(ii)",
        "Rule 504b(1)(iii)",
        "Rule 505",
        "Investment Company Act Section 3c",
        "Investment Company Act Section 3c",
        "Rule 506c",
        "Securities Act Section 4(a)(5)",
        "Investment Company Act Section 3c",
        "Investment Company Act Section 3c",
        "Investment Company Act Section 3c",
        "Investment Company Act Section 3c",
        "Investment Company Act Section 3c",
        "Investment Company Act Section 3c",
        "Investment Company Act Section 3c",
        "Investment Company Act Section 3c",
        "Investment Company Act Section 3c",
        "Investment Company Act Section 3c"
      )
    ) %>%
      mutate_all(str_to_upper)
  }



# form_parsing ------------------------------------------------------------


parse_full_form_names <-
  function(sec_names) {
    df_names <-
      1:length(sec_names) %>%
      map_df(function(x) {
        sec_name <-
          sec_names[[x]]

        name_pieces <-
          sec_name %>% str_replace_all('\\.value|\\.item', '')

        pieces <-
          name_pieces %>%
          str_split('\\.') %>%
          flatten_chr()

        pieces_no_num <-
          pieces[!pieces %>% str_detect("[0-9]")]
        peice_length <-
          pieces_no_num %>% length()

        is_street <-
          pieces %>% str_detect("street1|street2|Street1|Street2") %>% sum(na.rm = T) > 0

        name_item <-
          pieces_no_num[length(pieces_no_num)]

        if (sec_name %>% str_detect('filingManager')) {
          name_item <-
            pieces %>% paste0(collapse = '')

          df <-
            data_frame(nameSECFull = sec_name,
                       nameSEC = name_item)
          return(df)
        }

        if (is_street) {
          name_item <-
            pieces[pieces %>% str_detect("street1|street2|Street1|Street2")]
        }

        is_sig <-
          name_pieces %>% str_detect('signature') & peice_length == 1

        is_footnote <-
          sec_name %>% str_detect('footnote')

        is_issuer <-
          sec_name %>% str_detect('\\issuer.[A-Z]')

        is_federal <-
          sec_name %>% str_detect(pattern = "federalExemptionsExclusions")

        if (is_federal) {
          df <-
            data_frame(
              nameSECFull = sec_name,
              nameTable = pieces[[1]],
              nameSEC = name_item
            )

          return(df)
        }

        if (is_issuer) {

          items <-
            sec_name %>% str_split('\\.') %>% flatten_chr()

          countItem <-
            pieces[2] %>% readr::parse_number() %>% suppressWarnings()

          name_item <-
            items[length(items)]

          df <-
            data_frame(
              nameSECFull = sec_name,
              nameTable = 'issuer',
              countItem,
              nameSEC = name_item
            )
          return(df)
        }

        if (is_footnote) {
          if (pieces %>% length() == 1) {
            countItem <-
              0
            item <-
              pieces[[1]]
          } else {
            item <-
              pieces[[1]]
            countItem <-
              pieces[2] %>% readr::parse_number() %>% suppressWarnings()
          }
          return(data_frame(nameTable = 'footnotes', nameSECFull = sec_name, nameSEC = item, countItem))
        }

        if (is_sig) {
          df <-
            data_frame(nameTable = 'signatures', nameSECFull = sec_name, nameSEC = name_item)
          return(df)
        }

        if (peice_length == 1) {
          df <-
            data_frame(nameSECFull = sec_name, nameSEC = name_item)
          return(df)
        }

        piece_count <-
          length(pieces)

        if (piece_count == 1) {
          df <-
            data_frame(nameSECFull = sec_name, nameSEC = sec_name)
          return(df)
        }

        if (piece_count == 2 &!is_footnote) {


          df <-
            data_frame(nameSECFull = sec_name,
                       nameTable = pieces[[1]] ,
                       nameSEC = name_item)

          return(df)
        }

        if (piece_count > 2) {
          countItem <-
            pieces[2] %>% readr::parse_number() %>% suppressWarnings()

          df <-
            data_frame(
              nameSECFull = sec_name,
              nameTable = pieces[[1]] ,
              countItem,
              nameSEC = name_item
            )

          return(df)
        }

      }) %>%
      filter(!nameSEC == '')

    df_dictionary <-
      sec_form_title_df()

    has_missing_names <-
      df_names$nameSEC[!df_names$nameSEC %in% df_dictionary$nameSEC] %>%
      length() > 0
    if (has_missing_names) {
      missing <-
        df_names$nameSEC[!df_names$nameSEC %in% df_dictionary$nameSEC] %>%
        unique()

      missing_names <-
        missing %>%
        paste0(collapse = '\n')
      stop(list("Missing:\n", missing_names) %>%
             purrr::reduce(paste0))
    }

    df_names <-
      df_names %>%
      left_join(df_dictionary) %>%
      suppressWarnings() %>%
      suppressMessages()

    if (!'nameTable' %in% names(df_names)) {
      df_names <-
        df_names %>%
        mutate(nameTable = 'asset')
    }

    df_names <-
      df_names %>%
      select(nameTable, nameSECFull, nameSEC, nameActual, everything()) %>%
      mutate(nameTable = nameTable %>% str_replace('Id',''),
             nameTable = ifelse(nameTable %in% c('issuerCredentials','securitiesIssued'), NA, nameTable)) %>%
      suppressWarnings() %>%
      suppressMessages()
  }

parse_xml_tables <-
  function(url = "https://www.sec.gov/Archives/edgar/data/61004/000114036117000046/doc1.xml"){
    page <-
      url %>%
      xml2::read_xml()

    tables <-
      page %>%
      xml_contents() %>%
      xml_name() %>%
      unique()

    data <-
      1:length(tables) %>%
      map_df(function(x){
        table <-
          tables[[x]]

        if (table %in% c('headerData', 'formData')) {
          form_tables <-
            page %>% xml_contents() %>% xml_name()

          table_loc <-
            table %>% grep(form_tables)
          xml_nodes <-
            page %>%
            xml_contents() %>% .[[table_loc]]
        }

        if (table %in% c('infoTable' , 'assets')) {
          xml_nodes <-
            page %>%
            xml_contents()
        }

        if (table == 'comment') {
          value <-
            page %>% xml_contents() %>% xml_text()

          df <-
            data_frame(idTable = x, nameSECFull = table, value)
          return(df)

        }

        tables_special <-
          c('headerData', 'formData', 'infoTable', 'assets')

        if (!table %in% tables_special) {

          value_search <-
            list('//', table) %>% purrr::reduce(paste0)

          xml_nodes <-
            page %>%
            xml_contents() %>%
            xml_find_all(value_search)
        }
        if (xml_nodes %>% length() > 100) {
          list("Be patient there are ", xml_nodes %>% length() %>% formattable::comma(digits = 0), ' nodes to parse') %>%
            purrr::reduce(paste0) %>% message()
        }
        value_list <-
          xml_nodes %>%
          as_list()

        value_list <-
          value_list[value_list %>% map(length) %>% flatten_dbl() > 0]

        json_data <-
          value_list %>%
          jsonlite::toJSON(force = FALSE, dataframe = 'values') %>%
          jsonlite::fromJSON(simplifyDataFrame = TRUE, flatten = TRUE)

        wrong_output <-
          json_data %>% class() == 'array'

        if (wrong_output) {
          item <-
            xml_nodes %>% xml_name()
          value <-
            xml_nodes %>% xml_text()
          json_data <-
            data_frame(item, value) %>%
            spread(item, value)
        }

        if (json_data %>% length() == 0) {
          return(data_frame())
        }
        if ('summaryInfo' %in% names(json_data)) {
          json_data <-
            1:length(json_data) %>% map(
              function(x){
                js_d <- json_data[x]
                if ('summaryInfo' %in% names(js_d)) {
                  if (js_d$summaryInfo$clarificationResponses %>% length() == 0) {
                    js_d$summaryInfo$clarificationResponses <-
                      NULL
                  }
                }
                return(js_d)
              }) %>%
            flatten()

          json_data <-
            json_data[json_data %>% map(function(x){data.frame(x, stringsAsFactors = F)} %>% nrow()) > 0]
        }

        json_data <-
          json_data %>%
          data.frame(stringsAsFactors = FALSE) %>%
          as_data_frame() %>%
          mutate_all(as.character) %>%
          mutate(idTable = x) %>%
          gather(nameSECFull, value, -idTable) %>%
          arrange(idTable)
        return(json_data)
      })

    data <-
      data %>%
      mutate(isList = value %>% str_detect('list')) %>%
      filter(!isList) %>%
      select(-isList) %>%
      mutate(
        nameSECFull = nameSECFull %>% str_replace_all(
          "filerInfo.flags.|filerInfo.filer.|coverPage.|.filer.|\\flags.|filer.credentials.",
          ''
        ),
        nameSECFull = nameSECFull %>% str_replace_all('filerInfo.|issuerCredentials.', '')
      )

    closeAllConnections()
    rm(tables)
    rm(page)
    rm(url)
    return(data)
  }

parse_sec_form <-
  function(url = "https://www.sec.gov/Archives/edgar/data/61004/000114036117000046/doc1.xml",
           return_message = TRUE) {
    data <-
      parse_xml_tables(url = url)

    if (!'nameSECFull' %in% names(data)) {
      data <-
        data %>%
        mutate(nameSECFull = nameSEC)
    }

    cik <-
      url %>% str_replace_all('https://www.sec.gov/Archives/edgar/data/', '') %>% str_split('/') %>% flatten_chr() %>% .[[1]] %>% readr::parse_number() %>% suppressMessages()

    df_title <-
      sec_form_title_df()

    is_13FInfo <-
      url %>% str_detect('form13fInfoTable.xml|infotable.xml')
    sec_names <-
      data$nameSECFull %>% unique()

    df_names <-
      parse_full_form_names(sec_names = sec_names)

    df_names <-
      df_names %>%
      mutate(nameTable = ifelse(
        nameSECFull %>% str_detect("issuerAddress"),
        "issuerAddress",
        nameTable),
        nameTable =  ifelse(
          nameSECFull %>% str_detect("reportingOwner"),
          "reportingOwner",
          nameTable)
      ) %>%
      mutate(nameTable = ifelse(nameSECFull %>% str_detect("issuerInfo."), 'issuerInfo', nameTable),
             nameTable = ifelse(nameSECFull %>% str_detect("securitiesIssued."), 'securitiesIssued', nameTable),
             nameTable = ifelse(nameSECFull %>% str_detect("summaryInfo."), 'summaryInfo', nameTable),
             nameTable = ifelse(nameSECFull %>% str_detect("^comment[A-Z]"), 'Comments', nameTable)
      )

    if (is_13FInfo) {
      df_names <-
        df_names %>%
        mutate(nameTable = 'holdingsInformation')
    }
    if (!'nameSEC' %in% names(data)) {
      data <- data %>%
        mutate(nameSEC = nameSECFull)
    }
    data <-
      data %>%
      select(-nameSEC) %>%
      left_join(df_names) %>%
      mutate(nameActual = ifelse(nameSECFull == "X.1.A.A.", 'idForm', nameActual)) %>%
      suppressMessages()

    if ('countItem' %in% names(data)) {
      data <-
        data %>%
        select(nameTable, countItem, nameSECFull, nameActual, everything()) %>%
        mutate(countItem = countItem - 1) %>%
        suppressMessages()
    }

    if ('property' %in% data$nameTable) {
      data <-
        data %>%
        mutate(nameTable = ifelse(nameTable %>% is.na(), 'Asset', nameTable))
    }

    has_metadata <-
      data %>%
      filter(nameTable %>% is.na()) %>% nrow() > 0

    if (has_metadata) {
      df_metadata <-
        data %>%
        filter(nameTable %>% is.na()) %>%
        select(nameActual, value) %>%
        group_by(nameActual) %>%
        mutate(countItem = 1:n() - 1) %>%
        arrange(countItem) %>%
        ungroup() %>%
        filter(!nameActual %>% str_detect('idCCC')) %>%
        mutate(nameActual = ifelse(countItem == 0, nameActual, nameActual %>% paste0(countItem))) %>%
        select(-countItem)

      col_order <-
        df_metadata$nameActual

      df_metadata <-
        df_metadata %>%
        spread(nameActual, value) %>%
        select(one_of(col_order)) %>%
        mutate(urlSECFiling = url) %>%
        resolve_form_columns()
    } else {
      df_metadata <-
        data_frame(idCIKFiler = cik,
                   urlSECFiling = url)
    }

    tables <-
      data %>%
      filter(!nameTable %>% is.na()) %>%
      .$nameTable %>%
      unique()

    data <-
      1:length(tables) %>%
      map(function(x) {
        table <-
          tables[[x]]
        table_name <-
          list('data',
               table %>% substr(1, 1) %>% str_to_upper(),
               table %>% substr(2, nchar(table))) %>%
          purrr::reduce(paste0)

        table_df <-
          data %>%
          filter(nameTable == table) %>%
          select(matches("countItem"), nameActual, value) %>%
          select(which(colMeans(is.na(.)) < 1)) %>%
          group_by(nameActual) %>%
          mutate(countItem = 1:n() - 1) %>%
          ungroup()

        has_counts <-
          table_df$countItem %>% max(na.rm = TRUE) > 0

        if (has_counts) {
          table_df <-
            table_df %>%
            arrange(countItem)

          col_order <- c('countItem', table_df$nameActual)

          table_df <-
            table_df %>%
            spread(nameActual, value) %>%
            select(one_of(col_order)) %>%
            mutate(urlSECFiling = url) %>%
            resolve_form_columns()

          table_df <-
            table_df %>%
            nest(-urlSECFiling, .key = data)
        } else {
          table_df <-
            table_df %>%
            select(-countItem)
          col_order <- c(table_df$nameActual)

          table_df <-
            table_df %>%
            spread(nameActual, value) %>%
            select(one_of(col_order)) %>%
            resolve_form_columns() %>%
            mutate(urlSECFiling = url)

          table_df <-
            table_df %>%
            nest(-urlSECFiling, .key = data)
        }
        names(table_df)[[2]] <-
          table_name

        df_metadata <-
          df_metadata %>%
          left_join(table_df) %>%
          suppressMessages()

      }) %>%
      reduce(left_join) %>%
      suppressMessages()

    ## maybe add IDCK

    closeAllConnections()
    rm(df_metadata)
    return(data)
  }

parse_form_data <-
  function(all_filings, filter_parameter = 'isXBRLInstanceFile', return_message = TRUE) {
    df_search <-
      all_filings %>%
      filter_(.dots = filter_parameter)

    if (filter_parameter == 'isXBRLInstanceFile') {
      if (df_search %>% nrow() == 0) {
        return(data_frame())
      }
      parse_xbrl_filer_url_safe <-
        purrr::possibly(parse_xbrl_filer_url, data_frame())
      all_data <-
        df_search$urlSECFiling %>%
        unique() %>%
        map_df(function(x) {
          parse_xbrl_filer_url(url = x, return_message = return_message)
        })
      all_data <-
        all_data %>%
        select(-matches("idCIK1|nameFiler1")) %>%
        left_join(df_search %>% select(matches("idForm"), matches("idAccession"), matches("nameFile"), dateFiling, urlSECFiling)) %>%
        select(
          matches("idCIK"),
          matches("name[Entity]|name[Filer]"),
          dateFiling,
          matches("idForm"),
          matches("idAccession"),
          matches("nameFile"),
          everything()
        ) %>%
        suppressMessages()

      return(all_data)
    }

    if (filter_parameter == 'is13FFiling') {
      urls_df <-
        df_search %>% select(urlSECFiling, urlSECFilingDirectory)
      df_13f_urls <-
        1:nrow(urls_df) %>%
        map_df(function(x){

          row_df <-
            urls_df %>% slice(x)

          url <- row_df$urlSECFiling
          urlSECFilingDirectory <-
            row_df$urlSECFilingDirectory
          parts <-
            url %>%
            str_replace_all("https://www.sec.gov/Archives/edgar/data/", '') %>%
            str_split('\\/') %>%
            flatten_chr()
          idCIKFiler <-
            parts[[1]] %>% as.numeric()
          slugAccession <-
            parts[[2]]

          isPrimary <-
            parts[[3]] %>% str_detect("primary")

          data_frame(idCIKFiler, slugAccession, isPrimary, urlSECFiling = url, urlSECFilingDirectory)
        })

      slugs <-
        df_13f_urls$slugAccession %>% unique()

      df_13fs <-
        1:length(slugs) %>%
        map_df(function(x){
          slug <-
            slugs[[x]]
          df_period <-
            df_13f_urls %>%
            filter(slugAccession == slug)
          if (df_period %>% nrow() == 2) {
            primary_url <-
              df_period %>% filter(isPrimary) %>%
              .$urlSECFiling
            df_primary <-
              parse_sec_form(url = primary_url, return_message = return_message) %>%
              mutate(urlSECFiling = primary_url)

            df_primary <-
              df_primary %>%
              left_join(df_13f_urls) %>%
              suppressWarnings()

            no_primary_url <-
              df_period %>% filter(!isPrimary) %>%
              .$urlSECFiling

            urlSECFilingDirectory <-
              df_period %>% filter(!isPrimary) %>%
              .$urlSECFilingDirectory

            df_primary_no <-
              parse_sec_form(url = no_primary_url, return_message = return_message) %>%
              mutate(urlSECFiling = no_primary_url)

            data <-
              df_primary %>%
              select(-matches("urlSECFiling")) %>%
              left_join(df_primary_no %>% select(-matches("urlSECFiling"))) %>%
              mutate(urlSECFilingDirectory = urlSECFilingDirectory) %>%
              suppressMessages()
            return(data)
          } else {
            period_url <-
              df_period$urlFiling
            urlSECFilingDirectory <-
              df_period$urlSECFilingDirectory
            data <-
              parse_sec_form(url = period_url, return_message = return_message) %>%
              mutate(urlFiling = period_url) %>%
              left_join(df_period) %>%
              mutate(urlSECFilingDirectory = urlSECFilingDirectory)
            return(data)
          }
        })

      df_13fs <-
        df_13fs %>%
        left_join(urls_df) %>%
        left_join(df_search %>% select(dateFiling, datePeriodReport, datetimeAccepted, urlSECFilingDirectory, matches("urlTextFilingFull"))) %>%
        select(-matches("slugAcession")) %>%
        select(matches("idCIKFiler"), matches("nameFilingManager"), everything()) %>%
        select(dateFiling, everything()) %>%
        suppressMessages()

      return(df_13fs)
    }

    if (filter_parameter == 'isFormD') {
      if ('idForm' %in% names(df_search)){
        df_search <-
          df_search %>%
          filter(!idForm %>% str_detect("10"))
      }
    }
    if (df_search %>% nrow() == 0) {
      return(data_frame())
    }

    parse_sec_form_safe <-
      purrr::possibly(parse_sec_form, data_frame())
    all_data <-
      df_search$urlSECFiling %>%
      unique() %>%
      map_df(function(x) {
        parse_sec_form_safe(url = x, return_message = return_message)
      })

    if (all_data %>% nrow() == 0) {
      return(all_data)
    }

    all_data <-
      all_data %>%
      select(-matches("idCIK1|nameFiler1")) %>%
      left_join(df_search %>% select(matches("idForm"), matches("idAccession"), matches("nameFile"), dateFiling, urlSECFiling)) %>%
      select(
        matches("idCIK"),
        matches("name[Entity]|name[Filer]"),
        dateFiling,
        matches("idForm"),
        matches("idAccession"),
        matches("nameFile"),
        everything()
      ) %>%
      suppressMessages()

    if (filter_parameter == 'hasAssetFile') {
      if('dataComments' %in% names(all_data)) {
        df_comments <-
          all_data %>%
          select(idCIKFiler, matches("idAccession"), matches("dataComments")) %>%
          mutate(isNULL = dataComments %>% map_lgl(is_null)) %>%
          filter(!isNULL) %>%
          distinct() %>%
          select(-isNULL)

        all_data <-
          all_data %>%
          select(-dataComments) %>%
          mutate(isNULL = dataAsset %>% map_lgl(is_null)) %>%
          filter(!isNULL) %>%
          filter(!nameFile == "ASSET RELATED DOCUMENT") %>%
          distinct() %>%
          select(-isNULL) %>%
          left_join(df_comments) %>%
          suppressMessages()
      }
    }

    all_data <-
      all_data %>%
      select(which(colMeans(is.na(.)) < 1))
    return(all_data)
  }


# index_parsing -----------------------------------------------------------
parse_sec_filing_index <-
  function(urls, return_message = TRUE) {
    df <-
      data_frame()
    success <- function(res) {
      if (return_message) {
        list("Parsing: ", res$url) %>% purrr::reduce(paste0) %>% message()
      }
      page <-
        res$content %>%
        read_html()

      not_503 <-
        !res$status_code == 503

      cik <-
        res$url %>%
        str_split('data/') %>%
        flatten_chr() %>%
        .[[2]] %>%
        str_split('/') %>%
        flatten_chr() %>%
        .[[1]] %>%
        as.numeric()
      if (not_503){
        values <-
          page %>%
          html_nodes('.info') %>%
          html_text()

        items <-
          page %>%
          html_nodes('.infoHead') %>%
          html_text()

        all_items <-
          items %>%
          map_chr(function(x) {
            is_zero <-
              x %>% str_count('\\ ') == 0

            if (x == 'Accepted') {
              return("datetimeAccepted")
            }

            if (x == 'Documents') {
              return('countDocuments')
            }
            if (x == "items") {
              return('descriptionItems')
            }

            if (is_zero) {
              return('item' %>% paste0(x))
            }

            if (x == "Period of Report") {
              return("datePeriodReport")
            }

            if (x == "429 Reference" | x %>% str_detect("Reference")) {
              return("reference429")
            }

            name_items <-
              x %>% str_split('\\ ') %>%
              flatten_chr()

            first <-
              name_items[name_items %>% length()] %>% str_to_lower()

            end <-
              name_items[1:(name_items %>% length() - 1)] %>%
              paste0(collapse = '') %>%
              str_to_title()

            final_name <-
              list(first, end) %>% purrr::invoke(paste0, .)
            return(final_name)
          })
        search_url <-
          res$url
        df_metadata <-
          data_frame(item = all_items,
                     value = values) %>%
          mutate(urlSECFilingDirectory = search_url) %>%
          spread(item, value)

        df_metadata <-
          df_metadata %>%
          mutate_at(df_metadata %>% select(matches('count')) %>% names(),
                    funs(. %>% as.numeric())) %>%
          mutate_at(
            df_metadata %>% select(matches('^date[A-Z]')) %>%  select(-matches("datetime"))  %>% names(),
            funs(. %>% lubridate::ymd())
          ) %>%
          mutate_at(
            df_metadata %>% select(matches('^datetime')) %>%  select(-matches("datetime"))  %>% names(),
            funs(. %>% lubridate::ymd_hms())
          )

        urlSECFiling <-
          page %>%
          html_nodes('#formDiv a') %>%
          html_attr('href') %>%
          paste0('https://www.sec.gov', .)

        namehref <-
          page %>%
          html_nodes('#formDiv a') %>%
          html_text()

        files <-
          page %>%
          html_nodes('#formDiv td:nth-child(2)') %>%
          html_text() %>%
          str_to_upper()

        wrong_length <-
          !(namehref %>% length() == files %>% length())

        if (wrong_length) {
          namehref <-
            namehref[namehref %>% str_detect("\\.")]
          urlSECFiling <-
            urlSECFiling[2:length(urlSECFiling)]
        }

        files <-
          files %>%
          str_trim()

        types_form <-
          page %>%
          html_nodes('td:nth-child(4)') %>%
          html_text() %>%
          str_trim()

        types_form[types_form == ''] <-
          NA

        types_form <-
          types_form[1:length(files)]

        files[files == ''] <-
          NA
        search_url <-
          res$url
        data <-
          data_frame(
            nameFile = files,
            nameHref = namehref,
            typeForm = types_form,
            urlSECFiling
          ) %>%
          mutate(
            isXML = nameHref %>% str_detect("xml"),
            isForm3_4 = nameHref %>% str_detect('doc3.xml|doc4.xml'),
            isFormD = ifelse(
              isXML & typeForm %in% c("D", "D/A"),
              TRUE,
              FALSE
            ),
            is13FFiling = ifelse(
              isXML& typeForm %>% str_detect("13F-HR|INFORMATION TABLE"),
              TRUE,
              FALSE
            ),
            hasSmallOfferingData = ifelse(isXML &
                                            typeForm %>% str_detect("1-A|1-A/A"),
                                          TRUE,
                                          FALSE),
            hasSmallOfferingData = ifelse(typeForm == "C" & isXML, TRUE, hasSmallOfferingData),
            hasAssetFile = typeForm %>% str_detect("EX-102|EX-103")
          ) %>%
          tidyr::separate(nameHref,
                          into = c('nameHREF', 'typeFile'),
                          sep = '\\.') %>%
          mutate(urlSECFilingDirectory = search_url) %>%
          mutate(
            nameFile = ifelse(nameFile == '', NA, nameFile %>% str_to_upper()),
            isCompleteTextFiling = nameFile %>% str_detect("COMPLETE SUBMISSION"),
            isXBRLInstanceFile = ifelse(nameFile %>% str_detect("XBRL INSTANCE"), TRUE, FALSE),
            isImage = ifelse(typeFile %in% c('jpg', 'gif', 'tiff', 'png'), TRUE, FALSE),
            isPDF = ifelse(typeFile %in% c('pdf'), TRUE, FALSE)
          )

        data <-
          data %>%
          left_join(df_metadata) %>%
          mutate(idCIK = cik) %>%
          select(idCIK, matches("date"), matches("count"), everything()) %>%
          suppressWarnings() %>%
          suppressMessages()
      } else {
        search_url <-
          res$url
        data <-
          data_frame(idCIK = cik,
                     urlSECFilingDirectory = search_url)
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
    df
  }

get_all_filings <- function(urls, return_message = TRUE)  {
  df_filings <-
    urls %>%
    map_df(function(x){
      parse_sec_filing_index(urls = x, return_message = return_message)
    })
  closeAllConnections()
  return(df_filings)
}

get_all_filing_urls <-
  function(data, nest_data = TRUE,
           return_message = TRUE) {
    if (!'urlSECFilingDirectory' %in% names(data)) {
      stop("urlSECFilingDirectory needs to be in the data fields")
    }
    if (!'idAccession' %in% names(data)) {
      df_accession <-
        data$urlSECFilingDirectory %>%
        unique() %>%
        map_df(function(x){
          urlSECFilingDirectory <-
            x

          idAccession <-
            x %>% str_replace_all('https://www.sec.gov/Archives/edgar/data/', '') %>%
            str_split('\\/') %>%
            flatten_chr() %>% {
              .[length(.)] %>% str_replace_all('-index.htm', '')
            }
          data_frame(idAccession, urlSECFilingDirectory)
        })

      data <-
        data %>%
        left_join(df_accession) %>%
        suppressMessages()
    }
    data <-
      data %>%
      select(-matches("hasAssetFile|isFormD|is13F|isForm3_4|hasSmallOfferingData")) %>%
      filter(typeFile %>% str_detect("htm")) %>%
      group_by(idAccession) %>%
      mutate(countAccension = 1:n()) %>%
      filter(countAccension == max(countAccension)) %>%
      ungroup() %>%
      arrange(dateFiling)

    urls <-
      data$urlSECFilingDirectory

    df_all_filings <-
      get_all_filings(urls = urls, return_message = return_message)

    df_all_filings <-
      df_all_filings %>%
      left_join(data %>% select(urlSECFilingDirectory, countAccension, idAccession)) %>%
      suppressMessages()

    if (nest_data) {
      df_all_filings <-
        df_all_filings %>%
        nest(-c(idAccession, countAccension, urlSECFilingDirectory), .key = dataFilings)

    }
    return(df_all_filings)
  }

# Text Form ---------------------------------------------------------------
get_header_names <-
  function() {
    data_frame(
      nameSEC = c(
        "ACCEPTANCE-DATETIME",
        "ACCESSION NUMBER",
        "CONFORMED SUBMISSION TYPE",
        "PUBLIC DOCUMENT COUNT",
        "FILED AS OF DATE",
        "DATE AS OF CHANGE",
        "COMPANY CONFORMED NAME",
        "CENTRAL INDEX KEY",
        "STANDARD INDUSTRIAL CLASSIFICATION",
        "IRS NUMBER",
        "STATE OF INCORPORATION",
        "FISCAL YEAR END",
        "FORM TYPE",
        "SEC ACT",
        "SEC FILE NUMBER",
        "FILM NUMBER",
        "STREET 1",
        "CITY",
        "STATE",
        "ZIP",
        "BUSINESS PHONE",
        "FORMER CONFORMED NAME",
        "DATE OF NAME CHANGE",
        "STREET 2",
        "CONFORMED PERIOD OF REPORT",
        "ITEM INFORMATION"
      ),
      nameActual = c(
        "datetimeAccepted",
        "idAccession",
        "idForm",
        "countPublicDocuments",
        "dateFiling",
        "dateFilingChange",
        "nameCompany",
        "idCIK",
        "nameCodeSIC",
        "idIRS",
        "stateIncorporation",
        "monthdayFiscalYearEnd",
        "typeForm",
        "idSECAct",
        "idSEC",
        "idFilm",
        "addressStreet1",
        "city",
        "state",
        "zipcode",
        "telephone",
        "nameCompanyFormer",
        "dateNameChange",
        'addressStreet2',
        'dateReportPeriod',
        'descriptionItem'
      )
    )
  }

get_section_names <-
  function() {
    data_frame(nameSectionSEC = c(NA, "SUBJECT COMPANY", "FILED BY", 'ISSUER', 'REPORTING-OWNER'),
               nameSectionActual = c('', '', 'FilingEntity', 'Issuer', 'ownerReporting')
    )
  }
get_parent_names <-
  function() {
    data_frame(nameParentSEC = c(NA, "COMPANY DATA", "FILING VALUES", "BUSINESS ADDRESS", "MAIL ADDRESS",
                                 "FORMER COMPANY"),
               nameParentActual = c('', '', '', 'Business', 'Mailing', ''))
  }

parse_text_headers <- function(text_blob){
  header_start <-
    text_blob %>% grep("<SEC-HEADER>",.) + 1

  header_end <-
    text_blob %>% grep("</SEC-HEADER>",.) - 1

  header_text <-
    text_blob[header_start:header_end]

  header_text <-
    header_text %>% str_replace_all('\\<','') %>% str_replace_all('\\>',':')

  df_headers <- data_frame(text = header_text) %>%
    tidyr::separate(col = text, into = c('nameSEC', 'value'), sep = '\\:') %>%
    mutate(value = value %>% str_replace_all("\t", '')) %>%
    mutate(idRow = 1:n())

  df_parents <-
    df_headers %>%
    filter(value == '') %>%
    mutate(idRow = idRow + 1) %>%
    dplyr::rename(nameParentSEC = nameSEC) %>%
    select(-value)

  df_section <-
    df_parents %>%
    filter(nameParentSEC %in% c("SUBJECT COMPANY", "FILED BY")) %>%
    select(nameSectionSEC = nameParentSEC, idRow) %>%
    mutate(idRow = idRow + 1)

  df_parents <-
    df_parents %>%
    filter(!nameParentSEC %in% c("SUBJECT COMPANY", "FILED BY")) %>%
    left_join(df_section) %>%
    fill(nameSectionSEC) %>%
    select(nameSectionSEC, nameParentSEC, idRow) %>%
    suppressMessages()

  data <-
    df_headers %>%
    filter(!value == '') %>%
    left_join(df_parents) %>%
    select(idRow, nameSectionSEC, nameParentSEC, everything()) %>%
    tidyr::fill(nameSectionSEC) %>%
    tidyr::fill(nameParentSEC) %>%
    select(-idRow) %>%
    distinct() %>%
    suppressWarnings() %>%
    suppressMessages()

  df_parents <-
    get_parent_names()

  df_names <-
    get_header_names()
  df_sections <-
    get_section_names()

  has_missing_names <-
    data$nameSEC[!data$nameSEC %in% df_names$nameSEC] %>% length() > 0

  if (has_missing_names) {
    df_missing <-
      data$nameSEC[!data$nameSEC %in% df_names$nameSEC] %>% unique() %>%
      map_df(function(x){
        parts <-
          x %>% str_replace_all('\\-', ' ') %>%
          str_split('\\ ') %>% flatten_chr()


        first <-
          parts[length(parts)] %>%
          str_to_lower()

        is_cik <-
          first %>% str_detect('cik') %>% sum(na.rm = TRUE) > 0

        if (is_cik) {
          first <-
            'idCIK'
        }

        other <-
          list(parts[1:(length(parts) - 1)] %>% str_to_title) %>%
          purrr::reduce(paste0) %>%
          paste0(collapse = '')

        actual <-
          list(first,other) %>%
          purrr::reduce(paste0)

        data_frame(nameSEC = x, nameActual = actual)
      })

    df_names <- df_names %>%
      bind_rows(df_missing)
  }

  data <-
    data %>%
    left_join(df_parents) %>%
    left_join(df_sections) %>%
    left_join(df_names) %>%
    mutate(nameParentActual = ifelse(nameParentActual %>% is.na(), '', nameParentActual)) %>%
    suppressMessages() %>%
    unite(nameItem, nameActual, nameParentActual, nameSectionActual, sep = '') %>%
    select(nameItem, value) %>%
    suppressWarnings() %>%
    group_by(nameItem) %>%
    mutate(countItem = 1:n() - 1) %>%
    ungroup() %>%
    mutate(nameItem = ifelse(countItem == 0, nameItem, paste0(nameItem, countItem))) %>%
    suppressMessages() %>%
    select(-countItem)


  col_order <-
    data$nameItem

  data <-
    data %>%
    spread(nameItem, value) %>%
    select(one_of(col_order))

  data <-
    data %>%
    mutate_at(data %>% select(matches("datetime")) %>% names(),
              funs(. %>% lubridate::ymd_hms())) %>%
    mutate_at(data %>% select(matches("^date[A-Z]")) %>% select(-matches("datetime")) %>% names(),
              funs(. %>% lubridate::ymd())) %>%
    mutate_at(data %>% select(matches("idCIK|count|monthdayFiscalYearEnd")) %>% names(),
              funs(. %>% as.numeric())) %>%
    mutate_at(data %>% select(matches("name[A-Z]|type[A-Z]|description|class")) %>% names(),
              funs(. %>% stringr::str_to_upper()))

  if ('nameCodeSIC' %in% names(data)) {
    data <-
      data %>%
      separate(nameCodeSIC, into = c('nameIndustry', 'idSIC'), sep = '\\[') %>%
      mutate(nameIndustry = nameIndustry %>% str_trim() %>% str_to_upper(),
             idSIC = idSIC %>% readr::parse_number()) %>%
      suppressWarnings()
  }
  return(data)
}

parse_for_text <- function(text_blob) {
  text_start <-
    text_blob %>% grep("<TEXT>",.) %>% .[[1]] + 1

  text_end <-
    text_blob %>% grep("</TEXT>",.)

  text_end <-
    text_end %>% max() - 1

  df_text <-
    data_frame(textRow = text_blob[text_start:text_end]) %>%
    mutate(idRow = 1:n()) %>%
    select(idRow, everything())

  return(df_text)
}
parse_text_filing <-
  function(url = "https://www.sec.gov/Archives/edgar/data/732712/000119312517025716/0001193125-17-025716.txt") {
    text_blob <-
      url %>%
      readr::read_lines() %>% {
        .[!. == ''] %>%
          str_trim()
      }

    has_html <-
      text_blob %>% str_count("<HTML>") %>% sum(na.rm = TRUE) > 0
    has_xml <-
      text_blob %>% str_count("<XML>") %>% sum(na.rm = TRUE) > 0

    df_headers <-
      parse_text_headers(text_blob = text_blob)

    df_text <-
      parse_for_text(text_blob = text_blob) %>%
      mutate(idAccession = df_headers$idAccession) %>%
      nest(-idAccession, .key = 'textFiling')

    data <-
      df_headers %>%
      left_join(df_text) %>%
      mutate(urlSECFiling = url,
             hasHTML = has_html,
             hasXML = has_xml) %>%
      suppressWarnings() %>%
      suppressMessages() %>%
      select(matches("idCIK"), matches("dateFiling"), idAccession, matches("idForm"), matches("nameCompany"), everything())

    return(data)

  }


get_data_sec_complete_filings <-
  function(urls = c("https://www.sec.gov/Archives/edgar/data/732712/000119312517030264/0001193125-17-030264.txt", "https://www.sec.gov/Archives/edgar/data/732712/000161159317000024/0001611593-17-000024.txt", "https://www.sec.gov/Archives/edgar/data/1629703/000161159317000025/0001611593-17-000025.txt", "https://www.sec.gov/Archives/edgar/data/1284999/000161159317000014/0001611593-17-000014.txt"),
           return_message =  TRUE) {
    df <-
      data_frame()
    success <- function(res) {
      url <-
        res$url
      if (return_message) {
        list("Parsing: ", url, "\n") %>% purrr::reduce(paste0) %>% message()
      }

      data <-
        parse_text_filing(url = url)
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
    df
  }


# XBRL Finder -------------------------------------------------------------
parse_xbrl_filer_url <-
  function(url = "https://www.sec.gov/Archives/edgar/data/1037540/000165642316000023/bxp-20160930.xml",
           return_message = TRUE) {
    options(stringsAsFactors = FALSE, scipen = 999999)
    cik <-
      url %>%
      str_split('data/') %>%
      flatten_chr() %>%
      .[[2]] %>%
      str_split('/') %>%
      flatten_chr() %>%
      .[[1]] %>%
      as.numeric()
    td <-
      tempdir()
    tf <-
      tempfile(tmpdir = td, fileext = ".xml")

    url %>%
      curl::curl_download(destfile = tf)

    doc <-
      tf %>%
      XBRL::xbrlParse()


    ## Get a data frame with facts:
    df_fct <-
      XBRL::xbrlProcessFacts(doc) %>%
      as_data_frame()

    df_fct <-
      df_fct %>%
      mutate(
        isNumber = ifelse(!fact %>% readr::parse_number() %>% is.na(), TRUE, FALSE),
        amountFact = ifelse(isNumber == TRUE, fact %>% readr::parse_number(), NA)
      ) %>%
      separate(elementId,
               c('codeElement', 'nameElement'),
               sep = '\\_',
               remove = FALSE) %>%
      suppressWarnings()
    ## Get a data frame with contexts:
    df_cts <-
      XBRL::xbrlProcessContexts(doc) %>%
      as_data_frame()
    ## Get a data frame with units:
    df_unt <-
      XBRL::xbrlProcessUnits(doc) %>%
      as_data_frame()

    df_sch <-
      XBRL::xbrlGetSchemaName(doc) %>%
      as_data_frame()

    df_footnotes <-
      XBRL::xbrlProcessFootnotes(doc) %>%
      as_data_frame()


    ## Free the external memory used:
    XBRL::xbrlFree(doc)
    url_xsd <-
      url %>% str_replace(".xml", ".xsd")
    url_xsd %>%
      curl_download(destfile = tf)

    ## Parse the schema file:
    docS <-
      tf %>%
      XBRL::xbrlParse()
    ## Get roles:
    df_rls <-
      docS %>%
      XBRL::xbrlProcessRoles() %>%
      as_data_frame()

    ## calculation
    url_cal <-
      url %>% str_replace(".xml", "_cal.xml")
    if (suppressWarnings(httr::url_ok(url_cal))){
      url_cal %>%
        curl_download(destfile = tf)

      docS <-
        tf %>%
        XBRL::xbrlParse()

      df_calcs <-
        docS %>%
        XBRL::xbrlProcessArcs(arcType = 'calculation') %>%
        as_data_frame()
    } else {
      df_calcs <-
        data_frame()
    }

    ## definition
    url_def <-
      url %>% str_replace(".xml", "_def.xml")

    url_def %>%
      curl_download(destfile = tf)

    docS <-
      tf %>%
      XBRL::xbrlParse()

    df_defs <-
      docS %>%
      XBRL::xbrlProcessArcs(arcType = 'definition') %>%
      as_data_frame()

    ## labels
    url_lab <-
      url %>% str_replace(".xml", "_lab.xml")

    url_lab %>%
      curl_download(destfile = tf)

    docS <-
      tf %>%
      XBRL::xbrlParse()

    df_labels <-
      docS %>%
      XBRL::xbrlProcessLabels() %>%
      as_data_frame()

    ## presentation
    url_pre <-
      url %>% str_replace(".xml", "_pre.xml")

    url_pre %>%
      curl_download(destfile = tf)

    docS <-
      tf %>%
      XBRL::xbrlParse()

    ## Free the external memory used:
    tf %>%
      unlink()
    data <-
      data_frame(
        idCIK = cik,
        urlSECFiling = url,
        dataFacts = list(df_fct),
        dataContexts = list(df_cts),
        dataUnits = list(df_unt),
        dataFootnotes = list(df_footnotes),
        dataRoles = list(df_rls),
        dataCalculations = list(df_calcs) ,
        dataDefinitions = list(df_defs),
        dataLabel = list(df_labels)
      )
    td %>% unlink()
    tf %>% unlink()

    return(data)
  }


# dictionaries ------------------------------------------------------------
sec_form_title_df <-
  function() {
    data_frame(
      nameSEC = c(
        "conversionOrExercisePrice",
        "deemedExecutionDate",
        "directOrIndirectOwnership",
        "documentType",
        "equitySwapInvolved",
        "exerciseDate",
        "expirationDate",
        "footnote",
        "isDirector",
        "isOfficer",
        "isOther",
        "issuerCik",
        "issuerName",
        "issuerTradingSymbol",
        "isTenPercentOwner",
        "natureOfOwnership",
        "noSecuritiesOwned",
        "notSubjectToSection16",
        "officerTitle",
        "otherText",
        "periodOfReport",
        "postTransactionAmountsOwnedFollowingTransaction",
        "remarks",
        "rptOwnerCik",
        "rptOwnerCity",
        "rptOwnerName",
        "rptOwnerState",
        "rptOwnerStateDescription",
        "rptOwnerStreet1",
        "rptOwnerStreet2",
        "rptOwnerZipCode",
        "schemaVersion",
        "securityTitle",
        "sharesOwnedFollowingTransaction",
        "signatureDate",
        "signatureName",
        "transactionAcquiredDisposedCode",
        "transactionCode",
        "transactionDate",
        "transactionFormType",
        "transactionPricePerShare",
        "transactionShares",
        "transactionTimeliness",
        "transactionTotalValue",
        "underlyingSecurityShares",
        "underlyingSecurityTitle",
        "clarificationOfResponse", "isBusinessCombinationTransaction",
        "cik", "moreThanOneYear", "previousName", "edgarPreviousNameList",
        "entityName", "entityType", "entityTypeOtherDesc", "federalExemptionsExclusions",
        "industryGroupType", "investmentFundType", "investmentFundInfo",
        "hasNonAccreditedInvestors", "numberNonAccreditedInvestors",
        "totalNumberAlreadyInvested", "city", "stateOrCountry", "stateOrCountryDescription",
        "street1", "street2", "zipCode", "issuerPhoneNumber", "issuerPreviousNameList",
        "jurisdictionOfInc", "overFiveYears", "yearOfInc", "withinFiveYears",
        "yetToBeFormed", "aggregateNetAssetValueRange", "revenueRange",
        "minimumInvestmentAccepted", "totalAmountSold", "totalOfferingAmount",
        "totalRemaining", "firstName", "lastName", "middleName", "relationship",
        "relationshipClarification", "dollarAmount", "isEstimate", "associatedBDCRDNumber",
        "associatedBDName", "foreignSolicitation", "recipientCRDNumber",
        "recipientName", "description", "state", "statesOfSolicitationList",
        "authorizedRepresentative", "nameOfSigner", "signatureTitle",
        "submissionType", "testOrLive", "dateOfFirstSale", "yetToOccur",
        "isAmendment", "descriptionOfOtherType", "isDebtType", "isEquityType",
        "isMineralPropertyType", "isOptionToAcquireType", "isOtherType",
        "isPooledInvestmentFundType", "isSecurityToBeAcquiredType", "isTenantInCommonType",
        'notSubjectToSection16', 'rptOwnerStreet1', 'rptOwnerStreet2',

        "liveTestFlag", "confirmingCopyFlag", "returnCopyFlag", "overrideInternetFlag",
        "ccc", "reportCalendarOrQuarter", "filingManagername", "filingManageraddressstreet1",
        "filingManageraddressstreet2", "filingManageraddresscity", "filingManageraddressstateOrCountry",
        'filingManagerstateOrCountryDescription',
        "filingManageraddresszipCode", "reportType", "form13FFileNumber",
        "provideInfoForInstruction5", "name", "title", "phone", "signature",
        "otherIncludedManagersCount", "tableEntryTotal", "tableValueTotal",
        "isConfidentialOmitted",
        "nameOfIssuer", "titleOfClass", "cusip", "value", "investmentDiscretion",
        "otherManager", "putCall", "sshPrnamt", "sshPrnamtType", "Sole",
        "Shared", "None",

        "offeringFileNumber", "sinceLastFiling", "jurisdictionOrganization",
        "yearIncorporation", "sicCode", "irsNum", "fullTimeEmployees",
        "partTimeEmployees", "phoneNumber", "connectionName", "industryGroup",
        "cashEquivalents", "investmentSecurities", "accountsReceivable",
        "propertyPlantEquipment", "totalAssets", "accountsPayable", "longTermDebt",
        "totalLiabilities", "totalStockholderEquity", "totalLiabilitiesAndEquity",
        "totalRevenues", "costAndExpensesApplToRevenues", "depreciationAndAmortization",
        "netIncome", "earningsPerShareBasic", "earningsPerShareDiluted",
        "nameAuditor", "commonEquityClassName", "outstandingCommonEquity",
        "commonCusipEquity", "publiclyTradedCommonEquity", "preferredEquityClassName",
        "outstandingPreferredEquity", "preferredCusipEquity", "publiclyTradedPreferredEquity",
        "debtSecuritiesClassName", "outstandingDebtSecurities", "cusipDebtSecurities",
        "publiclyTradedDebtSecurities", "certifyIfTrue", "certifyIfNotDisqualified",
        "summaryInfo", "financialStatementAuditStatus", "securitiesOfferedTypes",
        "offerDelayedContinuousFlag", "offeringYearFlag", "offeringAfterQualifFlag",
        "offeringBestEffortsFlag", "solicitationProposedOfferingFlag",
        "resaleSecuritiesAffiliatesFlag", "securitiesOffered", "outstandingSecurities",
        "pricePerSecurity", "issuerAggregateOffering", "securityHolderAggegate",
        "qualificationOfferingAggregate", "concurrentOfferingAggregate",
        "totalAggregateOffering", "underwritersServiceProviderName",
        "underwritersFees", "auditorServiceProviderName", "auditorFees",
        "legalServiceProviderName", "legalFees", "promotersServiceProviderName",
        "promotersFees", "brokerDealerCrdNumber", "estimatedNetAmount",
        "clarificationResponses", "jurisdictionsOfSecOfferedSame", "issueJuridicationSecuritiesOffering",
        "dealersJuridicationSecuritiesOffering", "securitiesIssuerName",
        "securitiesIssuerTitle", "securitiesIssuedTotalAmount", "securitiesPrincipalHolderAmount",
        "securitiesIssuedAggregateAmount", "securitiesActExcemption",
        "certifyIfBadActor", "salesCommissionsServiceProviderName",
        "salesCommissionsServiceProviderFees", "jurisdictionsOfSecOfferedNone",
        "ifUnregsiteredNone", "blueSkyServiceProviderName", "blueSkyFees",
        'indicateTier1Tier2Offering', 'X.1.A.A.', 'X.1.A.A.', 'aggregateConsiderationBasis',
        'findersFeesServiceProviderName' , 'finderFeesFee',
        'loans', 'propertyAndEquipment', 'deposits', 'totalInterestIncome',
        'totalInterestExpenses', 'securitiesOfferedOtherDesc', 'comment',
        "assetTypeNumber",
        "assetNumber",
        "assetGroupNumber",
        "reportPeriodBeginningDate",
        "reportPeriodEndDate",
        "issuerName",
        "originalIssuanceDate",
        "originalSecurityAmount",
        "originalSecurityTermNumber",
        "securityMaturityDate",
        "originalAmortizationTermNumber",
        "originalInterestRatePercentage",
        "accrualTypeCode",
        "interestRateTypeCode",
        "originalInterestOnlyTermNumber",
        "firstPaymentDate",
        "underwritingIndicator",
        "securityTitleName",
        "denominationNumber",
        "currencyName",
        "trusteeName",
        "secFileNumber",
        "cik",
        "callableIndicator",
        "paymentFrequencyCode",
        "zeroCouponIndicator",
        "assetAddedIndicator",
        "assetModifiedIndicator",
        "reportPeriodBeginningAssetBalanceAmount",
        "reportPeriodBeginningScheduledAssetBalanceAmount",
        "reportPeriodScheduledPaymentAmount",
        "reportPeriodInterestRatePercentage",
        "totalActualPaidAmount",
        "actualInterestCollectionPercentage",
        "actualPrincipalCollectedAmount",
        "actualOtherCollectionAmount",
        "otherPrincipalAdjustmentAmount",
        "otherInterestAdjustmentAmount",
        "scheduledInterestAmount",
        "scheduledPrincipalAmount",
        "endReportingPeriodActualBalanceAmount",
        "endReportingPeriodScheduledBalanceAmount",
        "servicingFeePercentage",
        "servicingFlatFeeAmount",
        "zeroBalanceCode",
        "zeroBalanceEffectiveDate",
        "remainingTermToMaturityNumber",
        "currentDelinquentStatusNumber",
        "paymentPastDueDaysNumber",
        "paymentPastDueNumber",
        "nextReportPeriodPaymentDueAmount",
        "nextDueDate",
        "primaryLoanServicerName",
        "mostRecentServicingTransferReceivedDate",
        "assetSubjectToDemandIndicator",
        "statusAssetSubjectToDemandCode",
        "repurchaseAmount",
        "demandResolutionDate",
        "repurchaserName",
        "repurchaseReplacementReasonCode",
        "reportPeriodBeginDate",
        "originalLoanPurposeCode",
        "originatorName",
        "originalLoanAmount",
        "originalLoanMaturityDate",
        "originalInterestRateTypeCode",
        "originalLienPositionCode",
        "mostRecentJuniorLoanBalanceAmount",
        "mostRecentJuniorLoanBalanceDate",
        "mostRecentSeniorLoanAmount",
        "mostRecentSeniorLoanAmountDate",
        "loanTypeMostSeniorLienCode",
        "mostSeniorLienHybridPeriodNumber",
        "mostSeniorLienNegativeAmortizationLimitPercentage",
        "mostSeniorLienOriginationDate",
        "prepaymentPenaltyIndicator",
        "negativeAmortizationIndicator",
        "modificationIndicator",
        "modificationNumber",
        "mortgageInsuranceRequirementIndicator",
        "balloonIndicator",
        "coveredHighCostCode",
        "servicerHazardInsuranceCode",
        "refinanceCashOutAmount",
        "totalOriginationDiscountAmount",
        "brokerIndicator",
        "channelCode",
        "nationalMortgageLicenseSystemCompanyNumber",
        "buyDownNumber",
        "loanDelinquencyAdvanceNumber",
        "originationARMIndexCode",
        "armMarginPercentage",
        "fullyIndexedRatePercentage",
        "initialFixedRatePeriodHybridARMNumber",
        "initialInterestRateDecreasePercentage",
        "initialInterestRateIncreasePercentage",
        "indexLookbackNumber",
        "subsequentInterestRateResetNumber",
        "lifetimeRateCeilingPercentage",
        "lifetimeRateFloorPercentage",
        "subsequentInterestRateDecreasePercentage",
        "subsequentInterestRateIncreasePercentage",
        "subsequentPaymentResetNumber",
        "armRoundCode",
        "armRoundPercentage",
        "optionArmIndicator",
        "paymentMethodAfterRecastCode",
        "initialMinimumPaymentAmount",
        "convertibleIndicator",
        "HELOCIndicator",
        "HELOCDrawNumber",
        "prepaymentPenaltyCalculationCode",
        "prepaymentPenaltyTypeCode",
        "prepaymentPenaltyTotalTermNumber",
        "prepaymentPenaltyHardTermNumber",
        "negativeAmortizationLimitAmount",
        "negativeAmortizationInitialRecastNumber",
        "negativeAmortizationSubsequentRecastNumber",
        "negativeAmortizationBalanceAmount",
        "initialFixedPaymentNumber",
        "initialPaymentCapPercentage",
        "subsequentPaymentCapPercentage",
        "initialMinimumPaymentResetNumber",
        "subsequentMinimumPaymentResetNumber",
        "minimumPaymentAmount",
        "geographicalLocation",
        "occupancyStatusCode",
        "mostRecentOccupancyStatusCode",
        "propertyTypeCode",
        "mostRecentPropertyValueAmount",
        "mostRecentPropertyValueTypeCode",
        "mostRecentPropertyValueDate",
        "mostRecentAVMModelCode",
        "mostRecentAVMConfidenceNumber",
        "originalCLTVPercentage",
        "originalLTVPercentage",
        "originalObligorNumber",
        "originalObligorCreditScoreNumber",
        "originalObligorCreditScoreType",
        "mostRecentObligorCreditScoreNumber",
        "mostRecentObligorCreditScoreType",
        "mostRecentObligorCreditScoreDate",
        "obligorIncomeVerificationLevelCode",
        "IRSForm4506TIndicator",
        "originatorFrontEndDTIPercentage",
        "originatorBackEndDTIPercentage",
        "obligorEmploymentVerificationCode",
        "obligorEmploymentLengthCode",
        "obligorAssetVerificationCode",
        "originalPledgedAssetsAmount",
        "qualificationMethodCode",
        "mortgageInsuranceCompanyName",
        "mortgageInsuranceCoveragePercentage",
        "poolInsuranceCompanyName",
        "poolInsuranceStopLossPercentage",
        "mortgageInsuranceCoverageTypeCode",
        "modificationIndicatorReportingPeriod",
        "nextPaymentDueDate",
        "advancingMethodCode",
        "servicingAdvanceMethodologyCode",
        "stopPrincipalInterestAdvancingDate",
        "reportingPeriodBeginningLoanBalanceAmount",
        "reportingPeriodBeginningScheduledLoanBalanceAmount",
        "nextReportingPeriodPaymentDueAmount",
        "reportingPeriodInterestRatePercentage",
        "nextInterestRatePercentage",
        "otherAssessedUncollectedServicerFeeamount",
        "otherServicingFeeRetainedByServicerAmount",
        "reportingPeriodEndActualBalanceAmount",
        "reportingPeriodEndScheduledBalanceAmount",
        "reportingPeriodScheduledPaymentAmount",
        "actualInterestCollectedAmount",
        "actualOtherCollectedAmount",
        "paidThroughDate",
        "interestPaidThroughDate",
        "paidFullAmount",
        "servicerAdvancedPrincipalAmount",
        "servicerAdvancedRepaidPrincipalAmount",
        "servicerAdvancedCumulativePrincipalAmount",
        "servicerAdvanceInterestAmount",
        "servicerAdvanceRepaidInterestAmount",
        "servicerAdvanceCumulativeInterestAmount",
        "servicerAdvanceTaxesInsuranceAmount",
        "servicerAdvanceRepaidTaxesInsuranceAmount",
        "servicerAdvanceCumulativeTaxesInsuranceAmount",
        "servicerAdvanceCorporateAmount",
        "servicerAdvanceRepaidCorporateAmount",
        "servicerAdvanceCumulativeCorporateAmount",
        "mostRecentTwelveMonthHistoryCode",
        "nextResetRatePercentage",
        "nextPaymentChangeDate",
        "nextInterestRateChangeDate",
        "nextResetPaymentAmount",
        "exercisedArmConversionOptionIndicator",
        "primaryServicerName",
        "masterServicerName",
        "specialServicerName",
        "subServicerName",
        "assetSubjectDemandIndicator",
        "assetSubjectDemandStatusCode",
        "repurchaseReplacementCode",
        "chargeOffPrincipalAmount",
        "chargeOffInterestAmount",
        "lossMitigationTypeCode",
        "mostRecentLoanModificationEventCode",
        "mostRecentLoanModificationEffectiveDate",
        "postModificationMaturityDate",
        "postModificationInterestRateTypeCode",
        "postModificationAmortizationTypeCode",
        "postModificationInterestPercentage",
        "postModificationFirstPaymentDate",
        "postModificationLoanBalanceAmount",
        "postModificationPrincipalInterestPaymentAmount",
        "totalCapAmount",
        "incomeVerificationIndicatorAtModification",
        "modificationFrontEndDebtToIncomePercentage",
        "modificationBackEndDebtToIncomePercentage",
        "totalDeferredAmount",
        "forgivenPrincipalCumulativeAmount",
        "forgivenPrincipalReportingPeriodAmount",
        "forgivenInterestCumulativeAmount",
        "forgivenInterestReportingPeriodAmount",
        "actualEndingBalanceTotalDebtAmount",
        "scheduledEndingBalanceTotalDebtAmount",
        "postModificationARMCode",
        "postModificationARMIndexCode",
        "postModificationMarginPercentage",
        "postModificationInterestResetNumber",
        "postModificationNextResetDate",
        "postModificationIndexLookbackNumber",
        "postModificationARMRoundingCode",
        "postModificationARMRoundingPercentage",
        "postModificationInitialMinimumPayment",
        "postModificationNextPaymentAdjustmentDate",
        "postModificationARMPaymentRecastFrequency",
        "postModificationLifetimeFloorPercentage",
        "postModificationLifetimeCeilingPercentage",
        "postModificationInitialInterestRateIncreasePercentage",
        "postModificationInitialInterestRateDecreasePercentage",
        "postModificationSubsequentInterestIncreasePercentage",
        "postModificationSubsequentInterestRateDecreasePercentage",
        "postModificationPaymentCapPercentage",
        "postModificationPaymentMethodAfterRecastCode",
        "postModificationARMInterestRateTeaserNumber",
        "postModificationARMPaymentTeaserNumber",
        "postModificationARMNegativeAmortizationIndicator",
        "postModificationARMNegativeAmortizationCapPercentage",
        "postModificationInterestOnlyTermNumber",
        "postModificationInterestOnlyLastPaymentDate",
        "postModificationBalloonAmount",
        "postModificationInterestRateStepIndicator",
        "postModificationStepInterestPercentage",
        "postModificationStepDate",
        "postModificationStepPrincipalInterestPaymentAmount",
        "postModificationStepNumber",
        "postModificationMaximumFutureStepAgreementPercentage",
        "postModificationMaximumStepAgreementRateDate",
        "nonInterestBearingDeferredPrincipalCumulativeAmount",
        "nonInterestBearingDeferredPrincipalReportingPeriodAmount",
        "recoveryDeferredPrincipalReportingPeriodAmount",
        "nonInterestBearingDeferredPaidFullAmount",
        "nonInterestBearingDeferredInterestFeeReportingPeriodAmount",
        "nonInterestBearingDeferredInterestFeeCumulativeAmount",
        "recoveryDeferredInterestFeeReportingPeriodAmount",
        "mostRecentForbearancePlanOrTrialModificationStartDate",
        "mostRecentForbearancePlanOrTrialModificationScheduledEndDate",
        "mostRecentTrialModificationViolatedDate",
        "mostRecentRepaymentPlanStartDate",
        "mostRecentRepaymentPlanScheduledEndDate",
        "mostRecentRepaymentPlanViolatedDate",
        "shortSaleAcceptedOfferAmount",
        "mostRecentLossMitigationExitDate",
        "mostRecentLossMitigationExitCode",
        "attorneyReferralDate",
        "foreclosureDelayReasonCode",
        "foreclosureExitDate",
        "foreclosureExitReasonCode",
        "noticeOfIntentDate",
        "mostRecentAcceptedREOOfferAmount",
        "mostRecentAcceptedREOOfferDate",
        "grossLiquidationProceedsAmount",
        "netSalesProceedsAmount",
        "reportingPeriodLossPassedToIssuingEntityAmount",
        "cumulativeTotalLossPassedToIssuingEntityAmount",
        "subsequentRecoveryAmount",
        "evictionIndicator",
        "reoExitDate",
        "reoExitReasonCode",
        "UPBLiquidationAmount",
        "servicingFeesClaimedAmount",
        "servicerAdvanceReimbursedPrincipalAmount",
        "servicerAdvanceReimbursedInterestAmount",
        "servicerAdvanceReimbursedTaxesInsuranceAmount",
        "servicerAdvanceReimbursedCorporateAmount",
        "REOManagementFeesAmount",
        "cashKeyDeedAmount",
        "performanceIncentiveFeesAmount",
        "mortgageInsuranceClaimFiledDate",
        "mortgageInsuranceClaimAmount",
        "mortgageInsuranceClaimPaidDate",
        "mortgageInsuranceClaimPaidAmount",
        "mortgageInsuranceClaimDeniedRescindedDate",
        "marketableTitleTransferDate",
        "nonPayStatusCode",
        "reportingActionCode",
        "GroupID",
        "reportingPeriodBeginningDate",
        "reportingPeriodEndDate",
        "originationDate",
        "originalTermLoanNumber",
        "maturityDate",
        "interestRateSecuritizationPercentage",
        "interestAccrualMethodCode",
        "firstLoanPaymentDueDate",
        "lienPositionSecuritizationCode",
        "loanStructureCode",
        "paymentTypeCode",
        "periodicPrincipalAndInterestPaymentSecuritizationAmount",
        "scheduledPrincipalBalanceSecuritizationAmount",
        "NumberPropertiesSecuritization",
        "NumberProperties",
        "graceDaysAllowedNumber",
        "interestOnlyIndicator",
        "prepaymentPremiumIndicator",
        "modifiedIndicator",
        "armIndexCode",
        "firstRateAdjustmentDate",
        "firstPaymentAdjustmentDate",
        "armMarginNumber",
        "lifetimeRateCapPercentage",
        "periodicRateIncreaseLimitPercentage",
        "periodicRateDecreaseLimitPercentage",
        "periodicPaymentAdjustmentMaximumAmount",
        "periodicPaymentAdjustmentMaximumPercent",
        "rateResetFrequencyCode",
        "paymentResetFrequencyCode",
        "indexLookbackDaysNumber",
        "prepaymentLockOutEndDate",
        "yieldMaintenanceEndDate",
        "prepaymentPremiumsEndDate",
        "maximumNegativeAmortizationAllowedPercentage",
        "maximumNegativeAmortizationAllowedAmount",
        "negativeAmortizationDeferredInterestCapAmount",
        "deferredInterestCumulativeAmount",
        "deferredInterestCollectedAmount",
        "property",
        "reportPeriodModificationIndicator",
        "reportPeriodBeginningScheduleLoanBalanceAmount",
        "totalScheduledPrincipalInterestDueAmount",
        "servicerTrusteeFeeRatePercentage",
        "unscheduledPrincipalCollectedAmount",
        "reportPeriodEndActualBalanceAmount",
        "reportPeriodEndScheduledLoanBalanceAmount",
        "hyperAmortizingDate",
        "servicingAdvanceMethodCode",
        "nonRecoverabilityIndicator",
        "totalPrincipalInterestAdvancedOutstandingAmount",
        "totalTaxesInsuranceAdvancesOutstandingAmount",
        "otherExpensesAdvancedOutstandingAmount",
        "paymentStatusLoanCode",
        "armIndexRatePercentage",
        "nextInterestRateChangeAdjustmentDate",
        "nextPaymentAdjustmentDate",
        "mostRecentSpecialServicerTransferDate",
        "mostRecentMasterServicerReturnDate",
        "realizedLossToTrustAmount",
        "liquidationPrepaymentCode",
        "liquidationPrepaymentDate",
        "prepaymentPremiumYieldMaintenanceReceivedAmount",
        "workoutStrategyCode",
        "lastModificationDate",
        "modificationCode",
        "postModificationPaymentAmount",
        "postModificationAmortizationPeriodAmount",
        "propertyName",
        "propertyAddress",
        "propertyCity",
        "propertyState",
        "propertyZip",
        "propertyCounty",
        "netRentableSquareFeetNumber",
        "netRentableSquareFeetSecuritizationNumber",
        "unitsBedsRoomsNumber",
        "unitsBedsRoomsSecuritizationNumber",
        "yearBuiltNumber",
        "yearLastRenovated",
        "valuationSecuritizationAmount",
        "valuationSourceSecuritizationCode",
        "valuationSecuritizationDate",
        "mostRecentValuationAmount",
        "mostRecentValuationDate",
        "mostRecentValuationSourceCode",
        "physicalOccupancySecuritizationPercentage",
        "mostRecentPhysicalOccupancyPercentage",
        "propertyStatusCode",
        "defeasanceOptionStartDate",
        "DefeasedStatusCode",
        "largestTenant",
        "squareFeetLargestTenantNumber",
        "leaseExpirationLargestTenantDate",
        "secondLargestTenant",
        "squareFeetSecondLargestTenantNumber",
        "leaseExpirationSecondLargestTenantDate",
        "thirdLargestTenant",
        "squareFeetThirdLargestTenantNumber",
        "leaseExpirationThirdLargestTenantDate",
        "financialsSecuritizationDate",
        "mostRecentFinancialsStartDate",
        "mostRecentFinancialsEndDate",
        "revenueSecuritizationAmount",
        "mostRecentRevenueAmount",
        "operatingExpensesSecuritizationAmount",
        "operatingExpensesAmount",
        "netOperatingIncomeSecuritizationAmount",
        "mostRecentNetOperatingIncomeAmount",
        "netCashFlowFlowSecuritizationAmount",
        "mostRecentNetCashFlowAmount",
        "netOperatingIncomeNetCashFlowSecuritizationCode",
        "netOperatingIncomeNetCashFlowCode",
        "mostRecentDebtServiceAmount",
        "debtServiceCoverageNetOperatingIncomeSecuritizationPercentage",
        "mostRecentDebtServiceCoverageNetOperatingIncomePercentage",
        "debtServiceCoverageNetCashFlowSecuritizationPercentage",
        "mostRecentDebtServiceCoverageNetCashFlowpercentage",
        "debtServiceCoverageSecuritizationCode",
        "mostRecentDebtServiceCoverageCode",
        "mostRecentAnnualLeaseRolloverReviewDate",
        "reportingPeriodEndingDate",
        "originalLoanTerm",
        "loanMaturityDate",
        "interestCalculationTypeCode",
        "originalFirstPaymentDate",
        "gracePeriodNumber",
        "subvented",
        "vehicleManufacturerName",
        "vehicleModelName",
        "vehicleNewUsedCode",
        "vehicleModelYear",
        "vehicleTypeCode",
        "vehicleValueAmount",
        "vehicleValueSourceCode",
        "obligorCreditScoreType",
        "obligorCreditScore",
        "coObligorIndicator",
        "paymentToIncomePercentage",
        "obligorGeographicLocation",
        "reportingPeriodModificationIndicator",
        "nextReportingPeriodPaymentAmountDue",
        "otherServicerFeeRetainedByServicer",
        "otherAssessedUncollectedServicerFeeAmount",
        "reportingPeriodActualEndBalanceAmount",
        "totalActualAmountPaid",
        "servicerAdvancedAmount",
        "currentDelinquencyStatus",
        "chargedoffPrincipalAmount",
        "recoveredAmount",
        "modificationTypeCode",
        "paymentExtendedNumber",
        "repossessedIndicator",
        "repossessedProceedsAmount",
        "reportingPeriodBeginDate",
        "acquisitionCost",
        "originalLeaseTermNumber",
        "scheduledTerminationDate",
        "gracePeriod",
        "baseResidualValue",
        "baseResidualSourceCode",
        "contractResidualValue",
        "lesseeCreditScoreType",
        "lesseeCreditScore",
        "lesseeIncomeVerificationLevelCode",
        "lesseeEmploymentVerificationCode",
        "coLesseePresentIndicator",
        "lesseeGeographicLocation",
        "remainingTermNumber",
        "reportingPeriodSecuritizationValueAmount",
        "securitizationDiscountRate",
        "otherLeaseLevelServicingFeesRetainedAmount",
        "reportingPeriodEndingActualBalanceAmount",
        "reportingPeriodEndActualSecuritizationAmount",
        "primaryLeaseServicerName",
        "DemandResolutionDate",
        "repurchaseOrReplacementReasonCode",
        "chargedOffAmount",
        "leaseExtended",
        "terminationIndicator",
        "excessFeeAmount",
        "liquidationProceedsAmount",
        "commentNumber", "commentColumn", "commentDescription",
        'previousAccessionNumber', 'itemNumber', 'fieldName', 'notes', 'sequenceNumber',
        "amendmentNo",
        "amendmentType",
        "confDeniedExpired",
        'additionalInformation',
        'fileNumber'
      ),
      nameActual = c(
        "priceExerciseConversion",
        "dateDeemedExecution",
        "codeOwnershipDirectIndirect",
        "idDocument",
        "isEquitySwapInvolved",
        "dateExercised",
        "dateExpiration",
        "descriptionFootnote",
        "isDirector",
        "isOfficer",
        "isOther",
        "idCIKIssuer",
        "nameIssuer",
        "idTickerIssuer",
        "isTenPercentOwner",
        "descriptionNatureOfOwnership",
        "isNoSecuritiesOwned",
        "isNotSubjectToSection16",
        "titleOfficer",
        "descriptionOtherText",
        "dateReport",
        "countSharesOwnedPostTransaction",
        "descriptionRemarks",
        "idCIKOwner",
        "cityOwenr",
        "nameOwner",
        "stateOwner",
        "descriptionStateOwner",
        "addressStreet1Owner",
        "addressStreet2Owner",
        "zipcodeOwner",
        "idSchema",
        "titleSecurity",
        "countSharesOwnedPostTransaction",
        "dateSignature",
        "nameSignature",
        "codeTransactionAcquiredDisposed",
        "codeTransaction",
        "dateTransaction",
        "idFormTransaction",
        "pricePerShareTransaction",
        "countSharesTransaction",
        "idCodeTimelinessTransaction",
        "amountTransaction",
        "countSharesUnderlying",
        "titleSecurityUnderlying",
        "descriptionResponse", "isBusinessCombinationTransaction",
        "idCIK", "isMoreThanOneYear", "nameEntityPrevius", "listNameEntityPreviousEDGAR",
        "nameEntity", "typeEntity", "descriptionEntityTypeOther", "idFederalExemptionsExclusions",
        "typeIndustryGroup", "typeInvestmentFund", "descriptionInvestmentFund",
        "hasNonAccreditedInvestors", "countInvestorsNonAccredited",
        "countInvestorsActive", "cityEntity", "stateEntity", "descriptionStateEntity",
        "addressStreet1Entity", "addressStreet2Entity", "zipcodeEntity", "phoneNumberEntity", "listIssuerPreviousName",
        "jurisdictionOfInc", "isOverFiveYearsOld", "hasYearOfInc", "isFormedWithinFiveYears",
        "isYetToBeFormed", "rangeAgregateNetAssetValue", "rangeRevenue",
        "amountInvestmentMinimum", "amountSoldTotal", "amountOfferingTotal",
        "amountRemaining", "nameFirst", "nameLast", "nameMiddle", "relationshipEntity",
        "descriptionRelationship", "amountDollars", "isEstimate", "idCRDBroker",
        "nameBroker", "isForeignSolicitation", "idCRDRecipient",
        "nameRecipient", "stateDescription", "state", "listStatesSolicitation",
        "isAuthorizedRepresentative", "nameSignatory", "titleSignatory",
        "idForm", "codeTestOrLive", "dateFirstSale", "isYetToOccur",
        "isAmendment", "descriptionOtherType", "isDebtType", "isEquityType",
        "isMineralPropertyType", "isOptionToAcquireType", "isOtherType",
        "isPooledInvestmentFundType", "isSecurityToBeAcquiredType", "isTenantInCommonType",
        'isNotSubjectToSection16', 'addressStreet1Owner', 'addressStreet2Owner',
        "isLiveTestFlag", "isConfirmingCopyFlag", "isReturnCopyFlag", "isOverrideInternetFlag",
        "idCCC", "dateReportCalendarOrQuarter", "nameFilingManager", "addressStreet1FilingManager",
        "addressStreet2FilingManager", "cityFilingManager", "stateFilingManager",
        'descriptionStateFilingManager',
        "zipcodeFilingManager", "typeReport", "idSEC",
        "codeProvideInfoForInstruction5", "nameEntity", "titleEntity", "phoneEntity", "signatureEntity",
        "countOtherIncludedManagers", "countTableEntries", "amountValueHoldings",
        "isConfidentialOmitted", "nameIssuer", "classSecurities", "idCUSIP", "valueSecurities", "typeInvestmentDiscretion",
        "descriptionOtherManager", "codePutCall", "countSharesPrincipal", "codeSharesPrincipal", "countSharesVotingSole",
        "countSharesVotingShared", "countSharesVotingNone",

        "idSEC", "isSinceLastFiling", "codeJurisdictionOrganization",
        "yearIncorporation", "idSIC", "idIRS", "countEmployeesFullTime",
        "countEmployeesPartTime", "phoneEntity", "nameConnection", "nameIndustry",
        "amountCashEquivalents", "amountInvestmentSecurities", "amountAccountsReceivable",
        "amountPropertyPlantEquipment", "amountAssetsTotal", "amountAccountsPayable", "amountLongTermDebt",
        "amountLiabilitiesTotal", "amountStockholderEquityTotal", "amountLiabilitiesAndEquityTotal",
        "amountRevenuesTotal", "amountCostAndExpensesOfRevenue", "amountDepreciationAndAmortization",
        "amountNetIncome", "pershareEarningsBasic", "pershareEarningsDiluted",
        "nameAuditor", "nameCommonEquityClass", "amountCommonEquityOutstanding",
        "idCUSIPCommonEquity", "isCommonEquityPublic", "namePreferredEquityClass",
        "amountPreferredEquityOutstanding", "idCusipPreferrdEquity", "isdPreferredEquityPublic",
        "nameDebtSecuritiesClass", "amountOutstandingDebtSecurities", "idCUSIPDebtSecurities",
        "isDebtSecuritiesPublic", "isCertifyIfTrue", "isCertifyIfNotDisqualified",
        "codeTier1Tier2Offering", "codeFinancialStatementAuditStatus", "codeSecuritiesOfferedTypes",
        "codeOfferDelayedContinuous", "codeOfferingYearFlag", "codeOfferingAfterQualifFlag",
        "codeOfferingBestEffortsFlag", "codeSolicitationProposedOfferingFlag",
        "codeResaleSecuritiesAffiliates", "countSecuritiesOffered", "countSecuritiesOutstanding",
        "persharePrice", "amountOfferingIssuer", "amountOfferingExistingShareholdersSelling",
        "amountOfferingSold12MonthQualifiedOffering", "amountOfferingSoldConcurrent",
        "amountOfferingTotal", "nameUnderwritr",
        "amountUnderwritersFees", "nameAuditor", "amountAuditorFees",
        "nameLegal", "amountLegalFees", "namePromoter",
        "amountPromotersFees", "idCRDBroker", "amountOfferringProceedsNet",
        "descriptionResponse", "isJurisdictionsOfSecOfferedSame", "locatonJuridicationSecuritiesOffering",
        "locationDealersJuridicationSecuritiesOffering", "nameSecuritiesIssuer",
        "titleSecuritiesOffered", "amountSecuritiesIssued", "amountSecuritiesPrincipalHolder",
        "amountSecuritiesIssuedTotal", "nameSecuritiesActExemption",
        "isBadActor", "nameSalesCommissionsServiceProvider",
        "amountSalesCommissionsFees", "isJurisdictionsSecuritiesOfferingNone",
        "isUnRegisteredNone",
        "nameBlueSkyServiceProvider", "amountBlueSkyFees",
        'isTier1Tier2Offering', 'idForm', 'idForm', 'amountOfferingConsiderationBasis',
        'nameFindersFeeProvider' , 'amountFindersFee',
        'amountLoans', 'amountPropertyAndEquipment', 'amountDeposits', 'amountInterestIncomeTotal',
        'amountInterestExpenseTotal', 'descriptionOtherSecuritiesOffered',
        'commentFiling',
        "numberAssetType",
        "numberAsset",
        "numberAssetGroup",
        "dateReportPeriodBeginning",
        "dateReportPeriodEnd",
        "nameIssuer",
        "dateOriginalIssuance",
        "amountOriginalSecurity",
        "numberOriginalSecurityTerm",
        "dateSecurityMaturity",
        "numberOriginalAmortizationTerm",
        "percentageOriginalInterestRate",
        "codeAccrualType",
        "codeInterestRateType",
        "numberOriginalInterestOnlyTerm",
        "dateFirstPayment",
        "hasUnderwriting",
        "nameSecurityTitle",
        "numberDenomination",
        "nameCurrency",
        "nameTrustee",
        "numberSecFile",
        "idCIK",
        "hasCallable",
        "codePaymentFrequency",
        "hasZeroCoupon",
        "hasAssetAdded",
        "hasAssetModified",
        "amountReportPeriodBeginningAssetBalance",
        "amountReportPeriodBeginningScheduledAssetBalance",
        "amountReportPeriodScheduledPayment",
        "percentageReportPeriodInterestRate",
        "amountTotalActualPaid",
        "percentageActualInterestCollection",
        "amountActualPrincipalCollected",
        "amountActualOtherCollection",
        "amountOtherPrincipalAdjustment",
        "amountOtherInterestAdjustment",
        "amountScheduledInterest",
        "amountScheduledPrincipal",
        "amountEndReportingPeriodActualBalance",
        "amountEndReportingPeriodScheduledBalance",
        "percentageServicingFee",
        "amountServicingFlatFee",
        "codeZeroBalance",
        "dateZeroBalanceEffective",
        "numberRemainingTermToMaturity",
        "numberCurrentDelinquentStatus",
        "numberPaymentPastDueDays",
        "numberPaymentPastDue",
        "amountNextReportPeriodPaymentDue",
        "dateNextDue",
        "namePrimaryLoanServicer",
        "dateMostRecentServicingTransferReceived",
        "hasAssetSubjectToDemand",
        "codeStatusAssetSubjectToDemand",
        "amountRepurchase",
        "dateDemandResolution",
        "nameRepurchaser",
        "codeRepurchaseReplacementReason",
        "dateReportPeriodBegin",
        "codeOriginalLoanPurpose",
        "nameOriginator",
        "amountOriginalLoan",
        "dateOriginalLoanMaturity",
        "codeOriginalInterestRateType",
        "codeOriginalLienPosition",
        "amountMostRecentJuniorLoanBalance",
        "dateMostRecentJuniorLoanBalance",
        "amountMostRecentSeniorLoan",
        "dateMostRecentSeniorLoanAmount",
        "codeLoanTypeMostSeniorLien",
        "numberMostSeniorLienHybridPeriod",
        "percentageMostSeniorLienNegativeAmortizationLimit",
        "dateMostSeniorLienOrigination",
        "hasPrepaymentPenalty",
        "hasNegativeAmortization",
        "hasModification",
        "numberModification",
        "hasMortgageInsuranceRequirement",
        "hasBalloon",
        "codeCoveredHighCost",
        "codeServicerHazardInsurance",
        "amountRefinanceCashOut",
        "amountTotalOriginationDiscount",
        "hasBroker",
        "codeChannel",
        "numberNationalMortgageLicenseSystemCompany",
        "numberBuyDown",
        "numberLoanDelinquencyAdvance",
        "codeOriginationARMIndex",
        "percentageArmMargin",
        "percentageFullyIndexedRate",
        "numberInitialFixedRatePeriodHybridARM",
        "percentageInitialInterestRateDecrease",
        "percentageInitialInterestRateIncrease",
        "numberIndexLookback",
        "numberSubsequentInterestRateReset",
        "percentageLifetimeRateCeiling",
        "percentageLifetimeRateFloor",
        "percentageSubsequentInterestRateDecrease",
        "percentageSubsequentInterestRateIncrease",
        "numberSubsequentPaymentReset",
        "codeArmRound",
        "percentageArmRound",
        "hasOptionArm",
        "codePaymentMethodAfterRecast",
        "amountInitialMinimumPayment",
        "hasConvertible",
        "hasHELOC",
        "numberHELOCDraw",
        "codePrepaymentPenaltyCalculation",
        "codePrepaymentPenaltyType",
        "numberPrepaymentPenaltyTotalTerm",
        "numberPrepaymentPenaltyHardTerm",
        "amountNegativeAmortizationLimit",
        "numberNegativeAmortizationInitialRecast",
        "numberNegativeAmortizationSubsequentRecast",
        "amountNegativeAmortizationBalance",
        "numberInitialFixedPayment",
        "percentageInitialPaymentCap",
        "percentageSubsequentPaymentCap",
        "numberInitialMinimumPaymentReset",
        "numberSubsequentMinimumPaymentReset",
        "amountMinimumPayment",
        "locationGeographical",
        "codeOccupancyStatus",
        "codeMostRecentOccupancyStatus",
        "codePropertyType",
        "amountMostRecentPropertyValue",
        "codeMostRecentPropertyValueType",
        "dateMostRecentPropertyValue",
        "codeMostRecentAVMModel",
        "numberMostRecentAVMConfidence",
        "percentageOriginalCLTV",
        "percentageOriginalLTV",
        "numberOriginalObligor",
        "numberOriginalObligorCreditScore",
        "typeOriginalObligorCreditScore",
        "numberMostRecentObligorCreditScore",
        "typeMostRecentObligorCreditScore",
        "dateMostRecentObligorCreditScore",
        "codeObligorIncomeVerificationLevel",
        "hasIRSForm4506T",
        "percentageOriginatorFrontEndDTI",
        "percentageOriginatorBackEndDTI",
        "codeObligorEmploymentVerification",
        "codeObligorEmploymentLength",
        "codeObligorAssetVerification",
        "amountOriginalPledgedAssets",
        "codeQualificationMethod",
        "nameMortgageInsuranceCompany",
        "percentageMortgageInsuranceCoverage",
        "namePoolInsuranceCompany",
        "percentagePoolInsuranceStopLoss",
        "codeMortgageInsuranceCoverageType",
        "periodModificationHasReporting",
        "dateNextPaymentDue",
        "codeAdvancingMethod",
        "codeServicingAdvanceMethodology",
        "dateStopPrincipalInterestAdvancing",
        "amountReportingPeriodBeginningLoanBalance",
        "amountReportingPeriodBeginningScheduledLoanBalance",
        "amountNextReportingPeriodPaymentDue",
        "percentageReportingPeriodInterestRate",
        "percentageNextInterestRate",
        "feeamountOtherAssessedUncollectedServicer",
        "amountOtherServicingFeeRetainedByServicer",
        "amountReportingPeriodEndActualBalance",
        "amountReportingPeriodEndScheduledBalance",
        "amountReportingPeriodScheduledPayment",
        "amountActualInterestCollected",
        "amountActualOtherCollected",
        "datePaidThrough",
        "dateInterestPaidThrough",
        "amountPaidFull",
        "amountServicerAdvancedPrincipal",
        "amountServicerAdvancedRepaidPrincipal",
        "amountServicerAdvancedCumulativePrincipal",
        "amountServicerAdvanceInterest",
        "amountServicerAdvanceRepaidInterest",
        "amountServicerAdvanceCumulativeInterest",
        "amountServicerAdvanceTaxesInsurance",
        "amountServicerAdvanceRepaidTaxesInsurance",
        "amountServicerAdvanceCumulativeTaxesInsurance",
        "amountServicerAdvanceCorporate",
        "amountServicerAdvanceRepaidCorporate",
        "amountServicerAdvanceCumulativeCorporate",
        "codeMostRecentTwelveMonthHistory",
        "percentageNextResetRate",
        "dateNextPaymentChange",
        "dateNextInterestRateChange",
        "amountNextResetPayment",
        "hasExercisedArmConversionOption",
        "namePrimaryServicer",
        "nameMasterServicer",
        "nameSpecialServicer",
        "nameSubServicer",
        "hasAssetSubjectDemand",
        "codeAssetSubjectDemandStatus",
        "codeRepurchaseReplacement",
        "amountChargeOffPrincipal",
        "amountChargeOffInterest",
        "codeLossMitigationType",
        "codeMostRecentLoanModificationEvent",
        "dateMostRecentLoanModificationEffective",
        "datePostModificationMaturity",
        "codePostModificationInterestRateType",
        "codePostModificationAmortizationType",
        "percentagePostModificationInterest",
        "datePostModificationFirstPayment",
        "amountPostModificationLoanBalance",
        "amountPostModificationPrincipalInterestPayment",
        "amountTotalCap",
        "modificationIncomeVerificationHasAt",
        "percentageModificationFrontEndDebtToIncome",
        "percentageModificationBackEndDebtToIncome",
        "amountTotalDeferred",
        "amountForgivenPrincipalCumulative",
        "amountForgivenPrincipalReportingPeriod",
        "amountForgivenInterestCumulative",
        "amountForgivenInterestReportingPeriod",
        "amountActualEndingBalanceTotalDebt",
        "amountScheduledEndingBalanceTotalDebt",
        "codePostModificationARM",
        "codePostModificationARMIndex",
        "percentagePostModificationMargin",
        "numberPostModificationInterestReset",
        "datePostModificationNextReset",
        "numberPostModificationIndexLookback",
        "codePostModificationARMRounding",
        "percentagePostModificationARMRounding",
        "paymentPostModificationInitialMinimum",
        "datePostModificationNextPaymentAdjustment",
        "frequencyPostModificationARMPaymentRecast",
        "percentagePostModificationLifetimeFloor",
        "percentagePostModificationLifetimeCeiling",
        "percentagePostModificationInitialInterestRateIncrease",
        "percentagePostModificationInitialInterestRateDecrease",
        "percentagePostModificationSubsequentInterestIncrease",
        "percentagePostModificationSubsequentInterestRateDecrease",
        "percentagePostModificationPaymentCap",
        "codePostModificationPaymentMethodAfterRecast",
        "numberPostModificationARMInterestRateTeaser",
        "numberPostModificationARMPaymentTeaser",
        "hasPostModificationARMNegativeAmortization",
        "percentagePostModificationARMNegativeAmortizationCap",
        "numberPostModificationInterestOnlyTerm",
        "datePostModificationInterestOnlyLastPayment",
        "amountPostModificationBalloon",
        "hasPostModificationInterestRateStep",
        "percentagePostModificationStepInterest",
        "datePostModificationStep",
        "amountPostModificationStepPrincipalInterestPayment",
        "numberPostModificationStep",
        "percentagePostModificationMaximumFutureStepAgreement",
        "datePostModificationMaximumStepAgreementRate",
        "amountNonInterestBearingDeferredPrincipalCumulative",
        "amountNonInterestBearingDeferredPrincipalReportingPeriod",
        "amountRecoveryDeferredPrincipalReportingPeriod",
        "amountNonInterestBearingDeferredPaidFull",
        "amountNonInterestBearingDeferredInterestFeeReportingPeriod",
        "amountNonInterestBearingDeferredInterestFeeCumulative",
        "amountRecoveryDeferredInterestFeeReportingPeriod",
        "dateMostRecentForbearancePlanOrTrialModificationStart",
        "dateMostRecentForbearancePlanOrTrialModificationScheduledEnd",
        "dateMostRecentTrialModificationViolated",
        "dateMostRecentRepaymentPlanStart",
        "dateMostRecentRepaymentPlanScheduledEnd",
        "dateMostRecentRepaymentPlanViolated",
        "amountShortSaleAcceptedOffer",
        "dateMostRecentLossMitigationExit",
        "codeMostRecentLossMitigationExit",
        "dateAttorneyReferral",
        "codeForeclosureDelayReason",
        "dateForeclosureExit",
        "codeForeclosureExitReason",
        "dateNoticeOfIntent",
        "amountMostRecentAcceptedREOOffer",
        "dateMostRecentAcceptedREOOffer",
        "amountGrossLiquidationProceeds",
        "amountNetSalesProceeds",
        "amountReportingPeriodLossPassedToIssuingEntity",
        "amountCumulativeTotalLossPassedToIssuingEntity",
        "amountSubsequentRecovery",
        "hasEviction",
        "dateReoExit",
        "codeReoExitReason",
        "amountUPBLiquidation",
        "amountServicingFeesClaimed",
        "amountServicerAdvanceReimbursedPrincipal",
        "amountServicerAdvanceReimbursedInterest",
        "amountServicerAdvanceReimbursedTaxesInsurance",
        "amountServicerAdvanceReimbursedCorporate",
        "amountREOManagementFees",
        "amountCashKeyDeed",
        "amountPerformanceIncentiveFees",
        "dateMortgageInsuranceClaimFiled",
        "amountMortgageInsuranceClaim",
        "dateMortgageInsuranceClaimPaid",
        "amountMortgageInsuranceClaimPaid",
        "dateMortgageInsuranceClaimDeniedRescinded",
        "dateMarketableTitleTransfer",
        "codeNonPayStatus",
        "codeReportingAction",
        "idGroup",
        "dateReportingPeriodBeginning",
        "dateReportingPeriodEnd",
        "dateOrigination",
        "numberOriginalTermLoan",
        "dateMaturity",
        "percentageInterestRateSecuritization",
        "codeInterestAccrualMethod",
        "dateFirstLoanPaymentDue",
        "codeLienPositionSecuritization",
        "codeLoanStructure",
        "codePaymentType",
        "amountPeriodicPrincipalAndInterestPaymentSecuritization",
        "amountScheduledPrincipalBalanceSecuritization",
        "securitizationNumberProperties",
        "propertiesNumber",
        "numberGraceDaysAllowed",
        "hasInterestOnly",
        "hasPrepaymentPremium",
        "hasModified",
        "codeArmIndex",
        "dateFirstRateAdjustment",
        "dateFirstPaymentAdjustment",
        "numberArmMargin",
        "percentageLifetimeRateCap",
        "percentagePeriodicRateIncreaseLimit",
        "percentagePeriodicRateDecreaseLimit",
        "amountPeriodicPaymentAdjustmentMaximum",
        "percentPeriodicPaymentAdjustmentMaximum",
        "codeRateResetFrequency",
        "codePaymentResetFrequency",
        "numberIndexLookbackDays",
        "datePrepaymentLockOutEnd",
        "dateYieldMaintenanceEnd",
        "datePrepaymentPremiumsEnd",
        "percentageMaximumNegativeAmortizationAllowed",
        "amountMaximumNegativeAmortizationAllowed",
        "amountNegativeAmortizationDeferredInterestCap",
        "amountDeferredInterestCumulative",
        "amountDeferredInterestCollected",
        "propertyProperty",
        "hasReportPeriodModification",
        "amountReportPeriodBeginningScheduleLoanBalance",
        "amountTotalScheduledPrincipalInterestDue",
        "percentageServicerTrusteeFeeRate",
        "amountUnscheduledPrincipalCollected",
        "amountReportPeriodEndActualBalance",
        "amountReportPeriodEndScheduledLoanBalance",
        "dateHyperAmortizing",
        "codeServicingAdvanceMethod",
        "hasNonRecoverability",
        "amountTotalPrincipalInterestAdvancedOutstanding",
        "amountTotalTaxesInsuranceAdvancesOutstanding",
        "amountOtherExpensesAdvancedOutstanding",
        "codePaymentStatusLoan",
        "percentageArmIndexRate",
        "dateNextInterestRateChangeAdjustment",
        "dateNextPaymentAdjustment",
        "dateMostRecentSpecialServicerTransfer",
        "dateMostRecentMasterServicerReturn",
        "amountRealizedLossToTrust",
        "codeLiquidationPrepayment",
        "dateLiquidationPrepayment",
        "amountPrepaymentPremiumYieldMaintenanceReceived",
        "codeWorkoutStrategy",
        "dateLastModification",
        "codeModification",
        "amountPostModificationPayment",
        "amountPostModificationAmortizationPeriod",
        "nameProperty",
        "addressProperty",
        "cityProperty",
        "stateProperty",
        "zipcodeProperty",
        "countyProperty",
        "numberNetRentableSquareFeet",
        "numberNetRentableSquareFeetSecuritization",
        "numberUnitsBedsRooms",
        "numberUnitsBedsRoomsSecuritization",
        "yearBuilt",
        "yearLastRenovated",
        "amountValuationSecuritization",
        "codeValuationSourceSecuritization",
        "dateValuationSecuritization",
        "amountMostRecentValuation",
        "dateMostRecentValuation",
        "codeMostRecentValuationSource",
        "percentagePhysicalOccupancySecuritization",
        "percentageMostRecentPhysicalOccupancy",
        "codePropertyStatus",
        "dateDefeasanceOptionStart",
        "codeDefeasedStatus",
        "tenantLargest",
        "numberSquareFeetLargestTenant",
        "dateLeaseExpirationLargestTenant",
        "tenantSecondLargest",
        "numberSquareFeetSecondLargestTenant",
        "dateLeaseExpirationSecondLargestTenant",
        "tenantThirdLargest",
        "numberSquareFeetThirdLargestTenant",
        "dateLeaseExpirationThirdLargestTenant",
        "dateFinancialsSecuritization",
        "dateMostRecentFinancialsStart",
        "dateMostRecentFinancialsEnd",
        "amountRevenueSecuritization",
        "amountMostRecentRevenue",
        "amountOperatingExpensesSecuritization",
        "amountOperatingExpenses",
        "amountNetOperatingIncomeSecuritization",
        "amountMostRecentNetOperatingIncome",
        "amountNetCashFlowFlowSecuritization",
        "amountMostRecentNetCashFlow",
        "codeNetOperatingIncomeNetCashFlowSecuritization",
        "codeNetOperatingIncomeNetCashFlow",
        "amountMostRecentDebtService",
        "percentageDebtServiceCoverageNetOperatingIncomeSecuritization",
        "percentageMostRecentDebtServiceCoverageNetOperatingIncome",
        "percentageDebtServiceCoverageNetCashFlowSecuritization",
        "percentageMostRecentDebtServiceCoverageNetCash",
        "codeDebtServiceCoverageSecuritization",
        "codeMostRecentDebtServiceCoverage",
        "dateMostRecentAnnualLeaseRolloverReview",
        "dateReportingPeriodEnding",
        "termOriginalLoan",
        "dateLoanMaturity",
        "codeInterestCalculationType",
        "dateOriginalFirstPayment",
        "numberGracePeriod",
        "subventedSubvented",
        "nameVehicleManufacturer",
        "nameVehicleModel",
        "codeVehicleNewUsed",
        "yearVehicleModel",
        "codeVehicleType",
        "amountVehicleValue",
        "codeVehicleValueSource",
        "typeObligorCreditScore",
        "scoreObligorCredit",
        "hasCoObligor",
        "percentagePaymentToIncome",
        "locationObligorGeographic",
        "hasReportingPeriodModification",
        "amountPaymentDueNextReportingPeriod",
        "servicerOtherServicerFeeRetainedBy",
        "amountOtherAssessedUncollectedServicerFee",
        "amountReportingPeriodActualEndBalance",
        "amountPaidTotalActual",
        "amountServicerAdvanced",
        "isDelinquent",
        "amountChargedoffPrincipal",
        "amountRecovered",
        "codeModificationType",
        "numberPaymentExtended",
        "hasRepossessed",
        "amountRepossessedProceeds",
        "dateReportingPeriodBegin",
        "costAcquisition",
        "numberOriginalLeaseTerm",
        "dateScheduledTermination",
        "periodGrace",
        "valueBaseResidual",
        "codeBaseResidualSource",
        "valueContractResidual",
        "typeLesseeCreditScore",
        "scoreLesseeCredit",
        "codeLesseeIncomeVerificationLevel",
        "codeLesseeEmploymentVerification",
        "hasCoLesseePresent",
        "locationLesseeGeographic",
        "numberRemainingTerm",
        "amountReportingPeriodSecuritizationValue",
        "rateSecuritizationDiscount",
        "amountOtherLeaseLevelServicingFeesRetained",
        "amountReportingPeriodEndingActualBalance",
        "amountReportingPeriodEndActualSecuritization",
        "namePrimaryLeaseServicer",
        "dateDemandResolution",
        "codeRepurchaseOrReplacementReason",
        "amountChargedOff",
        "extendedLease",
        "hasTermination",
        "amountExcessFee",
        "amountLiquidationProceeds",
        "detailNumberComment", "columnComment", "descriptionComment",
        'idAccessionPrevious',
        'numberItem', 'nameField', 'descriptionNotes', 'idSequence',
        "numberAmendment",
        "typeAmendmentType",
        "confDeniedExpired",
        'descriptionInformationAdditional',
        'numberFile'
      )
    )}

get_filer_type_df <-
  function() {
    data_frame(
      idTypeFilerOwner = c(
        'insider',
        'private' ,
        'broker_dealer',
        'transfer_agent',
        'ia',
        'msd',
        'bank',
        'inv_co'
      ),
      typeFilerOwner = c(
        'Insider',
        'Private Placement',
        'Broker Dealer',
        'Transfer Agent',
        'Investment Advisor',
        'Bank',
        'Municipal Securities Dealer',
        'Investment Company'
      )
    ) %>%
      mutate_all(str_to_upper)
  }

#' Form-D dictionary
#'
#' This function returns searchable
#' industries for parsed SEC Form-D
#' filings
#'
#' @return a \code{data_frame}
#' @export
#' @import dplyr
#' @examples
#' get_dictionary_form_d_categories()
get_dictionary_form_d_categories <-
  function() {
    category_df <-
      dplyr::data_frame(
        idIndustry = 1:35,
        nameIndustry = c(
          "AGRICULTURE",
          "AIRLINES AND AIRPORTS",
          "BIOTECHNOLOGY",
          "BUSINESS SERVICES",
          "COAL MINING",
          "COMMERCIAL REAL ESTATE",
          "COMMERCIAL BANKING",
          "COMPUTERS",
          "CONSTRUCTION",
          "ELECTRIC UTILITIES",
          "ENERGY CONSERVATION",
          "ENVIORNMENTAL SERVICES",
          "HEALTH INSURANCE",
          "HOSPITALS AND PHYSICIANS",
          "INSURANCE",
          "INVESTING",
          "INVESTMENT BANKING",
          "LODGING AND CONVETION",
          "MANUFACTURING",
          "OIL AND GAS",
          "OTHER",
          "OTHER BANKING AND FINANCIAL SERVICES",
          "OTHER ENERGY",
          "OTHER HEALTH CARE",
          "OTHER REAL ESTATE",
          "OTHER TECHNOLOGY",
          "OTHER TRAVEL",
          "PHARMACEUTICALS",
          "POOLED INVESTMENT FUND",
          "REITS AND FINANCE",
          "RESIDENTIAL REAL ESTATE",
          "RESTAURANTS",
          "RETAIL",
          "TELECOMMUNICATIONS",
          "TRAVEL AND TOURISM"
        ),
        codeIndustryParent = c(
          "OTHER",
          "TRAVEL",
          "HEALTH",
          "OTHER",
          "ENERGY",
          "REAL",
          "FINANCE",
          "TECH",
          "REAL",
          "ENERGY",
          "ENERGY",
          "ENERGY",
          "HEALTH",
          "HEALTH",
          "FINANCE",
          "FINANCE",
          "FINANCE",
          "TRAVEL",
          "OTHER",
          "ENERGY",
          "OTHER",
          "FINANCE",
          "ENERGY",
          "HEALTH",
          "REAL",
          "TECH",
          "TRAVEL",
          "HEALTH",
          "FINANCE",
          "REAL",
          "REAL",
          "OTHER",
          "OTHER",
          "TECH",
          "TRAVEL"
        ),
        nameIndustryParent = c(
          "OTHER",
          "TRAVEL AND LEISURE",
          "HEALTHCARE",
          "OTHER",
          "ENERGY",
          "REAL ESTATE",
          "FINANCIAL",
          "TECHNOLOGY",
          "REAL ESTATE",
          "ENERGY",
          "ENERGY",
          "ENERGY",
          "HEALTHCARE",
          "HEALTHCARE",
          "FINANCIAL",
          "FINANCIAL",
          "FINANCIAL",
          "TRAVEL AND LEISURE",
          "OTHER",
          "ENERGY",
          "OTHER",
          "FINANCIAL",
          "ENERGY",
          "HEALTHCARE",
          "REAL ESTATE",
          "TECHNOLOGY",
          "TRAVEL AND LEISURE",
          "HEALTHCARE",
          "FINANCIAL",
          "REAL ESTATE",
          "REAL ESTATE",
          "OTHER",
          "OTHER",
          "TECHNOLOGY",
          "TRAVEL AND LEISURE"
        )
      )
    return(category_df)
  }

get_insider_code_df <-
  function() {
    insider_df <-
      data_frame(
        idInsiderTransaction =
          c(
            "A",
            "C",
            "D",
            "F",
            "G",
            "H",
            "I",
            "J",
            "K",
            "L",
            "M",
            "NONE",
            "O",
            "P",
            "S",
            "U",
            "V",
            "W",
            "X",
            "Z"
          ),
        nameInsiderTransaction = c(
          "AWARD",
          "CONVEYANCE",
          "DISPOSITION TO ISSUER",
          "PAYMENT WITH SECURITIES",
          "GIFT",
          "EXPIRATION OF LONG DERIVATIVE POSITION",
          "DISCRETIONARY TRANSACTION",
          "OTHER",
          "EQUITY SWAP OR SIMILAR",
          "SMALL ACQUISITIONS",
          "EXEMPT",
          NA,
          "OTM EXERCISE",
          "PURCHASE",
          "SALE",
          "MERGER AND ACQUISITION",
          "REPORTED EARLY",
          "WILL OR LAWS OF DESCENT",
          "ITM OR ATM EXERCISE",
          "DEPOSIT INTO/WITHDRAWAL FROM VOTING TRUST"
        ),
        idTypeInsiderTransaction = c(
          "A",
          "D",
          "D",
          "D",
          "D",
          NA,
          NA,
          NA,
          NA,
          "A",
          "A",
          NA,
          "A",
          "A",
          "D",
          NA,
          NA,
          "D",
          "A",
          "D"
        )
      )
    return(insider_df)
  }

#' SEC filing code dictionary
#'
#' This function returns a
#' dictionary of SEC form filing types
#'
#' @return a \code{data_frame}
#' @export
#' @import dplyr stringr
#' @family SEC
#' @family dictionary
#'
#' @examples
#' get_dictionary_sec_filing_codes()
get_dictionary_sec_filing_codes <-
  function() {
    data_frame(
      idFormType = c(
        "1.01",
        "1.02",
        "1.03",
        "1.04",
        "2.01",
        "2.02",
        "2.03",
        "2.04",
        "2.05",
        "2.06",
        "3.01",
        "3.02",
        "3.03",
        "4.01",
        "4.02",
        "5.01",
        "5.02",
        "5.03",
        "5.04",
        "5.05",
        "5.06",
        "5.07",
        "5.08",
        "6.01",
        "6.02",
        "6.03",
        "6.04",
        "6.05",
        "7.01",
        "8.01",
        "9.01"
      ),
      nameFormType = c(
        "Entry into a Material Definitive Agreement",
        "Termination of a Material Definitive Agreement",
        "Bankruptcy or Receivership",
        "Mine Safety Ð Reporting of Shutdowns and Patterns of Violations",
        "Completion of Acquisition or Disposition of Assets",
        "Results of Operations and Financial Condition",
        "Creation of a Direct Financial Obligation or an Obligation under an Off-Balance Sheet Arrangement of a Registrant",
        "Triggering Events That Accelerate or Increase a Direct Financial Obligation or an Obligation under an Off-Balance Sheet Arrangement",
        "Costs Associated with Exit or Disposal Activities",
        "Material Impairments",
        "Notice of Delisting or Failure to Satisfy a Continued Listing Rule or Standard; Transfer of Listing",
        "Unregistered Sales of Equity Securities",
        "Material Modification to Rights of Security Holders",
        "Changes in Registrant's Certifying Accountant",
        "Non-Reliance on Previously Issued Financial Statements or a Related Audit Report or Completed Interim Review",
        "Changes in Control of Registrant",
        "Departure of Directors or Certain Officers; Election of Directors; Appointment of Certain Officers; Compensatory Arrangements of Certain Officers",
        "Amendments to Articles of Incorporation or Bylaws; Change in Fiscal Year",
        "Temporary Suspension of Trading Under Registrant's Employee Benefit Plans",
        "Amendments to the Registrant's Code of Ethics, or Waiver of a Provision of the Code of Ethics",
        "Change in Shell Company Status",
        "Submission of Matters to a Vote of Security Holders",
        "Shareholder Director Nominations",
        "ABS Informational and Computational Material",
        "Change of Servicer or Trustee",
        "Change in Credit Enhancement or Other External Support",
        "Failure to Make a Required Distribution",
        "Securities Act Updating Disclosure",
        "Regulation FD Disclosure",
        "Other Events",
        "Financial Statements and Exhibits"
      ) %>% stringr::str_to_upper()
    )

  }

#' SEC form codes
#'
#' This function returns a
#' dictionary of SEC form codes
#'
#' @return a \code{data_frame}
#' @export
#' @family SEC
#' @family dictionary
#'
#' @examples
#' get_dictionary_sec_form_codes()
get_dictionary_sec_form_codes <-
  function() {
    data_frame(
      idForm = c(
        "R",
        "A",
        "Q",
        "CR",
        "REG",
        "REGX",
        "O",
        "P",
        "X",
        "W",
        "SEC",
        "PROXY",
        "CT",
        "IS",
        "CO",
        "T"
      ),
      nameForm = c(
        "Other Report",
        "Annual Report",
        "Quarterly Report",
        "Current Report",
        "Registration",
        "Private Offering",
        "Ownership",
        "Prospectus",
        "Exemption",
        "Withdrawal",
        "SEC Correspondence",
        "Proxy Statement",
        "Confidential Treatment",
        "Initial Statement",
        "Change in Ownership",
        "Trades"
      ) %>% stringr::str_to_upper()
    )
  }

get_company_type_df <-
  function() {
    data_frame(
      idCompanyType = c(
        "ic",
        "i",
        "ia",
        "bd",
        "m",
        "t",
        "b",
        "c",
        "p",
        "etf",
        "mmf",
        "mf",
        "uit",
        "cef"
      ),
      nameCompanyType = c(
        "Investment Company",
        "Insider",
        "Investment Adviser",
        "Broker-dealer",
        "Municipal Securities Dealer",
        "Transfer Agent",
        "Bank",
        "Company",
        "Private Issuer",
        "ETF",
        "Money Market Fund",
        "Mutual Fund",
        "UIT",
        "Closed-end Fund"
      )
    )
  }

#' SEC Rule dictionary
#'
#' This function retuns a
#' dictionary of SEC rules
#'
#' @return a \code{data_frame}
#' @export
#' @import dplyr stringr
#'
#' @examples
#' get_dictionary_sec_rules()
get_dictionary_sec_rules <-
  function() {
    data_frame(
      idRule = c(
        "06",
        "3C",
        "3C.7",
        "3C.1",
        "06b",
        "04",
        "46",
        "04.1",
        "04.2",
        "04.3",
        "05",
        "3C.6",
        "3C.5",
        "06c",
        "4a5",
        "3C.11",
        "3C.2",
        "3C.3",
        "3C.9",
        "3C.10",
        "3C.4",
        "3C.12",
        "3C.",
        "3C.14",
        "3"
      ),
      nameRule = c(
        "Rule 506",
        "Investment Company Act Section 3c",
        "Investment Company Act Section 3c",
        "Investment Company Act Section 3c",
        "Rule 506b",
        "Rule 504",
        "Rule 506c",
        "Rule 504b(1)(i)",
        "Rule 504b(1)(ii)",
        "Rule 504b(1)(iii)",
        "Rule 505",
        "Investment Company Act Section 3c",
        "Investment Company Act Section 3c",
        "Rule 506c",
        "Securities Act Section 4(a)(5)",
        "Investment Company Act Section 3c",
        "Investment Company Act Section 3c",
        "Investment Company Act Section 3c",
        "Investment Company Act Section 3c",
        "Investment Company Act Section 3c",
        "Investment Company Act Section 3c",
        "Investment Company Act Section 3c",
        "Investment Company Act Section 3c",
        "Investment Company Act Section 3c",
        "Investment Company Act Section 3c"
      )
    ) %>%
      mutate_all(str_to_upper)
  }




# Company Tickers ---------------------------------------------------------



#' SEC listed public companys
#'
#' @return
#' @export
#' @import jsonlite dplyr purrr stringr dplyr
#' @family SEC EDGAR
#' @examples
get_data_edgar_tickers <-
  function() {
    json_data <-
      "https://www.sec.gov/data/company_tickers.json" %>%
      jsonlite::fromJSON(simplifyDataFrame = TRUE)

    all_companies <-
      1:length(json_data) %>%
      map_df(function(x) {
        json_data[[x]] %>%
          flatten_df()
      }) %>%
      purrr::set_names(c(
        'idCIK',
        'nameCompany',
        'idTicker',
        'typeFiler',
        'countSearchRanking'
      )) %>%
      distinct() %>%
      mutate(nameCompany = nameCompany %>% str_to_upper())
    closeAllConnections()
    return(all_companies)
  }


# EDGAR Counts ------------------------------------------------------------

get_cik_filing_count <-
  function(cik = 886982,
           return_message = TRUE) {
    code_cik  <-
      cik %>%
      pad_cik()

    url <-
      list("https://www.sec.gov/cgi-bin/srch-edgar?text=CIK%3D",
           code_cik,'&first=1994&last=',
           Sys.Date() %>% lubridate::year() %>% as.numeric()
      ) %>%
      purrr::reduce(paste0)

    page <-
      url %>%
      read_html()
    no_data <-
      page %>%
      html_nodes(css = 'p+ b') %>%
      html_text() %>% length() == 0

    if (no_data) {
      return(data_frame(idCIK = cik))
    }

    filings <-
      page %>%
      html_nodes(css = 'p+ b') %>%
      html_text() %>%
      readr::parse_number()

    pages <-
      ceiling(filings/100)

    df <-
      data_frame(idCIK = cik,
                 countFilings = filings,
                 countPages = pages) %>%
      mutate(isMultiSearch = pages > 20)

    if (return_message) {
      list("CIK: ", cik, " has ", filings %>% formattable::comma(digits = 0), ' Filings') %>%
        purrr::reduce(paste0) %>%
        message()
    }
    return(df)
  }

#' Title
#'
#' @param sic
#' @param return_message
#'
#' @return
#' @export
#' @import dplyr purrr curl formattable tidyr stringr lubridate rvest httr xml2 jsonlite readr stringi
#' @examples
get_sic_filing_count <-
  function(sic = 800,
           return_message = TRUE) {
    code_sic  <-
      sic %>%
      pad_sic()

    url <-
      list("https://www.sec.gov/cgi-bin/srch-edgar?text=ASSIGNED-SIC%3D",
           code_sic,'&first=1994&last=',
           Sys.Date() %>% lubridate::year() %>% as.numeric()
      ) %>%
      purrr::reduce(paste0)

    page <-
      url %>%
      read_html()

    no_data <-
      page %>%
      html_nodes(css = 'p+ b') %>%
      html_text() %>% length() == 0

    if (no_data) {
      return(data_frame(idCIK = cik))
    }

    filings <-
      page %>%
      html_nodes(css = 'p+ b') %>%
      html_text() %>%
      readr::parse_number()

    pages <-
      ceiling(filings/100)

    df <-
      data_frame(idSIC = sic,
                 countFilings = filings,
                 countPages = pages) %>%
      mutate(isMultiSearch = pages > 20)

    if (return_message) {
      list("SIC: ", sic, " has ", filings %>% formattable::comma(digits = 0), ' Filings') %>%
        purrr::reduce(paste0) %>%
        message()
    }
    return(df)
  }


# SEC Dictionaries --------------------------------------------------------

resolve_form_columns <-
  function(data) {
    data %>%
      mutate_if(is.character,
                funs(ifelse(. %in% c('_', "NULL"), NA, .))) %>%
      mutate_at(data %>% select(
        matches(
          "^name|^description|^idDay|^type|^title|^description|^code|^address|^city|^state|^relationship"
        )
      ) %>% names(),
      funs(. %>% str_to_upper())) %>%
      mutate_at(data %>% select(
        matches("^price|^count|^amount|^value|^idCIK|^yearIncorporation|^idSIC|^pershare|^number|^percent|^term|^pct|^score|^year")
      ) %>% names(),
      funs(. %>% readr::parse_number())) %>%
      mutate_at(data %>% select(matches("^is|^has")) %>% names(),
                funs(
                  ifelse(
                    . %in% c('true', 'false'),
                    . %>% as.logical(),
                    . %>% as.numeric() %>% as.logical()
                  )
                )) %>%
      mutate_at(data %>% select(matches("^date")) %>% names(),
                funs(. %>% lubridate::ymd())) %>%
      mutate_at(data %>% select(matches("^amountValueHoldings|^valueSecurities")) %>% names(),
                funs(. * 1000)) %>%
      suppressWarnings() %>%
      suppressMessages() %>%
      select(which(colMeans(is.na(.)) < 1))
  }


# SIC codes
# https://www.sec.gov/info/edgar/siccodes.htm

#' SIC Code dictionary
#'
#' @return
#' @export
#' @import rvest stringr dplyr purrr tidyr xml2
#' @family SEC
#' @family dictionary
#'
#'
#' @examples
#' get_dictionary_sic_codes()

get_dictionary_sic_codes <-
  function() {
    page <-
      "https://www.sec.gov/info/edgar/siccodes.htm" %>%
      read_html()

    data <-
      c(1, 2, 4) %>%
      map_df(function(x) {
        css_node <-
          list("tr~ tr+ tr td:nth-child(", x, ')') %>%
          purrr::reduce(paste0)

        if (x == 1) {
          values <-
            page %>%
            html_nodes(css_node) %>%
            html_text() %>% {
              .[5:length(.)]
            }
          item <-
            'idSIC'
        }

        if (x == 2) {
          values <-
            page %>%
            html_nodes(css_node) %>%
            html_text() %>% {
              .[3:length(.)]
            }
          item <-
            'idADOffice'
        }

        if (x == 4) {
          values <-
            page %>%
            html_nodes(css_node) %>%
            html_text()
          item <-
            'nameIndustry'
        }
        data_frame(value = values) %>%
          mutate(item,
                 idRow = 1:n()) %>%
          select(idRow, item, value)
      }) %>%
      spread(item, value) %>%
      mutate(idSIC = idSIC %>% as.numeric()
      ) %>%
      select(idSIC, idADOffice, nameIndustry)

    return(data)
  }

# Form Descriptions

#' SEC form dictionary
#'
#' @return
#' @export
#' @import dplyr purrr curl formattable tidyr stringr lubridate rvest httr xml2 jsonlite readr stringi
#' @examples
get_dictionary_sec_forms <-
  function() {
    page <-
      "https://www.sec.gov/forms" %>%
      read_html()

    forms <-
      page %>%
      html_nodes('.release-number-content') %>%
      html_text() %>%
      str_trim() %>%
      str_to_upper() %>%
      str_replace_all('NUMBER:', '')

    form_names <-
      page %>%
      html_nodes('.views-field-field-display-title a') %>%
      html_text() %>%
      str_to_upper() %>%
      str_trim() %>%
      str_replace_all('\r|\n|\u0092|\u0097', '') %>%
      str_replace_all('(PDF)', '') %>%
      str_replace_all('\\(', '') %>%
      str_replace_all('\\)', '') %>%
      str_trim()

    url_description_form <-
      page %>%
      html_nodes('.views-field-field-display-title a') %>%
      html_attr('href') %>%
      paste0('https://www.sec.gov', .)

    date_updated <-
      page %>%
      html_nodes('.datetime') %>%
      html_text() %>%
      list("01-", .) %>%
      purrr::reduce(paste0) %>%
      lubridate::dmy()

    sec_ids <-
      page %>%
      html_nodes('.list-page-detail-content') %>%
      html_text() %>%
      str_trim() %>%
      str_replace_all('SEC Number:', '') %>%
      str_trim()

    sec_ids[sec_ids == ''] <-
      NA


    reference <-
      page %>%
      html_nodes('td.views-field-term-node-tid') %>%
      html_text() %>%
      str_trim() %>%
      str_to_upper() %>%
      str_replace_all('\\TOPIC(S):','') %>%
      str_split('\\:') %>%
      purrr::map(function(x){
        x %>% str_split('\\:') %>% purrr::flatten_chr() %>% .[[2]]
      }) %>%
      purrr::flatten_chr()

    data <-
      data_frame(
        idForm = forms,
        nameForm = form_names,
        urlFormDescription = url_description_form,
        dateFormUpdate = date_updated,
        idSECNumber = sec_ids,
        referenceForm = reference
      )
    return(data)

  }


# General -----------------------------------------------------------------

#' Parse an EDGAR data frame for underlying tables
#'
#' @param all_data
#' @param table_name_initial
#' @param parse_all_filings
#' @param parse_complete_text_filings
#' @param parse_form_d
#' @param parse_13F
#' @param parse_small_offerings
#' @param parse_form_3_4s
#' @param parse_asset_files
#' @param parse_xbrl
#' @param assign_to_environment
#' @param nest_data
#' @param return_message
#'
#' @return
#' @export
#'
#' @examples
parse_for_tables <-
  function(all_data,
           table_name_initial = "All Filings",
           parse_all_filings = TRUE,
           parse_complete_text_filings = TRUE,
           parse_form_d = TRUE,
           parse_13F = TRUE,
           parse_small_offerings = TRUE,
           parse_form_3_4s = TRUE,
           parse_asset_files = TRUE,
           parse_xbrl = TRUE,
           assign_to_environment = FALSE,
           nest_data = TRUE,
           return_message = TRUE) {
    all_tables <-
      data_frame()

    parse_form_data_safe <-
      purrr::possibly(parse_form_data, data_frame())
    parse_all_filings <-
      c(
        parse_complete_text_filings,
        parse_form_d,
        parse_13F,
        parse_small_offerings,
        parse_form_3_4s,
        parse_asset_files,
        parse_xbrl
      ) %>%
      sum() > 0
    if ('termSearch' %in% names(all_data)) {
      df_general <-
        all_data %>%
        select(termSearch, countFilings) %>%
        distinct()

      all_tables <-
        all_tables %>%
        bind_rows(data_frame(nameTable = 'Summary', dataTable = list(df_general)))

      all_data <-
        all_data %>% select(-c(termSearch, countFilings))

    } else {
      all_tables <-
        all_tables %>%
        bind_rows(data_frame(nameTable = 'Summary', dataTable = list(data_frame())))
    }

    if (parse_all_filings) {
      all_data <-
        all_data %>%
        select(-matches(
          "hasAssetFile|isFormD|is13F|isForm3_4|hasSmallOfferingData"
        )) %>%
        distinct()

      if (!'typeFile' %in% names(all_data)) {
        all_data <-
          all_data %>%
          mutate(typeFile = ifelse(urlSECFilingDirectory %>% str_detect('htm'),
                                   'html', NA))
      }

      search_df <-
        all_data %>%
        select(dateFiling,
               matches("typeFile"),
               matches("idForm"),
               urlSECFilingDirectory) %>%
        distinct()

      df_all_filings <-
        search_df$urlSECFilingDirectory %>%
        unique() %>%
        map_df(function(x){
          parse_sec_filing_index(urls = x)
        })



      df_all_filings <-
        df_all_filings %>%
        nest(-c(idCIK, urlSECFilingDirectory, matches("idAccession")), .key = dataFilings)

      all_data <-
        all_data %>%
        select(-matches("dataFilings")) %>%
        left_join(df_all_filings %>% select(-one_of(c('idCIK', 'idAccession')))) %>%
        mutate(hasNoFilings = dataFilings %>% map_lgl(is_null)) %>%
        suppressMessages()

      all_tables <-
        all_tables %>%
        bind_rows(data_frame(nameTable = table_name_initial, dataTable = list(all_data)))

      all_filings <-
        all_data %>%
        filter(!hasNoFilings) %>%
        select(idCIK:typeFile, dataFilings)

      if (!'idCIKFiler' %in% names(all_filings)) {
        all_filings <-
          all_filings %>%
          dplyr::rename(idCIKFiler = idCIK)
      }

      if (!'typeFileFiler' %in% names(all_filings)) {
        all_filings <-
          all_filings %>%
          dplyr::rename(typeFileFiler = typeFile)
      }

      all_filings <-
        all_filings %>%
        select(matches("idCIK|data")) %>%
        unnest() %>%
        distinct()

      all_tables <-
        all_tables %>%
        bind_rows(data_frame(nameTable = 'All Filing URLS', dataTable = list(all_filings)))

      if (parse_complete_text_filings) {
        if (!'urlTextFilingFull' %in% names(all_data)) {
          all_data <-
            all_data %>%
            mutate(urlTextFilingFull = urlSECFilingDirectory %>% str_replace_all("-index.htm", ".txt"))
        }
        urls <-
          all_data$urlTextFilingFull %>%
          unique()
        get_data_sec_complete_filings_safe <-
          purrr::possibly(get_data_sec_complete_filings, data_frame())
        all_text_df <-
          get_data_sec_complete_filings(urls = urls)

        all_tables <-
          all_tables %>%
          bind_rows(data_frame(nameTable = 'Text Filings', dataTable = list(all_text_df)))
      }

      if (parse_form_d) {
        df_form_ds <-
          all_filings %>%
          parse_form_data_safe(filter_parameter = 'isFormD')
        all_tables <-
          all_tables %>%
          bind_rows(data_frame(nameTable = 'FormDs', dataTable = list(df_form_ds)))
      }

      if (parse_13F) {
        df_13F <-
          all_filings %>%
          parse_form_data_safe(filter_parameter = 'is13FFiling')
        all_tables <-
          all_tables %>%
          bind_rows(data_frame(nameTable = '13Fs', dataTable = list(df_13F)))
      }

      if (parse_small_offerings) {
        df_small_offerings <-
          all_filings %>%
          parse_form_data_safe(filter_parameter = 'hasSmallOfferingData')
        all_tables <-
          all_tables %>%
          bind_rows(data_frame(
            nameTable = 'Small Offerings',
            dataTable = list(df_small_offerings)
          ))
      }

      if (parse_form_3_4s) {
        df_form3_4 <-
          all_filings %>%
          parse_form_data_safe(filter_parameter = 'isForm3_4')

        all_tables <-
          all_tables %>%
          bind_rows(data_frame(nameTable = 'Form 3 and 4', dataTable = list(df_form3_4)))
      }

      if (parse_asset_files) {
        df_assets <-
          all_filings %>%
          parse_form_data(filter_parameter = 'hasAssetFile')

        all_tables <-
          all_tables %>%
          bind_rows(data_frame(nameTable = 'Asset Data', dataTable = list(df_assets)))
      }

      if (parse_xbrl) {
        df_xbrl <-
          all_filings %>%
          parse_form_data_safe(filter_parameter = 'isXBRLInstanceFile')

        all_tables <-
          all_tables %>%
          bind_rows(data_frame(nameTable = 'XBRL', dataTable = list(df_xbrl)))
      }

    } else {
      all_tables <-
        all_tables %>%
        bind_rows(data_frame(nameTable = 'TermsFilings', dataTable = list(all_data)))

    }

    all_tables <-
      all_tables %>%
      mutate(countCols = dataTable %>% map_dbl(ncol)) %>%
      filter(countCols > 0) %>%
      select(-countCols)

    if (assign_to_environment) {
      table_name_df <-
        all_tables %>%
        select(nameTable) %>%
        distinct() %>%
        mutate(
          nameDF =
            list('data', nameTable %>% str_replace_all('\\ ', ''), 'EDGAR') %>% purrr::invoke(paste0, .)
        )

      1:nrow(table_name_df) %>%
        walk(function(x) {
          df_data <-
            all_tables %>%
            slice(x) %>%
            select(dataTable) %>%
            unnest()

          df_name <-
            table_name_df %>% slice(x) %>% .$nameDF

          df_data <-
            df_data %>%
            mutate_at(.vars =
                        df_data %>% select(matches("^amount|^price|^value")) %>% names(),
                      funs(. %>% formattable::currency(digits = 2))) %>%
            mutate_at(
              .vars =
                df_data %>% select(matches("^count[A-Z]|^number")) %>% select(-matches("country")) %>% names(),
              funs(. %>% as.numeric() %>%  formattable::comma(digits = 0))
            ) %>%
            mutate_at(
              .vars = df_data %>% select(matches("^percent|^pct")) %>% select(-matches("country")) %>% names(),
              funs(. %>% as.numeric() %>% formattable::percent(digits = 0))
            ) %>%
            select(which(colMeans(is.na(.)) < 1)) %>%
            tidy_column_formats()

          assign(x = df_name,
                 eval(df_data),
                 envir = .GlobalEnv)
        })
    }
    return(all_tables)
  }


# SEC Free Text Search ----------------------------------------------------


generate_ft_search_urls <-
  function(search_term = c('"Rockwood Capital"'),
           return_message = TRUE) {
    term <-
      search_term %>%
      URLencode()

    base_url <-
      list("https://searchwww.sec.gov/EDGARFSClient/jsp/EDGAR_MainAccess.jsp?search_text=", term, "&sort=Date&startDoc=0&numResults=100&isAdv=true&formType=1&fromDate=mm/dd/yyyy&toDate=mm/dd/yyyy&stemming=true") %>%
      purrr::reduce(paste0)

    page <-
      base_url %>%
      read_html()

    page_total <-
      page %>%
      html_nodes('#header .normal+ .normalbold') %>%
      html_text() %>%
      readr::parse_number() %>%
      max(na.rm = TRUE)

    length_out <-
      ceiling(page_total/100)

    times <-
      seq(0,by = 100, length.out = length_out)

    urls <-
      list("https://searchwww.sec.gov/EDGARFSClient/jsp/EDGAR_MainAccess.jsp?search_text=", term, "&sort=Date&startDoc=", times,"&numResults=100&isAdv=true&formType=1&fromDate=mm/dd/yyyy&toDate=mm/dd/yyyy&stemming=true") %>%
      purrr::reduce(paste0)
    if (return_message) {
      list("Found SEC free text urls for ", search_term) %>% purrr::reduce(paste0) %>% message()
    }
    data_frame(termSearch = search_term, urlSECSearch = urls)
  }

parse_ft_filing_page <-
  function(urls, return_message = TRUE) {
    df <-
      data_frame()
    success <- function(res) {
      if (return_message) {
        list("Parsing: ", res$url) %>% purrr::reduce(paste0) %>% message()
      }
      page <-
        res$content %>%
        read_html()
      search_url <-
        res$url
      dates <-
        page %>%
        html_nodes('i.blue') %>%
        html_text() %>%
        lubridate::mdy()

      search_items <-
        page %>%
        html_nodes('.infoBorder+ tr td+ td #viewFiling') %>%
        html_text() %>%
        str_trim() %>%
        str_to_upper()

      urlFiling <-
        page %>%
        html_nodes('.infoBorder+ tr td+ td #viewFiling') %>%
        html_attr('href') %>%
        str_replace_all("javascript:opennew",'') %>%
        str_replace_all("'|\\(",'') %>%
        map_chr(function(x){
          x %>%
            str_split('\\,') %>%
            flatten_chr() %>%
            .[[1]]
        })

      ciks <-
        urlFiling %>%
        map_dbl(function(x){
          x %>% str_replace_all('http://www.sec.gov/Archives/edgar/data/','') %>%
            str_split('/') %>%
            flatten_chr() %>%
            .[[1]] %>%
            readr::parse_number()
        })

      text <-
        page %>%
        html_nodes('.small') %>%
        html_text() %>%
        str_to_upper()

      data <-
        data_frame(
          dateFiling = dates[1:length(ciks)],
          idCIKFiler = ciks,
          nameFilerFilingExhibit = search_items,
          descriptionText = text[1:length(ciks)],
          urlSECFiling = urlFiling
        ) %>%
        tidyr::separate(nameFilerFilingExhibit, sep = '\\ FOR ',
                        into = c('exhibitFiling', 'nameFiler'), remove = FALSE) %>%
        tidyr::separate(exhibitFiling, sep = '\\ OF ',
                        into = c('idExhibit', 'idForm'), remove = TRUE) %>%
        suppressWarnings()

      data <-
        data %>%
        mutate(idForm = ifelse(idForm %>% is.na(), idExhibit, idForm),
               idExhibit = ifelse(idForm == idExhibit, NA, idExhibit),
               urlSECSearch = search_url) %>%
        select(dateFiling, idCIKFiler, nameFiler, idForm, idExhibit, everything()) %>%
        suppressWarnings() %>%
        suppressMessages()

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
    df
  }

#' Title
#'
#' @param search_terms
#' @param nest_data
#' @param return_message
#' @import dplyr purrr curl formattable tidyr stringr lubridate rvest httr xml2 jsonlite readr stringi
#' @return
#' @export
#' @examples

get_data_edgar_ft_terms <-
  function(search_terms = c('"Jared Kushner"', '"EJF Capital"', '"Blackstone Real Estate"'),
           include_counts = TRUE,
           nest_data = FALSE,
           return_message = TRUE) {
    generate_ft_search_urls_safe <-
      purrr::possibly(generate_ft_search_urls, data_frame())

    df_urls <-
      search_terms %>%
      map_df(function(x) {
        generate_ft_search_urls_safe(search_term = x)
      })

    all_data <-
      parse_ft_filing_page(urls = df_urls$urlSECSearch, return_message = return_message) %>%
      left_join(df_urls) %>%
      select(termSearch, everything()) %>%
      suppressMessages() %>%
      arrange(desc(termSearch), desc(dateFiling)) %>%
      find_target_filings()

    if (include_counts) {
      df_counts <-
        all_data %>%
        select(idCIKFiler) %>%
        .$idCIKFiler %>%
        unique() %>%
        map_df(function(x){
          get_cik_filing_count(cik = x)
        })

      all_data <-
        all_data %>%
        left_join(df_counts %>%
                    dplyr::rename(idCIKFiler = idCIK) %>%
                    select(idCIKFiler, countFilings)) %>%
        select(termSearch:idCIKFiler, countFilings, everything()) %>%
        suppressMessages() %>%
        arrange(dateFiling, termSearch)
    }

    if (return_message) {
      results <-
        all_data %>% group_by(termSearch) %>% count(termSearch) %>% mutate(n = n %>% formattable::comma(digits = 0)) %>%
        unite(termMessage, termSearch, n, sep = ': ') %>%
        .$termMessage

      list(
        "\nSEC free text search filing mentions in the last 4 years:\n",
        results %>% paste0(collapse = '\n')
      ) %>%
        purrr::reduce(paste0) %>% message()
    }

    if (nest_data) {
      all_data <-
        all_data %>%
        nest(-c(termSearch), .key = dataFilings)
    }

    return(all_data)
  }

# Boolean Archive Search --------------------------------------------------
sec_parameter_df <- function() {
  data_frame(
    nameParameter = c(
      "Company Name",
      "Company CIK",
      "Public Document Count",
      "Accession Number",
      "Form Type",
      "Period",
      "Filing Date",
      "Company Name Confirmed",
      "CIK",
      "SIC",
      "IRS Number",
      "State of Incorporation",
      "Fiscal Year End",
      "Form Type Exact",
      "SEC Act",
      "File Number",
      "Business Address",
      "Mailing Address",
      "Former Company Name",
      "Date of Company Name Change",
      "Company",
      "form"
    ),
    slugParameter = c(
      "company-name",
      "company-cik",
      "Public-Document-Count",
      "Accession-Number",
      "type",
      "period",
      "Filing-Date",
      "Company-Name-Confirmed",
      "cik",
      "ASSIGNED-SIC",
      "irs-number",
      "STATE-OF-INCORPORATION",
      "Fiscal-Year-End",
      "Form-Type",
      "Act",
      "File-Number",
      "Business-Address",
      "Mailing-Address",
      "FORMER-CONFORMED-NAME",
      "DATE-CHANGED",
      'company-name',
      "type"
    )
  )
}


# https://www.sec.gov/edgar/searchedgar/edgarzones.htm
# https://www.sec.gov/cgi-bin/srch-edgar?ASSIGNED-SIC%3D0800&first=1994&last=2017
# https://www.sec.gov/cgi-bin/srch-edgar?text=Rockwood&start=1901&count=100&first=1994&last=2017
# https://www.sec.gov/edgar/searchedgar/search_help.htm
parse_boolean_search_page <-
  function(urls, return_message = TRUE) {
    df <-
      data_frame()
    success <- function(res){
      if (return_message) {
        list("Parsing: ", res$url, "\n") %>% purrr::reduce(paste0) %>% message()
      }
      page <-
        res$content %>%
        read_html()

      use_url <-
        page %>%
        html_nodes('div td:nth-child(2) a') %>%
        html_text() %>%
        str_to_upper() %>% length() == 0
      if (use_url) {
        page <-
          res$url %>%
          read_html()

      }

      entities <-
        page %>%
        html_nodes('div td:nth-child(2) a') %>%
        html_text() %>%
        str_to_upper()

      stems <-
        page %>%
        html_nodes('div td:nth-child(2) a') %>%
        html_attr('href')

      if (stems %>% length() > 0 ) {
        data <-
          1:length(stems) %>%
          map_df(function(x){
            stem <-
              stems[[x]]

            url_filing <-
              'https://www.sec.gov' %>% paste0(stem)

            items <-
              stem %>%
              str_replace_all('/Archives/edgar/data/','') %>%
              str_split('/') %>%
              flatten_chr()

            cik <-
              items[[1]] %>% as.numeric()

            accession <-
              items[length(items)]

            is_html <-
              accession %>% str_detect(".htm|.html")

            data_frame(idRow = x, idCIK = cik,
                       isHTML = is_html,
                       slugAccension = accession,
                       urlSECFilingDirectory = url_filing)

          })
      } else {
        data <-
          data_frame(idRow = x, idCIK = NA)
      }

      form <-
        page %>%
        html_nodes('td:nth-child(4)') %>%
        html_text() %>%
        str_to_upper()

      if (!length(form) == nrow(data)) {
        form <-
          form[2:length(form)]
      }

      date_filing <-
        page %>%
        html_nodes('td:nth-child(5)') %>%
        html_text() %>%
        lubridate::mdy()

      file_size <-
        page %>%
        html_nodes('td:nth-child(6)') %>%
        html_text() %>%
        readr::parse_number()

      data <-
        data %>%
        mutate(
          nameEntityLegal = entities,
          idForm = form,
          dateFiling = date_filing,
          sizeFile = file_size
        ) %>%
        resolve_legal_name() %>%
        select(-idRow) %>%
        select(dateFiling, idCIK, nameEntity, idForm, everything()) %>%
        find_target_filings()
      search_url <-
        res$url
      data <-
        data %>%
        separate(slugAccension,
                 sep = '\\.',
                 into = c('idAccession', 'typeFile')) %>%
        mutate(idAccession = idAccession %>% str_replace_all('-index', '')) %>%
        separate(
          idAccession,
          into = c('idCIKFilerSubmission', 'codeYear', 'countFilerYearFilings'),
          sep = '\\-',
          remove = FALSE
        ) %>%
        mutate_at(
          c('idCIKFilerSubmission', 'codeYear', 'countFilerYearFilings'),
          funs(. %>% readr::parse_number())
        ) %>%
        suppressMessages() %>%
        suppressWarnings() %>%
        mutate(
          urlSECSearch = search_url,
          isSameFiler = ifelse(idCIK == idCIKFilerSubmission, TRUE, FALSE),
          urlTextFilingFull = ifelse(
            typeFile %>% str_detect('htm'),
            urlSECFilingDirectory %>% str_replace_all("-index.htm", ".txt"),
            urlSECFilingDirectory
          )
        )
      closeAllConnections()
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
    df
  }
generate_edgar_search_url <-
  function(search_term = c('"Rockwood Capital"'),
           parameter = NULL,
           year_start = NULL,
           year_end = NULL,
           page_start = 0) {
    if (search_term %>% purrr::is_null()) {
      stop("Please enter a search term")
    }
    base <-
      'https://www.sec.gov/cgi-bin/srch-edgar?text='

    is_non_numeric <-
      !search_term %>% class()  == 'numeric'
    if (is_non_numeric) {
      term <-
        search_term %>%
        URLencode()
    } else {
      term <-
        search_term
    }
    term <-
      term %>% str_replace_all('\\=', '%3D')
    has_parameter <-
      !parameter %>% is_null()
    if (has_parameter) {
      df_params <-
        sec_parameter_df() %>%
        mutate_all(str_to_lower)

      parameter <-
        parameter %>% str_to_lower()

      wrong_param <-
        !parameter %in% df_params$nameParameter

      if (wrong_param) {
        stop(list("SEC boolean search parameters can only be\n", paste0(df_params$nameParameter, collapse = '\n')) %>% purrr::reduce(paste0))
      }

      param_slug <-
        df_params %>%
        filter(nameParameter == parameter) %>%
        .$slugParameter

      if (parameter %>% str_to_lower() %in% c('cik', 'company cik')) {
        term <-
          term %>%
          pad_cik()
      }

      if (parameter %>% str_to_lower() %>% str_detect('date')) {
        term <-
          term <-
          lubridate::ymd() %>% as.character() %>% str_replace_all('\\-','')
      }

      if (parameter %>% str_to_lower() == 'sic') {
        term <-
          term %>%
          pad_sic()
      }

      slug_term <-
        list(param_slug, '%3D', term) %>%
        purrr::reduce(paste0)
    } else {
      slug_term <-
        term
    }

    if (year_start %>% purrr::is_null()) {
      year_start <-
        1994
    }

    if (year_end %>% purrr::is_null()) {
      year_end <-
        Sys.Date() %>% lubridate::year() %>%
        as.numeric()
    }

    url <-
      list(base, slug_term, '&start=', page_start, '&count=100',
           '&first=', year_start, '&last=', year_end) %>%
      purrr::reduce(paste0)

    return(url)
  }
generate_search_term_urls <-
  function(search_term = c('"Rockwood Capital"'),
           parameter = NULL,
           year_start = NULL,
           year_end = NULL){
    url <-
      generate_edgar_search_url(
        search_term = search_term,
        parameter = parameter,
        year_start = year_start,
        year_end = year_end,
        page_start = 0
      )

    page <-
      url %>%
      read_html()

    filings <-
      page %>%
      html_nodes(css = 'p+ b') %>%
      html_text() %>%
      readr::parse_number()

    if (parameter %>% is_null()){
      search_message <-
        search_term

    } else {
      search_message <-
        list(parameter, ' = ', search_term) %>% purrr::reduce(paste0)
    }

    pages <-
      ceiling(filings / 100)

    list('\n',filings %>% formattable::comma(digits = 0), " total filings for search term: ",
         search_message, ' to parse'
    ) %>%
      purrr::reduce(paste0) %>%
      message()


    page_count <-
      seq(0, by = 100, length.out = pages)

    if (page_count %>% length() == 0) {
      page_count <-
        0
    }

    urls <-
      page_count %>%
      map_chr(function(x) {
        generate_edgar_search_url(
          search_term = search_term,
          parameter = parameter,
          year_start = year_start,
          year_end = year_end,
          page_start = x
        )
      })
    rm(page)
    closeAllConnections()
    df_urls <-
      data_frame(termSearch = search_term,
                 countFilings = filings, urlSECSearch = urls)
    return(df_urls)
  }

get_data_sec_search_term <-
  function(search_term = "Boston Properties",
           parameter = NULL,
           year_start = NULL,
           year_end = NULL,
           return_message = TRUE){
    url_df <-
      generate_search_term_urls(
        search_term = search_term,
        parameter = parameter,
        year_start = year_start,
        year_end = year_end
      )
    urls <-
      url_df$urlSECSearch

    all_data <-
      parse_boolean_search_page(urls = urls)


    all_data <-
      all_data %>%
      left_join(url_df) %>%
      suppressMessages() %>%
      select(termSearch, countFilings, everything())

    if (return_message) {
      list(
        "\nFound ",
        all_data %>% nrow() %>% formattable::comma(digits = 0),
        ' SEC filings for ',
        search_term,
        '\n'
      ) %>%
        purrr::reduce(paste0) %>%
        message()
    }

    return(all_data)
  }


#' Title
#'
#' @param search_terms
#' @param parameter
#' @param year_start
#' @param year_end
#' @param parse_all_filings
#' @param parse_form_d
#' @param parse_13F
#' @param parse_small_offerings
#' @param parse_form_3_4s
#' @param parse_asset_files
#' @param parse_xbrl
#' @param assign_to_environment
#' @param nest_data
#' @param return_message
#'
#' @return
#' @export
#' @import dplyr purrr curl formattable tidyr stringr lubridate rvest httr xml2 jsonlite readr stringi XBRL jsonlite
#' @importFrom jsonlite fromJSON
#' @examples
get_data_edgar_search_terms <-
  function(search_terms = NULL,
           parameter = NULL,
           year_start = NULL,
           year_end = NULL,
           table_name_initial = "All Filings",
           parse_all_filings = TRUE,
           parse_complete_text_filings = FALSE,
           parse_form_d = FALSE,
           parse_13F = FALSE,
           parse_small_offerings = FALSE,
           parse_form_3_4s = FALSE,
           parse_asset_files = FALSE,
           parse_xbrl = FALSE,
           assign_to_environment = TRUE,
           nest_data = TRUE,
           return_message = TRUE) {

    get_data_sec_search_term_safe <-
      purrr::possibly(get_data_sec_search_term, data_frame())

    all_data <-
      search_terms %>%
      map_df(function(x) {
        get_data_sec_search_term_safe(
          search_term = x,
          parameter = parameter,
          year_start = year_start,
          year_end = year_end
        )
      }) %>%
      dplyr::select(-matches("urlSECSearch")) %>%
      distinct()


    if (all_data %>% nrow() == 0) {
      return(data_frame())
    }

    parse_for_tables_safe <-
      purrr::possibly(parse_for_tables, data_frame())

    all_tables <-
      parse_for_tables_safe(
        all_data = all_data,
        table_name_initial = table_name_initial,
        parse_all_filings = parse_all_filings,
        parse_complete_text_filings = parse_complete_text_filings,
        parse_form_d = parse_form_d,
        parse_13F = parse_13F,
        parse_small_offerings = parse_small_offerings,
        parse_form_3_4s = parse_form_3_4s,
        parse_asset_files = parse_asset_files,
        parse_xbrl = parse_xbrl,
        nest_data = nest_data,
        return_message = return_message
      )

    if (all_tables %>% nrow() == 0) {
      return(all_data)
    }

    all_tables <-
      all_tables %>%
      bind_rows(data_frame(nameTable = 'Search Filings', dataTable = list(all_data)))

    if (assign_to_environment) {
      table_name_df <-
        all_tables %>%
        select(nameTable) %>%
        distinct() %>%
        mutate(
          nameDF =
            list('dataFiler', nameTable %>% str_replace_all('\\ ', '')) %>% purrr::invoke(paste0, .)
        )

      1:nrow(table_name_df) %>%
        walk(function(x) {
          df_name <-
            table_name_df %>% slice(x) %>% .$nameDF

          df_data <-
            all_tables %>%
            filter(nameTable == table_name_df$nameTable[[x]]) %>%
            select(matches(c('idCIK|nameEntity|dataTable'))) %>%
            unnest() %>%
            suppressWarnings()

          has_unnest <-
            names(df_data) %>% str_detect('data') %>% sum(na.rm = TRUE) > 1

          if (has_unnest) {
            base_names <-
              df_data %>% select(-matches("data")) %>% names()

            df_data_names <-
              names(df_data)[names(df_data) %>% str_detect('data')]

            for (df_data_name in df_data_names) {
              table <-
                df_data %>%
                select(one_of(c(base_names, df_data_name))) %>%
                unnest() %>%
                select(which(
                  colMeans(is.na(.)) < 1
                ))

              df_table_name <-
                list(df_name, df_data_name %>% str_replace_all('data', '')) %>% purrr::reduce(paste0)
              assign(x = df_table_name,
                     eval(table),
                     envir = .GlobalEnv)
            }

          } else {
            has_unnest <-
              df_data %>% names() %>% str_detect('data') %>% sum(na.rm = TRUE) > 0
            if (has_unnest) {
              df_data <-
                df_data %>%
                unnest()

              select_cols <- data_frame(nameData = names(df_data)) %>%
                mutate(idColumn = 1:n()) %>%
                group_by(nameData) %>%
                mutate(countColumn = 1:n()) %>%
                ungroup() %>%
                filter(countColumn == min(countColumn)) %>%
                .$idColumn

              df_data <-
                df_data[, select_cols]

              table <-
                df_data %>%
                select(which(
                  colMeans(is.na(.)) < 1
                ))
              assign(x = df_name,
                     eval(table),
                     envir = .GlobalEnv)
            } else {
              table <-
                df_data %>%
                select(which(
                  colMeans(is.na(.)) < 1
                ))
              assign(x = df_name,
                     eval(table),
                     envir = .GlobalEnv)
            }
          }
        })
    }
    return(all_tables)
  }



# Most Recent Filings -----------------------------------------------------

parse_most_recent_filing_form_page <-
  function(url = "https://www.sec.gov/cgi-bin/current?q1=0&q2=6&q3=10-D") {
    page <-
      url %>%
      read_html()

    data <-
      page %>%
      html_nodes(css = 'td pre') %>%
      html_text() %>%
      str_replace_all('Date Filed   Form        CIK Code     Company Name','') %>%
      read_table(col_names = FALSE) %>%
      purrr::set_names(c('dateFiling', 'idForm', 'idCIK', 'nameFiler')) %>%
      mutate(nameFiler = nameFiler %>% str_to_upper()) %>%
      suppressWarnings() %>%
      suppressMessages()

    urls <-
      page %>%
      html_nodes('pre a') %>%
      html_attr('href') %>%
      paste0('https://www.sec.gov',.)
    data_match <-
      urls %>% length() / 2 == (nrow(data))
    if (data_match) {
      data <-
        data %>%
        mutate(idRow = 1:n()) %>%
        left_join(
          data_frame(urlCIKFiler = urls[c(FALSE,TRUE)] %>% paste0('&start=0&count=100'),
                     urlSECFilingDirectory = urls[c(FALSE, TRUE)]) %>%
            mutate(idRow = 1:n())

        ) %>%
        suppressWarnings() %>%
        suppressMessages()
    }

    if (return_message) {
      list("Parsed: ", url) %>%
        purrr::invoke(paste0, .) %>% message()
    }
    closeAllConnections()

    return(data)
  }

parse_most_recent_stream <-
  function(url = "https://www.sec.gov/cgi-bin/browse-edgar?company=&CIK=&type=&owner=include&count=100&action=getcurrent",
           return_message = TRUE) {
    page <-
      url %>%
      xml2::read_html()

    url_directory <-
      page %>%
      html_nodes('a+ table td:nth-child(2) a:nth-child(1)') %>%
      html_attr(name = 'href') %>%
      paste0('https://www.sec.gov', .)

    urlTextFilingFull <-
      page %>%
      html_nodes('div a+ a') %>%
      html_attr(name = 'href') %>%
      paste0('https://www.sec.gov', .)

    forms <-
      page %>%
      html_nodes('a+ table td:nth-child(1)') %>%
      html_text() %>%
      str_trim()

    forms <-
      forms[!forms == '']

    url_directory <-
      page %>%
      html_nodes('a+ table td:nth-child(2) a:nth-child(1)') %>%
      html_attr('href') %>%
      paste0('https://www.sec.gov',.)

    filing_descriptions <-
      page %>%
      html_nodes('.small') %>%
      html_text() %>%
      str_trim()

    df_descriptions <-
      1:length(filing_descriptions) %>%
      map_df(function(x){
        description <-
          filing_descriptions[[x]] %>%
          str_to_upper()

        has_act <-
          description %>% str_detect("ACT:")

        if (has_act) {
          items <-
            description %>%
            str_split("ACCESSION NUMBER: ") %>%
            flatten_chr() %>%
            str_replace_all('\n','')

          filing_description <-
            items[[1]]

          items <-
            items[[2]] %>%
            str_split('ACT:') %>%
            flatten_chr() %>%
            str_trim()

          accession <-
            items[1]

          items <-
            items[[2]] %>%
            str_split("SIZE:") %>%
            flatten_chr() %>%
            str_trim()

          df <-
            data_frame(idRow = x,
                       descriptionFiling = filing_description,
                       idAccession = accession,
                       idSECAct = items[[1]],
                       descriptionFileSize = items[[2]])
          return(df)

        }

        items <-
          description %>%
          str_split("ACCESSION NUMBER: ") %>%
          flatten_chr() %>%
          str_replace_all('\n','')

        filing_description <-
          items[[1]]

        items <-
          items[[2]] %>%
          str_split('SIZE:') %>%
          flatten_chr() %>%
          str_trim()

        df <-
          data_frame(idRow = x,
                     descriptionFiling = filing_description,
                     idAccession = items[[1]],
                     descriptionFileSize = items[[2]])
        return(df)
      })

    filer_description <-
      page %>%
      html_nodes('td:nth-child(3) a') %>%
      html_text()

    df_filers <-
      1:length(filer_description) %>%
      map_df(function(x){
        filer <-
          filer_description[[x]] %>%
          str_to_upper()

        is_messed <-
          filer %>% str_count("\\(") > 2

        if (!is_messed) {
          values <-
            filer %>%
            str_split('\\(') %>%
            flatten_chr() %>%
            str_replace_all('[\\)]','') %>%
            str_trim()

          df <-
            data_frame(idRow = x, item = c('nameEntityLegal', 'idCIK', 'typeSECEntity'), value = values) %>%
            spread(item, value) %>%
            mutate(idCIK = idCIK %>% as.numeric())

          df <-
            df %>%
            resolve_legal_name() %>%
            select(idCIK, nameEntity, everything())
          return(df)
        }

        if (is_messed) {
          values <-
            filer %>%
            str_split('\\(') %>%
            flatten_chr() %>%
            str_replace_all('[\\)]','') %>%
            str_trim()

          values <-
            c(list(values[1], values[2]) %>%
                purrr::reduce(paste), values[3], values[4])

          df <-
            data_frame(idRow = x, item = c('nameEntityLegal', 'idCIK', 'typeSECEntity'), value = values) %>%
            spread(item, value) %>%
            mutate(idCIK = idCIK %>% as.numeric())

          df <-
            df %>%
            resolve_legal_name() %>%
            select(idCIK, nameEntity, everything())
          return(df)
        }
      })

    url_cik_filer <-
      page %>%
      html_nodes('td:nth-child(3) a') %>%
      html_attr('href') %>%
      paste0('https://www.sec.gov',.) %>%
      paste0(., '&start=0')

    datetime_accepted <-
      page %>%
      html_nodes(css = '.small+ td') %>%
      html_text() %>%
      lubridate::ymd_hms()

    date_filed <-
      page %>%
      html_nodes('td:nth-child(5)') %>%
      html_text() %>%
      lubridate::ymd()

    file_film <- page %>%
      html_nodes('td:nth-child(6)') %>%
      html_text()

    df_films <-
      1:length(file_film) %>%
      map_df(function(x){
        parts <-
          file_film[[x]] %>% str_split('\n') %>% flatten_chr()
        data_frame(
          idRow = x,
          idFile = parts[[1]] %>% stringr::str_trim(),
          idSECFiling = parts[[2]] %>% as.numeric()
        ) %>%
          mutate(
            urlSECFile = list(
              "https://www.sec.gov/cgi-bin/browse-edgar?action=getcompany&filenum=",
              idFile,
              '&owner=include&count=100000'
            ) %>% purrr::reduce(paste0)
          )
      })

    data <-
      df_filers %>%
      left_join(df_descriptions) %>%
      select(-idRow) %>%
      mutate(urlTextFilingFull,
             dateFiling = date_filed,
             datetimeAccepted = datetime_accepted,
             idForm = forms,
             urlSECFilingDirectory = url_directory,
             urlCIKFIler = url_cik_filer,
             urlSearch = url) %>%
      select(idForm, everything()) %>%
      suppressMessages() %>%
      find_target_filings()

    if (df_films %>% nrow() == data %>% nrow()) {
      data <-
        data %>%
        mutate(idRow = 1:n()) %>%
        left_join(df_films) %>%
        suppressMessages() %>%
        select(-idRow)
    }

    if(return_message) {
      list("Parsed: ", url) %>%
        purrr::invoke(paste0, .) %>% message()
    }
    data <-
      data %>%
      mutate_at(
        data %>% select_if(is.character) %>% select(-matches("url")) %>% names(),
        funs(ifelse(. == '', NA, .) %>% str_to_upper())
      )
    if ('descriptionFileSize' %in% names(data)) {
      data <-
        data %>%
        mutate(
          typeFileDocument = descriptionFileSize %>% map_chr(stringi::stri_extract_last_boundaries),
          sizeFile = readr::parse_number(descriptionFileSize),
          sizeFileBytes = ifelse(typeFileDocument == "MB", sizeFile * 1024, 1048576 * sizeFile)
        ) %>%
        select(-c(typeFileDocument, descriptionFileSize, sizeFile))
    }

    return(data)
  }

get_most_recent_filing_urls <-
  function(filing_type = NULL, pages_out = 20) {
    start_pages <-
      seq(0, by = 100, length.out = pages_out)
    if ('dfEnd' %>% exists()) {
      eval(rm(dfEnd))
    }

    if (filing_type %>% is_null()) {
      slug_filing <-
        ''
    } else {
      if (filing_type %>% str_to_lower() == 'all') {
        slug_filing <-
          ''
      } else {
        slug_filing <-
          filing_type
      }
    }

    urls <-
      list('https://www.sec.gov/cgi-bin/browse-edgar?action=getcurrent&datea=&dateb=&company=&type=',slug_filing, '&SIC=&State=&Country=&CIK=&owner=include&accno=&start=',
           start_pages, '&count=100') %>%
      purrr::reduce(paste0)

    is_on <-
      TRUE
    for (url in urls) {
      if (!is_on) {
        invisible()
      }
      if('dfEnd' %>% exists()) {
        invisible()
      } else {
        df_end <-
          guess_page_ongoing(url = url, override = FALSE)
        is_over_zero <-
          df_end %>% length() > 0
        if (is_over_zero) {
          assign('dfEnd', eval(df_end), envir = .GlobalEnv)
          assign('is_on', eval(FALSE), envir = .GlobalEnv)
          rm(is_over_zero)
        }
      }
    }
    still_none <-
      df_end %>% length() == 0
    if (still_none)  {
      df_end <-
        urls %>%
        map_df(function(x){
          guess_page_ongoing(url = x, override = TRUE)
        })
      df_end <-
        df_end %>%
        slice(nrow(df_end))
    }

    if ('countPage' %in% names(df_end)) {
      df_end <-
        df_end %>%
        dplyr::rename(countStart = countPage)
    }

    if (df_end$countStart < 0) {
      df_end <-
        df_end %>%
        mutate(countStart = 0)
    }

    if (slug_filing == '') {
      df_end <-
        df_end %>%
        mutate(countStart = 2000)
    }

    length_actual_pages <-
      ceiling(df_end$countStart/100)
    length_actual <-
      seq(0, by = 100, length.out =  length_actual_pages)
    urls <-
      list('https://www.sec.gov/cgi-bin/browse-edgar?action=getcurrent&datea=&dateb=&company=&type=',slug_filing, '&SIC=&State=&Country=&CIK=&owner=include&accno=&start=',
           length_actual, '&count=100') %>%
      purrr::reduce(paste0)

    df_mr_urls <-
      data_frame(urlPageFiling = urls) %>%
      mutate(countPage = 1:n())

    if (!filing_type %>% is_null()) {
      df_mr_urls <-
        df_mr_urls %>%
        mutate(idForm = filing_type) %>%
        select(idForm, everything())
    }
    if('dfEnd' %>% exists()){
      rm(list = c('dfEnd'), pos = ".GlobalEnv")
    }
    return(df_mr_urls)
  }


get_data_sec_filing_most_recent <-
  function(filing_type = NULL, return_message = TRUE) {
    get_most_recent_filing_urls_safe <-
      purrr::possibly(get_most_recent_filing_urls, data_frame())
    url_df <-
      get_most_recent_filing_urls_safe(filing_type = filing_type)

    if (filing_type %>% is_null()) {
      filing_name <-
        'all'
    } else {
      filing_name <-
        filing_type
    }

    parse_most_recent_stream_safe <-
      purrr::possibly(parse_most_recent_stream, data_frame())

    all_data <-
      url_df$urlPageFiling %>%
      map_df(function(x){
        parse_most_recent_stream_safe(url = x, return_message = return_message)
      }) %>%
      mutate(idFormName = filing_name) %>%
      select(idFormName, everything())

    if (return_message) {
      list("\nReturned ", all_data %>% nrow() %>% formattable::comma(digits = 0),
           ' of the most recent filings from ', filing_type, ' forms\n') %>%
        purrr::reduce(paste0) %>%
        message()
    }
    return(all_data)
  }

#' Most recent EDGAR filings by type
#'
#' @param forms
#' @param nest_data
#' @param return_message
#'
#' @return
#' @export
#' @import dplyr tidyr purrr stringr formattable readr lubridate XBRL curl jsonlite lazyeval
#' @importFrom jsonlite fromJSON
#' @examples
get_data_edgar_filings_most_recent <-
  function(forms = c("All", "10-D", "10-K"),
           table_name_initial = "Recent Filings",
           parse_all_filings = TRUE,
           parse_form_d = FALSE,
           parse_complete_text_filings = FALSE,
           parse_13F = FALSE,
           parse_small_offerings =  FALSE,
           parse_form_3_4s =  FALSE,
           parse_asset_files = FALSE,
           parse_xbrl =  FALSE,
           assign_to_environment =  TRUE,
           nest_data = FALSE,
           return_message = TRUE) {
    get_data_sec_filing_most_recent_safe <-
      purrr::possibly(get_data_sec_filing_most_recent, data_frame())

    all_data <-
      forms %>%
      map_df(function(x) {
        get_data_sec_filing_most_recent_safe(filing_type = x,
                                             return_message = return_message)
      }) %>%
      select(matches("dateFiling"), idCIK, nameEntity, idForm, everything())

    all_data <-
      all_data %>%
      select(-matches("datetimeAccepted|^is[A-Z]|^has[A-Z]|is13FFiling")) %>%
      parse_for_tables(
        table_name_initial = table_name_initial,
        parse_all_filings = parse_all_filings,
        parse_complete_text_filings = parse_complete_text_filings,
        parse_form_d = parse_form_d,
        parse_13F = parse_13F,
        parse_small_offerings = parse_small_offerings,
        parse_form_3_4s = parse_form_3_4s,
        parse_asset_files = parse_asset_files,
        parse_xbrl = parse_xbrl,
        assign_to_environment = assign_to_environment,
        nest_data = nest_data,
        return_message = return_message
      )
    return(all_data)

  }

# SEC index logs ----------------------------------------------------------

get_year_index_urls <-
  function(url = "https://www.sec.gov/Archives/edgar/daily-index/2016/") {
    yearData <-
      url %>%
      str_replace_all('https://www.sec.gov/Archives/edgar/daily-index|/','') %>%
      readr::parse_number()

    page <-
      url %>%
      read_html()

    quarters <-
      page %>%
      html_nodes('td a') %>%
      html_attr('href') %>%
      str_replace_all('\\QTR|/','') %>%
      readr::parse_number()

    urls <-
      page %>%
      html_nodes('td a') %>%
      html_attr('href') %>%
      list(url, .) %>%
      purrr::reduce(paste0)

    url_df <-
      data_frame(idQuarter =quarters, yearData,
                 urlQuarter = urls)
    return(url_df)
  }

parse_quarter_urls <-
  function(url = "https://www.sec.gov/Archives/edgar/daily-index/2012/QTR4/",
           index_type = 'master',
           return_message = TRUE) {
    page <-
      url %>%
      read_html()

    slugs <-
      page %>%
      html_nodes('td a') %>%
      html_attr('href')

    slugs <-
      slugs[!slugs %>% str_detect("xml")]

    urls <-
      list(url, slugs) %>%
      purrr::reduce(paste0)

    df_urls <-
      data_frame(slugs, urlSECIndex = urls) %>%
      tidyr::separate(slugs,
                      into = c('typeIndex', 'dateData', 'remove'),
                      sep = '\\.')

    if (df_urls$dateData[[1]] %>% lubridate::ymd() %>% is.na()) {
      df_urls <-
        df_urls %>%
        mutate(dateIndex = dateData %>% lubridate::mdy()) %>%
        select(-c(remove)) %>%
        mutate(urlQuarter = url)

    } else {
      df_urls <-
        df_urls %>%
        mutate(dateIndex = dateData %>% lubridate::ymd()) %>%
        select(-c(remove)) %>%
        mutate(urlQuarter = url)

    }


    if (index_type %>% is_null()) {
      df_urls <-
        df_urls %>%
        filter(typeIndex == 'master')
    } else {
      df_urls <-
        df_urls %>%
        filter(typeIndex == index_type)
    }

    if (return_message) {
      list("Parsed: ", url) %>%
        purrr::invoke(paste0, .) %>% message()
    }

    return(df_urls)

  }

parse_index_filing_page <-
  function(url = "https://www.sec.gov/Archives/edgar/daily-index/1994/QTR3/company.070194.idx",
           return_message = TRUE) {
    start_skip <-
      url %>%
      read_lines() %>%
      grep('------', .)

    url_slug <-
      url %>%
      str_split('\\/') %>%
      flatten_chr() %>%
      .[length(.)] %>%
      str_replace_all('\\.idx', '') %>%
      str_split('\\.') %>%
      flatten_chr()

    index_type <-
      url_slug[[1]]

    index_date <-
      url_slug[[2]] %>%
      as.numeric() %>%
      lubridate::ymd() %>%
      suppressMessages()

    if (index_date %>% is.na()) {
      index_date <-
        url_slug[[2]] %>%
        as.numeric() %>%
        lubridate::mdy()
    }

    df <-
      url %>%
      readr::read_table(skip = start_skip,
                        col_names = FALSE,
                        progress = FALSE) %>%
      suppressMessages() %>%
      suppressWarnings()

    if (df %>% ncol() == 1) {
      df <-
        df %>%
        separate(X1, sep = '\\|',
                 c(
                   'idCIK',
                   'nameEntityLegal',
                   'idForm',
                   'dateFiling',
                   'slugAccension'
                 ))

      df <-
        df %>%
        mutate(idCIK = idCIK %>% as.numeric()) %>%
        mutate(nameEntity = nameEntityLegal %>% str_to_upper() %>% str_replace_all('\\.|\\,', '') %>% str_trim(),
               dateIndex = index_date,
               typeIndex = index_type) %>%
        separate(nameEntity,
                 sep = '\\ /',
                 into = c('nameEntity', 'idLocationEntity')) %>%
        mutate(
          dateFiling = dateFiling %>% lubridate::ymd(),
          idLocationEntity = idLocationEntity %>% str_replace_all('\\/', '') %>% str_trim()
        ) %>%
        suppressWarnings() %>%
        suppressMessages() %>%
        mutate(
          urlSECFilingText = list("https://www.sec.gov/Archives/", slugAccension) %>%
            purrr::reduce(paste0),
          urlSECFilingDirectory = urlSECFilingText %>% str_replace_all(".txt", '-index.html')
        ) %>%
        select(nameEntity, idLocationEntity, everything()) %>%
        suppressWarnings()

      df <-
        df %>%
        mutate(
          dataAccension = slugAccension %>% str_replace_all('edgar/data/|.txt', ''),
          urlSECIndex = url
        ) %>%
        tidyr::separate(dataAccension,
                        sep = '\\/',
                        into = c('remove', 'idAccession')) %>%
        select(-remove) %>%
        tidyr::separate(
          idAccession,
          into = c('idCIKFiler', 'codeYear', 'countFilerYearFilings'),
          remove = FALSE,
          sep = '\\-'
        ) %>%
        mutate_at(
          c('idCIKFiler', 'codeYear', 'countFilerYearFilings'),
          funs(. %>% as.numeric())
        ) %>%
        mutate(hasDifferentSECFiler = ifelse(!idCIK == idCIKFiler, TRUE, FALSE)) %>%
        select(
          typeIndex,
          dateIndex,
          dateFiling,
          idCIK,
          nameEntity,
          idForm,
          idAccession,
          countFilerYearFilings,
          hasDifferentSECFiler,
          everything()
        ) %>%
        suppressWarnings() %>%
        suppressMessages()

      if (return_message) {
        list("Parsed: ", url) %>%
          purrr::invoke(paste0, .) %>% message()
      }
      return(df)
    }


    is_form <-
      index_type == 'form'
    if (is_form) {
      if (df %>% ncol() == 6) {
        df <-
          df %>%
          tidyr::unite(X2, X2, X3, sep = ' ')
      }

      df <-
        df %>%
        purrr::set_names(c(
          'idForm',
          'nameEntityLegal',
          'idCIK',
          'dateFiling',
          'slugAccension'
        ))
    }

    if (!is_form) {
      if (df %>% ncol() == 6) {
        df <-
          df %>%
          tidyr::unite(X1, X1, X2, sep = ' ')
      }
      df <-
        df %>%
        purrr::set_names(c(
          'nameEntityLegal',
          'idForm',
          'idCIK',
          'dateFiling',
          'slugAccension'
        ))
    }

    df <-
      df %>%
      mutate(nameEntity = nameEntityLegal %>% str_to_upper() %>% str_replace_all('\\.|\\,', '') %>% str_trim(),
             dateIndex = index_date,
             typeIndex = index_type) %>%
      separate(nameEntity,
               sep = '\\ /',
               into = c('nameEntity', 'idLocationEntity')) %>%
      mutate(
        dateFiling = dateFiling %>% lubridate::ymd(),
        idLocationEntity = idLocationEntity %>% str_replace_all('\\/', '') %>% str_trim()
      ) %>%
      suppressWarnings() %>%
      suppressMessages()

    is_http <-
      df$slugAccension %>% str_count('\\.htm') %>% sum() / nrow(df) > .5

    if (is_http) {
      df <-
        df %>%
        dplyr::rename(urlSECFilingDirectory = slugAccension) %>%
        select(
          typeIndex,
          dateIndex,
          dateFiling,
          idCIK,
          nameEntity,
          everything())

      if (return_message) {
        list("Parsed: ", url) %>%
          purrr::invoke(paste0, .) %>% message()
      }

      return(df)
    }

    df <-
      df %>%
      mutate(
        urlSECFilingText = list("https://www.sec.gov/Archives/", slugAccension) %>%
          purrr::reduce(paste0),
        urlSECFilingDirectory = urlSECFilingText %>% str_replace_all(".txt", '-index.html')
      ) %>%
      select(nameEntity, idLocationEntity, everything()) %>%
      suppressWarnings()

    df <-
      df %>%
      mutate(
        dataAccension = slugAccension %>% str_replace_all('edgar/data/|.txt', ''),
        urlSECIndex = url
      ) %>%
      tidyr::separate(dataAccension,
                      sep = '\\/',
                      into = c('remove', 'idAccession')) %>%
      select(-remove) %>%
      tidyr::separate(
        idAccession,
        into = c('idCIKFiler', 'codeYear', 'countFilerYearFilings'),
        remove = FALSE,
        sep = '\\-'
      ) %>%
      mutate_at(
        c('idCIKFiler', 'codeYear', 'countFilerYearFilings'),
        funs(. %>% as.numeric())
      ) %>%
      mutate(hasDifferentSECFiler = ifelse(!idCIK == idCIKFiler, TRUE, FALSE)) %>%
      select(
        typeIndex,
        dateIndex,
        dateFiling,
        idCIK,
        nameEntity,
        idAccession,
        countFilerYearFilings,
        hasDifferentSECFiler,
        everything()
      ) %>%
      suppressWarnings() %>%
      suppressMessages()

    if (return_message) {
      list("Parsed: ", url) %>%
        purrr::invoke(paste0, .) %>% message()
    }
    closeAllConnections()
    return(df)
  }

get_years_page_urls <-
  function(years = 1994:2017,
           index_type = 'master',
           return_message = TRUE) {

    wrong_years <-
      (years < 1993) %>% as.numeric() %>% sum(na.rm = TRUE) > 0

    if (wrong_years) {
      stop("Years to search start in 1994")
    }

    urls <-
      list('https://www.sec.gov/Archives/edgar/daily-index/',
           years,
           '/') %>%
      purrr::reduce(paste0)

    df_urls <-
      urls %>%
      map_df(function(x) {
        get_year_index_urls(url = x)
      })

    all_url_df <-
      df_urls$urlQuarter %>%
      map_df(function(x) {
        parse_quarter_urls(url = x,
                           index_type = index_type,
                           return_message = return_message)
      }) %>%
      suppressWarnings()

    all_url_df <-
      all_url_df %>%
      left_join(df_urls) %>%
      select(yearData, idQuarter, everything()) %>%
      suppressMessages() %>%
      select(-dateData)

    return(all_url_df)
  }

#' SEC filing streams
#'
#' This function parses daily
#' SEC filing log starting in 1994
#' for specified periods
#'
#' @param start_date starting date in year-month-date form
#' @param end_date ending date starting in year-month-date form
#' @param only_most_recent_data \code{TRUE} return only most recent day's filing stream
#' @param index_type type of index to parse \itemize{
#' \item \code{master}: parses master log (default)
#' \item \code{compamy}: parses company log
#' \item \code{filer}: parses filer log
#' }
#' @param return_message \code{TRUE} return a message after data import
#' @param nest_data \code{TRUE} return nested data frame
#' @return nested \code{data_frame} or \code{data_frame} if \code{nest_data = FALSE}
#' @references \href{http://sec.gov}{The Securities and Exchange Commission}
#' @import dplyr tidyr purrr stringr formattable readr lubridate XBRL curl jsonlite lazyeval
#' @importFrom jsonlite fromJSON
#' @export
#' @family SEC
#' @family filing search
#' @examples
#' \dontrun{
#' get_data_edgar_filing_streams(start_date = "2016-01-01",
#' end_date = Sys.Date(), only_most_recent_data = FALSE, index_type = 'master',
#' nest_data = TRUE,
#' return_message = TRUE)
#' }

get_data_edgar_filing_streams <-
  function(start_date = "2017-02-15",
           end_date = Sys.Date(),
           only_most_recent_data = FALSE,
           index_type = 'master',
           table_name_initial = "Filing Logs",
           parse_all_filings = FALSE,
           parse_complete_text_filings = FALSE,
           parse_form_d = FALSE,
           parse_13F = FALSE,
           parse_small_offerings =  FALSE,
           parse_form_3_4s =  FALSE,
           parse_asset_files = FALSE,
           parse_xbrl =  FALSE,
           assign_to_environment =  TRUE,
           nest_data = TRUE,
           return_message = TRUE) {

    start_date <-
      start_date %>%
      lubridate::ymd()

    end_date <-
      end_date %>%
      lubridate::ymd()

    start_year <-
      lubridate::year(start_date)

    end_year <-
      end_date %>% lubridate::year()

    search_years <-
      start_year:end_year

    if (only_most_recent_data) {
      search_years <-
        Sys.Date() %>% lubridate::year()

      df_urls <-
        get_years_page_urls(years = search_years,
                            index_type = index_type,
                            return_message = return_message)

      urls <-
        df_urls %>%
        slice(nrow(df_urls)) %>%
        .$urlSECIndex
    }

    if (!only_most_recent_data) {
      df_urls <-
        get_years_page_urls(years = search_years,
                            index_type = index_type,
                            return_message = return_message)

      urls <-
        df_urls %>%
        filter(dateIndex >= start_date) %>%
        filter(dateIndex <= end_date) %>%
        .$urlSECIndex
    }

    parse_index_filing_page_safe <-
      purrr::possibly(parse_index_filing_page, data_frame())

    all_data <-
      1:length(urls) %>%
      map_df(function(x) {
        parse_index_filing_page_safe(url = urls[[x]], return_message = return_message)
      })

    all_data <-
      all_data %>%
      left_join(df_urls %>% select(urlSECIndex, yearData, idQuarter)) %>%
      select(yearData, idQuarter, dateIndex, everything()) %>%
      suppressMessages() %>%
      suppressWarnings()

    all_data <-
      all_data %>%
      mutate_at(all_data %>% select(matches("count")) %>% names(),
                funs(. %>% formattable::comma(digits = 0))) %>%
      select(-matches("slugAccension"))

    if (return_message) {
      list(
        "Parsed ",
        all_data %>% nrow() %>% formattable::comma(digits = 0),
        " SEC filings from ",
        all_data$dateIndex %>% min(na.rm = T),
        ' to ',
        all_data$dateIndex %>% max(na.rm = TRUE)
      ) %>%
        purrr::reduce(paste0) %>%
        message()
    }

    all_data <-
      all_data %>%
      parse_for_tables(
        table_name_initial = table_name_initial,
        parse_all_filings = parse_all_filings,
        parse_form_d = parse_form_d,
        parse_13F = parse_13F,
        parse_small_offerings = parse_small_offerings,
        parse_form_3_4s = parse_form_3_4s,
        parse_asset_files = parse_asset_files,
        parse_xbrl = parse_xbrl,
        nest_data = nest_data,
        return_message = return_message
      )

    return(all_data)
  }




# CIK Search --------------------------------------------------------------


guess_page_ongoing <-
  function(url = "https://www.sec.gov/cgi-bin/browse-edgar?action=getcompany&CIK=1184765&type=&dateb=&owner=include&start=0&count=100",
           override = FALSE) {
    page <-
      url %>%
      read_html()

    page_count <-
      url %>% str_split('start=') %>%
      flatten_chr() %>%
      .[[2]] %>%
      str_split('&') %>%
      flatten_chr() %>%
      .[[1]] %>%
      readr::parse_number()

    items <-
      page %>%
      html_nodes('input') %>%
      html_attr('value') %>% str_to_upper() %>%
      unique()

    if (items %>% length() == 0){
      return(invisible())
    }

    no_page <-
      page %>%
      html_nodes('h1') %>%
      html_text() %>%
      str_to_lower() == 'invalid parameter'

    no_page <-
      no_page %>%
      length() > 0
    is_end <-
      !items %>% str_detect("NEXT 100") %>% sum(na.rm = T) > 0

    if (is_end & (!no_page)) {
      return(data_frame(isEnd = TRUE, countStart = page_count))
    }
    if (!override) {
      if (!is_end) {
        return(data_frame())
      }
    } else {
      return(data_frame(countStart = page_count))
    }
    data_frame(isEnd = is_end, countPage = page_count -100)
  }

parse_search_page <-
  function(urls = "https://www.sec.gov/cgi-bin/browse-edgar?action=getcompany&company=Bank&owner=exclude&match=&start=500&count=100&hidefilings=0",
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
      cik <-
        page %>%
        html_nodes('td:nth-child(1) a') %>%
        html_text() %>%
        as.numeric()

      entities <-
        page %>%
        html_nodes('td:nth-child(2)') %>%
        html_text() %>%
        str_to_upper()

      locations <-
        page %>%
        html_nodes('td:nth-child(3)') %>%
        html_text() %>%
        str_to_title() %>%
        str_trim()

      locations[locations == ''] <-
        NA

      data <-
        data_frame(
          idCIK = cik,
          nameEntityLegal = entities,
          codeLocationBusiness = locations
        ) %>%
        mutate(codeLocationBusiness = codeLocationBusiness %>% str_to_upper()) %>%
        separate(nameEntityLegal,
                 into = c('nameEntityLegal', 'sic'),
                 sep = 'SIC: ') %>%
        separate(sic,
                 into = c('idSIC', 'nameIndustry'),
                 sep = '-') %>%
        mutate(
          idSIC = idSIC %>% str_trim() %>% as.numeric(),
          nameIndustry = nameIndustry %>% str_trim()
        ) %>%
        suppressWarnings() %>%
        suppressMessages() %>%
        select(which(colMeans(is.na(.)) < 1)) %>%
        mutate(nameEntity = nameEntityLegal %>% str_to_upper() %>% str_replace_all('\\.|\\,', '') %>% str_trim()) %>%
        select(idCIK, nameEntity, everything()) %>%
        separate(nameEntity,
                 sep = '\\ /',
                 into = c('nameEntity', 'idLocationEntity')) %>%
        mutate(
          nameEntity = nameEntity %>% gsub('/', '', .),
          idLocationEntity = idLocationEntity %>% str_replace_all('\\/', '') %>% str_trim()
        ) %>%
        suppressWarnings() %>%
        suppressMessages() %>%
        select(-matches("idLocationEntity"))
      df <<-
        df %>%
        bind_rows(data)
    }
    failure <- function(msg){
      data_frame()
    }
    urls %>%
      walk(function(x) {
        curl_fetch_multi(url = x, success, failure)
      })
    multi_run()
    df
  }
parse_search_page_length <-
  function(search_term = "BREA", pages_out = 5) {
    term <-
      search_term %>% URLencode()

    start_pages <-
      seq(0, by = 100, length.out = pages_out)
    if ('dfEnd' %>% exists()) {
      eval(rm(dfEnd))
    }
    urls <-
      list('https://www.sec.gov/cgi-bin/browse-edgar?action=getcompany&company=',term, '&type=&dateb=&owner=include&start=',
           start_pages, '&count=100') %>%
      purrr::reduce(paste0)

    is_on <-
      TRUE
    for (url in urls) {
      if (!is_on) {
        invisible()
      }
      if('dfEnd' %>% exists()) {
        invisible()
      } else {
        df_end <-
          guess_page_ongoing(url = url, override = FALSE)
        is_over_zero <-
          df_end %>% length() > 0
        if (is_over_zero) {
          assign('dfEnd', eval(df_end), envir = .GlobalEnv)
          assign('is_on', eval(FALSE), envir = .GlobalEnv)
          rm(is_over_zero)
        }
      }
    }
    still_none <-
      df_end %>% length() == 0
    if (still_none)  {
      df_end <-
        urls %>%
        map_df(function(x){
          guess_page_ongoing(url = x, override = TRUE)
        })
      df_end <-
        df_end %>%
        slice(nrow(df_end))
    }

    if (df_end %>% ncol() == 0) {
      df_end <-
        data_frame(countStart = 0)
    }
    length_actual_pages <-
      ceiling(df_end$countStart/100)

    length_actual <-
      seq(0, by = 100, length.out =  length_actual_pages)
    urls <-
      list('https://www.sec.gov/cgi-bin/browse-edgar?action=getcompany&company=',term, '&type=&dateb=&owner=include&start=',
           length_actual, '&count=100') %>%
      purrr::reduce(paste0)

    df_filing_urls <-
      data_frame(nameSearch = search_term, urlCIKPageFiling = urls) %>%
      mutate(countPage = 1:n())
    if('dfEnd' %>% exists()){
      rm(list = c('dfEnd'), pos = ".GlobalEnv")
    }

    if ('is_on' %>% exists()) {
      rm(list = c('is_on'), pos = ".GlobalEnv")
    }
    return(df_filing_urls)
  }

get_entity_ciks <-
  function(search_term = "BREA", return_message = TRUE) {
    url_df <-
      parse_search_page_length(search_term = search_term)

    data <-
      url_df$urlCIKPageFiling %>%
      map_df(function(x) {
        parse_search_page(url = x, return_message = FALSE)
      }) %>%
      mutate(nameSearch = search_term) %>%
      select(nameSearch, everything())

    if (return_message) {
      list("Returned ", nrow(data) %>% formattable::comma(digits = 0),
           ' SEC registered entities for the term ', search_term) %>%
        purrr::reduce(paste0) %>%
        message()
    }

    return(data)
  }

#' SEC registered entity search
#'
#' @param search_names
#' @param nest_data
#' @param return_message
#'
#' @return
#' @export
#' @import dplyr tidyr purrr stringr formattable readr lubridate XBRL curl jsonlite lazyeval
#' @importFrom jsonlite fromJSON
#' @examples
get_data_edgar_entities_cik <-
  function(search_names = c("Rockwood", "BREA", 'EJF'),
           nest_data = FALSE,
           return_message = TRUE) {

    get_data_sec_entity_safe <-
      purrr::possibly(get_entity_ciks, data_frame())

    all_data <-
      search_names %>%
      map_df(function(x) {
        get_data_sec_entity_safe(search_term = x, return_message = return_message)
      }) %>%
      select(which(colMeans(is.na(.)) < 1)) %>%
      mutate(nameEntity = nameEntityLegal %>% str_to_upper() %>% str_replace_all('\\.|\\,', '') %>% str_trim())

    all_data <-
      all_data %>%
      select(-matches("idLocationEntity")) %>%
      separate(nameEntity,
               sep = '\\ /',
               into = c('nameEntity', 'idLocationEntity')) %>%
      mutate(idLocationEntity = idLocationEntity %>% str_replace_all('\\/', '') %>% str_trim()) %>%
      select(nameSearch, idCIK, nameEntity, everything()) %>%
      suppressMessages() %>%
      suppressWarnings()

    closeAllConnections()

    all_data <-
      all_data %>%
      separate(nameEntity, sep = 'FORMERLY: ', c('nameEntity', 'nameEntityFormer')) %>%
      suppressMessages() %>%
      suppressWarnings()

    if (nest_data) {
      all_data <-
        all_data %>%
        nest(-c(nameSearch), .key = dataSearch)
    }

    return(all_data)
  }



# page_guess --------------------------------------------------------------
get_sec_filer_name_page_df <-
  function(){
    data_frame(
      nameSEC = c("dateFiled", "filingHREF", "formName", "type", "XBRLREF"),
      nameActual = c(
        "dateFiling",
        "urlSECFilingDirectory",
        "nameForm",
        "idForm",
        "urlXBRL"
      )
    )
  }

guess_page_ongoing <-
  function(url = "https://www.sec.gov/cgi-bin/browse-edgar?action=getcompany&CIK=1184765&type=&dateb=&owner=include&start=0&count=100",
           override = FALSE) {
    page <-
      url %>%
      read_html()

    page_count <-
      url %>% str_split('start=') %>%
      flatten_chr() %>%
      .[[2]] %>%
      str_split('&') %>%
      flatten_chr() %>%
      .[[1]] %>%
      readr::parse_number()

    items <-
      page %>%
      html_nodes('input') %>%
      html_attr('value') %>% str_to_upper() %>%
      unique()

    if (items %>% length() == 0){
      return(invisible())
    }

    no_page <-
      page %>%
      html_nodes('h1') %>%
      html_text() %>%
      str_to_lower() == 'invalid parameter'

    no_page <-
      no_page %>%
      length() > 0
    is_end <-
      !items %>% str_detect("NEXT 100") %>% sum(na.rm = T) > 0

    if (is_end & (!no_page)) {
      return(data_frame(isEnd = TRUE, countStart = page_count))
    }
    if (!override) {
      if (!is_end) {
        return(data_frame())
      }
    } else {
      return(data_frame(countStart = page_count))
    }
    data_frame(isEnd = is_end, countPage = page_count -100)
  }


# FIler Parsing -----------------------------------------------------------


get_cik_filer_page_urls <-
  function(cik = 1184765, pages_out = 20) {
    start_pages <-
      seq(0, by = 100, length.out = pages_out)
    if ('dfEnd' %>% exists()) {
      eval(rm(dfEnd))
    }
    urls <-
      list('https://www.sec.gov/cgi-bin/browse-edgar?action=getcompany&CIK=',cik, '&type=&dateb=&owner=include&start=',
           start_pages, '&count=100') %>%
      purrr::reduce(paste0)

    is_on <-
      TRUE
    for (url in urls) {
      if (!is_on) {
        invisible()
      }
      if('dfEnd' %>% exists()) {
        invisible()
      } else {
        df_end <-
          guess_page_ongoing(url = url, override = FALSE)
        is_over_zero <-
          df_end %>% length() > 0
        if (is_over_zero) {
          assign('dfEnd', eval(df_end), envir = .GlobalEnv)
          assign('is_on', eval(FALSE), envir = .GlobalEnv)
          rm(is_over_zero)
        }
      }
    }
    still_none <-
      df_end %>% length() == 0
    if (still_none)  {
      df_end <-
        urls %>%
        map_df(function(x){
          guess_page_ongoing(url = x, override = TRUE)
        })
      df_end <-
        df_end %>%
        slice(nrow(df_end))
    }
    length_actual_pages <-
      ceiling(df_end$countStart/100)
    length_actual <-
      seq(0, by = 100, length.out =  length_actual_pages)
    urls <-
      list('https://www.sec.gov/cgi-bin/browse-edgar?action=getcompany&CIK=',cik, '&type=&dateb=&owner=include&start=',
           length_actual, '&count=100', '&output=xml') %>%
      purrr::reduce(paste0)

    df_filing_urls <-
      data_frame(idCIK = cik, urlCIKPageFiling = urls) %>%
      mutate(countPage = 1:n())
    if('dfEnd' %>% exists()){
      rm(list = c('dfEnd'), pos = ".GlobalEnv")
    }
    return(df_filing_urls)
  }

parse_cik_filer_page <-
  function(url = "https://www.sec.gov/cgi-bin/browse-edgar?action=getcompany&CIK=899689&type=&dateb=&owner=include&start=600&count=100&output=xml",
           return_message = TRUE) {
    page <-
      url %>%
      read_xml()

    xml_nodes <-
      page %>%
      xml_contents() %>%
      .[[2]]

    filing_count <-
      xml_nodes %>%
      xml_contents() %>%
      xml_name()

    df_page_items <-
      1:length(filing_count) %>%
      map_df(function(x) {
        xml_node <-
          xml_nodes %>%
          xml_contents() %>%
          .[[x]]

        items <-
          xml_node %>%
          xml_children() %>%
          xml_name()

        values <-
          xml_node %>%
          xml_children() %>%
          xml_text()

        return(data_frame(
          countPageFiling = x,
          nameSEC = items,
          value = values
        ))
      }) %>%
      left_join(get_sec_filer_name_page_df()) %>%
      suppressWarnings() %>%
      suppressMessages() %>%
      select(-nameSEC)

    df_page_items <-
      df_page_items %>%
      spread(nameActual, value) %>%
      resolve_form_columns() %>%
      mutate(urlCIKPageFiling = url)

    df_page_items <-
      df_page_items %>%
      find_target_filings()

    if (return_message) {
      list("Parsed: ", url) %>%
        purrr::invoke(paste0, .) %>% message()
    }
    closeAllConnections()
    return(df_page_items)
  }
get_df_general_name_df <-
  function() {
    data_frame(nameSEC = c("CIK", "CIKHREF", "Location", "SIC", "SICDescription", "SICHREF",
                           "businessAddresscity", "businessAddressphoneNumber", "businessAddressstate",
                           "businessAddressstreet", "businessAddresszipCode", "fiscalYearEnd",
                           "mailingAddresscity", "mailingAddressstate", "mailingAddressstreet",
                           "mailingAddresszipCode", "name", "stateOfIncorporation", "businessAddressstreet2", "mailingAddressstreet2", 'formerNames',
                           'businessAddress', 'formerNamedate', 'formerNamename',
                           'mailingAddress'),
               nameActual = c("idCIK", "urlCIKFiling", "locationEntity", "idSICEntity", "nameIndustry", "urlSICMembers",
                              "cityAddressBusiness", "phoneAddressBusiness", "stateAddressBusiness",
                              "addressStreet1Business", "zipcodeBusiness", "periodFiscalYearEnd",
                              "cityAddressMailing", "stateAddressMailing", "addressStreet1Mailing",
                              "zipcodeMailing", "nameEntity", "stateIncorporation",
                              "addressStreet2Mailing", "addressStreet2Business", 'nameEntity',
                              'addressBusiness', 'dateFormerName', 'nameEntity',
                              'addressMailing')

    )
  }

parse_cik_filer_general_info <-
  function(url = "https://www.sec.gov/cgi-bin/browse-edgar?action=getcompany&CIK=1326801&type=&dateb=&owner=include&start=0&count=100&output=xml") {
    page <-
      url %>%
      read_xml()

    xml_nodes <-
      page %>%
      xml_contents() %>%
      .[[1]]

    items <-
      xml_nodes %>%
      xml_children() %>%
      xml_name()

    df_names <-
      get_df_general_name_df()

    df_general <-
      1:length(items) %>%
      map_df(function(x){

        xml_node <-
          xml_nodes %>%
          xml_contents() %>%
          .[[x]]

        item <-
          items[[x]]
        xml_search <-
          list('//', item, '/value') %>%
          purrr::reduce(paste0)

        value <-
          xml_node %>%
          xml_find_all(xml_search) %>%
          xml_text()
        no_val <-
          value %>% length() == 0

        if (item == 'formerNames') {
          value_search <-
            list('//', item) %>% purrr::reduce(paste0)

          item_parent <-
            xml_node %>%
            xml_find_all(value_search) %>%
            xml_children() %>%
            xml_name()

          items <-
            xml_node %>%
            xml_find_all(value_search) %>%
            xml_children() %>%
            xml_children() %>%
            xml_name()

          values <-
            xml_node %>%
            xml_find_all(value_search) %>%
            xml_children() %>%
            xml_children() %>%
            xml_text()

          df <-
            data_frame(
              countItem = x,
              itemParent = item_parent[1:length(item)],
              nameSEC = items,
              value = values
            ) %>%
            unite(nameSEC, itemParent, nameSEC, sep = '')
          return(df)
        }

        if (item == 'name') {
          value_search <-
            list('//', item) %>% purrr::reduce(paste0)
          items <-
            xml_node %>%
            xml_find_all(value_search) %>%
            xml_name()

          items <-
            items[length(items)]

          values <-
            xml_node %>%
            xml_find_all(value_search) %>%
            xml_text()

          values <-
            values[length(values)]

          df <-
            data_frame(
              countItem = x,
              nameSEC = items,
              value = values
            )
          return(df)
        }

        if (no_val) {
          value_search <-
            list('//', item) %>% purrr::reduce(paste0)
          has_children <-
            xml_node %>%
            xml_find_all(value_search) %>% xml_children() %>% xml_length() %>% length() > 1
          if (has_children) {
            has_more_children <-
              xml_node %>%
              xml_find_all(value_search) %>%
              xml_children() %>%
              xml_children() %>% xml_length() %>% length() > 1

            if (has_more_children) {
              item_parent <-
                xml_node %>%
                xml_find_all(value_search) %>%
                xml_children() %>%
                xml_name()

              items <-
                xml_node %>%
                xml_find_all(value_search) %>%
                xml_children() %>%
                xml_children() %>%
                xml_name()

              values <-
                xml_node %>%
                xml_find_all(value_search) %>%
                xml_children() %>%
                xml_children() %>%
                xml_text()

              df <-
                data_frame(
                  countItem = x,
                  itemParent = item_parent[1:length(item)],
                  nameSEC = items,
                  value = values
                ) %>%
                unite(nameSEC, itemParent, nameSEC, sep = '')
              return(df)
            }

            item_parent <-
              xml_node %>%
              xml_find_all(value_search) %>%
              xml_name()

            item <-
              xml_node %>%
              xml_find_all(value_search) %>%
              xml_children() %>%
              xml_name()

            values <-
              xml_node %>%
              xml_find_all(value_search) %>%
              xml_children() %>%
              xml_text()

            df <-
              data_frame(
                countItem = x,
                itemParent = item_parent,
                nameSEC = item,
                value = values
              ) %>%
              unite(nameSEC, itemParent, nameSEC, sep = '')
            return(df)
          }

          if (!has_children) {
            values <-
              xml_node %>%
              xml_find_all(value_search) %>%
              xml_text()
          }
          df <-
            data_frame(
              countItem = x,
              nameSEC = item,
              value = values
            )
          return(df)
        }
        nameSEC <-
          xml_node %>%
          xml_find_all(list('//', item) %>% purrr::reduce(paste0)) %>%
          xml_name()
        data_frame(idRow = countRow, nameSEC, value = value)
      })

    df_general <-
      df_general %>%
      left_join(df_names) %>%
      suppressMessages()

    missing_names <-
      df_general$nameSEC[!df_general$nameSEC %in% df_names$nameSEC] %>%
      length() >0

    if (missing_names) {
      missing_n <-
        df_general$nameSEC[!df_general$nameSEC %in% df_names$nameSEC]
      stop(list("Missing ", missing_n) %>%
             purrr::reduce(paste0))
    }

    df_general <-
      df_general %>%
      select(-c(nameSEC,countItem)) %>%
      group_by(nameActual) %>%
      mutate(idRow = 1:n()) %>%
      filter(idRow == min(idRow)) %>%
      ungroup() %>%
      suppressMessages()

    col_order <-
      df_general$nameActual

    df_general <-
      df_general %>%
      spread(nameActual, value) %>%
      select(one_of(col_order)) %>%
      dplyr::rename(nameEntityLegal = nameEntity) %>%
      mutate(nameEntity = nameEntityLegal %>% str_to_upper() %>% str_replace_all('\\.|\\,', '') %>% str_trim()) %>%
      separate(nameEntity,
               sep = '\\ /',
               into = c('nameEntity', 'idLocationEntity')) %>%
      mutate(
        idLocationEntity = idLocationEntity %>% str_replace_all('\\/', '') %>% str_trim()
      ) %>%
      select(nameEntity, everything()) %>%
      suppressWarnings() %>%
      suppressMessages()

    return(df_general)
  }


get_cik_filer_filings <-
  function(cik = 899689) {
    df_urls <-
      get_cik_filer_page_urls(cik = cik) %>%
      suppressWarnings() %>%
      suppressMessages()

    parse_cik_filer_page_safe <-
      purrr::possibly(parse_cik_filer_page, data_frame())

    df_filings <-
      df_urls$urlCIKPageFiling %>%
      map_df(function(x) {
        parse_cik_filer_page_safe(url = x)
      }) %>%
      mutate(idCIK = cik) %>%
      select(idCIK, everything())

    df_general <-
      df_urls$urlCIKPageFiling[[1]] %>%
      parse_cik_filer_general_info() %>%
      mutate_all(funs(ifelse(. == '', NA, .))) %>%
      resolve_form_columns() %>%
      select(which(colMeans(is.na(.)) < 1))

    df_filings <-
      df_filings %>%
      left_join(df_general %>% select(idCIK, nameEntity)) %>%
      suppressMessages()

    df_filings <-
      df_filings %>%
      select(-countPageFiling) %>%
      arrange(dateFiling) %>%
      mutate(countFilingEntity = 1:n()) %>%
      arrange(desc(dateFiling)) %>%
      select(countFilingEntity, idCIK, nameEntity, everything())

    if ('urlXBRL' %in% names(df_filings)) {
      df_filings <-
        df_filings %>%
        mutate(hasXBRL = ifelse(!urlXBRL %>% is.na(), TRUE, FALSE))
    }

    return(df_filings)
  }

# SIC Search --------------------------------------------------------------


get_sic_filer_page_urls <-
  function(sic = 6798, pages_out = 20) {
    start_pages <-
      seq(0, by = 100, length.out = pages_out)
    if ('dfEnd' %>% exists()) {
      eval(rm(dfEnd))
    }

    urls <-
      list('https://www.sec.gov/cgi-bin/browse-edgar?action=getcompany&SIC=',sic, '&type=&dateb=&owner=include&start=',
           start_pages, '&count=100') %>%
      purrr::reduce(paste0)

    is_on <-
      TRUE
    for (url in urls) {
      if (!is_on) {
        invisible()
      }
      if('dfEnd' %>% exists()) {
        invisible()
      } else {
        df_end <-
          guess_page_ongoing(url = url, override = FALSE)
        is_over_zero <-
          df_end %>% length() > 0
        if (is_over_zero) {
          assign('dfEnd', eval(df_end), envir = .GlobalEnv)
          assign('is_on', eval(FALSE), envir = .GlobalEnv)
          rm(is_over_zero)
        }
      }
    }
    still_none <-
      df_end %>% length() == 0
    if (still_none)  {
      df_end <-
        data_frame(countStart = 0)
    }
    length_actual_pages <-
      ceiling(df_end$countStart/100)
    length_actual <-
      seq(0, by = 100, length.out =  length_actual_pages)
    urls <-
      list('https://www.sec.gov/cgi-bin/browse-edgar?action=getcompany&SIC=',sic, '&type=&dateb=&owner=include&start=',
           length_actual, '&count=100', '&output=xml') %>%
      purrr::reduce(paste0)

    df_sic_urls <-
      data_frame(idSIC = sic, urlSICPageFiling = urls) %>%
      mutate(countPage = 1:n())
    if('dfEnd' %>% exists()){
      rm(list = c('dfEnd'), pos = ".GlobalEnv")
    }
    return(df_sic_urls)
  }

get_data_sic_code_filer <-
  function(sic = 6798,
           return_message = TRUE) {

    get_sic_filer_page_urls_safe <-
      purrr::possibly(get_sic_filer_page_urls, data_frame())

    url_df <-
      get_sic_filer_page_urls_safe(sic = sic)

    parse_search_page_safe <-
      purrr::possibly(parse_search_page, data_frame())

    all_data <-
      url_df$urlSICPageFiling %>%
      map_df(function(x){
        parse_search_page_safe(url = x, return_message = return_message)
      }) %>%
      mutate(idSIC = sic) %>%
      select(idSIC, everything())

    closeAllConnections()

    if (return_message) {
      list('\nReturned ', all_data %>% nrow() %>% formattable::comma(digits = 0),
           ' SEC registered entities for SIC industry code ', sic,'\n') %>%
        purrr::reduce(paste0) %>%
        message()
    }
    return(all_data)
  }

#' SIC Cod Companies
#'
#' @param sic_codes
#' @param merge_names
#' @param return_message
#' @param nest_data
#'
#' @return
#' @export
#' @import dplyr tidyr purrr stringr formattable readr lubridate XBRL curl jsonlite lazyeval
#' @importFrom jsonlite fromJSON
#' @examples
get_data_edgar_sic_filers <-
  function(sic_codes = c(3949, 3690, 3711),
           merge_names = TRUE,
           return_message = TRUE,
           nest_data = FALSE) {

    if (sic_codes %>% purrr::is_null()) {
      stop("Please enter SIC codes to search")
    }

    get_data_sic_code_filer_safe <-
      purrr::possibly(get_data_sic_code_filer, data_frame())

    all_data <-
      sic_codes %>%
      map_df(function(x){
        get_data_sic_code_filer_safe(sic = x, return_message = return_message)
      })

    if (merge_names) {
      if (!'dataSICCodes' %>% exists()) {
        assign(x = 'dataSICCodes', value = eval(get_dictionary_sic_codes()),
               envir = .GlobalEnv)
      }

      all_data <-
        all_data %>%
        left_join(
          dataSICCodes %>% select(-idADOffice)
        ) %>%
        select(idSIC, nameIndustry, everything()) %>%
        suppressMessages()
    }
    return(all_data)
  }

# SEC - Subsidiary --------------------------------------------------------

parse_sec_url_for_cik <-
  function(url) {
    url %>%
      str_replace_all("https://www.sec.gov/Archives/edgar/data/", '') %>%
      str_split('\\/') %>%
      flatten_chr() %>%
      .[[1]] %>%
      as.numeric()
  }

get_loc_df <-
  function() {
    data_frame(
      nameLocation = c(
        "AFGHANISTAN",
        "ALAND ISLANDS",
        "ALBANIA",
        "ALGERIA",
        "AMERICAN SAMOA",
        "ANDORRA",
        "ANGOLA",
        "ANGUILLA",
        "ANTARCTICA",
        "ANTIGUA AND BARBUDA",
        "ARGENTINA",
        "ARMENIA",
        "ARUBA",
        "AUSTRALIA",
        "AUSTRIA",
        "AUSTRIA-HUNGARY",
        "AZERBAIJAN",
        "BADEN",
        "BAHAMAS",
        "BAHRAIN",
        "BANGLADESH",
        "BARBADOS",
        "BAVARIA",
        "BELARUS",
        "BELGIUM",
        "BELIZE",
        "BENIN",
        "BERMUDA",
        "BHUTAN",
        "BOLIVIA, PLURINATIONAL STATE OF",
        "BONAIRE, SINT EUSTATIUS AND SABA",
        "BOSNIA AND HERZEGOVINA",
        "BOTSWANA",
        "BOUVET ISLAND",
        "BRAZIL",
        "BRITISH INDIAN OCEAN TERRITORY",
        "BRUNEI DARUSSALAM",
        "BULGARIA",
        "BURKINA FASO",
        "BURUNDI",
        "CAMBODIA",
        "CAMEROON",
        "CANADA",
        "CABO VERDE",
        "CAYMAN ISLANDS",
        "CENTRAL AFRICAN REPUBLIC",
        "CHAD",
        "CHILE",
        "CHINA",
        "CHRISTMAS ISLAND",
        "COCOS (KEELING) ISLANDS",
        "COLOMBIA",
        "COMOROS",
        "CONGO, THE DEMOCRATIC REPUBLIC OF THE",
        "CONGO",
        "COOK ISLANDS",
        "COSTA RICA",
        "COTE D'IVOIRE",
        "CROATIA",
        "CUBA",
        "CURACAO",
        "CYPRUS",
        "CZECH REPUBLIC",
        "CZECHOSLOVAKIA",
        "DENMARK",
        "DJIBOUTI",
        "DOMINICA",
        "DOMINICAN REPUBLIC",
        "ECUADOR",
        "EGYPT",
        "EL SALVADOR",
        "EQUATORIAL GUINEA",
        "ERITREA",
        "ESTONIA",
        "ETHIOPIA",
        "FALKLAND ISLANDS (MALVINAS)",
        "FAROE ISLANDS",
        "FIJI",
        "FINLAND",
        "FRANCE",
        "FRENCH GUIANA",
        "FRENCH POLYNESIA",
        "FRENCH SOUTHERN TERRITORIES",
        "GABON",
        "GAMBIA",
        "GEORGIA",
        "GERMAN DEMOCRATIC REPUBLIC",
        "FEDERAL REPUBLIC OF GERMANY",
        "GERMANY",
        "GHANA",
        "GIBRALTAR",
        "GREECE",
        "GREENLAND",
        "GRENADA",
        "GUADELOUPE",
        "GUAM",
        "GUATEMALA",
        "GUERNSEY",
        "GUINEA",
        "GUINEA-BISSAU",
        "GUYANA",
        "HAITI",
        "HANOVER",
        "HEARD ISLAND AND MCDONALD ISLANDS",
        "HESSE ELECTORAL",
        "HESSE GRAND DUCAL",
        "HOLY SEE (VATICAN CITY STATE)",
        "HONDURAS",
        "HONG KONG",
        "HUNGARY",
        "ICELAND",
        "INDIA",
        "INDONESIA",
        "IRAN, ISLAMIC REPUBLIC OF",
        "IRAQ",
        "IRELAND",
        "ISLE OF MAN",
        "ISRAEL",
        "ITALY",
        "JAMAICA",
        "JAPAN",
        "JERSEY",
        "JORDAN",
        "KAZAKHSTAN",
        "KENYA",
        "KIRIBATI",
        "KOREA",
        "KOREA, DEMOCRATIC PEOPLE'S REPUBLIC OF",
        "KOREA, REPUBLIC OF",
        "KOSOVO",
        "KUWAIT",
        "KYRGYZSTAN",
        "LAO PEOPLE'S DEMOCRATIC REPUBLIC",
        "LATVIA",
        "LEBANON",
        "LESOTHO",
        "LIBERIA",
        "LIBYA",
        "LIECHTENSTEIN",
        "LITHUANIA",
        "LUXEMBOURG",
        "MACAO",
        "MACEDONIA, THE FORMER YUGOSLAV REPUBLIC OF",
        "MADAGASCAR",
        "MALAWI",
        "MALAYSIA",
        "MALDIVES",
        "MALI",
        "MALTA",
        "MARSHALL ISLANDS",
        "MARTINIQUE",
        "MAURITANIA",
        "MAURITIUS",
        "MAYOTTE",
        "MECKLENBURG SCHWERIN",
        "MEXICO",
        "MICRONESIA, FEDERATED STATES OF",
        "MODENA",
        "MOLDOVA, REPUBLIC OF",
        "MONACO",
        "MONGOLIA",
        "MONTENEGRO",
        "MONTSERRAT",
        "MOROCCO",
        "MOZAMBIQUE",
        "MYANMAR",
        "NAMIBIA",
        "NAURU",
        "NEPAL",
        "NETHERLANDS",
        "NETHERLANDS ANTILLES",
        "NEW CALEDONIA",
        "NEW ZEALAND",
        "NICARAGUA",
        "NIGER",
        "NIGERIA",
        "NIUE",
        "NORFOLK ISLAND",
        "NORTHERN MARIANA ISLANDS",
        "NORWAY",
        "OMAN",
        "PAKISTAN",
        "PALAU",
        "PALESTINE, STATE OF",
        "PANAMA",
        "PAPUA NEW GUINEA",
        "PARAGUAY",
        "PARMA",
        "PERU",
        "PHILIPPINES",
        "PITCAIRN",
        "POLAND",
        "PORTUGAL",
        "PUERTO RICO",
        "QATAR",
        "REPUBLIC OF VIETNAM",
        "REUNION",
        "ROMANIA",
        "RUSSIAN FEDERATION",
        "RWANDA",
        "SAINT BARTHELEMY",
        "SAINT HELENA, ASCENSION AND TRISTAN DA CUNHA",
        "SAINT KITTS AND NEVIS",
        "SAINT LUCIA",
        "SAINT MARTIN (FRENCH PART)",
        "SAINT PIERRE AND MIQUELON",
        "SAINT VINCENT AND THE GRENADINES",
        "SAMOA",
        "SAN MARINO",
        "SAO TOME AND PRINCIPE",
        "SAUDI ARABIA",
        "SAXONY",
        "SENEGAL",
        "SERBIA",
        "SEYCHELLES",
        "SIERRA LEONE",
        "SINGAPORE",
        "SINT MAARTEN (DUTCH PART)",
        "SLOVAKIA",
        "SLOVENIA",
        "SOLOMON ISLANDS",
        "SOMALIA",
        "SOUTH AFRICA",
        "SOUTH GEORGIA AND THE SOUTH SANDWICH ISLANDS",
        "SOUTH SUDAN",
        "SPAIN",
        "SRI LANKA",
        "SUDAN",
        "SURINAME",
        "SVALBARD AND JAN MAYEN",
        "SWAZILAND",
        "SWEDEN",
        "SWITZERLAND",
        "SYRIAN ARAB REPUBLIC",
        "TAIWAN, PROVINCE OF CHINA",
        "TAJIKISTAN",
        "TANZANIA, UNITED REPUBLIC OF",
        "THAILAND",
        "TIMOR-LESTE",
        "TOGO",
        "TOKELAU",
        "TONGA",
        "TRINIDAD AND TOBAGO",
        "TUNISIA",
        "TURKEY",
        "TURKMENISTAN",
        "TURKS AND CAICOS ISLANDS",
        "TUSCANY",
        "TUVALU",
        "TWO SICILIES",
        "UGANDA",
        "UKRAINE",
        "UNITED ARAB EMIRATES",
        "UNITED KINGDOM",
        "UNITED STATES",
        "UNITED STATES MINOR OUTLYING ISLANDS",
        "URUGUAY",
        "UZBEKISTAN",
        "VANUATU",
        "VENEZUELA, BOLIVARIAN REPUBLIC OF",
        "VIET NAM",
        "VIRGIN ISLANDS, BRITISH",
        "VIRGIN ISLANDS, U.S.",
        "WALLIS AND FUTUNA",
        "WESTERN SAHARA",
        "WUERTTEMBURG",
        "YEMEN",
        "YEMEN ARAB REPUBLIC",
        "YEMEN PEOPLE'S REPUBLIC",
        "YUGOSLAVIA",
        "ZAMBIA",
        "ZANZIBAR",
        "ZIMBABWE",
        "ALABAMA",
        "ALASKA",
        "ARIZONA",
        "ARKANSAS",
        "CALIFORNIA",
        "COLORADO",
        "CONNECTICUT",
        "DELAWARE",
        "FLORIDA",
        "GEORGIA",
        "HAWAII",
        "IDAHO",
        "ILLINOIS",
        "INDIANA",
        "IOWA",
        "KANSAS",
        "KENTUCKY",
        "LOUISIANA",
        "MAINE",
        "MARYLAND",
        "MASSACHUSETTS",
        "MICHIGAN",
        "MINNESOTA",
        "MISSISSIPPI",
        "MISSOURI",
        "MONTANA",
        "NEBRASKA",
        "NEVADA",
        "NEW HAMPSHIRE",
        "NEW JERSEY",
        "NEW MEXICO",
        "NEW YORK",
        "NORTH CAROLINA",
        "NORTH DAKOTA",
        "OHIO",
        "OKLAHOMA",
        "OREGON",
        "PENNSYLVANIA",
        "RHODE ISLAND",
        "SOUTH CAROLINA",
        "SOUTH DAKOTA",
        "TENNESSEE",
        "TEXAS",
        "UTAH",
        "VERMONT",
        "VIRGINIA",
        "WASHINGTON",
        "WEST VIRGINIA",
        "WISCONSIN",
        "WYOMING",
        "DISTRICT OF COLUMBIA",
        "ENGLAND",
        "BRITISH VIRGIN ISLANDS",
        "NETHERLAND ANTILLES",
        "RUSSIA",
        "SOUTH KOREA",
        'TAIWAN',
        "VENEZUELA",
        'CHANNEL ISLANDS'
      )
    )
  }

parse_page_sub_multi_item_html <-
  function(page) {
    locations <-
      get_loc_df() %>%
      .$nameLocation
    subsidiaries <-
      page %>%
      html_nodes('td div') %>%
      html_text() %>%
      str_replace_all('\u0095 |\u0096|\u0095\n', '') %>%
      str_trim()

    subsidiaries <-
      subsidiaries[!subsidiaries == '']

    data_nodes <-
      page %>%
      html_nodes('td') %>%
      html_text() %>%
      str_replace_all('\u0095 |\u0096|\u0095\n', '') %>%
      str_trim() %>%
      str_to_upper()

    data_nodes <-
      data_nodes[!data_nodes == '']

    location_items <-
      data_nodes[data_nodes %in% locations]

    pct_vals <-
      data_frame(value = data_nodes) %>%
      filter(!value %>% str_detect("\\([(1-9)]\\)")) %>%
      mutate(pctSubsidiaryOwned = value %>% as.numeric()) %>%
      filter(!pctSubsidiaryOwned %>% is.na()) %>%
      slice(1:length(subsidiaries)) %>%
      .$pctSubsidiaryOwned / 100 %>%
      suppressWarnings() %>%
      suppressMessages()

    all_data <-
      data_frame(
        nameSubsidiary = subsidiaries,
        nameLocationSubsidiary = location_items,
        pctSubsidiaryOwned = pct_vals
      ) %>%
      mutate(nameSubsidiary = nameSubsidiary %>% str_to_upper())

    return(all_data)
  }

parse_page_subsidiary_table_html <-
  function(page,
           numbers = 1:10,
           hit_terms = c(
             "Organized",
             "STATE OR|STATE OF|JURISDICTION OF|JURISDICTION OF INCORPORATION OR ORGANIZATION|JURISDICTION|JURISDICTION OF INCORPORATION OR\nORGANIZATION",
             "NAME|ORGANIZED UNDER THE LAWS OF",
             'STATE OF ORGANIZATION',
             'STATE OR COUNTRY OF ORGANIZATION',
             'NAME OF SUBSIDIARY',
             'NAME',
             'ENTITY NAME',
             'the laws of',
             'Percentage of voting',
             'securities owned by',
             'immediate parent',
             'CERTAIN INTERMEDIARY SUBSIDIARIES',
             'Note:',
             'Organized',
             'Under the',
             'Laws of',
             'OWNED BY',
             'IMMEDIATE',
             'PARENT',
             "OWNS",
             "CERTAIN INTERMEDIARY SUBSIDIARIES",
             'PERCENTAGE',
             'OF VOTING',
             'SECURITIES'
           )) {
    is_ib1 <-
      page %>%
      html_nodes('b font') %>%
      html_text() %>% length() > 0

    if (is_ib1) {
      items_bold <-
        page %>%
        html_nodes('b font') %>%
        html_text() %>%
        str_to_upper() %>%
        str_replace_all('\n', ' ') %>%
        str_split('\\–') %>%
        flatten_chr() %>%
        str_trim()
    } else {
      items_bold <-
        page %>%
        html_nodes('b') %>%
        html_text() %>%
        str_to_upper() %>%
        str_replace_all('\n', ' ') %>%
        str_split('\\–') %>%
        flatten_chr() %>%
        str_trim() %>%
        unique()
    }

    has_date <-
      items_bold %>% grep(month.name %>% str_to_upper() %>% paste(collapse = '|'), .) %>% length > 0

    if (has_date) {
      date_data <-
        items_bold[items_bold %>% grep(month.name %>% str_to_upper() %>% paste(collapse = '|'), .)] %>%
        lubridate::mdy()
    } else {
      date_data <-
        NA
    }

    hit_terms <-
      hit_terms %>%
      append(items_bold) %>%
      str_to_upper() %>%
      unique() %>%
      append(list('(', letters, ')') %>%
               purrr::invoke(paste0, .)) %>%
      paste0(collapse = '|')


    hit_terms_in <-
      hit_terms %>% str_split('\\|') %>%
      flatten_chr()

    locations <-
      get_loc_df() %>%
      .$nameLocation

    all_data <-
      numbers %>%
      map_df(function(x) {
        css_selector <-
          paste0('td:nth-child(', x, ')')
        has_length <-
          page %>%
          html_nodes(css_selector) %>% length() > 0
        if (has_length) {
          item <-
            paste0("X" , x)

          value <-
            page %>%
            html_nodes(css_selector) %>%
            html_text() %>%
            str_trim()
          data_frame(item, value)
        }
      }) %>%
      mutate(
        value = value %>% str_to_upper() %>% str_replace_all('\n  ', ' ') %>% str_replace_all('\u0096 ', '')
      ) %>%
      filter(!value == '')

    has_loc_key <-
      all_data %>%
      filter(value %in% locations) %>%
      nrow() > 0

    if (has_loc_key) {
      loc_cols <-
        all_data %>%
        filter(value %in% locations) %>%
        .$item %>%
        unique()
      if (loc_cols %>% length == 1) {
        loc_col <-
          loc_cols[[1]]
      }
    }

    has_pct <-
      all_data %>%
      filter(value %>% str_detect("PERCENT")) %>%
      .$item %>% unique() %>% length() > 0

    if (has_pct) {
      pct_col <-
        all_data %>%
        filter(value %>% str_detect("PERCENT")) %>%
        .$item %>% unique()
    } else {
      pct_col <-
        NA
    }

    is_whack <-
      pct_col[[1]] %in% loc_cols

    if (is_whack) {
      all_data <-
        page %>%
        parse_page_sub_multi_item_html() %>%
        mutate(dateSubsidiaryAsOf = date_data)

      return(all_data)
    }

    all_data <-
      all_data %>%
      filter(!value %in% items_bold) %>%
      filter(!value %>% str_detect(paste0(items_bold %>% unique(), collapse = '|'))) %>%
      filter(!value %in% hit_terms_in) %>%
      filter(!value %>% str_detect(hit_terms))

    count_df <-
      all_data %>% count(item, sort = T) %>%
      arrange(item) %>%
      spread(item, n)

    off_one <-
      (count_df[, 2] %>% extract2(1)) - (count_df[, 1] %>% extract2(1)) == 1

    min_item <-
      count_df %>% gather(item, value) %>% filter(value == min(value)) %>% .$item

    change_pct <-
      has_pct & (pct_col == min_item) %>% sum() > 0

    if (change_pct) {
      pct_col <-
        names(count_df)[[3]]
    }

    if (off_one) {
      df <-
        all_data$item %>% unique() %>%
        map_df(function(x) {
          has_data <-
            all_data %>%
            filter(item == x) %>%
            filter(!value %>% is.na()) %>%
            filter(!value == '') %>%
            nrow()

          if (has_data) {
            all_data %>%
              filter(item == x) %>%
              filter(!value %>% is.na()) %>%
              filter(!value == '') %>%
              filter(!value %>% str_detect(hit_terms)) %>%
              mutate(idSubsidiary = 1:n())
          }
        }) %>%
        filter(!value %>% str_detect(hit_terms)) %>%
        spread(item, value)

      if (change_pct) {
        df <-
          df %>%
          select(-one_of(min_item))
      }
    }

    if (!off_one) {
      has_property <-
        items_bold %>% str_detect('PROPERTY') %>% sum() > 0
      if (has_property) {
        tables <-
          page %>%
          html_table(fill = T)
        df <-
          1:length(tables) %>%
          map_df(function(x) {
            table_df <-
              tables[[x]] %>%
              data.frame(stringsAsFactors = FALSE) %>%
              as_data_frame()

            column_df <-
              table_df %>% slice(1) %>%
              gather(column, value) %>%
              mutate(idColumn = 1:n()) %>%
              filter(!value %>% is.na()) %>%
              left_join(data_frame(
                value = c(
                  "PROPERTY",
                  "ENTITIES",
                  "STATE OF FORMATION",
                  "DATE OF FORMATION",
                  " ",
                  'General Information:'
                ),
                nameItem = c(
                  'nameProperty',
                  'nameSubsidiary',
                  'locationOrganizationSubsidiary',
                  'dateSubsidiaryFormed',
                  'locationOrganizationSubsidiary',
                  'nameSubsidiary'
                )
              )) %>%
              suppressMessages()
            two_col <-
              column_df %>% nrow() == 2
            if (two_col) {
              column_df$nameItem[[2]] <-
                'locationOrganizationSubsidiary'
            }

            columns_keep <-
              column_df$idColumn

            table_df <-
              table_df <-
              table_df %>%
              select(columns_keep) %>%
              slice(-1) %>%
              purrr::set_names(column_df$nameItem)

            table_df <-
              table_df %>%
              mutate_all(funs(. %>% str_trim() %>% str_to_upper())) %>%
              mutate(nameSubsidiary = ifelse(nameSubsidiary == '', NA, nameSubsidiary)) %>%
              filter(!nameSubsidiary %>% is.na())


            if (two_col) {
              table_df <-
                table_df %>%
                tidyr::separate(
                  locationOrganizationSubsidiary,
                  into = c(
                    'locationOrganizationSubsidiary',
                    'dateSubsidiaryFormed'
                  ),
                  sep = 'FORMED'
                ) %>%
                suppressWarnings() %>%
                mutate(locationOrganizationSubsidiary = locationOrganizationSubsidiary %>% str_replace_all('\\,', '')) %>%
                mutate_all(funs(. %>% str_replace('\n', '') %>% str_trim()))
            }


            if ('nameProperty' %in% names(table_df)) {
              table_df <-
                table_df %>%
                mutate(nameProperty = ifelse(nameProperty == '', NA, nameProperty)) %>%
                mutate_all(funs(. %>% str_replace('\n|\n  |\n  ', '') %>% str_trim())) %>%
                mutate_all(funs(. %>% str_replace('\n', '') %>% str_trim())) %>%
                mutate_all(funs(. %>% str_replace('  ', ' ') %>% str_trim())) %>%
                fill(nameProperty)

            }

            return(table_df)
          })

        if ('dateSubsidiaryFormed' %in% names(df)) {
          df <-
            df %>%
            mutate(dateSubsidiaryFormed = dateSubsidiaryFormed %>% lubridate::mdy())
        }

        df <-
          df %>%
          mutate(idCIK = cik, urlSEC = url) %>%
          select(idCIK, nameSubsidiary, everything()) %>%
          mutate(
            locationOrganizationSubsidiary = locationOrganizationSubsidiary %>% str_replace_all(
              'A |LIMITED LIABILITY COMPANY|CORPORATION|LIMITED PARTNERSHIP'
            ) %>% str_trim()
          )

        return(df)
      }
      if (!has_property) {
        df <-
          all_data %>%
          mutate(value = ifelse(value == '', NA, value)) %>%
          filter(!value %>% is.na()) %>%
          group_by(item) %>%
          mutate(idSubsidiary = 1:n()) %>%
          spread(item, value) %>%
          filter(!X1 == '') %>%
          mutate(idSubsidiary = 1:n()) %>%
          gather(item, value, -c(X1, idSubsidiary)) %>%
          ungroup() %>%
          filter(!value %>% str_detect(hit_terms)) %>%
          spread(item, value)
      }

    }

    df <-
      df %>%
      dplyr::rename(nameSubsidiary = X1) %>%
      tidyr::separate(nameSubsidiary,
                      sep = '\\(',
                      into = c('nameSubsidiary', 'remove')) %>%
      select(-matches("remove")) %>%
      mutate(nameSubsidiary = nameSubsidiary %>% str_trim()) %>%
      suppressWarnings() %>%
      select(-matches("idSubsidiary"))

    if (has_pct) {
      names(df)[names(df) %>% grep(pct_col, .)] <-
        'pctSubsidiaryOwned'

      df <-
        df %>%
        mutate_at(df %>% select(matches('pct')) %>% names(),
                  funs(. %>% as.numeric() / 100)) %>%
        suppressWarnings()
    }

    if (has_loc_key) {
      names(df)[names(df) %>% grep(loc_col, .)] <-
        'locationOrganizationSubsidiary'
    }

    df <-
      df %>%
      select(-matches("X"))

    return(df)
  }

parse_sec_subsidiary_url_html <-
  function(url = "https://www.sec.gov/Archives/edgar/data/34088/000003408816000065/xomexhibit21.htm",
           return_message = TRUE) {
    cik <-
      url %>%
      parse_sec_url_for_cik()

    page <-
      url %>%
      read_html()

    is_zero <-
      page %>%
      html_nodes(paste0('td:nth-child(', 1, ')')) %>%
      length() == 0
    locations <-
      get_loc_df() %>%
      .$nameLocation

    if (is_zero) {
      data <-
        page %>%
        html_nodes('font') %>%
        html_text() %>%
        str_replace_all('\\ ', ' ')

      data <-
        data[!data == '']


      is_parenth <-
        data %>% str_detect('\\(') %>% sum() / length(data) > .25

      if (is_parenth) {
        data <-
          data[data %>% str_detect('\\(')]

        df <-
          data_frame(data) %>%
          separate(
            data,
            sep = '\\(',
            into = c('nameSubsidiary', 'locationOrganizationSubsidiary')
          ) %>%
          separate(
            locationOrganizationSubsidiary,
            sep = '\\)',
            into = c('locationOrganizationSubsidiary', 'remove')
          ) %>%
          select(-remove) %>%
          mutate_all(funs(. %>% str_trim() %>% str_to_upper())) %>%
          mutate(idCIK = cik, urlSEC = url) %>%
          select(-matches("idSubsidiary"))

        if (return_message) {
          list("Parsed: ", url) %>%
            purrr::invoke(paste0, .) %>% message()
        }

        return(df)
      }

      is_nested <-
        page %>%
        html_nodes('b font') %>%
        html_text() %>% length() > 2

      if (is_nested) {
        locations_raw <-
          page %>%
          html_nodes('b font') %>%
          html_text() %>%
          str_replace_all('\\:', '') %>%
          str_to_upper()

        locations <-
          locations_raw[!locations_raw %>% str_detect('EXHIBIT|SUBSIDIARY|SUBSIDIARIES')]

        data <-
          data[data %>% nchar() > 3] %>% str_to_upper()

        df <-
          data_frame(nameSubsidiary = data) %>%
          mutate(idRow = 1:n())

        loc_df <-
          data_frame(nameSubsidiary = locations) %>%
          inner_join(df %>% select(idRow, nameSubsidiary)) %>%
          mutate(idRow = idRow + 1) %>%
          select(locationOrganizationSubsidiary = nameSubsidiary, idRow) %>%
          suppressMessages()

        df <-
          df %>%
          filter(!nameSubsidiary %>% str_detect('SUBSIDIARY|SUBSIDIARIES')) %>%
          filter(!nameSubsidiary %>% str_detect(paste0(locations_raw, collapse = '|'))) %>%
          suppressWarnings()

        df <-
          df %>%
          left_join(loc_df) %>%
          fill(locationOrganizationSubsidiary) %>%
          mutate(urlSEC = url, idCIK = cik) %>%
          select(idCIK,
                 nameSubsidiary,
                 locationOrganizationSubsidiary,
                 everything()) %>%
          select(-idRow) %>%
          suppressMessages() %>%
          select(-matches("idSubsidiary"))
        if (return_message) {
          list("Parsed: ", url) %>%
            purrr::invoke(paste0, .) %>% message()
        }

        return(df)
      }
    }

    is_font_table <-
      page %>%
      html_nodes('b') %>%
      html_text() %>% length() == 0

    if (is_font_table) {
      all_data <-
        1:10 %>%
        map_df(function(x) {
          css_selector <-
            paste0('td:nth-child(', x, ')')
          has_length <-
            page %>%
            html_nodes(css_selector) %>% length() > 0
          if (has_length) {
            item <-
              paste0("X" , x)

            value <-
              page %>%
              html_nodes(css_selector) %>%
              html_text() %>%
              str_trim()
            data_frame(item, value)
          }
        }) %>%
        mutate(
          value = value %>% str_to_upper() %>% str_replace_all('\n  ', ' ') %>% str_replace_all('\u0096 ', '')
        ) %>%
        filter(!value == '')


      has_loc_key <-
        all_data %>%
        filter(value %in% locations) %>%
        nrow() > 0

      if (has_loc_key) {
        loc_col <-
          all_data %>%
          filter(value %in% locations) %>%
          .$item %>%
          unique()
      }

      hit_terms_in <-
        c(
          "Organized",
          "STATE OR|STATE OF|JURISDICTION OF|JURISDICTION OF INCORPORATION OR ORGANIZATION|JURISDICTION|JURISDICTION OF INCORPORATION OR\nORGANIZATION",
          "NAME|ORGANIZED UNDER THE LAWS OF",
          'STATE OF ORGANIZATION',
          'STATE OR COUNTRY OF ORGANIZATION',
          'NAME OF SUBSIDIARY',
          'NAME',
          'ENTITY NAME',
          'the laws of',
          'Percentage of voting',
          'securities owned by',
          'immediate parent',
          'CERTAIN INTERMEDIARY SUBSIDIARIES',
          'PERCENT OWNED'
        )
      hit_terms <-
        hit_terms %>%
        str_to_upper() %>%
        paste0(collapse = '|')

      hit_terms_in <-
        hit_terms %>% str_split('\\|') %>%
        flatten_chr()

      has_pct_col <-
        all_data %>%
        filter(value %in% "100") %>%
        nrow() > 0 |
        (all_data %>% filter(value %>% str_detect('PERCENT')) %>% nrow() > 0)

      if (has_pct_col) {
        pct_col <-
          all_data %>%
          filter((value %in% "100") |
                   (value %>% str_detect("PERCENT"))) %>%
          .$item %>%
          unique() %>%
          .[[1]]
      }

      all_data <-
        all_data %>%
        filter(!value %in% hit_terms_in) %>%
        filter(!value %>% str_detect(hit_terms)) %>%
        filter(!value == '') %>%
        mutate(valueNC = value %>% nchar()) %>%
        filter(!value %>% str_detect("PERCENT"))

      if (!has_pct_col) {
        all_data <-
          all_data %>%
          filter(valueNC > 3)
      }
      all_data <-
        all_data %>%
        select(-valueNC) %>%
        group_by(item) %>%
        mutate(idSubsidiary = 1:n()) %>%
        spread(item, value) %>%
        ungroup() %>%
        dplyr::rename(nameSubsidiary = X1)

      if (has_loc_key) {
        names(all_data)[names(all_data) %in% loc_col] <-
          'locationOrganizationSubsidiary'
      }

      if (has_pct_col) {
        names(all_data)[names(all_data) %in% pct_col] <-
          'pctSubsidiaryOwned'

        all_data <-
          all_data %>%
          mutate(pctSubsidiaryOwned = pctSubsidiaryOwned %>% as.numeric() / 100)
      }

      all_data <-
        all_data %>%
        mutate(idCIK = cik,
               dateSubsidiaryAsOf = NA,
               urlSEC = url) %>%
        select(-matches("idSubsidiary|^X"))

      if (return_message) {
        list("Parsed: ", url) %>%
          purrr::invoke(paste0, .) %>% message()
      }

      return(all_data)

    }

    df <-
      page %>%
      parse_page_subsidiary_table_html() %>%
      suppressWarnings()

    df <-
      df %>%
      filter(!nameSubsidiary == '') %>%
      mutate(idCIK = cik, urlSEC = url) %>%
      select(-matches("idSubsidiary")) %>%
      select(idCIK, everything())

    if (return_message) {
      list("Parsed: ", url) %>%
        purrr::invoke(paste0, .) %>% message()
    }

    return(df %>% select(-matches("idSubsidiary")))

  }

# url = 'https://www.sec.gov/Archives/edgar/data/19617/000095012301002499/y46253ex21-1.txt'
parse_sec_subsidiary_url_text <-
  function(url = "https://www.sec.gov/Archives/edgar/data/899689/000104746903007996/a2104897zex-21.txt",
           return_message = TRUE) {
    cik <-
      url %>%
      parse_sec_url_for_cik()
    data <-
      url %>%
      read_lines()

    data <-
      data[!data == '']
    has_s <-
      data %>% str_detect("<S>") %>% sum() > 0

    if (has_s) {
      data <-
        data[(data %>% grep("<S>", .) %>% .[[1]] + 1):length(data)]
    }

    data <-
      data[!data %>% str_detect("STATE OF|NAME OF|---|NAME OF SUBSIDIARY|ORGANIZED UNDER|THE LAWS OF|<")]

    data <-
      data[data %>% nchar() > 3]

    df <-
      1:length(data) %>%
      map_df(function(x) {
        item <-
          data[[x]]

        items <-
          item %>%
          str_replace_all('\\   ', '\\:') %>%
          str_split('\\:') %>%
          flatten_chr() %>%
          str_trim() %>%
          str_to_upper()

        items <-
          items[!items == '']

        if (items %>% length() == 1) {
          return(data_frame())
        }

        two_items <-
          items %>% length() == 2
        if (two_items) {
          table_data <-
            data_frame(
              idSubsidiary = x,
              nameSubsidiary = items[[1]],
              locationOrganizationSubsidiary = items[[2]]
            )
        }
        three_items <-
          items %>% length() == 3
        if (three_items) {
          table_data <-
            data_frame(
              idSubsidiary = x,
              nameSubsidiary = items[[1]],
              locationOrganizationSubsidiary = items[[2]],
              pctSubsidiaryOwned = items[[3]] %>% as.numeric() / 100
            )
        }

        table_data <-
          table_data %>%
          mutate(
            isChildSubsidiary = ifelse(nameSubsidiary %>% substr(1, 1) == "-", TRUE, FALSE),
            nameSubsidiary = nameSubsidiary %>% str_replace('\\-', '') %>% str_trim()
          )
        return(table_data)
      }) %>%
      mutate(idCIK = cik, urlSEC = url) %>%
      select(-matches("idSubsidiary")) %>%
      select(idCIK,
             nameSubsidiary,
             locationOrganizationSubsidiary,
             everything()) %>%
      filter(!nameSubsidiary %in% c('NAME', 'ORGANIZED UNDER'))

    df <-
      df %>%
      filter(!nameSubsidiary == '')

    if (return_message) {
      list("Parsed: ", url) %>%
        purrr::invoke(paste0, .) %>% message()
    }

    return(df)

  }

parse_sec_subsidiary_url  <-
  function(url = "https://www.sec.gov/Archives/edgar/data/34088/000003408816000065/xomexhibit21.htm",
           return_message = TRUE)  {
    is_text <-
      url %>%
      str_detect("txt")

    is_html <-
      url %>%
      str_detect("html|htm")
    parse_sec_subsidiary_url_text_safe <-
      purrr::possibly(parse_sec_subsidiary_url_text, data_frame())

    parse_sec_subsidiary_url_html_safe <-
      purrr::possibly(parse_sec_subsidiary_url_html, data_frame())

    if (is_text) {
      data <-
        url %>%
        parse_sec_subsidiary_url_text_safe()
    }

    if (is_html) {
      data <-
        url %>%
        parse_sec_subsidiary_url_html_safe()
    }
    return(data)
  }
