# functions ---------------------------------------------------------------
drop_na_columns <-
  function(data) {
    data %>%
      select(which(colMeans(is.na(.)) < 1))
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
      ) %>% dplyr::select(-dplyr::matches("nameElement")) %>% names(),
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
      data %>% dplyr::select(dplyr::matches("^date")) %>% future_map(class)
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
    cik_char <- cik %>%
      as.character()
    cik_chars <-
       cik_char %>%
      nchar()

    zeros_needed <-
      10 - cik_chars

    zeros <-
      rep(0,zeros_needed) %>% as.character() %>% paste0(collapse = '')

    cik_code <-
      glue::glue("{zeros}{cik_char}") %>% as.character()
    cik_code
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
          select(-dplyr::matches("url")) %>% names(),
        funs(. %>% str_to_upper())
      )

    return(data)
  }

remove_duplicate_columns <-
  function(data) {
    column_ids <-
      tibble(name = names(data)) %>%
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

filer_type_df <-
  function() {
    tibble(
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

general_name_df <-
  function() {
    general_name_df <-
      tibble(
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

private_name_df <-
  function() {
    tibble(
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
#' @return a \code{tibble}
#' @export
#' @import dplyr
#' @examples
#' dictionary_form_d_categories()
dictionary_form_d_categories <-
  function() {
    category_df <-
      dplyr::tibble(
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

insider_code_df <-
  function() {
    insider_df <-
      tibble(
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
#' @return a \code{tibble}
#' @export
#' @import dplyr stringr
#' @family SEC
#' @family dictionary
#'
#' @examples
#' dictionary_sec_filing_codes()
dictionary_sec_filing_codes <-
  function() {
    tibble(
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
        "Mine Safety Reporting of Shutdowns and Patterns of Violations",
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
        "Amendments to the Registrantion Code of Ethics, or Waiver of a Provision of the Code of Ethics",
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
#' @return a \code{tibble}
#' @export
#' @family SEC
#' @family dictionary
#'
#' @examples
#' dictionary_sec_form_codes()
dictionary_sec_form_codes <-
  function() {
    tibble(
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

company_type_df <-
  function() {
    tibble(
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
#' @return a \code{tibble}
#' @export
#' @import dplyr stringr
#'
#' @examples
#' dictionary_sec_rules()
dictionary_sec_rules <-
  function() {
    tibble(
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
              data %>% dplyr::select(dplyr::matches("^name")) %>% names(),
            funs(. %>% stringr::str_to_upper())
          )
      } else {
        data <-
          data %>%
          mutate_at(
            .vars =
              data %>% dplyr::select(dplyr::matches("^name")) %>% names(),
            funs(. %>% stringr::str_to_lower())
          )
      }
      return(data)
    }
  }

.parse_rank_and_filed_data <-
  function(url = "http://rankandfiled.com/static/export/sic_naics.csv",
           column_names = c('idSIC',
                            'classificationSIC',
                            'idNAICS',
                            'classificationNAICS')) {
    data <-
      url %>%
      readr::read_csv(col_names = F)

    data <-
      data %>%
      tidyr::separate(col = X1,
                      sep = '\\|',
                      into = column_names,
                      extra = "merge",
                      fill = "right") %>%
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
      cat(fill = T)
  }

.resolve_name_df <-
  function(data) {
    name_df <-
      general_name_df() %>%
      bind_rows(private_name_df()) %>%
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
                      dplyr::matches(
                        "idCIK|idMidas|idIRS|^count|^price|^amount|^ratio|^pct|idMDA|^dateiso|idRF|price|amount|^year"
                      )
                    ) %>% names,
                  funs(. %>% as.character() %>% readr::parse_number())) %>%
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
                    dplyr::matches(
                      "idCIK|idMidas|idIRS|^count|^price|^amount|^ratio|^pct|idMDA|^dateiso|idRF|price|amount|^year"
                    )
                  ) %>% names,
                funs(. %>% as.character() %>% readr::parse_number())) %>%
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
#' @return a \code{tibble}
#' @export
#' @import dplyr
#' @importFrom readr read_csv
#' @importFrom tidyr separate
#' @family dictionary
#' @examples
#' sic_naics_codes(filter_duplicates = TRUE)
sic_naics_codes <-
  function(filter_duplicates = TRUE) {
    sic <-
      "http://rankandfiled.com/static/export/sic_naics.csv" %>%
      .parse_rank_and_filed_data(column_names = c(
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
#' @return a  \code{tibble}
#' @export
#' @import dplyr purrr
#' @importFrom  tidyr separate
#' @importFrom readr read_csv
#' @examples
#' @family dictionary
#' @family SEC
#' sec_rules()
sec_rules <-
  function() {
    codes <-
      "http://rankandfiled.com/static/export/file_numbers.csv" %>%
      .parse_rank_and_filed_data(column_names = c("idPrefixSEC", "typeFiling", "nameLawSEC")) %>%
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
#' @return a \code{tibble}
#' @export
#' @import dplyr purrr
#' @importFrom  tidyr separate
#' @importFrom readr read_csv
#' @family dictionary
#' @examples
#' location_codes()
location_codes <-
  function() {
    countries <-
      "http://rankandfiled.com/static/export/edgar_state_country.csv" %>%
      data.table::fread() %>%
      as_tibble() %>%
      setNames(c('codeLocation', 'nameLocation'))
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
#' rf_leis()
rf_leis <-
  function(return_message = TRUE) {
    leis <-
      "http://rankandfiled.com/static/export/cik_lei.csv" %>%
      .parse_rank_and_filed_data(column_names = c('idCIK',
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
        purrr::invoke(paste0, .) %>% cat(fill = T)
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
#' @examples
#' rf_us_tickers(return_message = TRUE)
rf_us_tickers <-
  function(return_message = TRUE) {
    data <-
      "http://rankandfiled.com/static/export/cik_ticker.csv" %>%
      .parse_rank_and_filed_data(
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
      left_join(tibble(
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
      location_codes()


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
        sic_naics_codes(filter_duplicates = TRUE) %>%
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
        purrr::invoke(paste0, .) %>% cat(fill = T)
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
#' @return a \code{tibble}
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
#' mmf_owned_debt_securities(return_message = TRUE)
mmf_owned_debt_securities <-
  function(return_message = TRUE) {
    debt <-
      "http://rankandfiled.com/static/export/mmf_cusips.csv" %>%
      .parse_rank_and_filed_data(
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
#' @return a \code{tibble}
#' @import dplyr purrr
#' @importFrom  tidyr separate
#' @importFrom readr read_csv
#' @export
#' @family entity search
#' @family SEC
#' @family Rank and Filed
#' @family securities search
#' @examples
#' sec_13F_companies()
rf_sec_13F_companies <-
  function(return_message = TRUE) {
    data <-
      "http://rankandfiled.com/static/export/13f_cusips.csv" %>%
      .parse_rank_and_filed_data(column_names = c('idCIK', 'nameIssuer', 'idCUSIP', 'classSecurity')) %>%
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
#' @return nested \code{tibble} or \code{tibble} if \code{nest_data = FALSE}
#' @import dplyr purrr tidyr readr lubridate stringr
#' @importFrom jsonlite fromJSON
#' @export
#' @family SEC
#' @family Rank and Filed
#' @family securities transaction
#' @examples
#' recent_insider_trades(nest_data = TRUE)

recent_insider_trades <-
  function(nest_data = FALSE,
           return_message = TRUE) {
    options(scipen = 9999)
    json_data <-
      "http://rankandfiled.com/data/buy_sell" %>%
      jsonlite::fromJSON()
    insider_name_df <-
      tibble(
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
      future_map_dfr(function(x) {
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
            tibble(X1 = table) %>%
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
            tibble(X1 = table) %>%
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
            tibble(X1 = table) %>%
            tidyr::separate(col = X1,
                            into = table_names,
                            sep = '\\*') %>%
            suppressWarnings()
        }

        df <-
          df %>%
          select(-dplyr::matches("^X[1-9]")) %>%
          separate(idTicker,
                   into = c('replace', 'idTicker'),
                   sep = 'c|f') %>%
          select(-replace) %>%
          mutate(idTicker = ifelse(idTicker == '', NA, idTicker)) %>%
          mutate_at(.vars = df %>% select(dplyr::matches("^count|^amount|idCIK")) %>% names,
                    funs(. %>% as.numeric())) %>%
          mutate_at(.vars = df %>% select(dplyr::matches("^name")) %>% names,
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
        .vars = all_data %>% select(dplyr::matches("amount")) %>% names(),
        funs(. %>% as.numeric %>% formattable::currency(digits = 0))
      ) %>%
      mutate_at(
        .vars = all_data %>% select(dplyr::matches("count")) %>% names(),
        funs(. %>% as.numeric %>% formattable::comma(digits = 0))
      ) %>%
      mutate_at(
        .vars = all_data %>% select(dplyr::matches("name|type")) %>% names(),
        funs(. %>% stringr::str_to_upper())
      ) %>%
      arrange(nameCompany, nameTable)

    if (return_message) {
      list("You got ", all_data %>% nrow() %>% formattable::comma(digits = 0), ' Insider Transactions from the last 7 days') %>%
        purrr::reduce(paste0) %>%
        cat(fill = T)
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
#' @return a \code{tibble}
#' @export
#' @family SEC
#' @family Rank and Fild
#'
#' @examples
#' sec_securities_filing_counts()
sec_securities_filing_counts <-
  function(return_message = TRUE) {
    dict_count <- tibble(
      idForm = c('D', 'W', 'S-1', 'S-3', 'S-4', "F"),
      nameForm = c(
        "Exempt Offering",
        "Secondary Sale",
        "IPOs",
        "Simplified Registration Form",
        "Merger",
        "Foreign Issuer"
      ) %>% stringr::str_to_upper()
    )
    data <-
      "http://rankandfiled.com/data/registered_offerings" %>%
      jsonlite::fromJSON(simplifyDataFrame = TRUE, flatten = T)


    data <-
      data$registered

    data <-
      seq_along(data) %>%
      map_df(function(x) {
        df_row <-
          data[[x]]
        year_no <-
          names(data[x]) %>% as.numeric()
        df_row %>%
          flatten_df() %>%
          as_tibble() %>%
          gather(idForm, countFilings) %>%
          mutate(yearFiling = year_no) %>%
          select(yearFiling, everything())
      })

    filing_count_data <-
      data %>%
      mutate(
        countFilings = countFilings %>% formattable::comma(digits = 0)
      ) %>%
      filter(countFilings > 0) %>%
      left_join(
        dict_count
      ) %>%
      select(yearFiling, idForm,nameForm, everything()) %>%
      suppressMessages()

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
        cat(fill = T)
    }

    filing_count_data

  }

.generate_securities_urls <-
  function() {
    count_df <-
      sec_securities_filing_counts(return_message = FALSE) %>%
      mutate(lengthOut = ceiling(countFilings/50) + 1)

    url_df <-
      1:nrow(count_df) %>%
      future_map_dfr(function(x) {
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

        tibble(
          idForm = row_df$idForm,
          yearFiling = row_df$yearFiling,
          urlData = url_data
        )
      })

    return(url_df)
  }

.parse_securities_url <-
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
        dplyr::as_tibble() %>%
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
                    data %>% select(dplyr::matches("^amount|^count|idIndustry")) %>% names(),
                  funs(. %>% as.numeric()))

      if (return_message) {
        list("Parsed: ", url) %>%
          purrr::invoke(paste0, .) %>% cat(fill = T)
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
#' @return where \code{nest_data} is \code{TRUE} a nested tibble by asset,
#' where \code{nest_data} is \code{FALSE} a tibble
#' @export
#' @family SEC
#' @family entity search
#' @family securities search
#' @family Rank and Filed
#' @examples
#' #' \dontrun{
#'
#' ## All Securities Filings
#' securities_offerings(year_forms = NULL, forms = NULL, return_message = TRUE, nest_data = TRUE)
#'
#' ## IPOs since 1999
#' securities_offerings(year_forms = 1999:2017, forms = "S-1", return_message = TRUE, nest_data = FALSE)
#' }
securities_offerings <-
  function(year_forms = NULL,
           forms = NULL,
           nest_data = FALSE,
           return_message = TRUE) {
    url_df <-
      .generate_securities_urls()

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

    .parse_securities_url_safe <-
      purrr::possibly(.parse_securities_url, NULL)

    all_data <-
      urls %>%
      future_map_dfr(function(x) {
        .parse_securities_url_safe(
          url = x,
          column_names = c('idCIK', 'nameIssuer', 'idTicker', 'idForm'),
          return_message = TRUE
        )
      }) %>%
      arrange(dateFiling) %>%
      distinct()

    all_data <-
      all_data %>%
      left_join(tibble(
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
        cat(fill = T)
    }

    if (nest_data) {
      all_data <-
        all_data %>%
        nest(-c(idForm, typeForm), .key = dataOfferings)

    }

    return(all_data)

  }


# search_names ------------------------------------------------------------------

.generate_search_name <-
  function(entity_name = "Rockwood Capital") {
    json_url <-
      list(
        "http://rankandfiled.com/data/search?q=",
        entity_name %>% stringr::str_to_lower() %>% URLencode()
      ) %>%
      purrr::invoke(paste0, .)

    return(json_url)
  }

.parse_rf_search_name <-
  function(url = "http://rankandfiled.com/data/search?q=kav") {
    data <-
      url %>%
      jsonlite::fromJSON(simplifyDataFrame = TRUE) %>%
      .$results %>%
      data.frame(results = ., stringsAsFactors = FALSE) %>%
      as_tibble()

    type_df <-
      tibble(
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
      mutate(idCIK = idCIKTicker %>% as.character() %>% readr::parse_number()) %>%
      left_join(type_df) %>%
      left_join(company_type_df()) %>%
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

.sec_entity <-
  function(entity_name = "Rockwood Capital",
           return_message = TRUE) {
    json_url <-
      entity_name %>%
      .generate_search_name()

    .parse_rf_search_name_safe <-
      possibly(.parse_rf_search_name, tibble())

    data <-
      json_url %>%
      .parse_rf_search_name_safe() %>%
      mutate(nameEntitySearch = entity_name) %>%
      select(nameEntitySearch, dplyr::matches("^name"), everything())

    if (data %>% nrow() == 0) {
      return(tibble())
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
        purrr::invoke(paste0, .) %>% cat(fill = T)
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
#' @return a \code{tibble}
#' @export
#' @import purrr dplyr stringr tidyr formattable
#' @importFrom jsonlite fromJSON
#' @family SEC
#' @family Rank and Filed
#' @family entity search
#' @examples
#' sec_filing_entities(entity_names = c('Rockwood Capital', 'Vornado', 'Two Sigma'))
sec_filing_entities <-
  function(entity_names = c('Rockwood Capital', 'Vornado', 'Two Sigma'),
           return_message = TRUE) {
    no_entry <-
      (entity_names %>% purrr::is_null() |
         !'entity_names' %>% exists())

    if (no_entry) {
      stop("Please enter a search name")
    }

    .sec_entity_safe <-
      purrr::possibly(.sec_entity, NULL)

    all_data <-
      entity_names %>%
      future_map_dfr(function(x) {
        .sec_entity_safe(entity_name = x, return_message = return_message)
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
        future_map_dfr(function(x) {
          entity <-
            all_data$nameEntity[[x]]

          has_double <-
            entity %>% str_detect("|")
          if (!has_double) {
            return(tibble(idRow = x, nameEntity = entity))
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
            tibble(idRow = x,
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
        select(nameEntitySearch, dplyr::matches("nameEntity"), everything()) %>%
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
#' \code{sec_form_ds()} queries all SEC filed form-d's since 2009 and returns the associated data.
#' the default parameters search every industry and year which you
#' can change by modifying the parameters
#'
#' @param industries industries to search options: \itemize{
#' \item \code{NULL}: returns all industries(default)
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
#' }
#' @param return_message \code{TRUE} return a message after data import
#' @param nest_data \code{TRUE} return nested data frame
#' @import purrr dplyr stringr tidyr formattable lubridate
#' @importFrom jsonlite fromJSON
#' @return where \code{nest_data} is \code{TRUE} a nested tibble by asset,
#' where \code{nest_data} is \code{FALSE} a tibble

#' @export
#'
#' @examples
#' \dontrun{
#' sec_form_ds()
#' sec_form_ds(form_years = 2016:2017, industries = c("Real Estate", "Technology", "Other"))
#' }
sec_form_ds <-
  function(industries = NULL,
           form_years = 2019,
           months = NULL,
           nest_data = FALSE,
           return_message = TRUE) {
    now_year <-
      Sys.Date() %>% lubridate::year()

    now_month <-
      Sys.Date() %>% lubridate::month()

    if (length(industries) == 0) {
      industries <- c("ENERGY",
                      "FINANCIAL",
                      "HEALTHCARE",
                      "OTHER",
                      "REAL ESTATE",
                      "TECHNOLOGY",
                      "TRAVEL AND LEISURE")
    }

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
      purrr::invoke(paste0, .) %>%
      as.numeric()

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

    if (length(form_years) == 0) {
      form_years <- years
    }

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
      filter(as.numeric(namePeriod) <= now_period)

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
      dictionary_form_d_categories()

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

    .parse_securities_url_safe <-
      purrr::possibly(.parse_securities_url, NULL)

    all_data <-
      urls %>%
      future_map_dfr(function(x) {
        .parse_securities_url_safe(
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
      mutate_at(.vars = all_data %>% select(dplyr::matches("^amount")) %>% names,
                funs(. %>% formattable::currency(digits = 0))) %>%
      mutate_at(.vars = all_data %>% select(dplyr::matches("^count")) %>% names,
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
        cat(fill = T)
    }

    if (nest_data) {
      all_data <-
        all_data %>%
        nest(-nameIndustryParent, .key = dataFormDs)
    }
    return(all_data)
  }




