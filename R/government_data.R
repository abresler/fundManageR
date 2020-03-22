dictionary_fdic_names <-
  function() {
    tibble(nameFDIC = c("STNAME", "CERT", "DOCKET", "ACTIVE", "ADDRESS", "ASSET", "BKCLASS",
                        "CHANGEC1", "CHANGEC2", "CHANGEC3", "CHANGEC4", "CHANGEC5", "CHARTER",
                        "CHRTAGNT", "CONSERVE", "CITY", "CLCODE", "CMSA_NO", "CMSA",
                        "COUNTY", "DATEUPDT", "DENOVO", "DEP", "EFFDATE", "ENDEFYMD",
                        "EQ", "ESTYMD", "FDICDBS", "FDICREGN", "FDICSUPV", "FED", "FED_RSSD",
                        "FEDCHRTR", "FLDOFF", "IBA", "INACTIVE", "INSAGNT1", "INSAGNT2",
                        "INSDATE", "INSTCRCD", "INSBIF", "INSCOML", "INSDIF", "INSFDIC",
                        "INSSAIF", "INSSAVE", "MSA_NO", "MSA", "NAME", "NEWCERT", "OAKAR",
                        "OTSDIST", "OTSREGNM", "PROCDATE", "QBPRCOML", "REGAGNT", "REPDTE",
                        "RISDATE", "STCHRTR", "ROA", "ROAQ", "ROE", "ROEQ", "RUNDATE",
                        "SASSER", "LAW_SASSER_FLG", "STALP", "STCNTY", "STNUM", "ZIP",
                        "SUPRV_FD", "OCCDIST", "UNINUM", "ULTCERT", "CFPBEFFDTE", "CFPBENDDTE",
                        "CFPBFLAG", "REGAGENT2", "TE01N528", "TE02N528", "TE03N528",
                        "TE04N528", "TE05N528", "TE06N528", "TE07N528", "TE08N528", "TE09N528",
                        "TE10N528", "TE01N529", "TE02N529", "TE03N529", "TE04N529", "TE05N529",
                        "TE06N529", "WEBADDR", "OFFICES", "CERTCONS", "PARCERT", "CITYHCR",
                        "DEPDOM", "FORM31", "HCTMULT", "INSTAG", "MUTUAL", "NAMEHCR",
                        "NETINC", "NETINCQ", "OFFDOM", "OFFFOR", "OFFOA", "RSSDHCR",
                        "STALPHCR", "STMULT", "SUBCHAPS", "ROAPTX", "ROAPTXQ", "TRUST",
                        "SPECGRP", "SPECGRPN", "TRACT", "CSA", "CSA_NO", "CSA_FLG", "CBSA",
                        "CBSA_NO", "CBSA_METRO_NAME", "CBSA_METRO", "CBSA_METRO_FLG",
                        "CBSA_MICRO_FLG", "CBSA_DIV", "CBSA_DIV_NO", "CBSA_DIV_FLG",
                        "CB","idFDIC",
      "CHANGEC6",
"CHANGEC7",
"CHANGEC8",
"CHANGEC9",
"CHANGEC10",
"CHANGEC11",
"CHANGEC12",
"CHANGEC13",
"CHANGEC14",
"CHANGEC15"),
           nameActual = c("nameState", "idCertificate", "idDocket", "isActiveBank", "addressStreet", "amountAssetsMillions", "classCharterFDIC",
                          "codeChange1", "codeChange2", "codeChange3", "codeChange4", "codeChange5", "idCharter",
                          "agencyCharter", "isConservatorship", "city", "idCLCode", "idCMSA", "nameCMSA",
                          "county", "dateUpdated", "isDenovo", "amountDepositsMillions", "dateChangeMostRecent", "dateLastEvent",
                          "amountEquityMillions", "dateEstablished", "idFDICRegion", "nameFDICRegion", "nameFDICSupervisor", "idFederalReserveRegion", "idFederalReserve",
                          "hasFedCharter", "idFDICFieldOffice", "isInsuredForeignBankOffice", "isInactive", "codeInsuranceFundMembership", "codeInsuranceFundMembership2",
                          "dateDepositInsurance", "isCreditCardIssuer", "isBankInsuranceFund", "idInsuredCommercialBank", "isDepositFundMember", "isFDICInsured",
                          "isSAAMember", "isSAIFInsured", "idMSA", "nameMSA", "nameInstitution", "idNewCertification", "isOakarInstitution",
                          "idOTS", "nameOTSRegion", "dateLastProcessChange", "idQuarterlyBankingProfile", "codeRegulator", "dateReportingLast",
                          "dateReport", "hasStateCharter", "pctReturnOnAssets", "pctReturnOnAssetsQuarter", "pctReturnOnEquity", "pctReturnOnEquityQuarter", "dateInformationUpdated",
                          "isSasserInstitution", "hasSasserFlag", "codeStateHeadQuarters", "idFIPSHeadQuarters", "idFIPSStateHeadQuarters", "zipcodeHeadQUarters",
                          "idFDICSupervisory", "idOCCSupervisory", "idFDIC", "idCertificatePurchaser", "dateCPFBSecondarySupervision", "dateCPFBSecondarySupervisionEnd",
                          "hasCPFBSUpervision", "remove", "urlDepositTaker", "urlDepositTaker2", "urlDepositTaker3",
                          "urlDepositTaker4", "urlDepositTaker5", "urlDepositTaker6", "urlDepositTaker7", "urlDepositTaker8", "urlDepositTaker9",
                          "urlDepositTaker10", "nameTradeInstitution", "nameTradeInstitution1", "nameTradeInstitution2", "nameTradeInstitution3", "nameTradeInstitution4",
                          "nameTradeInstitution5", "urlInstitution", "countOffices", "countBanksOwned", "countSubsidiaries", "cityHeadQuarters",
                          "amountDepositsDomesticMillions", "isForm31Filer", "isMultiBankHolingCompany", "isAgriculturalLendingInstitution", "isMutualOwned", "nameBankHoldingCompany",
                          "amountNetIncomeMillions", "amountNetIncomeMillionsQuarter", "OFFDOM", "countOfficesDomestic", "countOfficesUSA", "idFederalReserveHoldingParent",
                          "stateHeadQuarters", "isInterstateBank", "isSubChapterS", "pctPreTaxReturnOnAsset", "pctPreTaxReturnOnAssetQuarter", "hasTrustPowers",
                          "idAssetTotalCode", "typeAssetCode", "hasTractPowers", "nameCSA", "idCSA", "hasCSAFlag", "nameCBSA",
                          "idCBSA", "nameCBSA2", "idCBSAMetro", "hasCBSA",
                          "hasCBSAMicroFlag", "nameCBSADiv", "idCBSADiv", "hasCBSADiv",
                          "isCommunityBank", "idFDIC")

    )
  }

.assign_fdic_names <-
  function(data) {

    fdic_names <- names(data)
    df_gov_names <-
      dictionary_fdic_names()

    all_names <-
      fdic_names %>%
      map_chr(function(fdic_name) {
        no_name <-
          df_gov_names %>%
          filter(fdic_name == nameFDIC) %>%
          nrow() == 0

        if (no_name) {
          glue::glue("Missing {fdic_name} in dictionary") %>% message()
          return(fdic_name)
        }
        df_gov_names %>%
          filter(nameFDIC  == fdic_name) %>%
          pull(nameActual) %>%
          unique() %>%
          .[[1]]
      })

    data %>%
      set_names(all_names)

  }

.munge_fdic <-
  function(data) {
    is_has <- data %>%
      dplyr::select(dplyr::matches("^is|^has")) %>% names()

    data <-
      data %>%
      mutate_at(is_has,
                list(function(x) {
                  x %>% as.character() %>% str_replace_all("Y", "1") %>% str_replace_all("N", "0") %>% as.numeric() %>% as.logical()
                }))

    date_cp <- data %>%
      dplyr::select(dplyr::matches("dateCPFB")) %>% names()

    data <-
      data %>%
      mutate_at(date_cp,
                list(function(x){
                  x %>% str_replace_all("9999", "1999") %>% dmy()
                }))

    data <-
      data %>%
      mutate_if(is.numeric,
                list(function(x) {
                  ifelse(x == 0, NA_real_, x)
                }))

    websites <- data %>%
      dplyr::select(dplyr::matches("url")) %>% names()

    data <-
      data %>%
      mutate_at(websites,
                list(function(x) {
                  x %>% str_remove_all("\\http://|\\https://|https://|http://") %>% str_to_lower()
                })) %>%
      mutate_at(websites,
                list(function(x) {
                  str_c("https://", x)
                })) %>%
      mutate_at(websites,
                list(function(x) {
                  ifelse(x == "https://0", NA_character_, x)
                }))

    amount_cols <- data %>%
      dplyr::select(dplyr::matches("amount")) %>% names()

    data <- data %>%
      mutate_at(amount_cols,
                list(function(x){
                  x %>% formattable::currency(digits = 0)
                }))

    pct_cols <- data %>%
      dplyr::select(dplyr::matches("pct")) %>% names()

    data <-
      data %>%
      mutate_at(pct_cols,
                list(function(x){
                  (as.numeric(x) / 100 )
                })) %>%
      mutate_at(pct_cols,
                list(function(x){
                  x %>% percent(digits = 2)
                }))

    count_cols <-
      data %>%
      dplyr::select(dplyr::matches("^count[A-Z]")) %>%
      dplyr::select(-dplyr::matches("county")) %>%
      names()

    data <-
      data %>%
      mutate_at(count_cols,
                list(function(x){
                  comma(x, digits = 0)
                }))

    data <- data %>%
      mutate_if(is.character,
                list(function(x){
                  ifelse(x == "0", NA_character_, x)
                })
      )

    data <-
      data %>%
      mutate_at(
        data %>% select_if(is.character) %>% dplyr::select(-dplyr::matches("url")) %>% names(),
        str_to_upper
      )

    data %>%
      dplyr::select(
        idFDIC,
        idFederalReserve,
        nameBankHoldingCompany,
        nameTradeInstitution,
        urlInstitution,
        dplyr::matches("amount|pct"),
        everything()
      )

    data




  }

#' United States Bank Data
#'
#' Information about United States Banks as monitored by the FDIC
#'
#' @return
#' @export
#'
#' @examples
us_banks <-
  memoise::memoise(function() {
    data <-
      "https://cg-8f5302ff-11ad-4d26-a9b9-7c7ebcd6f322.s3-us-gov-west-1.amazonaws.com/downloads/institutions.csv" %>%
      read_csv() %>%
      dplyr::select(which(colMeans(is.na(.)) < 1)) %>%
      .assign_fdic_names()

  date_cols <- data %>% select(matches("date")) %>% names()

  if (length(date_cols) > 0) {
    data <- data %>%
      mutate_at(date_cols, mdy)
  }

  log_cols <-
    data %>% select(matches("^is|^has")) %>% select_if(is.character) %>% names()

  if (length(log_cols) > 0) {
    data <- data %>%
      mutate_at(log_cols,
        list(function(x) {
          case_when(x == "N" ~ F,
            x == "Y" ~ T)
        }))
  }

  log_cols <-
    data %>% select(matches("^is|^has")) %>% select_if(is.numeric) %>% names()

  if (length(log_cols) > 0) {
    data <-
      data %>%
      mutate_at(log_cols,
        as.logical)
  }

  id_cols <- data %>% select(matches("^id[A-Z]")) %>% select_if(is.numeric) %>% names()

  if (length(id_cols)  > 0) {
    data <- data %>%
      mutate_at(id_cols,
        list(function(x) {
          case_when(x == 0 ~ NA_real_,
            TRUE ~ as.numeric(x))
        }))
  }

  data <- data %>%
    select(one_of(
      c(
        "idFDIC",
        "idFederalReserve",
        "nameInstitution",
        "nameBankHoldingCompany",
        "dateEstablished"
      )
    ), everything())

  data
})