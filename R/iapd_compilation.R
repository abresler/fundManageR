# IAPD Compilation XML parser — structured Items 1-11 for every SEC-registered
# adviser. Replaces the broken HTML DOM/offset scanner for Items 1-11.
#
# Source:
#   https://reports.adviserinfo.sec.gov/reports/CompilationReports/IA_FIRM_SEC_Feed_<MM_DD_YYYY>.xml.gz
#   Daily refresh. ~77MB gz, ~80MB extracted. SEC-published authoritative.
#
# Coverage (per adviserinfo.sec.gov/compilation documentation):
#   Item 1  Identifying Information
#   Item 2  SEC Registration (A-B)
#   Item 3  Form of Organization (A-C)
#   Item 5  Advisory Business Information (A-L)
#   Item 6  Other Business Activities (A-B)
#   Item 7  Financial Industry Affiliations (A) + Private Fund Reporting flag (B)
#   Item 8  Participation or Interest in Client Transactions (A-I)
#   Item 9  Custody (A-F)
#   Item 10 Control Persons (A)
#   Item 11 Disclosure Information (and subitems A-H)
#
# NOT in this feed (still requires per-CRD scrape):
#   - Item 4 Successions
#   - Item 7.B per-fund detail (GP, custodian, auditor, marketers) — requires Schedule D
#   - Schedule A (Direct Owners)
#   - Schedule B (Indirect Owners)
#   - Schedule D details (private fund specifics)
#   - Brochures (Part 2)
#
# The IAPD State Adviser feed contains state-registered advisers, same schema.

.iapd_compilation_url <- function(date = Sys.Date(), type = c("SEC", "STATE")) {
  type <- match.arg(type)
  mmddyyyy <- format(as.Date(date), "%m_%d_%Y")
  glue::glue("https://reports.adviserinfo.sec.gov/reports/CompilationReports/IA_FIRM_{type}_Feed_{mmddyyyy}.xml.gz")
}

#' Download the daily IAPD compilation XML feed
#'
#' @param date Date of the feed to fetch. Defaults to today.
#' @param type "SEC" (registered + ERA with SEC) or "STATE" (state-registered).
#' @param dest destination file path (uncompressed .xml). If NULL a tempfile.
#' @param ua optional User-Agent; defaults to \code{FUND_MANAGER_UA} env var.
#' @return character path to the extracted XML
#' @export
#' @family iapd compilation
iapd_compilation_download <- function(date = Sys.Date(),
                                      type = c("SEC", "STATE"),
                                      dest = NULL,
                                      ua = NULL) {
  type <- match.arg(type)
  if (is.null(dest)) dest <- tempfile(fileext = ".xml")
  gz <- paste0(dest, ".gz")
  if (is.null(ua)) ua <- Sys.getenv("FUND_MANAGER_UA",
                                    "SHELDON Research Alex Bresler alexbresler@pwcommunications.com")
  h <- curl::new_handle()
  curl::handle_setheaders(h, `User-Agent` = ua, Accept = "application/xml")
  url <- .iapd_compilation_url(date, type)
  curl::curl_download(url, destfile = gz, handle = h, quiet = TRUE)
  R.utils::gunzip(gz, destname = dest, overwrite = TRUE, remove = TRUE)
  dest
}

# Convert Y/N to scalar logical; NULL or missing attribute -> NA.
.yn <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA)
  if (is.na(x)) return(NA)
  x == "Y"
}
.as_num <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_real_)
  suppressWarnings(as.numeric(x))
}
.as_int <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_integer_)
  suppressWarnings(as.integer(x))
}
.as_chr <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  as.character(x)
}

# Pull all attributes of a child element as a named list.
.attrs_of <- function(parent, xpath) {
  n <- xml2::xml_find_first(parent, xpath)
  if (inherits(n, "xml_missing")) return(list())
  as.list(xml2::xml_attrs(n))
}

# Parse one <Firm> node into a single-row tibble (long column set).
.parse_firm_node <- function(firm) {
  info  <- .attrs_of(firm, "Info")
  addr  <- .attrs_of(firm, "MainAddr")
  mail  <- .attrs_of(firm, "MailingAddr")
  rgstn <- .attrs_of(firm, "Rgstn")
  filing <- .attrs_of(firm, "Filing")

  i1  <- .attrs_of(firm, "FormInfo/Part1A/Item1")
  i2a <- .attrs_of(firm, "FormInfo/Part1A/Item2A")
  i2b <- .attrs_of(firm, "FormInfo/Part1A/Item2B")
  i3a <- .attrs_of(firm, "FormInfo/Part1A/Item3A")
  i3b <- .attrs_of(firm, "FormInfo/Part1A/Item3B")
  i3c <- .attrs_of(firm, "FormInfo/Part1A/Item3C")
  i5a <- .attrs_of(firm, "FormInfo/Part1A/Item5A")
  i5b <- .attrs_of(firm, "FormInfo/Part1A/Item5B")
  i5c <- .attrs_of(firm, "FormInfo/Part1A/Item5C")
  i5d <- .attrs_of(firm, "FormInfo/Part1A/Item5D")
  i5e <- .attrs_of(firm, "FormInfo/Part1A/Item5E")
  i5f <- .attrs_of(firm, "FormInfo/Part1A/Item5F")
  i5g <- .attrs_of(firm, "FormInfo/Part1A/Item5G")
  i5h <- .attrs_of(firm, "FormInfo/Part1A/Item5H")
  i5i <- .attrs_of(firm, "FormInfo/Part1A/Item5I")
  i5j <- .attrs_of(firm, "FormInfo/Part1A/Item5J")
  i5k <- .attrs_of(firm, "FormInfo/Part1A/Item5K")
  i5l <- .attrs_of(firm, "FormInfo/Part1A/Item5L")
  i6a <- .attrs_of(firm, "FormInfo/Part1A/Item6A")
  i6b <- .attrs_of(firm, "FormInfo/Part1A/Item6B")
  i7a <- .attrs_of(firm, "FormInfo/Part1A/Item7A")
  i7b <- .attrs_of(firm, "FormInfo/Part1A/Item7B")
  i9a <- .attrs_of(firm, "FormInfo/Part1A/Item9A")
  i9b <- .attrs_of(firm, "FormInfo/Part1A/Item9B")
  i9c <- .attrs_of(firm, "FormInfo/Part1A/Item9C")
  i9f <- .attrs_of(firm, "FormInfo/Part1A/Item9F")
  i10a <- .attrs_of(firm, "FormInfo/Part1A/Item10A")
  i11   <- .attrs_of(firm, "FormInfo/Part1A/Item11")

  # Web addresses (Item 1 sub-element)
  web_nodes <- xml2::xml_find_all(firm, "FormInfo/Part1A/Item1/WebAddrs/WebAddr")
  urls <- if (length(web_nodes)) xml2::xml_text(web_nodes) else character(0)

  tibble::tibble(
    # --- identity ---
    id_crd                   = .as_int(info$FirmCrdNb),
    id_sec                   = info$SECNb,
    name_entity_manager          = info$BusNm,
    name_entity_manager_business = info$BusNm,
    name_entity_manager_legal    = info$LegalNm,
    id_region_sec            = info$SECRgnCD,
    is_umbrella_registration = .yn(info$UmbrRgstn),

    # --- main address ---
    address_street_1_office_primary = addr$Strt1,
    address_street_2_office_primary = addr$Strt2,
    city_office_primary      = addr$City,
    state_office_primary     = addr$State,
    country_office_primary   = addr$Cntry,
    zip_office_primary       = addr$PostlCd,
    phone_office_primary     = addr$PhNb,
    fax_office_primary       = addr$FaxNb,

    # --- registration ---
    type_firm                = rgstn$FirmType,
    status_sec               = rgstn$St,
    date_status_sec          = rgstn$Dt,
    date_filing_adv_latest   = filing$Dt,
    version_form_adv         = filing$FormVrsn,

    # --- Item 1: Identifying Information ---
    count_advisory_offices              = .as_int(i1$Q1F5),
    is_firm_has_website                 = .yn(i1$Q1I),
    is_firm_has_chief_compliance_officer = .yn(i1$Q1M),
    is_firm_public_reporting            = .yn(i1$Q1N),
    is_firm_wholly_owned_subsidiary     = .yn(i1$Q1O),
    urls_firm                           = list(urls),

    # --- Item 2A: SEC Registration grounds ---
    is_sec_reg_large_aum_100m          = .yn(i2a$Q2A1),
    is_sec_reg_mid_aum_25m_100m        = .yn(i2a$Q2A2),
    is_sec_reg_nationally_recognized   = .yn(i2a$Q2A4),
    is_sec_reg_pension_consultant      = .yn(i2a$Q2A5),
    is_sec_reg_related_adviser         = .yn(i2a$Q2A6),
    is_sec_reg_newly_formed            = .yn(i2a$Q2A7),
    is_sec_reg_multi_state             = .yn(i2a$Q2A8),
    is_sec_reg_internet_adviser        = .yn(i2a$Q2A9),
    is_sec_reg_has_own_rule            = .yn(i2a$Q2A10),
    is_sec_reg_expecting_aum_100m      = .yn(i2a$Q2A11),
    is_sec_reg_exempt_pooled_investment = .yn(i2a$Q2A12),
    is_sec_reg_non_resident            = .yn(i2a$Q2A13),

    # --- Item 3: Form of Organization ---
    name_organization_form       = i3a$OrgFormNm,
    month_fiscal_year_end        = i3b$Q3B,
    state_organization           = i3c$StateCD,
    country_organization         = i3c$CntryNm,

    # --- Item 5A: Total Employees ---
    count_employees_total                         = .as_int(i5a$TtlEmp),

    # --- Item 5B: Employee breakdown (Q5B6 is firm count, alias kept for scrape compat) ---
    count_employees_investment_advisory           = .as_int(i5b$Q5B1),
    count_employees_broker_dealer                 = .as_int(i5b$Q5B2),
    count_employees_state_registered_adviser      = .as_int(i5b$Q5B3),
    count_employees_registered_adviser_other      = .as_int(i5b$Q5B4),
    count_employees_licensed_insurance_agents     = .as_int(i5b$Q5B5),
    count_employees_solicit_advisory_clients      = .as_int(i5b$Q5B6),
    count_firms_solicit_advisory_clients          = .as_int(i5b$Q5B6),

    # --- Item 5C: Clients without AUM + non-US % ---
    count_clients_without_aum                     = .as_int(i5c$Q5C1),
    pct_clients_non_us                            = .as_num(i5c$Q5C2) / 100,

    # --- Item 5D: client-type counts + pooled-vehicle AUM ---
    count_clients_individuals_other               = .as_int(i5d$Q5DA1),
    count_clients_individuals_hnw                 = .as_int(i5d$Q5DB1),
    count_clients_banks_thrifts                   = .as_int(i5d$Q5DC1),
    count_clients_investment_companies            = .as_int(i5d$Q5DD1),
    count_clients_business_dev_companies          = .as_int(i5d$Q5DE1),
    count_clients_pooled_investment_vehicles      = .as_int(i5d$Q5DF1),
    count_clients_pension_profit_sharing          = .as_int(i5d$Q5DG1),
    count_clients_charitable_orgs                 = .as_int(i5d$Q5DH1),
    count_clients_state_municipal_govt            = .as_int(i5d$Q5DI1),
    count_clients_other_advisers                  = .as_int(i5d$Q5DJ1),
    count_clients_insurance_companies             = .as_int(i5d$Q5DK1),
    count_clients_sovereign_wealth_funds          = .as_int(i5d$Q5DL1),
    count_clients_corporations_businesses         = .as_int(i5d$Q5DM1),
    count_clients_other                           = .as_int(i5d$Q5DN1),

    amount_aum_individuals_other                  = .as_num(i5d$Q5DA3),
    amount_aum_individuals_hnw                    = .as_num(i5d$Q5DB3),
    amount_aum_banks_thrifts                      = .as_num(i5d$Q5DC3),
    amount_aum_investment_companies               = .as_num(i5d$Q5DD3),
    amount_aum_business_dev_companies             = .as_num(i5d$Q5DE3),
    amount_aum_pooled_investment_vehicles         = .as_num(i5d$Q5DF3),
    amount_aum_pension_profit_sharing             = .as_num(i5d$Q5DG3),
    amount_aum_charitable_orgs                    = .as_num(i5d$Q5DH3),
    amount_aum_state_municipal_govt               = .as_num(i5d$Q5DI3),
    amount_aum_other_advisers                     = .as_num(i5d$Q5DJ3),
    amount_aum_insurance_companies                = .as_num(i5d$Q5DK3),
    amount_aum_sovereign_wealth_funds             = .as_num(i5d$Q5DL3),
    amount_aum_corporations_businesses            = .as_num(i5d$Q5DM3),
    amount_aum_other                              = .as_num(i5d$Q5DN3),

    # --- Item 5E: Compensation methods ---
    has_fee_aum                                   = .yn(i5e$Q5E1),
    has_fee_hourly                                = .yn(i5e$Q5E2),
    has_fee_subscription                          = .yn(i5e$Q5E3),
    has_fee_fixed                                 = .yn(i5e$Q5E4),
    has_fee_commission                            = .yn(i5e$Q5E5),
    has_fee_performance                           = .yn(i5e$Q5E6),
    has_fee_other                                 = .yn(i5e$Q5E7),

    # --- Item 5F: AUM + account counts ---
    is_securities_portfolio_manager              = .yn(i5f$Q5F1),
    amount_aum_discretionary                     = .as_num(i5f$Q5F2A),
    amount_aum_non_discretionary                 = .as_num(i5f$Q5F2B),
    amount_aum_total                             = .as_num(i5f$Q5F2C),
    count_accounts_discretionary                 = .as_int(i5f$Q5F2D),
    count_accounts_non_discretionary             = .as_int(i5f$Q5F2E),
    count_accounts_total                         = .as_int(i5f$Q5F2F),
    amount_aum_non_us_clients                    = .as_num(i5f$Q5F3),

    # --- Item 5G: Advisory services types (flags) ---
    has_service_financial_planning                      = .yn(i5g$Q5G1),
    has_service_portfolio_management_individuals        = .yn(i5g$Q5G2),
    has_portfolio_management_individual_small_business  = .yn(i5g$Q5G2),
    has_service_portfolio_management_inv_companies      = .yn(i5g$Q5G3),
    has_portfolio_management_institutional_clients      = .yn(i5g$Q5G3),
    has_service_portfolio_management_pooled             = .yn(i5g$Q5G4),
    has_portfolio_management_pooled_investment_vehicles = .yn(i5g$Q5G4),
    has_service_portfolio_management_other              = .yn(i5g$Q5G5),
    has_service_pension_consulting                      = .yn(i5g$Q5G6),
    has_service_adviser_selection                       = .yn(i5g$Q5G7),
    has_service_publication                             = .yn(i5g$Q5G8),
    has_service_educational_seminars                    = .yn(i5g$Q5G9),
    has_service_security_rating                         = .yn(i5g$Q5G10),
    has_service_market_timing                           = .yn(i5g$Q5G11),
    has_service_other                                   = .yn(i5g$Q5G12),
    is_securities_portfolio_management                  = .yn(i5f$Q5F1),
    has_securities_portfolio_management                 = .yn(i5f$Q5F1),
    type_other                                          = .as_chr(i5g$Q5G12Desc),

    # --- Item 5H: Investment advice limitations ---
    has_investment_advice_limited                = .yn(i5h$Q5H),

    # --- Item 5I: Wrap fee sponsor ---
    has_fee_wrap_sponsor                         = .yn(i5i$Q5I1),

    # --- Item 5J: Different client report / other 5D AUM ---
    has_different_client_report_method           = .yn(i5j$Q5J1),
    has_aum_other_5_d                            = .yn(i5j$Q5J2),

    # --- Item 5K: Separately managed accounts ---
    has_custodian_10_pct                         = .yn(i5k$Q5K1),
    has_seperate_account_margin                  = .yn(i5k$Q5K2),
    has_seperate_account_dervatives              = .yn(i5k$Q5K3),
    has_seperate_account_derivatives             = .yn(i5k$Q5K3),

    # --- Item 5L: Financial planning client range ---
    has_less_than_5_clients_individual_high_net_worth = .yn(i5l$Q5L1A),
    has_less_than_5_clients_pension_plan              = .yn(i5l$Q5L1B),
    has_less_than_5_clients_corporation_other         = .yn(i5l$Q5L1C),
    has_less_than_5_clients_soverign_wealth_fund      = .yn(i5l$Q5L1D),
    range_clients_financial_planning_zero             = .yn(i5l$Q5L2),

    # --- Item 6: Other Business Activities ---
    has_other_business_registered                = .yn(i6b$Q6B1),
    has_other_business_unregistered              = .yn(i6b$Q6B3),

    # --- Item 7A: Financial Industry Affiliations ---
    has_affil_broker_dealer                      = .yn(i7a$Q7A1),
    has_affil_investment_adviser                 = .yn(i7a$Q7A2),
    has_affil_bank                               = .yn(i7a$Q7A3),
    has_affil_trust_company                      = .yn(i7a$Q7A4),
    has_affil_savings_institution                = .yn(i7a$Q7A5),
    has_affil_insurance_company                  = .yn(i7a$Q7A6),
    has_affil_real_estate_broker                 = .yn(i7a$Q7A7),
    has_affil_futures_commission_merchant        = .yn(i7a$Q7A8),
    has_affil_commodity_pool_operator            = .yn(i7a$Q7A9),
    has_affil_accountant                         = .yn(i7a$Q7A10),
    has_affil_lawyer                             = .yn(i7a$Q7A11),
    has_affil_pension_consultant                 = .yn(i7a$Q7A12),
    has_affil_other_adviser                      = .yn(i7a$Q7A13),
    has_affil_other_investment                   = .yn(i7a$Q7A14),
    has_affil_trust_trustee                      = .yn(i7a$Q7A15),
    has_affil_private_fund_manager               = .yn(i7a$Q7A16),

    # --- Item 7B: Private Fund Flag ---
    has_private_funds                            = .yn(i7b$Q7B),

    # --- Item 9: Custody ---
    amount_aum_custody_discretionary             = .as_num(i9a$Q9A2A),
    count_clients_custody_discretionary          = .as_int(i9a$Q9A2B),
    amount_aum_custody_non_discretionary         = .as_num(i9b$Q9B2A),
    count_clients_custody_non_discretionary      = .as_int(i9b$Q9B2B),

    # --- Item 10: Control Persons ---
    is_control_person_exists                     = .yn(i10a$Q10A),

    # --- Item 11: Disclosure Information ---
    is_disclosure_question                        = .yn(i11$Q11),

    # --- meta ---
    source_parser = "iapd_compilation_xml"
  )
}

#' Parse the SEC IAPD compilation XML feed
#'
#' Authoritative source for Form ADV Items 1-11 on every SEC-registered
#' adviser and SEC ERA. Replaces the broken HTML DOM scanner for these items.
#'
#' @param xml_path path to the extracted compilation XML
#'   (\code{iapd_compilation_download()} fetches + unzips).
#' @param crd_ids optional integer vector; if provided, only those CRDs are
#'   returned.
#' @param name_pattern optional case-insensitive substring to match against
#'   firm business/legal name (e.g. "BLACKSTONE" or "ROCKWOOD"). Combined
#'   with \code{crd_ids} as a logical OR.
#' @return tibble one row per firm, with ~130 structured columns
#' @export
#' @family iapd compilation
#' @examples
#' \dontrun{
#' # One-time daily download (~77 MB gz -> ~80 MB XML)
#' xml_path <- iapd_compilation_download()
#'
#' # Spot-checks against known real-estate and credit managers:
#' #   156663 Rockwood Capital LLC
#' #   138854 EJF Capital LP
#' #   328584 Shorenstein Investment Advisers LLC
#' #   142979 Blackstone Real Estate Advisers L.P.
#' targets <- parse_iapd_compilation_xml(
#'   xml_path,
#'   crd_ids = c(156663L, 138854L, 328584L, 142979L)
#' )
#'
#' # Every Blackstone-related adviser in one call (30+ rows as of 2026-04):
#' bx <- parse_iapd_compilation_xml(xml_path, name_pattern = "BLACKSTONE")
#' bx %>% dplyr::arrange(dplyr::desc(amount_aum_total))
#'
#' # Full SEC universe
#' all_sec <- parse_iapd_compilation_xml(xml_path)
#'
#' # Westbrook Partners is NOT SEC-registered; use the STATE feed:
#' state_path <- iapd_compilation_download(type = "STATE")
#' westbrook <- parse_iapd_compilation_xml(state_path, name_pattern = "WESTBROOK")
#' }
#' Parse BOTH the SEC and STATE IAPD feeds for full IAPD universe coverage
#'
#' SEC feed covers SEC-registered advisers + SEC ERAs (\code{~23K} firms).
#' STATE feed covers state-registered advisers + state ERAs (\code{~17K} firms).
#' Combined = the full IAPD adviser universe.
#'
#' Some managers (e.g. Westbrook Partners) are state-registered only and will
#' NOT appear in the SEC feed.
#'
#' @param date feed date
#' @param crd_ids optional integer vector
#' @param name_pattern optional case-insensitive substring on BusNm / LegalNm
#' @return tibble with a \code{source_feed} column distinguishing "SEC" vs
#'   "STATE" origin for every row
#' @export
#' @family iapd compilation
parse_iapd_compilation_universe <- function(date = Sys.Date(), crd_ids = NULL, name_pattern = NULL) {
  sec_path   <- iapd_compilation_download(date = date, type = "SEC")
  state_path <- iapd_compilation_download(date = date, type = "STATE")
  sec_df   <- parse_iapd_compilation_xml(sec_path,   crd_ids = crd_ids, name_pattern = name_pattern)
  state_df <- parse_iapd_compilation_xml(state_path, crd_ids = crd_ids, name_pattern = name_pattern)
  if (nrow(sec_df))   sec_df$source_feed   <- "SEC"
  if (nrow(state_df)) state_df$source_feed <- "STATE"
  dplyr::bind_rows(sec_df, state_df)
}

# Batch extractor: one xml_find_all per item-element across ALL firms, then
# column-bind by position. Assumes each <Firm> contains 0 or 1 of each
# <ItemX> element so alignment-by-parent works. Falls back to NA for firms
# missing an item.
.parse_firm_batch <- function(firms) {
  # Each firm gets an ordinal. For each Item path, xml_find_first returns the
  # node (or xml_missing) — aligned 1:1 with firms.
  pull <- function(rel_path, attr_name) {
    nodes <- xml2::xml_find_first(firms, rel_path)
    xml2::xml_attr(nodes, attr_name)  # NA for missing nodes
  }
  pull_text <- function(rel_path) {
    nodes <- xml2::xml_find_first(firms, rel_path)
    out <- xml2::xml_text(nodes)
    ifelse(nchar(out) == 0 & is.na(out), NA_character_, out)
  }

  n <- length(firms)
  df <- tibble::tibble(
    # identity
    id_crd                = suppressWarnings(as.integer(pull("Info", "FirmCrdNb"))),
    id_sec                = pull("Info", "SECNb"),
    name_entity_manager          = pull("Info", "BusNm"),
    name_entity_manager_business = pull("Info", "BusNm"),
    name_entity_manager_legal    = pull("Info", "LegalNm"),
    id_region_sec                = pull("Info", "SECRgnCD"),
    is_umbrella_registration     = pull("Info", "UmbrRgstn") == "Y",

    # main address
    address_street_1_office_primary = pull("MainAddr", "Strt1"),
    address_street_2_office_primary = pull("MainAddr", "Strt2"),
    city_office_primary      = pull("MainAddr", "City"),
    state_office_primary     = pull("MainAddr", "State"),
    country_office_primary   = pull("MainAddr", "Cntry"),
    zip_office_primary       = pull("MainAddr", "PostlCd"),
    phone_office_primary     = pull("MainAddr", "PhNb"),
    fax_office_primary       = pull("MainAddr", "FaxNb"),

    # registration
    type_firm                = pull("Rgstn", "FirmType"),
    status_sec               = pull("Rgstn", "St"),
    date_status_sec          = pull("Rgstn", "Dt"),
    date_filing_adv_latest   = pull("Filing", "Dt"),
    version_form_adv         = pull("Filing", "FormVrsn"),

    # Item 1
    count_advisory_offices               = suppressWarnings(as.integer(pull("FormInfo/Part1A/Item1", "Q1F5"))),
    is_firm_has_website                  = pull("FormInfo/Part1A/Item1", "Q1I") == "Y",
    is_firm_has_chief_compliance_officer = pull("FormInfo/Part1A/Item1", "Q1M") == "Y",
    is_firm_public_reporting             = pull("FormInfo/Part1A/Item1", "Q1N") == "Y",
    is_firm_wholly_owned_subsidiary      = pull("FormInfo/Part1A/Item1", "Q1O") == "Y",

    # Item 2A — SEC registration grounds
    is_sec_reg_large_aum_100m           = pull("FormInfo/Part1A/Item2A", "Q2A1") == "Y",
    is_sec_reg_mid_aum_25m_100m         = pull("FormInfo/Part1A/Item2A", "Q2A2") == "Y",
    is_sec_reg_nationally_recognized    = pull("FormInfo/Part1A/Item2A", "Q2A4") == "Y",
    is_sec_reg_pension_consultant       = pull("FormInfo/Part1A/Item2A", "Q2A5") == "Y",
    is_sec_reg_related_adviser          = pull("FormInfo/Part1A/Item2A", "Q2A6") == "Y",
    is_sec_reg_newly_formed             = pull("FormInfo/Part1A/Item2A", "Q2A7") == "Y",
    is_sec_reg_multi_state              = pull("FormInfo/Part1A/Item2A", "Q2A8") == "Y",
    is_sec_reg_internet_adviser         = pull("FormInfo/Part1A/Item2A", "Q2A9") == "Y",
    is_sec_reg_has_own_rule             = pull("FormInfo/Part1A/Item2A", "Q2A10") == "Y",
    is_sec_reg_expecting_aum_100m       = pull("FormInfo/Part1A/Item2A", "Q2A11") == "Y",
    is_sec_reg_exempt_pooled_investment = pull("FormInfo/Part1A/Item2A", "Q2A12") == "Y",
    is_sec_reg_non_resident             = pull("FormInfo/Part1A/Item2A", "Q2A13") == "Y",

    # Item 3
    name_organization_form       = pull("FormInfo/Part1A/Item3A", "OrgFormNm"),
    month_fiscal_year_end        = pull("FormInfo/Part1A/Item3B", "Q3B"),
    state_organization           = pull("FormInfo/Part1A/Item3C", "StateCD"),
    country_organization         = pull("FormInfo/Part1A/Item3C", "CntryNm"),

    # Item 5A / 5B
    count_employees_total                         = suppressWarnings(as.integer(pull("FormInfo/Part1A/Item5A", "TtlEmp"))),
    count_employees_investment_advisory           = suppressWarnings(as.integer(pull("FormInfo/Part1A/Item5B", "Q5B1"))),
    count_employees_broker_dealer                 = suppressWarnings(as.integer(pull("FormInfo/Part1A/Item5B", "Q5B2"))),
    count_employees_state_registered_adviser      = suppressWarnings(as.integer(pull("FormInfo/Part1A/Item5B", "Q5B3"))),
    count_employees_registered_adviser_other      = suppressWarnings(as.integer(pull("FormInfo/Part1A/Item5B", "Q5B4"))),
    count_employees_licensed_insurance_agents     = suppressWarnings(as.integer(pull("FormInfo/Part1A/Item5B", "Q5B5"))),
    count_employees_solicit_advisory_clients      = suppressWarnings(as.integer(pull("FormInfo/Part1A/Item5B", "Q5B6"))),
    count_firms_solicit_advisory_clients          = suppressWarnings(as.integer(pull("FormInfo/Part1A/Item5B", "Q5B6"))),

    # Item 5C
    count_clients_without_aum                     = suppressWarnings(as.integer(pull("FormInfo/Part1A/Item5C", "Q5C1"))),
    pct_clients_non_us                            = suppressWarnings(as.numeric(pull("FormInfo/Part1A/Item5C", "Q5C2"))) / 100,

    # Item 5D — client counts
    count_clients_individuals_other               = suppressWarnings(as.integer(pull("FormInfo/Part1A/Item5D", "Q5DA1"))),
    count_clients_individuals_hnw                 = suppressWarnings(as.integer(pull("FormInfo/Part1A/Item5D", "Q5DB1"))),
    count_clients_banks_thrifts                   = suppressWarnings(as.integer(pull("FormInfo/Part1A/Item5D", "Q5DC1"))),
    count_clients_investment_companies            = suppressWarnings(as.integer(pull("FormInfo/Part1A/Item5D", "Q5DD1"))),
    count_clients_business_dev_companies          = suppressWarnings(as.integer(pull("FormInfo/Part1A/Item5D", "Q5DE1"))),
    count_clients_pooled_investment_vehicles      = suppressWarnings(as.integer(pull("FormInfo/Part1A/Item5D", "Q5DF1"))),
    count_clients_pension_profit_sharing          = suppressWarnings(as.integer(pull("FormInfo/Part1A/Item5D", "Q5DG1"))),
    count_clients_charitable_orgs                 = suppressWarnings(as.integer(pull("FormInfo/Part1A/Item5D", "Q5DH1"))),
    count_clients_state_municipal_govt            = suppressWarnings(as.integer(pull("FormInfo/Part1A/Item5D", "Q5DI1"))),
    count_clients_other_advisers                  = suppressWarnings(as.integer(pull("FormInfo/Part1A/Item5D", "Q5DJ1"))),
    count_clients_insurance_companies             = suppressWarnings(as.integer(pull("FormInfo/Part1A/Item5D", "Q5DK1"))),
    count_clients_sovereign_wealth_funds          = suppressWarnings(as.integer(pull("FormInfo/Part1A/Item5D", "Q5DL1"))),
    count_clients_corporations_businesses         = suppressWarnings(as.integer(pull("FormInfo/Part1A/Item5D", "Q5DM1"))),
    count_clients_other                           = suppressWarnings(as.integer(pull("FormInfo/Part1A/Item5D", "Q5DN1"))),

    # Item 5D — AUM per client type
    amount_aum_individuals_other                  = suppressWarnings(as.numeric(pull("FormInfo/Part1A/Item5D", "Q5DA3"))),
    amount_aum_individuals_hnw                    = suppressWarnings(as.numeric(pull("FormInfo/Part1A/Item5D", "Q5DB3"))),
    amount_aum_banks_thrifts                      = suppressWarnings(as.numeric(pull("FormInfo/Part1A/Item5D", "Q5DC3"))),
    amount_aum_investment_companies               = suppressWarnings(as.numeric(pull("FormInfo/Part1A/Item5D", "Q5DD3"))),
    amount_aum_business_dev_companies             = suppressWarnings(as.numeric(pull("FormInfo/Part1A/Item5D", "Q5DE3"))),
    amount_aum_pooled_investment_vehicles         = suppressWarnings(as.numeric(pull("FormInfo/Part1A/Item5D", "Q5DF3"))),
    amount_aum_pension_profit_sharing             = suppressWarnings(as.numeric(pull("FormInfo/Part1A/Item5D", "Q5DG3"))),
    amount_aum_charitable_orgs                    = suppressWarnings(as.numeric(pull("FormInfo/Part1A/Item5D", "Q5DH3"))),
    amount_aum_state_municipal_govt               = suppressWarnings(as.numeric(pull("FormInfo/Part1A/Item5D", "Q5DI3"))),
    amount_aum_other_advisers                     = suppressWarnings(as.numeric(pull("FormInfo/Part1A/Item5D", "Q5DJ3"))),
    amount_aum_insurance_companies                = suppressWarnings(as.numeric(pull("FormInfo/Part1A/Item5D", "Q5DK3"))),
    amount_aum_sovereign_wealth_funds             = suppressWarnings(as.numeric(pull("FormInfo/Part1A/Item5D", "Q5DL3"))),
    amount_aum_corporations_businesses            = suppressWarnings(as.numeric(pull("FormInfo/Part1A/Item5D", "Q5DM3"))),
    amount_aum_other                              = suppressWarnings(as.numeric(pull("FormInfo/Part1A/Item5D", "Q5DN3"))),

    # Item 5E — fees
    has_fee_aum          = pull("FormInfo/Part1A/Item5E", "Q5E1") == "Y",
    has_fee_hourly       = pull("FormInfo/Part1A/Item5E", "Q5E2") == "Y",
    has_fee_subscription = pull("FormInfo/Part1A/Item5E", "Q5E3") == "Y",
    has_fee_fixed        = pull("FormInfo/Part1A/Item5E", "Q5E4") == "Y",
    has_fee_commission   = pull("FormInfo/Part1A/Item5E", "Q5E5") == "Y",
    has_fee_performance  = pull("FormInfo/Part1A/Item5E", "Q5E6") == "Y",
    has_fee_other        = pull("FormInfo/Part1A/Item5E", "Q5E7") == "Y",

    # Item 5F — AUM + accounts
    is_securities_portfolio_manager     = pull("FormInfo/Part1A/Item5F", "Q5F1") == "Y",
    is_securities_portfolio_management  = pull("FormInfo/Part1A/Item5F", "Q5F1") == "Y",
    has_securities_portfolio_management = pull("FormInfo/Part1A/Item5F", "Q5F1") == "Y",
    amount_aum_discretionary            = suppressWarnings(as.numeric(pull("FormInfo/Part1A/Item5F", "Q5F2A"))),
    amount_aum_non_discretionary        = suppressWarnings(as.numeric(pull("FormInfo/Part1A/Item5F", "Q5F2B"))),
    amount_aum_total                    = suppressWarnings(as.numeric(pull("FormInfo/Part1A/Item5F", "Q5F2C"))),
    count_accounts_discretionary        = suppressWarnings(as.integer(pull("FormInfo/Part1A/Item5F", "Q5F2D"))),
    count_accounts_non_discretionary    = suppressWarnings(as.integer(pull("FormInfo/Part1A/Item5F", "Q5F2E"))),
    count_accounts_total                = suppressWarnings(as.integer(pull("FormInfo/Part1A/Item5F", "Q5F2F"))),
    amount_aum_non_us_clients           = suppressWarnings(as.numeric(pull("FormInfo/Part1A/Item5F", "Q5F3"))),

    # Item 5G — services
    has_service_financial_planning                      = pull("FormInfo/Part1A/Item5G", "Q5G1") == "Y",
    has_service_portfolio_management_individuals        = pull("FormInfo/Part1A/Item5G", "Q5G2") == "Y",
    has_portfolio_management_individual_small_business  = pull("FormInfo/Part1A/Item5G", "Q5G2") == "Y",
    has_service_portfolio_management_inv_companies      = pull("FormInfo/Part1A/Item5G", "Q5G3") == "Y",
    has_portfolio_management_institutional_clients      = pull("FormInfo/Part1A/Item5G", "Q5G3") == "Y",
    has_service_portfolio_management_pooled             = pull("FormInfo/Part1A/Item5G", "Q5G4") == "Y",
    has_portfolio_management_pooled_investment_vehicles = pull("FormInfo/Part1A/Item5G", "Q5G4") == "Y",
    has_service_portfolio_management_other              = pull("FormInfo/Part1A/Item5G", "Q5G5") == "Y",
    has_service_pension_consulting                      = pull("FormInfo/Part1A/Item5G", "Q5G6") == "Y",
    has_service_adviser_selection                       = pull("FormInfo/Part1A/Item5G", "Q5G7") == "Y",
    has_service_publication                             = pull("FormInfo/Part1A/Item5G", "Q5G8") == "Y",
    has_service_educational_seminars                    = pull("FormInfo/Part1A/Item5G", "Q5G9") == "Y",
    has_service_security_rating                         = pull("FormInfo/Part1A/Item5G", "Q5G10") == "Y",
    has_service_market_timing                           = pull("FormInfo/Part1A/Item5G", "Q5G11") == "Y",
    has_service_other                                   = pull("FormInfo/Part1A/Item5G", "Q5G12") == "Y",
    type_other                                          = pull("FormInfo/Part1A/Item5G", "Q5G12Desc"),

    # Item 5H/I/J/K/L
    has_investment_advice_limited   = pull("FormInfo/Part1A/Item5H", "Q5H") == "Y",
    has_fee_wrap_sponsor            = pull("FormInfo/Part1A/Item5I", "Q5I1") == "Y",
    has_different_client_report_method = pull("FormInfo/Part1A/Item5J", "Q5J1") == "Y",
    has_aum_other_5_d               = pull("FormInfo/Part1A/Item5J", "Q5J2") == "Y",
    has_custodian_10_pct            = pull("FormInfo/Part1A/Item5K", "Q5K1") == "Y",
    has_seperate_account_margin     = pull("FormInfo/Part1A/Item5K", "Q5K2") == "Y",
    has_seperate_account_dervatives = pull("FormInfo/Part1A/Item5K", "Q5K3") == "Y",
    has_seperate_account_derivatives = pull("FormInfo/Part1A/Item5K", "Q5K3") == "Y",
    has_less_than_5_clients_individual_high_net_worth = pull("FormInfo/Part1A/Item5L", "Q5L1A") == "Y",
    has_less_than_5_clients_pension_plan              = pull("FormInfo/Part1A/Item5L", "Q5L1B") == "Y",
    has_less_than_5_clients_corporation_other         = pull("FormInfo/Part1A/Item5L", "Q5L1C") == "Y",
    has_less_than_5_clients_soverign_wealth_fund      = pull("FormInfo/Part1A/Item5L", "Q5L1D") == "Y",
    range_clients_financial_planning_zero             = pull("FormInfo/Part1A/Item5L", "Q5L2") == "Y",

    # Item 6
    has_other_business_registered   = pull("FormInfo/Part1A/Item6B", "Q6B1") == "Y",
    has_other_business_unregistered = pull("FormInfo/Part1A/Item6B", "Q6B3") == "Y",

    # Item 7A
    has_affil_broker_dealer               = pull("FormInfo/Part1A/Item7A", "Q7A1") == "Y",
    has_affil_investment_adviser          = pull("FormInfo/Part1A/Item7A", "Q7A2") == "Y",
    has_affil_bank                        = pull("FormInfo/Part1A/Item7A", "Q7A3") == "Y",
    has_affil_trust_company               = pull("FormInfo/Part1A/Item7A", "Q7A4") == "Y",
    has_affil_savings_institution         = pull("FormInfo/Part1A/Item7A", "Q7A5") == "Y",
    has_affil_insurance_company           = pull("FormInfo/Part1A/Item7A", "Q7A6") == "Y",
    has_affil_real_estate_broker          = pull("FormInfo/Part1A/Item7A", "Q7A7") == "Y",
    has_affil_futures_commission_merchant = pull("FormInfo/Part1A/Item7A", "Q7A8") == "Y",
    has_affil_commodity_pool_operator     = pull("FormInfo/Part1A/Item7A", "Q7A9") == "Y",
    has_affil_accountant                  = pull("FormInfo/Part1A/Item7A", "Q7A10") == "Y",
    has_affil_lawyer                      = pull("FormInfo/Part1A/Item7A", "Q7A11") == "Y",
    has_affil_pension_consultant          = pull("FormInfo/Part1A/Item7A", "Q7A12") == "Y",
    has_affil_other_adviser               = pull("FormInfo/Part1A/Item7A", "Q7A13") == "Y",
    has_affil_other_investment            = pull("FormInfo/Part1A/Item7A", "Q7A14") == "Y",
    has_affil_trust_trustee               = pull("FormInfo/Part1A/Item7A", "Q7A15") == "Y",
    has_affil_private_fund_manager        = pull("FormInfo/Part1A/Item7A", "Q7A16") == "Y",

    # Item 7B
    has_private_funds = pull("FormInfo/Part1A/Item7B", "Q7B") == "Y",

    # Item 9 — custody
    amount_aum_custody_discretionary        = suppressWarnings(as.numeric(pull("FormInfo/Part1A/Item9A", "Q9A2A"))),
    count_clients_custody_discretionary     = suppressWarnings(as.integer(pull("FormInfo/Part1A/Item9A", "Q9A2B"))),
    amount_aum_custody_non_discretionary    = suppressWarnings(as.numeric(pull("FormInfo/Part1A/Item9B", "Q9B2A"))),
    count_clients_custody_non_discretionary = suppressWarnings(as.integer(pull("FormInfo/Part1A/Item9B", "Q9B2B"))),

    # Item 10
    is_control_person_exists = pull("FormInfo/Part1A/Item10A", "Q10A") == "Y",

    # Item 11
    is_disclosure_question = pull("FormInfo/Part1A/Item11", "Q11") == "Y",

    source_parser = "iapd_compilation_xml"
  )

  # URLs — multi-valued; gather into list-column keyed by parent firm.
  # Build a map: for each <WebAddr> under //Firm[i]/FormInfo/Part1A/Item1/WebAddrs,
  # find parent <Firm> ordinal.
  web_nodes <- xml2::xml_find_all(firms, "FormInfo/Part1A/Item1/WebAddrs/WebAddr")
  if (length(web_nodes)) {
    web_parents <- xml2::xml_find_first(web_nodes, "ancestor::Firm[1]")
    # Map each firm node to its index in `firms`
    firm_ids <- vapply(firms, xml2::xml_path, character(1))
    parent_ids <- vapply(web_parents, xml2::xml_path, character(1))
    idx <- match(parent_ids, firm_ids)
    url_text <- xml2::xml_text(web_nodes)
    # Drop empty values BEFORE split so idx alignment is preserved.
    keep <- nzchar(trimws(url_text))
    urls_by_firm <- split(url_text[keep], idx[keep])
    df$urls_firm <- lapply(seq_len(n), function(i) {
      v <- urls_by_firm[[as.character(i)]]
      if (is.null(v)) character(0) else .clean_urls(v)
    })
  } else {
    df$urls_firm <- replicate(n, character(0), simplify = FALSE)
  }
  df
}

#' @rdname parse_iapd_compilation_xml
#' @export
parse_iapd_compilation_xml <- function(xml_path, crd_ids = NULL, name_pattern = NULL) {
  doc <- xml2::read_xml(xml_path)
  predicates <- character(0)
  if (length(crd_ids)) {
    predicates <- c(predicates,
                    paste(sprintf("Info/@FirmCrdNb=\"%s\"", as.integer(crd_ids)), collapse = " or "))
  }
  if (length(name_pattern) && nzchar(name_pattern)) {
    p <- toupper(name_pattern)
    predicates <- c(predicates, sprintf(
      "contains(translate(Info/@BusNm, \"abcdefghijklmnopqrstuvwxyz\", \"ABCDEFGHIJKLMNOPQRSTUVWXYZ\"), \"%s\") or contains(translate(Info/@LegalNm, \"abcdefghijklmnopqrstuvwxyz\", \"ABCDEFGHIJKLMNOPQRSTUVWXYZ\"), \"%s\")",
      p, p))
  }
  if (length(predicates)) {
    firms <- xml2::xml_find_all(doc, sprintf("//Firm[%s]", paste(predicates, collapse = " or ")))
  } else {
    firms <- xml2::xml_find_all(doc, "//Firm")
  }
  if (!length(firms)) return(tibble::tibble())
  .parse_firm_batch(firms)
}
