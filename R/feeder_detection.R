# ----------------------------------------------------------------------------
# feeder_detection.R — Bayesian feeder/master detection over Form ADV Section 7.B
#
# Operates on adv_funds-shape tibbles. Adds composite scoring for whether a
# fund row is likely a feeder of another row in the same adviser's filing.
# Source-of-truth: ~/obsidian/SHELDON Knowledge Bases/FoF_FamilyOffice_Foreign_Feeders/wiki/feeder_funds.md
#
# Composition: logit(prior) + Σ ln(LR_i) per signal, posterior via inverse logit.
# No internet calls; pure transformation over input data.
# ----------------------------------------------------------------------------

#' Detect feeder-fund rows in a Form ADV Section 7.B tibble
#'
#' Given a tibble of `adv_funds` rows from a single adviser CRD (or a stacked
#' tibble across many advisers), append columns flagging which rows are likely
#' feeders, which are likely masters, and the best-matched master candidate per
#' feeder row. Bayesian composite over five signals:
#'
#' \enumerate{
#'   \item Name-suffix patterns: `(Cayman)`, `(Offshore)`, `Master`, `Onshore`
#'   \item Same adviser CRD + same GP, distinct jurisdiction of incorporation
#'   \item Same auditor + administrator + prime broker (service-stack overlap)
#'   \item `is_master_fund` flag when populated (high-confidence direct flag)
#'   \item Negative LR for lone offshore rows with no candidate master
#' }
#'
#' @param data tibble. Must contain at minimum: `id_crd_batch` (or comparable
#'   adviser ID), `name_fund_clean` (or `name_fund` / `nameFund`),
#'   `location_fund_incorporation` (or `nameJurisdiction`).
#'   Optional but improves recall: `name_fund_gp_manager_trustee_director`,
#'   `name_administrator`, `name_custodian`, `name_fund_auditor`,
#'   `name_fund_prime_broker`, `is_master_fund`, `id_private_fund`.
#' @param prior numeric. Prior probability of feeder-status. Default 0.10
#'   (~10 percent of rows in a private-fund universe are feeders).
#' @param threshold_likely numeric. Posterior threshold for `is_likely_feeder`.
#'   Default 0.65.
#' @param fund_name_col character. Override auto-detection.
#' @param adviser_id_col character. Override auto-detection.
#' @param jurisdiction_col character. Override auto-detection.
#' @param gp_col character. Override auto-detection.
#' @param service_cols character vector. Override service-stack columns.
#'
#' @return Input tibble with appended columns:
#' \itemize{
#'   \item `is_likely_feeder` logical
#'   \item `is_likely_master` logical (paired with at least one likely feeder)
#'   \item `id_master_candidate` character (best-matched master row identifier)
#'   \item `score_feeder` numeric posterior 0-1
#'   \item `feeder_evidence` character JSON of which signals fired
#' }
#'
#' @examples
#' \dontrun{
#'   # From dq adv_funds parquet
#'   adv <- arrow::read_parquet("~/Desktop/data/sec_adv/section=section_7_b_private_fund_reporting/batch=2026-04/sec.zstd.parquet")
#'   flagged <- flag_feeder_funds(adv)
#'   flagged %>%
#'     dplyr::filter(is_likely_feeder) %>%
#'     dplyr::count(location_fund_incorporation, sort = TRUE)
#' }
#'
#' @export
#' @family ADV
#' @family feeder
flag_feeder_funds <- function(data,
                              prior = 0.10,
                              threshold_likely = 0.65,
                              fund_name_col = NULL,
                              adviser_id_col = NULL,
                              jurisdiction_col = NULL,
                              gp_col = NULL,
                              service_cols = NULL) {

  if (!inherits(data, "data.frame")) {
    stop("flag_feeder_funds() requires a data.frame / tibble")
  }
  if (nrow(data) == 0) {
    return(.append_empty_feeder_cols(data))
  }

  cols <- .resolve_feeder_columns(
    data,
    fund_name_col   = fund_name_col,
    adviser_id_col  = adviser_id_col,
    jurisdiction_col = jurisdiction_col,
    gp_col          = gp_col,
    service_cols    = service_cols
  )

  # ----- Per-row signal extraction ----------------------------------------
  fund_name      <- as.character(data[[cols$fund_name]])
  adviser_id     <- as.character(data[[cols$adviser_id]])
  jurisdiction   <- if (!is.null(cols$jurisdiction)) as.character(data[[cols$jurisdiction]]) else rep(NA_character_, nrow(data))
  gp_name        <- if (!is.null(cols$gp))           as.character(data[[cols$gp]])           else rep(NA_character_, nrow(data))

  # Service-stack triple
  svc_admin      <- if (!is.null(cols$svc[["administrator"]])) as.character(data[[cols$svc[["administrator"]]]]) else rep(NA_character_, nrow(data))
  svc_auditor    <- if (!is.null(cols$svc[["auditor"]]))       as.character(data[[cols$svc[["auditor"]]]])       else rep(NA_character_, nrow(data))
  svc_prime      <- if (!is.null(cols$svc[["prime_broker"]]))  as.character(data[[cols$svc[["prime_broker"]]]])  else rep(NA_character_, nrow(data))

  is_master_flag <- if ("is_master_fund" %in% names(data)) as.logical(data[["is_master_fund"]]) else rep(NA, nrow(data))

  # ----- Signal 1: name-suffix patterns -----------------------------------
  feeder_suffix_re <- "\\((Cayman|Offshore|BVI|Bermuda|Lux|Luxembourg|Ireland)\\)|\\bOffshore\\b|\\b\\(QP\\)\\b"
  master_suffix_re <- "\\bMaster\\s+Fund\\b|\\bMaster\\s+LP\\b|\\bMaster\\s+Ltd\\b|\\bMaster$"
  onshore_re       <- "\\bOnshore\\b|\\(US\\)\\s|\\(Domestic\\)|\\bDomestic\\s+Fund\\b"

  has_feeder_suffix <- grepl(feeder_suffix_re, fund_name, ignore.case = TRUE)
  has_master_suffix <- grepl(master_suffix_re, fund_name, ignore.case = TRUE)
  has_onshore_suffix <- grepl(onshore_re, fund_name, ignore.case = TRUE)

  # ----- Signal 2/3: cluster within (adviser, normalized stem) ------------
  name_stem <- .feeder_name_stem(fund_name)
  cluster_key <- paste(adviser_id, name_stem, sep = "||")

  # For each row, find sibling rows in the same cluster
  by_cluster <- split(seq_along(cluster_key), cluster_key)

  same_gp_diff_juris <- logical(nrow(data))
  service_stack_overlap <- logical(nrow(data))
  has_master_sibling <- logical(nrow(data))
  master_candidate_id <- rep(NA_character_, nrow(data))
  fund_id_col <- if ("id_private_fund" %in% names(data)) "id_private_fund" else NULL

  for (idx_vec in by_cluster) {
    if (length(idx_vec) < 2) next
    for (i in idx_vec) {
      siblings <- setdiff(idx_vec, i)

      # GP overlap with distinct jurisdiction
      if (!is.na(gp_name[i]) && nzchar(gp_name[i])) {
        gp_match <- !is.na(gp_name[siblings]) & gp_name[siblings] == gp_name[i]
        juris_diff <- !is.na(jurisdiction[siblings]) & !is.na(jurisdiction[i]) &
                      jurisdiction[siblings] != jurisdiction[i]
        if (any(gp_match & juris_diff, na.rm = TRUE)) {
          same_gp_diff_juris[i] <- TRUE
        }
      }

      # Service-stack triple overlap
      svc_match <- (
        !is.na(svc_admin[i])   & !is.na(svc_admin[siblings])   & svc_admin[siblings]   == svc_admin[i]
      ) & (
        !is.na(svc_auditor[i]) & !is.na(svc_auditor[siblings]) & svc_auditor[siblings] == svc_auditor[i]
      ) & (
        !is.na(svc_prime[i])   & !is.na(svc_prime[siblings])   & svc_prime[siblings]   == svc_prime[i]
      )
      if (any(svc_match, na.rm = TRUE)) {
        service_stack_overlap[i] <- TRUE
      }

      # Master sibling lookup — pick first sibling with master-suffix or is_master_flag
      master_idx <- siblings[
        has_master_suffix[siblings] |
        (!is.na(is_master_flag[siblings]) & is_master_flag[siblings])
      ]
      if (length(master_idx) > 0) {
        has_master_sibling[i] <- TRUE
        if (!is.null(fund_id_col)) {
          master_candidate_id[i] <- as.character(data[[fund_id_col]][master_idx[1]])
        } else {
          master_candidate_id[i] <- fund_name[master_idx[1]]
        }
      }
    }
  }

  # ----- Log-LR composition ------------------------------------------------
  # Weights from KB feeder_funds.md
  ln_lr_name_suffix       <- ifelse(has_feeder_suffix | has_onshore_suffix, 2.5, 0)
  ln_lr_gp_juris_split    <- ifelse(same_gp_diff_juris, 2.0, 0)
  ln_lr_svc_overlap       <- ifelse(service_stack_overlap, 1.5, 0)
  ln_lr_master_flag       <- ifelse(!is.na(is_master_flag) & is_master_flag, -4.0, 0) # masters are NOT feeders
  ln_lr_master_sibling    <- ifelse(has_master_sibling, 1.0, 0) # has a master in same cluster
  # Lone offshore: offshore jurisdiction with no cluster siblings
  is_offshore <- jurisdiction %in% c("CAYMAN ISLANDS", "BRITISH VIRGIN ISLANDS",
                                     "BERMUDA", "LUXEMBOURG", "IRELAND",
                                     "JERSEY", "GUERNSEY", "ISLE OF MAN")
  cluster_size <- vapply(by_cluster, length, integer(1))
  cluster_size_per_row <- cluster_size[match(cluster_key, names(cluster_size))]
  is_lone <- cluster_size_per_row == 1
  ln_lr_lone_offshore     <- ifelse(is_offshore & is_lone, -2.0, 0)

  ln_lr_total <- ln_lr_name_suffix + ln_lr_gp_juris_split + ln_lr_svc_overlap +
                 ln_lr_master_flag + ln_lr_master_sibling + ln_lr_lone_offshore

  prior_logit <- log(prior / (1 - prior))
  posterior_logit <- prior_logit + ln_lr_total
  posterior <- 1 / (1 + exp(-posterior_logit))

  is_likely_feeder <- posterior >= threshold_likely
  is_likely_master <- has_master_suffix |
                      (!is.na(is_master_flag) & is_master_flag)

  # ----- Build evidence JSON ----------------------------------------------
  evidence_json <- vapply(seq_along(posterior), function(i) {
    pieces <- c()
    if (has_feeder_suffix[i]) pieces <- c(pieces, '"name_suffix":"feeder"')
    if (has_master_suffix[i]) pieces <- c(pieces, '"name_suffix":"master"')
    if (has_onshore_suffix[i]) pieces <- c(pieces, '"name_suffix":"onshore"')
    if (same_gp_diff_juris[i]) pieces <- c(pieces, '"gp_juris_split":true')
    if (service_stack_overlap[i]) pieces <- c(pieces, '"service_stack_overlap":true')
    if (has_master_sibling[i]) pieces <- c(pieces, '"master_sibling":true')
    if (!is.na(is_master_flag[i]) && is_master_flag[i]) pieces <- c(pieces, '"is_master_flag":true')
    if (is_offshore[i] && is_lone[i]) pieces <- c(pieces, '"lone_offshore":true')
    paste0("{", paste(pieces, collapse = ","), "}")
  }, character(1))

  data$is_likely_feeder    <- is_likely_feeder
  data$is_likely_master    <- is_likely_master
  data$id_master_candidate <- master_candidate_id
  data$score_feeder        <- round(posterior, 4)
  data$feeder_evidence     <- evidence_json

  data
}

# ----- Internals ------------------------------------------------------------

.resolve_feeder_columns <- function(data, fund_name_col, adviser_id_col,
                                     jurisdiction_col, gp_col, service_cols) {
  pick <- function(candidates, override = NULL) {
    if (!is.null(override)) {
      if (!override %in% names(data)) stop("Column not found: ", override)
      return(override)
    }
    hit <- candidates[candidates %in% names(data)]
    if (length(hit) == 0) NULL else hit[1]
  }

  fund_name <- pick(c("name_fund_clean", "name_fund", "nameFund", "fund_name"),
                    fund_name_col)
  if (is.null(fund_name)) {
    stop("flag_feeder_funds(): no fund name column found. Pass fund_name_col explicitly.")
  }

  adviser_id <- pick(
    c("id_crd_batch", "id_crd", "crd_batch", "crd_number", "idCrdBatch"),
    adviser_id_col
  )
  if (is.null(adviser_id)) {
    stop("flag_feeder_funds(): no adviser id column found. Pass adviser_id_col explicitly.")
  }

  jurisdiction <- pick(
    c("location_fund_incorporation", "locationFundIncorporation",
      "name_jurisdiction", "nameJurisdiction"),
    jurisdiction_col
  )
  gp <- pick(
    c("name_fund_gp_manager_trustee_director",
      "nameFundGpManagerTrusteeDirector"),
    gp_col
  )

  default_svc <- list(
    administrator = c("name_administrator", "name_fund_administrator", "nameAdministrator"),
    auditor       = c("name_fund_auditor", "name_auditor", "nameFundAuditor"),
    prime_broker  = c("name_fund_prime_broker", "name_prime_broker", "nameFundPrimeBroker")
  )
  svc <- list()
  if (is.null(service_cols)) {
    for (role in names(default_svc)) {
      svc[[role]] <- pick(default_svc[[role]])
    }
  } else {
    svc <- service_cols
  }

  list(
    fund_name = fund_name, adviser_id = adviser_id,
    jurisdiction = jurisdiction, gp = gp, svc = svc
  )
}

.feeder_name_stem <- function(x) {
  s <- toupper(as.character(x))
  s <- gsub("\\(CAYMAN|\\(OFFSHORE|\\(BVI|\\(BERMUDA|\\(LUX|\\(LUXEMBOURG|\\(IRELAND|\\(QP|\\(US|\\(DOMESTIC|\\)", " ", s)
  s <- gsub("\\bMASTER\\s+FUND\\b|\\bMASTER\\s+LP\\b|\\bMASTER\\s+LTD\\b|\\bMASTER$", " ", s)
  s <- gsub("\\bONSHORE\\b|\\bOFFSHORE\\b|\\bDOMESTIC\\s+FUND\\b", " ", s)
  s <- gsub("\\bFUND\\b|\\bL\\.?P\\.?\\b|\\bLTD\\.?\\b|\\bLLC\\b|\\bINC\\.?\\b", " ", s)
  s <- gsub("[[:punct:]]", " ", s)
  s <- gsub("\\s+", " ", s)
  trimws(s)
}

.append_empty_feeder_cols <- function(data) {
  data$is_likely_feeder    <- logical(0)
  data$is_likely_master    <- logical(0)
  data$id_master_candidate <- character(0)
  data$score_feeder        <- numeric(0)
  data$feeder_evidence     <- character(0)
  data
}
