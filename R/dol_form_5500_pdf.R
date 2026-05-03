#' DoL Form 5500 Schedule of Assets PDF Pipeline (Phase 1.5)
#'
#' Phase 1 (Schedule D structured CSV) captures CCT/PSA/103-12 IE pooled
#' vehicles — but misses direct LP commitments to PE/HF/RE/VC megafunds because
#' those live in the unstructured "Schedule of Assets Held for Investment
#' Purposes" PDF attachment to Form 5500 Schedule H.
#'
#' Phase 1.5 closes that gap by:
#'   1. Querying EFAST2's AWS CloudSearch endpoint for candidate filings
#'      (master trusts + Schedule H filers with alts).
#'   2. Downloading the master PDF from the public S3 bucket
#'      (efast2-filings-public.s3.amazonaws.com/prd).
#'   3. Parsing the PDF text via pdftools for structured holding rows
#'      (regex: name + 2 right-aligned $ values per line).
#'   4. Filtering to LP-shaped instruments (excluding bonds, common stock).
#'   5. Cross-walking via name_norm() against ADV 7B id_private_fund.
#'   6. Persisting to lake_lp_fund_commitments under
#'      subdomain=erisa_schedule_of_assets.
#'
#' Endpoints (verified 2026-04-30):
#'   - CloudSearch: https://www.efast.dol.gov/services/afs?q.parser=lucene
#'   - PDF bucket:  https://efast2-filings-public.s3.amazonaws.com/prd
#'   - Hit field `pdfpath` is appended to bucket URL.

EFAST2_CLOUDSEARCH <- "https://www.efast.dol.gov/services/afs"
EFAST2_PDF_BASE    <- "https://efast2-filings-public.s3.amazonaws.com/prd"


#' Search EFAST2 CloudSearch for Filings
#'
#' Returns a tibble of (id_ack, year, name_plan, name_sponsor, ein, pdfpath).
#' Use Lucene query syntax in `q`. Examples:
#'   q = 'planname:"MASTER TRUST" AND planyear:"2022"'
#'   q = 'plansponsor:"BOEING" AND planname:"MASTER"'
#'
#' @param q Lucene query
#' @param size results per call (max 200 by EFAST2 limit)
#' @param max_records hard cap on total returned (CloudSearch max 5000)
#' @return tibble of filings
#' @export
search_efast2 <- function(q, size = 200, max_records = 1000) {
  size <- min(size, 200L)
  out <- list()
  start <- 0L
  while (start < max_records) {
    url <- glue::glue("{EFAST2_CLOUDSEARCH}?q.parser=lucene&q={utils::URLencode(q, reserved = TRUE)}&size={size}&start={start}")
    resp <- try(httr::GET(url, httr::user_agent("Mozilla/5.0 fundManageR")), silent = TRUE)
    if (inherits(resp, "try-error")) break
    dat <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"))
    if (is.null(dat$hits$hit) || NROW(dat$hits$hit) == 0) break
    out[[length(out) + 1L]] <- dat$hits$hit
    start <- start + size
    if (start >= dat$hits$found) break
    Sys.sleep(0.2)
  }
  if (length(out) == 0) return(tibble::tibble())
  hits <- dplyr::bind_rows(out)
  fields <- hits$fields

  tibble::tibble(
    id_ack       = hits$id,
    year_filing  = as.integer(fields$planyear),
    name_plan    = fields$planname,
    name_sponsor = fields$plansponsor,
    id_ein       = fields$ein,
    id_pn        = fields$pn,
    state        = fields$state,
    city         = fields$city,
    pdfpath      = fields$pdfpath,
    # HARD RULE: dates as DATE, never VARCHAR. Source emits ISO YYYY-MM-DD.
    date_received = suppressWarnings(as.Date(fields$datereceived))
  )
}


#' Download Form 5500 Filing PDF from EFAST2 S3 Bucket
#'
#' @param pdfpath the `pdfpath` field from search_efast2() (starts with /YYYY/MM/DD/)
#' @param dest local destination path
#' @return path to file or NULL on failure
#' @export
download_efast2_pdf <- function(pdfpath, dest) {
  url <- paste0(EFAST2_PDF_BASE, pdfpath)
  res <- try(curl::curl_download(url, dest, quiet = TRUE,
                                   handle = curl::new_handle(useragent = "Mozilla/5.0 fundManageR")),
              silent = TRUE)
  if (inherits(res, "try-error") || !file.exists(dest) || file.info(dest)$size < 5000) {
    return(NULL)
  }
  dest
}


#' Parse Form 5500 PDF into Holding Rows
#'
#' Heuristic line parser. Each holding row in Schedule of Assets has form:
#'   <NAME>  <COST/SHARES>  <VALUE>
#' where the two numeric columns are right-aligned with 2+ spaces of padding.
#'
#' @param pdf_path local PDF
#' @return tibble (page, name_holding, val_a, val_b)
#' @export
parse_5500_pdf_holdings <- function(pdf_path) {
  txt <- pdftools::pdf_text(pdf_path)
  pat <- "^\\s*(.+?)\\s{2,}([\\(\\-]?\\$?[0-9,\\.]+\\)?)(?:\\s{2,}([\\(\\-]?\\$?[0-9,\\.]+\\)?))?\\s*$"
  parse_money <- function(x) {
    suppressWarnings(as.numeric(stringr::str_replace_all(x, "[\\$,()]", "")))
  }
  rows <- purrr::map_dfr(seq_along(txt), function(p) {
    lines <- strsplit(txt[p], "\n", fixed = TRUE)[[1]]
    m <- stringr::str_match(lines, pat)
    ok <- !is.na(m[, 1]) & !is.na(m[, 3])
    if (!any(ok)) return(NULL)
    tibble::tibble(
      page = p,
      name_holding = stringr::str_squish(m[ok, 2]),
      val_a = parse_money(m[ok, 3]),
      val_b = parse_money(m[ok, 4])
    )
  })
  rows %>% dplyr::filter(nchar(.data$name_holding) >= 6,
                          !is.na(.data$val_a))
}


#' Filter Holdings to LP-Shaped Instruments
#'
#' Keeps rows whose name matches private-fund patterns and excludes
#' obvious bonds/stocks/treasuries.
#'
#' @param holdings tibble from parse_5500_pdf_holdings()
#' @return filtered tibble
#' @export
filter_to_fund_holdings <- function(holdings) {
  fund_re <- "\\b(L\\.?P\\.?|LTD|LIMITED|FUND|PARTNERS|PARTNERSHIP|HOLDINGS|CAPITAL|TRUST|VENTURES?|GROWTH|EQUITY|FOF|FEEDER|MASTER|OFFSHORE|ONSHORE|ADVISORS?|ADVISERS?|MANAGEMENT)\\b"
  exclude_re <- paste0(
    "COMMON STOCK|^BOND |\\bBOND\\b|^NOTES?\\b|MORTGAGE TRUST [0-9]|PFD\\b|PFDS\\b|",
    "PREFERRED|U\\.?S\\.? TREASURY|TREASURY (NOTE|BILL|BOND|INFLATION|FRN)|",
    "FANNIE MAE|FREDDIE MAC|GINNIE MAE|FNMA|FHLMC|GNMA|FHLB\\b|",
    "MUNICIPAL BOND|REPURCHASE AGREEMENT|REVERSE REPO|REPO\\b|",
    "INTEREST RATE SWAP|SWAP\\b|FUTURE\\b|FORWARD\\b|OPTION ON\\b"
  )
  holdings %>%
    dplyr::filter(stringr::str_detect(.data$name_holding, fund_re),
                   !stringr::str_detect(.data$name_holding, exclude_re))
}


#' Process One ERISA Filing End-to-End
#'
#' Downloads PDF, parses holdings, filters to fund-shaped, returns tibble
#' with provenance fields. Cleans up the PDF after.
#'
#' @param hit one row from search_efast2() output
#' @param tmp_dir staging dir for PDFs (deleted after)
#' @return tibble of holdings with id_ack, name_plan, name_sponsor metadata
#' @export
process_one_filing <- function(hit, tmp_dir = tempdir()) {
  dest <- file.path(tmp_dir, paste0(hit$id_ack, ".pdf"))
  pdf_path <- download_efast2_pdf(hit$pdfpath, dest)
  if (is.null(pdf_path)) {
    return(tibble::tibble(id_ack = hit$id_ack, status = "download_failed", n_funds = 0L))
  }
  holdings <- try(parse_5500_pdf_holdings(pdf_path), silent = TRUE)
  if (inherits(holdings, "try-error") || nrow(holdings) == 0) {
    file.remove(pdf_path)
    return(tibble::tibble(id_ack = hit$id_ack, status = "parse_failed", n_funds = 0L))
  }
  funds <- filter_to_fund_holdings(holdings)
  out <- funds %>%
    dplyr::mutate(
      id_ack            = hit$id_ack,
      year_filing       = hit$year_filing,
      name_plan         = hit$name_plan,
      name_sponsor      = hit$name_sponsor,
      id_sponsor_ein    = hit$id_ein,
      state             = hit$state,
      pdfpath           = hit$pdfpath,
      status            = "ok"
    )
  file.remove(pdf_path)
  out
}


#' Process Batch of Filings and Write to Lake
#'
#' @param hits tibble from search_efast2()
#' @param data_root output root
#' @param state_file resume-tracking JSONL (each line: ack_id processed)
#' @param max_filings cap per run (for cron-batched processing)
#' @return invisible summary tibble
#' @export
write_schedule_of_assets_to_lake <- function(hits,
                                              data_root = "~/Desktop/data",
                                              state_file = "~/Desktop/data/_raw/dol_5500/sched_of_assets_processed.jsonl",
                                              max_filings = 100) {
  data_root <- path.expand(data_root)
  state_file <- path.expand(state_file)

  done_ids <- character(0)
  if (file.exists(state_file)) {
    lines <- readLines(state_file, warn = FALSE)
    done_ids <- vapply(lines, function(l) jsonlite::fromJSON(l)$id_ack %||% "", "")
  }
  todo <- hits %>%
    dplyr::filter(!.data$id_ack %in% done_ids,
                   !is.na(.data$pdfpath)) %>%
    head(max_filings)

  if (nrow(todo) == 0) {
    .fm_info("No new filings to process.")
    return(invisible(tibble::tibble()))
  }

  .fm_headline(glue::glue("Processing {nrow(todo)} filings"))

  all_holdings <- list()
  for (i in seq_len(nrow(todo))) {
    h <- todo[i, ]
    res <- try(process_one_filing(h), silent = TRUE)
    if (inherits(res, "try-error")) {
      .fm_warning(glue::glue("Failed: {h$id_ack}"))
      next
    }
    status <- if (nrow(res) > 0) (res$status[1] %||% "ok") else "no_funds"
    if (nrow(res) > 0 && status == "ok") {
      all_holdings[[length(all_holdings) + 1L]] <- res
      .fm_info(glue::glue("[{i}/{nrow(todo)}] {h$name_plan}: {nrow(res)} fund-shaped holdings"))
    } else {
      .fm_info(glue::glue("[{i}/{nrow(todo)}] {h$name_plan}: skipped ({status})"))
    }
    line <- jsonlite::toJSON(list(id_ack = h$id_ack, ts = format(Sys.time()), status = status),
                              auto_unbox = TRUE)
    cat(line, "\n", sep = "", file = state_file, append = TRUE)
  }

  if (length(all_holdings) == 0) return(invisible(tibble::tibble()))
  combined <- dplyr::bind_rows(all_holdings)

  # Add normalized name + canonical IDs
  combined <- combined %>%
    dplyr::mutate(
      name_holding_norm = name_norm_entity(.data$name_holding),
      id_lp_canonical   = paste0("erisa-", .data$id_sponsor_ein),
      type_vehicle      = "schedule_of_assets",
      source            = "erisa_5500_schedule_of_assets",
      confidence_tier   = 2L,
      currency          = "USD",
      date_disclosed    = paste0(.data$year_filing, "-12-31"),
      url_source        = paste0(EFAST2_PDF_BASE, .data$pdfpath)
    )

  yr <- unique(combined$year_filing)[1]
  out_dir <- file.path(data_root, "lake_lp_fund_commitments/subdomain=erisa_schedule_of_assets",
                        glue::glue("year={yr}"))
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
  out_file <- file.path(out_dir, glue::glue("erisa_sched_assets_{yr}_{ts}.zstd.parquet"))
  arrow::write_parquet(combined %>%
                          dplyr::transmute(
                            .data$id_lp_canonical, .data$id_sponsor_ein,
                            name_fund = .data$name_holding,
                            name_fund_norm = .data$name_holding_norm,
                            name_manager = NA_character_,
                            name_manager_norm = NA_character_,
                            .data$type_vehicle,
                            amount_commitment = .data$val_b,
                            amount_cost       = .data$val_a,
                            .data$currency, .data$date_disclosed,
                            .data$source, .data$confidence_tier, .data$url_source,
                            id_ack_filing = .data$id_ack,
                            .data$year_filing, .data$page,
                            participating_plan_name = .data$name_plan
                          ),
                        out_file, compression = "zstd")

  .fm_success(glue::glue(
    "Wrote {format(nrow(combined), big.mark=',')} fund-holding rows to {basename(out_file)}"
  ))

  invisible(combined)
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || is.na(x)) y else x
