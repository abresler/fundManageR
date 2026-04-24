# Unified securities ------------------------------------------------------

#' Scanner endpoint column specs per asset class
#'
#' Returns the list of columns (in strict positional order) that each
#' TradingView scanner endpoint accepts. Maintained against live probes.
#'
#' @keywords internal
.unified_scanner_specs <- function() {
  list(
    equity = list(
      endpoint = "global",
      cols = c("name","description","type","typespecs","exchange","currency",
               "close","change","volume","market_cap_basic","sector","industry",
               "country","logoid"),
      filter = list(list(left = "type", operation = "in_range",
                         right = list("stock","fund","dr","structured")))
    ),
    bond = list(
      endpoint = "bond",
      cols = c("name","close","description","type","exchange","country",
               "coupon","maturity_date","change","bid","ask"),
      filter = NULL
    ),
    futures = list(
      endpoint = "futures",
      cols = c("name","close","description","type","exchange","currency",
               "change","volume","high","low","open","expiration"),
      filter = NULL
    ),
    crypto = list(
      endpoint = "crypto",
      cols = c("name","close","description","type","exchange","currency",
               "change","volume","high","low"),
      filter = NULL
    ),
    coin = list(
      endpoint = "coin",
      cols = c("base_currency","close","24h_vol_cmc","market_cap_calc",
               "24h_close_change|5","circulating_supply","total_supply"),
      filter = NULL
    ),
    forex = list(
      endpoint = "forex",
      cols = c("name","close","description","type","exchange","currency",
               "change","change_abs","high","low","volume"),
      filter = NULL
    ),
    cfd = list(
      endpoint = "cfd",
      cols = c("name","close","description","type","exchange","currency","change"),
      filter = NULL
    )
  )
}

#' Call a TradingView scanner endpoint and return raw positional rows
#'
#' @keywords internal
.scan_tv <- function(endpoint, cols, filter = NULL, range = c(0L, 50000L)) {
  url <- glue::glue("https://scanner.tradingview.com/{endpoint}/scan") %>% as.character()

  body <- list(columns = as.list(cols), range = as.list(as.integer(range)))
  if (!is.null(filter)) body$filter <- filter

  resp <- httr::POST(
    url,
    httr::add_headers(`Content-Type` = "application/json",
                      `User-Agent`   = "Mozilla/5.0",
                      Origin         = "https://www.tradingview.com"),
    body   = jsonlite::toJSON(body, auto_unbox = TRUE, null = "null"),
    encode = "raw"
  )
  if (httr::status_code(resp) != 200L) {
    stop(sprintf("[%s] HTTP %d: %s", endpoint,
                 httr::status_code(resp),
                 substr(httr::content(resp, as = "text", encoding = "UTF-8"), 1, 200)))
  }
  parsed <- jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"),
                               simplifyVector = FALSE)
  total  <- parsed$totalCount %||% 0L
  rows   <- parsed$data %||% list()

  if (length(rows) == 0L) {
    return(list(total = total, df = tibble::tibble()))
  }

  ids <- vapply(rows, function(r) r$s %||% NA_character_, character(1))
  mat <- do.call(rbind, lapply(rows, function(r) {
    d <- r$d
    vapply(seq_along(cols), function(i) {
      val <- d[[i]]
      if (is.null(val)) return(NA_character_)
      if (is.list(val)) return(paste(unlist(val), collapse = "|"))
      as.character(val)
    }, character(1))
  }))
  colnames(mat) <- cols
  df <- tibble::as_tibble(mat) %>% dplyr::mutate(idSecurity = ids, .before = 1L)
  list(total = total, df = df)
}

#' Paginate a scanner call in 50k chunks up to max_rows
#'
#' @keywords internal
.scan_tv_paged <- function(endpoint, cols, filter = NULL, max_rows = 50000L,
                           page_size = 50000L, return_message = TRUE) {
  start <- 0L
  out   <- list()
  total <- NA_integer_
  repeat {
    # Respect both the user cap and the universe total once known
    hard_cap <- if (is.na(total)) max_rows else min(max_rows, total)
    if (start >= hard_cap) break
    end <- min(start + page_size, hard_cap)
    chunk <- tryCatch(
      .scan_tv(endpoint = endpoint, cols = cols, filter = filter,
               range = c(start, end)),
      error = function(e) list(total = 0L, df = tibble::tibble(), err = conditionMessage(e))
    )
    if (!is.null(chunk$err)) {
      if (return_message) message(sprintf("[%s] %s", endpoint, chunk$err))
      break
    }
    if (is.na(total)) total <- chunk$total
    out[[length(out) + 1L]] <- chunk$df
    got <- nrow(chunk$df)
    if (got == 0L || got < (end - start)) break
    start <- end
  }
  df <- if (length(out)) dplyr::bind_rows(out) else tibble::tibble()
  if (nrow(df)) df <- dplyr::distinct(df, idSecurity, .keep_all = TRUE)
  list(total = total %||% 0L, df = df)
}

#' Normalize a scanner result into the unified schema
#'
#' @keywords internal
.normalize_unified <- function(df, asset_class, endpoint, snapshot_time) {
  if (!nrow(df)) return(tibble::tibble())

  parse_num  <- function(x) suppressWarnings(as.numeric(x))
  parse_date <- function(x) {
    n <- suppressWarnings(as.numeric(x))
    out <- rep(as.Date(NA), length(n))
    ok  <- !is.na(n) & nchar(x) == 8L
    if (any(ok)) out[ok] <- as.Date(as.character(n[ok]), format = "%Y%m%d")
    out
  }

  split_ticker <- function(ids) {
    # idSecurity is "EXCHANGE:TICKER"; exchange may contain letters only
    m <- stringr::str_match(ids, "^([^:]+):(.+)$")
    tibble::tibble(nameExchange = m[, 2], idTicker = m[, 3])
  }

  parts <- split_ticker(df$idSecurity)

  base <- tibble::tibble(
    idSecurity       = df$idSecurity,
    idTicker         = parts$idTicker,
    nameExchange     = parts$nameExchange,
    typeAsset        = asset_class,
    typeSecurity    = if ("type" %in% names(df)) df$type else NA_character_,
    typeSpecs        = if ("typespecs" %in% names(df)) df$typespecs else NA_character_,
    nameSecurity     = df$description %||% NA_character_,
    nameCurrency     = df$currency %||% NA_character_,
    nameCountry      = df$country %||% NA_character_,
    nameSector       = df$sector %||% NA_character_,
    nameIndustry     = df$industry %||% NA_character_,
    priceClose       = parse_num(df$close),
    priceOpen        = parse_num(df$open %||% NA),
    priceHigh        = parse_num(df$high %||% NA),
    priceLow         = parse_num(df$low %||% NA),
    priceBid         = parse_num(df$bid %||% NA),
    priceAsk         = parse_num(df$ask %||% NA),
    pctChangeDaily   = parse_num(df$change %||% NA),
    amountChange     = parse_num(df$change_abs %||% NA),
    countVolume      = parse_num(df$volume %||% NA),
    amountMarketCap  = parse_num(df$market_cap_basic %||% NA),
    pctCoupon        = parse_num(df$coupon %||% NA),
    dateMaturity     = parse_date(df$maturity_date %||% NA),
    dateExpiration   = parse_date(df$expiration %||% NA),
    slugLogo         = df$logoid %||% NA_character_,
    endpointScanner  = endpoint,
    dateSnapshot     = as.Date(snapshot_time, tz = "America/New_York"),
    datetimeSnapshot = snapshot_time
  )

  if (asset_class == "coin") {
    # coin endpoint uses different columns entirely
    base <- base %>% dplyr::mutate(
      idTicker        = df$base_currency %||% parts$idTicker,
      nameSecurity    = df$base_currency %||% NA_character_,
      priceClose      = parse_num(df$close),
      countVolume     = parse_num(df$`24h_vol_cmc` %||% NA),
      amountMarketCap = parse_num(df$market_cap_calc %||% NA),
      pctChangeDaily  = parse_num(df$`24h_close_change|5` %||% NA)
    )
  }

  base
}

#' Pull all major publicly traded securities in a unified schema
#'
#' Fetches a same-time snapshot of equities, bonds, futures, crypto spot/swap,
#' coin-market-cap view, forex pairs, and CFD indices from TradingView's
#' public scanner endpoints and returns a single tibble with a consistent
#' schema suitable for parquet mirroring to a data lake.
#'
#' Each asset class is fetched in 50k-row pages up to \code{max_rows_per_class}.
#' Network failures degrade gracefully to an empty tibble for that class.
#'
#' @param asset_classes vector of classes to include. One or more of
#'   \code{"equity"}, \code{"bond"}, \code{"futures"}, \code{"crypto"},
#'   \code{"coin"}, \code{"forex"}, \code{"cfd"}.
#' @param max_rows_per_class integer cap on rows per class. Default 250,000
#'   (covers the largest class, bonds, at ~253k live).
#' @param return_message if \code{TRUE}, prints per-class fetch counts.
#'
#' @return a \code{tibble} with one row per security and these columns:
#'   \code{idSecurity}, \code{idTicker}, \code{nameExchange},
#'   \code{typeAsset}, \code{typeSecurity}, \code{typeSpecs},
#'   \code{nameSecurity}, \code{nameCurrency}, \code{nameCountry},
#'   \code{nameSector}, \code{nameIndustry},
#'   \code{priceClose}, \code{priceOpen}, \code{priceHigh}, \code{priceLow},
#'   \code{priceBid}, \code{priceAsk}, \code{pctChangeDaily}, \code{amountChange},
#'   \code{countVolume}, \code{amountMarketCap}, \code{pctCoupon},
#'   \code{dateMaturity}, \code{dateExpiration}, \code{slugLogo},
#'   \code{endpointScanner}, \code{dateSnapshot}, \code{datetimeSnapshot}.
#'
#' @export
#' @import dplyr tibble stringr purrr jsonlite httr glue
#' @examples
#' \dontrun{
#' df <- get_unified_securities(asset_classes = c("equity","bond"),
#'                              max_rows_per_class = 5000)
#' dplyr::count(df, typeAsset)
#' }
get_unified_securities <- function(asset_classes = c("equity","bond","futures","crypto",
                                                     "coin","forex","cfd"),
                                   max_rows_per_class = 250000L,
                                   return_message = TRUE) {
  specs <- .unified_scanner_specs()
  asset_classes <- match.arg(asset_classes, names(specs), several.ok = TRUE)

  snapshot_time <- Sys.time()
  attr(snapshot_time, "tzone") <- "America/New_York"

  fetch_safe <- purrr::possibly(
    function(ac) {
      s <- specs[[ac]]
      res <- .scan_tv_paged(endpoint = s$endpoint, cols = s$cols, filter = s$filter,
                            max_rows = max_rows_per_class, return_message = return_message)
      if (return_message) {
        message(sprintf("[%-7s] fetched %d of %d rows (%s/scan)",
                        ac, nrow(res$df), res$total, s$endpoint))
      }
      .normalize_unified(df = res$df, asset_class = ac,
                         endpoint = s$endpoint, snapshot_time = snapshot_time)
    },
    otherwise = tibble::tibble()
  )

  out <- purrr::map(asset_classes, fetch_safe) %>%
    dplyr::bind_rows()

  if (return_message && nrow(out)) {
    counts <- dplyr::count(out, typeAsset, name = "n")
    message("Unified snapshot ", format(snapshot_time, "%Y-%m-%d %H:%M:%S %Z"),
            "  total=", nrow(out))
    for (i in seq_len(nrow(counts))) {
      message(sprintf("  %-8s %s", counts$typeAsset[i],
                      formatC(counts$n[i], big.mark = ",", format = "d")))
    }
  }
  out
}

# Null-coalescing helper -------------------------------------------------
`%||%` <- function(a, b) if (is.null(a)) b else a
