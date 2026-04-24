# Unified securities ------------------------------------------------------

#' Scanner endpoint column specs per asset class
#'
#' @keywords internal
.unified_scanner_specs <- function() {
  list(
    equity = list(
      endpoint = "global",
      cols = c(
        # Identity + price (14)
        "name","description","type","typespecs","exchange","currency",
        "close","change","volume","market_cap_basic","sector","industry",
        "country","logoid",
        # Valuation (9)
        "market_cap_calc","enterprise_value_fq","enterprise_value_ebitda_ttm",
        "price_earnings_ttm","price_earnings_forward_fy","price_sales_current",
        "price_book_fq","price_free_cash_flow_ttm","price_revenue_ttm",
        # Income (7)
        "total_revenue_ttm","gross_profit_ttm","net_income_ttm","ebitda_ttm",
        "earnings_per_share_forecast_next_fq","earnings_per_share_diluted_ttm",
        "revenue_per_share_ttm",
        # Margins (4)
        "gross_margin_ttm","operating_margin_ttm","net_margin_ttm","pre_tax_margin_ttm",
        # Returns (3)
        "return_on_equity_fq","return_on_assets_fq","return_on_invested_capital_fq",
        # Balance sheet (8)
        "total_assets_fq","total_debt_fq","total_equity_fq","total_current_assets_fq",
        "total_current_liabilities_fq","cash_n_short_term_invest_fq",
        "shares_outstanding","float_shares_outstanding",
        # Ratios (4)
        "debt_to_equity_fq","current_ratio_fq","quick_ratio_fq","altman_z_score_ttm",
        # Cash flow (2)
        "cash_f_operating_activities_ttm","free_cash_flow_ttm",
        # Growth (2)
        "total_revenue_yoy_growth_ttm","earnings_per_share_diluted_yoy_growth_ttm",
        # Risk / technical (6)
        "beta_1_year","beta_5_year","Volatility.W","Volatility.M",
        "average_volume_10d_calc","relative_volume_10d_calc",
        # 52-week (4)
        "price_52_week_high","price_52_week_low",
        "price_52_week_high_date","price_52_week_low_date",
        # Analyst / HR (2)
        "recommendation_mark","number_of_employees",
        # Dividend (3)
        "dividend_yield_recent","dps_common_stock_prim_issue_fy","ex_dividend_date_recent",
        # Earnings calendar (1)
        "earnings_release_next_trading_date_fq",
        # Surprise / growth (2)
        "eps_surprise_percent_fq","eps_diluted_growth_percent_fq",
        # Update time (1)
        "update-time"
      ),
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
  df <- tibble::as_tibble(mat) %>% dplyr::mutate(id_security = ids, .before = 1L)
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
  if (nrow(df)) df <- dplyr::distinct(df, id_security, .keep_all = TRUE)
  list(total = total %||% 0L, df = df)
}

#' Normalize a scanner result into the unified snake_case schema
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
    m <- stringr::str_match(ids, "^([^:]+):(.+)$")
    tibble::tibble(name_exchange = m[, 2], id_ticker = m[, 3])
  }

  parts <- split_ticker(df$id_security)

  base <- tibble::tibble(
    id_security        = df$id_security,
    id_ticker          = parts$id_ticker,
    name_exchange      = parts$name_exchange,
    type_asset         = asset_class,
    type_security      = if ("type" %in% names(df)) df$type else NA_character_,
    type_specs         = if ("typespecs" %in% names(df)) df$typespecs else NA_character_,
    name_security      = df$description %||% NA_character_,
    name_currency      = df$currency %||% NA_character_,
    name_country       = df$country %||% NA_character_,
    name_sector        = df$sector %||% NA_character_,
    name_industry      = df$industry %||% NA_character_,
    price_close        = parse_num(df$close),
    price_open         = parse_num(df$open %||% NA),
    price_high         = parse_num(df$high %||% NA),
    price_low          = parse_num(df$low %||% NA),
    price_bid          = parse_num(df$bid %||% NA),
    price_ask          = parse_num(df$ask %||% NA),
    pct_change_daily   = parse_num(df$change %||% NA),
    amount_change      = parse_num(df$change_abs %||% NA),
    count_volume       = parse_num(df$volume %||% NA),
    amount_market_cap  = parse_num(df$market_cap_basic %||% NA),
    pct_coupon         = parse_num(df$coupon %||% NA),
    date_maturity      = parse_date(df$maturity_date %||% NA),
    date_expiration    = parse_date(df$expiration %||% NA),
    slug_logo          = df$logoid %||% NA_character_,
    endpoint_scanner   = endpoint,
    date_snapshot      = as.Date(snapshot_time, tz = "America/New_York"),
    datetime_snapshot  = snapshot_time
  )

  if (asset_class == "coin") {
    base <- base %>% dplyr::mutate(
      id_ticker         = df$base_currency %||% parts$id_ticker,
      name_security     = df$base_currency %||% NA_character_,
      price_close       = parse_num(df$close),
      count_volume      = parse_num(df$`24h_vol_cmc` %||% NA),
      amount_market_cap = parse_num(df$market_cap_calc %||% NA),
      pct_change_daily  = parse_num(df$`24h_close_change|5` %||% NA)
    )
  }

  if (asset_class == "equity") {
    # TradingView update-time is a unix timestamp (seconds since epoch)
    parse_epoch <- function(x) {
      n <- suppressWarnings(as.numeric(x))
      out <- as.POSIXct(rep(NA_real_, length(n)), origin = "1970-01-01", tz = "America/New_York")
      ok <- !is.na(n) & n > 0
      if (any(ok)) out[ok] <- as.POSIXct(n[ok], origin = "1970-01-01", tz = "America/New_York")
      out
    }
    base <- base %>% dplyr::mutate(
      # Valuation
      amount_market_cap_calc    = parse_num(df$market_cap_calc          %||% NA),
      amount_enterprise_value   = parse_num(df$enterprise_value_fq      %||% NA),
      ratio_ev_to_ebitda        = parse_num(df$enterprise_value_ebitda_ttm %||% NA),
      ratio_pe_ttm              = parse_num(df$price_earnings_ttm       %||% NA),
      ratio_pe_forward          = parse_num(df$price_earnings_forward_fy %||% NA),
      ratio_ps                  = parse_num(df$price_sales_current      %||% NA),
      ratio_pb                  = parse_num(df$price_book_fq            %||% NA),
      ratio_pfcf                = parse_num(df$price_free_cash_flow_ttm %||% NA),
      ratio_price_revenue_ttm   = parse_num(df$price_revenue_ttm        %||% NA),
      # Income
      amount_revenue_ttm        = parse_num(df$total_revenue_ttm        %||% NA),
      amount_gross_profit_ttm   = parse_num(df$gross_profit_ttm         %||% NA),
      amount_net_income_ttm     = parse_num(df$net_income_ttm           %||% NA),
      amount_ebitda_ttm         = parse_num(df$ebitda_ttm               %||% NA),
      amount_eps_forecast_nfq   = parse_num(df$earnings_per_share_forecast_next_fq %||% NA),
      amount_eps_diluted_ttm    = parse_num(df$earnings_per_share_diluted_ttm %||% NA),
      amount_revenue_per_share_ttm = parse_num(df$revenue_per_share_ttm %||% NA),
      # Margins
      pct_gross_margin_ttm      = parse_num(df$gross_margin_ttm         %||% NA),
      pct_operating_margin_ttm  = parse_num(df$operating_margin_ttm     %||% NA),
      pct_net_margin_ttm        = parse_num(df$net_margin_ttm           %||% NA),
      pct_pretax_margin_ttm     = parse_num(df$pre_tax_margin_ttm       %||% NA),
      # Returns
      pct_roe                   = parse_num(df$return_on_equity_fq      %||% NA),
      pct_roa                   = parse_num(df$return_on_assets_fq      %||% NA),
      pct_roic                  = parse_num(df$return_on_invested_capital_fq %||% NA),
      # Balance sheet
      amount_total_assets       = parse_num(df$total_assets_fq          %||% NA),
      amount_total_debt         = parse_num(df$total_debt_fq            %||% NA),
      amount_total_equity       = parse_num(df$total_equity_fq          %||% NA),
      amount_current_assets     = parse_num(df$total_current_assets_fq  %||% NA),
      amount_current_liabilities = parse_num(df$total_current_liabilities_fq %||% NA),
      amount_cash_and_sti       = parse_num(df$cash_n_short_term_invest_fq %||% NA),
      count_shares_outstanding  = parse_num(df$shares_outstanding       %||% NA),
      count_shares_float        = parse_num(df$float_shares_outstanding %||% NA),
      # Leverage / liquidity ratios
      ratio_debt_to_equity      = parse_num(df$debt_to_equity_fq        %||% NA),
      ratio_current             = parse_num(df$current_ratio_fq         %||% NA),
      ratio_quick               = parse_num(df$quick_ratio_fq           %||% NA),
      score_altman_z            = parse_num(df$altman_z_score_ttm       %||% NA),
      # Cash flow
      amount_cf_operating_ttm   = parse_num(df$cash_f_operating_activities_ttm %||% NA),
      amount_fcf_ttm            = parse_num(df$free_cash_flow_ttm       %||% NA),
      # Growth
      pct_revenue_yoy_growth    = parse_num(df$total_revenue_yoy_growth_ttm %||% NA),
      pct_eps_yoy_growth        = parse_num(df$earnings_per_share_diluted_yoy_growth_ttm %||% NA),
      # Risk / technical
      beta_1y                   = parse_num(df$beta_1_year              %||% NA),
      beta_5y                   = parse_num(df$beta_5_year              %||% NA),
      pct_volatility_week       = parse_num(df$Volatility.W             %||% NA),
      pct_volatility_month      = parse_num(df$Volatility.M             %||% NA),
      count_avg_volume_10d      = parse_num(df$average_volume_10d_calc  %||% NA),
      ratio_relative_volume_10d = parse_num(df$relative_volume_10d_calc %||% NA),
      # 52-week
      price_52w_high            = parse_num(df$price_52_week_high       %||% NA),
      price_52w_low             = parse_num(df$price_52_week_low        %||% NA),
      date_52w_high             = parse_date(df$price_52_week_high_date %||% NA),
      date_52w_low              = parse_date(df$price_52_week_low_date  %||% NA),
      # Analyst / HR
      score_recommendation      = parse_num(df$recommendation_mark      %||% NA),
      count_employees           = parse_num(df$number_of_employees      %||% NA),
      # Dividend
      pct_dividend_yield        = parse_num(df$dividend_yield_recent    %||% NA),
      amount_dividend_per_share_fy = parse_num(df$dps_common_stock_prim_issue_fy %||% NA),
      date_ex_dividend          = parse_epoch(df$ex_dividend_date_recent %||% NA),
      # Earnings
      date_earnings_next        = parse_epoch(df$earnings_release_next_trading_date_fq %||% NA),
      pct_eps_surprise          = parse_num(df$eps_surprise_percent_fq  %||% NA),
      pct_eps_growth_fq         = parse_num(df$eps_diluted_growth_percent_fq %||% NA),
      # As-of
      datetime_as_of            = parse_epoch(df$`update-time`          %||% NA)
    )
  }

  base
}

#' Pull all major publicly traded securities in a unified snake_case schema
#'
#' Fetches a same-time snapshot of equities, bonds, futures, crypto, coin
#' market-cap view, forex, and CFD from TradingView's public scanner
#' endpoints. Output column names use \code{snake_case} to match the
#' convention used by the dq data lake.
#'
#' @param asset_classes vector of classes to include. One or more of
#'   \code{"equity"}, \code{"bond"}, \code{"futures"}, \code{"crypto"},
#'   \code{"coin"}, \code{"forex"}, \code{"cfd"}.
#' @param max_rows_per_class integer cap on rows per class.
#' @param return_message if \code{TRUE}, prints per-class fetch counts.
#'
#' @return a \code{tibble} with one row per security. Columns:
#'   \code{id_security}, \code{id_ticker}, \code{name_exchange},
#'   \code{type_asset}, \code{type_security}, \code{type_specs},
#'   \code{name_security}, \code{name_currency}, \code{name_country},
#'   \code{name_sector}, \code{name_industry},
#'   \code{price_close}, \code{price_open}, \code{price_high},
#'   \code{price_low}, \code{price_bid}, \code{price_ask},
#'   \code{pct_change_daily}, \code{amount_change}, \code{count_volume},
#'   \code{amount_market_cap}, \code{pct_coupon}, \code{date_maturity},
#'   \code{date_expiration}, \code{slug_logo}, \code{endpoint_scanner},
#'   \code{date_snapshot}, \code{datetime_snapshot}.
#'
#' @export
#' @import dplyr tibble stringr purrr jsonlite httr glue
#' @examples
#' \dontrun{
#' df <- get_unified_securities(asset_classes = c("equity","bond"),
#'                              max_rows_per_class = 5000)
#' dplyr::count(df, type_asset)
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
    counts <- dplyr::count(out, type_asset, name = "n")
    message("Unified snapshot ", format(snapshot_time, "%Y-%m-%d %H:%M:%S %Z"),
            "  total=", nrow(out))
    for (i in seq_len(nrow(counts))) {
      message(sprintf("  %-8s %s", counts$type_asset[i],
                      formatC(counts$n[i], big.mark = ",", format = "d")))
    }
  }
  out
}

# Null-coalescing helper -------------------------------------------------
`%||%` <- function(a, b) if (is.null(a)) b else a
