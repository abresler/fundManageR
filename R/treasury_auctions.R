



.dictionary_treasury_names <-
  function() {
    tibble(
      nameTreasury = c(
        "cusip",
        "issue_date",
        "security_type",
        "security_term",
        "maturity_date",
        "interest_rate",
        "ref_cpi_on_issue_date",
        "ref_cpi_on_dated_date",
        "announcement_date",
        "auction_date",
        "auction_date_year",
        "dated_date",
        "accrued_interest_per1000",
        "accrued_interest_per100",
        "adjusted_accrued_interest_per1000",
        "adjusted_price",
        "allocation_percentage",
        "allocation_percentage_decimals",
        "announced_cusip",
        "auction_format",
        "average_median_discount_rate",
        "average_median_investment_rate",
        "average_median_price",
        "average_median_discount_margin",
        "average_median_yield",
        "back_dated",
        "back_dated_date",
        "bid_to_cover_ratio",
        "call_date",
        "callable",
        "called_date",
        "cash_management_bill_cmb",
        "closing_time_competitive",
        "closing_time_noncompetitive",
        "competitive_accepted",
        "competitive_bid_decimals",
        "competitive_tendered",
        "competitive_tenders_accepted",
        "corpus_cusip",
        "cpi_base_reference_period",
        "currently_outstanding",
        "direct_bidder_accepted",
        "direct_bidder_tendered",
        "estimated_amount_of_publicly_held_maturing_securities_by_type",
        "fima_included",
        "fima_noncompetitive_accepted",
        "fima_noncompetitive_tendered",
        "first_interest_period",
        "first_interest_payment_date",
        "floating_rate",
        "frn_index_determination_date",
        "frn_index_determination_rate",
        "high_discount_rate",
        "high_investment_rate",
        "high_price",
        "high_discount_margin",
        "high_yield",
        "index_ratio_on_issue_date",
        "indirect_bidder_accepted",
        "indirect_bidder_tendered",
        "interest_payment_frequency",
        "low_discount_rate",
        "low_investment_rate",
        "low_price",
        "low_discount_margin",
        "low_yield",
        "maturing_date",
        "maximum_competitive_award",
        "maximum_noncompetitive_award",
        "maximum_single_bid",
        "minimum_bid_amount",
        "minimum_strip_amount",
        "minimum_to_issue",
        "multiples_to_bid",
        "multiples_to_issue",
        "nlp_exclusion_amount",
        "nlp_reporting_threshold",
        "noncompetitive_accepted",
        "noncompetitive_tenders_accepted",
        "offering_amount",
        "original_cusip",
        "original_dated_date",
        "original_issue_date",
        "original_security_term",
        "pdf_filename_announcement",
        "pdf_filename_competitive_results",
        "pdf_filename_noncompetitive_results",
        "pdf_filename_special_announcement",
        "price_per100",
        "primary_dealer_accepted",
        "primary_dealer_tendered",
        "reopening",
        "security_term_day_month",
        "security_term_week_year",
        "series",
        "soma_accepted",
        "soma_holdings",
        "soma_included",
        "soma_tendered",
        "spread",
        "standard_interest_payment_per1000",
        "strippable",
        "term",
        "tiin_conversion_factor_per1000",
        "tips",
        "total_accepted",
        "total_tendered",
        "treasury_retail_accepted",
        "treasury_retail_tenders_accepted",
        "type",
        "unadjusted_accrued_interest_per1000",
        "unadjusted_price",
        "updated_timestamp",
        "xml_filename_announcement",
        "xml_filename_competitive_results",
        "xml_filename_special_announcement",
        "tint_cusip1",
        "tint_cusip2"
      ),

      nameActual =
        c(
          "id_cusip",
          "date_issue",
          "type_security",
          "term_security",
          "date_maturity",
          "pct_interest",
          "ratio_ref_cpi_on_issue_date",
          "ratio_ref_cpi_on_dated_date",
          "date_announcement",
          "date_auction",
          "year_auction",
          "date_dated",
          "amount_accrued_interest_per1000",
          "amount_accrued_interest_per100",
          "amount_adjusted_accrued_interest_per1000",
          "amount_price_per_100",
          "pct_allocation",
          "amount_allocation_decimals",
          "id_cusip_announced",
          "type_auction_format",
          "pct_average_median_discount_rate",
          "pct_average_median_investment_rate",
          "amount_amount_average_median_price",
          "amount_average_median_discount_margin",
          "pct_average_median_yield",
          "is_back_dated",
          "date_back_dated_date",
          "ratio_bid_to_cover_ratio",
          "date_call",
          "is_callable",
          "date_called",
          "is_cash_management_bill_cmb",
          "time_closing_competitive",
          "time_closing_noncompetitive",
          "amount_competitive_accepted",
          "amount_competitive_bid_decimals",
          "amount_competitive_tendered",
          "is_competitive_tenders_accepted",
          "id_cusip_corpus",
          "type_cpi_base_reference_period",
          "amount_currently_outstanding",
          "amount_direct_bidder_accepted",
          "amount_direct_bidder_tendered",
          "amount_estimated_amount_of_publicly_held_maturing_securities_by_type",
          "is_fima_included",
          "amount_fima_noncompetitive_accepted",
          "amount_fima_noncompetitive_tendered",
          "type_first_interest_period",
          "date_first_interest_payment",
          "is_floating_rate",
          "date_frn_index_determination",
          "amount_frn_index_determination_rate",
          "pct_discount_rate_high",
          "pct_investment_rate_high",
          "amount_price_high",
          "pct_discount_margin_high",
          "pct_yield_high",
          "ratio_index_on_issue_date",
          "amount_indirect_bidder_accepted",
          "amount_indirect_bidder_tendered",
          "type_interest_payment_frequency",
          "pct_discount_rate_low",
          "pct_investment_rate_low",
          "amount_price_low",
          "pct_discount_margin_low",
          "pct_yield",
          "date_maturing",
          "amount_maximum_competitive_award",
          "amount_maximum_noncompetitive_award",
          "amount_maximum_single_bid",
          "amount_minimum_bid_amount",
          "amount_minimum_strip_amount",
          "amount_minimum_to_issue",
          "amount_multiples_to_bid",
          "amount_multiples_to_issue",
          "amount_nlp_exclusion",
          "amount_nlp_reporting_threshold",
          "amount_noncompetitive_accepted",
          "is_noncompetitive_tenders_accepted",
          "amount_offering",
          "id_cusip_original",
          "date_original_date",
          "date_original_issue",
          "term_original_security",
          "slug_pdf_filename_announcement",
          "slug_pdf_filename_competitive_results",
          "slug_pdf_filename_noncompetitive_results",
          "slug_pdf_filename_special_announcement",
          "amount_price_per100",
          "amount_primary_dealer_accepted",
          "amount_primary_dealer_tendered",
          "is_reopening",
          "term_security_term_day_month",
          "term_security_term_week_year",
          "name_series",
          "amount_soma_accepted",
          "amount_soma_holdings",
          "is_soma_included",
          "amount_soma_tendered",
          "amount_spread",
          "pct_standard_interest_payment_per1000",
          "is_strippable",
          "type_term",
          "amount_tiin_conversion_factor_per1000",
          "is_tips",
          "amount_total_accepted",
          "amount_total_tendered",
          "amount_treasury_retail_accepted",
          "is_treasury_retail_tenders_accepted",
          "type_treasury",
          "unadjusted_accrued_interest_per1000",
          "amount_unadjusted_price",
          "datetime_updated_timestamp",
          "slug_xml_filename_announcement",
          "slug_xml_filename_competitive_results",
          "slug_xml_filename_special_announcement",
          "id_tint_cusip1",
          "id_tint_cusip2"
        )

    )
  }


.munge_treasury_direct_names <-
  function(data) {
    dict_names <- .dictionary_treasury_names()
    actual_names <-
      names(data) %>%
      map_chr(function(name){
        df_row <- dict_names %>% filter(nameTreasury == name)
        if (nrow(df_row) == 0) {
          glue::glue("Missing {name}")
          return(name)
        }

        df_row$nameActual
      })

    data <-
      setNames(data, actual_names)

    data
  }

.extract_treasury_json <-
  function(json) {
    start_substr <- 11
    total_char <- nchar(json)
    json <- json |> substr(11, total_char - 2) |> fromJSON()

    json
  }


# parse json data
.parse_auction_json <-
  function(url = "https://www.treasurydirect.gov/TA_WS/securities/jqsearch?format=jsonp&filterscount=0&groupscount=0&pagenum=0&pagesize=1000&recordstartindex=0&recordendindex=0") {
    json <- url |> read_lines()
    json <- .extract_treasury_json(json = json)

    data <-
      json$securityList |> as_tibble() |>
      mutate_all(list(function(x) {
        case_when(x == "" ~ NA_character_, TRUE ~ x)
      })) |>
      janitor::clean_names()

    data <- data |> .munge_treasury_direct_names()

    date_cols <- data |> select(matches("^date")) |> names()
    date_regular <-
      data |> select(matches("^date")) |>
      select(-matches("datetime")) |>
      names()

    data <-
      data |> mutate_at(date_cols, function(x) {
        x |> ymd_hms()
      }) |>
      mutate_at(date_regular, function(x) {
        x |> as.Date()
      })

    logical_cols <- data |> select(matches("^is_")) |> names()
    data <- data |>
      mutate_at(logical_cols, list(function(x) {
        case_when(x == "Yes" ~ TRUE, x == "No" ~ FALSE, TRUE ~ NA)
      }))

    amount_ratio <- data |> select(matches("^ratio|^index|^amount")) |> names()

    pct_cols <-
      data |> select(matches("^pct")) |> names()

    data <- data |> mutate_at(amount_ratio, function(x) {
      x |> readr::parse_number()
    })

    data <-
      data |> mutate_at(pct_cols, function(x) {
        x |> readr::parse_number() / 100
      })

    data <-
      data |>
      mutate(duration_days = (date_maturity - date_issue) |> as.numeric())

    data <-
      data |>
      mutate(url_treasury_json = url)



    data
  }

#' Treasury Auction URL Dictionary
#'
#' @return
#' @export
#'
#' @examples
dictionary_treasury_auction_urls <-
  memoise::memoise(function() {
    url = "https://www.treasurydirect.gov/TA_WS/securities/jqsearch?format=jsonp&filterscount=0&groupscount=0&pagenum=0&pagesize=1000&recordstartindex=0&recordendindex=0"
    json <- url |> read_lines()
    json <- .extract_treasury_json(json = json)
    count_pages <- json$totalResultsCount %/% 1000
    all_pages <- 0:count_pages

    urls <- glue::glue("https://www.treasurydirect.gov/TA_WS/securities/jqsearch?format=jsonp&filterscount=0&groupscount=0&pagenum={all_pages}&pagesize=1000&recordstartindex=0&recordendindex=0") |> as.character()

    tibble(url_treasury_json = urls)

  })

#' Treasury Auction History
#'
#' @return
#' @export
#'
#' @examples
us_treasury_auctions <-
  function() {
    df_urls <- dictionary_treasury_auction_urls()
    .parse_auction_json_safe <-
      purrr::possibly(.parse_auction_json, tibble())
    data <-
      df_urls$url_treasury_json |>
      map_dfr(function(x) {
        x |> message()
        .parse_auction_json_safe(url = x)
      })

    data
  }