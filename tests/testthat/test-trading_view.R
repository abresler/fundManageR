# Smoke tests for trading_view.R wrappers.
# Skipped on CRAN — these hit live TradingView endpoints.

test_that("tv_metainfo returns snake_case tibble for america", {
  testthat::skip_on_cran()
  testthat::skip_if_offline()
  r <- tv_metainfo("america")
  expect_s3_class(r, "tbl_df")
  expect_gt(nrow(r), 100)
  expect_true(all(grepl("^[a-z][a-z0-9_]*$", names(r))))
})

test_that("tv_market_events returns snake_case tibble", {
  testthat::skip_on_cran()
  testthat::skip_if_offline()
  r <- tv_market_events()
  expect_s3_class(r, "tbl_df")
  expect_true(all(grepl("^[a-z][a-z0-9_]*$", names(r))))
  expect_true(any(c("date_release", "datetime_release") %in% names(r)))
})

test_that("tv_regions_tickers returns snake_case tibble for america", {
  testthat::skip_on_cran()
  testthat::skip_if_offline()
  r <- tv_regions_tickers("america")
  expect_s3_class(r, "tbl_df")
  expect_gt(nrow(r), 1000)
  expect_true(all(grepl("^[a-z][a-z0-9_]*$", names(r))))
})

test_that("tv_regions_metrics returns snake_case tibble for america", {
  testthat::skip_on_cran()
  testthat::skip_if_offline()
  r <- tv_regions_metrics("america")
  expect_s3_class(r, "tbl_df")
  expect_gt(nrow(r), 100)
  expect_true(all(grepl("^[a-z][a-z0-9_]*$", names(r))))
})

test_that("tv_tickers_news returns snake_case tibble", {
  testthat::skip_on_cran()
  testthat::skip_if_offline()
  r <- tv_tickers_news("NASDAQ:AAPL")
  expect_s3_class(r, "tbl_df")
  expect_true(all(grepl("^[a-z][a-z0-9_]*$", names(r))))
})

test_that("get_tradeview_term returns entity-resolved snake_case tibble", {
  testthat::skip_on_cran()
  testthat::skip_if_offline()
  r <- get_tradeview_term("AAPL")
  expect_s3_class(r, "tbl_df")
  expect_gt(nrow(r), 1)
  expect_true(all(grepl("^[a-z][a-z0-9_]*$", names(r))))
  expect_true("term_search" %in% names(r))
  # Entity-resolution columns present
  expect_true(any(c("symbol", "exchange", "type") %in% names(r)))
})

test_that(".tv_with_retry retries on errors and raises after exhaustion", {
  testthat::skip_on_cran()
  fail_fn <- function() stop("simulated network error")
  expect_error(.tv_with_retry(fail_fn, max_tries = 2, base_sleep = 0.01),
               "failed after 2 tries")
})

test_that(".tv_safe_fromJSON warns on schema drift", {
  testthat::skip_on_cran()
  testthat::skip_if_offline()
  resp <- httr::GET("https://httpbin.org/json")
  expect_warning(
    .tv_safe_fromJSON(resp, expect = c("definitely_not_a_real_key"), context = "test"),
    "schema drift"
  )
})

test_that("generate_tv_bond_query returns valid query list", {
  q <- generate_tv_bond_query()
  expect_type(q, "list")
  expect_named(q)
})

test_that("tv_metric_query returns valid query list", {
  q <- tv_metric_query()
  expect_type(q, "list")
  expect_named(q)
})
