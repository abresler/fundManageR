context("basic functionality")
test_that("data wrappers works", {

  testthat::skip_on_cran()

  testthat::expect_that(get_data_cb_unicorns(), testthat::is_a("data.frame"))
  testthat::expect_that(get_data_ycombinator_alumni(), testthat::is_a("data.frame"))
  testthat::expect_that(get_data_sec_closed_end_funds(), testthat::is_a("data.frame"))
  testthat::expect_that(get_data_sec_foia_requests(), testthat::is_a("data.frame"))
  testthat::expect_that(get_data_finra_entities(entity_names = c("EJF", "Blackstone", "Next Play", "Simple Capital"), score_threshold = .25, ocr_pdf = TRUE),
                        testthat::is_a("data.frame"))
  testthat::expect_that(get_data_crsp_indicies_returns(), testthat::is_a("data.frame"))
})
