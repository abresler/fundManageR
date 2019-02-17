.munge_inflation_data <-
  function(data) {
    data <-
      data %>%
      mutate_all(funs(. %>% as.character() %>% readr::parse_number())) %>%
      mutate(pctChangeIndex = pctChangeIndex / 100)

    current <-
      data %>% filter(yearIndex == max(yearIndex)) %>% pull(ratioIndex)

    data <-
      data %>%
      mutate(ratioYearAdjusterCurrentDollars = current / ratioIndex) %>%
      dplyr::select(yearIndex,
                  ratioIndex,
                  ratioYearAdjusterCurrentDollars,
                  everything())

    data
  }

.get_1913_inflation <-
  function() {
    page <-
      "https://minneapolisfed.org/community/financial-and-economic-education/cpi-calculator-information/consumer-price-index-and-inflation-rates-1913" %>% read_html()

    data <-
      page %>%
      html_table(fill = F) %>%
      flatten_df() %>%
      dplyr::as_tibble() %>%
      purrr::set_names(c("yearIndex", "ratioIndex", "pctChangeIndex"))

    data %>%
      .munge_inflation_data()

  }

.get_1800_inflation <-
  function() {
    page <-
      "https://minneapolisfed.org/community/financial-and-economic-education/cpi-calculator-information/consumer-price-index-1800" %>% read_html()

    data <-
      page %>%
      html_table(fill = F) %>%
      flatten_df() %>%
      dplyr::as_tibble() %>%
      purrr::set_names(c("yearIndex", "ratioIndex", "pctChangeIndex"))

    data %>%
      .munge_inflation_data()

  }

#' US Inflation Adjustment Table
#'
#' Gets a table to adjust prices to current dollars
#'
#' @param base_year 1913 or 1800
#'
#' @return a \code{tibble}
#' @export
#' @import rvest xml2 dplyr readr purrr
#' @examples
#' us_inflation(base_year = 1913)
us_inflation <-
  function(base_year = 1913) {
    if (base_year == 1913) {
      "Getting US inflation data from 1913" %>% cat(fill = T)
      data <- .get_1913_inflation()
      return(data)
    }
      "Getting US inflation data from 1800" %>% cat(fill = T)
      data <- .get_1800_inflation()
  }
