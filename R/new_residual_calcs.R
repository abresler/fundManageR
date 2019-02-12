# gdeltr2::load_needed_packages(c("tidyverse", "asbmisc", "fundManageR", "furrr", "magrittr"))

.residual_value <-
  function(
    basis = 100,
    debt = 75,
    net_operating_income = 10,
    exit_cap_rate = .08,
    cost_of_sale = .05,
    purchase_date = "2016-08-01",
    sale_date = Sys.Date(),
    pct_lp = .8,
    promote_structure = c("20 over 12", "40 over 18"),
    has_guarantee = F,
    calculate_irr = T,
    return_message = T
    ) {
    if (length(net_operating_income) == 0) {
      stop("Enter Net Operating Income")
    }
    if (length(exit_cap_rate) == 0) {
      stop("Enter exit cap")
    }

    if (length(sale_date) == 0) {
      stop("Enter Sale Date")
    }

    gross_value <-
      formattable::currency(net_operating_income / exit_cap_rate, digits = 2)

    if (length(cost_of_sale) == 0) {
      cost_of_sale <- 0
    }

  transaction_cost <-
    gross_value * -cost_of_sale

  residual_net <-
    formattable::currency(gross_value + transaction_cost, digits = 2)


  exit_cap_rate <- formattable::percent(exit_cap_rate, digits = 3)
  net_operating_income <- formattable::currency(net_operating_income, digits = 2)
  basis <-
    formattable::currency(basis, digits = 2)
  equity <-
    basis - debt
  debt <- formattable::currency(debt, digits = 2)
  purchase_date <- lubridate::ymd(purchase_date)

  data <-
    tibble(
      datePurchase = purchase_date,
      amountBasis = -basis,
      amountDebt = debt,
      amountEquity = amountBasis + amountDebt,
      equityLP = -pct_lp * amountEquity,
      equityGP = -(1 - pct_lp) * amountEquity,
      dateSale = sale_date,
      amountNOI = net_operating_income,
      pctExitCap = exit_cap_rate,
      amountResidualGross = gross_value,
      amountTransactionCost = transaction_cost,
      amountResidualNet = residual_net,
      amountUnleveragedCashFlow = residual_net
    )

  if (return_message) {
    glue::glue(
      "\n\nExiting asset with a basis of {basis} on {sale_date} for {gross_value} [{residual_net} NET] - an {exit_cap_rate} cap rate on {net_operating_income} in Net Operating Income less {transaction_cost} in transactions costs\n\n"
    ) %>% message()
  }

  is_under_water <-
    residual_net - debt < 0

  if (is_under_water) {
    if (has_guarantee) {
      glue::glue("\n\nDidn't MAMMA tell you to NEVER personally guarantee a loan??\n\nI guess not\n\nYou lost all of your equity and are on the hook for an additional {abs(residual_net - debt)}") %>% message()
    } else {
    glue::glue("\n\nProject is underwater by {residual_net - debt}\nYou will lose all your equity but that is it because there is no loan guarantee\n\n") %>% message()
    }
  }

  lncf <-
    residual_net - debt
  to_equity <-
    ifelse(
      lncf < 0 &
        !has_guarantee,
      0,
      -lncf) %>% formattable::currency(digits  = 2)

  equity_slug <- case_when(
    to_equity == 0 ~ "to equity",
    to_equity > 0 ~ "additional equity contribution",
    to_equity < 0 ~ "equity distribution"
  )

  glue::glue("\n\n{to_equity} {equity_slug} net of {formattable::currency(debt, digits = 2)} existing debt\n\n") %>% message()

  data <-
    data %>%
    mutate(
      amountLeveragedCashFlow = amountUnleveragedCashFlow - amountDebt,
      toEquity = ifelse(
        amountLeveragedCashFlow < 0 &
          !has_guarantee,
        0,
        -amountLeveragedCashFlow
      ),
      isEquityUnderWater = as.logical(amountLeveragedCashFlow < 0)
    )

  if (calculate_irr) {
    "\nCalculating Partnership Levered IRR\n" %>% message()
    dates <- c(purchase_date, sale_date)
    cfs <- c(equity, to_equity)
    df_irr <- calculate_irr_periods(dates = dates, cash_flows = cfs)
  }

  if (length(promote_structure) > 0) {
    dates <-
      c(purchase_date, sale_date)
    cfs <-
      c(equity, to_equity)
    calculate_cash_flow_waterfall_partnership(
      dates = dates,
      cash_flows = cfs
    )
  }

  data
  }


#' Residual value calculations
#'
#' @param net_operating_income vector of net operating incomes
#' @param exit_cap_rate vector of exit cap rates
#' @param cost_of_sale vector of cost of sale
#' @param debt vector
#' @param transaction_cost
#' @param sale_date
#' @param has_guarantee
#' @param return_message
#'
#' @return
#' @export
#'
#' @examples
residual_values <-
  function(net_operating_income = 10,
    exit_cap_rate = .08,
    cost_of_sale = .05,
    debt = 0,
    sale_date = Sys.Date(),
    has_guarantee = F,
    return_message = T) {
    df_inputs <-
      expand.grid(
        net_operating_income = net_operating_income,
        exit_cap_rate = exit_cap_rate,
        cost_of_sale = cost_of_sale,
        debt = debt,
        sale_date = sale_date,
        has_guarantee = has_guarantee,
        stringsAsFactors = F
      ) %>% as_tibble()

    scenarios <- nrow(df_inputs)
    .residual_value_safe <- purrr::possibly(.residual_value, tibble())

    all_data <-
      1:nrow(df_inputs) %>%
      map_dfr(function(x) {
        df_row <- df_inputs %>% slice(x)
        if (return_message) {
          glue::glue("Scenario {x} of {scenarios}") %>% message()
        }

        .residual_value_safe(
          net_operating_income = df_row$net_operating_income,
          exit_cap_rate = df_row$exit_cap_rate,
          cost_of_sale = df_row$cost_of_sale,
          debt = df_row$debt,
          sale_date = df_row$sale_date,
          has_guarantee = df_row$has_guarantee,
          return_message = return_message
        )
      }) %>%
      suppressWarnings()

    amount_names <- all_data %>% select(matches("^amount|^to|equity")) %>% names()

    if (length(amount_names) >0)  {
      all_data <- all_data %>%
        mutate_at(amount_names,
          list(function(x) {
            x %>% formattable::currency(digits = 3)
          }))
    }

    percent_names <-
      all_data %>% select(matches("^pct|percent")) %>% names()
    if (length(percent_names) > 0)  {
      all_data <- all_data %>%
        mutate_at(percent_names,
          list(function(x) {
            x %>% formattable::percent(digits = 3)
          }))
    }
    all_data <- all_data %>%
      mutate(isEquityUnderWater = as.logical(isEquityUnderWater))
    all_data
  }
