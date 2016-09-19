calculate_periods_irr <-
  function(dates = c(
    "2016-06-01",
    "2017-05-31",
    "2018-05-31",
    "2019-05-31",
    "2020-05-31",
    "2021-05-31",
    "2022-05-31",
    "2023-05-31",
    "2024-05-31",
    "2025-05-31",
    "2026-05-31"
  ),
  cash_flows = c(
    -3000,
    478.515738547242,
    478.515738547242,
    478.515738547242,
    478.515738547242,
    478.515738547242,
    478.515738547242,
    478.515738547242,
    478.515738547242,
    478.515738547242,
    478.515738547278
  ),
  date_format = '%Y-%m-%d',
  scale_to_100 = F,
  return_percentage = F,
  return_df = T,
  return_wide = T,
  return_message = T) {
    secant <-
      function(par,
               fn,
               tol = 1.e-07,
               itmax = 100,
               trace = TRUE,
               ...) {
        # par = a starting vector with 2 starting values
        # fn = a function whose first argument is the variable of interest
        #
        if (length(par) != 2)
          stop("You must specify a starting parameter vector of length 2")
        p.2 <- par[1]
        p.1 <- par[2]
        f <- rep(NA, length(par))
        f[1] <- fn(p.1, ...)
        f[2] <- fn(p.2, ...)
        iter <- 1
        pchg <- abs(p.2 - p.1)
        fval <- f[2]
        if (trace)
          # cat("par: ", par, "fval: ", f, "\n")
          while (pchg >= tol & abs(fval) > tol & iter <= itmax) {
            p.new <- p.2 - (p.2 - p.1) * f[2] / (f[2] - f[1])
            pchg <- abs(p.new - p.2)
            fval <- fn(p.new, ...)
            p.1 <- p.2
            p.2 <- p.new
            f[1] <- f[2]
            f[2] <- fval
            iter <- iter + 1
          }
        list(par = p.new,
             value = fval,
             iter = iter)
      }
    npv <-
      function (irr, cashFlow, times)
        sum(cashFlow / (1 + irr) ^ times)
    cfDate <-
      dates %>%
      as.Date(format = date_format)
    times <-
      difftime(cfDate, cfDate[1], units = "days") %>% as.numeric() / 365.24

    s <-
      secant(
        par = c(0, 0.1),
        fn = npv,
        cashFlow = cash_flows,
        times = times
      )
    irr <-
      s$par

    if (return_percentage == T & scale_to_100 == T) {
      stop("Sorry you cannot return a percentage and scale to 100")
    }

    if (return_percentage) {
      irr <-
        irr %>% formattable::percent()
    }

    if (scale_to_100) {
      irr <-
        irr * 100
    }
    dateStart <-
      min(dates) %>% ymd

    dateEnd <-
      max(dates) %>% ymd
    equityContributions <-
      cash_flows[cash_flows < 0] %>%
      sum %>%
      formattable::currency(digits = 2)

    equityDistributions <-
      cash_flows[cash_flows > 0] %>%
      sum %>%
      formattable::currency(digits = 2)
    multipleCapital <-
      -(equityDistributions / equityContributions) %>% digits(digits = 3)
    valueProfit <-
      equityDistributions + equityContributions
    if (return_df ==  T)
      data <-
      data_frame(
        dateStart,
        dateEnd,
        equityContributions,
        equityDistributions,
        pctIRR = irr,
        valueProfit,
        multipleCapital,
        dateTimeCF = Sys.time()
      )
    else {
      data <-
        irr
    }

    if (return_message) {
      "Cash Flow Produces a " %>%
        paste0(
          irr * 100,
          '% irr\nFrom ',
          dateStart,
          ' to ',
          dateEnd,
          '\n',
          'Profit of ',
          valueProfit,
          '\nCapital Multiple of ',
          multipleCapital
        ) %>%
        message()
    }

    if (return_wide == F) {
      data %>%
        gather(metric, value, -c(dateStart, dateEnd, dateTimeCF))
    }

    return(data)
  }

parse_for_currency_value <-
  function(x) {
  value <-
    x %>%
    readr::parse_number() %>%
    currency
  return(value)
}

parse_for_percentage <-
  function(x) {
    value <-
      x %>%
      parse_number()

    if (value >= 1) {
      value <-
        value /  100
    }
    value <-
      value %>%
      percent()

    return(value)
  }

parse_multiple <-
  function(x) {
   value <-
     x %>%
      parse_number

   return(value)
  }

cap_rate_valuation <-
  function(cap_rate = .0615, net_operating_income = "$27,500,000",
         cost_of_sale = "5%", debt_balance = "$350,000,000", return_wide = T) {

    noi <-
      net_operating_income %>%
      parse_for_currency_value

    debt <-
      debt_balance %>%
      parse_for_currency_value()

    pct_cap_rate <-
      cap_rate %>%
      parse_for_percentage()

    pct_sale_cost <-
      cost_of_sale %>%
      parse_for_percentage()

    amountValuationGross <-
      noi  / cap_rate

    amountCostSale <-
      -((pct_sale_cost %>% as.numeric) * amountValuationGross)

    amountValuationNet <-
      amountValuationGross + amountCostSale

    amountDebtRepayment <-
      -min(amountValuationNet, debt)

    amountEquityDistribution <-
      -max(0, amountValuationNet + amountDebtRepayment) %>% currency

    cash_check <-
    !amountValuationGross + amountCostSale + amountDebtRepayment + amountEquityDistribution == 0

    if (cash_check) {
      stop("Cash waterfall does not tie")
    }

    value_df <-
      data_frame(pctCapRate = pct_cap_rate,
               pctCostSale = pct_sale_cost,
               amountNetOperatingIncome = noi,
               amountDebtBalance = debt,
               amountValuationGross,
               amountCostSale,
               amountValuationNet,
               amountDebtRepayment,
               amountEquityDistribution)

    if (!return_wide) {
      value_df <-
        value_df %>%
        dplyr::select(-amountValuationNet) %>%
        gather(item, value, -c(pctCapRate, amountNetOperatingIncome, amountDebtBalance, pctCostSale)) %>%
        mutate(value = value %>% currency(digits = 2)) %>%
        suppressWarnings()
    }
    return(value_df)
  }

ebtida_multiple_value <-
  function(ebitda_multiple = 10, ebitda = "$27,500,000",
           cost_of_sale = "5%", debt_balance = "$350,000,000", return_wide = T) {

    ebitda <-
      ebitda %>%
      parse_for_currency_value

    debt <-
      debt_balance %>%
      parse_for_currency_value()

    multipleEBITDA <-
      ebitda_multiple %>%
      parse_multiple()

    pct_sale_cost <-
      cost_of_sale %>%
      parse_for_percentage()

    amountValuationGross <-
      ebitda * multipleEBITDA

    amountCostSale <-
      -((pct_sale_cost %>% as.numeric) * amountValuationGross)

    amountValuationNet <-
      amountValuationGross + amountCostSale

    amountDebtRepayment <-
      -min(amountValuationNet, debt)

    amountEquityDistribution <-
      -max(0, amountValuationNet + amountDebtRepayment) %>% currency

    cash_check <-
      !amountValuationGross + amountCostSale + amountDebtRepayment + amountEquityDistribution == 0

    if (cash_check) {
      stop("Cash waterfall does not tie")
    }

    value_df <-
      data_frame(multipleEBITDA,
                 pctCostSale = pct_sale_cost,
                 amountEBITDA = ebitda,
                 amountDebtBalance = debt,
                 amountValuationGross,
                 amountCostSale,
                 amountValuationNet,
                 amountDebtRepayment,
                 amountEquityDistribution)

    if (!return_wide) {
      value_df <-
        value_df %>%
        dplyr::select(-amountValuationNet) %>%
        gather(item, value, -c(multipleEBITDA, amountEBITDA, amountDebtBalance, pctCostSale)) %>%
        mutate(value = value %>% currency(digits = 2)) %>%
        suppressWarnings()
    }
    return(value_df)
  }

#' Calculate residual value
#'
#' @param cap_rates Vector of Capitalization Rates in percent or character percent form
#' @param net_operating_income Vector of Net Operating Income in numeric or character numeric/currency form
#' @param cost_of_sale Vector of Cost of Sale in percent or character percent form
#' @param debt_balance Vector of anticipated Debt Balance at sale in numeric or character numeric/currency form
#' @param return_wide
#' @import readr dplyr purrr formattable
#' @return
#' @export
#'
#' @examples
calculate_residual_valuation_cap_rates <-
  function(cap_rates = c(.05, .0525, .06, .2),
           net_operating_income = "$27,500,000",
           cost_of_sale = "5%", debt_balance = "$350,000,000", return_wide = T) {

    scenario_matrix <-
      expand.grid(cap_rate = cap_rates,
                noi = net_operating_income,
                cost_sale = cost_of_sale,
                debt = debt_balance,
                stringsAsFactors = F) %>%
      as_data_frame

    scenario_df <-
      1:nrow(scenario_matrix) %>%
      map_df(function(x) {
        cap_rate_valuation(
          cap_rate = scenario_matrix$cap_rate[[x]],
          net_operating_income = scenario_matrix$noi[[x]],
          cost_of_sale = scenario_matrix$cost_sale[[x]],
          debt_balance = scenario_matrix$debt[[x]],
          return_wide = T
        )
      }) %>%
      mutate(idScenario = 1:n()) %>%
      dplyr::select(idScenario, everything())

    scenario_df <-
      scenario_df %>%
      mutate_at(.cols =
                  scenario_df %>% dplyr::select(matches("^pct")) %>% names,
                funs(. %>% percent(digits = 2))) %>%
      mutate_at(.cols =
                  scenario_df %>% dplyr::select(matches("^amount")) %>% names,
                funs(. %>% currency(digits = 2)))
    if (!return_wide) {
      scenario_df <-
        scenario_df %>%
        dplyr::select(-amountValuationNet) %>%
        gather(item, value, -c(idScenario, pctCapRate, amountNetOperatingIncome, amountDebtBalance, pctCostSale)) %>%
        mutate(value = value %>% currency(digits = 2)) %>%
        suppressWarnings()
    }

    return(scenario_df)
  }


#' Calculate residual value for a set of given EBITDA based inputs
#'
#' @param ebitda_multiples Vector of EBITDA Multiples in numeric or character
#' @param ebitda Vector of EBITDA in numeric or character numeric/currency form
#' @param cost_of_sale Vector of Cost of Sale in percent or character percent form
#' @param debt_balance Vector of anticipated Debt Balance at sale in numeric or character numeric/currency form
#' @param return_wide Return data in wide or long form
#' @import readr dplyr purrr formattable
#' @return
#' @export
#'
#' @examples
calculate_residual_valuation_ebitda_multiples <-
  function(ebitda_multiples = c(5, 10, 15, 20),
           ebitda = "$27,500,000",
           cost_of_sale = "5%", debt_balance = "$350,000,000",
           return_wide = T) {

    scenario_matrix <-
      expand.grid(ebitda_multiple = ebitda_multiples,
                  ebitda = ebitda,
                  cost_sale = cost_of_sale,
                  debt = debt_balance,
                  stringsAsFactors = F) %>%
      as_data_frame

    scenario_df <-
      1:nrow(scenario_matrix) %>%
      map_df(function(x) {
        ebtida_multiple_value(
          ebitda_multiple = scenario_matrix$ebitda_multiple[[x]],
          ebitda = scenario_matrix$ebitda[[x]],
          cost_of_sale = scenario_matrix$cost_sale[[x]],
          debt_balance = scenario_matrix$debt[[x]],
          return_wide = T
        )
      }) %>%
      mutate(idScenario = 1:n()) %>%
      dplyr::select(idScenario, everything())

    scenario_df <-
      scenario_df %>%
      mutate_at(.cols =
                  scenario_df %>% dplyr::select(matches("^pct")) %>% names,
                funs(. %>% percent(digits = 2))) %>%
      mutate_at(.cols =
                  scenario_df %>% dplyr::select(matches("^amount")) %>% names,
                funs(. %>% currency(digits = 2)))
    if (!return_wide) {
      scenario_df <-
        scenario_df %>%
        dplyr::select(-amountValuationNet) %>%
        gather(item, value, -c(multipleEBITDA, amountEBITDA, amountDebtBalance, pctCostSale)) %>%
        mutate(value = value %>% currency(digits = 2)) %>%
        suppressWarnings()
    }

    return(scenario_df)
  }


post_money_valuation <-
  function(pre_money_valuation = "$45,000,000",
   percent_sold = "10%") {
    options(scipen = 999999)
    pre_money <-
      pre_money_valuation %>%
      parse_for_currency_value()

    pct_sold <-
      percent_sold %>%
      parse_for_percentage() %>%
      as.numeric

    new_capital <-
      (pct_sold * pre_money) %>% currency

    total_val <-
      pre_money + new_capital

    valuation_data <-
      data_frame(valuationPreMoney = pre_money,
                 amountCapitalInvestment = new_capital) %>%
      mutate(
        pctOwnershipExistingShareholders = (pre_money / total_val) %>% percent,
        pctOwnershipNewInvestment = (new_capital / total_val) %>% percent
      )
    return(valuation_data)
  }

#' Calculates range of post investment valuations
#'
#' @param pre_money_valuation  Vector of of valuations in numeric or character
#' @param percent_sold  Vector of of amount of business sold in percent or character percent form
#' @param return_wide
#' @param return_wide Return data in wide or long form
#' @import readr dplyr purrr formattable
#' @return
#' @export
#'
#' @examples
calculate_valuation_post_money <-
  function(pre_money_valuation = "$45,000,000",
           percent_sold = "10%",
           return_wide = T) {

    scenario_matrix <-
      expand.grid(pre_money_valuation = pre_money_valuation,
                  percent_sold = percent_sold,
                  stringsAsFactors = F) %>%
      as_data_frame

    scenario_df <-
      1:nrow(scenario_matrix) %>%
      map_df(function(x) {
        post_money_valuation(
          pre_money_valuation = scenario_matrix$pre_money_valuation[[x]],
          percent_sold = scenario_matrix$percent_sold[[x]]
        )
      }) %>%
      mutate(idScenario = 1:n()) %>%
      dplyr::select(idScenario, everything())

    scenario_df <-
      scenario_df %>%
      mutate_at(.cols =
                  scenario_df %>% dplyr::select(matches("^pct")) %>% names,
                funs(. %>% percent(digits = 2))) %>%
      mutate_at(.cols =
                  scenario_df %>% dplyr::select(matches("^amount|valuation")) %>% names,
                funs(. %>% currency(digits = 2)))
    if (!return_wide) {
      scenario_df <-
        scenario_df %>%
        dplyr::select(-matches("pct")) %>%
        gather(item, value, -c(idScenario)) %>%
        mutate(value = value %>% currency(digits = 2)) %>%
        suppressWarnings()
    }
    return(scenario_df)
  }


#' Calculate proceeds from share sale
#'
#' @param price
#' @param shares
#'
#' @return
#' @export
#' @importFrom formattable currency
#' @examples
calculate_share_proceeds <-
  function(price, shares) {
    proceeds <-
      (price * shares) %>% currency()
    return(proceeds)
  }
