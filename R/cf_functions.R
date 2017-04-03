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
      formattable::currency()
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
  function(cap_rate = .0615,
           net_operating_income = "$27,500,000",
           cost_of_sale = "5%",
           debt_balance = "$350,000,000",
           return_wide = T) {
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
      data_frame(
        pctCapRate = pct_cap_rate,
        pctCostSale = pct_sale_cost,
        amountNetOperatingIncome = noi,
        amountDebtBalance = debt,
        amountValuationGross,
        amountCostSale,
        amountValuationNet,
        amountDebtRepayment,
        amountEquityDistribution
      )

    if (!return_wide) {
      value_df <-
        value_df %>%
        dplyr::select(-amountValuationNet) %>%
        gather(
          item,
          value,
          -c(
            pctCapRate,
            amountNetOperatingIncome,
            amountDebtBalance,
            pctCostSale
          )
        ) %>%
        mutate(value = value %>% currency(digits = 2)) %>%
        suppressWarnings()
    }
    return(value_df)
  }

ebtida_multiple_value <-
  function(ebitda_multiple = 10,
           ebitda = "$27,500,000",
           cost_of_sale = "5%",
           debt_balance = "$350,000,000",
           return_wide = T) {
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
      data_frame(
        multipleEBITDA,
        pctCostSale = pct_sale_cost,
        amountEBITDA = ebitda,
        amountDebtBalance = debt,
        amountValuationGross,
        amountCostSale,
        amountValuationNet,
        amountDebtRepayment,
        amountEquityDistribution
      )

    if (!return_wide) {
      value_df <-
        value_df %>%
        dplyr::select(-amountValuationNet) %>%
        gather(item,
               value,
               -c(
                 multipleEBITDA,
                 amountEBITDA,
                 amountDebtBalance,
                 pctCostSale
               )) %>%
        mutate(value = value %>% currency(digits = 2)) %>%
        suppressWarnings()
    }
    return(value_df)
  }

#' Residual value, capitalization rate method
#'
#' This function calculates \href{https://en.wikipedia.org/wiki/Residual_value}{residual values}
#' based upon the specified inputs.  See \code{\link{calculate_residual_valuation_ebitda_multiples}} for
#' EBITDA multiple method.
#'
#' @param cap_rates vector of capitalization rates in percent or character form
#' @param net_operating_income vector of net operating income in numeric or character numeric/currency form
#' @param cost_of_sale vector of cost of sale in percent or character percent form
#' @param debt_balance vector of anticipated debt balance at sale in numeric or character numeric/currency form
#' @param return_wide \code{TRUE} return wide or \code{FALSE}
#' @import readr dplyr purrr formattable
#' @return a \code{data_frame}
#' @family calculation
#' @family residual value calculation
#' @export
#'
#' @examples
#' calculate_residual_valuation_cap_rates(cap_rates = c(.05, .0525, .06, .2),
#' net_operating_income = "$27,500,000", cost_of_sale = "5%",debt_balance = "$350,000,000", return_wide = T)
calculate_residual_valuation_cap_rates <-
  function(cap_rates = c(.05, .0525, .06, .2),
           net_operating_income = "$27,500,000",
           cost_of_sale = "5%",
           debt_balance = "$350,000,000",
           return_wide = T) {
    scenario_matrix <-
      expand.grid(
        cap_rate = cap_rates,
        noi = net_operating_income,
        cost_sale = cost_of_sale,
        debt = debt_balance,
        stringsAsFactors = F
      ) %>%
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
      mutate_at(.vars =
                  scenario_df %>% dplyr::select(matches("^pct")) %>% names,
                funs(. %>% percent(digits = 2))) %>%
      mutate_at(.vars =
                  scenario_df %>% dplyr::select(matches("^amount")) %>% names,
                funs(. %>% currency(digits = 2)))
    if (!return_wide) {
      scenario_df <-
        scenario_df %>%
        dplyr::select(-amountValuationNet) %>%
        gather(
          item,
          value,
          -c(
            idScenario,
            pctCapRate,
            amountNetOperatingIncome,
            amountDebtBalance,
            pctCostSale
          )
        ) %>%
        mutate(value = value %>% currency(digits = 2)) %>%
        suppressWarnings()
    }

    return(scenario_df)
  }


#' Residual value, EBITDA multiple method
#'
#' This function calculates \href{https://en.wikipedia.org/wiki/Residual_value}{residual values}
#' based upon the specified inputs.  See \code{\link{calculate_residual_valuation_cap_rates}} for
#' capitalization rate method.
#'
#' @param ebitda_multiples vector of EBITDA multiples in numeric or character
#' @param ebitda vector of EBITDA in numeric or character numeric/currency form
#' @param cost_of_sale vector of cost of sale in percent or character percent form
#' @param debt_balance vector of anticipated debt balance at sale in numeric or character numeric/currency form
#' @param return_wide return data in wide or long form
#' @import readr dplyr purrr formattable
#' @return a \code{data_frame}
#' @export
#' @family calculation
#' @family residual value calculation
#' @examples
#' calculate_residual_valuation_ebitda_multiples(ebitda_multiples = c(5, 10, 15, 20), ebitda = "$27,500,000", cost_of_sale = "5%", debt_balance = "$350,000,000", return_wide = T)
calculate_residual_valuation_ebitda_multiples <-
  function(ebitda_multiples = c(5, 10, 15, 20),
           ebitda = "$27,500,000",
           cost_of_sale = "5%",
           debt_balance = "$350,000,000",
           return_wide = T) {
    scenario_matrix <-
      expand.grid(
        ebitda_multiple = ebitda_multiples,
        ebitda = ebitda,
        cost_sale = cost_of_sale,
        debt = debt_balance,
        stringsAsFactors = F
      ) %>%
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
      mutate_at(.vars =
                  scenario_df %>% dplyr::select(matches("^pct")) %>% names,
                funs(. %>% percent(digits = 2))) %>%
      mutate_at(.vars =
                  scenario_df %>% dplyr::select(matches("^amount")) %>% names,
                funs(. %>% currency(digits = 2)))
    if (!return_wide) {
      scenario_df <-
        scenario_df %>%
        dplyr::select(-amountValuationNet) %>%
        gather(item,
               value,
               -c(
                 multipleEBITDA,
                 amountEBITDA,
                 amountDebtBalance,
                 pctCostSale
               )) %>%
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

#' Post money valuation splits
#'
#' This function calculates the the post-money
#' ownership splits of an entity for specified inputs.
#'
#' @param pre_money_valuation vector of of valuations in numeric or character
#' @param percent_sold  vector of of amount of business sold in percent or character percent form
#' @param return_wide Return data in wide or long form
#' @import readr dplyr purrr formattable tidyr
#' @return a \code{data_frame()}
#' @family calculation
#' @family venture capital
#' @export
#'
#' @examples
#' calculate_valuation_post_money(pre_money_valuation = "$45,000,000", percent_sold = "10%", return_wide = T)
calculate_valuation_post_money <-
  function(pre_money_valuation = "$45,000,000",
           percent_sold = "10%",
           return_wide = T) {
    scenario_matrix <-
      expand.grid(
        pre_money_valuation = pre_money_valuation,
        percent_sold = percent_sold,
        stringsAsFactors = F
      ) %>%
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
      mutate_at(.vars =
                  scenario_df %>% dplyr::select(matches("^pct")) %>% names,
                funs(. %>% percent(digits = 2))) %>%
      mutate_at(.vars =
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


#' Share proceeds
#'
#' This function calculates
#' distributable proceeds from
#' a share sale based upon specified inputs.
#'
#' @param price numeric price
#' @param shares share count
#'
#' @return numeric \code{vector}
#' @export
#' @family calculation
#' @importFrom formattable currency
#' @examples
#' calculate_share_proceeds(price = 9, shares = 150000)
calculate_share_proceeds <-
  function(price = 10, shares = 1000000) {
    proceeds <-
      (price * shares) %>% currency()
    return(proceeds)
  }

calculate_basis <-
  function(purchase_price = "$10,000,000",
           capitalized_acquisition_costs = "$300,0000",
           capital_investment = "$1,200,000") {
    options(scipen = 999)

    if (purchase_price %>% is_null) {
      stop("Please enter a purchase price")
    }

    amountPurchasePrice <-
      purchase_price %>%
      parse_for_currency_value()

    if (!capitalized_acquisition_costs %>% is_null) {
      amountCapitalizedCosts <-
        capitalized_acquisition_costs %>%
        parse_for_currency_value()
    } else {
      amountCapitalizedCosts <-
        0
    }

    if (!capitalized_acquisition_costs %>% is_null) {
      amountCapitalInvestment <-
        capital_investment %>%
        parse_for_currency_value()
    } else {
      capital_investment <-
        0
    }

    basis_df <-
      data_frame(amountPurchasePrice,
                 amountCapitalInvestment,
                 amountCapitalizedCosts) %>%
      mutate(amountBasis = amountPurchasePrice + amountCapitalInvestment + amountCapitalizedCosts)
    return(basis_df)
  }

calculate_capitalization <-
  function(purchase_price = "$9,700,000",
           capitalized_acquisition_costs = "$300,000",
           capital_investment = "$0",
           loan_to_cost = .7,
           borrow_capital_investment = T,
           borrow_capitalized_costs = F,
           leverage_threshold = .95) {
    if (loan_to_cost %>% is_null()) {
      stop("Please enter a loan to cost even if it is zero")
    }

    if (leverage_threshold %>% is_null()) {
      leverage_threshold <-
        1
    }

    pct_ltc <-
      loan_to_cost %>% parse_for_percentage()

    if (pct_ltc > leverage_threshold) {
      leverage_message <-
        "\nDon't be a reckless idiot, remember what happend to Lehman Brothers???\nDon't know Lehman Brothers, Google it\n" %>%
        paste0(
          pct_ltc,
          ' is unprudent leverage\nA more reasonable amount of leverage is: ',
          leverage_threshold %>% formattable::percent(),
          '\nChange your leverage assumptions and try again'
        )

      stop(leverage_message)
    }
    basis_df <-
      calculate_basis(
        purchase_price = purchase_price,
        capitalized_acquisition_costs = capitalized_acquisition_costs,
        capital_investment = capital_investment
      )
    if (borrow_capitalized_costs) {
      debt_basis <-
        basis_df %>% mutate(amountDebtBasis = amountPurchasePrice + amountCapitalizedCosts) %>%
        .$amountDebtBasis
    } else {
      debt_basis <-
        basis_df$amountPurchasePrice
    }

    loan_proceeds <-
      debt_basis * pct_ltc

    if (borrow_capital_investment) {
      loan_proceeds <-
        loan_proceeds + basis_df$amountCapitalInvestment
    }

    capital_stack_df <-
      basis_df %>%
      mutate(
        amountLoanProceeds = -loan_proceeds,
        amountEquity = -(amountBasis + amountLoanProceeds)
      )

    return(capital_stack_df)
  }

get_data_monthly_periods <-
  function(start_date = "2016-06-01",
           term_years = 25,
           term_months = 0) {
    periods <-
      term_years * 12 + term_months

    periods <-
      0:periods

    start_date <-
      start_date %>% lubridate::ymd %>% as.Date()

    get_end_of_period <-
      function(period = 0) {
        if (period == 0) {
          period_date <-
            start_date %m+% months(period) %>%
            as.character() %>%
            as.Date()
        }

        if (period == 1) {
          period_date <-
            start_date %m+% months(0) %>%
            timeDate::timeLastDayInMonth %>%
            as.character() %>%
            as.Date()
        }

        if (period > 1) {
          period_date <-
            start_date %m+% months(period - 1) %>%
            timeDate::timeLastDayInMonth %>%
            as.character() %>%
            as.Date()
        }
        period_df <-
          data_frame(idPeriod = period, datePeriod = period_date)
        return(period_df)

      }

    all_periods <-
      periods %>%
      purrr::map(function(x) {
        get_end_of_period(period = x)
      }) %>%
      compact %>%
      bind_rows()

    all_periods <-
      all_periods %>%
      mutate(yearPeriod = ifelse(idPeriod == 0, 0, (idPeriod %/% 12) + 1)) %>%
      dplyr::select(idPeriod, yearPeriod, everything())

    return(all_periods)
  }

pmt <-
  function (r, n, pv, fv, type = 0) {
    if (type != 0 && type != 1) {
      print("Error: type should be 0 or 1!")
    }
    else {
      pmt <- (pv + fv / (1 + r) ^ n) * r / (1 - 1 / (1 + r) ^ n) * (-1) *
        (1 + r) ^ (-1 * type)
      return(pmt)
    }
  }

#' Loan payment calculation
#'
#' Calculate loan payment information
#' based upon specified parameters.
#'
#' @param loan_start_date date loan starts in year, month, date form
#' @param amount_initial_draw amount of initial draw
#' @param is_interest_only \code{TRUE} has interest only periods
#' @param interest_only_periods  count of interest only periods
#' @param interest_rate  interest rate in character or numeric form
#' @param is_actual_360 \code{TRUE} interest calculated on actual/360 basis
#' @param amortization_years amortization in years
#' @param amortization_months amortization additional months
#' @param term_years term of the loan in years
#' @param term_months term of the loan additional months
#' @param pct_loan_fee loan fee in percent
#' @param balloon_year year loan baloons
#' @param override_monthly_interest  \code{TRUE} override
#' @param interest_reserve_period periods of interest reserve
#' @param balloon_month month loan baloons
#' @param return_annual_summary \code{TRUE} returns annual summary
#'
#' @return a \code{data_frame}
#' @export
#' @family leveraged finance calculation
#' @family calculation
#' @examples
#' calculate_loan_payment(loan_start_date = "2016-06-01", amount_initial_draw = 3000, is_interest_only = F, interest_only_periods = 24,
#' interest_rate = "10%", is_actual_360 = TRUE, amortization_years = 10, amortization_months = 0,
#' term_years = 10, term_months = 0, pct_loan_fee = 0, balloon_year = 10,
#' override_monthly_interest = FALSE, interest_reserve_period = 0, balloon_month = 0,
#' return_annual_summary = F)
calculate_loan_payment <-
  function(loan_start_date = "2016-06-01",
           amount_initial_draw = 3000,
           is_interest_only = F,
           interest_only_periods = 24,
           interest_rate = "10%",
           is_actual_360 = TRUE,
           amortization_years = 10,
           amortization_months = 0,
           term_years = 10,
           term_months = 0,
           pct_loan_fee = 0,
           balloon_year = 10,
           override_monthly_interest = FALSE,
           interest_reserve_period = 0,
           balloon_month = 0,
           return_annual_summary = F) {
    options(scipen = 99999)
    options(digits = 10)

    interest_rate <-
      interest_rate %>%
      parse_for_percentage()
    if (is_actual_360 == T) {
      daily_interest <-
        interest_rate / 360
      net_rate <-
        interest_rate / 360 * 365
    } else {
      daily_interest <-
        interest_rate / 365
      net_rate <-
        interest_rate
    }

    amortization_periods <-
      (amortization_years * 12) + amortization_months

    loan_periods <-
      (balloon_year * 12) + balloon_month

    loan_period_df <-
      get_data_monthly_periods(start_date = loan_start_date,
                               term_years = term_years,
                               term_months = term_months)
    loan_period_df <-
      loan_period_df %>%
      mutate(
        isIO = ifelse(is_interest_only == T &
                        idPeriod <= interest_only_periods, T, F),
        isActiveLoan = ifelse(idPeriod <= loan_periods, T, F),
        amountInitialDraw = ifelse(idPeriod == 0, amount_initial_draw, 0),
        amountLoanFee = amountInitialDraw * pct_loan_fee
      )

    loan_period_df <-
      loan_period_df %>%
      dplyr::filter(isActiveLoan == T)

    periods <-
      loan_period_df$idPeriod
    all_payment_data <-
      data_frame()

    for (period in periods) {
      period_index <-
        period + 1

      datePeriod <-
        loan_period_df$datePeriod[period_index]

      drawInitial <-
        loan_period_df$amountInitialDraw[period_index]

      drawAdditional <-
        0 # loan_period_df$amountAdditionalDraw[period_index] -- layer in eventully

      is_interest_only <-
        loan_period_df$isIO[period_index]

      periodFee <-
        loan_period_df$amountLoanFee[period_index]

      if (period == 0) {
        month_df <-
          data_frame(
            idPeriod = period,
            dateStartPeriod = datePeriod,
            dateEndPeriod = datePeriod,
            isIO = is_interest_only,
            balanceInitial = 0 %>% currency(digits = 2),
            amountInitialDraw = drawInitial %>% currency(digits = 2),
            amountAdditionalDraw = drawAdditional %>% currency(digits = 2),
            paymentInterest = 0 %>% currency(digits = 2),
            paymentPrincipal = 0 %>% currency(digits = 2),
            amountRepayment = 0 %>% currency(digits = 2),
            amountLoanFee = periodFee %>% currency(digits = 2)
          ) %>%
          mutate(balanceEnd = amountInitialDraw + amountAdditionalDraw)
      }
      if (period > 0) {
        initial_balance <-
          all_payment_data$balanceEnd[period_index - 1]

        total_balance <-
          initial_balance + drawInitial + drawAdditional

        balance_basis <-
          (all_payment_data$amountInitialDraw %>% sum()) +
          all_payment_data$amountAdditionalDraw %>% sum()

        start_month <-
          timeDate::timeFirstDayInMonth(datePeriod) %>% as.Date()

        days <-
          (datePeriod  - start_month) + 1

        month_days <-
          as.numeric(days, units = 'days')

        if (override_monthly_interest == T) {
          monthly_interest <-
            net_rate / 12
        } else {
          monthly_interest <-
            daily_interest * month_days
        }

        paymentInterest <-
          monthly_interest * (total_balance)

        if (interest_reserve_period > 0 &
            period <= interest_reserve_period) {
          paymentInterest <-
            0
        }

        if (is_interest_only == T) {
          paymentTotal <-
            paymentInterest
        } else {
          paymentTotal <-
            pmt(
              r = monthly_interest,
              pv = balance_basis,
              fv = 0,
              n = amortization_periods
            )
        }

        paymentPrincipal <-
          abs(paymentTotal) - paymentInterest

        if (period == loan_periods) {
          amountRepayment <-
            initial_balance  + drawInitial + drawAdditional - paymentPrincipal
        } else {
          amountRepayment <-
            0
        }

        month_df <-
          data_frame(
            idPeriod = period,
            dateStartPeriod = start_month,
            dateEndPeriod = datePeriod,
            isIO = is_interest_only,
            balanceInitial = initial_balance %>% currency(digits = 2),
            amountInitialDraw = drawInitial %>% currency(digits = 2),
            amountAdditionalDraw = drawAdditional %>% currency(digits = 2),
            paymentInterest = -paymentInterest %>% currency(digits = 2),
            paymentPrincipal = -paymentPrincipal %>% currency(digits = 2),
            amountRepayment = -amountRepayment %>% currency(digits = 2),
            amountLoanFee = periodFee %>% currency(digits = 2)
          ) %>%
          mutate(
            balanceEnd =
              balanceInitial + amountInitialDraw + amountAdditionalDraw +
              paymentPrincipal + amountRepayment
          )

      }

      all_payment_data <-
        month_df %>%
        bind_rows(all_payment_data) %>%
        arrange((idPeriod))
    }

    all_payment_data <-
      all_payment_data %>%
      mutate(
        balanceInitial = balanceInitial %>% currency(digits = 2),
        amountInitialDraw = amountInitialDraw %>% currency(digits = 2),
        amountAdditionalDraw = amountAdditionalDraw %>% currency(digits = 2),
        paymentInterest = paymentInterest %>% currency(digits = 2),
        paymentPrincipal = paymentPrincipal %>% currency(digits = 2),
        amountRepayment = amountRepayment %>% currency(digits = 2),
        amountLoanFee = amountLoanFee %>% currency(digits = 2),
        balanceEnd = balanceEnd %>% currency(digits = 2)
      ) %>%
      mutate(yearPeriod = ifelse(idPeriod > 0,
                                 ((idPeriod - 1) %/% 12) + 1,
                                 0)) %>%
      dplyr::select(yearPeriod, everything())

    if (return_annual_summary == T) {
      all_payment_data <-
        all_payment_data %>%
        group_by(yearPeriod) %>%
        summarise(
          dateStartPeriod = min(dateStartPeriod),
          dateEndPeriod = max(dateEndPeriod),
          amountInitialDraw = sum(amountInitialDraw),
          amountAdditionalDraw = sum(amountAdditionalDraw),
          paymentInterest = sum(paymentInterest),
          paymentPrincipal = sum(paymentPrincipal),
          amountRepayment = sum(amountRepayment),
          amountLoanFee = sum(amountLoanFee),
          cfLoan = (
            amountInitialDraw + amountAdditionalDraw + paymentInterest + paymentPrincipal + amountRepayment + amountLoanFee
          ) %>% currency(digits = 2)
        ) %>%
        mutate(
          balanceEnd = cumsum(amountInitialDraw) + cumsum(paymentPrincipal) + cumsum(amountRepayment)
        ) %>%
        ungroup
    }

    return(all_payment_data)
  }


calculate_average_payment <-
  function(amount_initial_draw = 3000,
           is_interest_only = F,
           interest_only_periods = 24,
           interest_rate = "10%",
           is_actual_360 = T,
           amortization_years = 10,
           amortization_months = 0,
           term_years = 10,
           term_months = 0,
           pct_loan_fee = 0,
           balloon_year = 10,
           override_monthly_interest = F,
           interest_reserve_period = 0,
           balloon_month = 0) {
    library(lubridate)
    first_of_the_month <-
      (ceiling_date((Sys.Date() %m+% months(0)), "month") - days(1)) + 1

    pmt_df <-
      calculate_loan_payment(
        loan_start_date = first_of_the_month,
        amount_initial_draw = amount_initial_draw,
        is_interest_only = is_interest_only,
        interest_only_periods = interest_only_periods,
        interest_rate = interest_rate,
        is_actual_360 = is_actual_360,
        amortization_years = amortization_years,
        amortization_months = amortization_months,
        term_years = term_years,
        term_months = term_months,
        pct_loan_fee = pct_loan_fee,
        balloon_year = balloon_year,
        balloon_month = balloon_month,
        return_annual_summary = F
      )

    pmt_df <-
      pmt_df %>%
      dplyr::filter(!idPeriod == 0) %>%
      group_by(yearPeriod) %>%
      summarise(
        paymentPrincipal = sum(paymentPrincipal, na.rm = T),
        paymentInterest = sum(paymentInterest, na.rm = T)
      ) %>%
      ungroup %>%
      summarise(
        meanPrincipal = mean(paymentPrincipal, na.rm = T) %>% formattable::currency(),
        meanInterest = mean(paymentInterest, na.rm = T) %>% formattable::currency()
      ) %>%
      mutate(meanPayment = meanPrincipal + meanInterest)

    return(pmt_df)
  }

calculate_leverage_metric <-
  function(purchase_price = "$9,700,000",
           capitalized_acquisition_costs = "$300,000",
           capital_investment = "0",
           revenue = "$1,500,000",
           expenses = "$115,000",
           loan_to_cost = .7,
           borrow_capital_investment = F,
           borrow_capitalized_costs = T,
           leverage_threshold = .95,
           is_interest_only = TRUE,
           interest_only_periods = 12,
           interest_rate = "5%",
           is_actual_360 = TRUE,
           amortization_years = 30,
           amortization_months = 0,
           term_years = 10,
           term_months = 0,
           pct_loan_fee = 0,
           balloon_year = 10,
           balloon_month = 0,
           return_message = F) {
    basis_df <-
      calculate_capitalization(
        purchase_price = purchase_price,
        capitalized_acquisition_costs = capitalized_acquisition_costs,
        capital_investment = capital_investment,
        loan_to_cost = loan_to_cost,
        borrow_capital_investment = borrow_capital_investment,
        borrow_capitalized_costs = borrow_capitalized_costs
      )

    revenue_amount <-
      revenue %>%
      parse_for_currency_value()

    expense_amount <-
      expenses %>%
      parse_for_currency_value()

    if (expense_amount > 0) {
      expense_amount <-
        -expense_amount
    }

    operating_income <-
      revenue_amount + expense_amount

    interest_rate <-
      interest_rate %>%
      parse_for_percentage()

    pct_loan_fee <-
      pct_loan_fee %>%
      parse_for_percentage()

    average_pmt <-
      calculate_average_payment(
        amount_initial_draw = basis_df$amountLoanProceeds %>% abs(),
        is_interest_only = is_interest_only,
        interest_only_periods = interest_only_periods,
        interest_rate = interest_rate,
        is_actual_360 = is_actual_360,
        amortization_years = amortization_years,
        amortization_months = amortization_months,
        term_years = term_years,
        term_months = term_months,
        pct_loan_fee = pct_loan_fee,
        balloon_year = balloon_year,
        override_monthly_interest = override_monthly_interest,
        interest_reserve_period = interest_reserve_period
      )

    data <-
      basis_df %>%
      mutate(
        amountRevenue = revenue_amount,
        amountExpense = expense_amount,
        amountEBITDA_NOI = operating_income
      ) %>%
      bind_cols(average_pmt) %>%
      mutate(
        amountLNCFMean = amountEBITDA_NOI + meanPayment,
        pctLeverage = (-amountLoanProceeds / amountBasis) %>% formattable::percent(),
        pctMarginEBITDA_NOI = (amountEBITDA_NOI / amountRevenue) %>% formattable::percent(),
        pctReturnOnCost = (amountEBITDA_NOI / amountBasis) %>% formattable::percent(),
        pctDebtYieldInitial = -(amountEBITDA_NOI / amountLoanProceeds) %>% formattable::percent(),
        ratioDSCRMean = (amountEBITDA_NOI / -meanPayment) %>% as.numeric() %>% formattable::digits(3),
        pctCashOnCashMean = (amountLNCFMean / -amountEquity) %>% formattable::percent(),
        pctReturnOnEquity = ((amountEBITDA_NOI + meanInterest) / -amountEquity) %>% formattable::percent(),
        rule72Multiple2x = (72 / (pctCashOnCashMean * 100)) %>% as.numeric()
      )
    if (return_message) {
      metric_message <-
        "Basis: " %>%
        paste0(
          data$amountBasis,
          '\n',
          'Leverage: ',
          data$pctLeverage,
          '\n',
          'Interest Rate: ',
          interest_rate,
          '\n',
          'Amortization: ',
          ((amortization_years * 12) + amortization_months),
          ' periods\n',
          'Return on Cost: ',
          data$pctReturnOnCost,
          '\nCash on Cash: ',
          data$pctCashOnCashMean,
          "\nReturn on Equity: ",
          data$pctReturnOnEquity,
          "\nRule of 72: Equity doubles in ",
          data$rule72Multiple2x,
          ' years\n'
        )

      metric_message %>% message()
    }

    return(data)
  }


#' Leveraged return metrics
#'
#' This function returns data with the outputs
#' from a "back-of-the-envelope" leveraged math calculation
#' that can be used for a quick and dirty underwriting of a
#' leveragd cash flow stream.
#'
#' @param purchase_price vector of purchase prices
#' @param capitalized_acquisition_costs  vector of capitalized acquisition costs
#' @param capital_investment vector of capital investment
#' @param revenue vector of revenue amounts
#' @param expenses vector of expenses
#' @param loan_to_cost vector of loan to cost
#' @param interest_rate vector nterest rates
#' @param borrow_capital_investment \code{TRUE} borrow capital investment
#' @param borrow_capitalized_costs \code{TRUE} borrow capitalized costs
#' @param leverage_threshold maximum leverage allowed
#' @param is_interest_only \code{TRUE} has interest only periods
#' @param interest_only_periods count of interest only periods
#' @param is_actual_360 \code{TRUE} is interest calculated on actual 360 basis
#' @param is_actual_360 \code{TRUE} interest calculated on actual/360 basis
#' @param amortization_years amortization in years
#' @param amortization_months amortization additional months
#' @param term_years term of the loan in years
#' @param term_months term of the loan additional months
#' @param pct_loan_fee loan fee in percent
#' @param return_wide \code{TRUE} return data in wide form
#' @param return_message \code{TRUE} return a message after data import
#'
#' @return a \code{data_frame}
#' @export
#' @import readr dplyr lubridate stringr purrr tidyr formattable
#' @family calculation
#' @family leveraged finance calculation
#' @examples
#' calculate_leverage_metrics(purchase_price = c("13,000,000", "9,000,000"),
#' capitalized_acquisition_costs = "300,000",
#' capital_investment = "$100,000", revenue = "$1,200,000", expenses = "$400,000",
#' loan_to_cost = .75,interest_rate = .045,borrow_capital_investment = TRUE,
#' borrow_capitalized_costs = TRUE, leverage_threshold = .8, is_interest_only = FALSE,
#' interest_only_periods = 0, is_actual_360 = TRUE, amortization_years = 30,
#' amortization_months = 0, term_years = 10, term_months = 0, pct_loan_fee = 0, return_wide = TRUE,
#' return_message = TRUE)

calculate_leverage_metrics <-
  function(purchase_price = 0,
           capitalized_acquisition_costs = 0,
           capital_investment = 0,
           revenue = 0,
           expenses = 0,
           loan_to_cost = 0,
           interest_rate = 0,
           borrow_capital_investment = F,
           borrow_capitalized_costs = T,
           leverage_threshold = .95,
           is_interest_only = FALSE,
           interest_only_periods = 0,
           is_actual_360 = TRUE,
           amortization_years = 30,
           amortization_months = 0,
           term_years = 30,
           term_months = 0,
           pct_loan_fee = 0,
           return_wide = T,
           return_message = T) {
    variable_matrix <-
      expand.grid(
        purchase_price = purchase_price,
        capitalized_acquisition_costs = capitalized_acquisition_costs,
        capital_investment = capital_investment,
        revenue = revenue,
        expenses = expenses,
        loan_to_cost = loan_to_cost,
        borrow_capital_investment = borrow_capital_investment,
        borrow_capitalized_costs = borrow_capitalized_costs,
        leverage_threshold = leverage_threshold,
        is_interest_only = is_interest_only,
        interest_only_periods = interest_only_periods,
        interest_rate = interest_rate,
        is_actual_360 = is_actual_360,
        amortization_years = amortization_years,
        amortization_months = amortization_months,
        term_years = term_years,
        term_months = term_months,
        pct_loan_fee = pct_loan_fee,
        stringsAsFactors = F
      ) %>%
      as_data_frame()

    all_data <-
      1:nrow(variable_matrix) %>%
      map_df(function(x) {
        calculate_leverage_metric(
          purchase_price = variable_matrix$purchase_price[[x]],
          capitalized_acquisition_costs = variable_matrix$capitalized_acquisition_costs[[x]],
          capital_investment = variable_matrix$capital_investment[[x]],
          revenue = variable_matrix$revenue[[x]],
          expenses = variable_matrix$expenses[[x]],
          loan_to_cost = variable_matrix$loan_to_cost[[x]],
          borrow_capital_investment = variable_matrix$borrow_capital_investment[[x]],
          borrow_capitalized_costs = variable_matrix$borrow_capitalized_costs[[x]],
          leverage_threshold = variable_matrix$leverage_threshold[[x]],
          is_interest_only = variable_matrix$is_interest_only[[x]],
          interest_only_periods = variable_matrix$interest_only_periods[[x]],
          is_actual_360 = variable_matrix$is_actual_360[[x]],
          amortization_years = variable_matrix$amortization_years[[x]],
          amortization_months = variable_matrix$amortization_months[[x]],
          term_years = variable_matrix$term_years[[x]],
          term_months = variable_matrix$term_months[[x]],
          pct_loan_fee = variable_matrix$pct_loan_fee[[x]],
          balloon_year = variable_matrix$term_years[[x]],
          balloon_month = variable_matrix$term_months[[x]],
          return_message = return_message
        ) %>%
          mutate(idScenario = x) %>%
          dplyr::select(idScenario, everything())

      })

    if (!return_wide) {
      all_data <-
        all_data %>%
        gather(item, value, -c(idScenario))
    } else {
      all_data <-
        all_data %>%
        mutate_at(
          .vars =
            all_data %>% dplyr::select(matches("^amount[A-Z]|^mean[A-Z]")) %>% names(),
          funs(. %>% formattable::currency(digits = 0))
        ) %>%
        mutate_at(.vars =
                    all_data %>% dplyr::select(matches("^pct[A-Z]")) %>% names(),
                  funs(. %>% formattable::percent(digits = 2))) %>%
        mutate_at(.vars =
                    all_data %>% dplyr::select(matches("^ratio[A-Z]|^rule")) %>% names(),
                  funs(. %>% formattable::comma(digits = 4)))
    }
    return(all_data)
  }