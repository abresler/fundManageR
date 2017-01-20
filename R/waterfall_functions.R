#' Interal rate of return
#'
#' This function returns a data frame that produces
#' the \href{https://en.wikipedia.org/wiki/Internal_rate_of_return}{internal rate of return}
#' for specified dates and cash flows
#'
#' @param cash_flows vector of cash flows
#' @param dates vector of dates, year-month-date format
#' @param date_format date format
#' @param scale_to_100 \code{TRUE} scale to 100
#' @param return_percentage \code{TRUE} return percentages
#' @param return_df \code{TRUE} returns a data frame
#' @param return_message \code{TRUE} return a message after data import
#' @importFrom magrittr %>%
#' @importFrom formattable digits currency percent
#' @importFrom lubridate ymd
#' @importFrom dplyr data_frame
#'
#' @return \code{data_frame}
#' @family calculation
#' @family leveraged finance calculation
#' @family partnership calculation
#' @export
#'
#' @examples
#' calculate_irr_periods(dates = c("2016-06-01","2017-05-31", "2018-05-31", "2019-05-31", "2020-05-31", "2021-05-31",
#' "2022-05-31", "2023-05-31", "2024-05-31", "2025-05-31", "2026-05-31"), cash_flows = c( -3000, 478.515738547242, 478.515738547242, 478.515738547242, 478.515738547242,
#' 478.515738547242, 478.515738547242, 478.515738547242, 478.515738547242, 478.515738547242, 478.515738547278 ), date_format = '%Y-%m-%d', scale_to_100 = FALSE,
#' return_percentage = FALSE, return_df = TRUE, return_wide = TRUE, return_message = TRUE)
calculate_irr_periods <-
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
  scale_to_100 = FALSE,
  return_percentage = FALSE,
  return_df = TRUE,
  return_wide = TRUE,
  return_message = TRUE) {
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
      "Cash Flow produces a " %>%
        paste0(
          irr * 100,
          '% IRR\nFrom ',
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



#' Summary cash flows
#'
#' This function returns a data frame of
#' summarised cash flows for given a set of inputs.
#'
#' @param dates vector of dates
#' @param cash_flows vector of cash flows
#' @param working_capital amount of working capital, minimum cash
#' @param remove_cumulative_cols \code{TRUE} remove summary columns
#' @param include_final_day \code{TRUE} include the final day in calculation
#' @param distribution_frequency frequency of distribution \itemize{
#' \\item \code{NA}: NA
#' \item \code{weekly}: weekly distributions
#' \item \code{monthly}: monthly distributions
#' \item \code{quarterly}: quarterly distributions
#' \item \code{annually}: annual distributions
#' \item \code{sale}: distribution on residual
#' }
#' @import tidyr dplyr stringr formattable purrr
#' @importFrom magrittr %>%
#' @importFrom lubridate ymd
#' @return data_frame
#' @export
#' @family calculation
#' @family leveraged finance calculation
#' @family partnership calculation
#' @examples
#' calculate_cash_flow_dates(dates = c( "2016-09-01", "2017-08-31", "2018-08-31", "2019-08-31", "2020-08-31", "2021-08-31", "2022-08-31", "2023-08-31" ),
#' cash_flows = c( -4151601, 119499.036215643, 257186.036215643, 447646.036215643, 200652.036215643, 510409.036215643, 193.036215643166, 8788626.7640915 ),
#' working_capital = 125000, remove_cumulative_cols = TRUE, include_final_day = TRUE, distribution_frequency = NA)
calculate_cash_flow_dates <-
  function(dates = c(
    "2016-09-01",
    "2017-08-31",
    "2018-08-31",
    "2019-08-31",
    "2020-08-31",
    "2021-08-31",
    "2022-08-31",
    "2023-08-31"
  ),
  cash_flows = c(
    -4151601,
    119499.036215643,
    257186.036215643,
    447646.036215643,
    200652.036215643,
    510409.036215643,
    193.036215643166,
    8788626.76409155
  ),
  working_capital = 125000,
  remove_cumulative_cols = TRUE,
  include_final_day = TRUE,
  distribution_frequency = NA) {
    distribution_frequencies <-
      c(NA,
        'weekly',
        'monthly',
        'quarterly',
        'yearly',
        'sale')

    distribution_frequency <-
      distribution_frequency %>% str_to_lower %>%
      str_replace_all('yearly', 'annually') %>% str_replace_all('residual', 'sale')

    if (!distribution_frequency %>% str_to_lower %in% distribution_frequencies) {
      stop("'\nDistribution frequency can only be\n" %>%
             paste0(paste0(distribution_frequencies, collapse = '\n')))
    }
    dates <-
      dates %>%
      ymd()

    is_annual_budget <-
      as.numeric((dates[2] - dates[1] + 1)) %%
      364 == 1

    if (distribution_frequency %in% c(NA, 'sale')) {
      is_at_sale <-
        T
    } else {
      is_at_sale <-
        F
    }

    ## Distribution

    distribution_dates_df <-
      data_frame(date = dates) %>%
      mutate(idPeriod = 0:(nrow(.) - 1))

    distribution_dates_df <-
      distribution_dates_df %>%
      mutate(
        isDistribution =
          case_when(
            (
              distribution_dates_df$idPeriod > 0 &
                is_annual_budget == T &
                !distribution_frequency == 'sale'
            ) ~ T,
            (
              is_at_sale == T &
                distribution_dates_df$idPeriod == max(distribution_dates_df$idPeriod)
            ) ~ T,
            NA ~ F
          ),
        isDistribution = if_else(isDistribution %>% is.na, F, T) %>% as.numeric,
        isDistribution = if_else(idPeriod == max(idPeriod), T, F) %>% as.numeric,
        isDistribution = if_else(distribution_frequency %>% is.na, T, F, )  %>% as.numeric
      )

    if (working_capital > 0) {
      working_capital <-
        -working_capital
    }

    cf_data <-
      data_frame(date = dates %>% ymd,
                 cashFlow = cash_flows %>% currency(digits = 2)) %>%
      mutate(idPeriod = 0:(nrow(.) - 1)) %>%
      dplyr::select(idPeriod, everything()) %>%
      left_join(distribution_dates_df) %>%
      suppressMessages()

    cf_data <-
      cf_data %>%
      mutate(
        isSalePeriod = if_else(idPeriod == max(idPeriod), T, F),
        workingCapital =
          case_when(
            cf_data$idPeriod == 0 ~ working_capital,
            cf_data$idPeriod == max(cf_data$idPeriod) ~ -working_capital,
            TRUE ~ 0
          ),
        totalCF = workingCapital + cashFlow,
        capitalContribution = if_else(totalCF < 0, -totalCF, 0),
        cashAvailableDistribution = if_else(totalCF > 0, -totalCF, 0),
        capitalDistributionCurrent = cashAvailableDistribution * isDistribution,
        undistributedCash = capitalDistributionCurrent - cashAvailableDistribution,
        cumUndistributedCash = -cumsum(undistributedCash),
        distributionAccruedCash = cumUndistributedCash * isDistribution,
        capitalDistribution = distributionAccruedCash + capitalDistributionCurrent,
        capitalCF = capitalContribution + capitalDistribution
      ) %>%
      dplyr::select(
        -c(
          undistributedCash,
          cumUndistributedCash,
          distributionAccruedCash,
          capitalDistributionCurrent
        )
      ) %>%
      mutate(
        daysAccrued = as.numeric((date - dplyr::lag(date))),
        daysAccrued = ifelse(daysAccrued %>% is.na, 0, daysAccrued),
        cumDays = cumsum(daysAccrued),
        cumContribution = cumsum(capitalContribution),
        cumDistribution = cumsum(capitalDistribution),
        ratioCapitalReturned = (abs(cumDistribution) / cumContribution) %>% as.numeric,
        cumCF = cumsum(cashFlow),
        endCash = cumContribution + cumDistribution + cumCF,
        beginCash = ifelse(idPeriod == 0, 0, dplyr::lag(endCash))
      ) %>%
      mutate_at(
        .cols = c(
          "cashFlow",
          "workingCapital",
          'capitalCF',
          "totalCF",
          "capitalContribution",
          "capitalDistribution",
          "beginCash",
          "endCash",
          "cumContribution",
          "cumDistribution",
          "cumCF"
        ),
        currency
      ) %>%
      dplyr::select_(
        .dots = c(
          "idPeriod",
          "date",
          'daysAccrued',
          'cumDays',
          "cashFlow",
          "isSalePeriod",
          "workingCapital",
          "totalCF",
          "capitalContribution",
          "capitalDistribution",
          'capitalCF',
          'ratioCapitalReturned',
          "beginCash",
          "endCash",
          "cumContribution",
          "cumDistribution",
          "cumCF"
        )
      )

    if (!(cf_data$totalCF %>% sum) + ((cf_data$capitalContribution %>% sum) + (cf_data$capitalDistribution %>% sum)) == 0) {
      stop("Cash does not balance")
    }

    if (remove_cumulative_cols) {
      cf_data <-
        cf_data %>%
        dplyr::select(-matches("cum"))
    }

    return(cf_data)

  }


#' Calculate returns for a given set of cash flows
#'
#' @param dates vector of dates
#' @param cash_flows vector of cash flows
#' @param working_capital amount of working capital, minimum cash
#' @param remove_cumulative_cols remove summary columns
#' @param include_final_day include the final day in calculation
#' @param distribution_frequency when is the cash distributed
#' @param date_format format of the date inputs
#' @param scale_to_100 scale numbers to 100
#' @param return_percentage return percentages
#' @param return_df return data frame
#' @param return_message include a message
#'
#' @return data_frame
#' @export
#' @family calculation
#' @family leveraged finance calculation
#' @family partnership calculation
#' @examples
#' calculate_cash_flows_returns(dates = c( "2016-09-01", "2017-08-31", "2018-08-31", "2019-08-31", "2020-08-31", "2021-08-31", "2022-08-31", "2023-08-31" ),
#' cash_flows = c( -4151601, 119499.036215643, 257186.036215643, 447646.036215643, 200652.036215643, 510409.036215643, 193.036215643166, 8788626.7640915 ),
#' working_capital = 125000, remove_cumulative_cols = T, distribution_frequency = 'annually', date_format = '%Y-%m-%d', scale_to_100 = F, return_percentage = F, return_df = T, return_message = T
#' )
calculate_cash_flows_returns <-
  function(dates = c(
    "2016-09-01",
    "2017-08-31",
    "2018-08-31",
    "2019-08-31",
    "2020-08-31",
    "2021-08-31",
    "2022-08-31",
    "2023-08-31"
  ),
  cash_flows = c(
    -4151601,
    119499.036215643,
    257186.036215643,
    447646.036215643,
    200652.036215643,
    510409.036215643,
    193.036215643166,
    8788626.76409155
  ),
  working_capital = 125000,
  remove_cumulative_cols = T,
  distribution_frequency = 'annually',
  date_format = '%Y-%m-%d',
  scale_to_100 = F,
  return_percentage = F,
  return_df = T,
  return_message = T) {
    cf_data <-
      calculate_cash_flow_dates(
        dates = dates,
        cash_flows = cash_flows,
        working_capital = working_capital,
        distribution_frequency = distribution_frequency,
        remove_cumulative_cols = remove_cumulative_cols
      )

    cf_return_data <-
      calculate_irr_periods(
        dates = cf_data$date,
        cash_flows = -cf_data$capitalCF,
        date_format = date_format,
        return_percentage = return_percentage,
        return_df = return_df,
        return_message = return_message
      )

    return(cf_data)

  }

scale_to_pct <- function(x) {
  if (x > 1) {
    x <-
      x / 100
  }
  return(x)
}


parse_promote_structure <-
  function(promote_structure = "20 over a 12") {
    hit_words <-
      c(" over a ", " over ", "over", "over ", "over a",
        "/", " / ")
    has_no_promote <-
      !promote_structure %>%
      str_detect(hit_words %>% paste0(collapse = "|"))

    if (has_no_promote) {
      stop(
        "No promote structure detected\nPromote structure looks like 20 over a 12, 30 over 5x, 12 / 9"
      )
    }
    has_multiple_hurdle <-
      promote_structure %>% str_detect("x")
    if (has_multiple_hurdle) {
      typeHurdle <-
        'multiple'
    } else {
      typeHurdle <-
        'pref'
    }

    hurdle_promote <-
      promote_structure %>%
      str_split(pattern = hit_words %>% paste0(collapse = "|")) %>%
      map(parse_number) %>%
      flatten_dbl()

    if (typeHurdle == 'multiple') {
      hurdle_promote[[1]] <-
        hurdle_promote[[1]] %>%
        scale_to_pct()
    } else {
      hurdle_promote <-
        hurdle_promote %>%
        map_dbl(scale_to_pct)
    }


    items <-
      c("pctPromote", 'valueHurdle')

    promote_df <-
      data_frame(typeHurdle,
                 item = items,
                 value = hurdle_promote) %>%
      mutate(item = if_else(
        typeHurdle == "pref",
        item %>% str_replace(pattern = "valueHurdle", 'pctPref'),
        item %>% str_replace(pattern = "valueHurdle", 'ratioCapitalMultiple')
      )) %>%
      spread(item, value)

    return(promote_df)

  }


#' Tidy waterfall structure
#'
#' This function parses a character vector
#' describing a partnership waterfall into a
#' date frame.  The function looks to recogonize
#' whether the promote is based upon an internal rate of return
#' or capital multiple hurdle.
#'
#' @param promote_structures character vector of promote structures
#' @param return_wide return data in wide form
#'
#' @return a data frame
#' @export
#' @importFrom formattable percent
#' @family calculation
#' @family partnership calculation
#' @examples
#' tidy_promote_structure(promote_structures = c("20 over a 12", '30 / 18', "40 over a 10x"), return_wide = T)
tidy_promote_structure <-
  function(promote_structures = c("20 over a 12", '30 / 18', "40 over a 10x"),
           return_wide = F) {
    parse_promote_structure_safe <-
      failwith(NULL, parse_promote_structure)

    promote_data <-
      1:length(promote_structures) %>%
      map_df(function(x) {
        parse_promote_structure_safe(promote_structure = promote_structures[x]) %>%
          mutate(tierWaterfall = x) %>%
          dplyr::select(tierWaterfall, everything())
      })

    if (return_wide) {
      promote_data <-
        promote_data %>%
        gather(item, hurdle, -c(tierWaterfall)) %>%
        replace_na(list(hurdle = 0)) %>%
        arrange(tierWaterfall) %>%
        unite(item, item, tierWaterfall, sep = '') %>%
        mutate(item = item %>% factor(ordered = T, levels = item)) %>%
        spread(item, hurdle)

      promote_data <-
        promote_data %>%
        mutate_at(.cols =
                    promote_data %>% dplyr::select(matches("^pct[A-Z]|ratio[A-Z]")) %>% names,
                  funs(. %>% as.numeric)) %>%
        mutate_at(.cols =
                    promote_data %>% dplyr::select(matches("^pct[A-Z]")) %>% names,
                  funs(. %>% percent))

    } else {
      promote_data <-
        promote_data %>%
        mutate(nameTier = promote_structures) %>%
        dplyr::select(tierWaterfall, nameTier, everything())

      promote_data <-
        promote_data %>%
        mutate_at(.cols =
                    promote_data %>% dplyr::select(matches("^pct[A-Z]")) %>% names,
                  funs(. %>% percent)) %>%
        dplyr::select(
          tierWaterfall,
          nameTier,
          typeHurdle,
          matches("pctPref|ratioCapitalMultiple"),
          pctPromote
        )
    }

    return(promote_data)
  }

#' Accrued preference
#'
#' This function calcuates accrued
#' preferences for a specified inputs
#'
#' @param pct_pref rate of accrued preference
#' @param is_actual_360 \code{TRUE} calculate rate actual/360 terms
#' @param days count of days
#' @param equity_bb vector of begining equity balance
#' @param pref_accrued_bb vector of accrued preference begining balance
#'
#' @return a \code{data_Fram}
#' @export
#' @import dplyr
#' @importFrom formattable currency
#' @family calculation
#' @family partnership calculation
#' @examples
#' calculate_days_accrued_pref(pct_pref = .1, is_actual_360 = T, days = 31, equity_bb = 1700000.00, pref_accrued_bb = 0)
calculate_days_accrued_pref <-
  function(pct_pref = .1,
           is_actual_360 = TRUE,
           days = 31,
           equity_bb = 1700000.00,
           pref_accrued_bb = 0) {
    if (is_actual_360) {
      accrual_days <-
        360
    } else {
      accrual_days <-
        365
    }

    calc_basis <-
      (equity_bb  + pref_accrued_bb) %>% currency(digits = 2)

    accrued_pref <-
      ((pct_pref / accrual_days) * days * calc_basis)
    return(accrued_pref)
  }

get_pct_to_promote <-
  function(promote_df, tier_waterfall = 2) {
    if (tier_waterfall %in% promote_df$tierWaterfall) {
      to_promote <-
        promote_df %>%
        dplyr::filter(tierWaterfall == tier_waterfall) %>%
        .$pctPromote
    } else {
      to_promote <-
        0
    }
    return(to_promote)
  }


get_waterfall_tier_df <-
  function(tiers = 1:5,
           return_wide = F) {
    waterfall_df <-
      data_frame(
        tierWaterfall = 1,
        bbAccruedPref = 0,
        accruedPref = 0,
        distributionPriorPref = 0,
        toAccruedPref = 0,
        ebAccruedPref = 0,
        bbCapitalMultiple = 0,
        capitalMultipleDraw = 0,
        toEquity = 0,
        distributionPriorMultiple = 0,
        toCapitalMultiple = 0,
        ebCapitalMultiple = 0,
        toPromote = 0,
        toCapital = 0
      )

    if (tiers %>% length > 1) {
      other_tiers <-
        tiers[tiers > 1] %>%
        map_df(function(x) {
          data_frame(
            tierWaterfall = rep(x),
            bbAccruedPref = 0,
            accruedPref = 0,
            distributionPriorPref = 0,
            toAccruedPref = 0,
            ebAccruedPref = 0,
            bbCapitalMultiple = 0,
            capitalMultipleDraw = 0,
            toEquity = 0,
            distributionPriorMultiple = 0,
            toCapitalMultiple = 0,
            ebCapitalMultiple = 0,
            toPromote = 0,
            toCapital = 0
          )
        })

      waterfall_df <-
        waterfall_df %>%
        bind_rows(other_tiers)
    }
    waterfall_df <-
      waterfall_df %>%
      mutate_at(
        .cols = c(
          "bbAccruedPref",
          "accruedPref",
          "distributionPriorPref",
          "toAccruedPref",
          "ebAccruedPref",
          'bbCapitalMultiple',
          'capitalMultipleDraw',
          'toEquity',
          'toCapitalMultiple',
          'ebCapitalMultiple',
          'toPromote',
          'toCapital'
        ),
        currency
      )

    if (return_wide) {
      waterfall_df <-
        waterfall_df %>%
        gather(item, amount, -tierWaterfall, convert = F) %>%
        arrange(tierWaterfall, item) %>%
        unite(item, item, tierWaterfall, sep = '') %>%
        suppressWarnings()

      col_order <-
        waterfall_df$item

      waterfall_df <-
        waterfall_df %>%
        spread(item, amount) %>%
        dplyr::select(one_of(col_order))
    }
    return(waterfall_df)
  }


get_initial_equity_df <-
  function(equity_bb = 0,
           to_equity = 0,
           period = 0,
           equity_draw = 0) {
    equityBB <-
      equity_bb

    toEquity <-
      to_equity

    equityDraw <-
      equity_draw

    equityEB <-
      equityBB + equityDraw + toEquity

    equity_df <-
      data_frame(idPeriod = period,
                 equityBB,
                 equityDraw,
                 toEquity,
                 equityEB) %>%
      mutate_at(
        .cols = c('equityBB', 'equityDraw',
                  'toEquity', 'equityEB'),
        .funs = currency
      )
    return(equity_df)
  }


#'  Cash-flow waterfall
#'
#'  This function performs waterfall calculations
#'  on a set of leveraged or unleveraged cash flows
#'  based upon the user's inputs
#'
#'
#' @param dates vector of dates in year-month-day format
#' @param cash_flows vector of cash flows
#' @param working_capital amount of working capital
#' @param promote_structure character vector of promote structures
#' @param distribution_frequency frequency of distributions
#' @param is_actual_360 \code{TRUE} is the rate of return actual 360
#' @param widen_promote_structure \code{TRUE} widen the promote structure
#' @param bind_to_cf \code{TRUE}  bind results to data frame
#' @param remove_zero_cols \code{TRUE} remove zero-value columns
#' @param widen_waterfall \code{TRUE} returns waterfall in wide form
#' @import tidyr formattable dplyr stringr
#' @return data_frame
#' @export
#' @family calculation
#' @family leveraged finance calculation
#' @family partnership calculation
#' @examples
#' calculate_cash_flow_waterfall(dates = c("2015-03-11", "2015-11-20", "2016-10-15"), cash_flows = c(-100000, -200000, 698906.76849),
#' working_capital = 0, promote_structure = c("20 / 12", "30 / 18"),
#'  distribution_frequency = NA, is_actual_360 = TRUE,
#'  widen_promote_structure = FALSE, bind_to_cf = FALSE,
#' remove_zero_cols = TRUE, widen_waterfall = FALSE)
calculate_cash_flow_waterfall <-
  function(dates =
             c("2015-03-11", "2015-11-20", "2016-10-15"),
           cash_flows = c(-100000, -200000, 698906.76849),
           working_capital = 0,
           promote_structure = c("20 / 12", "30 / 18"),
           distribution_frequency = NA,
           is_actual_360 = TRUE,
           widen_promote_structure = FALSE,
           bind_to_cf = FALSE,
           remove_zero_cols = TRUE,
           widen_waterfall = FALSE) {
    options(scipen = 999999)
    cf_data <-
      calculate_cash_flow_dates(
        dates = dates,
        cash_flows = cash_flows,
        remove_cumulative_cols = T,
        working_capital = working_capital,
        distribution_frequency = distribution_frequency
      )

    waterfall_data <-
      cf_data %>%
      dplyr::select(idPeriod:daysAccrued,
                    capitalContribution,
                    capitalDistribution,
                    capitalCF) %>%
      mutate(cashDistributionAvailable = -pmin(0, capitalCF) %>% currency)

    promote_df <-
      tidy_promote_structure(promote_structures = promote_structure,
                             return_wide = widen_promote_structure)

    waterfall_periods <-
      waterfall_data$idPeriod

    tiers <-
      promote_df$tierWaterfall

    waterfall_df <-
      data_frame()

    for (x in 1:length(waterfall_periods)) {
      period <-
        waterfall_periods[x]

      period_data <-
        waterfall_data %>%
        dplyr::filter(idPeriod == period)

      days <-
        period_data$daysAccrued

      equityDraw <-
        period_data$capitalContribution

      periodCAD <-
        period_data$cashDistributionAvailable %>% as.numeric %>% digits(2) %>% currency

      if (period == 0) {
        equity_df <-
          get_initial_equity_df(
            equity_bb = 0,
            to_equity = 0,
            equity_draw = equityDraw,
            period = 0
          )

        waterfall_df <-
          get_waterfall_tier_df(tiers = tiers) %>%
          mutate(idPeriod = period) %>%
          dplyr::select(idPeriod, everything())
      }

      if (period > 0)  {
        equityBB <-
          equity_df %>% dplyr::filter(idPeriod == period - 1) %>% .$equityEB

        for (tier in tiers) {
          typeHurdle <-
            promote_df %>% dplyr::filter(tierWaterfall == tier) %>% .$typeHurdle

          bbAccruedPref <-
            max(
              0,
              waterfall_df %>% dplyr::filter(idPeriod == period - 1 &
                                               tierWaterfall == tier) %>% .$ebAccruedPref
            )

          bbCapitalMultiple <-
            max(
              0,
              waterfall_df %>% dplyr::filter(idPeriod == period - 1, tierWaterfall == tier) %>% .$ebCapitalMultiple
            )

          tierPromote <-
            promote_df %>% dplyr::filter(tierWaterfall == tier) %>% .$pctPromote

          if (tier == max(tiers)) {
            is_max_tier <-
              T
          } else {
            is_max_tier <-
              F
          }

          if (typeHurdle == 'pref') {
            tierPref <-
              promote_df %>% dplyr::filter(tierWaterfall == tier) %>% .$pctPref

            accruedPref <-
              calculate_days_accrued_pref(
                pct_pref = tierPref,
                is_actual_360 = is_actual_360,
                days = days,
                equity_bb = equityBB,
                pref_accrued_bb = bbAccruedPref
              )

            capitalMultipleDraw <-
              0

            distributionPriorMultiple <-
              0

            toCapitalMultiple <-
              0

            ebCapitalMultiple <-
              0

            if (tier == 1)  {
              toAccruedPref <-
                -min(periodCAD, (accruedPref + bbAccruedPref)) %>% as.numeric %>% digits(2)

              ebAccruedPref <-
                (bbAccruedPref + accruedPref + toAccruedPref)  %>% as.numeric %>% digits(2)

              cash_to_equity <-
                max(0, (periodCAD + toAccruedPref))  %>% as.numeric %>% digits(2)

              toEquity <-
                -min(cash_to_equity, (equityBB + equityDraw)) %>% as.numeric %>% digits(2)

              equityEB <-
                (equityBB + equityDraw + toEquity) %>% as.numeric %>% digits(2)

              to_promote_tier <-
                max(0, (cash_to_equity + toEquity))  %>% as.numeric %>% digits(2)

              distributionPriorPref <-
                0

              if (is_max_tier) {
                toPromote <-
                  -to_promote_tier * tierPromote

                toCapital <-
                  -to_promote_tier * (1 - tierPromote)

              }
              if (is_max_tier == F) {
                typeNextTier <-
                  promote_df %>%
                  dplyr::filter(tierWaterfall == tier + 1) %>% .$typeHurdle

                if (typeNextTier == 'pref') {
                  nextPref <-
                    promote_df %>%
                    dplyr::filter(tierWaterfall == tier + 1) %>% .$pctPref

                  bbAccruedPrefNext <-
                    waterfall_df %>%
                    dplyr::filter(tierWaterfall == tier + 1) %>% .$bbAccruedPref

                  prefAccruedNext <-
                    calculate_days_accrued_pref(
                      pct_pref = nextPref,
                      is_actual_360 = is_actual_360,
                      days = days,
                      equity_bb = equityBB,
                      pref_accrued_bb = bbAccruedPrefNext
                    )

                  cash_for_promote <-
                    -min(to_promote_tier, max(
                      0,
                      (
                        bbAccruedPref + prefAccruedNext + toAccruedPref
                      ) / (1 - tierPromote)
                    ))
                }
                if (typeNextTier == 'multiple') {
                  nextMultiple <-
                    promote_df %>%
                    dplyr::filter(tierWaterfall == tier + 1) %>% .$ratioCapitalMultiple

                  bbCapitalMultipleNext <-
                    waterfall_df %>% dplyr::filter(idPeriod == period - 1, tierWaterfall == tier + 1) %>% .$ebCapitalMultiple

                  capitalMultipleDrawNext <-
                    (equity_df %>% dplyr::filter(idPeriod == period - 1) %>% .$equityDraw) * nextMultiple


                  cash_for_promote <-
                    -min(to_promote_tier, max(
                      0,
                      (
                        bbCapitalMultipleNext + capitalMultipleDrawNext + toAccruedPref
                      ) / (1 - tierPromote)
                    ))


                }

                toCapital <-
                  cash_for_promote * (1 - tierPromote)

                toPromote <-
                  cash_for_promote * (tierPromote)
              }
            }

            if (tier > 1) {
              priorLevelDistribution <-
                waterfall_df %>%
                dplyr::filter(idPeriod == period &
                                tierWaterfall < tier) %>% dplyr::select(toAccruedPref,
                                                                        toPromote,
                                                                        toCapital,
                                                                        toCapitalMultiple) %>% gather(item, value) %>%
                .$value %>% sum

              equityDistribution <-
                equity_df %>% dplyr::filter(idPeriod == period) %>% dplyr::select(toEquity) %>% gather(item, value) %>% .$value %>% sum

              remainingCash <-
                periodCAD + priorLevelDistribution + equityDistribution

              distributionPriorMultiple <-
                waterfall_df %>% dplyr::filter(idPeriod == period &
                                                 tierWaterfall == (tier - 1)) %>% dplyr::select(distributionPriorMultiple) %>% gather(item, value) %>% .$value %>% sum

              distributionPriorPref <-
                waterfall_df %>% dplyr::filter(idPeriod == period &
                                                 tierWaterfall == (tier - 1)) %>% dplyr::select(distributionPriorPref, toAccruedPref) %>% gather(item, value) %>% .$value %>% sum

              toAccruedPref <-
                -max(0, min(
                  remainingCash,
                  (
                    bbAccruedPref + accruedPref + distributionPriorPref + distributionPriorMultiple
                  )
                ))

              to_promote_tier <-
                remainingCash + toAccruedPref

              ebAccruedPref <-
                bbAccruedPref + accruedPref + distributionPriorPref + distributionPriorMultiple + toAccruedPref

              if (is_max_tier) {
                toPromote <-
                  -to_promote_tier * tierPromote
                toCapital <-
                  -to_promote_tier * (1 - tierPromote)
              }
              if (is_max_tier == F) {
                typeNextTier <-
                  promote_df %>%
                  dplyr::filter(tierWaterfall == tier + 1) %>% .$typeHurdle

                if (typeNextTier == 'pref') {
                  nextPref <-
                    promote_df %>%
                    dplyr::filter(tierWaterfall == tier + 1) %>% .$pctPref

                  bbAccruedPrefNext <-
                    waterfall_df %>%
                    dplyr::filter(tierWaterfall == tier + 1) %>% .$bbAccruedPref

                  prefAccruedNext <-
                    calculate_days_accrued_pref(
                      pct_pref = nextPref,
                      is_actual_360 = is_actual_360,
                      days = days,
                      equity_bb = equityBB,
                      pref_accrued_bb = bbAccruedPrefNext
                    )

                  cash_for_promote <-
                    -min(to_promote_tier, max(
                      0,
                      (
                        bbAccruedPref + prefAccruedNext + toAccruedPref
                      ) / (1 - tierPromote)
                    ))
                }
                if (typeNextTier == 'multiple') {
                  nextMultiple <-
                    promote_df %>%
                    dplyr::filter(tierWaterfall == tier + 1) %>% .$ratioCapitalMultiple

                  bbCapitalMultipleNext <-
                    waterfall_df %>% dplyr::filter(idPeriod == period - 1, tierWaterfall == tier + 1) %>% .$ebCapitalMultiple

                  capitalMultipleDrawNext <-
                    (equity_df %>% dplyr::filter(idPeriod == period - 1) %>% .$equityDraw) * nextMultiple

                  cash_for_promote <-
                    -min(to_promote_tier, max(
                      0,
                      (
                        bbCapitalMultipleNext + capitalMultipleDrawNext + toAccruedPref + distributionPriorPref + toEquity
                      ) / (1 - tierPromote)
                    ))


                }

                toCapital <-
                  cash_for_promote * (1 - tierPromote)

                toPromote <-
                  cash_for_promote * (tierPromote)
              }
            }

          }

          if (typeHurdle == 'multiple') {
            ratioCapitalMultiple <-
              promote_df %>% dplyr::filter(tierWaterfall == tier) %>% .$ratioCapitalMultiple
            if (tier == 1)  {
              accruedPref <-
                0
              toAccruedPref <-
                0

              ebAccruedPref <-
                0

              cash_to_equity <-
                max(0, (periodCAD + toAccruedPref))  %>% as.numeric %>% digits(2) %>% currency

              toEquity <-
                -min(cash_to_equity, (equityBB + equityDraw)) %>% as.numeric %>% digits(2) %>% currency

              equityEB <-
                (equityBB + equityDraw + toEquity) %>% as.numeric %>% digits(2) %>% currency

              cash_to_multiple <-
                max(0, (cash_to_equity + toEquity))  %>% as.numeric %>% digits(2) %>% currency

              capitalMultipleDraw <-
                (equity_df %>% dplyr::filter(idPeriod == period - 1) %>% .$equityDraw) * ratioCapitalMultiple

              toCapitalMultiple <-
                -min(cash_to_multiple,
                     (bbCapitalMultiple + capitalMultipleDraw + toEquity)) %>% as.numeric %>% digits(2) %>%
                currency

              ebCapitalMultiple <-
                bbCapitalMultiple + capitalMultipleDraw + toEquity + toCapitalMultiple

              to_promote_tier <-
                max(0, (cash_to_multiple + toCapitalMultiple))  %>% as.numeric %>% digits(2) %>%
                currency

              distributionPriorPref <-
                0

              distributionPriorMultiple <-
                0

              if (is_max_tier) {
                toPromote <-
                  -to_promote_tier * tierPromote

                toCapital <-
                  -to_promote_tier * (1 - tierPromote)

              }

              if (is_max_tier == F) {
                nextMultiple <-
                  promote_df %>%
                  dplyr::filter(tierWaterfall == tier + 1) %>% .$ratioCapitalMultiple

                bbCapitalMultipleNext <-
                  waterfall_df %>% dplyr::filter(idPeriod == period - 1, tierWaterfall == tier + 1) %>% .$ebCapitalMultiple

                capitalMultipleDrawNext <-
                  (equity_df %>% dplyr::filter(idPeriod == period - 1) %>% .$equityDraw) * nextMultiple

                cash_for_promote <-
                  -min(to_promote_tier, max(
                    0,
                    (
                      bbCapitalMultipleNext + capitalMultipleDrawNext + toCapitalMultiple + toEquity
                    ) / (1 - tierPromote)
                  ))

                toCapital <-
                  cash_for_promote * (1 - tierPromote)

                toPromote <-
                  cash_for_promote * (tierPromote)
              }
            }

            if (tier > 1) {
              priorLevelDistribution <-
                waterfall_df %>%
                dplyr::filter(idPeriod == period &
                                tierWaterfall < tier) %>%
                dplyr::select(tierWaterfall,
                              toAccruedPref,
                              toPromote,
                              toCapital,
                              toCapitalMultiple) %>%
                gather(item, value, -tierWaterfall) %>% .$value %>% sum %>%
                currency

              equityDistribution <-
                equity_df %>% dplyr::filter(idPeriod == period) %>% dplyr::select(toEquity) %>% gather(item, value) %>% .$value %>% sum %>% currency

              remainingCash <-
                periodCAD + priorLevelDistribution + equityDistribution

              accruedPref <-
                0

              toAccruedPref <-
                0

              ebAccruedPref <-
                bbAccruedPref + accruedPref + toAccruedPref

              capitalMultipleDraw <-
                (equity_df %>% dplyr::filter(idPeriod == period - 1) %>% .$equityDraw) * ratioCapitalMultiple %>%
                currency()

              distributionPriorMultiple <-
                waterfall_df %>%
                dplyr::filter(idPeriod == period &
                                tierWaterfall <= (tier - 1)) %>%
                dplyr::select(toCapitalMultiple, toCapital) %>% gather(item, value) %>%
                .$value %>%
                sum %>%
                currency

              distributionPriorPref <-
                waterfall_df %>% dplyr::filter(idPeriod == period &
                                                 tierWaterfall <= (tier - 1)) %>% dplyr::select(toAccruedPref) %>% gather(item, value) %>% .$value %>% sum %>%
                currency

              priorEquity <-
                waterfall_df %>% dplyr::filter(idPeriod == period &
                                                 tierWaterfall == (tier - 1)) %>% dplyr::select(toEquity) %>% gather(item, value) %>% .$value %>% sum %>%
                currency()

              toCapitalMultiple <-
                -max(0,
                     min(
                       remainingCash,
                       (
                         bbCapitalMultiple + capitalMultipleDraw + distributionPriorMultiple + distributionPriorPref + priorEquity
                       )
                     )) %>%
                currency()

              to_promote_tier <-
                remainingCash + toCapitalMultiple


              ebCapitalMultiple <-
                bbCapitalMultiple + capitalMultipleDraw + distributionPriorMultiple + distributionPriorPref + priorEquity + toCapitalMultiple

              if (is_max_tier) {
                toPromote <-
                  -to_promote_tier * tierPromote

                toCapital <-
                  -to_promote_tier * (1 - tierPromote)
              }
              if (is_max_tier == F) {
                nextMultiple <-
                  promote_df %>%
                  dplyr::filter(tierWaterfall == tier + 1) %>% .$ratioCapitalMultiple

                bbCapitalMultipleNext <-
                  waterfall_df %>% dplyr::filter(idPeriod == period - 1, tierWaterfall == tier + 1) %>% .$ebCapitalMultiple %>%
                  currency()

                capitalMultipleDrawNext <-
                  (equity_df %>% dplyr::filter(idPeriod == period - 1) %>% .$equityDraw) * nextMultiple %>%
                  currency

                cash_for_promote <-
                  -min(to_promote_tier, max(
                    0,
                    (
                      bbCapitalMultipleNext + capitalMultipleDrawNext + toCapitalMultiple + distributionPriorMultiple + distributionPriorPref + equityDistribution
                    ) / (1 - tierPromote)
                  ))

                toCapital <-
                  cash_for_promote * (1 - tierPromote)

                toPromote <-
                  cash_for_promote * (tierPromote)
              }

            }
          }

          period_waterfall <-
            data_frame(
              idPeriod = period,
              tierWaterfall = tier,
              bbAccruedPref,
              accruedPref,
              distributionPriorPref = distributionPriorPref,
              toAccruedPref,
              ebAccruedPref,
              bbCapitalMultiple,
              capitalMultipleDraw,
              distributionPriorMultiple,
              toEquity,
              toCapitalMultiple,
              ebCapitalMultiple,
              toPromote,
              toCapital
            )

          waterfall_df <-
            waterfall_df %>%
            bind_rows(period_waterfall) %>%
            distinct()

          period_equity <-
            data_frame(idPeriod = period,
                       equityBB,
                       equityDraw,
                       toEquity,
                       equityEB)

          equity_df <-
            equity_df %>%
            bind_rows(period_equity) %>%
            distinct()
        }

      }

    }

    if ('toEquity' %in% names(waterfall_df)) {
      waterfall_df <-
        waterfall_df %>% dplyr::select(-toEquity)
    }
    if (remove_zero_cols) {
      waterfall_df <-
        waterfall_df[, colSums(abs(waterfall_df) != 0) > 0]
    }

    waterfall_df <-
      waterfall_df %>%
      mutate_at(waterfall_df %>% dplyr::select(-c(idPeriod, tierWaterfall)) %>% names,
                .funs = currency)

    equity_df <-
      equity_df %>%
      mutate_at(equity_df %>% dplyr::select(-c(idPeriod)) %>% names,
                .funs = currency)

    equityDistributions <-
      equity_df$toEquity %>% sum

    levelDistributions <-
      waterfall_df %>% dplyr::select(matches("to")) %>% gather(item, value) %>% .$value %>% sum %>% currency %>% suppressWarnings()

    cash_check <-
      ((cf_data$capitalDistribution %>% sum %>% abs) + (equityDistributions  + levelDistributions)
      ) %>% as.integer()

    if (!cash_check == 0) {
      stop("Waterfall does not tie to distributable by\n" %>%
             paste0(cash_check %>% currency))
    }

    if (widen_waterfall)  {
      col_order <-
        waterfall_df %>%
        gather(item, value, -c(idPeriod, tierWaterfall)) %>%
        arrange(idPeriod, tierWaterfall) %>%
        unite(item, item, tierWaterfall, sep = '') %>%
        .$item %>%
        suppressWarnings()

      waterfall_df <-
        waterfall_df %>%
        gather(item, value, -c(idPeriod, tierWaterfall)) %>%
        arrange(idPeriod, tierWaterfall) %>%
        unite(item, item, tierWaterfall, sep = '') %>%
        spread(item, value) %>%
        mutate_at(col_order, currency) %>%
        dplyr::select(one_of(c('idPeriod', col_order))) %>%
        suppressWarnings()

      waterfall_df <-
        equity_df %>%
        left_join(waterfall_df) %>%
        suppressMessages()
      if (bind_to_cf) {
        waterfall_df <-
          cf_data %>%
          left_join(waterfall_df) %>%
          suppressMessages()
      }

    }
    if (widen_waterfall == F) {
      waterfall_df <-
        waterfall_df %>%
        bind_rows(equity_df %>% mutate(tierWaterfall = 0)) %>%
        arrange(idPeriod, tierWaterfall) %>%
        mutate(tierWaterfall = tierWaterfall + 1)

      numeric_cols <-
        waterfall_df %>% dplyr::select(-c(idPeriod, tierWaterfall)) %>% names

      waterfall_df <-
        waterfall_df %>%
        mutate_at(
          .cols = numeric_cols,
          .funs = function(x)
            if_else(x %>% is.na, 0, x)
        ) %>%
        mutate_at(.cols = numeric_cols,
                  .funs = currency)
    }

    waterfall_df <-
      waterfall_df %>%
      left_join(waterfall_data %>% dplyr::select(idPeriod, date)) %>%
      dplyr::select(idPeriod, date, everything()) %>%
      suppressMessages()

    return(waterfall_df)
  }

#' Partnership waterfall and cash flows
#'
#' This function is a variant of the \code{\link{calculate_cash_flow_waterfall}}
#' function that also includes the calculation of the share of cash flow available
#' to each member based upon the specified percentage equity splits and promote allocation
#'
#' @param dates vector of dates in year-month-day format
#' @param cash_flows vector of cash flows
#' @param working_capital amount of working capital
#' @param promote_structure vector of the promote structure
#' @param general_partner_pct ercentage of capital provided by general partner
#' @param gp_promote_share share of promote to general partner
#' @param unnest_data unnest final results
#' @param exclude_partnership_total exclude total columns
#' @param distribution_frequency frequency of distribution
#' @param is_actual_360 \code{TRUE} is the rate of return actual 360
#' @param widen_promote_structure \code{TRUE} widen the promote structure
#' @param bind_to_cf \code{TRUE}  bind results to data frame
#' @param remove_zero_cols \code{TRUE} remove zero-value columns
#' @param widen_waterfall \code{TRUE} returns waterfall in wide form
#' @family calculation
#' @family leveraged finance calculation
#' @family partnership calculation
#' @return \code{data_frame}O
#' @export
#'
#' @examples
#' calculate_cash_flow_waterfall_partnership(dates =c("2016-09-01", "2017-08-31"), cash_flows = c(-1500000, 105000000),
#' working_capital = 200000, promote_structure = c("20 over 12", "30 over 20", "50 over 3.5x", "100 over 10x"),
#' general_partner_pct = .05, gp_promote_share = 1, unnest_data = F,
#' exclude_partnership_total = F,
#' distribution_frequency = 'annually', is_actual_360 = TRUE,
#' widen_promote_structure = FALSE, bind_to_cf = FALSE, remove_zero_cols = TRU,
#' widen_waterfall = FALSE)
calculate_cash_flow_waterfall_partnership <-
  function(dates =
             c("2016-09-01",
               "2017-08-31"),
           cash_flows = c(-1500000,
                          105000000),
           working_capital = 200000,
           promote_structure = c("20 over 12", "30 over 20", "50 over 3.5x", "100 over 10x"),
           assign_to_environment = TRUE,
           general_partner_pct = .05,
           gp_promote_share = 1,
           unnest_data = FALSE,
           exclude_partnership_total = FALSE,
           distribution_frequency = NA,
           is_actual_360 = TRUE,
           widen_promote_structure = FALSE,
           bind_to_cf = FALSE,
           remove_zero_cols = TRUE,
           widen_waterfall = FALSE) {
    options(scipen = 9999999)
    pct_gp <-
      general_partner_pct %>% scale_to_pct()

    pct_lp <-
      1 - pct_gp

    share_gp_promote <-
      gp_promote_share

    share_lp_promote_share <-
      1 - gp_promote_share

    promote_name_df <-
      data_frame(tierWaterfall = 1,
                 nameTier = "Return of Equity") %>%
      bind_rows(
        tidy_promote_structure(promote_structures = promote_structure,
                               return_wide = F) %>%
          mutate(tierWaterfall = tierWaterfall + 1) %>%
          dplyr::select(tierWaterfall, nameTier)
      )

    waterfall_data <-
      calculate_cash_flow_waterfall(
        dates = dates,
        cash_flows = cash_flows,
        working_capital = working_capital,
        is_actual_360 = is_actual_360,
        promote_structure = promote_structure,
        bind_to_cf = F,
        widen_promote_structure = F
      ) %>%
      dplyr::select(idPeriod:tierWaterfall,
                    equityDraw, toEquity, everything())

    entity_waterfall <-
      waterfall_data %>%
      dplyr::select(idPeriod,
                    date,
                    tierWaterfall,
                    matches("equityDraw"),
                    matches("to")) %>%
      gather(item, toCF, -c(idPeriod, date, tierWaterfall)) %>%
      dplyr::filter(!toCF == 0) %>%
      mutate(
        toGP = -ifelse(item == 'toPromote', toCF * share_gp_promote, toCF * pct_gp),
        toLP = -ifelse(
          item == 'toPromote',
          toCF * share_lp_promote_share,
          toCF * pct_lp
        )
      ) %>%
      mutate_at(.cols = c('toCF', 'toGP', 'toLP'),
                .funs = currency) %>%
      arrange(idPeriod, tierWaterfall) %>%
      left_join(promote_name_df) %>%
      dplyr::select(idPeriod:tierWaterfall, nameTier, everything()) %>%
      suppressMessages() %>%
      suppressWarnings()

    cash_check <-
      ((entity_waterfall$toCF %>% sum()) + (entity_waterfall$toGP %>% sum())  + (entity_waterfall$toLP %>% sum())
      ) %>% as.integer()

    if (!cash_check == 0) {
      stop("Cash does not tie")
    }

    entity_cf <-
      entity_waterfall %>%
      dplyr::select(date, toCF) %>%
      group_by(date) %>%
      mutate(toCF = -toCF) %>%
      summarise(totalCF = sum(toCF))
    gp_cf <-
      entity_waterfall %>%
      dplyr::select(date, toGP) %>%
      group_by(date) %>%
      summarise(totalCF = sum(toGP))

    lp_cf <-
      entity_waterfall %>%
      dplyr::select(date, toLP) %>%
      group_by(date) %>%
      summarise(totalCF = sum(toLP))

    total_return_df <-
      calculate_irr_periods(
        dates = entity_cf$date,
        cash_flows = entity_cf$totalCF,
        return_percentage = T,
        return_df = T
      ) %>%
      mutate(typeEntity = 'Partnership') %>%
      dplyr::select(typeEntity, everything())

    gp_return_df <-
      calculate_irr_periods(
        dates = gp_cf$date,
        cash_flows = gp_cf$totalCF,
        return_percentage = T,
        return_df = T
      ) %>%
      mutate(typeEntity = 'General Partner') %>%
      dplyr::select(typeEntity, everything())

    lp_return_df <-
      calculate_irr_periods(
        dates = lp_cf$date,
        cash_flows = lp_cf$totalCF,
        return_percentage = T,
        return_df = T
      ) %>%
      mutate(typeEntity = 'Limited Partner') %>%
      dplyr::select(typeEntity, everything())

    partnership_return_summary <-
      gp_return_df %>%
      bind_rows(list(lp_return_df, total_return_df)) %>%
      mutate_at(.cols = c('pctIRR'),
                .funs = percent) %>%
      mutate_at(
        .cols = c(
          "equityContributions",
          "equityDistributions",
          "valueProfit"
        ),
        .funs = currency
      )

    if (exclude_partnership_total) {
      partnership_return_summary <-
        partnership_return_summary %>%
        dplyr::filter(!typeEntity %in% 'Partnership')
    }

    data <-
      data_frame(
        nameTable =
          c(
            'Cash Flow Waterfall',
            'Entity Waterfall',
            'Partnership Return Summary'
          ),
        dataTable =
          list(
            waterfall_data,
            entity_waterfall,
            partnership_return_summary
          )
      )

    if (assign_to_environment) {
      data <-
        data %>%
        left_join(data_frame(
          nameTable = c(
            "Cash Flow Waterfall",
            "Entity Waterfall",
            "Partnership Return Summary"
          ),
          idDF = c(
            'cashflowWaterfall',
            'entityWaterfall',
            'partnershipReturns'
          )
        )) %>%
        suppressMessages()

      1:nrow(data) %>%
        map(function(x) {
          table_data <-
            data$dataTable[[x]]
          df_name <-
            data$idDF[[x]]

          assign(x = df_name, eval(table_data), envir = .GlobalEnv)
        })
      data <-
        data %>%
        dplyr::select(-idDF)
    }

    if (unnest_data) {
      data <-
        data %>%
        unnest()
    }

    return(data)
  }
