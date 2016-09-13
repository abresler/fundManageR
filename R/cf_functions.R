calculate_data_periods_irr <-
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

get_10yrUST_conversion_factor_df <-
  function(bond_coupon = 3.3375,
           date_maturity = '2019-11-15',
           date_delivery = '2009-12-01',
           return_message = T) {
    options(scipen = 999)
    options(digits = 5)

    bond_coupon <-
      bond_coupon / 100

    date_maturity <-
      date_maturity %>% ymd

    date_delivery <-
      date_delivery %>% ymd

    days <-
      as.numeric((date_maturity - date_delivery))

    year <-
      floor(days / 365.25)

    month <-
      floor((days / 365.25 - year) * 365 / 30)



    n <-
      year

    if (month %in% c(0, 1, 2)) {
      z <- 0
    }
    if (month %in% c(3, 4, 5)) {
      z <-
        3
    }
    if (month %in% c(6, 7, 8)) {
      z <-
        6
    }
    if (month %in% c(9, 10, 11, 12)) {
      z <-
        9
    }


    if (z < 7) {
      v <-
        z
    } else {
      v <-
        3
    }

    a <-
      (1 / 1.03) ^ (v / 6)
    b <-
      (bond_coupon / 2) * ((6 - v) / 6)
    if (z < 7) {
      c1 <-
        (1 / 1.03) ^ (2 * n)
    }  else {
      c1 <-
        (1 / 1.03) ^ ((2 * n) + 1)
    }

    d <-
      (bond_coupon / 0.06) * (1 - c1)

    conversion_factor <-
      a * ((bond_coupon / 2) + c1 + d) - b

    cfactor_df <-
      data_frame(
        dateDelivery = date_delivery,
        dateMaturity = date_maturity,
        pctConversionFactor = conversion_factor
      )

    if (return_message == T) {
      conversion_factor %>%
        paste0(
          ' conversion factor for the bond maturing on ',
          date_maturity,
          ' delivered on ',
          date_delivery,
          ' with a ',
          bond_coupon * 100,
          '% coupon'
        )
    }

    return(cfactor_df)
  }


elapsed_months <- function(end_date, start_date) {
  ed <- as.POSIXlt(end_date)
  sd <- as.POSIXlt(start_date)
  12 * (ed$year - sd$year) + (ed$mon - sd$mon)
}

get_b <- function(v = 4, bond_coupon = .015) {
  b <-
    (bond_coupon  /  2)  *  (6 - v)  /  6
  return(b)
}

get_c <- function(z = 10, n = 1) {
  if (z > 7) {
    c_val <-
      (1 / (1.03 ^ (2 * n + 1)))
  } else {
    c_val <-
      (1 / (1.03 ^ (2 * n)))
  }
  return(c_val)
}

get_v <-
  function(z = 10, n = 9) {
    if (n > 5 & z >= 7) {
      v <-
        3
    }
    if (n <= 5 & z >= 7) {
      v <-
        z - 6
    }
    if (n <= 5 & z < 7) {
      v <-
        3
    }
    return(v)
  }

get_a <- function(v = 4) {
  a <-
    (1 / 1.03) ^ (v / 6)
  return(a)
}
get_d <-
  function(bond_coupon = .015,
           c_val = 0.915142) {
    d <-
      (bond_coupon / .06) * (1 - c_val)
    return(d)
  }


get_factor_val <-
  function(a = 0.980487,
           b = 0.000025,
           bond_coupon = .015,
           c_val = 0.915142,
           d = 0.0212146) {
    factor_val <-
      (a  *  ((bond_coupon  /  2)  +  c_val  +  d))  -  b
    return(factor_val)
  }


#' Get conversion factor for US treasury futures curve
#'
#' @param date_delivery
#' @param date_maturity
#' @param bond_coupon
#' @param return_df
#'
#' @return
#' @export
#' @importFrom lubridate ymd
#' @importFrom dplyr data_frame
#' @examples

calculate_ust_futures_conversion_factor <-
  function(date_delivery = "2009-03-01",
           date_maturity = "2012-01-15",
           bond_coupon  = 1.125,
           return_df = T) {
    options(digits = 5)
    date_delivery <-
      date_delivery %>% ymd

    date_maturity <-
      date_maturity %>% ymd

    bond_coupon <-
      bond_coupon / 100

    days <-
      as.numeric((date_maturity - date_delivery))

    n <- # year
      floor(days / 365.25)

    days_through <-
      (1 - (days / 365.2 - n)) * 365.25

    months_remaining <-
      (365.25 - days_through) / (365.25 / 12)

    z <-
      months_remaining %>% floor

    v <-
      z %>%
      get_v(n = n)

    a <-
      v %>%
      get_a
    b <-
      v %>%
      get_b(bond_coupon = bond_coupon)
    c_val <-
      get_c(z = z, n = n)

    d <-
      get_d(bond_coupon = bond_coupon, c_val = c_val)

    factor_value <-
      get_factor_val(
        a = a,
        b = b,
        bond_coupon = bond_coupon,
        c_val = c_val,
        d = d
      )

    if (return_df == T) {
      factor_value <-
        data_frame(
          dateDelivery = date_delivery,
          dateMaturity = date_maturity,
          rateCoupon = bond_coupon,
          pctFactor = factor_value
        )
    }
    return(factor_value)
  }


parse_purchase_price <- function(price = "97-18+", market = "cash") {
  if (!market %in% c("cash", "futures")) {
    stop("Market can only be cash or futures")
  }
  options(digits = 7)
  base <-
    price %>% substr(1, 2) %>% readr::parse_number

  values <-
    price %>% str_split('\\-') %>% flatten_chr()


if (values %>%  length > 1) {
  ratio_32s <-
    values[2] %>% readr::parse_number() / 32

  if (price %>% str_detect("\\+")) {
    ratio_32s <-
      (1 / 64) + ratio_32s
  }
  if (values[2] %>% readr::parse_number() > 100) {
    base_ratio <-
      values[2] %>%  substr(1, 2) %>% readr::parse_number()
    frac_ratio <-
      values[2] %>% substr(3, 3) %>% readr::parse_number() / 8

    ratio_32s <-
      (base_ratio / 32) + (frac_ratio * (1 / 32))
    price <-
      base + ratio_32s %>% digits(7)
  } else {
    price <-
      base + ratio_32s
  }
} else{
  price <-
    base
}

  return(price)
}

#' Get bond data data frame
#'
#' @param face_value
#' @param date_purchase
#' @param date_issue
#' @param bond_duration_years
#' @param purchase_price
#' @param bond_coupon
#' @param use_currency
#' @param unnest data
#'
#' @return
#' @export
#' @importFrom tidyr unnest
#' @importFrom RQuantLib Schedule
#' @import formattable lubridate dplyr
#' @examples
#' calculate_data_bond_future_irr(face_value = 1000000, date_purchase = "2013-01-10", id_period_type = "S", date_issue = "2012-11-15", bond_duration_years = 10, purchase_price = "97-18+", bond_coupon =  1 + (5 / 8), use_currency = T)
calculate_data_bond_future_irr <-
  function(face_value = 1000000,
           date_purchase = "2013-01-10",
           id_period_type = "S",
           assign_to_environment = T,
           date_issue = "2012-11-15",
           bond_duration_years = 10,
           purchase_price = "97-18+",
           bond_coupon =  1 + (5 / 8),
           use_currency = T,
           unnest_data = F) {
    options(digits = 6)

    period_df <-
      dplyr::data_frame(
      idPeriodType = c("D", "W", "M", "S", "Y"),
      period = c("Day", "Week", "Month", "Semiannual", "year")
    )

    period_type <-
      period_df %>%
      dplyr::filter(id_period_type == idPeriodType) %>%
      .$period

    date_purchase <-
      date_purchase %>% ymd

    date_issue <-
      date_issue %>% ymd

    date_expiry <-
      date_issue + months((12 * bond_duration_years))

    params <- list(
      effectiveDate = date_issue,
      maturityDate = date_expiry,
      period = period_type,
      calendar = 'UnitedStates/GovernmentBond',
      businessDayConvention = 'Unadjusted',
      terminationDateConvention = 'Unadjusted',
      dateGeneration = 'Forward',
      endOfMonth = 1
    )
    payment_dates <-
      RQuantLib::Schedule(params)

    upcoming_payments <-
      payment_dates[!date_purchase > payment_dates]
    date_next_coupon <-
      upcoming_payments %>% .[[1]]
    pct_coupon <-
      bond_coupon / 100
    purchase_value <-
      purchase_price %>% parse_purchase_price()
    purchase_amount <-
      purchase_value / 100 * face_value
    if (use_currency == T) {
      purchase_amount <-
        purchase_amount %>% formattable::currency(digits = 2)
    }


    ## accrued interest

    accrued_days <-
      as.numeric(date_purchase - date_issue) + 1

    accrued_period <-
      as.numeric(date_next_coupon - date_issue)

    accrued_interest <-
      (pct_coupon * (face_value) / 2) * (accrued_days / accrued_period)

    if (use_currency == T) {
      accrued_interest <-
        accrued_interest %>% formattable::currency(digits = 2)
    }

    bond_price <-
      purchase_amount + accrued_interest

    metadata_df <-
      bond_df <-
      data_frame(
        amountBond = purchase_amount,
        amountAccruedInterest = accrued_interest,
        amountTotal = amountBond + amountAccruedInterest
      )

    pmt_data <-
      data_frame(dateCF = c(date_purchase, upcoming_payments)) %>%
      mutate(
        idPeriod = 1:n() - 1,
        days = as.numeric(dateCF - dplyr::lag(dateCF)),
        amountPurchase = ifelse(idPeriod == min(idPeriod), -bond_price, 0),
        amountInterest = ifelse(idPeriod > 0,
                                pct_coupon / 365 * days * face_value, 0),
        amountRepayment = ifelse(idPeriod == max(idPeriod), face_value, 0),
        amountCF =  amountPurchase + amountInterest + amountRepayment
      ) %>%
      dplyr::select(idPeriod, everything()) %>%
      suppressWarnings()

    if (use_currency) {
      pmt_data <-
        pmt_data %>%
        mutate_each_(funs(currency(., digits = 2)),
                     vars = pmt_data %>% dplyr::select(matches("amount")) %>% names)
    }

    bond_df <-
      data_frame(
        amountBond = purchase_amount,
        amountAccruedInterest = accrued_interest,
        amountTotal = amountBond + amountAccruedInterest
      ) %>%
      bind_cols(calculate_data_periods_irr(cash_flows = pmt_data$amountCF, dates = pmt_data$dateCF))

    data <-
      data_frame(nameTable = c('Metadata', 'Payment Data'),
                 dataTable = list(metadata_df = bond_df, payment_df = pmt_data))

    if (assign_to_environment) {
      data <-
        data %>%
        left_join(
          data_frame(nameTable = c('Metadata', 'Payment Data'),
                     idDF = c('metdataBond', 'payymentDataBond')
          )
        ) %>%
        suppressMessages()

      for(x in 1:nrow(data)){
        table_data <-
          data$dataTable[[x]]
        df_name <-
          data$idDF[[x]]

        assign(x = df_name, eval(table_data), env = .GlobalEnv)
      }
      data <-
        data %>%
        dplyr::select(-idDF)
    }

    if (unnest_data) {
      data <-
        data %>%
        unnest
    }

    return(data)

  }