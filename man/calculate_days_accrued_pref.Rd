% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/waterfall_functions.R
\name{calculate_days_accrued_pref}
\alias{calculate_days_accrued_pref}
\title{Accrued preference}
\usage{
calculate_days_accrued_pref(
  pct_pref = 0.1,
  is_actual_360 = TRUE,
  days = 31,
  equity_bb = 1700000,
  pref_accrued_bb = 0
)
}
\arguments{
\item{pct_pref}{rate of accrued preference}

\item{is_actual_360}{\code{TRUE} calculate rate actual/360 terms}

\item{days}{count of days}

\item{equity_bb}{vector of begining equity balance}

\item{pref_accrued_bb}{vector of accrued preference begining balance}
}
\value{
a \code{data_Fram}
}
\description{
This function calcuates accrued
preferences for a specified inputs
}
\examples{
calculate_days_accrued_pref(pct_pref = .1, is_actual_360 = T, days = 31, equity_bb = 1700000.00, pref_accrued_bb = 0)
}
\seealso{
Other calculation: 
\code{\link{calculate_cash_flow_dates}()},
\code{\link{calculate_cash_flow_waterfall}()},
\code{\link{calculate_cash_flow_waterfall_partnership}()},
\code{\link{calculate_cash_flows_returns}()},
\code{\link{calculate_irr_periods}()},
\code{\link{calculate_leverage_metrics}()},
\code{\link{calculate_loan_payment}()},
\code{\link{calculate_residual_valuation_cap_rates}()},
\code{\link{calculate_residual_valuation_ebitda_multiples}()},
\code{\link{calculate_share_proceeds}()},
\code{\link{calculate_valuation_post_money}()},
\code{\link{tidy_promote_structure}()}

Other partnership calculation: 
\code{\link{calculate_cash_flow_dates}()},
\code{\link{calculate_cash_flow_waterfall}()},
\code{\link{calculate_cash_flow_waterfall_partnership}()},
\code{\link{calculate_cash_flows_returns}()},
\code{\link{calculate_irr_periods}()},
\code{\link{tidy_promote_structure}()}
}
\concept{calculation}
\concept{partnership calculation}
