% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trading_view.R
\name{tv_regions_metrics}
\alias{tv_regions_metrics}
\title{Tradingview searchable metrics by region}
\usage{
tv_regions_metrics(
  regions = c("america", "uk", "australia", "brazil", "canada", "euronext", "germany",
    "hongkong", "india", "japan", "mexico", "newzealand", "russia", "singapore", "spain",
    "switzerland", "taiwan", "turkey"),
  return_message = TRUE,
  nest_data = FALSE
)
}
\arguments{
\item{regions}{vector of regions \itemize{
\item cfd
\item america
\item uk
\item australia
\item brazil
\item canada
\item euronext
\item germany
\item hongkong
\item india
\item japan
\item mexico
\item newzealand
\item russia
\item singapore
\item spain
\item switzerland
\item taiwan
\item turkey
}}

\item{return_message}{if \code{TRUE} return message}
}
\description{
Get searchable metrics by region
}
