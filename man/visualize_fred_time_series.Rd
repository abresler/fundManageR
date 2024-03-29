% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fred_api.R
\name{visualize_fred_time_series}
\alias{visualize_fred_time_series}
\title{Plot any FRED time series}
\usage{
visualize_fred_time_series(
  series_id = "DGS2",
  use_random = FALSE,
  fred_data_transformation = NULL,
  date_start = NULL,
  date_end = NULL,
  plot_transformations = c("mean", "median", "smooth"),
  use_hrbr_theme = FALSE,
  plot_labels = FALSE,
  interactive = FALSE
)
}
\arguments{
\item{series_id}{any FRED series ID}

\item{use_random}{if \code{TRUE} a random FRED series ID is chosen}

\item{date_start}{data start date, if \code{NULL} all chosen}

\item{date_end}{data end date}

\item{plot_transformations}{Any plot transformations you wish to apply \itemize{
\code{median}: Median value
\code{mean}: Mean value
\code{smooth}: GAM smooth line
}}

\item{use_hrbr_theme}{uses Bob Rudis theme}

\item{plot_labels}{if \code{TRUE} text of any plot transformations are plotted}

\item{interactive}{if \code{TRUE} visualization turned into an interactive plotly widget}

\item{fred_data_transformations}{Any FRED transformation \itemize{
\item \code{default}
\item \code{change}
\item \code{change prior year}
\item \code{percent change}
\item \code{percent change prior year}
\item \code{compounded rate of change}
\item \code{continiously compounded rate of change}
\item \code{continiously compounded annual rate of change}
\item \code{natural log}
\item \code{index}
}}
}
\value{
if \code{interactive} a plotly htmlwidget or else a static ggplot2 visualization
}
\description{
Plot any FRED time series
}
