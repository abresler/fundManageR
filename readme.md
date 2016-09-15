fundManageR
================

<strong>`fundManageR` — R's toolkit for the investment management</strong>

<img src = 'http://i.imgur.com/ryDGtVV.jpg' alt="fundManageR">

#### <strong>What is fundManageR?</strong>

Outside of some isolated pockets, the players in the $67,0000,000,000+ United States investment management are largely trapped in Excel. Whether you are talking about Private Equity, Venture Capital, Real Estate Private Equity or Hedge Funds, industry professionals are executing important business functions dangerously and inefficiently via spreadsheets. The purpose of this package is to provide R proficient industry professionals a generalized framework that provides out-of-the-box access to functions that perform many of the industry's most important calculations. As an added bonus this package also wraps access to an growing list of data silos that may be of use to industry professionals, academics, journalists, or anyone who enjoys exploring interesting data sets.

#### <strong>Why fundManageR?</strong>

Excel, while convenient for some, is poorly suited for important data analysis and modelling. Countless billions of dollars have been lost throughout the investment management universe as a direct result of uncaught Excel errors. Additionally, for many of the most common calculations, there is countless duplication of methods to perform some of the industries most commonly important calculations. `fundManageR`, though in its extreme infancy, is my attempt to provide an easy to use framework for all of these important calculations to be performed in R. These calculations have fault checks to ensure correct calculations and can be mapped and iterated to perform complex iterations of these calculations that would be extremely difficult to replicate in Excel.

My second motivation for creating this package is to provide easy access to some of the industry's [dark data](http://www.gartner.com/it-glossary/dark-data), hidden APIs, and public APIs to enable more transparency around one of the United States' most important industries. As the old saying goes, when in doubt <strong>`Follow the Money`</strong>, this package will help you do just that.

#### Package Idioms

`fundManageR` is built around 2 families of functions:

-   `calculate_` -- this family of functions performs common industry specific and generalized calculations.
-   `get_data_` -- this family of functions retrieves data either from a specified silo or based upon user inputs.

In the future there may be a third family of functions that would allow one to easily visualize calculation output and/or collected data.

### Package Dependancies

In order for this package to work you need the following packages installed if they aren't already. You can run this code to install the packages.

For `pdftools` you may need to follow the installation instructions [here](https://github.com/ropensci/pdftools).

``` r
packages <- 
  c("curl", "curlconverter", "dplyr", "formattable", "httr", "jsonlite", 
    "lazyeval", "lubridate", "magrittr", "pdftools", "purrr", "readr", 
    "readxl", "rvest", "stringi", "stringr", "tibble", "tidyr", "urltools", 
    "xml2")

lapply(packages, install.packages)
```

#### <strong>Installation</strong>

``` r
devtools::install_github("abresler/fundManageR")
```

### `calculate_` Functions

The purpose of these functions are to perform calculations. The function family includes pertinent functions specific to Real Estate, Private Equity and generally relevent functions that apply to Investment Managers in Hedge Funds, Real Estate Private Equity, Venture Capital and traditional private equity. Additionally the package provides a suite of generalized financial functions some of which are already available in other R Packages.

### `get_data_` Functions

### Vignettes
