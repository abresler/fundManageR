fundManageR
================

<strong>`fundManageR` — R's Investment Management Toolkit<br>Because the Industry Must Liberate Itself from the Chains of Excel</strong>

<img src = 'http://i.imgur.com/ryDGtVV.jpg' alt="fundManageR">

#### <strong>What is fundManageR?</strong>

Outside of a few isolated pockets, the participants in the $67,0000,000,000 + United States investment management are trapped in Excel-centric universe. This is a dangerous state of being

Whether you are talking about [Private Equity](https://en.wikipedia.org/wiki/Private_equity), [Venture Capital](https://en.wikipedia.org/wiki/Venture_capital), [Real Estate Private Equity](https://en.wikipedia.org/wiki/Private_equity_real_estate) or [Hedge Funds](https://en.wikipedia.org/wiki/Hedge_fund), industry professionals are executing important business functions dangerously and inefficiently through a maze of Excel spreadsheets.

The purpose of this package is to provide R proficient industry professionals a generalized framework that provides out-of-the-box access to functions that perform many standardizable industry pertinent calculations.

As an added bonus this package also wraps access to a growing array of data silos, including the first full wrapper of the [SEC's Investment Adviser Public Disclosure](https://adviserinfo.sec.gov/) database in any of the major programming languages. It's my hope that this package's data acquisition functionality will be of use to industry professionals, academics, journalists, or anyone who enjoys exploring interesting data sets.

#### <strong>Why fundManageR?</strong>

Excel, while convenient for some, is poorly suited for important data analysis and modelling. Billions of dollars have been lost throughout the investment management universe as a *direct* result of uncaught Excel errors. Here are few examples:

-   [Spreadsheet Errors Costing Business Billions, a CNBC Exploration](http://www.cnbc.com/id/100923538)
-   [Spreadsheet Mistake Costs Tibco Shareholders $100 Million](http://blogs.wsj.com/moneybeat/2014/10/16/spreadsheet-mistake-costs-tibco-shareholders-100-million/)
-   [The $400 Million Mistake at Lazard: Solarcity "Undervalued" in Tesla Purchase](http://www.wallstreetoasis.com/forums/the-400-million-mistake-at-lazard-solarcity-undervalued-in-tesla-purchase)
-   [Eight of the Worst Spreadsheet Blunders](http://www.cio.com/article/2438188/enterprise-software/eight-of-the-worst-spreadsheet-blunders.html)
-   [ASB ALLEGIANCE REAL ESTATE FUND vs SCION MANAGER](http://courts.state.de.us/opinions/download.aspx?ID=172670) - warning, PDF file of a lawsuit.
-   [Did an Excel error bring down the London Whale?](http://blog.revolutionanalytics.com/2013/02/did-an-excel-error-bring-down-the-london-whale.html)

Further more, for many of the most common calculations, there is countless duplication of methods to perform common calculations, and in many cases formulas are left unchecked and errors end up having extreme consequences down the line.

Though in its extreme infancy, `fundManageR` is my attempt to provide an easy to use framework for these calculations to be performed in R, through an open and readable API, consistent with [Hadley Wickham's](https://twitter.com/hadleywickham) [tidy tools](https://mran.microsoft.com/web/packages/tidyverse/vignettes/manifesto.html) manifesto. These calculations have fault checks to ensure accuracy and designed for iteration which allows for complex calculations that Excel would be unable to execute.

The package's other motive is to provide easy access to the financial industry's [dark data](http://www.gartner.com/it-glossary/dark-data), hidden and public APIs. This should enable better transparency around one of the United States' most vital industries. As the old saying goes, when in doubt <strong>`Follow the Money`</strong>, `fundManageR` should help to do this.

### Package Dependencies

In order for this package to work you need the packages listed below must be installed. You can run the code snippet to install them if necessary.

For `pdftools` you may need to follow the installation instructions [here](https://github.com/ropensci/pdftools).

``` r
packages <- 
  c("curl", "curlconverter", "dplyr", "formattable", "httr", "jsonlite", 'devtools',
    "lazyeval", "lubridate", "magrittr", "pdftools", "purrr", "readr",  'quantmod',
    "readxl", "rvest", "stringi", "stringr", "tibble", "tidyr", 'tidyverse',
    "xml2")

lapply(packages, install.packages)
```

#### <strong>Package Installation</strong>

``` r
devtools::install_github("abresler/fundManageR")
```

#### Package Idioms

`fundManageR` is built around 2 full function families and another with 1 usable, but powerful, with many more on the way.

-   `calculate_` -- this function family performs common industry specific and generalized calculations.
-   `get_data_` -- this function family retrieves data either from a specified silo or based upon user inputs.
-   `visualize_` -- this function family performs pre-packaged visualizations geared towards the data this package brings in or helps the user to create.

### `calculate_` Functions

-   `calculate_cash_flow_dates` -- Calculates summary cash flows for a specified dates and cash flows
-   `calculate_irr_periods` -- Calculates [interal rate of return](https://en.wikipedia.org/wiki/Internal_rate_of_return) for specified dates and cash flows
-   `calculate_cash_flows_returns` -- Calculates investment returns for specified dates and cash flows
-   `calculate_cash_flow_waterfall` -- Calculates a cash flow waterfall given specified dates, cash flows and promote structure
-   `calculate_cash_flow_waterfall_partnership` -- Calculates partnership level returns and waterfall given specified equity splits, promote structure, and cash flows.
-   `calculate_loan_payment` -- Calculates loan repayment data given specified parameters
-   `calculate_residual_valuation_ebitda_multiples` -- Calculates residual values given specified [EBITDA](https://en.wikipedia.org/wiki/Earnings_before_interest,_taxes,_depreciation,_and_amortization) and [EBITDA Multiples](http://www.investopedia.com/terms/e/ev-ebitda.asp)
-   `calculate_residual_valuation_cap_rates` -- Calculates residual value given specified [Net Operating Income](http://www.investopedia.com/terms/n/noi.asp) and [Capitalization Rates](http://www.investopedia.com/terms/c/capitalizationrate.asp)
-   `calculate_valuation_post_money` -- Calculates entity ownership post investment
-   `calculate_days_accrued_pref` -- Calculates accrued preference/interest for a specified period.

### `get_data_` Functions

-   `get_data_ycombinator_alumni` -- Retrieves data on [YCombinator](http://www.ycombinator.com/) graduates.
-   `get_data_libor_current` --Retrieves most recent [LIBOR](https://en.wikipedia.org/wiki/Libor) by duration.
-   `get_data_promote_structure` -- Returns a [carried/promoted interest](https://en.wikipedia.org/wiki/Carried_interest) given promote syntax.
-   `get_data_fred_index_symbol_time_series` -- Retrieves time series data for specified index from the [FRED Database](https://en.wikipedia.org/wiki/Federal_Reserve_Economic_Data).
-   `get_data_index_symbol_current_value` -- Retrieves current value for a specified symbol.
-   `get_data_cik_codes` -- Retrieves all entities with a registered [Central Index Key](https://en.wikipedia.org/wiki/Central_Index_Key).

### ADV Specific `get_data_` Functions

-   `get_data_adv_period_urls` -- Retrieves all possible ADV summary periods.
-   `get_data_adv_managers_current_period_summary` -- Retrieves summary data for ADV filing managers from the most recent monthly filing period.
-   `get_data_adv_managers_periods_summaries`-- Retrieves summary ADV filings for specified periods and filing type. By default the files will be saved to and loaded from a temporary directory however a user can override this default by specifying a file directory. If you specify a file directory you can also specify folder name, if you don't do this the folder `adv_data` is created by default. You can also tell the function to delete the folders and empty the trash by setting `remove_files` and `empty_trash` to TRUE.
-   `get_data_sec_adv_manager_sitemap`-- Retrieves a data frame with the possible detailed ADV sections and their descriptions.
-   `get_data_adv_managers_metadata` -- Retrieves metadata for specified search name or CRD ID; fastest way to search for managers you may wish to explore further.
-   `get_data_adv_managers_filings` -- Retrieves detailed ADV filing for specified [Central Registration Depository ID](http://www.finra.org/industry/crd) \[CRD\] and/or company name by by specified ADV section.
-   `get_data_adv_managers_brochures` -- Retrieves and [OCRs](https://en.wikipedia.org/wiki/Optical_character_recognition) for SEC mandated annual [Uniform Requirements for the Investment Adviser Brochure and Brochure Supplements](https://www.sec.gov/about/forms/formadv-part2.pdf) for specified CRDs and/or company names.

### `visualize_` Functions

-   `visualize_data_frame` -- This function allows the user to visualize a data frame object to better understand the structure and contents of a data frame. The function utlizes the fantastic [listviewer](https://github.com/timelyportfolio/listviewer) package which wraps Jos de Jong's [JSON Editor](http://jsoneditoronline.org/) project

### Vignettes

-   [ADV Function Tutorial -- Sleuthing The Blackstone Group](http://rstudio-pubs-static.s3.amazonaws.com/211588_9a2e2051c7d2472aa8879ce7ed4fe48b.html)
-   [Partnership Waterfall Calculation: Hypothetical Facebook Seed Investment](http://rstudio-pubs-static.s3.amazonaws.com/212788_b319809622a942b4a760d307a4cfe636.html)

### Brief Examples

#### Most Recent ADV Summary Data

``` r
library(fundManageR)
recent_period_summary <- 
  get_data_adv_managers_current_period_summary(file_directory = NULL)
```

#### Detailed Manager Search

``` r
test_manager <- 
  fundManageR::get_data_adv_managers_filings(
    search_names = 'EJF Capital',
    crd_ids = 156663,
    all_sections = T,
    assign_to_enviornment = T
  )
```

#### YCombinator Alumni

``` r
ycombinator_alumni <- 
  fundManageR::get_data_ycombinator_alumni()

ycombinator_alumni %>% 
  visualize_data_frame(edit = F) ## visualize it
```

##### IRR Calculation

``` r
library(dplyr)
dates <- 
  c("2015-10-09", "2016-09-26") %>% lubridate::ymd

land_purchase_price <-
  -1000000 %>% formattable::currency()

land_sale_price <-
  1300000 %>% formattable::currency()

cash_flows <- 
  c(land_purchase_price, land_sale_price)

fundManageR::calculate_irr_periods(dates = dates, cash_flows = cash_flows, return_wide = T)
```

### Coming Soon

-   Full roll out of `visualize_` functions
-   Mergers and Acquisition data
-   Integrated leveraged cash flow analysis calculations
-   Catch-up calculations in promote structures
-   [XBRL](https://www.xbrl.org/) wrapper
-   [Open Corporates]('https://opencorporates.com/) wrapper
-   [Form D](https://en.wikipedia.org/wiki/Form_D) wrapper
-   State specific [UCC](https://en.wikipedia.org/wiki/Uniform_Commercial_Code) wrappers
-   Delaware, Cayman Island, and Nevada entity registration wrappers
