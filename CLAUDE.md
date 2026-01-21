# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

fundManageR is an investment management toolkit for R that provides:
1. **Financial calculation functions** - IRR, waterfall distributions, valuations, loan payments
2. **Data API wrappers** - SEC/EDGAR, FRED, FINRA, NAREIT, DTCC, CRSP, MSCI, TradingView, and more

The package follows Hadley Wickham's tidy tools manifesto with consistent naming conventions and tibble outputs.

## Development Commands

```bash
# Install package locally (from R console)
devtools::install()

# Load for development
devtools::load_all()

# Run tests
devtools::test()
# Or: testthat::test_dir("tests/testthat")

# Check package (full R CMD check)
devtools::check()

# Generate documentation from roxygen2 comments
devtools::document()

# Build documentation site
pkgdown::build_site()
```

## Architecture

### Function Families

**`calculate_*` functions** (R/cf_functions.R, R/waterfall_functions.R, R/new_residual_calcs.R):
- `calculate_irr_periods()` - Internal rate of return
- `calculate_cash_flow_waterfall()` - Partnership waterfall with promote tiers
- `calculate_residual_valuation_*()` - Exit valuations by EBITDA or cap rate
- `calculate_leverage_metrics()` - LBO back-of-envelope calculations

**Data retrieval functions** (named by source):
- `adv_*` (R/adv_functions.R) - SEC ADV investment adviser data
- `sec_*`, `edgar_*` (R/sec_functions.R, R/sec_2.R, R/edgar_rf.R) - SEC filings
- `fred_*` (R/fred_api.R) - Federal Reserve economic data
- `nareit_*` (R/nareit.R) - REIT data
- `finra_*` (R/finra.R) - Broker/dealer data
- `crsp_*`, `msci_*` (R/indicies.R) - Market indices
- `dtcc_*` (R/dtcc.R) - Securities transaction data

**Utility functions** (R/utils.R):
- `tidy_column_formats()` - Auto-format columns by naming patterns
- `tidy_column_relations()` - Unnest nested data
- `drop_na_columns()` - Remove all-NA columns

### Column Naming Convention

The package uses consistent column prefixes:
- `id*` - Identifiers (idCRD, idCIK, idSymbol)
- `name*` - Text names (nameEntity, nameFund)
- `date*`, `datetime*` - Temporal values
- `amount*` - Currency values (formatted with formattable::currency)
- `pct*` - Percentages (formatted with formattable::percent)
- `count*` - Counts (formatted with formattable::comma)
- `is*`, `has*` - Boolean flags
- `url*` - URLs
- `data*` - Nested data frames

### Data Access Patterns

Web scraping uses `rvest` with CSS selectors. API calls use `httr`/`curl` with `jsonlite` for JSON. Parallel operations use `furrr::future_map_dfr()` - enable with `future::plan(multiprocess)`.

Error handling wraps functions with `purrr::possibly()` for graceful API failures.

### Key Files by Size/Complexity

- R/sec_2.R (~13K lines) - Extended SEC data functions
- R/waterfall_functions.R - Complex partnership waterfall calculations
- R/adv_functions.R - SEC ADV wrapper (first full wrapper in any language)
- R/utils.R - Core utility functions used throughout

## Dependencies

Core: dplyr, tidyr, purrr, tibble, stringr
Web: rvest, httr, curl, jsonlite, xml2
Formatting: formattable (currency, percent, comma)
Parallel: furrr, future
Special: pdftools (OCR), XBRL (financial statements), tabulapdf (PDF tables)

## Testing

Tests are minimal - primarily integration tests that hit live APIs (skipped on CRAN). When adding functionality, wrap API calls with `purrr::possibly()` to handle failures gracefully.
