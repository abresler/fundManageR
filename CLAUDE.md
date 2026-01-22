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

## Dictionary Maintenance

When you see "Missing {name} in dictionary" warnings, **always add the missing names** to the appropriate dictionary function:

- **SEC ADV data**: Add to `dictionary_sec_names()` in `R/adv_functions.R`
  - Add the raw SEC column name to the `nameSEC` vector
  - Add the corresponding cleaned name to the `nameActual` vector (follow naming conventions above)

- **FINRA data**: Add to dictionary in `R/finra.R`
- **FDIC data**: Add to dictionary in `R/government_data.R`

The `nameActual` values must follow the column naming conventions (id*, name*, amount*, pct*, count*, is*, has*, etc.).

## Console Messaging System (Ferrara Style)

The package uses a styled console messaging system inspired by the visual aesthetic of Abel Ferrara films - neon-soaked noir with atmospheric darkness. All user-facing messages should use the helper functions in `R/ferrara_messaging.R`.

### The Ferrara Palette

| Element | Color | Inspiration |
|---------|-------|-------------|
| Headlines | Neon Magenta | Times Square sleaze |
| Info/Counts | Electric Cyan | Surveillance cold |
| Success | Amber Gold | Streetlight glow |
| Warnings | Blood Crimson | Violence undertones |
| Errors | Deep Purple | Corruption, surreal |
| Values | Stark White | Honest, clinical |
| Subtle | Smoke Gray | Fog, ambiguity |

### Core Message Functions

```r
# Success - The Deal Closes
.fm_success("Transaction complete for {.fm_entity Blackstone}")

# Info - The Surveillance
.fm_info("Monitoring {.fm_value 500} entities")

# Warning - The Danger
.fm_warning("Missing {.fm_value idCRD} in dictionary")

# Danger/Error - The Fall
.fm_danger("Connection failed to {.fm_entity SEC EDGAR}")

# Headlines - The Title Card
.fm_headline("Into the void...")

# Parsing progress
.fm_parsing("https://www.sec.gov/...")
```

### Compound Message Builders

```r
# Data acquisition (most common)
.fm_data_acquired(
  n_rows = 1500,
  source = "SEC EDGAR",
  entity = "Apollo",
  extra = "Q4 2024 filings"
)

# Time range results
.fm_time_range(
  start_year = 2010,
  end_year = 2024,
  n_records = 50000,
  data_type = "REIT transactions"
)

# Entity spotlight (for random highlights)
.fm_spotlight(
  company = "ANTHROPIC",
  description = "AI safety research company",
  url = "https://anthropic.com",
  extra = list("Valuation" = "$15B", "Batch" = "Winter 2021")
)

# Financial results
.fm_financial_result("Total AUM", 5000000000, is_currency = TRUE)
```

### Inline Styling Classes

Use these within message strings for consistent formatting:

- `{.fm_value 1500}` - Numbers and values (white, bold)
- `{.fm_entity Blackstone}` - Entity names (magenta, italic)
- `{.fm_url https://...}` - URLs (cyan, italic)
- `{.fm_subtle additional info}` - Secondary info (gray)

### When to Use Messages

- **Data acquisition**: Always show count and source on success
- **Parsing**: Show URL being parsed for transparency
- **Warnings**: Dictionary misses, data structure changes
- **Errors**: API failures, missing required parameters
- **Spotlights**: Random entity highlights (YC companies, unicorns)

### Migration Pattern

When updating legacy messages:

```r
# OLD (deprecated)
glue("Found {nrow(data)} records") %>% cat(fill = T)
list("Parsed: ", url) %>% purrr::invoke(paste0, .) %>% cat(fill = T)

# NEW (Ferrara style)
.fm_data_acquired(n_rows = nrow(data), source = "SEC")
.fm_parsing(url)
```

## Testing

Tests are minimal - primarily integration tests that hit live APIs (skipped on CRAN). When adding functionality, wrap API calls with `purrr::possibly()` for graceful API failures.
