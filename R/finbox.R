#gdeltr2::load_needed_packages(c('dplyr', 'rvest', 'jsonlite', 'stringr', 'purrr', 'lubridate', 'stringi', 'tidyr', 'rlang', 'curl', 'jsonlite', 'anytime', 'reticulate'))

# munge -------------------------------------------------------------------

.extract_date <-
  function(period = "6 days ago", return_character = T) {
    period <-
      period %>% str_replace_all("^a |^an ", "1 ")
    time_number <-
      period %>% as.character() %>% readr::parse_number()
    period_slug <- str_to_lower(period)
    time_span <- dplyr::case_when(
      period_slug %>% str_detect("second") ~ "secs",
      period_slug %>% str_detect("minute") ~ "mins",
      period_slug %>% str_detect("hour") ~ "hours",
      period_slug %>% str_detect("day") ~ "days",
      period_slug %>% str_detect("week") ~ "weeks",
      period_slug %>% str_detect("month") ~ "months",
      period_slug %>% str_detect("year") ~ "years"
    )

    if (time_span %>% str_detect("year")) {
      x <- Sys.time() - lubridate::years(time_number)

    }

    if  (time_span %>% str_detect("month")) {
      x <-
        Sys.time() %m-% months(time_number)
    }

    if (!period_slug %>% str_detect("month|year")) {
      x <-
        Sys.time() - as.difftime(time_number, units = time_span)
    }

    if (return_character) {
      x <- as.character(x)
    }
    x
  }

# dictionaries ------------------------------------------------------------

.finbox_metric_names <-
  function() {
    structure(
        list(
          typeMetric = c(
            "Valuation",
            "Valuation",
            "Valuation",
            "Valuation",
            "Valuation",
            "Valuation",
            "Valuation",
            "Valuation",
            "Valuation",
            "Valuation",
            "Valuation",
            "Valuation",
            "Valuation",
            "Valuation",
            "Valuation",
            "Valuation",
            "Valuation",
            "Valuation",
            "Valuation",
            "Valuation",
            "Valuation",
            "Valuation",
            "Valuation",
            "Valuation",
            "Valuation",
            "Valuation",
            "Valuation",
            "Valuation",
            "Valuation",
            "Valuation",
            "Valuation",
            "Valuation",
            "Valuation",
            "Valuation",
            "Valuation",
            "Valuation",
            "Valuation",
            "Valuation",
            "Valuation",
            "Valuation",
            "Valuation",
            "Valuation",
            "Valuation",
            "Valuation",
            "Valuation",
            "Valuation",
            "Valuation",
            "Valuation",
            "Valuation",
            "Valuation",
            "Valuation",
            "Valuation",
            "Valuation",
            "Valuation",
            "Valuation",
            "Valuation",
            "Valuation",
            "Valuation",
            "Valuation",
            "Valuation",
            "Valuation",
            "Valuation",
            "Valuation",
            "Valuation",
            "Valuation",
            "Valuation",
            "Risk",
            "Risk",
            "Risk",
            "Risk",
            "Risk",
            "Risk",
            "Risk",
            "Risk",
            "Risk",
            "Efficency",
            "Efficency",
            "Efficency",
            "Efficency",
            "Efficency",
            "Efficency",
            "Efficency",
            "Efficency",
            "Efficency",
            "Efficency",
            "Efficency",
            "Efficency",
            "Efficency",
            "Efficency",
            "Efficency",
            "Efficency",
            "Efficency",
            "Efficency",
            "Efficency",
            "Efficency",
            "Efficency",
            "Efficency",
            "Efficency",
            "Efficency",
            "Efficency",
            "Efficency",
            "Efficency",
            "Efficency",
            "Efficency",
            "Efficency",
            "Efficency",
            "Efficency",
            "Efficency",
            "Efficency",
            "Efficency",
            "Efficency",
            "Efficency",
            "Efficency",
            "Efficency",
            "Efficency",
            "Efficency",
            "Efficency",
            "Efficency",
            "Efficency",
            "Efficency",
            "Efficency",
            "Efficency",
            "Efficency",
            "Efficency",
            "Efficency",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Financials",
            "Forecasts",
            "Forecasts",
            "Forecasts",
            "Forecasts",
            "Forecasts",
            "Forecasts",
            "Forecasts",
            "Forecasts",
            "Forecasts",
            "Forecasts",
            "Forecasts",
            "Forecasts",
            "Forecasts",
            "Forecasts",
            "Forecasts",
            "Forecasts",
            "Forecasts",
            "Forecasts",
            "Forecasts",
            "Forecasts",
            "Forecasts",
            "Forecasts",
            "Forecasts",
            "Forecasts",
            "Forecasts",
            "Forecasts",
            "Forecasts",
            "Forecasts",
            "Forecasts",
            "Forecasts",
            "Forecasts",
            "Forecasts",
            "Forecasts",
            "Forecasts",
            "Forecasts",
            "Forecasts",
            "Forecasts",
            "Forecasts",
            "Forecasts",
            "Forecasts",
            "Forecasts",
            "Misc",
            "Misc",
            "Misc",
            "Misc",
            "Misc",
            "Misc",
            "Misc",
            "Misc",
            "Misc"
          ),
          descriptionMetric = c(
            "$ Change Today",
            "% of 50 Day Moving Avg",
            "52 Week High",
            "Avg Daily Volume",
            "Book Value / Market Cap",
            "Cost of Debt (Pre-tax)",
            "Daily Stock Price High (Adjusted)",
            "Daily Trading Volume (Unadjusted)",
            "Damodaran's Industry Cost of Equity",
            "Downside (Analyst Target)",
            "EV / Fwd EBITDA",
            "EV / LTM EBITDA",
            "Enterprise Value",
            "Fair Value (Lowest Analyst Target)",
            "Fair Value (finbox.io)",
            "Fwd Earnings Yield",
            "Market Cap / LTM AFFO",
            "P/E Ratio (Fwd)",
            "PEG Ratio Fwd",
            "Price / LTM Sales",
            "Stock Price (Current)",
            "Upside (Analyst Target)",
            "% Change Today",
            "% of 52 Week High",
            "52 Week Low",
            "Ben Graham Formula Upside",
            "Confidence (finbox.io Fair Value)",
            "Cost of Equity",
            "Daily Stock Price Low (Adjusted)",
            "Damodaran's Industry Cost of Capital",
            "Days To Next Earnings",
            "Downside (finbox.io)",
            "EV / Fwd Revenue",
            "EV / LTM Revenue",
            "Fair Value (Analyst Target)",
            "Fair Value (Mean Analyst Target)",
            "Float Shares / Outstanding",
            "Market Cap / Book Value",
            "Market Cap / LTM Revenue",
            "P/E Ratio (LTM)",
            "Price / Book",
            "Short Ratio",
            "Trading Volume",
            "Upside (finbox.io)",
            "% of 200 Day Moving Avg",
            "% of 52 Week Low",
            "AFFO Yield (Adjusted Funds From Oper...",
            "Ben Graham Formula Value",
            "Cost of Debt (After-tax)",
            "Daily Stock Price Close (Adjusted)",
            "Daily Stock Price Open (Adjusted)",
            "Damodaran's Industry Cost of Debt",
            "Dividend Yield",
            "EV / Fwd EBIT",
            "EV / LTM EBIT",
            "Earnings Yield",
            "Fair Value (Highest Analyst Target)",
            "Fair Value (Total Analyst Targets)",
            "Free Cash Flow Yield",
            "Market Cap / Fwd Revenue",
            "Market Capitalization",
            "PEG Ratio",
            "Price / Fwd Sales",
            "Stock Price",
            "Uncertainty (finbox.io Fair Value)",
            "WACC",
            "Altman Z-Score",
            "Current Ratio",
            "Financial Leverage",
            "Beta",
            "Debt / Equity",
            "Interest Coverage Ratio",
            "Cash / Total Capital",
            "Debt / Total Capital",
            "Quick Ratio",
            "Adjusted Funds From Operations",
            "Asset Turnover",
            "Cost of Goods Sold Margin",
            "Days Sales Outstanding",
            "EBIT Margin",
            "EBITDA Margin",
            "Effective Interest Rate",
            "Funds From Operations",
            "Gross Profit Margin",
            "Levered Free Cash Flow",
            "Minority Interest in Earnings Margin",
            "Net Profit to Common (Cont. Ops) Margin",
            "Piotroski Score",
            "Provision for Taxes Margin",
            "Return on Equity",
            "Unadjusted EBITDA Margin",
            "Unlevered Free Cash Flow Margin",
            "Adjusted Funds From Operations Growth",
            "Capital Expenditures Margin",
            "Days Inventory Outstanding",
            "Depreciation & Amortization Margin",
            "EBITDA - 3Yr Avg CapEx",
            "EBITDA minus 3Yr Avg CapEx Margin",
            "Effective Tax Rate",
            "Funds From Operations Growth",
            "Interest Expense Margin",
            "Levered Free Cash Flow Growth",
            "Net Profit (Cont. Ops) Margin",
            "Operating Income Margin",
            "Pre-tax Income Margin",
            "Research and Development Margin",
            "Return on Invested Capital",
            "Unlevered Free Cash Flow",
            "Unlevered Return on Assets",
            "Adjusted Funds From Operations Margin",
            "Cash Conversion Cycle",
            "Days Payables Outstanding",
            "EBIT Margin",
            "EBITDA - CAPEX",
            "EBITDA minus CAPEX Margin",
            "Fixed Asset Turnover",
            "Funds From Operations Margin",
            "Inventory Turnover",
            "Levered Free Cash Flow Margin",
            "Net Profit Margin",
            "Payout Ratio",
            "Preferred Dividends & Other Adj. Margin",
            "Return on Assets",
            "Selling, General and Admin Margin",
            "Unlevered Free Cash Flow Growth",
            "Accounts Payable",
            "Additional Paid-in Capital",
            "Basic EPS (Cont. Ops)",
            "Basic EPS (Cont. Ops) CAGR (7y)",
            "Basic EPS CAGR (5y)",
            "Beginning Cash",
            "Capitalized Software",
            "Cash Dividends to Common Shareholders",
            "Cash from Operations",
            "Change in Accounts Receivables",
            "Cost of Revenue",
            "Cost of Revenue CAGR (7y)",
            "Current Liabilities",
            "Depreciation & Amortization Growth",
            "Dil. EPS (Cont. Ops) CAGR (3y)",
            "Dil. EPS (Cont. Ops) Growth",
            "Dil. EPS CAGR (7y)",
            "EBIT",
            "EBIT CAGR (3y)",
            "EBIT CAGR (7y)",
            "EBIT Growth",
            "EBITDA CAGR (5y)",
            "Ending Cash",
            "Goodwill Impairment",
            "Gross Profit CAGR (5y)",
            "Interest Expense",
            "Interest Expense CAGR (7y)",
            "Litigation Expenses",
            "Merger / Acquisition Expenses",
            "Minority Interest in Earnings CAGR (3y)",
            "Minority Interest in Earnings Growth",
            "Net Profit (Cont. Ops) CAGR (3y)",
            "Net Profit (Cont. Ops) Growth",
            "Net Profit CAGR (7y)",
            "Net Profit to Common (Cont. Ops) CAG...",
            "Net Profit to Common (Cont. Ops) Growth",
            "Net Working Capital Margin",
            "Operating Income",
            "Operating Income CAGR (7y)",
            "Other Long-term Assets",
            "Plant, Property, & Equipment, net",
            "Pre-tax Income CAGR (5y)",
            "Preferred Dividends & Other Adj.",
            "Preferred Dividends & Other Adj. CAG...",
            "Prepaid Expenses",
            "Provision for Taxes CAGR (5y)",
            "Real Estate Investments",
            "Research and Development CAGR (5y)",
            "Restructuring Charges",
            "Revenue CAGR (3y)",
            "Revenue Growth",
            "Selling, General and Admin CAGR (5y)",
            "Shares Outstanding",
            "Total Change in Cash",
            "Total Equity",
            "Treasury Stock & Other",
            "Unadjusted EBITDA CAGR (5y)",
            "Weighted Avg Basic Shares Outstanding",
            "Accounts Receivable, net",
            "Asset Writedown",
            "Basic EPS (Cont. Ops) CAGR (3y)",
            "Basic EPS (Cont. Ops) Growth",
            "Basic EPS CAGR (7y)",
            "Capital Expenditures",
            "Cash & Equivalents",
            "Cash from Financing",
            "Change in AP and Accrued Liab.",
            "Change in Inventory",
            "Cost of Revenue CAGR (3y)",
            "Cost of Revenue Growth",
            "Debt and Capital Leases, Current",
            "Dil. EPS",
            "Dil. EPS (Cont. Ops) CAGR (5y)",
            "Dil. EPS CAGR (3y)",
            "Dil. EPS Growth",
            "EBIT",
            "EBIT CAGR (5y)",
            "EBIT CAGR (7y)",
            "EBITDA",
            "EBITDA CAGR (7y)",
            "FX Adjustments",
            "Gross Profit",
            "Gross Profit CAGR (7y)",
            "Interest Expense CAGR (3y)",
            "Interest Expense Growth",
            "Long-term Debt",
            "Minority Interest",
            "Minority Interest in Earnings CAGR (5y)",
            "Net Profit",
            "Net Profit (Cont. Ops) CAGR (5y)",
            "Net Profit CAGR (3y)",
            "Net Profit Growth",
            "Net Profit to Common (Cont. Ops) CAG...",
            "Net Working Capital",
            "Next Earnings Date (Estimated)",
            "Operating Income CAGR (3y)",
            "Operating Income Growth",
            "Other Non-current Liabilities",
            "Pre-tax Income",
            "Pre-tax Income CAGR (7y)",
            "Preferred Dividends & Other Adj. CAG...",
            "Preferred Dividends & Other Adj. Growth",
            "Provision for Taxes",
            "Provision for Taxes CAGR (7y)",
            "Research and Development",
            "Research and Development CAGR (7y)",
            "Retained Earnings",
            "Revenue CAGR (5y)",
            "Selling, General and Admin",
            "Selling, General and Admin CAGR (7y)",
            "Short-term Investments",
            "Total Common Equity",
            "Total Liabilities",
            "Unadjusted EBITDA",
            "Unadjusted EBITDA CAGR (7y)",
            "Weighted Avg Dil. Shares Outstanding",
            "Accrued Expenses and Other",
            "Basic EPS",
            "Basic EPS (Cont. Ops) CAGR (5y)",
            "Basic EPS CAGR (3y)",
            "Basic EPS Growth",
            "Capital Expenditures Growth",
            "Cash & Short-term Investments",
            "Cash from Investing",
            "Change in Accounts Payables",
            "Common Stock",
            "Cost of Revenue CAGR (5y)",
            "Current Assets",
            "Depreciation & Amortization",
            "Dil. EPS (Cont. Ops)",
            "Dil. EPS (Cont. Ops) CAGR (7y)",
            "Dil. EPS CAGR (5y)",
            "Dividends Payable",
            "EBIT CAGR (3y)",
            "EBIT CAGR (5y)",
            "EBIT Growth",
            "EBITDA CAGR (3y)",
            "EBITDA Growth",
            "Goodwill & Intangibles",
            "Gross Profit CAGR (3y)",
            "Gross Profit Growth",
            "Interest Expense CAGR (5y)",
            "Inventory",
            "Long-term Investments",
            "Minority Interest in Earnings",
            "Minority Interest in Earnings CAGR (7y)",
            "Net Profit (Cont. Ops)",
            "Net Profit (Cont. Ops) CAGR (7y)",
            "Net Profit CAGR (5y)",
            "Net Profit to Common (Cont. Ops)",
            "Net Profit to Common (Cont. Ops) CAG...",
            "Net Working Capital Growth",
            "Notes Payable",
            "Operating Income CAGR (5y)",
            "Other Current Assets",
            "Other Payables",
            "Pre-tax Income CAGR (3y)",
            "Pre-tax Income Growth",
            "Preferred Dividends & Other Adj. CAG...",
            "Preferred Stock",
            "Provision for Taxes CAGR (3y)",
            "Provision for Taxes Growth",
            "Research and Development CAGR (3y)",
            "Research and Development Growth",
            "Revenue",
            "Revenue CAGR (7y)",
            "Selling, General and Admin CAGR (3y)",
            "Selling, General and Admin Growth",
            "Total Assets",
            "Total Debt",
            "Total Liabilities and Equity",
            "Unadjusted EBITDA CAGR (3y)",
            "Unadjusted EBITDA Growth",
            "Avg NOPAT Margin (10y)",
            "Avg Net Income Margin (5y)",
            "Avg Proj EBITDA Margin (10y)",
            "Avg Unlevered FCF Margin (5y)",
            "Proj CapEx Margin",
            "Proj EBIT CAGR (5y)",
            "Proj EBITDA",
            "Proj EBITDA Growth",
            "Proj EPS Growth",
            "Proj Net Income",
            "Proj Net Income Growth",
            "Proj Net Working Capital Growth",
            "Proj Revenue CAGR (10y)",
            "Proj Unlevered FCF CAGR (10y)",
            "Avg NOPAT Margin (5y)",
            "Avg Proj EBIT Margin (10y)",
            "Avg Proj EBITDA Margin (5y)",
            "Proj CapEx",
            "Proj EBIT",
            "Proj EBIT Growth",
            "Proj EBITDA CAGR (10y)",
            "Proj EBITDA Margin",
            "Proj NOPAT CAGR (10y)",
            "Proj Net Income CAGR (10y)",
            "Proj Net Income Margin",
            "Proj Net Working Capital Margin",
            "Proj Revenue CAGR (5y)",
            "Proj Unlevered FCF CAGR (5y)",
            "Avg Net Income Margin (10y)",
            "Avg Proj EBIT Margin (5y)",
            "Avg Unlevered FCF Margin (10y)",
            "Proj CapEx Growth",
            "Proj EBIT CAGR (10y)",
            "Proj EBIT Margin",
            "Proj EBITDA CAGR (5y)",
            "Proj EPS",
            "Proj NOPAT CAGR (5y)",
            "Proj Net Income CAGR (5y)",
            "Proj Net Working Capital",
            "Proj Revenue",
            "Proj Revenue Growth",
            "Company Description",
            "Sector",
            "Similar Companies",
            "Company Name",
            "Sector ID",
            "Stock Exchange",
            "Index Membership",
            "Similar Companies",
            "Ticker"
          ),
          nameFinbox = c(
            "market_change_today",
            "pct_50_day_moving_average",
            "year_range_high",
            "average_daily_volume",
            "book_value_market_cap",
            "pre_tax_cost_of_debt_mid",
            "stock_price_high",
            "daily_volume",
            "industry_cost_of_equity",
            "analyst_price_target_downside",
            "ev_ebitda_fwd",
            "ev_ebitda_ltm",
            "enterprise_value",
            "analyst_target_low",
            "master_fair_value",
            "earnings_yield_fwd",
            "market_cap_affo_ltm",
            "pe_ratio_fwd",
            "peg_ratio_fwd",
            "price_sales_ltm",
            "stock_price_latest",
            "analyst_price_target_upside",
            "stock_price_latest_pct",
            "year_perc_range_high",
            "year_range_low",
            "ben_graham_formula_upside",
            "master_fair_value_confidence",
            "cost_of_equity_mid",
            "stock_price_low",
            "industry_cost_of_capital",
            "days_to_earnings",
            "master_downside",
            "ev_revenue_fwd",
            "ev_revenue_ltm",
            "analyst_price_target",
            "analyst_target_mean",
            "float_shares",
            "market_cap_book_value",
            "market_cap_revenue_ltm",
            "pe_ratio_ltm",
            "price_book_ltm",
            "short_ratio",
            "volume",
            "master_upside",
            "pct_200_day_moving_average",
            "year_perc_range_low",
            "affo_yield_ltm",
            "ben_graham_formula_value",
            "after_tax_cost_of_debt_mid",
            "stock_price_close",
            "stock_price_open",
            "industry_cost_of_debt",
            "dividend_yield",
            "ev_ebit_fwd",
            "ev_ebit_ltm",
            "earnings_yield_ltm",
            "analyst_target_high",
            "analyst_target_count",
            "fcf_yield_ltm",
            "market_cap_revenue_fwd",
            "market_cap",
            "peg_ratio_ltm",
            "price_sales_fwd",
            "stock_price",
            "master_fair_value_uncertainty",
            "wacc_mid",
            "altman_z_score",
            "current_ratio",
            "financial_leverage",
            "beta",
            "debt_to_equity",
            "interest_coverage",
            "cash_to_total_capital",
            "debt_to_total_capital",
            "quick_ratio",
            "affo",
            "asset_turnover",
            "cogs_margin",
            "days_sales_outstanding",
            "adj_ebit_margin",
            "adj_ebitda_margin",
            "effective_interest_rate",
            "ffo",
            "gross_profit_margin",
            "levered_fcf",
            "minority_interest_earnings_margin",
            "net_income_common_margin",
            "piotroski_score",
            "taxes_margin",
            "roe",
            "ebitda_margin",
            "unlevered_fcf_margin",
            "affo_growth",
            "capex_margin",
            "days_inventory_outstanding",
            "depreciation_amortization_margin",
            "ebitda_less_3yr_avg_capex",
            "ebitda_less_3yr_avg_capex_margin",
            "effective_tax_rate",
            "ffo_growth",
            "interest_expense_margin",
            "levered_fcf_growth",
            "total_net_income_margin",
            "operating_income_margin",
            "pre_tax_income_margin",
            "research_development_margin",
            "roic",
            "unlevered_fcf",
            "unlevered_roa",
            "affo_margin",
            "cash_conversion_cycle",
            "days_payables_outstanding",
            "ebit_margin",
            "ebitda_less_capex",
            "ebitda_less_capex_margin",
            "fixed_asset_turnover",
            "ffo_margin",
            "inventory_turnover",
            "levered_fcf_margin",
            "adj_net_income_margin",
            "payout_ratio",
            "preferred_dividends_other_margin",
            "roa",
            "selling_general_admin_margin",
            "unlevered_fcf_growth",
            "accounts_payable",
            "add_paid_in_capital",
            "basic_eps",
            "basic_eps_cagr_7yr",
            "adj_basic_eps_cagr_5yr",
            "cash_at_beginning",
            "capitalized_software",
            "dividends_to_common",
            "cash_from_operations",
            "change_accounts_receivables",
            "cogs",
            "cogs_cagr_7yr",
            "total_current_liabilities",
            "depreciation_amortization_growth",
            "diluted_eps_cagr_3yr",
            "diluted_eps_growth",
            "adj_diluted_eps_cagr_7yr",
            "ebit",
            "adj_ebit_cagr_3yr",
            "ebit_cagr_7yr",
            "adj_ebit_growth",
            "adj_ebitda_cagr_5yr",
            "cash_at_end",
            "goodwill_impairment",
            "gross_profit_cagr_5yr",
            "interest_expense",
            "interest_expense_cagr_7yr",
            "litigation_expenses",
            "acquisition_expenses",
            "minority_interest_earnings_cagr_3yr",
            "minority_interest_earnings_growth",
            "total_net_income_cagr_3yr",
            "total_net_income_growth",
            "adj_net_income_cagr_7yr",
            "net_income_common_cagr_3yr",
            "net_income_common_growth",
            "net_working_capital_margin",
            "operating_income",
            "operating_income_cagr_7yr",
            "other_lt_assets",
            "ppe",
            "pre_tax_income_cagr_5yr",
            "preferred_dividends_other",
            "preferred_dividends_other_cagr_7yr",
            "prepaid_expenses",
            "taxes_cagr_5yr",
            "real_estate_investments",
            "research_development_cagr_5yr",
            "restructuring_charges",
            "total_revenue_cagr_3yr",
            "total_revenue_growth",
            "selling_general_admin_cagr_5yr",
            "shares_outstanding",
            "total_change_cash",
            "total_equity",
            "treasury_stock",
            "ebitda_cagr_5yr",
            "wa_basic_shares_outstanding",
            "accounts_receivable",
            "asset_writedown",
            "basic_eps_cagr_3yr",
            "basic_eps_growth",
            "adj_basic_eps_cagr_7yr",
            "capex",
            "cash_equivalents",
            "cash_from_financing",
            "change_accounts_payables_accrued",
            "change_inventory",
            "cogs_cagr_3yr",
            "cogs_growth",
            "current_debt_cap_leases",
            "adj_diluted_eps",
            "diluted_eps_cagr_5yr",
            "adj_diluted_eps_cagr_3yr",
            "adj_diluted_eps_growth",
            "adj_ebit",
            "ebit_cagr_5yr",
            "adj_ebit_cagr_7yr",
            "adj_ebitda",
            "adj_ebitda_cagr_7yr",
            "fx_adjustments",
            "gross_profit",
            "gross_profit_cagr_7yr",
            "interest_expense_cagr_3yr",
            "interest_expense_growth",
            "lt_debt",
            "minority_interest",
            "minority_interest_earnings_cagr_5yr",
            "adj_net_income",
            "total_net_income_cagr_5yr",
            "adj_net_income_cagr_3yr",
            "adj_net_income_growth",
            "net_income_common_cagr_5yr",
            "net_working_capital",
            "next_earnings_date",
            "operating_income_cagr_3yr",
            "operating_income_growth",
            "other_lt_liabilities",
            "pre_tax_income",
            "pre_tax_income_cagr_7yr",
            "preferred_dividends_other_cagr_3yr",
            "preferred_dividends_other_growth",
            "taxes",
            "taxes_cagr_7yr",
            "research_development",
            "research_development_cagr_7yr",
            "retained_earnings",
            "total_revenue_cagr_5yr",
            "selling_general_admin",
            "selling_general_admin_cagr_7yr",
            "st_investments",
            "total_common_equity",
            "total_liabilities",
            "ebitda",
            "ebitda_cagr_7yr",
            "wa_diluted_shares_outstanding",
            "accrued_expenses_other",
            "adj_basic_eps",
            "basic_eps_cagr_5yr",
            "adj_basic_eps_cagr_3yr",
            "adj_basic_eps_growth",
            "capex_growth",
            "total_cash_st_investments",
            "cash_from_investing",
            "change_accounts_payables",
            "common_stock",
            "cogs_cagr_5yr",
            "total_current_assets",
            "depreciation_amortization",
            "diluted_eps",
            "diluted_eps_cagr_7yr",
            "adj_diluted_eps_cagr_5yr",
            "dividends_payable",
            "ebit_cagr_3yr",
            "adj_ebit_cagr_5yr",
            "ebit_growth",
            "adj_ebitda_cagr_3yr",
            "adj_ebitda_growth",
            "goodwill_intangibles",
            "gross_profit_cagr_3yr",
            "gross_profit_growth",
            "interest_expense_cagr_5yr",
            "inventory",
            "lt_investments",
            "minority_interest_earnings",
            "minority_interest_earnings_cagr_7yr",
            "total_net_income",
            "total_net_income_cagr_7yr",
            "adj_net_income_cagr_5yr",
            "net_income_common",
            "net_income_common_cagr_7yr",
            "net_working_capital_growth",
            "notes_payable",
            "operating_income_cagr_5yr",
            "other_current_assets",
            "other_payables",
            "pre_tax_income_cagr_3yr",
            "pre_tax_income_growth",
            "preferred_dividends_other_cagr_5yr",
            "preferred_stock",
            "taxes_cagr_3yr",
            "taxes_growth",
            "research_development_cagr_3yr",
            "research_development_growth",
            "total_revenue",
            "total_revenue_cagr_7yr",
            "selling_general_admin_cagr_3yr",
            "selling_general_admin_growth",
            "total_assets",
            "total_debt",
            "total_liabilities_equity",
            "ebitda_cagr_3yr",
            "ebitda_growth",
            "projected_avg_nopat_margin_10yr",
            "projected_avg_ni_margin_5yr",
            "projected_avg_ebitda_margin_10yr",
            "projected_avg_ufcf_margin_5yr",
            "projected_capex_margin",
            "projected_ebit_cagr_5yr",
            "projected_ebitda",
            "projected_ebitda_growth",
            "projected_eps_growth",
            "projected_net_income",
            "projected_net_income_growth",
            "projected_net_working_capital_growth",
            "projected_revenue_cagr_10yr",
            "projected_unlevered_fcf_cagr_10yr",
            "projected_avg_nopat_margin_5yr",
            "projected_avg_ebit_margin_10yr",
            "projected_avg_ebitda_margin_5yr",
            "projected_capex",
            "projected_ebit",
            "projected_ebit_growth",
            "projected_ebitda_cagr_10yr",
            "projected_ebitda_margin",
            "projected_nopat_cagr_10yr",
            "projected_ni_cagr_10yr",
            "projected_net_income_margin",
            "projected_net_working_capital_margin",
            "projected_revenue_cagr_5yr",
            "projected_unlevered_fcf_cagr_5yr",
            "projected_avg_ni_margin_10yr",
            "projected_avg_ebit_margin_5yr",
            "projected_avg_ufcf_margin_10yr",
            "projected_capex_growth",
            "projected_ebit_cagr_10yr",
            "projected_ebit_margin",
            "projected_ebitda_cagr_5yr",
            "projected_eps",
            "projected_nopat_cagr_5yr",
            "projected_ni_cagr_5yr",
            "projected_net_working_capital",
            "projected_revenue",
            "projected_revenue_growth",
            "description",
            "sector",
            "benchmarks",
            "company_name",
            "sector_id",
            "exchange",
            "index",
            "benchmark_ids",
            "ticker"
          )
        ),
        .Names = c("typeMetric", "nameMetric", "nameFinbox"),
        class = c("tbl_df", "tbl", "data.frame"),
        row.names = c(NA,
                      -348L),
        spec = structure(
          list(
            cols = structure(
              list(
                typeMetric = structure(list(), class = c("collector_character",
                                                         "collector")),
                nameMetric = structure(list(), class = c("collector_character",
                                                         "collector")),
                slugMetric = structure(list(), class = c("collector_character",
                                                         "collector"))
              ),
              .Names = c("typeMetric", "nameMetric", "nameFinbox")
            ),
            default = structure(list(), class = c("collector_guess",
                                                  "collector"))
          ),
          .Names = c("cols", "default"),
          class = "col_spec"
        )
      ) -> df

    df %>%
      mutate(isPeriodBased = if_else(
        typeMetric %in% c('Financials', 'Efficency') &
          !typeMetric %>% str_detect('yr'),
        TRUE, FALSE
      ),
      slugFinbox =
        if_else(isPeriodBased,
                str_c(nameFinbox, 'fy', sep = '_'),
                nameFinbox))
  }

.get_dictionary_finbox_names <-
  function() {
    tibble(
      nameFinbox = c(
        "stock_price",
        "market_change_today",
        "market_change_today_pct",
        "year_range_low",
        "year_range_high",
        "analyst_price_target",
        "next_earnings_date",
        "ticker",
        "exchange",
        "name",
        "sector",
        "finbox_id",
        "master_fair_value",
        "stock_price_latest",
        "dividend_yield",
        "market_cap",
        "stock_price_latest_pct",
        "pe_ratio",
        "price_book",
        "debt_to_total_capital",
        "company_name",
        "uid"
      ),
      nameActual = c(
        'priceLast',
        'priceChangeToday',
        'pctPriceChangeToday',
        'priceYearLow',
        'priceYearHigh',
        'priceAnalystTarget',
        'dateEarningsNext',
        "idTicker",
        "slugExchange",
        "nameCompany",
        "sectorCompany",
        "idFinbox",
        "priceFairValue",
        "priceLast",
        "pctDividendYield",
        "amountMarketCapitalizationMillions",
        "pctChangePrice",
        "ratioPriceToEarnings",
        "ratioPriceToBook",
        "pctDebtToCapital",
        "nameCompany",
        "idFinboxUnique"
      )

    )


  }
.get_finbox_base_names <- function() {
  df_finbox <- tibble(
    nameFinbox = c(
      "market_change_today",
      "pct_50_day_moving_average",
      "year_range_high",
      "average_daily_volume",
      "book_value_market_cap",
      "pre_tax_cost_of_debt_mid",
      "stock_price_high",
      "daily_volume",
      "industry_cost_of_equity",
      "analyst_price_target_downside",
      "ev_ebitda",
      "enterprise_value",
      "analyst_target_low",
      "master_fair_value",
      "earnings_yield",
      "market_cap_affo",
      "pe_ratio",
      "peg_ratio",
      "price_sales",
      "stock_price_latest",
      "analyst_price_target_upside",
      "stock_price_latest_pct",
      "year_perc_range_high",
      "year_range_low",
      "ben_graham_formula_upside",
      "master_fair_value_confidence",
      "cost_of_equity_mid",
      "stock_price_low",
      "industry_cost_of_capital",
      "days_to_earnings",
      "master_downside",
      "ev_revenue",
      "analyst_price_target",
      "analyst_target_mean",
      "float_shares",
      "market_cap_book_value",
      "market_cap_revenue",
      "pe_ratio",
      "price_book",
      "short_ratio",
      "volume",
      "master_upside",
      "pct_200_day_moving_average",
      "year_perc_range_low",
      "affo_yield",
      "ben_graham_formula_value",
      "after_tax_cost_of_debt_mid",
      "stock_price_close",
      "stock_price_open",
      "industry_cost_of_debt",
      "dividend_yield",
      "ev_ebit",
      "earnings_yield",
      "analyst_target_high",
      "analyst_target_count",
      "fcf_yield",
      "market_cap_revenue",
      "market_cap",
      "peg_ratio",
      "price_sales",
      "stock_price",
      "master_fair_value_uncertainty",
      "wacc_mid",
      "altman_z_score",
      "current_ratio",
      "financial_leverage",
      "beta",
      "debt_to_equity",
      "interest_coverage",
      "cash_to_total_capital",
      "debt_to_total_capital",
      "quick_ratio",
      "affo",
      "asset_turnover",
      "cogs_margin",
      "days_sales_outstanding",
      "adj_ebit_margin",
      "adj_ebitda_margin",
      "effective_interest_rate",
      "ffo",
      "gross_profit_margin",
      "levered_fcf",
      "minority_interest_earnings_margin",
      "net_income_common_margin",
      "piotroski_score",
      "taxes_margin",
      "roe",
      "ebitda_margin",
      "unlevered_fcf_margin",
      "affo_growth",
      "capex_margin",
      "days_inventory_outstanding",
      "depreciation_amortization_margin",
      "ebitda_less_avg_capex",
      "ebitda_less_avg_capex_margin",
      "effective_tax_rate",
      "ffo_growth",
      "interest_expense_margin",
      "levered_fcf_growth",
      "total_net_income_margin",
      "operating_income_margin",
      "pre_tax_income_margin",
      "research_development_margin",
      "roic",
      "unlevered_fcf",
      "unlevered_roa",
      "affo_margin",
      "cash_conversion_cycle",
      "days_payables_outstanding",
      "ebit_margin",
      "ebitda_less_capex",
      "ebitda_less_capex_margin",
      "fixed_asset_turnover",
      "ffo_margin",
      "inventory_turnover",
      "levered_fcf_margin",
      "adj_net_income_margin",
      "payout_ratio",
      "preferred_dividends_other_margin",
      "roa",
      "selling_general_admin_margin",
      "unlevered_fcf_growth",
      "accounts_payable",
      "add_paid_in_capital",
      "basic_eps",
      "basic_eps_cagr",
      "adj_basic_eps_cagr",
      "cash_at_beginning",
      "capitalized_software",
      "dividends_to_common",
      "cash_from_operations",
      "change_accounts_receivables",
      "cogs",
      "cogs_cagr",
      "total_current_liabilities",
      "depreciation_amortization_growth",
      "diluted_eps_cagr",
      "diluted_eps_growth",
      "adj_diluted_eps_cagr",
      "ebit",
      "adj_ebit_cagr",
      "ebit_cagr",
      "adj_ebit_growth",
      "adj_ebitda_cagr",
      "cash_at_end",
      "goodwill_impairment",
      "gross_profit_cagr",
      "interest_expense",
      "interest_expense_cagr",
      "litigation_expenses",
      "acquisition_expenses",
      "minority_interest_earnings_cagr",
      "minority_interest_earnings_growth",
      "total_net_income_cagr",
      "total_net_income_growth",
      "adj_net_income_cagr",
      "net_income_common_cagr",
      "net_income_common_growth",
      "net_working_capital_margin",
      "operating_income",
      "operating_income_cagr",
      "other_lt_assets",
      "ppe",
      "pre_tax_income_cagr",
      "preferred_dividends_other",
      "preferred_dividends_other_cagr",
      "prepaid_expenses",
      "taxes_cagr",
      "real_estate_investments",
      "research_development_cagr",
      "restructuring_charges",
      "total_revenue_cagr",
      "total_revenue_growth",
      "selling_general_admin_cagr",
      "shares_outstanding",
      "total_change_cash",
      "total_equity",
      "treasury_stock",
      "ebitda_cagr",
      "wa_basic_shares_outstanding",
      "accounts_receivable",
      "asset_writedown",
      "basic_eps_growth",
      "capex",
      "cash_equivalents",
      "cash_from_financing",
      "change_accounts_payables_accrued",
      "change_inventory",
      "cogs_growth",
      "current_debt_cap_leases",
      "adj_diluted_eps",
      "adj_diluted_eps_growth",
      "adj_ebit",
      "adj_ebitda",
      "fx_adjustments",
      "gross_profit",
      "interest_expense_growth",
      "lt_debt",
      "minority_interest",
      "adj_net_income",
      "adj_net_income_growth",
      "net_working_capital",
      "next_earnings_date",
      "operating_income_growth",
      "other_lt_liabilities",
      "pre_tax_income",
      "preferred_dividends_other_growth",
      "taxes",
      "research_development",
      "retained_earnings",
      "selling_general_admin",
      "st_investments",
      "total_common_equity",
      "total_liabilities",
      "ebitda",
      "wa_diluted_shares_outstanding",
      "accrued_expenses_other",
      "adj_basic_eps",
      "adj_basic_eps_growth",
      "capex_growth",
      "total_cash_st_investments",
      "cash_from_investing",
      "change_accounts_payables",
      "common_stock",
      "total_current_assets",
      "depreciation_amortization",
      "diluted_eps",
      "dividends_payable",
      "ebit_growth",
      "adj_ebitda_growth",
      "goodwill_intangibles",
      "gross_profit_growth",
      "inventory",
      "lt_investments",
      "minority_interest_earnings",
      "total_net_income",
      "net_income_common",
      "net_working_capital_growth",
      "notes_payable",
      "other_current_assets",
      "other_payables",
      "pre_tax_income_growth",
      "preferred_stock",
      "taxes_growth",
      "research_development_growth",
      "total_revenue",
      "selling_general_admin_growth",
      "total_assets",
      "total_debt",
      "total_liabilities_equity",
      "ebitda_growth",
      "avg_nopat_margin",
      "avg_ni_margin",
      "avg_ebitda_margin",
      "avg_ufcf_margin",
      "eps_growth",
      "net_income",
      "net_income_growth",
      "revenue_cagr",
      "unlevered_fcf_cagr",
      "avg_ebit_margin",
      "nopat_cagr",
      "ni_cagr",
      "net_income_margin",
      "eps",
      "revenue",
      "revenue_growth",
      "description",
      "sector",
      "benchmarks",
      "company_name",
      "sector_id",
      "exchange",
      "index",
      "benchmark_ids",
      "ticker",
      'uid',
      'industry'
    ),
    nameActual = c(
      "priceChangeToday",
      "pct50DayMovingAverage",
      "priceHighYear",
      "volumeAverageDaily",
      "pctBookMarketCap",
      "pctPreTaxDebtCostMid",
      "priceHighDay",
      "volumeDaily",
      "pctEquityCostIndustry",
      "pctTargetAnalystDownside",
      "ratioEnterpriseValueEbitda",
      "amountEnterpriseValue",
      "priceAnalystTargetLow",
      "priceFairValueMaster",
      "pctEarningsYield",
      "pctMarketCapAffo",
      "ratioPriceToEarnings",
      "ratioPriceEarningsGrowth",
      "ratioPriceSales",
      "priceCurrent",
      "pctAnalystTargetHigh",
      "pctChangeToday",
      "pctPrice52WeekHigh",
      "price52WeekLow",
      "pctBenGrahamUpside",
      "groupFairValueConfidence",
      "pctEquityCost",
      "priceLowDay",
      "pctCostCapitalIndustryDamodaran",
      "countDaysToEarnings",
      "pctDownsideFinboxMasterModel",
      "ratioEnterpriseValueRevenue",
      "priceTargetAnalyst",
      "pctPriceAnalstTargetMean",
      "amountSharesFloat",
      "ratioMarketCapBookValue",
      "ratioMarketCapRevenue",
      "ratioPriceEarnings",
      "ratioPriceBook",
      "ratioShort",
      "volumeDay",
      "pctUpsideFinboxMasterModel",
      "pctPrice200DayMovingAverage",
      "pctPrice52WeekLow",
      "pctAffoYield",
      "priceBenGrahamFormula",
      "pctAfterTaxDebtCost",
      "priceClose",
      "priceOpen",
      "pctCostofDebtIndustry",
      "pctDividendYield",
      "ratioEnterpriseValueEbit",
      "pctEarningsYield",
      "priceAnalystTargetHighest",
      "countAnalysts",
      "pctFreeCashFlowYield",
      "ratioMarketCapRevenue",
      "amountMarketCap",
      "ratioPriceEarningsGrowth",
      "ratioPriceSales",
      "priceLast",
      "groupUncertaintyFinBox",
      "pctWACC",
      "ratioAltman",
      "ratioCurrentAssetsLiabilities",
      "ratioFinancialLeverage",
      "ratioBeta",
      "ratioDebtToEquity",
      "ratioInterestCoverage",
      "ratioCashToCapitalTotal",
      "ratioDebtCapitalTotal",
      "ratioQuick",
      "amountAffo",
      "amountAssetTurnOver",
      "pctCOGS",
      "ratioDaysSalesOutsanding",
      "pctEbitMarginAdjusted",
      "pctEbitdaMarginAdjusted",
      "pctInterestRateEffective",
      "amountFfo",
      "pctProfitMarginGross",
      "amountLeveredFreeCashFlow",
      "pctMinorityInterest",
      "pctNetIncomeMargin",
      "scorePiotroski",
      "pctTaxes",
      "pctReturnOnEquity",
      "pctEbitda",
      "pctFreeCashFlow",
      "pctAffoGrowth",
      "pctCapex",
      "ratioDaysInventoryOutstanding",
      "pctDepreciationAmortization",
      "amountEbtidaLessAvgCapex3yr",
      "pctEbtidaLessAvgCapex3yr",
      "pctTaxRateEffective",
      "pctFFOGrowth",
      "pctInterestExpenseMargin",
      "pctLeveredFreeCashFlowGrowth",
      "pctTotalNetIncomeMargin",
      "pctOperatingIncomeMargin",
      "pctPreTaxIncome",
      "pctResearchDevelopmentMargin",
      "pctReturnOnInvestedCapital",
      "amountUnleveredFreeCashFlow",
      "pctUnleveredReturnOnAssets",
      "pctAffoMargin",
      "ratioCashConversaion",
      "ratioDaysPayableOutstanding",
      "pctEbitMargin",
      "amountEbitdaLessCapex",
      "pctEbitdaLessCapex",
      "ratioFixedAssetTurnover",
      "fctFFOMargin",
      "ratioInventoryTurnover",
      "pctFreeCashFlowMargin",
      "pctNetIncomeAdjustedMargin",
      "ratioDividendPayout",
      "pctPreferredDividendOtherMargin",
      "pctReturnOnAssets",
      "pctSellingGeneralAdministrativeMargin",
      "pctUnleveredFreeCashFlowGrowth",
      "amountAccountsPayable",
      "amountAdditionPaidInCapial",
      "pershareEarnings",
      "pctEPSCagr",
      "pctEPSCagr",
      "amountCashBegining",
      "amountCapitalzedSoftware",
      "amountDividendsToShareholders",
      "amountCashFromOperations",
      "amountChangeAccountsReceivable",
      "amountCostOfGoodsSOld",
      "pctCostOfGoodsSoldCAGR",
      "amountLiabilitiesCurrentTotal",
      "pctDepreciationAmortizationGrowth",
      "pctEPSDilutedCAGR",
      "pctEPSGrowth",
      "pctEPSDilutdCAGR",
      "amountEbit",
      "pctEbitAdjustedCAGR",
      "pctEbitCAGR",
      "pctEbitAdjustedGrowth",
      "pctEbitdaAdjustedCAGR",
      "amountCashEnd",
      "amountGoodwillImapirment",
      "pctGrossProfitCAGR",
      "amountInterestExpense",
      "pctInterestExpenseCAGR",
      "amountLitigationExpense",
      "amountAcquisitionExpense",
      "pctMinorityInterestCAGR",
      "pctMinorityIntrestEarningsGrowth",
      "pctNetIncomeCAGR",
      "pctNetIncomeGrowth",
      "pctNetIncomeAdjustedCAGR",
      "pctNetIncomeCommonCAGR",
      "pctNetIncomeCommonGrowth",
      "pctWorkingCapitalMargin",
      "amountOperatingIncome",
      "pctOperatingIncomeCAGR",
      "amountOtherLongTermAssets",
      "amountPlantPropertyEquipment",
      "pctPreTaxIncomeCAGR",
      "amountPreferredDividendsOther",
      "pctPreferredDividendsOtherCAGR",
      "amountPrepaidExpense",
      "pctTaxesCAGR",
      "amountRealEstateInvestment",
      "pctResearchDevelopmentCAGR",
      "amountRestructuringCharges",
      "pctTotalRevenueCAGR",
      "pctTotalRevenueGrowth",
      "pctSellingGeneralAdministrativeCAGR",
      "sharesOutstanding",
      "amountChangeCash",
      "amountEquityTotal",
      "amountTreasuryStock",
      "pctEbitdaCAGR",
      "sharesOutstandingBasic",
      "amountAccountsReceivable",
      "amountAssetWritedown",
      "pctEPSBasicGrowth",
      "amountCAPEX",
      "amountCashAndEquivalents",
      "amountCashFromFinancing",
      "amountChangeInAccountsPayableAccrued",
      "amountChangeInInventory",
      "pctCostofGoodsSoldGrowth",
      "amountDebtCapitalizedLeases",
      "pershareEarningsDilutedAdjusted",
      "pctEarningsDilutedGrowth",
      "amountEbitAdjusted",
      "amountEbitdaAdjusted",
      "amountForexAdjustment",
      "amountGrossProfit",
      "pctInterestExpenseGrowth",
      "amountLongTermDebt",
      "amountMinorityInterest",
      "amountNetIncomeAdjusted",
      "pctNetIncomeAdjustedGrowth",
      "amountWorkingCapitalNet",
      "dateNextEarnings",
      "pctOperatingIncomeGrowth",
      "amountLiabilitiesOtherLongTerm",
      "amountIncomePreTax",
      "pctPreferredDividendsOtherGrowth",
      "amountTaxes",
      "amountResearchAndDevelopment",
      "amountRetainedEarnings",
      "amountSellingGeneralAdministrative",
      "amountInvestmentsShortTerm",
      "amountCommonEquity",
      "amountTotalLiabilities",
      "amountEbitda",
      "sharesOutstandingDilutedWeightedAverage",
      "amountAccruedExpenseOther",
      "pershareEPSBasicAdjusted",
      "pctEPSBasicAdjustedGrowth",
      "pctCapexGrowth",
      "amountCashShortTermInvestment",
      "amountCashFromInvesting",
      "amountChangeAccountsPayable",
      "amountCommonStock",
      "amountAssetsCurrent",
      "amountDepreciationAmortization",
      "pershareEarningsDiluted",
      "amountDividendsPayable",
      "pctEbitGrowth",
      "pctEbitdaAdjustedGrowth",
      "amountGoodwillIntagibles",
      "pctGrossProfitGrowth",
      "amountInventory",
      "amountInvestmentsLongerm",
      "amountMinorityInterestEarnings",
      "amountTotalNetIncome",
      "amountNetIncomeCommon",
      "pctNetWorkingCapitalGrowth",
      "amountNotesPayable",
      "amountAssetsCurrentOther",
      "amountPayableOther",
      "pctPreTaxIncomeGrowth",
      "amountPreferredStock",
      "pctTaxesGrowth",
      "pctResearchDevelopmentGrowth",
      "amountTotalRevenue",
      "pctSellingGeneralAdministrativeGrowth",
      "amountAssetsTotal",
      "amountDebtTotal",
      "amountLiabilitiesEquityTotal",
      "pctEbitdaGrowth",
      "pctNopatMarginMean",
      "pctNetIncomeMarginMean",
      "pctEbitdaMarginMean",
      "pctUnleveredFreeCashFlowMarginMean",
      "pctEPSGrowth",
      "amountNetIncome",
      "pctNetIncomeGrowth",
      "pctRevenueCAGR",
      "pctUnleveredFreeCashFlowCAGR",
      "pctEbitMarginMean",
      "pctNopatCAGR",
      "pctNetIncomeCAGR",
      "pctNetIncomeMargin",
      "pershareEarnings",
      "amountRevenue",
      "pctRevenueGrowth",
      "descriptionCompany",
      "sectorCompany",
      "idBenchmarks",
      "nameCompany",
      "idSector",
      "slugExchange",
      "idIndicies",
      "idTickersBenchmark",
      "idTicker",
      'idUnique',
      'industryCompany'
    ),
    slugFinboxBase =
      c(
        "market_change_today",
        "pct_50_day_moving_average",
        "year_range_high",
        "average_daily_volume",
        "book_value_market_cap",
        "pre_tax_cost_of_debt_mid",
        "stock_price_high",
        "daily_volume",
        "industry_cost_of_equity",
        "analyst_price_target_downside",
        "ev_ebitda_fwd",
        "enterprise_value_fy",
        "analyst_target_low",
        "master_fair_value",
        "earnings_yield",
        "market_cap_affo_ltm",
        "pe_ratio_fwd",
        "peg_ratio_fwd",
        "price_sales_ltm",
        "stock_price_latest",
        "analyst_price_target_upside",
        "stock_price_latest_pct",
        "year_perc_range_high",
        "year_range_low",
        "ben_graham_formula_upside",
        "master_fair_value_confidence",
        "cost_of_equity_mid",
        "stock_price_low",
        "industry_cost_of_capital",
        "days_to_earnings",
        "master_downside",
        "ev_revenue_fwd",
        "analyst_price_target",
        "analyst_target_mean",
        "float_shares",
        "market_cap_book_value",
        "market_cap_revenue_ltm",
        "pe_ratio_ltm",
        "price_book_ltm",
        "short_ratio",
        "volume",
        "master_upside",
        "pct_200_day_moving_average",
        "year_perc_range_low",
        "affo_yield_ltm",
        "ben_graham_formula_value",
        "after_tax_cost_of_debt_mid",
        "stock_price_close",
        "stock_price_open",
        "industry_cost_of_debt",
        "dividend_yield",
        "ev_ebit_fwd",
        "earnings_yield_ltm",
        "analyst_target_high",
        "analyst_target_count",
        "fcf_yield_ltm",
        "market_cap_revenue_fwd",
        "market_cap",
        "peg_ratio_ltm",
        "price_sales_fwd",
        "stock_price",
        "master_fair_value_uncertainty",
        "wacc_mid",
        "altman_z_score",
        "current_ratio",
        "financial_leverage",
        "beta",
        "debt_to_equity",
        "interest_coverage",
        "cash_to_total_capital",
        "debt_to_total_capital",
        "quick_ratio",
        "affo_fy",
        "asset_turnover_fy",
        "cogs_margin_fy",
        "days_sales_outstanding_fy",
        "adj_ebit_margin_fy",
        "adj_ebitda_margin_fy",
        "effective_interest_rate_fy",
        "ffo_fy",
        "gross_profit_margin_fy",
        "levered_fcf_fy",
        "minority_interest_earnings_margin_fy",
        "net_income_common_margin_fy",
        "piotroski_score",
        "taxes_margin_fy",
        "roe_fy",
        "ebitda_margin_fy",
        "unlevered_fcf_margin_fy",
        "affo_growth_fy",
        "capex_margin_fy",
        "days_inventory_outstanding_fy",
        "depreciation_amortization_margin_fy",
        "ebitda_less_3yr_avg_capex_fy",
        "ebitda_less_3yr_avg_capex_margin_fy",
        "effective_tax_rate_fy",
        "ffo_growth_fy",
        "interest_expense_margin_fy",
        "levered_fcf_growth_fy",
        "total_net_income_margin_fy",
        "operating_income_margin_fy",
        "pre_tax_income_margin_fy",
        "research_development_margin_fy",
        "roic_fy",
        "unlevered_fcf_fy",
        "unlevered_roa_fy",
        "affo_margin_fy",
        "cash_conversion_cycle_fy",
        "days_payables_outstanding_fy",
        "ebit_margin_fy",
        "ebitda_less_capex_fy",
        "ebitda_less_capex_margin_fy",
        "fixed_asset_turnover_fy",
        "ffo_margin_fy",
        "inventory_turnover_fy",
        "levered_fcf_margin_fy",
        "adj_net_income_margin_fy",
        "payout_ratio_fy",
        "preferred_dividends_other_margin_fy",
        "roa_fy",
        "selling_general_admin_margin_fy",
        "unlevered_fcf_growth_fy",
        "accounts_payable_fy",
        "add_paid_in_capital_fy",
        "basic_eps_fy",
        "basic_eps_cagr_7yr",
        "adj_basic_eps_cagr_5yr",
        "cash_at_beginning_fy",
        "capitalized_software_fy",
        "dividends_to_common_fy",
        "cash_from_operations_fy",
        "change_accounts_receivables_fy",
        "cogs_fy",
        "cogs_cagr_7yr",
        "total_current_liabilities_fy",
        "depreciation_amortization_growth_fy",
        "diluted_eps_cagr_3yr",
        "diluted_eps_growth_fy",
        "adj_diluted_eps_cagr_7yr",
        "ebit_fy",
        "adj_ebit_cagr_3yr",
        "ebit_cagr_7yr",
        "adj_ebit_growth_fy",
        "adj_ebitda_cagr_5yr",
        "cash_at_end_fy",
        "goodwill_impairment_fy",
        "gross_profit_cagr_5y",
        "interest_expense_fy",
        "interest_expense_cagr_7yr",
        "litigation_expenses_fy",
        "acquisition_expenses_fy",
        "minority_interest_earnings_cagr_3yr",
        "minority_interest_earnings_growth_fy",
        "total_net_income_cagr_3yr",
        "total_net_income_growth_fy",
        "adj_net_income_cagr_7yr",
        "net_income_common_cagr_3yr",
        "net_income_common_growth_fy",
        "net_working_capital_margin_fy",
        "operating_income_fy",
        "operating_income_cagr_7yr",
        "other_lt_assets_fy",
        "ppe_fy",
        "pre_tax_income_cagr_5yr",
        "preferred_dividends_other_fy",
        "preferred_dividends_other_cagr_7yr",
        "prepaid_expenses_fy",
        "taxes_cagr_5yr",
        "real_estate_investments_fy",
        "research_development_cagr_5yr",
        "restructuring_charges_fy",
        "total_revenue_cagr_3yr",
        "total_revenue_growth_fy",
        "selling_general_admin_cagr_5yr",
        "shares_outstanding_fy",
        "total_change_cash_fy",
        "total_equity_fy",
        "treasury_stock_fy",
        "ebitda_cagr_5yr",
        "wa_basic_shares_outstanding_fy",
        "accounts_receivable_fy",
        "asset_writedown_fy",
        "basic_eps_growth_fy",
        "capex_fy",
        "cash_equivalents_fy",
        "cash_from_financing_fy",
        "change_accounts_payables_accrued_fy",
        "change_inventory_fy",
        "cogs_growth_fy",
        "current_debt_cap_leases_fy",
        "adj_diluted_eps_fy",
        "adj_diluted_eps_growth_fy",
        "adj_ebit_fy",
        "adj_ebitda_fy",
        "fx_adjustments_fy",
        "gross_profit_fy",
        "interest_expense_growth_fy",
        "lt_debt_fy",
        "minority_interest_fy",
        "adj_net_income_fy",
        "adj_net_income_growth_fy",
        "net_working_capital_fy",
        "next_earnings_date",
        "operating_income_growth_fy",
        "other_lt_liabilities_fy",
        "pre_tax_income_fy",
        "preferred_dividends_other_growth_fy",
        "taxes_fy",
        "research_development_fy",
        "retained_earnings_fy",
        "selling_general_admin_fy",
        "st_investments_fy",
        "total_common_equity_fy",
        "total_liabilities_fy",
        "ebitda_fy",
        "wa_diluted_shares_outstanding_fy",
        "accrued_expenses_other_fy",
        "adj_basic_eps_fy",
        "adj_basic_eps_growth_fy",
        "capex_growth_fy",
        "total_cash_st_investments_fy",
        "cash_from_investing_fy",
        "change_accounts_payables_fy",
        "common_stock_fy",
        "total_current_assets_fy",
        "depreciation_amortization_fy",
        "diluted_eps_fy",
        "dividends_payable_fy",
        "ebit_growth_fy",
        "adj_ebitda_growth_fy",
        "goodwill_intangibles_fy",
        "gross_profit_growth_fy",
        "inventory_fy",
        "lt_investments_fy",
        "minority_interest_earnings_fy",
        "total_net_income_fy",
        "net_income_common_fy",
        "net_working_capital_growth_fy",
        "notes_payable_fy",
        "other_current_assets_fy",
        "other_payables_fy",
        "pre_tax_income_growth_fy",
        "preferred_stock_fy",
        "taxes_growth_fy",
        "research_development_growth_fy",
        "total_revenue_fy",
        "selling_general_admin_growth_fy",
        "total_assets_fy",
        "total_debt_fy",
        "total_liabilities_equity_fy",
        "ebitda_growth_fy",
        "projected_avg_nopat_margin_10yr",
        "projected_avg_ni_margin_5yr",
        "projected_avg_ebitda_margin_10yr",
        "projected_avg_ufcf_margin_5yr",
        "projected_eps_growth",
        "projected_net_income",
        "projected_net_income_growth",
        "projected_revenue_cagr_10yr",
        "projected_unlevered_fcf_cagr_10yr",
        "projected_avg_ebit_margin_10yr",
        "projected_nopat_cagr_10yr",
        "projected_ni_cagr_10yr",
        "projected_net_income_margin",
        "projected_eps",
        "projected_revenue",
        "projected_revenue_growth",
        "description",
        "sector",
        "benchmarks",
        "company_name",
        "sector_id",
        "exchange",
        "index",
        "benchmark_ids",
        "ticker",
        'uid',
        'industry'
      )

  )

  df_finbox <-
    df_finbox %>%
    mutate(
      isMultiple = slugFinboxBase %>% str_detect("_ltm|_fwd"),
      isMultiple = if_else(nameFinbox %>% str_detect("_ltm"), FALSE, isMultiple),
      isPeriod = slugFinboxBase %>% str_detect("_fy|_fq"),
      isPct = slugFinboxBase %>% str_detect("_yr"),
    )
  df_finbox
}

.parse_metrics <- function(metrics,
                          multiple_types,
                          period_types) {
  if (!'df_finbox_names' %>% exists()) {
    df_finbox_names <-
      .get_finbox_base_names()

  }
  slug_metrics <- c()
  base_names <- c('ticker', 'description', 'company_name', 'industry', 'sector')

  slug_metrics <- slug_metrics %>% append(base_names)

  has_metrics <-
    df_finbox_names %>%
    filter(isMultiple) %>%
    filter(nameFinbox %in% metrics) %>% nrow() > 0

  if (has_metrics) {
    multiple_metrics <-
      df_finbox_names %>%
      filter(isMultiple) %>%
      filter(nameFinbox %in% metrics) %>%
      pull(nameFinbox)

    metrics_names <-
      expand.grid(metric = multiple_metrics,
                  type = multiple_types,
                  stringsAsFactors = F) %>%
      as_tibble() %>%
      tidyr::unite(typeMetric, metric, type, sep = '_') %>%
      pull(typeMetric)

    slug_metrics <-
      slug_metrics %>%
      append(metrics_names)
  }

  has_periods <-
    df_finbox_names %>%
    filter(isPeriod) %>%
    filter(nameFinbox %in% metrics) %>% nrow() > 0


  if (has_periods) {
    period_metrics <-
      df_finbox_names %>%
      filter(isPeriod) %>%
      filter(nameFinbox %in% metrics) %>%
      pull(nameFinbox)

    periods <-
      expand.grid(metric = period_metrics,
                  period = period_types,
                  stringsAsFactors = F) %>%
      as_tibble() %>%
      tidyr::unite(periodMetric, metric, period, sep = '_') %>%
      pull(periodMetric)

    slug_metrics <-
      slug_metrics %>%
      append(periods)
  }


  has_other <-
    df_finbox_names %>%
    filter(!isPeriod) %>%
    filter(!isMultiple) %>%
    filter(nameFinbox %in% metrics) %>%
    nrow() > 0

  if (has_other) {
    other_names <-
      df_finbox_names %>%
      filter(!isPeriod) %>%
      filter(!isMultiple) %>%
      filter(nameFinbox %in% metrics) %>%
      pull(nameFinbox)

    slug_metrics <-
      slug_metrics %>%
      append(other_names)
  }

  slug_metrics <-
    slug_metrics %>% unique()
  slug_metrics
}

.parse_actual_names <-
  function(df) {

    if (!'df_names' %>% exists()) {
      df_names <-
        .get_dictionary_finbox_names()
    }

    actual_names <-
      names(df) %>%
      map_chr(function(x){
        no_match <-
          df_names %>%
          filter(x %in% nameFinbox) %>% nrow() == 0

        if (no_match) {
          return(x)
        }

        df_names %>% filter(nameFinbox == x) %>% pull(nameActual)
      })
    actual_names
  }

.clean_names <-
  function(data) {
    data <-
      data %>%
      mutate_at(data %>% dplyr::select(dplyr::matches("^pct[A-Z]")) %>% names(),
                funs(. %>% formattable::percent(digits = 2))) %>%
      mutate_at(data %>% dplyr::select(dplyr::matches("^ratio[A-Z]")) %>% names(),
                funs(. %>% formattable::digits(digits = 5))) %>%
      mutate_at(data %>% dplyr::select(dplyr::matches("^price[A-Z]|pershare")) %>% names(),
                funs(. %>% formattable::currency(digits = 4))) %>%
      mutate_at(
        data %>% dplyr::select(dplyr::matches("^amount[A-Z]")) %>% names(),
        funs(. * 1000000 %>% formattable::currency(digits = 0))
      )

    if (data %>% tibble::has_name('idIndicies')) {
      df_indicies <-
        seq_along(data$idIndicies) %>%
        future_map_dfr(function(x){
          if (data$idIndicies[x] %>% flatten_chr() %>% length() == 0) {
            return(tibble(idRow = x, nameIndicies = NA))
          }
          indicies <- data$idIndicies[x] %>% flatten_chr() %>% str_c(collapse = ', ')
          tibble(idRow = x, nameIndicies = indicies)
        })

      data <-
        data %>%
        mutate(idRow = 1:n()) %>%
        left_join(df_indicies) %>%
        dplyr::select(-idIndicies) %>%
        dplyr::select(-idRow) %>%
        suppressMessages()
    }

    if (data %>% tibble::has_name('idBenchmarks')) {
      df_benchmarks <-
        seq_along(data$idBenchmarks) %>%
        future_map_dfr(function(x){
          if (data$idBenchmarks[x] %>% flatten_chr() %>% length() == 0) {
            return(tibble(idRow = x, idTickersBenchmark = NA))
          }
          benchmarks <- data$idBenchmarks[x] %>% flatten_chr() %>% str_c(collapse = ', ')

          tibble(idRow = x, idTickersBenchmark = benchmarks)
        })

      data <-
        data %>%
        mutate(idRow = 1:n()) %>%
        left_join(df_benchmarks) %>%
        dplyr::select(-idBenchmarks) %>%
        dplyr::select(-idRow) %>%
        suppressMessages()
    }

    data <-
      data %>%
      dplyr::select(
        one_of(
          "idTicker",
          "nameCompany",
          "industryCompany",
          "descriptionCompany",
          "sectorCompany",
          "nameIndicies",
          "idTickersBenchmark"
        ),
        everything()
      ) %>%
      suppressWarnings()

    data
  }

# companies ---------------------------------------------------------------

# https://api.finbox.io/beta/companies

#' FinboxIO Companies
#'
#' Returns companies with FinBOXIO data
#'
#' @param return_message if \code{TRUE} return a message
#'
#' @return
#' @export
#' @import glue dplyr purrr stringr
#' @importFrom jsonlite fromJSON
#'
#' @examples
#' dictionary_finbox_companies()
dictionary_finbox_companies <-
  function(return_message = TRUE) {
    data <-
      "https://api.finbox.io/beta/companies" %>%
      jsonlite::fromJSON(flatten = TRUE) %>%
      .$data %>%
      as_tibble() %>%
      purrr::set_names(c(
        'idTicker',
        'slugExchange',
        'nameCompany',
        'sectorCompany',
        'idFinbox'
      )) %>%
      mutate(
        idExchange =
          case_when(
            slugExchange %>% str_detect("NYSE") ~ "NYSE",
            slugExchange %>% str_detect("Nasdaq") ~ "NASDAQ",
            slugExchange %>% str_detect("Nasdaq") ~ "OTC"

          )
      ) %>%
      dplyr::select(idExchange, slugExchange, idTicker, nameCompany, everything()) %>%
      mutate_all(funs(. %>% str_replace_all('\\ , ', '\\, ') %>% str_trim())) %>%
      arrange(idExchange, slugExchange, sectorCompany, idTicker) %>%
      suppressWarnings()

    if (return_message) {
      glue::glue("Acquired {nrow(data)} searchable companies from finbox.io") %>%
        cat(fill = T)
    }
    gc()
    data
  }

# metrics -----------------------------------------------------------------

# https://api.finbox.io/beta/metrics

#' Get Searchable FinBOX metrics
#'
#' @return
#' @export
#' @import purrr dplyr stringr
#' @importFrom jsonlite fromJSON
#' @examples
#' dictionary_finbox_metrics()
dictionary_finbox_metrics <-
  function() {
    data <-
      "https://api.finbox.io/beta/metrics" %>%
      jsonlite::fromJSON(simplifyDataFrame = TRUE)

    df <-
      data$data[c("id", "datatype", "default_period")] %>%
      as_tibble() %>%
      purrr::set_names(c('nameFinbox', 'typeData', 'periodDefault')) %>%
      mutate(idRow = 1:n())

    df_periods <-
      seq_along(data$data$periods) %>%
      future_map_dfr(function(x) {
        null_value <-
          data$data$periods[[x]] %>% purrr::is_null()
        if (null_value) {
          return(tibble())
        }

        periods <-
          data$data$periods[[x]] %>%
          stringr::str_c(collapse = ', ')

        tibble(idRow = x, periodData = periods)
      })

    df <-
      df %>%
      left_join(df_periods) %>%
      suppressWarnings() %>%
      dplyr::select(-idRow) %>%
      suppressMessages()

    df_metrics <- .finbox_metric_names()

    df <-
      df %>%
      mutate(slugMetric = if_else(
        periodData %>% is.na(),
        nameFinbox,
        stringr::str_c(nameFinbox, periodDefault, sep = "_") %>% str_to_lower()
      ))

    df
  }

# screener ----------------------------------------------------------------

metrics <-
  c(
    'sector',
    'company_name',
    'ticker',
    'exchange',
    'description',
    'stock_price_latest',
    'stock_price_latest_pct',
    'market_change_today',
    'year_range_low',
    'year_range_high',
    'analyst_price_target',
    'master_fair_value',
    'market_cap',
    'price_book_ltm',
    'pe_ratio_ltm',
    'debt_to_total_capital',
    'dividend_yield',
    'ebitda'
  )

# {base}/{tickers}/{metrics}
# https://screener.finbox.io/companies/AAPL,DIS,FB,JNJ,MSFT,V,GIS,TWTR,FARM,MCD,NKE,TGT,VLO/company_name,ticker,stock_price_latest,stock_price_latest_pct,market_change_today,year_range_low,year_range_high,analyst_price_target,master_fair_value,market_cap,price_book_ltm,pe_ratio_ltm,debt_to_total_capital,dividend_yield

#' finbox ticker metrics
#'
#' @param tickers
#' @param metrics
#' @param multiple_type
#' @param period_types
#' @param return_message
#'
#' @return
#' @export
#' @import glue purrr curl rvest jsonlite dplyr anytime reticulate lubridate stringr
#' @examples
tickers_metrics <-
  function(tickers = c('AAPL', 'NFLX', "FB", "GOOG", "TSLA", "VNO"),
         metrics = c("total_current_assets", "total_net_income_margin", "total_net_income", "pre_tax_income_cagr",
                     "cash_from_operations", "stock_price_open", "operating_income_margin",
                     "total_current_liabilities", "avg_ebit_margin", "after_tax_cost_of_debt_mid"),
         multiple_types = c('ltm', 'fwd'),
         period_types = c('fy', 'fq', 'ytd'),
         return_message = TRUE) {
  base <-
    'https://screener.finbox.io/companies'

  ticker_slugs <-
    tickers %>% str_c(collapse = ',')

  slug_metrics <-
    .parse_metrics(metrics = metrics,
                  multiple_types = multiple_types,
                  period_types = period_types)

  metric_slugs <-
    slug_metrics %>%
    str_c(collapse = ',')

  url <-
    glue::glue("{base}/{ticker_slugs}/{metric_slugs}")

  data <-
    url %>%
    readr::read_lines() %>%
    jsonlite::fromJSON(simplifyDataFrame = TRUE) %>%
    as_tibble()

  df_names <-
    tibble(slugFinboxBase = names(data)) %>%
    mutate(
      periodFinbox =
        case_when(
          slugFinboxBase %>% str_detect("_ltm") ~ "LTM",
          slugFinboxBase %>% str_detect("_ytd") ~ "YTD",
          slugFinboxBase %>% str_detect("_fwd") ~ "FWD",
          slugFinboxBase %>% str_detect("_yr") ~ "YR",
          slugFinboxBase %>% str_detect("_fy") ~ "FY"
        ),
      nameBase = slugFinboxBase %>% str_replace("_ltm|_ytd|_yr|_fwd|_fy", '')
    )

  df_fb_names <-
    .get_finbox_base_names()

  actual_names <-
    1:nrow(df_names) %>%
    map_chr(function(x){
      row_name <-
        df_names$nameBase[[x]]
      name_actual <-
        df_fb_names %>%
        filter(nameFinbox == row_name) %>%
        pull(nameActual) %>%
        unique() %>%
        .[1]

      if (!df_names$periodFinbox[[x]] %>% is.na()) {
        period <-
          df_names$periodFinbox[[x]] %>% unique() %>% .[1]
        name_actual <-
          str_c(name_actual, period, sep = '')
      }
      name_actual
    })

  data <-
    data %>%
    purrr::set_names(actual_names)


  data <-
    data %>%
    .clean_names()
  gc()
  data
}


# explorer ----------------------------------------------------------------

# {https://finbox.io/{idTicker}/explorer/{slugMetric}}



# tickers -----------------------------------------------------------------

.get_ticker_information <- function(ticker = "VNO") {
  curl_text <-
    "curl 'https://api.finbox.io/v2/query?raw=true' -H 'Pragma: no-cache' -H 'Origin: https://finbox.io' -H 'Accept-Encoding: gzip, deflate, br' -H 'Accept-Language: en-US,en;q=0.8' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_5) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/60.0.3112.24 Safari/537.36' -H 'Content-Type: application/json' -H 'Accept: application/json' -H 'Cache-Control: no-cache' -H 'Referer: https://finbox.io/VNO/models/historical-10yr' -H 'Cookie: _gat_UA-52372956-2=1; _lorid=80573-1497191056992-ee322de3d89c6948; _lo_v=1; _loups=1-0; intercom-id-emiw4mmr=f08d4c83-c1d5-4d83-87c0-0c205e69cdc4; lo_session_in=1; _ga=GA1.2.478469300.1497191057; _gid=GA1.2.501285166.1497191057; _lo_u=1' -H 'Connection: keep-alive' -H 'DNT: 1' --data-binary '{\"query\":\"\\n    query load_company ($ticker: String!) {\\n      company (ticker: $ticker) {\\n        \\n  name\\n  short_name\\n  ticker\\n  sector\\n  industry\\n  country\\n  exchange\\n  last_reported: last_period_end_date\\n  description: short_description\\n  index\\n  similar: benchmarks\\n  enabled\\n  stats\\n  financials\\n  \\nfinql {\\n  stock_price\\n  market_change_today\\n  market_change_today_pct\\n  year_range_low\\n  year_range_high\\n  analyst_price_target\\n  next_earnings_date\\n}\\n  \\nfair_value {\\n  market\\n  analyst_target\\n  ranges\\n  averages\\n  confidence\\n  models\\n}\\n  \\ntemplates {\\n  _id\\n  slug\\n  build_date\\n  name\\n  verified\\n  category\\n  checks_passed\\n}\\n  \\nnews {\\n  title\\n  link\\n  published\\n  host\\n}\\n\\n        \\n\\n        \\n\\n      }\\n    }\\n  \",\"variables\":{\"ticker\":\"ticker_name\"}}' --compressed"

  curl_text <-
    curl_text %>%
    stringr::str_replace_all("ticker_name", ticker)


  json_data <-
    system(curl_text, intern = TRUE, ignore.stderr = TRUE) %>%
    suppressMessages() %>%
    fromJSON()

  .parse_ticker_json_data_safe <-
    purrr::possibly(.parse_ticker_json_data, tibble())
  all_data <-
    json_data %>%
    .parse_ticker_json_data_safe()

  all_data <-
    all_data %>%
    mutate(idTicker = ticker) %>%
    dplyr::select(idTicker, everything())

  all_data
}

.parse_ticker_json_data <-
  function(json_data) {
    data <-
      json_data$data$company

    parts <-
      data %>% names()

    df_names <-
      .get_dictionary_finbox_names()


    df_classes <-
      data %>% future_map_dfr(class) %>%
      tidyr::gather(namePart, classPart) %>%
      mutate(idColumn = 1:n())

    df_classes <-
      df_classes %>%
      filter(!classPart == "NULL")

    char_cols <-
      df_classes %>%
      filter(classPart %in% c('character')) %>%
      pull(namePart)

    df_description <-
      seq_along(char_cols) %>%
      future_map_dfr(function(x) {
        nameFinbox <- char_cols[x]
        value <-
          data[nameFinbox] %>% flatten_chr()

        if (value %>% length() > 1) {
          value <- value %>% stringr::str_c(collapse = ', ')
        }

        tibble(idColumn = x, nameFinbox, value)
      }) %>%
      suppressWarnings()

    col_order <-
      df_description$nameFinbox

    df_description <-
      df_description %>%
      dplyr::select(-idColumn) %>%
      tidyr::spread(nameFinbox, value) %>%
      dplyr::select(one_of(col_order))

    df_description <-
      df_description %>%
      dplyr::select(-one_of("ticker"))

    if (df_description %>% ncol() == 8) {
      df_description <-
        df_description %>%
        purrr::set_names(
          c(
            'nameCompany',
            'nameCompanyShort',
            'sectorCompany',
            'industryCompany',
            'slugExchange',
            'dateEarningsLast',
            'descriptionCompany',
            'idTickersSimilar'
          )
        )

      df_description <-
        df_description %>%
        mutate(
          dateEarningsLast = dateEarningsLast %>% substr(4, nchar(dateEarningsLast)),
          dateEarningsLast = dateEarningsLast %>% substr(1, 12) %>% lubridate::mdy(),
          idExchange =
            case_when(
              slugExchange %>% str_detect("NYSE") ~ "NYSE",
              slugExchange %>% str_detect("Nasdaq") ~ "NASDAQ",
              slugExchange %>% str_detect("Nasdaq") ~ "OTC"

            )
        )
    }


    list_cols <-
      df_classes %>%
      filter(classPart %in% c('list')) %>%
      pull(namePart)

    df_lists <-
      seq_along(list_cols) %>%
      future_map_dfr(function(x){
        nameData <-
          list_cols[[x]]
        list_data <-
          data[nameData][[nameData]]

        list_parts <-
          list_data %>% names()

        if (nameData == 'finql') {
          df <-
            list_data %>% flatten_df()

          actual_names <-
            .parse_actual_names(df = df)

          df <-
            df %>%
            purrr::set_names(actual_names)

          data <- tibble(nameTable = 'finql',
                             dataTable = list(df))
        }

        if (nameData == 'fair_value') {
          list_dfs <-
            seq_along(list_parts) %>%
            future_map(function(x) {
              list_name <-
                list_parts[[x]]


              if (list_name == 'models') {
                df_list <-
                  list_data[[list_parts[x]]]

                if (df_list %>% length() == 0) {
                  return(tibble())
                }

                df_models <-
                  c('low', 'mid', 'high') %>%
                  future_map(function(x){
                    df_row <-
                      df_list[x] %>% flatten_df() %>%
                      purrr::set_names(c('priceModel', 'pctUpside'))

                    names(df_row) <-
                      names(df_row) %>% str_c(x %>% str_to_title(), sep = '')

                    df_row
                  })

                df_models <-
                  df_models %>% reduce(bind_cols)

                df <-
                  df_list[1:3] %>%
                  as_tibble() %>%
                  purrr::set_names(c('slugModel', 'nameModel', 'datetimeModel')) %>%
                  mutate(datetimeModel = datetimeModel %>% readr::parse_datetime()) %>%
                  bind_cols(df_models)

                return(df)
              }

              if (list_name == 'confidence') {
                return(tibble())
              }
              if (list_name == 'ranges') {
                df <-
                  list_data[[list_parts[x]]]$price %>% as_tibble() %>%
                  purrr::set_names(c('price52WeeekLow',
                                     'price52WeekHigh')) %>%
                  bind_cols(
                    list_data[[list_parts[x]]]$upside %>% as_tibble() %>%
                      purrr::set_names(c('pctTo52WeekLow', 'pctTo52WeekhHigh'))
                  )
                return(df)
              }

              df <-
                list_data[[list_parts[x]]]

              df <- df %>% flatten_df()

              if (df %>% nrow() == 0) {
                return(tibble())
              }


              if (list_name == 'analyst_target') {
                names(df)

                df <-
                  df %>%
                  purrr::set_names(c('priceAnalystMid', 'priceAnalystLow', 'priceAnalystHigh', 'countAnalystRatings'))
              }

              if (list_name == 'market') {
                df <-
                  df %>%
                  purrr::set_names(c('priceLast', 'priceLowYear', 'pricehighYear'))
              }

              if (list_name == 'averages') {
                df <- df %>% purrr::set_names(c('priceAverage', 'pctChangeToAverage'))
              }

              df

            })

          list_df_rows <-
            list_dfs %>% map_dbl(nrow)


          df_pricing <- list_dfs[list_df_rows == 1] %>% purrr::reduce(bind_cols)

          has_models <- list_dfs[list_df_rows == 5] %>% flatten_df() %>% nrow() > 0

          if (has_models) {
            df_pricing <-
              df_pricing %>%
              mutate(dataModels = list(list_dfs[list_df_rows == 5] %>% flatten_df()))
          }

          data <-
          tibble(nameTable = 'models',
                       dataTable = list(df_pricing))
        }

        if (nameData == 'stats') {
          list_dfs <-
            seq_along(list_parts) %>%
            future_map(function(x) {
              df_list <-
                list_data[[list_parts[x]]] %>% as_tibble()

              all_data <-
                df_list %>% names() %>%
                future_map(function(x){
                  df_list[x] %>% pull() %>% flatten_df() %>%
                    flatten_df() %>%
                    gather(typeParty, value) %>%
                    mutate(nameItem = x,
                           value = value %>% as.character() %>%  readr::parse_number()) %>%
                    spread(nameItem, value)
                }) %>%
                suppressWarnings()

              all_data <-
                all_data %>% reduce(left_join) %>%
                suppressWarnings() %>%
                suppressMessages()


            })

          data <-
            list_dfs %>% purrr::reduce(left_join) %>% suppressMessages()

          data <-
            tibble(nameTable = list_cols[x],
                       dataTable = list(data))
        }

        if (nameData == 'financials') {
          df <- list_data %>% flatten_df()
          df_periods <-
            df %>%
            gather(item, value) %>%
            filter(item %>% str_detect('period')) %>%
            mutate(item =item %>% str_replace_all('period_end_', '')) %>%
            dplyr::select(period = item, datePeriodEnd = value) %>%
            mutate(datePeriodEnd = datePeriodEnd %>% lubridate::ymd())

          df_values <-
            df %>%
            gather(item, value) %>%
            filter(!item %>% str_detect('period')) %>%
            mutate(value = value %>% as.numeric())

          periods <-
            df_periods$period %>%
            append(c('q1', 'q2', 'q3', 'q4'))

          period_slugs <-
            glue::glue("_{periods}") %>%
            str_c(collapse = '|')

          df_values <-
            df_values %>%
            mutate(nameItem = item %>% str_replace_all(period_slugs, '') %>% str_replace_all('\\__', '\\_'))

          items <-
            glue::glue("{df_values$nameItem %>% unique()}_") %>% str_c(collapse = '|')

          df_values <-
            df_values %>%
            mutate(period = item %>% str_replace_all(items, '')) %>%
            left_join(df_periods) %>%
            dplyr::select(datePeriodEnd,
                          typePeriod = period,
                          nameItem,
                          value) %>%
            tidyr::separate(typePeriod,
                            into = c('idPeriod', 'idQuarter'),
                            sep = '_') %>%
            suppressWarnings() %>%
            suppressMessages() %>%
            filter(!datePeriodEnd %>% is.na())
          data <-
            tibble(nameTable = nameData,
                       dataTable = list(df_values))
        }

        data
      })

    df_cols <-
      df_classes %>%
      filter(classPart %in% c('data.frame')) %>%
      pull(namePart)

    dfs <-
      seq_along(df_cols) %>%
      future_map_dfr(function(x){
        nameData <-
          df_cols[[x]]
        list_data <-
          data[nameData][[nameData]] %>%
          as_tibble()

        if (nameData == 'news') {
          list_data <-
            list_data %>%
            purrr::set_names(c('titleArticle',
                               'urlArticle',
                               'timeSincePublished', 'domainArticle'))

          list_data <-
            list_data %>%
            mutate(datetimeArticle = timeSincePublished %>% map_chr(.extract_date) %>% ymd_hms(),
                   dateArticle = datetimeArticle %>% as.Date()) %>%
            select(one_of(c("datetimeArticle","dateArticle", "domainArticle", "titleArticle", "urlArticle"))) %>%
            arrange(datetimeArticle)

        }

        if (nameData == 'templates') {
          list_data <-
            list_data %>%
            purrr::set_names(c('idModel',
                               'slugModel',
                               'dateModel', 'nameModel',
                               'isVerified', 'categoryModel', 'hasChecksPassed'))
        }

        data <-
          tibble(nameTable = nameData,
                     dataTable = list(list_data))
      })

    data <-
      dfs %>%
      bind_rows(df_lists) %>%
      bind_rows(
        tibble(nameTable = 'description',
                   dataTable = list(df_description))
      )
    data
  }

#' Finbox ticker data
#'
#' @param tickers \code{vector} of tickers
#' @param assign_to_environment
#'
#' @return
#' @export
#' @import glue purrr curl rvest jsonlite dplyr anytime reticulate lubridate stringr future furrr
#' @examples
finbox_tickers <-
  function(tickers = c("VNO", "BXP"),
           assign_to_environment = TRUE) {
    .get_ticker_information_safe <-
      purrr::possibly(.get_ticker_information, tibble())

    all_data <-
      tickers %>%
      future_map_dfr(function(x){
        .get_ticker_information_safe(ticker = x)
      }) %>%
      suppressMessages() %>%
      suppressWarnings()

    if (assign_to_environment) {
      tables <-
        all_data$nameTable %>% unique()

      seq_along(tables) %>%
        walk(function(x){
          df <-
            all_data %>%
            filter(nameTable == tables[[x]]) %>%
            dplyr::select(-nameTable) %>%
            unnest()

          table_name <- str_c('data', tables[[x]] %>% str_to_upper())

          assign(table_name, eval(df), envir = .GlobalEnv)
        })

    }
    all_data
  }

# models ------------------------------------------------------------------

# https://api.finbox.io/beta/fairvalues/{idTicker}/models


# fairvalue ---------------------------------------------------------------

# https://api.finbox.io/beta/fairvalues/FB


# data_query --------------------------------------------------------------
# {base}/{idTicker}/{metric}?{query}
# https://api.finbox.io/beta/data/AAPL/total_revenue?period=FQ-7:FQ # last 10 quarters of revenue

