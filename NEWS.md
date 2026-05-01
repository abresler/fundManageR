# fundManageR News

## Unreleased (0.5.04)

### Observability

- **`with_telemetry()`** — wrap any data-fetcher to emit a JSONL row per call (timestamp, function, row count, duration, success, error, args hash). Companion `read_telemetry()` and `summarize_telemetry()` for retros and per-function IRR audits. Opt-out via `options(fundManageR.telemetry = FALSE)`.

### Jurisdiction risk (already shipped — surfacing)

- **`flag_fund_jurisdictions(data)`** is the canonical column-lift for any tibble carrying a fund-incorporation or counterparty-jurisdiction column. Auto-detects from `location_fund_incorporation`, `name_jurisdiction`, `country_office_primary`, and friends. Appends `rating_jurisdiction_risk` (0-100) and `flag_jurisdiction_risk` (CLEAR/WATCH/CAUTION/HIGH/CRITICAL).
- **`dictionary_jurisdiction_risk()`** returns the underlying Bayesian-composed risk table sourced from FATF + EU Council + TJN FSI + OFAC. Use this directly for filtering or joining at scale.

Pattern:
```r
adv_managers_filings(crd_ids = 156663, section_names = "Private Fund Reporting") %>%
  flag_fund_jurisdictions() %>%
  dplyr::filter(flag_jurisdiction_risk %in% c("HIGH", "CRITICAL"))
```
