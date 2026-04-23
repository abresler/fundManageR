# Red-Team: Waterfall + ADV Functions — 2026-04-23

## Methodology
ParallelAnalysis workflow from RedTeam skill, LIBERTAS+FAT+Autism doctrines. Two focused agents (cos-debugger, sonnet) each producing decomposition, attack, steelman, counter-argument, "one thing." Every claim cites file:line.

## Summary Table

| Area | P0/CRITICAL | HIGH | MED/LOW | Total |
|------|-------------|------|---------|-------|
| waterfall_functions.R | 5 | 2 | 8 | 15 |
| adv_functions.R | 5 | 3 | 6 | 14 |

## Waterfall Findings

| # | File:Line | Issue | Severity | Fix |
|---|-----------|-------|----------|-----|
| W1 | waterfall_functions.R:153-154 | `multipleCapital = abs(distributions)/contributions` no zero-guard | P0 | Zero-check, coerce to numeric, use `NA_real_` |
| W2 | waterfall_functions.R:115-120 | Secant method on all-positive CF — no root, returns garbage | P0 | Validate sign change before calling secant; `stop()` with message |
| W3 | waterfall_functions.R:~1675 | `distinct()` dedup can drop tier rows on numeric coincidence | P0 | `distinct(idPeriod, tierWaterfall, .keep_all = TRUE)` |
| W4 | waterfall_functions.R:997-998 | Equity carryforward `filter(idPeriod == period - 1)` silent max on duplicates | P0 | De-dupe equity_df before filter; add assertion |
| W5 | waterfall_functions.R:~1694 | Zero-col removal on formattable — `abs()`/`colSums()` may not coerce cleanly | P0 | `as.numeric` before colSums |
| W6 | waterfall_functions.R:112 | IRR uses 365.24-day year; pref accrual configurable 360/365 — mismatch | P1 | Add `day_count` param to calculate_irr_periods |
| W7 | waterfall_functions.R:583-584 | `.parse_promote_structure` silently truncates >2-token strings | P1 | Validate token count; stop on unexpected |

## ADV Findings

| # | File:Line | Issue | Severity | Fix |
|---|-----------|-------|----------|-----|
| A1 | adv_functions.R:945 | `httr::set_config(httr::config(ssl_verifypeer = 0L))` disables SSL globally | CRITICAL | Remove. If SEC cert issue, use `httr::config(ssl_verifypeer = 0L)` per-request only |
| A2 | adv_functions.R:9601 | `str_detect('[0-99]%')` — `[0-99]` is redundant char class `[0-9]` | MED | Change to `"\\d+%"` for multi-digit |
| A3 | adv_functions.R:9606 | `filter(word <= 5)` drops all fees >5% (dumps hedge fund carries, performance fees) | CRITICAL | Invert: flag high fees, don't drop. Rename threshold semantics |
| A4 | adv_functions.R:1260-1262 | No rate limit on 20K+ SEC IAPD requests — IP throttled silently | CRITICAL | Add `Sys.sleep(0.2)` or httr2 req_throttle |
| A5 | adv_functions.R:948-953 | CSS selector brittleness on SEC IAPD redesign | CRITICAL | Add schema validation + fallback selectors |
| A6 | adv_functions.R:8462-8470 | Missing dictionary entries log but silently return raw SEC name → join drops | HIGH | Elevate warning to error with actionable message |
| A7 | (pipeline) | Original vs amended filings conflated — no amendment-date column | HIGH | Add `dateAmendment` column to all adv_managers_* output |
| A8 | adv_functions.R:1256, 8285 | Universal `purrr::possibly(.fn, tibble())` — 403 indistinguishable from empty | HIGH | Session-scoped fail counter; log URLs that returned empty |

## "The One Thing"

**Waterfall:** Division by zero in `multipleCapital` (L153-154) producing `NaN` in LP-facing summary output. Lawsuit risk when LP sees "Capital Multiple of NaN".

**ADV:** Fee extraction silently loses all managers charging >5%. A pipeline reporting "average fund fee = 0.85%" would be missing every hedge fund in the dataset.

## Agents Used
- cos-debugger (sonnet) × 2, 51k + 87k tokens, 15 + 14 findings
