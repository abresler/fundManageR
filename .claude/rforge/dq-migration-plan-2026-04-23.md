# dq Migration Plan: SIC Codes + CUSIPs + ADV Data

Design sketch for moving three fundManageR datasets into `~/Desktop/dq/src/lakes/`.

Each lake follows the `LakeDefinition` pattern (TypeScript file with key, label, viewSQL pointing to parquet).

## 1. SIC CODES — Smallest, Cleanest (DO FIRST)

### Source Functions
- `dictionary_sic_codes()` — sec_2.R L12678-12687 (live SEC HTML scrape, ~980 rows)
- `sic_naics_codes()` — edgar_rf.R (bridges SIC 4-digit ↔ NAICS 6-digit, ~500 rows)

### Proposed Lake: `sic_codes`
**Path**: `~/Desktop/data/sic_codes/sic_codes.parquet`
**Key schema**:
```
id_sic          INTEGER   PRIMARY (4-digit SIC)
name_sic        VARCHAR   industry description
name_office_ad  VARCHAR   SEC Office of Asset Management assignment
id_naics_6      INTEGER[] bridge to NAICS (array since 1:N)
id_naics_3      INTEGER[] NAICS 3-digit parents
date_scraped    DATE      when pulled from SEC
url_source      VARCHAR   "https://www.sec.gov/info/edgar/siccodes.htm"
```

### Backfill Script (R -> parquet -> dq lake)
```r
# R/data-raw/build_sic_codes.R
library(fundManageR); library(arrow); library(dplyr)
sic   <- dictionary_sic_codes()  |> mutate(id_sic = as.integer(idSIC))
naics <- sic_naics_codes()       # produces sic↔naics bridge

out <- sic |>
  left_join(
    naics |> group_by(id_sic) |>
      summarise(id_naics_6 = list(id_naics_6), id_naics_3 = list(id_naics_3)),
    by = "id_sic"
  ) |>
  mutate(date_scraped = Sys.Date(),
         url_source   = "https://www.sec.gov/info/edgar/siccodes.htm")

arrow::write_parquet(out, "~/Desktop/data/sic_codes/sic_codes.parquet")
```

### dq Lake Def (TypeScript)
```ts
// ~/Desktop/dq/src/lakes/sic_codes.ts
export const sic_codes: LakeDefinition = {
  key: 'sic_codes',
  label: 'SEC SIC Codes + NAICS Bridge',
  viewSQL: `
    SELECT id_sic, name_sic, name_office_ad,
           id_naics_6, id_naics_3, date_scraped, url_source
    FROM read_parquet('${HOME}/Desktop/data/sic_codes/sic_codes.parquet')
  `
};
```

### IRR: HIGH. 1K rows, <1MB. Enables SIC joins on any fpds/sam/edgar/adv UEI data. 2 hour implementation.

---

## 2. CUSIPS — Moderate Complexity

### Source Functions
- `sec_cusips(years, quarters)` — sec_functions.R, scrapes 13F PDF filings per year/quarter
- Each PDF: `https://www.sec.gov/divisions/investment/13f/13flistYYYYQn.pdf`
- ~50K rows/quarter × 4 quarters × ~25 years = up to 5M rows (historical full backfill)

### Threat surfaces (from red-team):
- PDF OCR quality varies; text extraction returns garbage occasionally
- `purrr::possibly(..., tibble())` silently returns empty tibble on parse failure
- Year/quarter coverage gaps (some quarters have no 13F list published)

### Proposed Lake: `sec_cusips_13f`
**Path**: `~/Desktop/data/sec_cusips/cusips_13f_{year}.parquet` (partitioned)
**Schema**:
```
id_cusip          VARCHAR(9)   PRIMARY (within year+quarter)
name_security     VARCHAR      issuer + security description
description_type  VARCHAR      equity / debt / option / etc.
name_issuer       VARCHAR      cleaned issuer name
pct_rate_note     DECIMAL(5,4) for debt securities (coupon)
is_debt_security  BOOLEAN
is_adr            BOOLEAN      American Depositary Receipt flag
year_filing       INTEGER      2000-present
quarter_filing    INTEGER      1-4
url_pdf           VARCHAR      provenance
date_scraped      DATE
```

### Backfill (incremental, by year)
```r
# R/data-raw/build_cusips_13f.R — run as one-shot or annually
for (yr in 2000:year(Sys.Date())) {
  df <- tryCatch(
    sec_cusips(years = yr, only_most_recent = FALSE),
    error = function(e) NULL
  )
  if (!is.null(df) && nrow(df) > 0) {
    arrow::write_parquet(
      df, sprintf("~/Desktop/data/sec_cusips/cusips_13f_%d.parquet", yr)
    )
  }
}
```

### dq Lake Def
```ts
// Union all year partitions
export const sec_cusips_13f: LakeDefinition = {
  key: 'sec_cusips_13f',
  label: 'SEC 13F Filer CUSIP List',
  viewSQL: `
    SELECT id_cusip, name_security, name_issuer, description_type,
           pct_rate_note, is_debt_security, is_adr,
           year_filing, quarter_filing, url_pdf, date_scraped
    FROM read_parquet('${HOME}/Desktop/data/sec_cusips/cusips_13f_*.parquet', union_by_name=TRUE)
  `
};
```

### IRR: MEDIUM-HIGH. 5M rows, ~200MB. Enables security-level joins (13F holdings, SIFMA data, etc). 1 day implementation including historical backfill.

---

## 3. ADV DATA — Most Complex (3 related lakes)

### Source Functions (all adv_functions.R)
- `adv_period_urls()` — monthly snapshot URLs from data.gov + SEC fallback
- `adv_managers_metadata()` — name → CRD → firm summary
- `adv_managers_filings()` — all Form ADV sections per CRD
- `adv_managers_brochures()` — Part 2A PDF text
- `adv_managers_periods_summaries()` — quarterly AUM/AUM-by-category aggregates
- `adv_managers_current_period_summary()` — latest period snapshot
- `sec_adv_manager_sitemap()` — URL directory

### Proposed Lakes (3)
**Lake A: `adv_managers_dimension`** (dim table, ~25K RIAs)
```
id_crd                    INTEGER     PRIMARY (SEC Central Registration Depository)
name_entity_manager       VARCHAR
name_entity_manager_legal VARCHAR
name_entity_manager_dba   VARCHAR[]
is_exempt_filer           BOOLEAN
type_filer                VARCHAR     state/SEC/exempt
date_first_filing         DATE
date_latest_filing        DATE
url_iapd_summary          VARCHAR
date_amendment_latest     DATE        (new: red-team A7 fix)
```
Refresh: monthly.

**Lake B: `adv_managers_aum_history`** (fact table, time series)
```
id_crd                       INTEGER
date_period                  DATE      period-end
amount_aum_discretionary     BIGINT
amount_aum_non_discretionary BIGINT
amount_aum_total             BIGINT
count_clients                INTEGER
count_accounts               INTEGER
type_period                  VARCHAR   monthly / quarterly / annual
date_filing                  DATE
date_amendment               DATE      (new: A7)
is_amendment                 BOOLEAN   (new: A7)
```
Refresh: quarterly. Time-series — partition by year_period.

**Lake C: `adv_managers_fees`** (derived from `extract_fee_references`)
```
id_crd               INTEGER
id_sentence          INTEGER
fee_pct              DECIMAL(5,2)    extracted percentage
text_sentence        VARCHAR         original context (audit trail)
type_fee             VARCHAR         management / performance / carry / unknown
date_brochure        DATE
date_extracted       DATE
url_brochure_pdf     VARCHAR
```
Refresh: quarterly alongside brochures.

### Backfill (ordered)
```r
# 1. Dimension first (bootstrap CRD universe)
periods <- adv_period_urls()
all_mgrs <- adv_managers_metadata(urls_sitemap = NULL)  # full sweep
arrow::write_parquet(all_mgrs, "~/Desktop/data/adv/managers_dim.parquet")

# 2. AUM history (quarterly summaries)
adv_hist <- adv_managers_periods_summaries(
  urls = periods$urlData,
  return_message = FALSE
)
arrow::write_dataset(adv_hist, "~/Desktop/data/adv/aum_history/",
  partitioning = "year_period")

# 3. Fees (AFTER A3 red-team fix lands — previous runs silently dropped >5%)
brochures <- adv_managers_brochures(crd_ids = all_mgrs$idCRD)
fees <- brochures |> extract_fee_references(fee_ceiling_pct = 30)
arrow::write_parquet(fees, "~/Desktop/data/adv/fees.parquet")
```

### dq Lake Defs (3 files)
`~/Desktop/dq/src/lakes/adv_managers_dimension.ts`
`~/Desktop/dq/src/lakes/adv_managers_aum_history.ts`
`~/Desktop/dq/src/lakes/adv_managers_fees.ts`

Each follows the pattern above; viewSQL reads parquet with `union_by_name=TRUE` where partitioned.

### IRR: HIGH strategic, MEDIUM implementation. 25K managers × 40 periods = ~1M AUM rows. 2-3 days including the red-team A3/A4/A7 prerequisites.

---

## Dependency Order + Prerequisites

```
Phase 1 (now, independent): SIC codes
   └── No code changes needed beyond Batch 06 (edgar_rf.R mechanical sweep)

Phase 2 (after CUSIP tests): CUSIPs  
   └── sec_cusips() is in Batch 03 (forged). Just need parquet writer + lake def.

Phase 3 (after ADV fixes): ADV three-lake suite
   ├── Red-team fixes A1, A3, A4 applied (commit 85711cd) ✓
   ├── Still need: A7 (amendment date column) — ADV file schema addition
   ├── Still need: A5, A6, A8 (silent-failure hardening) — recommended before prod
   └── Then backfill in dim → fact → fees order
```

## Next Decision

1. Build SIC lake immediately (2h) — unblocks SIC joins across AFWERX/dq_fpds lakes.
2. Before ADV lakes: decide if A7 (amendment-date) is a schema addition to `adv_managers_*` output columns (breaking change callers must tolerate).
3. CUSIPs: check if user wants ALL years or just N years back (size consideration).
