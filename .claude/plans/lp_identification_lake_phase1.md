# LP-Identification Data Lake — Phase 1 Build Record

**Built:** 2026-04-30
**Status:** Phase 1 complete (single year POC + 7-year backfill running)
**Author:** fundManageR + dq lake integration

## What the lake does

Identifies actual LPs (limited partners) inside private funds disclosed by SEC ADV Form Part 1A Section 7B. ADV redacts LP names by design — so we identify them by aggregating the LP-side disclosure ecosystem (LPs disclose their fund holdings even when funds don't disclose their LPs).

## What was built in this session

### R-side ETL (`fundManageR/R/dol_form_5500.R`)

Functions:
- `get_dol_form_5500_bulk(years, schedules)` — downloads + extracts EFAST2 archives
- `parse_5500_main(year)` — Form 5500 main filing → plan sponsor universe
- `parse_5500_schedule_d_part1(year)` — DFE → participating plan edges (the LP→fund edge)
- `parse_5500_schedule_d_part2(year)` — Plan → master trust / 103-12 IE edges
- `parse_5500_schedule_c(year)` — Plan → service provider (incl. investment managers)
- `name_norm_entity(x)` — entity-name normalization for joining
- `write_5500_year_to_lake(year)` — writes 4 partitioned parquet outputs

### dq lake schemas (`dq/config/lakes.json`)

3 new lakes registered, all `lake-schema-check` compliant:

| Lake | Domain | Subdomain | Tier | Purpose |
|---|---|---|---|---|
| `lake_lp_entities` | investments | lp_universe | t1 | Canonical LP entity registry (id_lp_canonical, type_lp, jurisdiction) |
| `lake_lp_fund_commitments` | investments | lp_fund_edges | t1 | Edge table (LP, fund, $ commitment, source, confidence_tier) |
| `lake_lp_signals` | investments | lp_soft_signals | t2 | Soft-evidence signals (service provider relationships, etc.) |

### Single-year empirical results (FY2023 only)

From DoL Form 5500 alone, ONE year:

- **231,046** ERISA plans (main filing)
- **414,538** Schedule D Part 1 LP→fund edges (CCT/PSA/MTIA participations)
- **1,217,594** Schedule D Part 2 plan→vehicle edges (master trust / 103-12 IE)
- **137,498** Schedule C provider rows
- **168,704** distinct LP entities written to lake
- **1,628,011** edges written to lake
- **137,452** soft signals written to lake

### Cross-walk to ADV 7B (108,202 private funds)

After applying `name_norm()` macro to both sides:

| Confidence Tier | Funds Matched | LPs Matched | Edges |
|---|---:|---:|---:|
| T1 STRICT (fund-name + manager-name match) | 16 | 16 | 17 |
| T2 FUND-NAME-ONLY | 143 | 162 | 420 |
| T3 MANAGER-ONLY (LP committed to ≥1 of manager's funds) | 3,730 | 262 | 14,840 |

By ADV fund type at T2 (fund-name match):
- 49 hedge funds (88 ERISA LPs)
- 10 private equity funds (8 LPs)
- 2 real estate funds
- 1 venture fund
- 104 "Other" alternatives
- = 166 distinct ADV `id_private_fund` values with ≥1 confirmed ERISA LP

## What this means honestly

ERISA Schedule D mostly captures **CCT/PSA/MTIA pooled vehicles** — not the typical PE/HF/RE/VC LP partnership. The reason most PE funds don't show up here: ERISA plans typically invest in PE through 103-12 IEs (which file Schedule D Part 2 with limited fund disclosure) or directly as LPs without the 103-12 structure (which DOES appear on Schedule of Assets attachments — Phase 1.5).

**Coverage estimate from this single source alone**: ~3.4% of ADV private funds have ≥1 indirectly-confirmed ERISA LP (manager-level match). Direct fund-name match: ~0.13%. This is a floor, not a ceiling — multi-source aggregation lifts it dramatically.

## Source ranking (post-research, IRR-ordered)

| Rank | Source | Coverage | Effort | Status |
|---|---|---|---|---|
| 1 | DoL Form 5500 EFAST2 (Schedule D + C) | ERISA LP universe | LOW (structured CSV, free) | ✅ Phase 1 done |
| 2 | DoL Form 5500 Schedule of Assets PDFs | Full ERISA fund-by-fund detail | HIGH (PDF OCR per filing) | Phase 1.5 |
| 3 | Public pension quarterly reports (CalPERS PEP, NY CRF, Colorado PERA) | ~70% of $100M+ US PE funds | MEDIUM (PDF table OCR) | Phase 2 |
| 4 | Norges Bank NBIM all-investments | Largest single sovereign LP | LOW (structured web) | Phase 2 |
| 5 | Canadian Maple-8 annual reports | Co-LP signal on most large-cap funds | MEDIUM (PDF parse) | Phase 2 |
| 6 | NAIC Schedule BA via state DOI portals | US insurer alt holdings | HIGH (state-by-state) | Phase 3 |
| 7 | ProPublica 990-PF full-text | Top 200 foundations | MEDIUM (full-text + NER) | Phase 3 |
| 8 | SEC Form D Item 5/14 co-invest SPVs | Anchor LP names in small SPVs | MEDIUM (SEC EDGAR bulk) | Phase 3 |
| 9 | Pension press releases ("X commits to Y fund") | Real-time freshness | MEDIUM (RSS aggregation) | Phase 3 |
| 10 | PACER bankruptcy claim agent dockets | One-time goldmines (Lehman, Madoff) | HIGH (case-by-case) | Phase 4 |
| - | Wayback Machine pension IR archaeology | Historical depth | MEDIUM | Phase 4 |
| ❌ | Cayman/Lux/Channel Islands UBO registries | Post-Sovim 2022, all gate-kept. Zero IRR. | - | EXCLUDED |
| ❌ | LinkedIn IR-team scraping | ToS violation. Civil liability. | - | EXCLUDED |
| ❌ | S3 bucket access / breach corpora | CFAA risk. Tortious. | - | EXCLUDED |

## Anti-criteria (what we deliberately did NOT build)

1. No PDF OCR (Phase 1.5 explicitly out of scope)
2. No international UBO scraping (post-Sovim ECJ ruling = zero-IRR)
3. No LinkedIn / S3 / leaked-corpus paths (legal-risk exclusion)
4. No commercial subscriptions (zero-budget constraint)

## Next-session continuation

```bash
# Re-load and run incremental year
cd ~/Desktop/r_packages/fundManageR
Rscript -e "devtools::load_all('.'); write_5500_year_to_lake(2024)"

# Query the lake
duckdb -c "SELECT * FROM read_parquet('~/Desktop/data/lake_lp_fund_commitments/**/*.parquet', union_by_name=true) LIMIT 10"

# Joined cross-walk to ADV
# (full SQL in this session's transcript — uses name_norm() macro for fuzzy matching)
```

## Files modified

- `R/dol_form_5500.R` — new (370+ lines, exports 7 functions)
- `scripts/build_lp_lake_phase1.R` — POC runner
- `scripts/build_lp_lake_backfill.R` — 2017-2023 backfill
- `~/Desktop/dq/config/lakes.json` — 3 new lake definitions
- `~/Desktop/data/lake_lp_entities/` — parquet partitions
- `~/Desktop/data/lake_lp_fund_commitments/` — parquet partitions
- `~/Desktop/data/lake_lp_signals/` — parquet partitions
