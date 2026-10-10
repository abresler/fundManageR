# LP Lake — Phase 2+ Source Queue (post-AUTORESEARCH 2026-04-30)

## IRR-Ranked Build Order

| # | Source | Format | Effort | Coverage Gain | Verdict |
|---|---|---|---|---|---|
| 1 | **NJ Pension Explorer Socrata API** | JSON/CSV via REST | 0.5 day | 7 NJ funds, structured fund commitments | **BUILD NEXT** |
| 2 | **State Comptroller ACFRs (NY, IL, PA, CA, NJ)** | PDFs with named LP rosters | 2 days (reuses Schedule of Assets parser) | 5-10 state-pension Schedule-of-Assets equivalents | **BUILD AFTER NJ** |
| 3 | **CourtListener RECAP — ERISA excessive-fee suits** | REST API, discovery exhibits | 2 days | Fund-name extraction from class-action exhibits, zero legal risk | **BUILD #3** |
| 4 | NM SIC + KY KRS quarterly PDFs | PDF OCR | 1 day each | Marginal — small/mid pensions | Backlog |

## Dead Ends (do NOT pursue)

- **BDC 10-K Schedule of Investments** — wrong direction (BDCs are GPs, not LPs in the LP-roster sense)
- **Federal Audit Clearinghouse SEFA** — federal-award line items only, no alt-investment detail
- **Schedule 13D/13G fund-of-funds** — only triggers at 5%+ position; FoF→fund LP relationships not disclosed there
- **AIFMD Annex IV** — confidential, regulator-only; FOIA success rates near-zero
- **MSRB EMMA POB official statements** — POB docs rarely include detailed pension LP rosters; pension audits are separate
- **Municipal pensions w/o open-data** — Chicago Teachers, Boston, SF Employees, Philly: no machine-readable LP feeds; manual ACFR scrape ROI <3%

## NO-BLOAT INVARIANT (applies to all new ingestion)

- Lake hard cap: 1GB combined `~/Desktop/data/lake_lp_*` (currently 372MB; ~628MB headroom)
- Pre-ingestion size check: abort if would breach 1GB
- Stream → parse → compressed parquet → delete raw, no caching
- Schedule of Assets pipeline pattern is the template (PDFs deleted post-parse)

## Coverage Math (post Phase 2)

If all 3 top picks ship:
- + ~2K NJ fund-commitment edges (named pension LP)
- + ~5K-15K named LP edges from state ACFRs (NY CRF alone: 100+ fund commitments/quarter)
- + ~500-1K fund-name extractions from CourtListener ERISA dockets
- Total Phase 2 lift: ~10K-20K NEW high-confidence edges

Combined with Phase 1 (12M structured) + Phase 1.5 (13.6K Schedule of Assets), the lake hits ~12-13M edges with rich provenance across 5 source types.

## SIGINT Round 2 — 2026-05-01 (3 net-new high-IRR sources)

| # | Source | Format | URL | Yield |
|---|---|---|---|---|
| 5 | **IFC Disclosure Portal** — $9B+ across ~400 PE funds (emerging-market anchor LP) | API-queryable database | disclosures.ifc.org | HIGH |
| 6 | **EBRD Project Summary Documents** — €250-350M/yr fund commitments across 40+ countries | CSV export | ebrd.com/work-with-us/project-finance/project-summary-documents.html | HIGH |
| 7 | **Washington State Investment Board (WSIB)** — $170B AUM, separate PE IRR reports | PDF (consistent naming) | sib.wa.gov/reports.html | MEDIUM-HIGH |

## SIGINT Round 2 — Confirmed dead ends

- Chilean AFP, Mexican Afore — no public LP-fund disclosure
- Oregon Treasurer — statutory exemption (ORS 192.502)
- UK Charity Commission — no Schedule-R-equivalent
- Massachusetts PRIM — public-records-request only, no archive API
- Singapore MAS AIF — fragmented, low yield

## Run order for next session

1. EBRD CSV (lowest effort, HIGH yield)
2. IFC API probe (registration may be needed)
3. WSIB PDF parser (reuses Phase 1.5 parser — same pattern as Boeing/Lockheed)
