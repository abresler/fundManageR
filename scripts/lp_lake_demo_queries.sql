-- LP-Identification Lake — Demo Queries
-- Built 2026-04-30. Source: DoL Form 5500 EFAST2 2017-2023 (~12M edges)
-- Run with: duckdb -c "$(cat lp_lake_demo_queries.sql)"

-- =============================================================================
-- Q1: For a fund manager, list every named ERISA LP and the funds they hold
-- =============================================================================

WITH bw AS (
  SELECT DISTINCT
    e.name_fund, e.id_lp_canonical, e.amount_commitment, e.source, e.year_filing
  FROM read_parquet('/Users/alexbresler/Desktop/data/lake_lp_fund_commitments/**/*.parquet', union_by_name=true) e
  WHERE (LOWER(COALESCE(e.name_manager,'')) LIKE '%bridgewater%'
       OR LOWER(e.name_fund) LIKE '%bridgewater%')
    AND e.year_filing = 2023
),
lp AS (
  SELECT DISTINCT id_lp_canonical, name_lp, state, code_business
  FROM read_parquet('/Users/alexbresler/Desktop/data/lake_lp_entities/**/*.parquet', union_by_name=true)
  WHERE name_lp IS NOT NULL AND year_filing = 2023
)
SELECT
  l.name_lp AS lp_name,
  l.state,
  COUNT(DISTINCT bw.name_fund) AS funds_held,
  STRING_AGG(DISTINCT bw.name_fund, ' | ') AS fund_list
FROM bw
INNER JOIN lp l USING (id_lp_canonical)
GROUP BY 1, 2
ORDER BY funds_held DESC
LIMIT 50;


-- =============================================================================
-- Q2: For an ADV `id_private_fund`, find all confirmable ERISA LPs
-- =============================================================================

CREATE OR REPLACE MACRO name_norm(x) AS
  TRIM(REGEXP_REPLACE(
    REGEXP_REPLACE(
      REGEXP_REPLACE(LOWER(x), '[[:punct:]]', ' ', 'g'),
      '\b(lp|llc|llp|inc|incorporated|corp|corporation|company|co|trust|fund|fd|partners|partnership|partner|advisors|advisers|management|mgmt|capital|cap|holdings|holding|group|grp|usa|us|the|of|and|na|bank|institutional|fiduciary|global|investments|investment|asset|services|service|inst|advisory|series|master|feeder|onshore|offshore|cayman|delaware)\b',
      ' ', 'g'
    ),
    '\s+', ' ', 'g'
  ));

WITH adv_target AS (
  SELECT
    name_fund_clean,
    name_entity_manager,
    id_private_fund,
    type_fund,
    name_norm(name_fund_clean) AS norm_fund,
    name_norm(name_entity_manager) AS norm_manager
  FROM read_parquet('/Users/alexbresler/Desktop/data/sec_adv/section=section_7_b_private_fund_reporting/**/*.parquet', union_by_name=true)
  WHERE id_private_fund = '805-1442067152'  -- swap CRD-fund ID here
),
erisa AS (
  SELECT DISTINCT
    name_norm(name_fund) AS norm_fund,
    name_norm(name_manager) AS norm_manager,
    id_lp_canonical, name_fund, name_manager, year_filing
  FROM read_parquet('/Users/alexbresler/Desktop/data/lake_lp_fund_commitments/**/*.parquet', union_by_name=true)
),
lp AS (
  SELECT DISTINCT id_lp_canonical, name_lp, state, year_filing
  FROM read_parquet('/Users/alexbresler/Desktop/data/lake_lp_entities/**/*.parquet', union_by_name=true)
  WHERE name_lp IS NOT NULL
)
SELECT
  a.id_private_fund,
  a.name_fund_clean AS adv_fund_name,
  a.name_entity_manager AS adv_manager,
  l.name_lp AS lp_name,
  l.state,
  e.year_filing AS year_disclosed
FROM adv_target a
INNER JOIN erisa e ON a.norm_fund = e.norm_fund
LEFT JOIN lp l ON e.id_lp_canonical = l.id_lp_canonical AND e.year_filing = l.year_filing
ORDER BY year_disclosed DESC, lp_name;


-- =============================================================================
-- Q3: Coverage summary by ADV fund type and confidence tier
-- =============================================================================

WITH adv AS (
  SELECT DISTINCT
    name_norm(name_fund_clean) AS norm_fund,
    name_norm(name_entity_manager) AS norm_manager,
    id_private_fund,
    type_fund
  FROM read_parquet('/Users/alexbresler/Desktop/data/sec_adv/section=section_7_b_private_fund_reporting/**/*.parquet', union_by_name=true)
  WHERE id_private_fund IS NOT NULL AND name_fund_clean IS NOT NULL
),
erisa AS (
  SELECT DISTINCT
    name_norm(name_fund) AS norm_fund,
    name_norm(name_manager) AS norm_manager,
    id_lp_canonical
  FROM read_parquet('/Users/alexbresler/Desktop/data/lake_lp_fund_commitments/**/*.parquet', union_by_name=true)
)
SELECT
  COALESCE(a.type_fund, 'unknown') AS adv_type,
  COUNT(DISTINCT a.id_private_fund) FILTER (
    WHERE EXISTS (SELECT 1 FROM erisa e WHERE e.norm_fund = a.norm_fund AND e.norm_manager = a.norm_manager)
  ) AS t1_strict_match,
  COUNT(DISTINCT a.id_private_fund) FILTER (
    WHERE EXISTS (SELECT 1 FROM erisa e WHERE e.norm_fund = a.norm_fund)
  ) AS t2_fund_match,
  COUNT(DISTINCT a.id_private_fund) AS total_adv_funds
FROM adv a
WHERE LENGTH(a.norm_fund) >= 5
GROUP BY 1
ORDER BY t2_fund_match DESC;


-- =============================================================================
-- Q4: Top-10 ERISA plan sponsors by # distinct private-fund holdings
-- =============================================================================

WITH e AS (
  SELECT DISTINCT id_lp_canonical, name_fund_norm, year_filing
  FROM read_parquet('/Users/alexbresler/Desktop/data/lake_lp_fund_commitments/**/*.parquet', union_by_name=true)
  WHERE source = 'erisa_5500_d_part1' AND year_filing = 2023
),
lp AS (
  SELECT DISTINCT id_lp_canonical, name_lp, state
  FROM read_parquet('/Users/alexbresler/Desktop/data/lake_lp_entities/**/*.parquet', union_by_name=true)
  WHERE name_lp IS NOT NULL AND year_filing = 2023
)
SELECT
  l.name_lp,
  l.state,
  COUNT(DISTINCT e.name_fund_norm) AS distinct_funds_held
FROM e
INNER JOIN lp l USING (id_lp_canonical)
GROUP BY 1, 2
ORDER BY distinct_funds_held DESC
LIMIT 10;
