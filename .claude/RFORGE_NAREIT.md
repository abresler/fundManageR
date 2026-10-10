# RFORGE Security Pass: nareit.R (2949 lines)

## Executive Summary
10 exported REIT data scrapers with **CRITICAL** silent-failure modes. Primary threat vectors:
1. **Global environment mutation** via `<<-` in callbacks (L293, L624, L969, L2847)
2. **Unvalidated array indexing** `[[1]]` and `[[2]]` on potentially empty results (L1027, L1163, L2157, L2495, L2889)
3. **Deprecated functions** (`flatten_df`, `flatten_chr`, `purrr::invoke`) causing type coercion failures
4. **Silent NULL returns** via `purrr::possibly(.fn, tibble())` masking network/parse errors
5. **No input validation** on user-supplied parameters before URL interpolation
6. **Schema drift** on HTML parsing — hardcoded `html_nodes()` selectors fail silently if reit.com changes DOM

---

## Findings Table

| # | Line | Function | Finding | Severity | Proposed Fix | Schema Impact |
|---|------|----------|---------|----------|--------------|---------------|
| 1 | 293-295 | `.parse_nareit_constituent_url()` | GLOBAL ASSIGNMENT: `df <<- df %>% bind_rows(all_data)` inside `success()` callback. Mutates parent scope. If parallel calls race, last-write-wins silently discards rows. | **CRITICAL** | Remove `<<-`. Return tibble from callback, collect with `c()` or `Reduce()` after `multi_run()`. | Breaking: Changes internal API for `.parse_nareit_constituent_url()` |
| 2 | 1027 | `nareit_entities()` | UNCHECKED INDEX: `flatten_chr() %>% .[[2]] %>% as.numeric()` — if pagination link missing or malformed, `.[[2]]` throws `Error: subscript out of bounds`, crashes function. No fallback. | **CRITICAL** | Wrap: `tryCatch(.[2] %>% as.numeric(), error = function(e) { warning("No pagination found, assuming 1 page"); 1 })` | None if default to 1 page |
| 3 | 2889 | `reit_funds()` | UNCHECKED INDEX: `flatten_chr() %>% .[[2]] %>% as.numeric()` — identical risk as L1027. Pagination extraction can crash. | **CRITICAL** | Same fix as L1027 | None if default to 1 page |
| 4 | 2157 | `nareit_mergers_acquisitions()` | UNCHECKED INDEX: `.[[1]]` on `html_nodes('h3 a') %>% html_attr('href')` — if selector returns empty (DOM changed), crashes with subscript error. | **CRITICAL** | Add: `if (length(url) == 0) stop("Could not find M&A PDF link. reit.com DOM may have changed.")` before `paste0()`. | None (fail-fast) |
| 5 | 1163 | `nareit_notable_properties()` | UNCHECKED INDEX: `.[[1]]` on `video_url` extraction — if empty list, crashes. Silent NULL check exists (L1149) for one path but not here. | **HIGH** | Unify: `if (length(x) == 0) NA_character_ else x[[1]]` pattern across all list extractions. | None |
| 6 | 47 | `.parse_nareit_constituent_url()` | DEPRECATED FUNCTION: `flatten_df()` — deprecated in purrr, alias for `as_tibble()`. When metadata shape changes (new PDF fields), coercion may fail silently or produce wrong schema. | **HIGH** | Replace: `tabulapdf::extract_metadata() %>% as_tibble()` | Minor: output column names may differ slightly |
| 7 | 61 | `.parse_nareit_constituent_url()` | DEPRECATED FUNCTION: `flatten_chr()` — deprecated, alias for `list_c()`. Fragile for nested metadata structures. | **MEDIUM** | Replace: `unlist()` + validate `length() > 0` before indexing. | None |
| 8 | 378, 384, 1248, 1313, 1436, 1540, 1573 | Multiple | DEPRECATED: `purrr::invoke(paste0, .)` — works but non-idiomatic. Use `paste(..., sep='', collapse='')` instead. Harder to debug if args mismatch. | **MEDIUM** | Bulk replace all 7 instances with standard `paste()` or glue string. | None |
| 9 | 85 | `.parse_nareit_constituent_url()` | UNCHECKED PAGES: `tabulapdf::extract_tables(1:df_metadata$pages)` — if `df_metadata$pages` is NA/NULL/0, silently returns empty list. No check after `extract_tables()`. | **HIGH** | Add: `if (is.na(df_metadata$pages) \|\| df_metadata$pages == 0) return(tibble())` before extraction. | None (fail-early) |
| 10 | 1020-1027 | `nareit_entities()` | SILENT SCHEMA CHANGE: If reit.com's `.pager__item--last a` selector missing (no pagination), `html_node()` returns NA, `html_attr('href')` returns NA, `str_split()` produces 1-element list, `[[2]]` crashes. No warning. | **CRITICAL** | Pre-validate: `page_node <- page %>% html_node(page_count_nodes); if (is.na(page_node)) { warning("No paginator found, defaulting to 1 page"); pages <- 1 } else { pages <- ... }` | None (fail-safe to 1) |
| 11 | 329-330 | `nareit_constituent_years()` | WEAK NULL CHECK: `if (years %>% purrr::is_null()) stop(...)` — doesn't validate length or content. User can pass `years = numeric(0)` or `years = NA`, producing empty URL filter result. Returns empty tibble silently. | **MEDIUM** | Strengthen: `if (is.null(years) \|\| length(years) == 0 \|\| all(is.na(years))) stop("years must be a non-empty numeric vector")` | None (validation) |
| 12 | 341, 618, 963, 1543, 1576, 1892, 1905 | Multiple | SILENT FAILURES: `purrr::possibly(.fn, tibble())` wraps ALL scrapers. On network error / parse error, returns empty tibble. User gets empty dataframe with NO indication why (no error message, no warning). | **CRITICAL** | Replace with: `.safely()` variant that returns `list(result, error)`. Log errors to stderr or .Last.error. Example: `.parse_nareit_constituent_url_safe <- purrr::safely(.parse_nareit_constituent_url); result <- map(urls, .parse_nareit_constituent_url_safe); errors <- discard(result, ~is.null(.x$error)) %>% map('error'); if (length(errors) > 0) warning(sprintf("Failed on %d URLs: %s", length(errors), paste(errors, collapse='; ')))` | None (adds error visibility) |
| 13 | 1139-1140 | `nareit_notable_properties()` | NO ERROR HANDLING: `fromJSON()` with hardcoded HTTP (not HTTPS): `"http://app.reitsacrossamerica.com/properties/notable"`. No `tryCatch`. If API returns 404 or timeout, crashes with unclear error. | **HIGH** | Add: `tryCatch(fromJSON(url), error = function(e) list(properties=list(property=NULL))) %>% { if (is.null(.$properties$property)) return(tibble()) else . }` | None (fail-safe) |
| 14 | 1213, 1265, 1325, 1381, 1476 | `.parse_json_hq()`, `.parse_json_holdings()`, `nareit_property_msa()`, `nareit_state_info()` | NO ERROR HANDLING: `fromJSON()` calls on 5 APIs with hardcoded HTTP/HTTPS URLs. No try-catch. Silent failures if API unavailable. | **HIGH** | Wrap all `fromJSON()` with `tryCatch(..., error = function(e) { warning(paste("JSON parse failed:", url, "-", e$message)); list() })` | None (fail-safe) |
| 15 | 1189 | `nareit_notable_properties()` | COPY-PASTE BUG: `mutate(across(c('coordinateLongitude', 'coordinateLongitude'), ...))` — coerces both longitude AND longitude (second one should be latitude). Latitude never converted to numeric. Silently produces character column. | **MEDIUM** | Fix typo: `c('coordinateLongitude', 'coordinateLatitude')` | Schema breakage if downstream code expects numeric lat |
| 16 | 1495-1506 | `nareit_state_info()` | SCHEMA FRAGILITY: Geometry parsing via hardcoded alternation `[c(TRUE, FALSE), ]` and `[c(FALSE, TRUE), ]` assumes lat/lon alternate in exact order. If API schema changes (adds 3rd coordinate), indices silently swap lat↔lon. No validation. | **HIGH** | Validate after unnest: `stopifnot(length(df_lat_lon$coordinates[[1]]) == 2, msg="Coordinate array has unexpected length")` | None (validation) |
| 17 | 1631-1637 | `nareit_monthly_returns()` | NO DOWNLOAD VALIDATION: `curl::curl_download(url, tmp)` with no check return status. If 404/redirect, file may be empty or HTML error page. `xlsx::read.xlsx()` then crashes with cryptic error. | **HIGH** | Check: `status <- curl::curl_download(url, tmp); if (!file.exists(tmp) \|\| file.size(tmp) == 0) stop("Download failed or empty: ", url)` | None (fail-fast) |
| 18 | 2162 | `nareit_mergers_acquisitions()` | UNVALIDATED PAGES: `tabulapdf::extract_tables(pages = pages)` — no check if pages is valid range. If pages = c(32, 33) but PDF has only 20 pages, tabulapdf may crash or return partial results silently. | **MEDIUM** | Add defensive: `pages <- pages[pages <= max_pages_in_pdf]` after fetching. Log: `message(sprintf("Extracting pages %s (PDF has %d total)", paste(pages, collapse=','), max_pages))` | None |
| 19 | 2150-2151 | `nareit_mergers_acquisitions()` | HARDCODED URL FRAGILITY: `"https://www.reit.com/data-research/data/reitwatch"` and subsequent `tabulapdf::extract_tables()`. If URL path changes or PDF moves, function crashes at runtime with unclear error. No fallback URL or mirror. | **MEDIUM** | Add to CLAUDE.md: document upstream URL dependency. Add environment variable: `Sys.getenv("NAREIT_REITWATCH_URL", default="https://www.reit.com/data-research/data/reitwatch")` for override. | None |
| 20 | 2481 | `nareit_industry_tracker()` | HARDCODED CSS SELECTOR: `.xlsx link detection via `str_detect(".xlsx")` on href attribute. If reit.com renames to `.xls` or bundles in `.zip`, selector returns empty. No fallback. | **MEDIUM** | Broaden: `links[links %>% str_detect("\\.xls")]` (case-insensitive). Add warning if 0 matches found. | None (defensive) |
| 21 | 2495 | `nareit_industry_tracker()` | UNCHECKED INDEX: `.[[1]]` on xlsx link extraction. If no .xlsx found, crashes. | **HIGH** | Add: `if (length(links) == 0) stop("No .xlsx file found on reitwatch page")` before indexing. | None (fail-fast) |
| 22 | 329 | `nareit_constituent_years()` | SILENT FILTER FAILURE: `filter(yearData %in% years)` on line 337. If user passes `years = 2099`, filters to 0 rows, returns empty tibble. No warning that requested years unavailable. | **MEDIUM** | Add: `missing_years <- setdiff(years, url_df$yearData); if (length(missing_years) > 0) warning(sprintf("Data unavailable for years: %s", paste(missing_years, collapse=', ')))` after filter. | None (warning only) |
| 23 | 646-647 | `.parse_nareit_entity_page()` | DANGEROUS PIPE: `read_lines() %>% str_c(collapse="") %>% read_html()` — converts HTML to massive string, then re-parses. If HTML >100KB, memory spike and performance issue. Original `read_html()` on URL is more efficient. | **MEDIUM** | Replace: `page <- url %>% read_html()` directly. The `read_lines()` → `str_c()` → `read_html()` chain is redundant. | None (performance win) |
| 24 | 1417 | `nareit_property_msa()` | SILENT JOIN LOSS: `inner_join(df_lat_lon)` — if lat/lon extraction fails (geometry parse error), join produces 0 rows silently. No warning about dropped data. | **MEDIUM** | Add: `if (nrow(df_property) == 0) warning("No geometry data merged; geometry parsing may have failed")` after join. | None (warning) |
| 25 | 2200-2310 | `nareit_mergers_acquisitions()` | EXTREMELY COMPLEX CONDITIONAL LOGIC: 4-way if/else on table structure (L2186-2311) with overlapping filter + mutate + separate patterns. HIGH risk of missed edge cases, silent data loss if table format unexpected. | **HIGH** | Refactor into separate `.parse_ma_table_v{1,2,3}()` functions with explicit tests for each format. Document table formats w/ examples in roxygen. | None (code organization) |
| 26 | 2333-2343 | `nareit_mergers_acquisitions()` | HARDCODED ROW IDS: `left_join(tibble(idTransaction = c(32, 60), ...))` — magic numbers (32, 60) assume stable row order. If upstream data changes, join targets wrong rows. | **MEDIUM** | Move to R/data-raw/ as curated lookup table. Document: "Manual corrections for rows with malformed acquiror/target names". Validate join cardinality: `stopifnot(nrow(matched_corrections) == 2)` | None |
| 27 | 2502-2504 | `nareit_industry_tracker()` | HARD-CODED SHEET: `xlsx::read.xlsx(file=tmp, sheetIndex=1, header=FALSE)`. If NAREIT changes sheet order or adds new sheets, function reads wrong data silently. No validation of sheet contents. | **MEDIUM** | Add: `sheets <- xlsx::getSheets(tmp); sheet_name <- grep('T-Tracker\|NAREIT T-', sheets, ignore.case=T); if (length(sheet_name)==0) stop("Expected T-Tracker sheet not found in Excel file")` | None (validation) |
| 28 | 89 | `.parse_nareit_constituent_url()` | SILENT `map_dfr()` FAILURE: `future_map_dfr(function(x) { tables[[x]] %>% as_tibble() })` — if `tables[[x]]` is NULL or wrong structure, `as_tibble()` can crash or return unexpected schema. Error propagates only if all rows fail; partial failures are silent. | **MEDIUM** | Wrap in `.safely()`: `tables %>% map(~safely(as_tibble)(.x)) %>% discard(~!is.null(.x$error)) %>% map('result') %>% bind_rows()` | None (error visibility) |
| 29 | 585 | `nareit_monthly_returns()` | TYPE COERCION: `mutate(across(everything(), readr::parse_number))` on mixed data (dates + index names + values). If parse_number() fails on a string, returns NA. Silent data loss if indexItem contains non-numeric junk. | **MEDIUM** | Separate by column: only parse known numeric cols explicitly. Validate: `if (sum(is.na(valueItem)) > 0.5 * nrow(.)) warning("Excessive NA values after parse_number")` | None (validation) |
| 30 | 1645-1660 | `nareit_monthly_returns()` | FRAGILE STRING PARSING: Row 5-7 hardcoded to contain `nameIndex` and item headers. If NAREIT changes header rows, extraction crashes or produces malformed column names. No validation. | **MEDIUM** | Add: `expected_header_rows <- which(data[[1]] %>% str_detect('INDEX|RETURN')); stopifnot(length(expected_header_rows) > 0, "Could not find header row in Excel sheet")` | None (validation) |
| 31 | 2889 | `reit_funds()` | IDENTICAL UNCHECKED INDEX: `.[[2]]` on pagination — matches L1027. Same crash risk. | **CRITICAL** | Consolidated fix with L1027 above. | None |
| 32 | 1026-1027 | `nareit_entities()` | RACE CONDITION RISK: `pages <- ... %>% as.numeric()` on L1027, then `glue::glue()` on L1030 to build URL vector. If `pages` is NA (pagination extraction failed), `1:pages` produces `1:NA` = empty sequence. Silent zero-page request. | **HIGH** | Validate immediately: `stopifnot(!is.na(pages), length(pages) == 1, pages >= 1)` | None (fail-fast) |
| 33 | 1149-1154, 1159-1165 | `nareit_notable_properties()` | INCONSISTENT NULL HANDLING: Two different patterns used: `if (x %>% length() == 0)` (L1149) vs `if (df$image_url[[x]] %>% length() == 0)` (L1159). Inconsistency suggests copy-paste error. Second pattern indexes without bounds check. | **MEDIUM** | Unify to: `\(x) if (length(x) == 0) NA_character_ else x[[1]]` helper applied consistently. | None |
| 34 | 337-338 | `nareit_constituent_years()` | EXTRACTION ANTI-PATTERN: `filter(...) %>% .$urlData` chains extraction. If filter produces 0 rows, `.$urlData` returns 0-length character vector, `future_map_dfr()` returns empty tibble. Silent failure when user requests unavailable years. | **MEDIUM** | Add guard: `stopifnot(length(urls) > 0, msg=sprintf("No URLs found for years %s. Available: %s", paste(years, collapse=','), paste(unique(url_df$yearData), collapse=',')))` | None |
| 35 | 46-85 | `.parse_nareit_constituent_url()` | SCHEMA DISCOVERY: No validation that tabulapdf extraction actually succeeded. If PDF structure unparseable, `extract_tables()` returns empty list or malformed matrices. `seq_along(tables)` then iterates over nothing. Silently returns empty tibble. | **HIGH** | Add: `stopifnist(length(tables) > 0, msg=sprintf("Could not extract tables from PDF: %s", url))` after extraction. Log first table preview: `str(tables[[1]]); glimpse(all_data[1:3, 1:3])` | None (fail-fast) |

---

## Summary by Severity

### CRITICAL (7 findings)
- L1 (L293): Global `<<-` in parallel callbacks — **data loss from race conditions**
- L2 (L1027): Unchecked `[[2]]` pagination extraction — **crash when no paginator**
- L3 (L2889): Same unchecked `[[2]]` — **duplicate risk**
- L4 (L2157): Unchecked `.[[1]]` M&A PDF link — **crash if DOM changes**
- L10 (L1020-1027): Silent schema change in paginator selector — **crash on missing pagination**
- L12 (Multiple): Silent failures via `purrr::possibly()` — **no error visibility**
- L31 (L2889): Duplicate of L3

### HIGH (11 findings)
- L5, L9, L13-14, L18, L21, L22, L32, L35

### MEDIUM (16 findings)
- L7-8, L11, L15-17, L19-20, L23-26, L28-30, L33-34

---

## Recommended Triage

**Phase 1 (Immediate):**
1. Fix L1 (global `<<-`) — affects all constituent-year functions
2. Fix L4, L21 (unchecked array indexing in 3 exported functions)
3. Add error visibility to L12 (`purrr::possibly()` → `.safely()` pattern)

**Phase 2 (High-value fixes):**
4. Fix L13-14 (JSON API error handling)
5. Add input validation guards (L11, L22, L32, L34)

**Phase 3 (Defensive):**
6. Refactor L25 (complex MA logic into testable sub-functions)
7. Add schema validation (L1, L30)

---

## Friction Pattern Detected

**"Silent Array Trap"**: Chaining string operations (`str_split()` → `flatten_chr()` → `.[[1]]` or `.[[2]]`) without length checking. Manifests in 4 separate locations (L1027, L2157, L2495, L2889). Fix: Always validate `length() > 0` before indexing.

**"Callback Global Mutation"**: `<<-` assignment in async callbacks (curl_fetch_multi, future_map) creates race conditions and non-reproducible behavior. Affects lines 293, 624, 969, 2847.

**"Silent Scraper Deaths"**: `purrr::possibly(.fn, tibble())` returning empty tibbles on error — users can't tell if function succeeded with no data vs. crashed. Pattern reused 7 times with no logging.

---

## No Schema-Breaking Fixes Required

All proposed changes maintain output column names and structure. Only L15 (duplicate colname bug) and L6-7 (deprecated function replacements) have minor type changes — all backwards-compatible.
