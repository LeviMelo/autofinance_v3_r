# README_data_engine

## Overview

This module is the **data engine** of a personal market analysis / portfolio optimization pipeline (R-based), focused on building a **clean, adjusted, auditable OHLCV panel** for B3-listed assets (equities, FIIs, ETFs, BDRs), with:

* raw price universe ingestion (via `rb3`)
* corporate actions ingestion (via Yahoo; chart API or `quantmod`)
* split validation / snapping against raw price gaps
* normalized event table construction (including manual overrides)
* split + dividend adjustment factor application
* debug/audit artifacts for traceability
* optional per-symbol diagnostic features (returns, vol, drawdown, Amihud)

This is not just a downloader; it is a **contract-driven ETL + adjustment engine** with explicit validation, caching, quarantine, and diagnostics.

---

## What this module produces

The main pipeline entrypoint (`de_run_data_engine`) returns a bundle containing:

* **Model-ready adjusted panel** (`panel_adj_model`)
* **Debug panel with raw + intermediate adjusted columns** (`panel_adj_debug`)
* **Normalized applied events** (`events_apply`)
* **Raw and corrected corp actions** (`corp_actions_raw`, `corp_actions_apply`)
* **Quarantined split events** (`corp_actions_quarantine`)
* **Split validation audit** (`split_audit`)
* **Candidate prefilter audit** (`prefilter_audit`)
* **Adjustment timeline (factor trail)** (`adjustments_timeline`)
* **Residual jump audit** (`residual_jump_audit`)
* **Optional diagnostics/features** (`features_diag`)
* **Meta/config/run info** (`meta`)

This is a strong design choice: the engine returns both the final output **and the provenance** needed to debug it.

---

## Architectural summary

### Pipeline stages (high-level)

1. **Build raw universe** from B3 market data (`rb3`)
2. **Select corporate-action candidates** using liquidity/trading coverage filters (+ optional forced symbols)
3. **Fetch corporate actions** (Yahoo chart API or quantmod), with validated caching
4. **Validate/snap Yahoo splits** against observed raw gaps
5. **Merge/manualize and normalize events** into a compact `events_apply` table
6. **Apply split and dividend adjustments** to OHLC
7. **Compute residual jump diagnostics** and assign adjustment states
8. **Optionally compute diagnostic features** (returns, volatility, drawdown, Amihud)

---

## Project structure (module map)

### `R/00_contracts.R`

Defines schema contracts and validators:

* required columns per table
* type checks
* domain checks (e.g., positive prices, allowed enums)
* duplicate key checks

### `R/01_config.R`

Default configuration, config validation, override merging, canonicalization, directory creation.

### `R/02_utils.R`

Helpers:

* package checks
* logging
* weekday/bizdays sequence helpers

### `R/03_providers.R`

Data providers:

* `rb3` setup/fetch (yearly/window)
* Yahoo symbol mapping
* Yahoo chart API events fetch
* Yahoo `quantmod` split/dividend fetch
* retry/backoff logic for rate limits

### `R/04_universe.R`

Transforms raw `rb3` output into a standardized raw universe:

* schema normalization
* liquidity proxy unification
* deduplication by `(symbol, refdate)`
* yearly/window builders

### `R/05_corp_actions.R`

Corporate actions pipeline:

* candidate selection + audit
* CA registry build with cache modes
* split gap validation / snapping
* event normalization + manual events integration

### `R/06_adjuster.R`

Core adjustment math:

* split factors
* dividend factors
* dividend “rescue” scaling
* adjusted panel + debug panel + audits

### `R/07_diagnostics_features.R`

Optional end-of-window symbol diagnostics:

* horizon returns
* realized vol
* drawdown / ulcer index
* Amihud illiquidity

### `R/08_pipeline.R`

Orchestrator (`de_run_data_engine`) that wires all steps together.

### `fixtures/manual_events_template.csv`

Template for manually supplied events (file listed, contents not shown in prompt).

### `tests/test_adjuster_cases.R`, `tests/test_smoke_data_engine.R`

Tests are listed, but contents were **not provided** here, so only their intent can be inferred from filenames.

---

## Core data contracts (schemas)

The module is explicitly contract-driven. `de_contracts` defines canonical column sets.

### 1) `universe_raw`

Raw standardized market data panel before adjustments.

Columns:

* `symbol`
* `refdate`
* `asset_type`
* `open`, `high`, `low`, `close`
* `turnover`
* `qty`

### 2) `corp_actions_raw`

Raw corporate actions registry (Yahoo/manual-like records before event aggregation).

Columns:

* `symbol`
* `yahoo_symbol`
* `refdate`
* `action_type` (`dividend` / `split`)
* `value`
* `source`

### 3) `events_apply`

Normalized event table used by the adjuster (one row per symbol/date):

* `split_value` (multiplicative price factor; e.g., 2:1 split => `0.5`)
* `div_cash` (cash dividend amount)
* source lineage flags

Columns:

* `symbol`
* `refdate`
* `split_value`
* `div_cash`
* `source_mask`
* `has_manual`

### 4) `panel_adj_model`

Final model-facing adjusted panel (compact).

Columns:

* `symbol`
* `refdate`
* `asset_type`
* `open`, `high`, `low`, `close` (adjusted)
* `turnover`
* `qty`
* `adjustment_state`

### 5) `panel_adj_debug`

Debug/audit panel preserving raw and intermediate columns.

Includes:

* raw OHLC (`open_raw`, `high_raw`, ...)
* split-adjusted OHLC (`open_adj_split`, ...)
* final adjusted OHLC (renamed back to `open/high/low/close`)
* `turnover`, `qty`
* `adjustment_state`

---

## Validation philosophy (important design feature)

`de_validate_contract()` is intentionally designed to be **pure**:

* it validates schema/types/domains/dupes
* it **does not sort rows** or enforce physical row order

This separation is very good engineering practice:

* validators check truth, not representation
* builders own ordering responsibilities

### What gets validated

* Presence of required columns
* Basic type constraints:

  * `symbol` must be character
  * `refdate` must be `Date`
* Price domain constraints:

  * OHLC > 0 (where present)
* `asset_type` enum (`equity`, `fii`, `etf`, `bdr`)
* `action_type` enum (`dividend`, `split`)
* CA value domains:

  * split values > 0, finite
  * dividend values >= 0, finite
* `events_apply` domains:

  * `split_value > 0`
  * `div_cash >= 0`
* Duplicate key check on `(symbol, refdate)` for most tables

  * **explicitly skipped for `corp_actions_raw`**, which can legitimately have multiple actions per symbol/date

---

## Configuration system (`R/01_config.R`)

The config layer is robust and well-validated.

### `de_config_default()`

Provides sensible defaults for:

* data scope (`years`, `include_types`)
* directories (`cache_dir`, `raw_dir`, `logs_dir`)
* liquidity filters
* CA prefilter diagnostics
* CA fetch/cache behavior
* split validation controls
* residual jump tolerance
* feature horizons

### Key config parameters (semantics)

#### Universe / selection

* `years`: defaults to current year and previous year
* `include_types`: asset classes to include
* `min_turnover`: minimum median turnover for CA candidate eligibility
* `min_days_traded_ratio`: minimum trading-day coverage ratio

#### CA prefilter diagnostics (not strict selection)

* `ca_prefilter_liq_window_days`
* `ca_prefilter_jump_log_thr`
* `ca_prefilter_gap_*` per asset type

These create audit flags like “has positive jump” / “has negative gap”.

#### Corporate actions fetch/cache

* `ca_fetch_mode`: `"chart"` or `"quantmod"`
* `ca_cache_mode`:

  * `"batch"`: cache one validated CA table per request hash
  * `"by_symbol"`: per-symbol exact window cache (debug/inspection-friendly)
  * `"none"`: no CA caching

#### Split validation / snapping

* `enable_split_gap_validation`
* `split_gap_tol_log`
* `split_gap_max_forward_days`
* `split_gap_max_back_days`
* `split_gap_use_open`

#### Post-adjustment diagnostics

* `adj_residual_jump_tol_log`

#### Feature engineering

* `feature_horizons_days` (default: 21, 63, 126, 252)

### `de_validate_config()`

Strict checks:

* enum validation
* logical flags
* directory strings
* numeric bounds
* integer windows/horizons
* optional `years` vector sanity

### `de_get_config(overrides)`

* merges user overrides safely (rejects unknown keys)
* validates config
* canonicalizes vectors (`unique`, sorted, integer-cast)
* creates required directories

This makes the engine safer for iterative experimentation.

---

## Utilities (`R/02_utils.R`)

Small but practical helper layer.

### Highlights

* `%||%`: NULL-coalescing operator
* `de_require(pkgs)`: explicit dependency gate with useful error message
* `de_log()`, `de_log_cfg()`: simple pipeline logging
* `de_weekdays_only()`: weekday filter
* `de_make_bizdays_seq()`: B3 business-day sequence via `bizdays::bizseq`, with error wrapping

---

## Provider layer (`R/03_providers.R`)

## 1) RB3 (B3 market data)

### `de_rb3_init(cfg)`

* sets `rb3` cache dir under `cfg$cache_dir/rb3`
* configures `options(rb3.cachedir=...)`
* attempts `rb3_bootstrap()`

### `de_rb3_fetch_yearly_lazy(year, cfg)`

* fetches yearly cotahist via `rb3`
* fallback path if `fetch_marketdata` fails
* returns filtered data for exact year window

### `de_rb3_fetch_window_lazy(start_date, end_date, cfg)`

* fetches daily cotahist for a business-day sequence
* fallback loops per date if bulk fetch fails
* returns requested date range only

**Note:** These functions return `rb3`/lazy outputs that are later standardized in universe builders.

---

## 2) Yahoo corporate actions

### `de_yahoo_symbol(symbol)`

Maps B3 ticker to Yahoo ticker:

* uppercases / trims
* appends `.SA` unless symbol already contains `.`

### `de_yahoo_with_retry(fun, ...)`

Retry wrapper with exponential backoff + jitter for rate limits:

* retries only for 429 / rate-limit-like errors
* returns `NULL` on non-retryable failure

### `de_yahoo_fetch_chart_events_one(yahoo_symbol, from, to)`

Direct Yahoo Chart API fetch (`events=div|splits`):

* uses `curl` + `jsonlite`
* parses dividends and splits
* converts split event into **price factor orientation**:

  * `pf = denominator / numerator`
  * e.g., 2:1 split => `1/2 = 0.5`
* returns typed `data.table`

### `de_yahoo_fetch_splits_quantmod_one()` / `de_yahoo_fetch_dividends_quantmod_one()`

Alternative fetch path via `quantmod`:

* splits and dividends fetched separately
* dividends call uses `split.adjust = TRUE`
* outputs standardized CA rows

---

## Universe building (`R/04_universe.R`)

This layer converts `rb3` outputs into a stable raw universe contract.

## Key strengths in this file

### 1) Schema resilience via column aliasing

`de_select_min_cols()` maps multiple possible column names to canonical columns:

* `symbol/ticker`
* `refdate/date`
* OHLC aliases (including Portuguese names)
* volume/quantity aliases

This is excellent defensive coding against provider schema variation.

### 2) Liquidity unification

`de_unify_liquidity()` creates:

* `qty`
* `turnover`

`turnover` is computed from:

1. `vol_fin` if available and valid
2. fallback `qty * close`

### 3) Global deduplication on `(symbol, refdate)`

`de_dedupe_symbol_date()`:

* sorts by `asset_type, symbol, refdate`
* if duplicates exist, keeps the “best” row using descending priority:

  * turnover
  * qty
  * close
* restores order

This is a pragmatic dedupe heuristic for noisy provider outputs.

### 4) Asset-type-specific processing

`de_process_rb3_lazy_df()`:

* applies `rb3::cotahist_filter_*` per included type
* collects each subset
* standardizes and tags `asset_type`
* removes invalid rows (`close > 0`, non-missing symbol/date)

### Builders

* `de_build_universe_year(year, include_types, cfg)`
* `de_build_universe_window(start_date, end_date, cfg)` ✅ includes dedupe + validation
* `de_build_universe(years, cfg)` ✅ merges years, dedupes globally, validates

---

## Corporate actions and events (`R/05_corp_actions.R`)

This is one of the most important files in the module. It does more than fetching—it performs **selection, cache hygiene, validation, split sanity checks, and event normalization**.

---

## A) Candidate selection (`de_select_ca_candidates`)

Goal: avoid fetching CA for every symbol in the universe.

### Selection logic (strict)

Based on recent liquidity/trading coverage:

* median turnover >= `min_turnover`
* traded-day ratio >= `min_days_traded_ratio`

### Force-in override

`force_symbols` are sanitized and included regardless of liquidity filters.

### Diagnostic audit (not strict filtering)

Computes symbol-level booleans:

* `has_positive_jump`
* `has_negative_gap`

Using:

* log-return threshold for positive jumps
* asset-type-specific simple-return gap thresholds for negative gaps

Returns:

* `candidates`: symbols selected for CA fetch
* `prefilter_audit`: symbol-level audit table (includes forced and non-forced)

This separation of **selection vs audit flags** is very good design.

---

## B) CA registry build (`de_build_ca_registry`)

This function builds the raw corporate actions table for candidate symbols.

### Notable design features

#### 1) Strictly typed empty tables

It defines `empty_ca_tbl()` and uses it consistently.
This avoids downstream breakage in no-event / no-candidate paths.

#### 2) Validated cache loading

Both batch and symbol caches are:

* read
* normalized
* **validated against contract**
  before reuse.

Corrupt or stale caches are automatically rebuilt.

#### 3) Cache modes

* **batch**: one file for the whole request hash `(symbols, from, to, fetch_mode)`
* **by_symbol**: one file per symbol + exact window hash
* **none**: no cache usage

#### 4) Empty-result caching

Even empty results are cached (including typed schema) to prevent refetch storms.

#### 5) Stale extra-column hygiene

`normalize_ca_tbl()` drops extra columns from caches (e.g., accidental debug leakage) and restores canonical column order.

---

## C) Split validation / snapping (`de_fix_yahoo_splits_by_raw_gap`)

This is a standout feature. Yahoo split dates/factors can be noisy or misoriented; this function validates them against raw price action.

### What it does

For Yahoo split rows:

1. Looks around vendor date in a configurable window:

   * backward days: `split_gap_max_back_days`
   * forward days: `split_gap_max_forward_days`
2. Computes observed gap ratio using:

   * `open / previous close` (if enabled)
   * else `close / previous close`
3. Compares observed ratio to **both**:

   * `value`
   * `1/value`
4. Chooses best match by minimum `abs(log(obs) - log(candidate))`
5. Classifies split:

   * `kept` (good match within tolerance)
   * `unverified` (no match found, but not automatically dropped)
   * `rejected` (bad mismatch)
   * `dup` (duplicate validated split same symbol/effective date/value)

### Outputs

* `corp_actions_apply`: corrected CA set for application
* `split_audit`: detailed evaluation results
* `quarantine`: rejected/duplicate splits

### Why this matters

It protects the adjusted panel from:

* wrong effective dates
* inverted split factors
* duplicate vendor events
* spurious split entries

This is exactly the kind of robustness a production-quality adjustment engine needs.

---

## D) Event normalization (`de_build_events`)

Converts row-level CA records (possibly multiple rows per day) into one normalized event row per `(symbol, refdate)`.

### Behavior

* optionally appends `manual_events` if enabled
* deduplicates exact duplicate CA rows
* aggregates:

  * splits by product (`prod(value)`)
  * dividends by sum (`sum(value)`)
* constructs `source_mask`
* normalizes repeated source labels (e.g., `yahoo+yahoo -> yahoo`)
* sets `has_manual`
* validates final `events_apply` contract

### Event semantics

* `split_value`: multiplicative **price factor**

  * no split => `1`
* `div_cash`: cash dividend

  * no dividend => `0`

### Manual events

The fixture suggests a template exists (`fixtures/manual_events_template.csv`), but its exact column contents were not shown. Based on the code path, manual events are expected to be compatible with CA rows (at least `symbol`, `refdate`, `action_type`, `value`, and optionally `source`/`yahoo_symbol`).

---

## Adjustment engine (`R/06_adjuster.R`)

This is the mathematical core of the module.

## Helper: reverse cumulative product (exclusive)

`de_rev_cumprod_exclusive(x)` computes the product of **future** factors only (exclusive of current row), per symbol.

This is the right tool for building **backward adjustment factors** that scale historical prices to the most recent basis.

---

## `de_apply_adjustments(universe_raw, events_apply, cfg)`

Merges events into raw universe and computes adjustment factors.

### Step 1 — merge and fill defaults

For rows without events:

* `split_value = 1`
* `div_cash = 0`
* `source_mask = "none"`
* `has_manual = FALSE`

### Step 2 — split adjustment

Per symbol:

* `split_factor_cum = reverse_cumprod_exclusive(split_value)`

Then:

* `*_adj_split = *raw * split_factor_cum`

This aligns pre-split history to post-split scale.

### Step 3 — dividend adjustment

Uses split-adjusted close to compute dividend factor events.

* `close_prev = lag(close_adj_split)`
* initialize:

  * `div_factor_event = 1`
  * `div_cash_eff = div_cash`

#### Dividend rescue (basis mismatch handling)

If `div_cash >= close_prev` on a dividend row (which is usually invalid), the code attempts a **rescue**:

* scales dividend by `split_factor_cum`
* accepts rescue if scaled dividend is valid (`0 <= scaled < close_prev`)

This is a smart practical fix for mixed vendor conventions (split-adjusted vs non-adjusted dividend values).

#### Boundary handling (truncated windows)

First visible row per symbol may have a dividend but no `close_prev`:

* flagged as `issue_div_boundary = TRUE`
* not treated as a generic invalid dividend issue

#### Bad dividend rows

Rows are flagged `issue_div = TRUE` if dividend factor cannot be safely computed (excluding boundary case).

#### Dividend factor event

For valid dividend rows:

* `div_factor_event = (close_prev - div_cash_eff) / close_prev`

Then:

* `div_factor_cum = reverse_cumprod_exclusive(div_factor_event)`

### Step 4 — final factor and adjusted OHLC

* `adj_factor_final = split_factor_cum * div_factor_cum`
* final adjusted OHLC = raw OHLC × `adj_factor_final`

### Step 5 — timeline audit

Returns a row-level `adjustments_timeline` containing:

* event inputs
* effective dividend value
* split/dividend cumulative factors
* final factor
* source lineage
* issue flags

This is excellent for postmortem debugging.

---

## `de_build_panel_adjusted(...)`

Wraps the adjustment computation and constructs final outputs.

### Adjustment state classification

Starts at symbol level using events and issue flags:

* `no_actions`
* `dividend_only`
* `split_only`
* `split_dividend`
* `manual_override`

### Residual jump safety net

Even after adjustments, the engine checks for suspicious jumps:

* computes max absolute log-return on adjusted close per symbol
* flags if `>= adj_residual_jump_tol_log`

Creates:

* `residual_jump_audit`
* symbol-level residual jump date and magnitude

### Final suspect states

State may be overwritten to:

* `suspect_dividend_factor`
* `suspect_residual_jump`
* `suspect_both`

This is useful because it distinguishes:

* event topology (split/dividend/manual)
  from
* quality/suspicion outcome.

### Two-panel output design

* `panel_adj_model`: compact, model-facing
* `panel_adj_debug`: raw + intermediate + final adjusted columns

This split is a strong pattern for quantitative pipelines.

---

## Diagnostic features (`R/07_diagnostics_features.R`)

Optional, lightweight symbol-level diagnostics computed from `panel_adj_model`.

## `de_compute_symbol_diag_features(dt_sym, horizons_days)`

Requires at least 30 observations; otherwise returns `NULL`.

### Computes

For the last row (end-of-window snapshot):

* `ret_{h}d` for each horizon
* `vol_cc_{h}d` (annualized SD of log close-close returns)
* `max_dd` (max drawdown)
* `ulcer_index`
* `amihud` (mean `|ret| / turnover`)

### Notes

* Uses adjusted close from `panel_adj_model`
* `amihud` uses simple returns and `turnover > 0`
* returns a **single-row** summary per symbol

## `de_build_features_diag(panel_adj_model, cfg)`

* orders by `(symbol, refdate)`
* trims each symbol to only the necessary tail window (`max_horizon + 1`)
* applies per-symbol feature computation
* returns consolidated feature table

This is intentionally diagnostic/QA-oriented, not yet a full alpha feature factory.

---

## Main entrypoint (`R/08_pipeline.R`)

## `de_run_data_engine(...)`

### Supported input modes (mutually exclusive)

1. **Year mode**

   * `years = ...`
2. **Window mode**

   * `start_date`, `end_date`

The function explicitly rejects invalid combinations:

* years + window together ❌
* only one of start/end ❌
* start > end ❌

### Parameters

* `force_symbols`: ensure CA fetch for specified symbols
* `manual_events`: optional manual CA/event rows
* `overrides`: config overrides list
* `build_diag_features`: toggle diagnostics features

### Execution sequence

1. Validate input mode
2. Build and log config
3. Build raw universe
4. Select CA candidates
5. Fetch CA registry
6. Validate/snap splits (optional)
7. Build normalized events
8. Apply adjustments / build panels
9. Build diagnostic features (optional)
10. Return bundle

### Output bundle (returned list)

* `panel_adj_model`
* `panel_adj_debug`
* `events_apply`
* `corp_actions_raw`
* `corp_actions_apply`
* `corp_actions_quarantine`
* `split_audit`
* `prefilter_audit`
* `adjustments_timeline`
* `residual_jump_audit`
* `features_diag`
* `meta` (config, run timestamp, row counts)

---

## Engineering strengths of this module

## 1) Contract-driven design

Schemas are explicit and enforced early.

## 2) Auditable by construction

It returns debug tables, timelines, split audits, quarantines, and residual jump diagnostics.

## 3) Practical resilience to vendor issues

* Yahoo retries
* split date/factor snapping
* dividend basis rescue
* validated cache reuse
* empty typed caches

## 4) Clear separation of concerns

Providers, normalization, validation, adjustment math, diagnostics, orchestration are separated across files.

## 5) Model vs debug outputs

Prevents contaminating model consumers with debug columns while preserving full traceability.

---

## Important implementation nuances / caveats

## 1) `corp_actions_raw` duplicates are allowed

This is intentional because multiple actions can occur on the same symbol/date.

## 2) Sorting is not a validator concern

Builders must ensure row ordering; validators stay pure.

## 3) Adjustment applies to OHLC only

`turnover` and `qty` are carried through but not adjusted.

## 4) Dividend boundary rows

Dividend events on the first visible row of a truncated window are flagged (`issue_div_boundary`) because prior close is unavailable.

## 5) `manual_override` vs suspect states

`manual_override` is assigned initially, but later suspect labels (`suspect_*`) can overwrite the final `adjustment_state` if issues are detected. This is not necessarily wrong, but it is a semantic choice worth documenting.

## 6) Feature layer is diagnostic, not full research feature engine

It computes one snapshot row per symbol (end-of-window), not a rolling time-indexed feature matrix.

---

## Dependency expectations (inferred from code)

Core packages referenced across files:

* `data.table`
* `dplyr`

Optional/feature/provider packages:

* `rb3`
* `bizdays`
* `digest`
* `jsonlite`
* `curl`
* `quantmod`
* `zoo`

The engine uses explicit `de_require()` checks in many paths, which helps fail fast.

---

## Inferred usage examples (README-ready)

### Example 1 — Year-based build

```r
res <- de_run_data_engine(
  years = 2024:2025,
  overrides = list(
    min_turnover = 1e6,
    ca_fetch_mode = "chart",
    ca_cache_mode = "batch"
  )
)

panel <- res$panel_adj_model
```

### Example 2 — Date-window build with forced symbols and manual events

```r
manual_events <- data.table::data.table(
  symbol = c("PETR4"),
  refdate = as.Date(c("2025-03-10")),
  action_type = c("dividend"),
  value = c(0.72),
  source = c("manual")
)

res <- de_run_data_engine(
  start_date = "2025-01-01",
  end_date   = "2025-12-31",
  force_symbols = c("PETR4", "VALE3"),
  manual_events = manual_events,
  overrides = list(
    enable_manual_events = TRUE,
    enable_split_gap_validation = TRUE
  )
)
```

---

## What the tests/fixtures likely cover (inference only)

The file names suggest:

* `test_adjuster_cases.R`: adjustment math edge cases (splits/dividends/boundaries/suspects)
* `test_smoke_data_engine.R`: end-to-end pipeline smoke execution
* `fixtures/manual_events_template.csv`: template for manual corporate actions/events ingestion

However, since their contents were not included in the prompt, this README summary cannot document exact test assertions or fixture schema line-by-line.

---

## Recommended future README additions (if you want to harden documentation further)

If you plan to make this module reusable by others (or future-you), add:

1. **Concrete schemas for manual events**

   * exact required/optional columns
   * examples for split and dividend rows

2. **Split factor convention note (very explicit)**

   * “price factor orientation” examples:

     * 2:1 split → `0.5`
     * 1:2 reverse split → `2.0`

3. **State machine documentation**

   * all `adjustment_state` values + precedence semantics

4. **Known Yahoo quirks**

   * date shifts, factor inversions, missing events, rate-limit behavior

5. **Performance notes**

   * expected runtime by universe size
   * when to use `batch` vs `by_symbol` cache mode

6. **Reproducibility notes**

   * cache invalidation strategy
   * dependency versions
   * calendar assumptions (`Brazil/B3`)

---

## Bottom line

This is a **well-structured, robust data-adjustment module** for a quant pipeline, with unusually strong attention to:

* schema contracts
* vendor error handling
* auditability
* adjustment correctness diagnostics
* operational caching hygiene

It already reads like a serious internal production component rather than a prototype script collection.

If you want, I can also turn this into a **more formal README format** with:

* installation section
* dependency matrix
* function reference index
* output schemas as explicit markdown tables
* “design rationale” and “failure modes” sections.
