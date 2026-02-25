## `DOCUMENTATION_data_engine.md` (technical reference / developer docs)

# data_engine — Technical Documentation (Developer Reference)

## Scope of this document

This document is the deeper technical reference for the `data_engine` module.

It complements `README_data_engine.md` by focusing on:
- internal architecture and data flow
- exact contracts and semantics
- adjustment math behavior
- audit artifacts and failure modes
- extension points and engineering constraints

This is the document you keep open while modifying the engine.

---

## 1) Architectural role in the project

`data_engine` is the **upstream ETL + adjustment layer**.

### Responsibilities
- ingest B3 data (via `rb3`)
- standardize schema and semantics
- normalize liquidity/activity fields
- fetch and sanitize corporate actions
- validate/snap Yahoo split events to raw price gaps
- build normalized event table
- apply split/dividend adjustments
- generate audit/debug artifacts
- produce model-facing adjusted panel

### Non-responsibilities (by design)
- strategy investability policies
- no-lookahead simulation semantics
- portfolio optimization
- execution/cost modeling
- backtest accounting

That separation must be preserved.

---

## 2) File-level architecture

## `R/00_contracts.R`
Defines:
- `de_contracts` schema contracts
- `de_assert_cols()`
- `de_assert_no_dupes()`
- `de_enforce_types_and_domains()`
- `de_validate_contract()`

Key design choice:
- validation is **pure** (no sorting or mutation side effects)

---

## `R/01_config.R`
Defines:
- `de_config_default()`
- `de_validate_config()`
- `de_get_config(overrides = NULL)`

Responsibilities:
- default config
- strict override validation
- enum/domain checks
- directory creation
- vector canonicalization (sort/unique/int-cast)

---

## `R/02_utils.R`
Helpers:
- `%||%`
- `de_require()`
- logging helpers (`de_log`, `de_log_cfg`)
- weekday helper
- B3 business-day sequence via `bizdays`

---

## `R/03_providers.R`
Provider layer:
- `rb3` init + fetch (year/window)
- Yahoo symbol mapping
- Yahoo chart API CA fetch
- Yahoo `quantmod` split/dividend fetch
- retry/backoff behavior for rate limits

Important operational detail:
- `rb3` is attached explicitly due to registry initialization behavior.

---

## `R/04_universe.R`
Raw universe builder:
- schema normalization (`de_select_min_cols`)
- liquidity/activity normalization (`de_unify_liquidity`)
- dedupe on `(symbol, refdate)` (`de_dedupe_symbol_date`)
- per-asset-type processing (`de_process_rb3_lazy_df`)
- year/window builders and validators

This file is where COTAHIST field semantics are standardized.

---

## `R/05_corp_actions.R`
Corporate actions pipeline:
- CA candidate selection (`de_select_ca_candidates`)
- CA registry build + cache validation (`de_build_ca_registry`)
- split validation/snapping (`de_fix_yahoo_splits_by_raw_gap`)
- event normalization and manual merge (`de_build_events`)

This is a major robustness layer.

---

## `R/06_adjuster.R`
Adjustment engine:
- reverse cumulative factor helper
- split factor application
- dividend factor application (+ rescue logic)
- adjustment timeline audit
- `panel_adj_model`, `panel_adj_debug`, `residual_jump_audit`

This is the mathematical core.

---

## `R/07_diagnostics_features.R`
Optional symbol-level diagnostic features:
- returns
- realized volatility
- drawdown
- ulcer index
- Amihud
- average trade size metrics

Outputs one row per symbol (end-of-window snapshot).

---

## `R/08_pipeline.R`
Orchestrator:
- input mode validation
- config build/log
- universe build
- CA candidate selection + fetch + split validation
- event normalization
- adjustment build
- optional diagnostics
- output bundle assembly

Main entrypoint: `de_run_data_engine(...)`

---

## 3) End-to-end data flow (actual pipeline)

### Stage 0 — Input mode + config
`de_run_data_engine(...)`
- validates year/window mode
- builds config via `de_get_config(overrides)`
- logs config

### Stage 1 — Raw universe build
- `de_build_universe(...)` **or** `de_build_universe_window(...)`
- `rb3` fetch
- per-type filtering (equity/fii/etf/bdr)
- schema normalization and liquidity unification
- global dedupe and contract validation

Output:
- `universe_raw`

### Stage 2 — CA candidate selection
`de_select_ca_candidates(universe_raw, cfg, force_symbols)`
- recent liquidity/coverage filters
- optional `force_symbols`
- produces `prefilter_audit`

Outputs:
- CA candidates (symbols)
- `prefilter_audit`

### Stage 3 — CA registry build
`de_build_ca_registry(candidates, from, to, cfg)`
- batch or per-symbol cache lookup
- cache validation and normalization
- Yahoo fetch (chart or quantmod)
- typed empty caching supported

Output:
- `corp_actions_raw`

### Stage 4 — Split validation / snapping (optional)
`de_fix_yahoo_splits_by_raw_gap(...)`
- evaluates Yahoo split events against observed raw gaps
- corrects effective dates and factor orientation when supported
- rejects/quarantines bad/duplicate split entries

Outputs:
- `corp_actions_apply`
- `split_audit`
- `corp_actions_quarantine`

### Stage 5 — Event normalization
`de_build_events(corp_actions_apply, manual_events, cfg)`
- merges manual events (if enabled)
- collapses row-level actions into one row per `(symbol, refdate)`
- creates `split_value`, `div_cash`, `source_mask`, `has_manual`

Output:
- `events_apply`

### Stage 6 — Adjustments
`de_build_panel_adjusted(universe_raw, events_apply, cfg)`
- split factors
- dividend factors (with rescue handling)
- final adjusted OHLC
- residual jump audit
- adjustment state labeling

Outputs:
- `panel_adj_model`
- `panel_adj_debug`
- `adjustments_timeline`
- `residual_jump_audit`

### Stage 7 — Optional diagnostics
`de_build_features_diag(panel_adj_model, cfg)`

Output:
- `features_diag` (or `NULL` if disabled)

### Stage 8 — Bundle return
The pipeline returns all outputs + meta information.

---

## 4) Contracts and schema semantics

The engine is contract-driven via `de_contracts`.

Important distinction:
- **Contract columns** = required minimum schema
- **Actual returned objects** may contain additional columns

---

## 4.1 `universe_raw` (contract + typical extras)

### Contract-required columns
- `symbol`
- `refdate`
- `asset_type`
- `open`, `high`, `low`, `close`
- `traded_value`
- `traded_units`
- `n_trades`
- `turnover`
- `qty`

### Typical additional columns currently produced (from `de_select_min_cols`)
Raw quote extras:
- `average_raw`
- `best_bid_raw`
- `best_ask_raw`

Metadata:
- `isin`
- `instrument_market`
- `corporation_name`
- `specification_code`
- `bdi_code`
- `trading_currency`
- `days_to_settlement`
- `allocation_lot_size`

### Semantics
- `traded_value`: monetary traded value (BRL)
- `traded_units`: quantity traded (units/contracts)
- `n_trades`: number of trades
- `turnover`: legacy alias of `traded_value`
- `qty`: legacy alias of `traded_units`

---

## 4.2 `corp_actions_raw` (and `corp_actions_apply` contract)

### Required columns
- `symbol`
- `yahoo_symbol`
- `refdate`
- `action_type` (`dividend` / `split`)
- `value`
- `source`

### Domains
- split values: finite and `> 0`
- dividend values: finite and `>= 0`

### Duplicate-key behavior
`(symbol, refdate)` duplicates are allowed because multiple actions can occur on the same date.

---

## 4.3 `events_apply`

Normalized, one-row-per-symbol-date event table used by the adjuster.

### Required columns
- `symbol`
- `refdate`
- `split_value`
- `div_cash`
- `source_mask`
- `has_manual`

### Semantics
- `split_value`: multiplicative **price factor**
  - no split = `1`
  - 2:1 split = `0.5`
  - reverse split 1:2 = `2.0`
- `div_cash`: total cash dividend aggregated on that date
- `source_mask`: collapsed source lineage string (e.g., `"yahoo"`, `"manual+yahoo"`)
- `has_manual`: whether any manual event contributed to that row

---

## 4.4 `panel_adj_model`

Compact downstream-facing adjusted panel.

### Contract-required columns
- `symbol`
- `refdate`
- `asset_type`
- adjusted `open`, `high`, `low`, `close`
- `traded_value`, `traded_units`, `n_trades`
- `turnover`, `qty`
- `adjustment_state`

### Intended use
This is the canonical input for downstream model/backtest engines.

---

## 4.5 `panel_adj_debug`

Debug/audit panel (raw + intermediate + final adjusted data trail).

### Contract-required columns
- `symbol`, `refdate`, `asset_type`
- `open_raw`, `high_raw`, `low_raw`, `close_raw`
- `open_adj_split`, `high_adj_split`, `low_adj_split`, `close_adj_split`
- final adjusted `open`, `high`, `low`, `close`
- `traded_value`, `traded_units`, `n_trades`
- `turnover`, `qty`
- `adjustment_state`

### Typical additional columns
Because the debug panel starts from the full working table, it may also contain:
- event columns (`split_value`, `div_cash`, `source_mask`, `has_manual`)
- factor internals (`split_factor_cum`, `div_factor_cum`, etc.)
- issue flags (`issue_div`, `issue_div_boundary`)
- raw quote extras + adjusted quote extras (`average_*`, `best_bid_*`, `best_ask_*`)
- metadata columns from `universe_raw`

Use the debug panel as a forensic table.

---

## 5) Liquidity semantics and downstream implications

This is the most important semantic correction introduced in recent iterations.

### Canonical field mapping

- `traded_value` ↔ B3 monetary traded value (VOLTOT-like semantics)
- `traded_units` ↔ B3 quantity traded (QUATOT-like semantics)
- `n_trades` ↔ B3 trade count / print count (TOTNEG-like semantics)

### Legacy aliases
- `turnover := traded_value`
- `qty := traded_units`

### Common mistake to avoid
Do **not** treat `qty` as trade count.

If a downstream metric needs number of trades (e.g., average trade size denominator), use `n_trades`.

### Diagnostic feature implications (current code)
`de_compute_symbol_diag_features()` now supports:
- `avg_trade_size_units = mean(traded_units / n_trades)`
- `avg_trade_size_brl = mean(traded_value / n_trades)`

Amihud uses `traded_value` (preferred) or `turnover` fallback.

---

## 6) Validation philosophy and invariants

## `de_validate_contract()` behavior
Validation includes:
- required columns present
- type checks (`symbol`, `refdate`)
- domain checks (prices > 0, nonnegative activity)
- enums (`asset_type`, `action_type`)
- duplicate key checks for most tables

### Explicit exception
No duplicate-key enforcement for `corp_actions_raw`.

### Intentional non-behavior
No row sorting checks / no reordering.
Builders must manage ordering.

This preserves validator purity and avoids hidden side effects.

---

## 7) Universe builder internals (COTAHIST normalization)

## `de_select_min_cols()`
This function is schema-resilient and supports multiple aliases for:
- identity/date
- OHLC
- activity fields
- optional quote extras
- metadata columns

It standardizes into a unified table before downstream processing.

### Activity staging fields extracted
- `traded_value_raw`
- `traded_units_raw`
- `n_trades_raw`

These are then normalized in `de_unify_liquidity()`.

---

## `de_unify_liquidity()`
Produces canonical fields:
- `traded_units := traded_units_raw`
- `n_trades := n_trades_raw`
- `traded_value := traded_value_raw` if valid, else fallback `traded_units * close`

Then creates legacy aliases:
- `turnover := traded_value`
- `qty := traded_units`

Raw staging fields are dropped afterwards.

### Practical note
The fallback for `traded_value` is only used when source monetary volume is unavailable/invalid.

---

## `de_dedupe_symbol_date()`
Dedupes `(symbol, refdate)` rows using descending quality priority:
1. `traded_value`
2. `traded_units`
3. `n_trades`
4. `close`

This is a pragmatic provider-noise heuristic.

---

## 8) Corporate actions pipeline internals

## 8.1 CA candidate selection (`de_select_ca_candidates`)
CA fetch is restricted to a candidate set to reduce cost and noise.

### Selection criteria
Uses recent window (`ca_prefilter_liq_window_days`) and selects symbols with:
- `median(turnover) >= min_turnover`
- traded-day coverage ratio `>= min_days_traded_ratio`

### Force-in support
`force_symbols` bypass eligibility filters.

### Diagnostic audit
Creates `prefilter_audit` with:
- `is_candidate`
- `is_forced`
- `has_positive_jump`
- `has_negative_gap`

These diagnostic flags do not directly define eligibility (except forced symbols).

---

## 8.2 CA registry build (`de_build_ca_registry`)
This function is built for operational resilience.

### Cache modes
- `"batch"`: one validated CA table per request hash
- `"by_symbol"`: one cache per symbol/request hash
- `"none"`: bypass cache

### Cache hygiene
On read:
- load cache
- normalize schema
- validate contract
- drop stale extra columns if present

If invalid/corrupt:
- log and rebuild

### Empty result caching
Typed empty tables are cached to prevent repeated refetches for symbols with no events.

---

## 8.3 Split validation/snapping (`de_fix_yahoo_splits_by_raw_gap`)
Yahoo split rows can be noisy (wrong date, inverted factor, duplicates).

This function validates each Yahoo split against observed raw gaps in `universe_raw`.

### Search procedure
For each split:
- inspect date offsets from `-split_gap_max_back_days` to `+split_gap_max_forward_days`
- compute observed gap ratio from:
  - `open / prev_close` (if `split_gap_use_open = TRUE`)
  - else `close / prev_close`
- compare to both `value` and `1/value`
- minimize absolute log error

### Split audit statuses
- `kept`: matched within tolerance and possibly snapped/reoriented
- `unverified`: no usable comparison found; retained without confirmation
- `rejected`: bad mismatch
- `dup`: duplicate of already-kept split (same symbol/effective date/value)

### Outputs
- `corp_actions_apply` (kept + unverified splits + all non-split CA rows)
- `split_audit` (detailed evaluation)
- `quarantine` (`rejected` + `dup`)

---

## 8.4 Event normalization (`de_build_events`)
Converts row-level CA records into one-row-per-symbol-date event table.

### Aggregation logic
Per `(symbol, refdate)`:
- split rows → `split_value = prod(value)`
- dividend rows → `div_cash = sum(value)`

### Source lineage
- combines split/dividend source labels
- deduplicates repeated labels (`"yahoo+yahoo"` → `"yahoo"`)
- sets `has_manual` via string match on `source_mask`

### Validation
Final `events_apply` is validated via contract and domain checks.

---

## 9) Adjustment engine internals

## 9.1 Factor orientation and cumulative direction
The engine applies **backward adjustments** (historical prices scaled to recent basis).

It uses `de_rev_cumprod_exclusive()` to compute cumulative products of future event factors (exclusive of current row).

This is the correct pattern for backward-adjusted series.

---

## 9.2 Split adjustment (`de_apply_adjustments`)
For each symbol:
- `split_factor_cum = reverse_cumprod_exclusive(split_value)`
- `OHLC_adj_split = OHLC_raw * split_factor_cum`

This aligns pre-split history.

---

## 9.3 Dividend adjustment and basis mismatch rescue
Dividend adjustment is applied on top of split-adjusted prices.

### Standard dividend factor event
For a valid dividend row:
- `div_factor_event = (close_prev - div_cash_eff) / close_prev`

Then:
- `div_factor_cum = reverse_cumprod_exclusive(div_factor_event)`

### Basis mismatch rescue
If a dividend amount is implausibly large (`div_cash >= close_prev`) and the symbol has split scaling (`split_factor_cum < 1`), the engine tries:
- `scaled_div = div_cash * split_factor_cum`

If valid, it replaces `div_cash_eff`.

This handles mixed vendor basis conventions (split-adjusted vs non-adjusted dividend amount).

---

## 9.4 Boundary dividend handling (truncated windows)
If the first visible row of a symbol in the requested window contains a dividend:
- `close_prev` is unavailable
- row is flagged `issue_div_boundary = TRUE`
- not treated as a generic invalid dividend factor row

This is important for window mode runs.

---

## 9.5 Final factor and adjusted OHLC
Final adjustment factor:
- `adj_factor_final = split_factor_cum * div_factor_cum`

Final adjusted prices:
- `OHLC_adj_final = OHLC_raw * adj_factor_final`

Optional quote extras are also adjusted if present:
- `average_raw`, `best_bid_raw`, `best_ask_raw`

---

## 9.6 `adjustments_timeline` audit
This row-level audit table includes:
- split/dividend event inputs
- effective dividend (`div_cash_eff`)
- cumulative split/dividend factors
- final factor
- source lineage
- manual flags
- issue flags (`issue_div`, `issue_div_boundary`)

This is the primary table for debugging adjustment math.

---

## 10) Adjustment state labeling and residual jump safety net

## 10.1 Initial symbolic state classification
At symbol level, `de_build_panel_adjusted()` derives initial labels:
- `no_actions`
- `dividend_only`
- `split_only`
- `split_dividend`
- `manual_override`

---

## 10.2 Residual jump audit
Even after adjustments, the engine checks adjusted-close residual jumps.

For each symbol:
- computes `max(abs(diff(log(close_adj_final))))`
- stores magnitude and date
- flags if `>= adj_residual_jump_tol_log`

Output:
- `residual_jump_audit`

---

## 10.3 Final suspect state overrides
Final state may be overwritten by:
- `suspect_dividend_factor`
- `suspect_residual_jump`
- `suspect_both`

### Precedence detail
These `suspect_*` states can override `manual_override`.
This is a semantic choice: suspicion labeling has final precedence.

---

## 11) `features_diag` internals and formulas

## `de_build_features_diag()`
Creates a **one-row-per-symbol** diagnostic snapshot from `panel_adj_model` tails.

It trims each symbol series to roughly `max(feature_horizons_days) + 1` observations to reduce work.

## `de_compute_symbol_diag_features()`
Requires at least 30 rows, else returns `NULL`.

### Computed metrics
For each horizon `h`:
- `ret_{h}d = close_t / close_{t-h} - 1`
- `vol_cc_{h}d = sd(log close-to-close returns over window) * sqrt(252)`

Also computes:
- `max_dd` (max drawdown)
- `ulcer_index`
- `amihud = mean(|simple_return| / traded_value)` (with fallback to `turnover` if needed)

### Trade-size metrics (newer liquidity-aware additions)
If fields are present and valid:
- `avg_trade_size_units = mean(traded_units / n_trades)`
- `avg_trade_size_brl = mean(traded_value / n_trades)`

These are useful for diagnosing market microstructure/liquidity behavior.

### Output shape
Single row per symbol with:
- `symbol`
- `end_refdate`
- `n_obs`
- metric columns

This is diagnostic-only, not a rolling feature matrix.

---

## 12) Configuration reference (developer-oriented)

## `de_config_default()` notable parameters

### Universe scope and types
- `years`
- `include_types`

### Paths
- `cache_dir`
- `raw_dir`
- `logs_dir`

### CA candidate selection
- `min_turnover`
- `min_days_traded_ratio`
- `ca_prefilter_liq_window_days`

### Prefilter diagnostics (gap/jump flags only)
- `ca_prefilter_jump_log_thr`
- `ca_prefilter_gap_equity`
- `ca_prefilter_gap_fii`
- `ca_prefilter_gap_etf`
- `ca_prefilter_gap_bdr`

### Corporate actions fetch/cache
- `ca_fetch_mode` = `"chart"` or `"quantmod"`
- `ca_cache_mode` = `"batch"`, `"by_symbol"`, `"none"`
- `enable_manual_events`

### Split validation/snapping
- `enable_split_gap_validation`
- `split_gap_tol_log`
- `split_gap_max_forward_days`
- `split_gap_max_back_days`
- `split_gap_use_open`

### Post-adjustment diagnostics
- `adj_residual_jump_tol_log`

### Diagnostic features
- `feature_horizons_days`

---

## 13) Error handling and defensive behaviors

The engine intentionally fails loudly in several cases:
- invalid config override keys
- invalid input mode combinations
- missing required packages
- empty universe after build
- contract violations
- invalid column domains (e.g., negative liquidity, nonpositive prices)

Other robustness behaviors:
- Yahoo retry/backoff for rate limits
- validated cache reuse
- typed empty-table outputs
- CA cache corruption tolerance (rebuild on invalid cache)
- split quarantine instead of silent acceptance

---

## 14) Known engineering constraints and invariants (do not break casually)

### A) Preserve canonical liquidity semantics
Downstream code now depends on:
- `traded_value`
- `traded_units`
- `n_trades`
and on legacy aliases being consistent:
- `turnover == traded_value`
- `qty == traded_units`

### B) Keep validators pure
Do not add sorting/mutation to `de_validate_contract()`.

### C) Keep `panel_adj_model` compact and stable
This is the downstream contract surface.

### D) Keep audits rich
Avoid “simplifying” the module by dropping:
- `split_audit`
- `corp_actions_quarantine`
- `adjustments_timeline`
- `residual_jump_audit`
- `panel_adj_debug`

They are essential for debugging real-world data pathologies.

### E) Do not push strategy assumptions upstream
No strategy-specific investability filters in `data_engine`.

---

## 15) Extension points (safe places to evolve)

### 1) Provider layer
- add alternative CA providers
- vendor consensus/merge logic
- improved Yahoo parsing resilience

### 2) Universe normalization
- additional COTAHIST fields
- better metadata canonicalization
- more quote/activity semantics

### 3) Adjustment logic
- more event types (amortization, bonus, rights, etc.)
- optional adjustment of additional price-like fields
- richer issue taxonomy

### 4) Diagnostics
- rolling feature panel (separate module recommended)
- more liquidity/microstructure diagnostics
- per-asset-type diagnostics

### 5) Instrument-specific handling
- more nuanced `asset_type` routing if needed (without polluting core path)

---

## 16) Suggested developer workflow for modifications

1. Update code in a single file/stage
2. Run smoke test (`tests/test_smoke_data_engine.R`)
3. Run adjustment edge-case tests (`tests/test_adjuster_cases.R`)
4. Execute a small real window run
5. Inspect:
   - `split_audit`
   - `adjustments_timeline`
   - `panel_adj_debug`
   - `residual_jump_audit`
6. Only then scale to larger date ranges

This module is easiest to break in subtle ways; inspect audits, not just final row counts.

---

## 17) Practical downstream interpretation guidance

### `panel_adj_model` is the canonical adjusted panel
Use it for:
- returns
- vol estimates
- liquidity diagnostics
- investability masks (downstream)

### Use `adjustment_state` and audits as *diagnostic signals*
Do not silently discard suspicious symbols upstream.
Downstream engines should decide how strict to be.

### `features_diag` is QA-oriented
Useful for:
- quick sanity checks
- symbol triage
- smoke diagnostics
Not sufficient as a research feature matrix by itself.

---

## 18) Summary

`data_engine` is a robust, contract-driven, audit-heavy B3 data adjustment module with explicit handling for:

- schema drift
- vendor CA noise
- split date/factor mismatch
- dividend basis mismatch
- cache corruption/staleness
- post-adjustment residual anomalies

The recent liquidity/activity field upgrade (canonical `traded_value`, `traded_units`, `n_trades`) materially improves downstream correctness, especially for liquidity diagnostics and microstructure-aware metrics.

Treat this module as stable infrastructure and preserve its separation from downstream model/backtest assumptions.