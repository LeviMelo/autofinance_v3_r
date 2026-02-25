## `README_data_engine.md`

# data_engine (B3 OHLCV + Corporate Actions Adjustment Engine)

## What this module is

`data_engine` is the upstream data/adjustment component of the `autofinance_v3_r` project.

It builds an **auditable, adjusted daily panel** for B3 assets (equities, FIIs, ETFs, BDRs), combining:

- B3 market data ingestion (via `rb3`)
- schema normalization
- liquidity/activity field normalization
- corporate actions fetch (Yahoo chart API or `quantmod`)
- split validation/snapping against raw price gaps
- normalized events table generation
- split + dividend adjustment application
- debug/audit artifacts for traceability
- optional diagnostic symbol features (returns, volatility, drawdown, Amihud, trade-size metrics)

This module is intentionally **contract-driven** and returns both **model-ready outputs** and **audit/debug artifacts**.

---

## What changed (important liquidity semantics update)

The engine now explicitly carries **three canonical activity/liquidity fields** from COTAHIST/rb3:

- `traded_value` → monetary traded value (BRL)
- `traded_units` → quantity traded (units/contracts, depending instrument convention)
- `n_trades` → number of trades (print count / trade count)

For backward compatibility, the engine still provides legacy aliases:

- `turnover` = `traded_value`
- `qty` = `traded_units`

### Migration note (very important)
If any downstream logic previously interpreted `qty` as “number of trades”, that is **incorrect**.

- Use `n_trades` for trade count
- Use `qty` / `traded_units` for units traded

This matters for liquidity metrics (e.g., Amihud, average trade size, turnover filters).

---

## What this module produces

Main entrypoint: `de_run_data_engine(...)`

It returns a list (bundle) with:

- `universe_raw`
- `panel_adj_model`
- `panel_adj_debug`
- `events_apply`
- `corp_actions_raw`
- `corp_actions_apply`
- `corp_actions_quarantine`
- `split_audit`
- `prefilter_audit`
- `adjustments_timeline`
- `residual_jump_audit`
- `features_diag` (or `NULL` if disabled)
- `meta`

### Core outputs you will usually use downstream

- **`panel_adj_model`**: compact adjusted OHLC + liquidity/activity + adjustment state (model-facing)
- **`panel_adj_debug`**: raw OHLC + split-adjusted OHLC + final adjusted OHLC + liquidity/activity + adjustment state (debug-facing)
- **`features_diag`**: optional symbol-level QA/diagnostic metrics (not a full rolling feature engine)

---

## What this module does *not* do

This module is upstream and should remain focused on **data correctness + adjustments**.

It does **not** implement:

- trading no-lookahead semantics
- strategy investability rules for backtests/models
- portfolio constraints or position sizing
- execution simulation
- alpha model logic

Those belong in downstream engines (model/backtest).

---

## Directory structure (module map)

```text
/data_engine/
  fixtures/manual_events_template.csv
  R/00_contracts.R
  R/01_config.R
  R/02_utils.R
  R/03_providers.R
  R/04_universe.R
  R/05_corp_actions.R
  R/06_adjuster.R
  R/07_diagnostics_features.R
  R/08_pipeline.R
  README_data_engine.md
  tests/test_adjuster_cases.R
  tests/test_smoke_data_engine.R
````

---

## Dependencies

### Core runtime dependencies

* `data.table`
* `dplyr`

### Provider / infra dependencies

* `rb3`
* `bizdays`
* `digest`
* `jsonlite`
* `curl`
* `quantmod`
* `zoo`

> Notes:
>
> * `rb3` is attached (not just namespace-loaded) by the engine because of template registry initialization behavior.
> * `bizdays` must support the `"Brazil/B3"` calendar in your environment.

---

## How to load the module in R

This is a script-based module (not a packaged R library yet), so source files in order.

### Option A — source all files (recommended during development)

```r
r_files <- sort(list.files("data_engine/R", pattern = "\\.R$", full.names = TRUE))
for (f in r_files) source(f)
```

### Option B — source explicitly

```r
source("data_engine/R/00_contracts.R")
source("data_engine/R/01_config.R")
source("data_engine/R/02_utils.R")
source("data_engine/R/03_providers.R")
source("data_engine/R/04_universe.R")
source("data_engine/R/05_corp_actions.R")
source("data_engine/R/06_adjuster.R")
source("data_engine/R/07_diagnostics_features.R")
source("data_engine/R/08_pipeline.R")
```

---

## Main entrypoint: `de_run_data_engine(...)`

### Signature

```r
de_run_data_engine(
  years = NULL,
  start_date = NULL, end_date = NULL,
  force_symbols = NULL,
  manual_events = NULL,
  overrides = NULL,
  build_diag_features = TRUE
)
```

### Input modes (mutually exclusive)

You must choose **one** mode:

#### 1) Year mode

```r
res <- de_run_data_engine(years = 2024:2025)
```

#### 2) Window mode

```r
res <- de_run_data_engine(
  start_date = "2025-01-01",
  end_date   = "2025-12-31"
)
```

### Invalid combinations (rejected by design)

* `years` + `start_date/end_date` together ❌
* only `start_date` without `end_date` (or vice versa) ❌
* `start_date > end_date` ❌

---

## Usage examples

## Example 1 — Basic year-based build

```r
res <- de_run_data_engine(
  years = 2024:2025,
  overrides = list(
    ca_fetch_mode = "chart",
    ca_cache_mode = "batch"
  )
)

panel <- res$panel_adj_model
str(panel)
```

---

## Example 2 — Date-window build with custom liquidity threshold

```r
res <- de_run_data_engine(
  start_date = "2025-01-01",
  end_date   = "2025-12-31",
  overrides = list(
    min_turnover = 1e6,
    min_days_traded_ratio = 0.75
  )
)

panel <- res$panel_adj_model
head(panel)
```

---

## Example 3 — Forcing CA fetch for specific symbols

Useful when a symbol is illiquid and would otherwise be excluded from CA candidate selection.

```r
res <- de_run_data_engine(
  years = 2025,
  force_symbols = c("PETR4", "VALE3")
)
```

---

## Example 4 — Manual events (e.g., patching vendor gaps)

```r
library(data.table)

manual_events <- data.table(
  symbol = c("PETR4", "PETR4"),
  refdate = as.Date(c("2025-03-10", "2025-05-15")),
  action_type = c("dividend", "split"),
  value = c(0.72, 0.5),   # split uses price-factor orientation (2:1 split => 0.5)
  source = c("manual", "manual")
)

res <- de_run_data_engine(
  start_date = "2025-01-01",
  end_date   = "2025-12-31",
  manual_events = manual_events,
  overrides = list(enable_manual_events = TRUE)
)
```

---

## Reading the outputs quickly (developer workflow)

### 1) Model-facing adjusted panel

```r
panel <- res$panel_adj_model
names(panel)
```

Expect (minimum contract):

* `symbol`, `refdate`, `asset_type`
* adjusted `open`, `high`, `low`, `close`
* `traded_value`, `traded_units`, `n_trades`
* `turnover`, `qty` (legacy aliases)
* `adjustment_state`

### 2) Debug panel (trace raw → split-adjusted → final-adjusted)

```r
dbg <- res$panel_adj_debug
names(dbg)
```

Includes (at least):

* raw OHLC: `open_raw`, `high_raw`, `low_raw`, `close_raw`
* split-adjusted OHLC: `open_adj_split`, ...
* final adjusted OHLC: `open`, `high`, `low`, `close`
* liquidity/activity canonical fields + aliases
* `adjustment_state`

### 3) Split audit / quarantine

```r
res$split_audit
res$corp_actions_quarantine
```

Use this to inspect Yahoo split date/factor validation behavior.

### 4) Residual jump audit

```r
res$residual_jump_audit[residual_jump_flag == TRUE]
```

Flags symbols with suspicious residual jumps even after adjustments.

### 5) Diagnostic features

```r
res$features_diag
```

A single-row per symbol diagnostic snapshot (if `build_diag_features = TRUE`).

---

## Liquidity and activity fields (canonical semantics)

This is the most common source of downstream mistakes, so keep this pinned.

### Canonical fields (preferred)

* `traded_value`: monetary traded value (BRL)
* `traded_units`: quantity traded
* `n_trades`: number of trades

### Legacy aliases (kept for compatibility)

* `turnover` = `traded_value`
* `qty` = `traded_units`

### Practical implications

* **Amihud illiquidity** should use `traded_value` (monetary denominator), not trade count
* **Average trade size (units)** = `traded_units / n_trades`
* **Average trade size (BRL)** = `traded_value / n_trades`

---

## Corporate action conventions (critical)

### `action_type`

Allowed values:

* `"dividend"`
* `"split"`

### Split value orientation

`split` values are stored as **price-factor orientation**:

* 2:1 split → `0.5`
* 1:2 reverse split → `2.0`

This is the convention used in `events_apply$split_value` and adjustment math.

---

## Manual events format (what to pass)

`manual_events` should be CA-like rows compatible with the CA pipeline.

### Minimum recommended columns

* `symbol` (character)
* `refdate` (`Date`)
* `action_type` (`"dividend"` or `"split"`)
* `value` (numeric)
* `source` (character; usually `"manual"`)

### Optional

* `yahoo_symbol` (not required for manual rows)

### Tip

If reading from CSV, make sure `refdate` is converted to `Date` before calling the engine.

---

## Configuration (`overrides`) — common knobs

Pass a named list to `overrides = list(...)`.

### Commonly changed options

* `include_types`: `c("equity","fii","etf","bdr")`
* `min_turnover`
* `min_days_traded_ratio`
* `ca_fetch_mode`: `"chart"` or `"quantmod"`
* `ca_cache_mode`: `"batch"`, `"by_symbol"`, `"none"`
* `enable_manual_events`
* `enable_split_gap_validation`
* `split_gap_tol_log`
* `adj_residual_jump_tol_log`
* `feature_horizons_days`

### Example

```r
res <- de_run_data_engine(
  years = 2025,
  overrides = list(
    include_types = c("equity", "fii", "etf"),
    ca_fetch_mode = "chart",
    ca_cache_mode = "batch",
    enable_split_gap_validation = TRUE,
    feature_horizons_days = c(21L, 63L, 126L, 252L)
  )
)
```

### Safety feature

Unknown override keys are rejected (the engine fails fast instead of silently ignoring typos).

---

## Contract-driven validation (design philosophy)

The engine uses explicit schema contracts (`de_contracts`) and validation via `de_validate_contract()`.

Validation covers:

* required columns
* types/domains (e.g., Date, positive prices)
* allowed enums (`asset_type`, `action_type`)
* duplicate `(symbol, refdate)` keys for most tables

### Important exception

`corp_actions_raw` is allowed to have duplicate `(symbol, refdate)` because multiple corporate actions can occur on the same date.

### Important design nuance

Validators are intentionally **pure**:

* they validate
* they do **not** reorder rows

Row ordering is the responsibility of builder functions.

---

## Pipeline summary (high level)

1. Build raw universe from `rb3`
2. Normalize schema + liquidity/activity fields
3. Select CA candidates from recent liquidity/trading coverage
4. Fetch CA from Yahoo (with cache)
5. Validate/snap Yahoo splits against raw price gaps
6. Build normalized `events_apply`
7. Apply split + dividend adjustments
8. Build adjusted panels + audits
9. Optionally build `features_diag`

---

## Diagnostics and audit artifacts (why they matter)

This engine is intentionally “audit-heavy”. Keep and inspect these when debugging data quality issues:

* `prefilter_audit`: why symbols were/weren’t CA candidates
* `split_audit`: split validation status (`kept`, `unverified`, `rejected`, `dup`)
* `corp_actions_quarantine`: rejected/duplicate split rows
* `adjustments_timeline`: per-row factors and issue flags
* `residual_jump_audit`: post-adjustment suspicious jump detection
* `panel_adj_debug`: raw + intermediate + final OHLC trail

---

## Adjustment states (`panel_adj_model$adjustment_state`)

Expected values include:

* `no_actions`
* `dividend_only`
* `split_only`
* `split_dividend`
* `manual_override`
* `suspect_dividend_factor`
* `suspect_residual_jump`
* `suspect_both`

### Precedence nuance

`manual_override` can be overwritten later by `suspect_*` labels if residual issues are detected. This is intentional: suspicion flags take precedence in final labeling.

---

## `features_diag` (what it is and what it is not)

`features_diag` is a **symbol-level diagnostic snapshot** (one row per symbol, end-of-window), not a rolling time-indexed feature panel.

Typical columns:

* `symbol`, `end_refdate`, `n_obs`
* `ret_{h}d`
* `vol_cc_{h}d`
* `max_dd`
* `ulcer_index`
* `amihud`
* `avg_trade_size_units` (when possible)
* `avg_trade_size_brl` (when possible)

It is primarily for QA / quick diagnostics.

---

## Troubleshooting

### 1) “Missing required packages...”

Install the packages listed in Dependencies and retry.

### 2) `rb3` errors / template registry issues

The engine attempts to attach `rb3` internally. If `rb3` still fails, restart R and retry after reinstall/update.

### 3) Yahoo rate-limit behavior

The Yahoo fetch layer retries rate-limit errors with backoff, but heavy runs may still fail. Re-run later or use cache (`ca_cache_mode = "batch"`).

### 4) Empty or sparse outputs

Common causes:

* strict date window too small
* too restrictive `include_types`
* no available rb3 data in the requested window
* very high liquidity thresholds (for CA candidates)

### 5) Suspicious `adjustment_state`

Inspect:

* `res$split_audit`
* `res$adjustments_timeline`
* `res$residual_jump_audit`
* `res$panel_adj_debug[symbol == "..."]`

---

## Tests

Included tests:

* `tests/test_adjuster_cases.R`
* `tests/test_smoke_data_engine.R`

Run them in your dev workflow after changing:

* adjustment math
* CA fetch/normalization logic
* contracts/schema
* liquidity semantics

---

## Recommended downstream usage policy

Use `panel_adj_model` as the canonical downstream input for modeling/backtesting, and treat:

* `adjustment_state`
* `residual_jump_audit`
* `prefilter_audit`

as **downstream filtering/auditing signals**, not as reasons to mutate upstream data construction logic.

That keeps the data engine clean, reproducible, and strategy-agnostic.

---

## Bottom line

`data_engine` is not a simple downloader. It is a **contract-validated, auditable adjustment pipeline** with explicit handling for real-world vendor quirks (Yahoo CA noise, split date/factor mismatch, dividend basis mismatch, cache corruption/staleness).

Use it as the stable upstream foundation for the model/backtest engines.

---

