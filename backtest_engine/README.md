# backtest_engine

Backtest engine for strategy comparison with explicit window protocol, strict contracts, and daily accounting.

## Purpose
`backtest_engine` runs comparable backtests across different strategy functions by enforcing:
- one scored window (`A..B`) mapped to trading dates,
- one prehistory policy (`W_star`) per run,
- deterministic decision/execution flow,
- consistent execution/cost/accounting rules,
- standardized outputs and analytics.

## Scope
Owned by this module:
- Backtest window construction and validation.
- Data context and trading calendar operations.
- Universe selection at decision dates.
- Strategy proposal contract enforcement.
- Execution simulation (with costs and cash constraints).
- Daily NAV accounting and summary analytics.

Not owned by this module:
- Data ingestion/corporate actions adjustment (handled by `data_engine`).
- Model internals (handled by each model engine, e.g. `model_engine_came`).

## File Map
- `R/00_contracts_and_spec.R`: spec defaults/validation, requirements contract, proposal/run validations.
- `R/01_data_context_and_clock.R`: panel context, calendar utilities, universe selection, price lookup policies.
- `R/02_strategy_adapter_and_bridge.R`: built-in adapters (`all_cash`, `equal_weight`, CAME bridge).
- `R/03_execution_and_costs.R`: target shares, orders, cost model, execution with cash budget.
- `R/04_accounting.R`: fills application, daily mark-to-market, weights/turnover helpers.
- `R/05_runner.R`: backtest window build + end-to-end simulation loop.
- `R/06_analytics.R`: scored/full-window performance metrics.

## Core Concepts
### Backtest Window
The runner builds a window object with:
- requested dates (`A_req`, `B_req`),
- effective trading dates (`A0`, `B0`),
- score range (`A_score`, `B_score`),
- model life start (`S`) based on prehistory,
- decision/execution schedule with `exec_lag`,
- status metadata (`OK`, truncated tail, no decisions).

### Model Requirements
Each strategy must expose:
- `model_id`
- `prehistory_days`
- `stateful`
- `supports_replay`

The runner combines model and universe needs into `W_star` and rejects infeasible ranges.

### Universe and Tradability
At decision dates, base universe is selected with strict investability constraints (`lookback_days`, coverage, liquidity, optional top-K). Holdings can be included in strategy-visible context via `keep_holdings`.

### Execution and Marking
- Execution price source is configurable (`open`/`close`), with `exec_lag`.
- Costs include fee + slippage.
- Cash budget is enforced; oversized buy legs are scaled.
- Symbols without valid execution price on execution day are skipped (logged), not hard-crashed.
- Daily mark-to-market supports:
  - `mark_missing_policy = "error"`
  - `mark_missing_policy = "carry_last"` with optional `max_stale_mark_days`.

## Public Entrypoints
- `bt_get_spec(overrides = NULL)`
- `bt_run_backtest_range(data_bundle_or_panel, strategy_fn, strategy_spec, start_date, end_date, bt_spec = NULL)`
- `bt_run_backtest(...)` (full available calendar wrapper)
- `bt_compute_analytics(run_artifact)`
- `bt_print_summary(analytics)`

## Inputs
Panel must contain at least:
- `symbol`, `refdate`, `open`, `close`, `traded_value`, `traded_units`, `n_trades`
- optional: `asset_type`

Panel is expected to be sparse by symbol/date (not rectangular).

## Outputs
Run artifact contains:
- `nav` (daily NAV, cash, positions count, `in_score` flag),
- `decisions` table,
- `executions` table,
- `execution_skips` table,
- `meta` (window/spec/final state).

## Typical Usage
```r
bt_spec <- bt_get_spec(list(
  clock = list(freq = "months"),
  universe = list(lookback_days = 63L, min_price_history_days = 252L, max_universe = 40L),
  execution = list(price_field = "open", exec_lag = 1L),
  accounting = list(mark_field = "close", mark_missing_policy = "carry_last", max_stale_mark_days = NULL)
))

res <- bt_run_backtest_range(panel, strategy_fn, strategy_spec, "2024-03-01", "2025-12-15", bt_spec)
an <- bt_compute_analytics(res)
bt_print_summary(an)
```

## Known Behavior to Watch
- Sparse panels can produce execution skips when `open` is missing on execution day.
- With `carry_last`, long quote gaps may still fail if stale cap is set and exceeded.
- Backtest comparability depends on using the same scored window and prehistory protocol across strategies.
