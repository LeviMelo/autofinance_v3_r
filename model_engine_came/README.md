# model_engine_came (CAME)

CAME is a stateful, causal portfolio model engine. It consumes historical panel data up to an `as_of_date` and produces one portfolio snapshot (weights + diagnostics + updated state).

## Purpose
Provide model logic only:
- risk estimation,
- structure/graph dynamics,
- signals/features,
- forecast mixture,
- constrained optimization.

CAME does **not** execute trades or account NAV. That is handled by `backtest_engine`.

## Entry Point
- `came_run_snapshot(data_bundle_or_panel, as_of_date, spec = NULL, state = NULL, prev_target = NULL)`

Returns a validated snapshot with:
- `weights` and `cash_weight`,
- `risk`, `structure`, `signals`, `forecast`, `optimizer` diagnostics,
- `state_out` for next call.

## File Map
- `R/00_utils.R`: utilities, typed errors/warnings, assertions, numerics.
- `R/01_contracts.R`: default spec + validators, snapshot/state contracts.
- `R/02_state.R`: state initialization/update helpers.
- `R/03_data_adapter.R`: causal data access adapter.
- `R/04_risk_engine.R`: covariance/precision and risk horizon transforms.
- `R/05_structure_engine.R`: graph/cluster dynamics.
- `R/06_signals.R`: momentum/state-linked signals.
- `R/07_features.R`: feature assembly for forecast/gating.
- `R/08_forecast.R`: component forecasts, reliability/gating blend.
- `R/09_optimizer.R`: portfolio optimization with constraints/tradability handling.
- `R/10_runner.R`: orchestration of all stages for one snapshot.
- `R/99_smoke_tests.R`: smoke checks.

## Key Design Rules
- Strict causality: only information available at `as_of_date`.
- Stateful evolution: each call should receive prior `state_out`.
- No silent fallback on hard failures (typed errors).
- Clear separation of model output vs execution/accounting.

## Spec Structure
`came_spec_default()` defines sections:
- `data`
- `risk`
- `structure`
- `signals`
- `forecast`
- `gating`
- `reliability`
- `optimizer`
- `meta`

Use `came_spec_validate(spec)` after custom edits.

## Backtest Integration
CAME is integrated through backtest adapter `bt_strategy_from_model_engine`.
Backtester controls trade/no-trade scheduling and execution; CAME controls target portfolio proposal.

For strict comparisons:
- use common scored window across strategies,
- use common prehistory protocol (`W_star` at backtest level),
- keep execution/cost rules equal for all models.

## Minimal Example
```r
spec <- came_spec_default()
state <- came_state_init()

snap <- came_run_snapshot(
  data_bundle_or_panel = panel,
  as_of_date = "2025-06-03",
  spec = spec,
  state = state,
  prev_target = NULL
)

head(snap$weights)
state <- snap$state_out
```

## Notes
- This README is the unified CAME documentation.
- `README.txt` is kept only as a pointer for compatibility.
