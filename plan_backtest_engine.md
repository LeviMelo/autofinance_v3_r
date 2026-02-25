# `plan_backtest_engine.md`

## 0) Why this document exists

This file is the **build and maintenance plan** for the `backtest_engine` codebase.

It defines a backtesting engine that is:

* **model-agnostic**
* **no-lookahead-safe**
* **compatible with stateful model engines**
* **simple and linear in code organization**
* capable of orchestrating complex models without absorbing their math

This document focuses on **software architecture and contracts**, not model formulas.

---

## 1) Scope and role of `backtest_engine`

The `backtest_engine` is the **master orchestrator** for walk-forward simulation.

Its job is to answer:

> “If a strategy/model proposes targets under a given decision/execution convention and trading assumptions, what happens over time to portfolio holdings, cash, NAV, turnover, and costs?”

### It is responsible for

* backtest calendar and rebalance schedule
* decision-date ↔ execution-date mapping
* strategy/model invocation at the correct time
* execution simulation and cost application
* accounting / NAV / ledger state
* walk-forward orchestration
* analytics and audit logs

### It is not responsible for

* internal quant modeling formulas
* risk/forecast/feature math
* data adjustment logic (upstream `data_engine`)
* mutating model internals outside contract-approved `state` handoff

---

## 2) Design philosophy (updated)

The backtester must stay simple even while driving very complex models.

### Core philosophy

1. **Model-agnostic core, stateful-compatible interface**

   * the runner does not know model math
   * it can pass opaque `model_state` through contracts when needed

2. **Clock and accounting are first-class**

   * most hidden bugs in backtests are timing/accounting bugs, not alpha math bugs

3. **No-lookahead is enforced at multiple layers**

   * data context
   * clock mapping
   * runner assertions
   * strategy/model contract validation

4. **Linear, audit-friendly code**

   * date → proposal → execution → accounting → logs
   * easy to inspect a single rebalance step end-to-end

5. **Separation of economic decision vs execution realization**

   * target weights are a decision artifact
   * fills, costs, and NAV are execution/accounting artifacts

6. **Research ergonomics without research leakage**

   * support parameter grids and multiple strategy variants
   * do not let experiment harness logic pollute core runner semantics

---

## 3) Relationship to `model_engine` and `architecture.md`

### `architecture.md`

Defines the mathematical model (for one family of models).

### `model_engine`

Implements one-date causal model snapshots and optional recursive state updates.

### `backtest_engine`

Controls time and execution, and repeatedly calls the strategy/model via a stable contract.

The backtester must support:

* simple non-stateful strategies
* stateful model engines (including the new architecture)
* future alternative models with different internal complexity

without changing the core runner.

---

## 4) Updated upstream assumptions (data interface)

The previous combined plan assumed older data semantics. This plan uses current data engine outputs and canonical field names.

### Canonical input

`data_bundle$panel_adj_model`

### Canonical columns used by backtesting

* `symbol`, `refdate`
* adjusted `open`, `high`, `low`, `close`
* `asset_type`
* canonical activity fields:

  * `traded_value`
  * `traded_units`
  * `n_trades`
* `adjustment_state` (optional for execution/tradability policy)
* any other optional fields are passed through but not assumed by core runner

### Important semantic rule

Backtester execution and liquidity policies must use canonical names.
Do not build new logic on legacy aliases (`turnover`, `qty`) unless explicitly mapping old runs for backward compatibility.

---

## 5) High-level backtest architecture (code)

The backtester is split into a small number of files that mirror the simulation pipeline.

### Core simulation chain

1. Contracts/specs
2. Data context + clock
3. Strategy/model adapter bridge
4. Execution + costs
5. Accounting
6. Runner
7. Analytics
8. (Optional) artifact IO / experiment utilities

This keeps the code linear and easy to audit.

---

## 6) Proposed repository structure (backtest engine)

```text
/backtest_engine/
  /R/
    00_contracts_and_spec.R
    01_data_context_and_clock.R
    02_strategy_adapter_and_bridge.R
    03_execution_and_costs.R
    04_accounting.R
    05_runner.R
    06_analytics.R
    07_artifacts_and_experiments.R   # optional at first
  /tests/
    testthat/
      test-bt-contracts.R
      test-bt-clock.R
      test-bt-strategy-bridge.R
      test-bt-execution-costs.R
      test-bt-accounting.R
      test-bt-runner.R
      test-bt-lookahead-guards.R
      test-bt-analytics.R
```

This is intentionally compact. The runner remains readable and the stage boundaries are clear.

---

## 7) Major data contracts (backtest engine)

## 7.1 `BacktestSpec` (configuration contract)

Defines simulation semantics and execution/accounting assumptions.

### Required top-level sections

* `clock`
* `execution`
* `costs`
* `accounting`
* `runner`
* `analytics`
* `meta`

### Examples of what belongs here

* rebalance frequency / custom schedule
* decision→execution mapping rule
* execution price field (e.g. next open)
* slippage/fee models
* initial capital / initial portfolio
* marking convention
* logging granularity

No backtester constants should be buried in runner internals.

---

## 7.2 `DataContext` (backtest data access contract)

A no-lookahead-aware data wrapper used by the runner and strategy bridge.

### Responsibilities

* canonicalize panel
* expose trading calendar
* provide price vectors for execution / mark-to-market
* provide as-of slices for strategy/model calls
* validate date ordering and availability
* optionally proxy the same adapter semantics used by `model_engine`

This allows the backtester to remain consistent with model adapter logic.

---

## 7.3 `StrategyAdapter` contract (model-agnostic strategy interface)

This is the most important abstraction in the backtester.

The runner calls a strategy via a standard interface, regardless of whether the strategy is:

* a simple heuristic,
* a wrapper around `model_engine::me_run_snapshot()`,
* or a custom stateful model.

### Required conceptual interface

A strategy adapter must accept:

* `decision_date`
* `data_context`
* `portfolio_state` (current holdings/cash)
* `strategy_state_in` (opaque state, may be `NULL`)
* `strategy_spec`
* optional `runtime_context` (for diagnostics/caches)

and return a `StrategyProposal`.

---

## 7.4 `StrategyProposal` (decision artifact contract)

The strategy/model output consumed by execution and accounting.

### Required fields

* `decision_date`
* `target_weights` (symbol + weight)
* `cash_weight` (explicit if model outputs cash; otherwise inferable)
* `warnings`
* `diagnostics_ref` or compact diagnostics
* `strategy_state_out` (opaque; may be `NULL`)
* `meta` (e.g., spec hash, runtime id)

### Optional fields

* `full_snapshot_artifact` (for debug mode)
* `universe_diag`
* `proposal_quality_flags`

The backtester should not require a full model snapshot to operate.

---

## 7.5 `ExecutionReport` contract

Output of execution simulation for a rebalance event.

### Required fields

* `decision_date`
* `execution_date`
* `orders`
* `fills`
* `costs`
* `cash_delta`
* `execution_warnings`
* `constraint_flags`

This artifact is essential for auditability.

---

## 7.6 `PortfolioState` (accounting state contract)

Internal portfolio/accounting state tracked through time.

### Recommended structure

* `as_of_date`
* `positions` (shares)
* `cash`
* `nav`
* `last_mark_prices`
* `ledger_refs`
* `accounting_diag`

Keep internal representation as **shares + cash** for execution realism and accounting consistency.

---

## 7.7 `BacktestRunArtifact` (final run output contract)

### Required top-level fields

* `spec`
* `strategy_spec`
* `date_range`
* `nav_series`
* `rebalance_log`
* `target_log`
* `execution_log`
* `cost_log`
* `portfolio_snapshots` (configurable granularity)
* `analytics`
* `warnings`
* `errors`
* `meta`

Must be sufficient to reconstruct and audit the run without rerunning.

---

## 8) File-by-file plan (backtest engine)

## `00_contracts_and_spec.R`

### Purpose

Contracts, validators, and default builders for all backtest artifacts.

### Must include

* `bt_spec_default()`
* `bt_get_spec(overrides = NULL)`
* `bt_validate_spec(spec)`
* validators for:

  * `DataContext` (light structural checks)
  * `StrategyProposal`
  * `ExecutionReport`
  * `PortfolioState`
  * `BacktestRunArtifact`

### Rule

No runner logic here.

---

## `01_data_context_and_clock.R`

### Purpose

Backtest data access and scheduling semantics (the timing backbone).

### Responsibilities

* canonicalize panel and validate schema
* build calendar
* generate rebalance dates
* map decision date to execution date(s)
* fetch execution and mark prices
* expose as-of slices for strategy calls
* enforce no-lookahead boundaries at the data-context level

### Important alignment

The behavior of as-of access here should match `model_engine` adapter semantics (same causality conventions).

### Why this file matters

Clock/data mistakes create fake alpha and invalid results more often than math bugs.

---

## `02_strategy_adapter_and_bridge.R`

### Purpose

Bridge between the model-agnostic backtest runner and specific strategy/model implementations.

This is the file that keeps the runner generic while allowing complex stateful models.

### Responsibilities

* define the canonical strategy adapter interface
* wrap simple heuristic strategies
* wrap `model_engine` snapshot calls
* manage opaque `strategy_state_in/out`
* optionally support two integration modes:

  * **decoupled mode**: runner only sees `StrategyProposal`
  * **diagnostic mode**: runner also stores the model snapshot artifact

### Recommended built-in adapters

* `bt_strategy_equal_weight(...)`
* `bt_strategy_from_model_engine(...)` (wraps `me_run_snapshot`)
* `bt_strategy_all_cash(...)` for testing/fallbacks

### Why separate file

This is where model-agnosticism is implemented in practice. Keeping it out of the runner preserves clarity.

---

## `03_execution_and_costs.R`

### Purpose

Convert target weights into executable trades and apply execution assumptions and costs.

### Responsibilities

* compute target notionals from target weights and current NAV
* generate rebalance orders from current holdings to target holdings
* translate notionals to shares using execution prices
* apply slippage/fees/taxes per configured model
* return fills and cost breakdown
* handle missing/unavailable execution prices using explicit policy

### Initial policy (recommended)

* full fills
* long-only
* next-open execution (configurable)
* proportional costs/slippage
* cash absorbs residual
* explicit warnings on price unavailability / symbol mismatch

### Rule

No PnL accumulation logic here; this file ends at execution artifacts.

---

## `04_accounting.R`

### Purpose

Maintain portfolio state, mark-to-market, and ledgers.

### Responsibilities

* initialize portfolio/account state
* apply fills and costs
* update positions and cash
* mark to market using closing prices (or configured mark field)
* compute NAV and weights
* compute turnover and other accounting diagnostics
* maintain ledgers/log entries

### Key design decision

Use **shares + cash** internally, even though strategies express targets in weights.

This avoids many execution/accounting inconsistencies and makes auditing easier.

### Rule

No strategy/model logic here.

---

## `05_runner.R`

### Purpose

Walk-forward orchestration (the master puppeteer).

This file must remain readable and boring.

### Responsibilities

* initialize data context, clock, and portfolio state
* iterate rebalance events
* call strategy adapter on `decision_date`
* validate proposal
* execute on `execution_date`
* update accounting
* mark to market across intervals
* collect logs/artifacts
* enforce timing assertions
* handle failures/fallbacks per policy

### Critical point

The runner controls the time semantics and invocation order. Strategies/models do not self-assign execution dates.

### Recommended runner modes

* **strict mode**: fail on contract or data violations
* **robust mode**: fallback-to-cash / skip-rebalance on selected failures, with explicit flags
* **debug mode**: persist full per-step artifacts

---

## `06_analytics.R`

### Purpose

Performance and operational analytics computed from `BacktestRunArtifact`.

### Responsibilities

* NAV-based performance metrics
* drawdown analysis
* turnover and cost drag summaries
* exposure and cash usage summaries
* rebalance statistics
* optional factor/cluster exposure diagnostics if provided in logs (but not required by core backtester)

### Rule

Analytics should consume logs/artifacts; they should not re-run strategy/model computations.

---

## `07_artifacts_and_experiments.R` (defer until needed)

### Purpose

Artifact persistence and experiment harness helpers, kept outside the core runner.

### Why this matters

Research orchestration tends to metastasize. Keeping experiment/grid tooling separate prevents pollution of the runner.

### Responsibilities (later)

* save/load run artifacts
* run metadata registry
* experiment tags and spec hashes
* parameter sweep helpers
* reproducibility manifests

This file is where research ergonomics belongs, not in `05_runner.R`.

---

## 9) Backtester ↔ model engine integration modes (important)

You explicitly raised that the backtester is the master orchestrator and must remain model-agnostic. This plan encodes that.

## 9.1 Mode A — Decoupled proposal mode (default)

The strategy adapter returns only a `StrategyProposal` (targets + minimal diagnostics).

The backtester does not inspect model internals.

### Best for

* production-like simulation
* speed
* clean abstraction

---

## 9.2 Mode B — Diagnostic snapshot mode

The strategy adapter returns `StrategyProposal` plus full `SnapshotArtifact` (or a reference to it).

The backtester stores these artifacts in logs for audit/debug.

### Best for

* model development
* debugging timing/state issues
* post-hoc analysis of forecast/risk behavior

---

## 9.3 Stateful model compatibility (without runner coupling)

The runner passes an opaque `strategy_state_in` and stores `strategy_state_out` from each proposal.

It does not know what is inside.

This is how the backtester stays model-agnostic while still supporting recursive models.

---

## 10) No-lookahead enforcement (must be explicit)

No-lookahead is not a slogan. It is a multi-layer engineering requirement.

### 10.1 Where it must be enforced

1. **DataContext (`01_data_context_and_clock.R`)**

   * strategy/model as-of slices must stop at `decision_date`

2. **Clock mapping**

   * `execution_date` must be derived by configured rule
   * not chosen ad hoc by strategy

3. **Runner (`05_runner.R`)**

   * strategy called only with `decision_date`
   * execution performed only on `execution_date`
   * mark-to-market interval respects fill timing

4. **Strategy proposal validation**

   * proposal cannot claim impossible dates
   * target symbols must map to executable universe or be handled by explicit policy

### 10.2 Required runtime assertions (recommended)

* no row with `refdate > decision_date` is passed into strategy/model data access
* `execution_date > decision_date` for next-open convention
* execution prices are fetched only at `execution_date`
* no PnL accrues on new positions before execution
* target weights are normalized and contract-valid before execution
* final executed portfolio respects accounting constraints

---

## 11) Data and control flow (backtest engine)

1. **Initialize**

   * validate `BacktestSpec`
   * build `DataContext`
   * build calendar and rebalance schedule
   * initialize `PortfolioState`
   * initialize `strategy_state` (optional)

2. **For each rebalance event**

   * determine `decision_date`
   * determine `execution_date`
   * call strategy adapter with:

     * `decision_date`
     * `data_context`
     * current `portfolio_state`
     * prior `strategy_state`
   * validate `StrategyProposal`
   * fetch execution prices
   * generate orders and execute
   * apply costs
   * update portfolio/accounting state
   * mark to market through interval
   * persist logs and optional diagnostics/snapshots
   * carry forward `strategy_state_out`

3. **Finalize**

   * build `BacktestRunArtifact`
   * compute analytics
   * validate artifact contract
   * return

---

## 12) What the backtester controls vs what the model controls

This separation is essential for maintainability and for keeping the backtester truly model-agnostic.

### Backtester controls

* date range and rebalance schedule
* decision→execution convention
* execution price field and fill rules
* cost/slippage models
* accounting semantics
* logging granularity
* fallback policy on failures
* experiment harness behavior (later)

### Strategy/model controls (through proposal contract)

* target weights / cash weight
* internal recursive state
* internal diagnostics
* model hyperparameters (`strategy_spec` / `ModelSpec`)
* forecast horizon `H` (if included in strategy/model spec)

This lets the backtester be the puppeteer without absorbing model math.

---

## 13) Development sequencing (ignore, just use as conceptual guide).

## Phase 0 — Contracts + clock + accounting spine

Deliver:

* `00`, `01`, `04`, `05` skeletons
* spec/contract validators
* basic data context and schedule generation
* portfolio/accounting initialization and mark-to-market
* dummy strategy adapter (equal-weight / all-cash)

Acceptance:

* dummy backtest runs and passes contract validation

## Phase 1 — Strategy bridge and model-engine wrapper

Deliver:

* `02_strategy_adapter_and_bridge.R`
* wrapper for `model_engine::me_run_snapshot()`
* support for opaque `strategy_state_in/out`
* diagnostic mode logging

Acceptance:

* backtester can run both simple heuristic and model-engine strategy with same runner

## Phase 2 — Execution and costs

Deliver:

* `03_execution_and_costs.R`
* target→orders→fills pipeline
* proportional cost/slippage model
* explicit missing-price policies

Acceptance:

* execution artifacts auditable and consistent with accounting

## Phase 3 — Analytics

Deliver:

* `06_analytics.R`
* performance, drawdown, turnover, cost drag, exposure summaries

Acceptance:

* run artifact can be summarized without manual scripts

## Phase 4 — Artifact IO / experiments (if needed)

Deliver:

* `07_artifacts_and_experiments.R`
* run persistence + metadata registry
* parameter sweep helpers

Acceptance:

* experiment tooling stays out of runner core

---

## 14) Testing strategy (backtest engine)

## A) Contract tests

* malformed `BacktestSpec` fails
* invalid `StrategyProposal` fails
* malformed `ExecutionReport` / `PortfolioState` caught

## B) Clock and date tests

* schedule generation (monthly/weekly/custom)
* decision→execution mapping
* end-of-series handling
* missing execution-date price behavior

## C) Strategy bridge tests

* equal-weight adapter
* all-cash adapter
* model-engine wrapper adapter (with mocked snapshot output)
* opaque state passthrough

## D) Execution/cost tests

* target-to-order conversion on toy examples
* cost calculations on known cases
* residual cash handling
* symbol mismatch / unavailable price policies

## E) Accounting tests

* fills update shares and cash correctly
* NAV and weights computed correctly
* turnover metrics consistent
* mark-to-market between rebalances

## F) Runner integration tests

* dummy strategy full run
* model-engine wrapped strategy full run (mock or small fixture)
* strict vs robust mode behavior
* diagnostic mode logging

## G) Lookahead guard tests

* attempt future access in strategy/model call path and ensure assertion/failure
* ensure execution happens after decision date

---

## 15) Coding conventions and implementation rules (Cursor/Gemini-facing)

### Do

* prefix functions with `bt_*`
* keep runner readable and sequential
* surface warnings/flags instead of silent coercions
* preserve audit logs even in successful runs
* separate core runner from experiment harness logic

### Do not

* embed model math in backtest files
* let strategies decide execution dates silently
* hide no-lookahead semantics in undocumented helpers
* over-engineer abstractions before the basic runner is stable
* mix experiment grid orchestration into `05_runner.R`

### Practical rule

If a feature only matters for research batching, it belongs in `07_artifacts_and_experiments.R`, not in the runner core.

---

## 16) Definition of done (backtest engine planning cycle)

This planning cycle is successful when:

1. the backtester has a stable model-agnostic strategy/proposal contract
2. the runner can drive both simple heuristics and stateful model-engine strategies
3. no-lookahead timing is enforced by data context + clock + runner + validations
4. execution/accounting/cost semantics are cleanly separated
5. the codebase remains linear, compressed, and auditable
6. experiment tooling (if added) is kept outside the core runner path

---

## 17) Short pinned philosophy (keep near top of repo)

* backtester owns time, execution, and accounting
* strategies/models own target proposals and internal state
* no-lookahead is enforced, not assumed
* logs are part of the product
* keep the runner boring and trustworthy