# `plan_model_engine.md`

## 0) Why this document exists

This file is the **build and maintenance plan** for the `model_engine` codebase.

It does **not** restate the mathematical model (that is the job of `architecture.md`).
Instead, it defines:

* how the model engine is organized in code,
* what contracts it exposes,
* how data and state move through it,
* how it stays maintainable while supporting models of very different complexity.

This plan is meant to be **implementation-facing** (Cursor/Gemini-ready) and **long-lived**.

---

## 1) Scope and role of `model_engine`

The `model_engine` is a **causal, end-of-day, stateful snapshot engine**.

Its job is to answer:

> “Given data observed up to decision date `t` and prior model state, what is the model output (forecasts, diagnostics, risk objects, portfolio target) for execution no earlier than `t+1`?”

### It is responsible for

* no-lookahead-safe **as-of data access**
* recursive **model state updates**
* risk / structure / signal / feature / forecast / portfolio stages
* emission of a **validated snapshot artifact**
* optional emission/update of recursive state for the next step

### It is **not** responsible for

* order execution simulation
* transaction cost realization
* NAV accounting
* walk-forward orchestration (except optional standalone convenience wrapper)
* backtest scheduling logic

Those belong in `backtest_engine`.

---

## 2) Design philosophy (updated)

This codebase must support **complex models** without becoming unmaintainable.

### Core philosophy

1. **Contract-first, pipeline-second, formulas-third**

   * define inputs/outputs and state boundaries before coding internals
   * formulas live inside stage modules; orchestration depends on contracts, not implementation details

2. **Linear code layout**

   * files should follow pipeline order
   * a human should be able to read the directory top-to-bottom and reconstruct the computation graph

3. **Compressed modules, not micro-fragmentation**

   * keep coherent stages in one file
   * split only when independent evolution/testing pressure justifies it

4. **State is explicit**

   * recursive state lives in typed/validated objects
   * no hidden global variables, no hidden caches, no mutation-through-side-effect unless explicitly documented

5. **Model-agnostic at the orchestration boundary**

   * the runner knows stage contracts and calls configured implementations
   * new models can reuse all or part of the pipeline by honoring the same contracts

6. **Diagnostics are first-class outputs**

   * numerical repairs, fallbacks, and coverage losses must be surfaced, never silently hidden

---

## 3) Relationship to `architecture.md`

`architecture.md` is the **mathematical source of truth**.

This file (`plan_model_engine.md`) is the **software architecture source of truth** for:

* file organization,
* object contracts,
* stage interfaces,
* state lifecycle,
* implementation sequencing,
* testing discipline.

If there is a conflict:

* mathematical definitions come from `architecture.md`
* code structure / boundaries come from this file
* any mismatch should be resolved by updating both deliberately (not via ad hoc code drift)

---

## 4) Updated upstream assumptions (data engine interface)

The previous plan is outdated regarding upstream fields and semantics. This plan uses the current `data_engine` outputs.

### Canonical upstream input (mandatory)

`data_bundle$panel_adj_model`

### Optional upstream inputs (supported, not mandatory)

* `data_bundle$panel_adj_debug`
* `data_bundle$features_diag` (if present)
* `data_bundle$residual_jump_audit`
* `data_bundle$split_audit`
* `data_bundle$prefilter_audit`
* `data_bundle$meta`

### Canonical observable columns (minimum)

* identifiers: `symbol`, `refdate`, `asset_type`
* adjusted prices: `open`, `high`, `low`, `close`
* activity/liquidity:

  * `traded_value` (BRL financial volume)
  * `traded_units` (units/shares traded)
  * `n_trades` (trade count)
* metadata / flags:

  * `adjustment_state`

### Compatibility aliases (must not define math)

* `turnover` (alias of `traded_value`)
* `qty` (alias of `traded_units`)

Use aliases only for backward compatibility. New model logic must use canonical names.

### Important note on richer upstream data

The data engine now preserves more market metadata and activity dimensions. The model engine must be able to:

* consume only the canonical panel by default
* optionally attach extra upstream fields/sidecars for diagnostics or features without hard-coding dependency on them

This keeps the model engine robust to upstream enrichment.

---

## 5) High-level model engine architecture (code)

The model engine is organized as a **stage pipeline** with explicit contracts and a recursive state object.

### Stage chain (conceptual)

1. Data adapter / as-of views
2. State mapping / state preparation
3. Risk engine
4. Structure engine
5. Signal primitives
6. Feature engine
7. Forecast engine
8. Portfolio engine
9. Snapshot assembly + validation

### Key principle

Each stage:

* consumes a well-defined subset of inputs,
* returns a typed artifact,
* may update a defined subset of recursive state.

No stage should “reach across” and inspect arbitrary internals from unrelated stages.

---

## 6) Proposed repository structure (model engine)

```text
/model_engine/
  /R/
    00_contracts_and_spec.R
    01_data_adapter.R
    02_state_store_and_mapping.R
    03_risk_engine.R
    04_structure_engine.R
    05_signal_primitives.R
    06_feature_engine.R
    07_forecast_engine.R
    08_portfolio_engine.R
    09_snapshot_runner.R
    10_diagnostics_and_artifacts.R   # optional at first; create when needed
  /tests/
    testthat/
      test-model-contracts.R
      test-data-adapter-no-lookahead.R
      test-state-mapping.R
      test-risk-engine.R
      test-structure-engine.R
      test-signals.R
      test-feature-engine.R
      test-forecast-engine.R
      test-portfolio-engine.R
      test-snapshot-runner.R
```

This is still **compressed** for a model of this scale. The split is by coherent stage, not by every sub-function.

---

## 7) Major data contracts (model engine)

## 7.1 `ModelSpec` (configuration contract)

`ModelSpec` is the single configuration object consumed by the model engine.

### Required top-level sections

* `data`
* `state`
* `risk`
* `structure`
* `signals`
* `features`
* `forecast`
* `portfolio`
* `runtime`
* `meta`

### Design rules

* every constant used by stage logic must live in `ModelSpec` or be a documented default in `me_spec_default()`
* no buried magic constants in stage code
* include a `spec_version` and deterministic hash

---

## 7.2 `ModelState` (recursive state contract)

`ModelState` stores all recursive state required for causal updates across dates.

It must be explicit and validated.

### Required characteristics

* keyed to the current universe mapping (symbols/index map)
* partitioned by stage ownership
* serializable (save/load for reproducibility and debugging)
* safe to pass back and forth through the backtest engine

### Recommended top-level structure

* `meta` (version, as_of_date, symbol map/hash)
* `risk_state`
* `structure_state`
* `signal_state`
* `forecast_state`
* `calibration_state`
* `diagnostics_state` (if needed for recursive diagnostics)

Do **not** collapse everything into one unstructured list.

---

## 7.3 `SnapshotArtifact` (single-date output contract)

This is the primary output of `me_run_snapshot()`.

### Required top-level fields

* `as_of_date`
* `decision_date` (same as `as_of_date` unless explicitly aliased)
* `forecast_horizon`
* `universe`
* `tradability`
* `risk`
* `structure`
* `signals`
* `features_diag` (summary/metadata, not full matrices unless configured)
* `forecast`
* `portfolio`
* `target_weights`
* `cash_weight` (if applicable)
* `state_out` (optional/required depending on run mode)
* `meta`
* `warnings`
* `errors` (empty on success)

### Important separation

* `target_weights` is the tradable portfolio proposal
* `portfolio` contains optimizer/shaping diagnostics
* `state_out` is recursive model state for next run

---

## 7.4 `ModelRuntime` (implementation registry contract)

To support different models while preserving engine structure, define a `ModelRuntime` object that contains stage callables.

### Purpose

It allows:

* swapping stage implementations,
* running simpler baselines,
* testing stages independently,
* keeping the runner stable.

### Runtime shape (conceptual)

A list of functions / handlers:

* `data_adapter`
* `state_mapper`
* `risk_engine`
* `structure_engine`
* `signal_engine`
* `feature_engine`
* `forecast_engine`
* `portfolio_engine`
* `snapshot_assembler`

`architecture.md` model = one runtime composition.
A simpler baseline model = another runtime composition, same contracts.

This is **not** a plugin framework. It is a lightweight stage registry.

---

## 8) File-by-file plan (model engine)

## `00_contracts_and_spec.R`

### Purpose

Centralize contracts, validators, spec defaults, and shared schema helpers.

### Must include

* `me_spec_default()`
* `me_get_spec(overrides = NULL)`
* `me_validate_spec(spec)`
* `me_hash_spec(spec)`
* validators for:

  * `ModelState`
  * stage artifacts (`RiskArtifact`, `StructureArtifact`, etc.)
  * `SnapshotArtifact`
* general helpers:

  * named vector checks
  * matrix symmetry/PSD/finite checks
  * required-column checks
  * simplex checks

### Rule

No stage math here. This file should remain mostly declarative and validation-focused.

---

## `01_data_adapter.R`

### Purpose

Provide no-lookahead-safe access to `panel_adj_model` and optional sidecars.

This is the **hard guardrail** between raw adjusted panel data and model logic.

### Responsibilities

* canonicalize and validate panel schema
* sort/dedupe
* build trading calendar
* expose as-of and rolling-window accessors
* enforce `refdate <= as_of_date`
* expose canonical activity/liquidity semantics
* produce aligned matrices/vectors for model stages
* surface tradability masks and data-quality flags (not score-based preselection)

### Adapter outputs/functions (behavioral contract)

* calendar access
* panel slices up to `t`
* rolling windows for prices/returns/activity
* aligned matrices by symbol
* as-of cross-sectional vectors
* tradability/admissibility masks
* optional joins to debug metadata / upstream sidecars

### Updated semantics requirement

Every function that emits liquidity/activity fields must use canonical names:

* `traded_value`
* `traded_units`
* `n_trades`

Aliases may be emitted only when explicitly requested.

---

## `02_state_store_and_mapping.R`

### Purpose

Handle recursive state lifecycle and changing-universe mapping.

This file is where the software implementation mirrors the `\Pi_t` logic from `architecture.md`.

### Responsibilities

* initialize `ModelState`
* validate `ModelState`
* map prior state from `U_{t-1}` to `U_t`
* initialize new names with configured priors
* drop departed names
* maintain symbol-index maps
* provide stage-specific state getters/setters with validation

### Why separate file

Universe mapping and recursive state management are fundamental and error-prone. Keeping them isolated improves reliability and testability.

---

## `03_risk_engine.R`

### Purpose

Implement the risk branch defined in `architecture.md`, including recursive updates and risk covariance outputs.

### Responsibilities (software-level; formulas live in `architecture.md`)

* update per-asset volatility state (EWMA or configured variant)
* build standardized returns
* run rolling PCA on standardized returns
* align PCA identities (sign/order) before recursive factor updates
* update factor covariance recursion
* compute residuals
* update residual covariance target
* run **Glasso** (residual precision only; no alternative estimators in core path)
* invert residual precision to residual covariance
* assemble daily risk covariance
* apply numerical repairs and flags
* build horizon-consistent covariance (`Σ^(H)`)

### Outputs

`RiskArtifact` with:

* daily and horizon covariance
* factor objects
* residual precision/covariance
* vol state
* diagnostics + repair flags

### Rule

Do not mix graph/structure logic in this file, even though `Θ_t` is later used there.

---

## `04_structure_engine.R`

### Purpose

Convert residual precision outputs into structural graph objects and operators.

### Responsibilities

* compute partial correlations from `Θ_t`
* temporal smoothing of partial-correlation matrix
* adaptive edge activation / persistence / top-k / global exception logic
* symmetric graph mask construction and post-symmetry control
* weighted adjacency and graph operators:

  * `A_t`, `A_t^sgn`, `L_t`, `L_t^norm`
* graph diagnostics
* clustering + label persistence
* cluster-derived structural context metadata

### Outputs

`StructureArtifact` with:

* `P_t`, `Pbar_t`
* `M_t`, weighted adjacency
* graph operators
* graph diagnostics
* cluster labels and persistence diagnostics

### Rule

This file is **operator-layer infrastructure**, not forecast logic.

---

## `05_signal_primitives.R`

### Purpose

Compute pre-graph signal families and their scalarization states.

### Responsibilities

* raw and residual TSMOM multiscale features
* Kalman per-asset state-space features
* factor trend features and factor-projected continuation primitives
* signal-family scalarization (rolling ridge + smoothing)
* signal vector alignment and coverage tracking

### Outputs

`SignalArtifact` with:

* primitive feature families
* scalarized signal vectors (e.g. `s_mom`, `s_kal`, `s_fac`)
* scalarization weights/state updates
* diagnostics (coverage, correlation, failure counts)

### Rule

No graph transforms here; those happen in feature assembly.

---

## `06_feature_engine.R`

### Purpose

Assemble causal feature vectors `X_{i,t}` from signal, structure, liquidity, PCA, and global state objects.

### Responsibilities

* graph-operator transforms of scalarized signals
* cluster-centered / cluster-z contextual features
* PCA–graph interaction features
* liquidity/activity block construction (including standardized transforms)
* global state vector assembly and replication/join into per-asset rows
* local structural diagnostics into features
* feature masking/default policy for non-tradable or missing cases
* feature metadata (column registry, groups, standardization params)

### Outputs

`FeatureArtifact` with:

* per-asset feature matrix / table
* feature registry (names, groups, provenance)
* global state vector `m_t`
* diagnostics (missingness, clipping, standardization flags)

### Rule

Keep feature construction deterministic and traceable. Avoid hidden learned logic here.

---

## `07_forecast_engine.R`

### Purpose

Map features into component forecasts, combine them with global/local confidence, and produce forecast mean + uncertainty + reliability-adjusted moments.

### Responsibilities

* component model fitting / updating on matured labels only
* component prediction (`μ^(c)`)
* global softmax mixture weights from `m_t`
* bounded asset-component confidence multipliers and normalized mixture weights
* final forecast mean (`μ_hat`)
* overlap-aware error state updates for `H>1` (stagger buckets)
* component residual covariance estimation (`Ω^μ`)
* forecast uncertainty (`s_hat`)
* asset-level reliability score and optimizer-facing effective moments (`μ_eff`, `s_eff`)

### Outputs

`ForecastArtifact` with:

* component forecasts
* mixture/confidence weights
* forecast mean and uncertainty
* reliability-adjusted outputs
* calibration state updates
* diagnostics (fit windows, residual stats, fallback flags)

### Rule

No optimizer logic here. Forecast ends at forecast outputs + diagnostics.

---

## `08_portfolio_engine.R`

### Purpose

Consume forecast outputs and risk covariance to produce continuous core portfolio solution + post-optimization support shaping (if configured).

### Responsibilities

* map global state to optimizer controls (e.g., risk aversion, turnover scale, risky gross cap)
* build liquidity-aware bounds and turnover penalty coefficients
* solve continuous core optimization (long-only, no hard cardinality in core path)
* apply post-optimization support shaping (thresholding / renorm / repair solve)
* enforce contract-safe fallbacks on solver failure
* emit final target weights + optimizer diagnostics

### Outputs

`PortfolioArtifact` with:

* `w_raw`
* `w_shaped` / `w_target`
* `w_cash`
* optimizer controls used
* solver diagnostics
* shaping diagnostics
* constraint checks / flags

### Rule

This file should not simulate execution or accounting.

---

## `09_snapshot_runner.R`

### Purpose

Orchestrate the single-date model pipeline.

This is the main entrypoint used by the backtester and manual inspections.

### Main responsibilities

* accept request (`data_bundle`, `as_of_date`, `ModelSpec`, `state_in`, mode options)
* build/reuse data adapter
* derive current universe and tradability masks
* map prior state to current universe
* call stages in pipeline order
* collect stage artifacts
* assemble `SnapshotArtifact`
* validate outputs
* return `SnapshotArtifact`

### Supported run modes (recommended)

* **stateless mode**: for debugging / one-off analyses (initializes state)
* **stateful mode**: consumes `state_in`, returns `state_out`
* **diagnostic mode**: optionally emits richer intermediate objects

---

## `10_diagnostics_and_artifacts.R` (defer until needed)

### Purpose

Artifact persistence, compact summaries, and reusable diagnostic extraction helpers.

This file is intentionally optional early on. Create it when:

* artifact payloads become large,
* repeated summary logic appears,
* you need save/load and reproducibility helpers.

---

## 9) End-to-end data flow (model engine)

This is the software flow, not a re-derivation of the mathematics.

1. **Request in**

   * `data_bundle`, `as_of_date`, `spec`, optional `state_in`

2. **Adapter layer**

   * validate panel
   * build as-of views and rolling windows
   * build tradability/admissibility masks

3. **State prep**

   * initialize or map prior `ModelState` to current universe

4. **Risk stage**

   * produce covariance objects and risk diagnostics
   * update risk-related recursive state

5. **Structure stage**

   * produce graph operators, clusters, structural diagnostics
   * update structure-related recursive state

6. **Signal stage**

   * produce signal primitives / scalar signals
   * update signal and scalarization states

7. **Feature stage**

   * assemble `X_{i,t}` and state vector `m_t`

8. **Forecast stage**

   * produce `μ_hat`, `s_hat`, `μ_eff`, `s_eff`
   * update forecast/calibration/error states

9. **Portfolio stage**

   * solve and shape portfolio
   * produce final target weights and portfolio diagnostics

10. **Snapshot assembly**

* package all outputs + `state_out` + diagnostics + warnings
* validate contract and return

---

## 10) Model-agnostic support (within model engine)

The engine must support models of varying complexity without rewriting the runner.

### Mechanism

Use a stable `SnapshotArtifact` contract and a `ModelRuntime` stage registry.

### Examples of models this supports

* simple momentum-only baseline (few stages used)
* risk-only diagnostic model (no portfolio solve)
* full architecture model (`architecture.md`)
* alternative forecast engines reusing same risk/structure layers
* alternative portfolio engines reusing same forecast outputs

### Constraint

Different models may omit internal artifacts, but they must either:

* provide contract-required fields, or
* provide explicit `NA/NULL + warning` placeholders and a declared capability profile

No silent missing fields.

---

## 11) Development sequencing (ignore, just use as conceptual guide).

This is the build order for a stable implementation.

### Phase 0 — Contracts + state + adapter skeleton

Deliver:

* `00`, `01`, `02`, `09` skeletons
* spec defaults + validators
* no-lookahead adapter
* state mapping
* dummy snapshot runner with trivial equal-weight target

Acceptance:

* validated snapshot on a real `as_of_date`
* no-lookahead tests pass

### Phase 1 — Core risk + portfolio plumbing (minimal forecast)

Deliver:

* `03_risk_engine.R`
* `08_portfolio_engine.R` with simple forecast placeholder
* snapshot integration

Acceptance:

* risk artifacts valid, covariance repairs surfaced
* continuous optimizer path works on real universe

### Phase 2 — Structure engine

Deliver:

* `04_structure_engine.R`
* graph operators + diagnostics + clustering
* contract tests for graph invariants

Acceptance:

* graph objects stable and valid across rolling dates

### Phase 3 — Signals + features

Deliver:

* `05_signal_primitives.R`
* `06_feature_engine.R`
* coverage and feature registry diagnostics

Acceptance:

* scalar signals and features reproducible and inspectable

### Phase 4 — Forecast engine

Deliver:

* `07_forecast_engine.R`
* component forecasts + gating/confidence + uncertainty

Acceptance:

* matured-label training discipline enforced
* overlap-aware uncertainty updates validated

### Phase 5 — Artifact ergonomics / caching / performance tuning

Deliver:

* `10_diagnostics_and_artifacts.R` as needed
* selective caching hooks
* compact summaries and save/load

---

## 12) Testing strategy (model engine)

Testing should follow the same “defensive engineering” mentality used in your data engine.

### A) Contract tests

* invalid spec structures fail loudly
* stage artifacts missing required fields fail validation
* snapshot artifact contract catches silent drift

### B) No-lookahead adapter tests

* synthetic panel tests proving `refdate <= as_of_date` always
* rolling windows never include future rows
* execution-time fields are not exposed in model adapter unless explicitly requested and still causal

### C) State mapping tests

* universe additions/removals correctly mapped
* recursive matrices/vectors reindexed correctly
* priors applied only to new names

### D) Numerical tests (risk/structure)

* covariance symmetry/PSD checks
* Glasso precision SPD checks
* graph mask symmetry and zero diagonal
* operator stability on isolated nodes

### E) Signal/feature tests

* signal coverage and alignment
* bounded outputs where applicable
* feature registry consistency and missingness policy

### F) Forecast tests

* no label leakage (`s <= t-H`)
* stagger-bucket updates for `H>1`
* confidence/reliability outputs within bounds

### G) Portfolio tests

* optimizer constraint checks
* fallback behavior on solver failure
* post-shaping repair preserves hard constraints

### H) Snapshot regression tests

* small frozen fixture slice + fixed spec → stable outputs within tolerance
* compare diagnostics and key artifacts, not only final weights

---

## 13) Coding conventions and implementation rules (Cursor/Gemini-facing)

### Do

* use explicit prefixes (`me_*`)
* keep stage modules readable and internally sectioned
* return rich diagnostics
* validate early and often
* keep pure helpers local to stage files until reuse justifies extraction
* document every fallback with a flag/warning

### Do not

* create a plugin system before contracts stabilize
* hide state mutation in nested closures without clear ownership
* leak backtest semantics into model stages
* hardcode constants outside specs/defaults
* use compatibility aliases (`turnover`, `qty`) as mathematical primitives

### Split rule (repeat)

Only split a file when there is concrete pressure:

* independent evolution,
* testing pain,
* or scanning pain.

Not because a style guide says “one concept per file.”

---

## 14) Definition of done (model engine planning cycle)

This planning cycle is successful when:

1. `model_engine` has a stable file architecture aligned with `architecture.md`
2. contracts for `ModelSpec`, `ModelState`, and `SnapshotArtifact` are explicit and validated
3. no-lookahead and state mapping boundaries are codified
4. the runner can host both simple and complex models through the same stage contracts
5. the code organization remains linear, compressed, and maintainable under future model growth

---

## 15) Short pinned philosophy (keep near top of repo)

* `architecture.md` defines the math
* `plan_model_engine.md` defines the software boundaries
* snapshot semantics are sacred
* recursive state is explicit
* diagnostics are first-class
* complexity is allowed, but hidden complexity is not