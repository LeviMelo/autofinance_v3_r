# Autofinance v2 Rewrite Plan (Canonical `plan.md`)

## 0) Purpose of this plan

This file is the **single source of truth** for the rewrite of the Autofinance v2 project into a **new repo**, built from scratch, with a focus on:

- readability
- maintainability
- modularity
- testability
- future compatibility with the modeling / portfolio optimization pipeline

This is **not** a patch plan for the old codebase.  
We are **rebuilding the system from scratch** and using the old code only as:

1. a **behavioral reference** (what logic worked / what must be preserved), and  
2. a **bug archive** (what patterns to avoid).

---

## 1) Core goals (what we are building now vs later)

## 1.1 What we are building now (priority)

### A) Refactored data engine (B3 + corporate actions + adjusted panel)
Build a clean, modular data pipeline that can:

- ingest B3 OHLCV-like market data (rb3-backed in R for now)
- normalize schemas across source inconsistencies
- fetch/select corporate actions (Yahoo)
- apply split/dividend adjustments robustly
- produce a validated `panel_adj` dataset
- produce a diagnostic/visualization feature dataset (not yet model-driven)

### B) Stable contracts + fixtures + tests
We need a system that is hard to break accidentally.

This means:
- clear table schemas (contracts)
- deterministic behavior
- test fixtures
- no hidden global state / brittle sourcing order
- no `exists()`-driven spaghetti control flow

### C) Architecture that supports future portfolio modeling
The data engine must be designed so later modules can plug in easily:
- trend experts (Kalman + TSMOM)
- risk backbone (PCA → Graphical Lasso → HRP)
- market state features / gating
- portfolio optimization / allocation / backtest

---

## 1.2 What we are NOT building yet (but must stay compatible with)

- full execution/trading integration
- broker integration
- production scheduler
- live order management
- advanced factor engine (full version)
- full portfolio optimizer UI/app
- Python migration (but we will design for it)

---

## 2) Guiding principles for the rewrite

## 2.1 Keep the logic, replace the structure
The old code contains a lot of valuable logic:
- schema normalization
- dedupe rules
- selective Yahoo CA strategy
- split-gap validation / snapping / orientation repair
- robust adjuster logic
- useful screener diagnostics

We keep these behaviors, but rewrite into a cleaner architecture.

## 2.2 Contracts first, implementation second
Every major stage must define:
- required input columns
- output columns
- invariants (e.g., no duplicate `symbol,refdate`)
- failure behavior

## 2.3 Deterministic cache + explicit config
No hidden behavior.  
Every pipeline run should be explainable by:
- config
- input date range / symbol set
- cache mode
- source mode

## 2.4 Diagnostic layer != model feature layer
Current screener features are mainly a **diagnostic/visualization layer**.
They are useful and will be kept, but they do **not** define the final modeling feature layer.

We will later add a separate **model feature layer** aligned to:
- trend models
- risk models
- gating/regime models
- portfolio allocation needs

## 2.5 Selective corporate actions remains the core philosophy
We do **not** want to fetch CA blindly for the entire universe if we can avoid it.

We will preserve and refine the v2 philosophy:
- fetch Yahoo CA only for a **candidate set**
- use a **single Yahoo chart call** (splits + dividends together) per candidate symbol where possible
- prefilter candidates using fast heuristics from raw B3 data

---

## 3) What the old codebase currently does (behavioral summary we must preserve)

This section is here so we don’t forget important logic while rewriting.

---

## 3.1 Module 00 (core): config / logging / utils

### What it does
- Defines a large default config (`af2_config_default`) controlling:
  - universe years/types
  - liquidity thresholds
  - corporate action fetch/cache modes
  - selective CA toggles
  - split validation knobs
  - path settings
  - safety thresholds
- Creates directories automatically
- Validates some config knobs
- Provides utility assertions and helpers:
  - `%||%`
  - package requirements
  - column assertions
  - duplicate key checks
  - bizdays fallback (`bizdays` calendar or weekdays-only)
- Provides simple logging (`af2_log`)

### What to preserve
- config-driven pipeline behavior
- explicit safety knobs
- duplicate/assert helpers
- business-day sequence helper (with `bizdays` fallback)
- readable logging

### What to change in rewrite
- remove load-order fragility (`exists()` checks + scattered sourcing)
- move to package-based namespace
- centralize config validation cleanly
- separate pure config defaults from filesystem side effects

---

## 3.2 Module 01 (B3 universe): raw universe building from rb3

### What it does
- Initializes rb3 cache directory and bootstrap
- Fetches B3 COTAHIST yearly or daily-window data via `rb3`
- Applies asset-type filters lazily (equity/fii/etf/bdr)
- Collects each type separately
- Normalizes columns across rb3 schema variants:
  - symbol/refdate/OHLC
  - liquidity candidates (financial volume / quantity)
- Unifies liquidity into:
  - `turnover`
  - `qty`
- Concatenates all asset types into one `universe_raw`
- Enforces contract checks and removes obvious junk rows
- Deduplicates `(symbol, refdate)` using a deterministic rule:
  1. max turnover
  2. max qty
  3. max close
  4. first row deterministically
- Caches per year / per window

### What to preserve (critical)
- year and date-window modes
- rb3 cache bootstrap / deterministic cache path
- type filtering before collect
- schema normalization (`select_min_cols`)
- liquidity unification (`turnover`, `qty`)
- duplicate key fix with deterministic ranking
- strict output contract:
  - `symbol, refdate, open, high, low, close, turnover, qty, asset_type`

### What to change in rewrite
- better separation:
  - fetch (source IO)
  - normalize schema
  - dedupe/contract validation
  - cache repository
- no module-local dependency bootstraps (`zzz_depends.R`)
- cleaner function signatures and orchestration

---

## 3.3 Module 03 (corporate actions): Yahoo registry + selective fetch

### What it does
- Maps B3 symbols to Yahoo (`.SA`)
- Fetches CA from Yahoo in two possible modes:
  - `chart` (single call returns splits + dividends)
  - `quantmod` fallback (`getSplits`, `getDividends`)
- Supports cache modes:
  - `batch`
  - `by_symbol`
  - `none`
- Has retry/backoff logic for Yahoo rate limits (HTTP 429)
- Supports parallel fetching (Windows-safe cluster path)
- Builds a CA registry with schema:
  - `symbol, yahoo_symbol, refdate, action_type, value, source`
- Implements **candidate selection** for selective CA:
  - **Set A (broad dividends)** = liquid symbols (based on recent liquidity)
  - **Set B (split suspects)** = symbols with large price discontinuities over full history
  - final candidate set = union(A, B)

### What to preserve (critical)
- single Yahoo chart call mode (splits + dividends together)
- retry/backoff logic
- cache modes (`batch`, `by_symbol`, `none`)
- candidate prefiltering philosophy
- union strategy (liquid dividend names + split gap suspects)
- stable CA registry schema

### Important clarification (your point, and we will preserve it)
If Yahoo chart returns splits and dividends in one call, why not just fetch only for prefiltered names?

**Yes — that is the core v2 philosophy.**  
That is exactly what we will keep:

- prefilter candidate set
- one Yahoo chart call per candidate symbol
- use the same result for both splits and dividends

The only nuance is the **candidate definition**, which must still catch:
- liquid dividend-relevant symbols
- split events that may occur outside current liquid set / recent windows

So we keep the union logic, but make it cleaner and configurable.

---

## 3.4 Module 04 (adjuster): normalize events + apply split/dividend adjustments

### What it does
This is the most important part of the pipeline.

#### A) Normalizes CA and manual events
- accepts Yahoo/registry CA and manual overrides
- standardizes event schema
- filters invalid values
- preserves split values as **price factors**
- deduplicates exact duplicate events before aggregation

#### B) Builds unified event table
Aggregates same-day events per `symbol, refdate`:
- splits → multiply factors
- dividends → sum cash values
- composes `source_mask`
- flags `has_manual`

Output event table schema:
- `symbol, refdate, split_value, div_cash, source_mask, has_manual`

#### C) Applies adjustments to `universe_raw`
- joins events to market days
- computes **exclusive reverse cumulative split factor**
- builds split-adjusted OHLC
- computes dividend adjustment factor using previous close on split-adjusted basis
- includes dividend basis mismatch rescue:
  - scales dividend by split factor when needed (forward split mismatch case)
- flags impossible dividends (`issue_div`)
- computes final adjusted OHLC and adjustment timeline

#### D) Builds `panel_adj` and states
- adds raw aliases (`*_raw`)
- derives `adjustment_state` per symbol:
  - `no_actions`
  - `dividend_only`
  - `split_only`
  - `split_dividend`
  - `manual_override`
  - `suspect_unresolved`
- residual jump safety net:
  - flags symbols with large post-adjustment residual jumps
  - upgrades state to `suspect_unresolved`

#### E) Selective wrapper + split-gap validator
`build_panel_adj_selective` adds the v2 “trick”:
- select CA candidates
- fetch CA selectively
- validate Yahoo splits against raw B3 jumps
- **snap split dates** (forward/backward) to actual trading day
- normalize split orientation (`value` vs `1/value`)
- quarantine suspicious splits
- fail-soft policy:
  - keep validated splits
  - allow `unverified` in controlled cases
  - quarantine rejects/dups
- then run core adjuster

### What to preserve (critical)
- event normalization and aggregation rules
- split factor logic (exclusive reverse cumulative)
- dividend factor logic + basis mismatch rescue
- `adjustment_state` logic and safety net
- split-gap validator behavior:
  - date snap
  - orientation choice
  - tolerance-based accept/reject
  - quarantine + audit trail
- fail-soft selective CA policy

### What to change in rewrite
- split into smaller, testable components
- formal contracts for each intermediate table
- make “audit/quarantine” first-class outputs (not side effects)
- cleaner naming and reduced branching complexity in orchestration

---

## 3.5 Module 05 (screener): diagnostic feature layer + ranking

### What it does
- validates `panel_adj` input contract
- applies liquidity filter
- computes per-symbol features (mostly diagnostic / exploratory)
- optionally scores/ranks symbols using z-score weighted aggregation

### Current features include (diagnostic layer)
- returns (multi-horizon)
- close-close volatility
- Parkinson volatility
- Garman–Klass volatility
- gap stats
- intraday range stats
- ATR-like true range %
- body %
- drawdown / ulcer index
- liquidity stats
- Amihud illiquidity

### What to preserve
- this feature set as a **temporary diagnostic/visualization layer**
- ability to return features without scoring
- input validation (including unresolved adjustment policy)

### What to change
- decouple “feature computation” from “screener logic”
- make scoring/ranking optional and thin
- later create a separate **model feature layer** aligned to downstream models

---

## 3.6 Diagnostics module (symbol debrief)
### What it does
`diag_symbol` gives per-symbol debugging/debrief:
- raw vs adjusted jumps
- events
- CA applied
- split audit
- optional plots

### What to preserve
This is extremely useful for debugging CA/adjustment failures and should remain.

---

## 4) Main problems in the old code (why we are rewriting)

This is the anti-pattern list we will avoid.

## 4.1 Structural problems
- fragmented module scripts requiring manual source order
- `zzz_depends.R` style dependency bootstrapping
- global function existence checks / load-order coupling
- side effects mixed with pure logic
- inconsistent responsibilities inside files/functions

## 4.2 Maintainability problems
- hard to trace data contracts across modules
- orchestration logic and domain logic entangled
- duplicate validation / normalization in multiple places
- difficult to know what is cached, where, and why
- hard to add features without fear of breakage

## 4.3 Testing problems
- logic is testable in principle, but not systematically organized
- no clean package namespace + test harness workflow
- fixtures and golden behavior not formalized

## 4.4 Architectural gap (future modeling compatibility)
- current feature layer is diagnostic but not explicitly separated from future model features
- no explicit interfaces for:
  - risk backbone inputs
  - trend expert inputs
  - market state features
  - gating / allocator pipeline

---

## 5) New target architecture (new repo, from scratch)

## 5.1 High-level architecture (what the new repo will contain)

We will build a **single R package project** for the engine, with clear subdomains.

Suggested repo scope (R-first, package-based):

- data ingestion (B3)
- corporate actions (Yahoo + reconciliation)
- adjuster (events + panel adjustments)
- diagnostics
- feature store (diagnostic first; model features later)
- model primitives (later)
- portfolio pipeline (later)

### Why package now?
Because packaging gives us:
- namespace management
- exports/imports
- clean internal/private functions
- tests (`testthat`)
- documentation (`roxygen2`)
- `devtools::load_all()`
- no more source-order chaos / `exists()` checks

---

## 5.2 Package structure (proposed)

(Conceptual structure; exact file names can evolve.)

- `R/`
  - `config-*.R`
  - `utils-*.R`
  - `logging-*.R`
  - `contracts-*.R`
  - `b3-fetch-*.R`
  - `b3-normalize-*.R`
  - `b3-universe-*.R`
  - `ca-symbol-map.R`
  - `ca-fetch-yahoo-chart.R`
  - `ca-fetch-quantmod.R`
  - `ca-retry.R`
  - `ca-candidates.R`
  - `ca-registry.R`
  - `ca-split-reconcile.R`
  - `events-build.R`
  - `adjuster-apply.R`
  - `panel-build.R`
  - `diagnostics-symbol.R`
  - `features-diagnostic.R`
  - `screener-*.R` (thin)
  - later:
    - `features-model-*.R`
    - `risk-*.R`
    - `trend-*.R`
    - `gating-*.R`
    - `portfolio-*.R`

- `tests/testthat/`
  - unit tests by subdomain
  - fixtures/golden behavior tests

- `inst/`
  - `extdata/fixtures/` (small reproducible datasets)
  - optional sample configs

- `data-raw/`
  - scripts to build packaged fixtures (if needed)

- project root runtime dirs (gitignored)
  - `data/cache/`
  - `data/raw/`
  - `logs/`
  - `outputs/`

### Key policy
Runtime data/caches are **not package internals**.  
The package code *uses* them via config, but does not hardwire messy paths.

---

## 5.3 Domain boundaries (what each part owns)

## A) Core / contracts / config
Owns:
- defaults
- validation
- assertions
- logging
- path resolution
- cache key helpers
- schema contract definitions

Does NOT own:
- source-specific fetch logic
- adjustment math
- feature definitions

## B) B3 Universe domain
Owns:
- rb3 init/bootstrap
- yearly/window fetch
- schema normalization
- type filtering
- liquidity unification
- raw universe contract validation/dedup
- raw universe cache

Output:
- `universe_raw`

## C) Corporate Actions domain
Owns:
- Yahoo symbol mapping
- Yahoo fetch (chart/quantmod fallback)
- retries/backoff
- CA caching
- candidate prefilter selection
- CA registry build
- split reconciliation/validation against raw B3 jumps
- audit/quarantine outputs

Output:
- `corp_actions_registry`
- `corp_actions_apply`
- `corp_actions_quarantine`
- `split_audit`

## D) Event/Adjuster domain
Owns:
- normalize corp actions/manual events
- build event table
- apply split/dividend adjustments
- build `panel_adj`
- adjustment states
- residual jump audit

Output:
- `events`
- `adjustments_timeline`
- `panel_adj`
- `residual_jump_audit`

## E) Diagnostics domain
Owns:
- per-symbol debrief
- plots (optional)
- traceability of raw/adjusted/events/audits

## F) Features domain
Split into two layers:

### F1) Diagnostic Feature Layer (now)
- returns, vol, drawdown, liquidity, etc.
- for inspection, visualization, exploratory analysis
- not tightly coupled to portfolio logic

### F2) Model Feature Layer (later)
- features specifically required by:
  - trend experts (Kalman/TSMOM)
  - risk backbone (PCA/Glasso/HRP)
  - market-state/gating
  - portfolio optimizer

---

## 6) Data contracts (must be explicit and enforced)

These are the key tables we will preserve/formalize.

## 6.1 `universe_raw` contract (B3 normalized market panel)
Required columns:
- `symbol` (character, uppercase)
- `refdate` (Date)
- `open`, `high`, `low`, `close` (numeric)
- `turnover` (numeric)
- `qty` (numeric)
- `asset_type` (character: equity/fii/etf/bdr)

Invariants:
- no duplicate `(symbol, refdate)`
- sorted by `asset_type, symbol, refdate`
- obvious junk rows removed (missing symbol/date/close)

---

## 6.2 `corp_actions_registry` contract
Required columns:
- `symbol`
- `yahoo_symbol`
- `refdate`
- `action_type` (`split` | `dividend`)
- `value` (numeric)
- `source` (e.g., `yahoo`, `manual` later in merged stages)

Notes:
- raw vendor-level registry, before final application policy

---

## 6.3 `split_audit` contract (critical for traceability)
Contains validation/reconciliation metadata for Yahoo splits:
- vendor date
- effective snapped date
- lag in days
- original Yahoo value
- chosen normalized value (possibly inverted)
- error vs observed raw gap
- status (`kept`, `unverified`, `rejected`, `dup`, etc.)

This must remain a first-class output.

---

## 6.4 `events` contract (adjuster-ready event table)
Required columns:
- `symbol`
- `refdate`
- `split_value` (price factor; default 1)
- `div_cash` (cash dividend; default 0)
- `source_mask`
- `has_manual`

Aggregation rules:
- same-day splits multiply
- same-day dividends sum
- dedupe exact duplicates before aggregation

---

## 6.5 `panel_adj` contract (main adjusted panel)
Must include:
- keys: `symbol`, `refdate`, `asset_type`
- raw aliases: `open_raw`, `high_raw`, `low_raw`, `close_raw`
- adjusted fields:
  - split-adjusted OHLC (`*_adj_split`)
  - final-adjusted OHLC (`*_adj_final`)
- liquidity:
  - `turnover`, `qty`
- `adjustment_state`

Invariants:
- no duplicate `(symbol, refdate)`
- `close_adj_final` present and numeric
- screener/feature layer can run directly from this

---

## 6.6 `diagnostic_features` contract (temporary analytics layer)
Per-symbol rows (latest snapshot based on available history), including:
- returns (multi-horizon)
- vol stats
- drawdown/ulcer
- liquidity metrics
- Amihud
- metadata (`symbol`, `asset_type`, `end_refdate`, coverage, etc.)

This is intentionally separate from model features.

---

## 7) Pipeline flow (new orchestrated behavior)

This is the target end-to-end flow.

## 7.1 Build raw universe
1. resolve config
2. fetch B3 data (yearly or window)
3. filter asset types
4. normalize schema
5. unify liquidity
6. dedupe and validate
7. cache `universe_raw`

Output: `universe_raw`

## 7.2 Select CA candidates (selective philosophy)
From `universe_raw`:
- Set A (dividends): liquid names that can enter analysis
- Set B (splits): price-gap suspects across full history
- union = CA candidate set
- optional `force_symbols` union

Output: `candidate_symbols`

## 7.3 Fetch CA registry (Yahoo)
For candidates only:
- one Yahoo chart call per symbol (splits + dividends) if possible
- fallback quantmod if chart unavailable
- cache by batch or by symbol
- return standardized registry

Output: `corp_actions_registry`

## 7.4 Reconcile Yahoo splits with raw B3 data
- validate split date and orientation against raw discontinuity
- snap dates to actual trading day
- choose best orientation (`v` vs `1/v`)
- quarantine rejects/duplicates
- fail-soft policy for unverified

Outputs:
- `corp_actions_apply`
- `corp_actions_quarantine`
- `split_audit`

## 7.5 Build events + apply adjustments
- normalize CA + manual events
- aggregate event table
- apply split and dividend adjustments
- compute adjustment states and residual jump audit
- validate `panel_adj`

Outputs:
- `panel_adj`
- `events`
- `adjustments_timeline`
- `residual_jump_audit`

## 7.6 Build diagnostic features (current stage)
- compute per-symbol diagnostic features from `panel_adj`
- optionally score/rank (thin screener layer)
- save as a feature table for visualization/inspection

Outputs:
- `diagnostic_features`
- optional ranks

---

## 8) Modeling compatibility (must be designed in now)

This section is here because the data engine is not the final goal.

Your downstream system (as previously defined) is centered on:

- **Trend experts:** Kalman + TSMOM
- **Risk backbone:** PCA → Graphical Lasso → HRP
- **Market state primitives / gating**
- **Portfolio construction and allocation**

The refactor must therefore preserve and expose the right inputs.

## 8.1 Data requirements for later models (design constraints now)

### For trend experts (Kalman, TSMOM)
Need:
- reliable adjusted price series (`close_adj_final`)
- possibly adjusted OHLC for richer signals
- stable symbol/date panel
- missingness and coverage diagnostics
- clean returns (no fake jumps from unresolved splits)

### For risk backbone (PCA/Glasso/HRP)
Need:
- synchronized return matrix input
- consistent universe selection snapshots
- robust liquidity metadata (for tradeability filters)
- auditability of excluded/suspect names

### For market state features / gating
Need:
- dispersion measures
- correlation concentration / top eigenvalue proxy inputs
- volatility/vol-of-vol inputs
- cross-sectional coverage and liquidity stats

### For portfolio optimization
Need:
- a canonical feature/return store
- clear date indexing and lookback windows
- no silent survivorship or adjustment errors
- reproducible pipeline state from config + cache

## 8.2 Architectural decision (important)
We will maintain **two feature layers**:

### Layer A — Diagnostic Feature Layer (now)
Purpose:
- inspect assets
- visualize behavior
- sanity-check adjusted series
- basic exploratory ranking

### Layer B — Model Feature Layer (later)
Purpose:
- directly feed trend/risk/gating/optimizer modules

This prevents the current screener metrics from hardcoding the future architecture.

---

## 9) Refactor implementation plan (phased and executable)

This is the actual work order.

---

## Phase 0 — Bootstrap the new repo as an R package (foundation)

### Goal
Create a clean package project so the rest of the rewrite happens in a sane environment.

### Tasks
- create new repo (fresh)
- initialize as R package
- set up:
  - `DESCRIPTION`
  - `NAMESPACE` (generated by roxygen)
  - `R/`
  - `tests/testthat/`
- add dev tooling:
  - `devtools`
  - `usethis`
  - `roxygen2`
  - `testthat`
  - `lintr` (optional)
- add `.gitignore` for runtime dirs:
  - `data/cache/`
  - `data/raw/`
  - `logs/`
  - `outputs/`
- create minimal config and path helpers
- create assertion utilities (cols, dupes, types)
- create logging helpers

### Deliverable
Package skeleton loads with `devtools::load_all()` and passes a trivial test.

### Notes (R packaging, practical)
This is the minimum workflow you’ll use repeatedly:
- edit functions in `R/*.R`
- document via roxygen comments
- run `devtools::load_all()`
- run tests via `devtools::test()`

No more manual sourcing chains.

---

## Phase 1 — Formalize contracts + fixtures (before rewriting logic)

### Goal
Lock in expected behavior/contracts so rewrite does not drift.

### Tasks
- write contract docs for:
  - `universe_raw`
  - `corp_actions_registry`
  - `events`
  - `panel_adj`
  - `diagnostic_features`
- extract/build small fixtures representing:
  - normal equity
  - split case
  - dividend case
  - split+dividend collision day
  - bad Yahoo split date/orientation case
  - duplicate `(symbol, refdate)` universe rows
- write tests for contract assertions and dedupe rules

### Deliverable
Fixtures + tests that encode the behavior we want to preserve.

### Why this matters
This is what prevents “rewrite regression” where code looks cleaner but silently loses logic.

---

## Phase 2 — Rebuild B3 universe module (cleanly)

### Goal
Re-implement `universe_raw` pipeline with clear boundaries and tests.

### Subcomponents to build
1. `b3_init_cache()` / `b3_init_rb3()`
2. `b3_fetch_yearly_lazy()`
3. `b3_fetch_window_lazy()`
4. `b3_filter_types_lazy()`
5. `b3_select_min_cols()` (schema mapping)
6. `b3_unify_liquidity()`
7. `b3_dedupe_symbol_date()` (deterministic ranking)
8. `build_universe_year()`
9. `build_universe_window()`
10. `build_universe()`

### Preserve from old logic
- lazy filter before collect
- schema normalization across rb3 variants
- liquidity unification
- duplicate rule (turnover > qty > close > deterministic first)
- per-year and per-window caching

### Deliverable
`universe_raw` builder passing fixtures/tests and producing stable outputs.

---

## Phase 3 — Rebuild corporate actions module (selective-first)

### Goal
Clean CA pipeline with chart-first Yahoo fetch and selective candidate strategy.

### Subcomponents to build
1. symbol mapping (`.SA`)
2. retry/backoff helpers (429-aware)
3. Yahoo chart fetcher (splits + dividends)
4. quantmod fallback fetchers
5. CA cache repository (batch / by_symbol / none)
6. candidate selector (liquidity + gap suspects)
7. registry builder
8. parallel fetch (optional; guarded and tested lightly)
9. standardization/contract checks

### Preserve from old logic
- chart mode default
- fallback to quantmod if chart deps unavailable
- cache modes
- rate-limit backoff
- selective candidate prefilter
- union(A liquid dividend names, B split suspects)

### Planned simplification
The orchestration should explicitly reflect the v2 philosophy:
- candidate prefilter first
- then one Yahoo chart fetch per candidate symbol
- use that single result for both splits and dividends

### Deliverable
`corp_actions_registry` builder with selectable cache modes and tests.

---

## Phase 4 — Rebuild split reconciliation + adjuster (most critical phase)

### Goal
Recreate the robust adjustment logic, but split into smaller tested units.

### Subcomponents to build
#### Split reconciliation / audit
- `fix_yahoo_splits_by_raw_gap()`
  - date snapping
  - orientation selection (`v` vs `1/v`)
  - tolerance scoring
  - statuses
  - quarantine
  - audit table

#### Event normalization/build
- normalize CA events
- normalize manual events
- dedupe exact duplicates
- aggregate same-day splits/dividends
- build `events`

#### Adjustment math
- reverse cumulative split factors (exclusive)
- split-adjusted OHLC
- dividend factor on split-adjusted basis
- dividend basis mismatch rescue
- final adjusted OHLC
- adjustment timeline

#### Panel builder
- `build_panel_adj()`
- `build_panel_adj_selective()`
- adjustment states
- residual jump safety net
- validation

### Preserve from old logic (must not lose)
- exact split factor semantics (price factor)
- exclusive reverse cumprod logic
- dividend rescue behavior
- residual jump audit/state override
- fail-soft split application policy (`kept`/`unverified` vs quarantine)
- traceability outputs (`split_audit`, `corp_actions_apply`, `corp_actions_quarantine`)

### Deliverable
Reliable `panel_adj` build pipeline with diagnostic artifacts and tests for edge cases.

---

## Phase 5 — Rebuild diagnostics + temporary feature layer (clean separation)

### Goal
Retain useful diagnostics while separating them from future model features.

### Subcomponents to build
1. `diag_symbol()` (or equivalent)
2. `compute_diagnostic_features()`
3. optional `score_and_rank()` (thin wrapper)
4. `run_screener()` as orchestration, but keep feature layer independent

### Preserve from old logic
- per-symbol debugging utility
- feature set (returns/vol/PK/GK/drawdown/ulcer/liquidity/Amihud)
- ability to return features without ranking
- unresolved-adjustment policy checks

### Architectural rule
Feature computation should not be trapped inside “screener”.
Screener becomes a thin consumer of the feature table.

### Deliverable
Stable diagnostic feature dataset generation + optional ranking.

---

## Phase 6 — Add model-ready feature interfaces (foundation for portfolio system)

### Goal
Prepare the handoff from data engine to actual modeling stack without implementing the full stack yet.

### Tasks
Define interfaces/contracts for later modules:

#### A) Price/returns matrices for modeling
- aligned return matrix builders
- universe masks / liquidity masks
- missingness handling policies
- date alignment rules

#### B) Market state primitives (planned)
At minimum, reserve interfaces for:
- cross-sectional dispersion
- market-mode dominance / top eigenvalue proxy
- realized vol / vol-of-vol
- breadth / liquidity breadth

#### C) Trend feature hooks
Reserve clean inputs for:
- TSMOM horizons
- Kalman trend states / filtered estimates
- volatility-scaled momentum variants (later)

#### D) Risk backbone hooks
Reserve clean inputs for:
- covariance estimation windows
- correlation matrices
- shrinkage/denoising choices
- PCA outputs
- Glasso outputs
- HRP clustering/allocation inputs

### Deliverable
Clear model input builders and schemas (even if model implementation is staged later).

---

## Phase 7 — Implement modeling stack in layers (later, but planned now)

This phase is not immediate, but it is part of the architectural target.

## 7.1 Trend experts (first modeling layer)
- Kalman trend primitives
- TSMOM primitives
- signal normalization / combination

## 7.2 Risk backbone
- PCA diagnostics
- Graphical Lasso precision/correlation structure
- HRP allocation tree / weights

## 7.3 Gating / regime layer
- market state features
- risk-on/risk-off / leverage / exposure modulation
- signal confidence gating

## 7.4 Portfolio construction / allocator
- combine signals + risk + gating
- constraints / turnover / liquidity limits
- weights output
- backtest harness

---

## 10) Testing strategy (non-negotiable)

## 10.1 Fixtures-first tests (small, deterministic)
Use tiny fixtures to test exact logic:
- duplicate dedupe rule
- split orientation inversion selection
- date snapping
- dividend rescue edge case
- residual jump flagging
- event aggregation (same-day split + dividend)
- unresolved policy in screener/features

## 10.2 Contract tests
Every public build step validates outputs:
- required columns
- key uniqueness
- types
- sortedness (where relevant)

## 10.3 Golden behavior tests
For a few known symbols / windows, preserve expected outcomes:
- split-adjusted continuity improves
- known split dates appear in audit/apply tables
- no major regressions in adjusted panel row counts / keys

## 10.4 Integration tests (lightweight)
A small end-to-end run:
- build universe (small window)
- selective CA
- panel adj
- diagnostic features

This ensures orchestration doesn’t rot.

---

## 11) Caching and filesystem strategy (clean and explicit)

## 11.1 Principles
- cache is a performance optimization, not truth
- cache mode should be configurable and visible
- caches should be easy to invalidate by path/tag

## 11.2 Planned cache areas
- `data/cache/b3_universe/`
- `data/cache/rb3/`
- `data/cache/corp_actions/`
  - batch tags
  - by-symbol files

## 11.3 Cache modes to preserve
- `batch`
- `by_symbol`
- `none`

## 11.4 Reproducibility note
Runs must log:
- config summary
- date range
- cache mode
- fetch mode
- candidate counts
- audit/quarantine counts

---

## 12) Config strategy (what stays configurable)

We will keep a config object (list) but validate it more systematically.

Important knobs to preserve:
- universe years / asset types
- liquidity thresholds
- selective CA toggle
- CA cache mode
- CA fetch mode (chart / quantmod)
- candidate prefilter thresholds
- split validation:
  - tolerance
  - snap forward/back limits
  - open-vs-close validation preference
- adjustment residual jump tolerance
- unresolved policy for feature/screener layer

We should also add:
- explicit “candidate policy” mode (future option), e.g.:
  - `union_liquid_plus_gap` (default)
  - `liquid_only`
  - `all_symbols`

This keeps the philosophy explicit and testable.

---

## 13) Public API (proposed stable entry points)

These are the functions we should design as the main user-facing calls.

## 13.1 Data engine
- `build_universe(...)`
- `build_universe_window(...)`
- `build_corp_actions_registry(...)`
- `build_panel_adj(...)`
- `build_panel_adj_selective(...)`

## 13.2 Diagnostics / features
- `diag_symbol(...)`
- `build_diagnostic_features(...)`
- `run_screener(...)` (thin wrapper; optional scoring)

## 13.3 Later model pipeline (planned)
- `build_model_inputs(...)`
- `compute_market_state_features(...)`
- `fit_trend_experts(...)`
- `fit_risk_backbone(...)`
- `allocate_portfolio(...)`

---

## 14) Immediate next coding steps (practical order for us to execute)

This is the short execution checklist.

### Step 1 — New repo + package bootstrap
- create fresh repo
- initialize R package
- set up `testthat`, `roxygen2`
- add runtime dirs to `.gitignore`

### Step 2 — Core utilities and contracts
- config defaults + validation
- assertions (`assert_cols`, `assert_no_dupes`)
- logging
- contract docs/tests

### Step 3 — Rebuild B3 universe module
- fetch + normalize + liquidity + dedupe + cache
- test against fixtures and sample window

### Step 4 — Rebuild CA module (selective-first)
- candidate selector
- Yahoo chart fetcher + fallback
- cache modes + retry
- registry contract tests

### Step 5 — Rebuild split reconciliation + adjuster
- split audit/quarantine
- events
- adjuster
- panel builder
- residual jump safety
- heavy tests

### Step 6 — Rebuild diagnostics + diagnostic feature layer
- `diag_symbol`
- feature computation
- optional screener ranking

### Step 7 — Define model input interfaces (no full modeling yet)
- return matrices
- universe masks
- market-state primitive placeholders/contracts

---

## 15) “Done” criteria for this refactor stage (so we know when to stop)

This refactor stage is done when all are true:

### A) Engineering quality
- no manual source-order bootstrapping
- no `exists()` checks controlling core logic
- package loads cleanly
- tests run and pass

### B) Functional parity (important behaviors preserved)
- raw universe build works (year/window)
- selective CA fetch works (candidate prefilter + Yahoo chart mode)
- split reconciliation works (snap/orientation/audit/quarantine)
- adjusted panel builds and validates
- diagnostic features build successfully
- symbol-level diagnostics can trace problems

### C) Future compatibility
- outputs are clean enough to feed model inputs later
- diagnostic feature layer is clearly separated from future model feature layer
- architecture supports trend/risk/gating modules without refactoring again

---

## 16) Final architectural stance (to avoid future confusion)

### We are not rebuilding a “screener project.”
We are building a **market data + adjustment + feature foundation** for a larger quantitative system.

### The current screener/feature metrics are diagnostic tools.
They stay, but they do not define the final modeling architecture.

### Selective corporate actions is still the central v2 idea.
We preserve it, make it cleaner, and make the policy explicit.

### The rewrite is from scratch.
The old codebase is a behavioral reference, not a code template.

---

## 17) Notes for future Python migration (do not implement now, but design for it)

Because a Python migration is likely later, this R rewrite should keep:
- explicit contracts (table schemas)
- side-effect boundaries (fetch/cache vs transform)
- small pure functions for math/validation
- config-driven orchestration
- test fixtures that can be reused as cross-language parity tests

If we do this correctly, the Python port becomes:
- a reimplementation of stable contracts + tests,
not a reverse-engineering exercise from messy scripts.

---

## 18) Short summary (for quick recall)

- Rebuild from scratch in a **new R package repo**
- Preserve old logic, not old structure
- Prioritize:
  1. `universe_raw`
  2. selective CA registry
  3. split reconciliation + adjuster
  4. `panel_adj`
  5. diagnostic features
- Keep diagnostic features separate from future model features
- Design outputs now to support:
  - Kalman + TSMOM
  - PCA→Glasso→HRP
  - market-state gating
  - portfolio allocation later
- Use fixtures/tests to lock in behavior and avoid regressions