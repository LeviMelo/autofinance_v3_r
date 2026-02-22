# Model & Backtest Engine Plan (Cursor-Ready)

## 0) Why this document exists

This file is the **build plan for the next development cycle** after stabilizing the `data_engine`.

It is written to be **handed to Cursor (and/or Gemini inside Cursor)** so code can be generated in a controlled way **without architectural drift**.

This plan intentionally reflects the current project philosophy:

- **simple, layered, linear**
- **human-readable first**
- **modular where it matters, not fragmented for the sake of abstraction**
- **math-driven organization** (the code should mirror the pipeline)
- **strict no-lookahead discipline**
- **data repair stays upstream; quant logic stays downstream**

It also explicitly corrects a prior over-fragmented proposal: the new structure is **compressed on purpose** to reduce cognitive overhead while keeping swappable boundaries.

---

## 1) Project context (what we are building)

We are building a **personal market analysis + portfolio management system for B3** (Brazil), with these core characteristics:

- **OHLCV-only** (adjusted prices; no fundamentals/news)
- Daily state estimation and portfolio proposal
- Backtesting (walk-forward simulation)
- Rebalancing logic (cash, caps, turnover, costs)
- Model iteration (swap signal/risk components without rewriting the pipeline)
- Later: UI/service layer (likely Plumber), but **not now**

### Strategic model direction (already chosen)
The modeling stack is centered on:

- **Risk backbone (locked conceptually):**
  - PCA factor structure
  - residual covariance
  - Glasso (precision regularization)
  - covariance assembly
  - HRP allocator

- **Return/signal engine (initial implementation):**
  - Kalman trend
  - TSMOM trend
  - vol-scaled, bounded scores

- **Market-state / control:**
  - dispersion
  - market-mode dominance (`eta`)
  - vol-of-vol (`VoV`)
  - softmax gating → expert trust + cash mass

This is the conceptual design target. The implementation should be staged and testable.

---

## 2) Where we are now (data engine status, and what it means)

The `data_engine` has been built and hardened enough to become a stable upstream dependency.

### What the data engine currently does
It already covers the full upstream data pipeline:

1. **B3 OHLCV ingestion** via `rb3`
2. Universe building and asset-type filtering (equity, FII, ETF, BDR)
3. Liquidity field normalization (`turnover`, `qty`)
4. Corporate actions fetch (Yahoo-based CA registry)
5. Split validation / snapping vs raw gap behavior
6. Event normalization (`events_apply`)
7. Price adjustment application (split + dividend logic)
8. Debug/audit outputs and residual jump audits
9. Optional diagnostic features sidecar (`features_diag`)

### Important outputs (already usable downstream)
The main downstream artifact is:

- `panel_adj_model`: adjusted OHLC + liquidity + `adjustment_state`

And supporting audits/debug outputs:

- `panel_adj_debug`
- `events_apply`
- `corp_actions_raw`
- `corp_actions_apply`
- `split_audit`
- `prefilter_audit`
- `adjustments_timeline`
- `residual_jump_audit`
- `meta`

### Current practical conclusion
The data engine is **good enough to proceed**. We should **not** redesign it to support modeling/backtesting. Instead, we build **downstream adapters** that read `panel_adj_model` safely (especially for as-of slicing and no-lookahead).

### Explicit boundary (important)
The data engine is **not** the place to implement:

- investability blacklists for modeling
- signal/risk lookbacks
- no-lookahead trading semantics
- backtesting execution assumptions
- portfolio constraints / turnover management

Those belong downstream.

This boundary is deliberate and correct.

---

## 3) Architectural correction (reflecting the feedback on over-fragmentation)

A previous proposal split the system into many tiny folders/files. That may look “clean” on paper, but in practice it increases:

- navigation cost
- context switching
- onboarding friction
- maintenance burden
- “where does this live?” confusion

### New direction (compressed, still layered)
We will keep a **small number of files**, each representing a **coherent stage** in the modeling/backtesting pipeline.

We still preserve separation of concerns, but at the level a human can reason about quickly.

### Guiding rule for file splits
Split a file **only when at least one of these becomes true**:

1. It becomes too long to scan and reason about (roughly >500–700 LOC of dense logic)
2. It contains two domains that evolve independently (e.g., risk estimation vs order execution)
3. It materially harms testing (tests become awkward because functions are tangled)
4. Multiple developers are stepping on the same file repeatedly (less relevant now, but still a sign)

Until then, **compress**.

---

## 4) High-level target architecture (simple and linear)

We will build **two downstream modules** with a hard boundary:

- `model_engine` → pure quant state + target portfolio proposal (for one date)
- `backtest_engine` → simulation loop + execution + accounting + analytics

This is the clean split:

- `model_engine` answers: **“What should we hold at date t, given only data up to t?”**
- `backtest_engine` answers: **“What happens if we execute those targets over time under trading rules?”**

### Why this matters
This prevents:
- hidden lookahead
- model code contaminated by execution/accounting details
- backtester being tied to one specific model

This also keeps future model swaps cheap.

---

## 5) Proposed repository structure (compressed, human-readable)

This is the recommended structure for the next cycle.

```text
/autofinance_v3_r/
  /data_engine/                         # finished upstream module (stable)
    /R/
      00_contracts.R
      01_config.R
      02_utils.R
      03_providers.R
      04_universe.R
      05_corp_actions.R
      06_adjuster.R
      07_diagnostics_features.R
      08_pipeline.R

  /model_engine/
    /R/
      00_contracts_and_spec.R           # contracts + spec validation + small shared helpers
      01_data_adapter.R                 # as-of slices, matrix builders, no-lookahead-safe views
      02_risk_engine.R                  # vol, PCA, residuals, Glasso, Sigma assembly, HRP
      03_signal_engine.R                # Kalman + TSMOM + score scaling/combination primitives
      04_state_and_gating.R             # disp, eta, VoV + softmax gating
      05_portfolio_constructor.R        # HRP tilt, caps, cash integration, target weights
      06_snapshot_runner.R              # orchestrates one-date model run; returns snapshot artifact

  /backtest_engine/
    /R/
      00_contracts_and_spec.R           # backtest spec, strategy interface contract, validators
      01_data_and_clock.R               # date calendar, decision/execution schedule, adapters
      02_execution_and_costs.R          # order generation, fill pricing, fees/slippage
      03_accounting.R                   # positions, cash, NAV, turnover, ledgers
      04_runner.R                       # walk-forward loop (model-agnostic), logs/artifacts
      05_analytics.R                    # performance/risk metrics, drawdowns, summaries
```

### Why this structure is better for this project
It preserves a **linear reading order**:

- contracts/spec
- data views
- risk
- signals
- state/control
- portfolio build
- snapshot orchestration

and then the backtesting sequence:

- contracts/spec
- clock/data feed
- execution
- accounting
- runner
- analytics

That is both **mathematically natural** and **human-scannable**.

---

## 6) What stays stable vs what is expected to change frequently

This matters because it guides code organization and testing intensity.

### Stable (or relatively stable) layers
- `data_engine` contracts and adjustment pipeline
- `model_engine` snapshot output schema (artifact contract)
- `backtest_engine` accounting semantics and core loop semantics
- no-lookahead rules

### Frequently changing layers
- signal formulas and parameters
- risk estimation hyperparameters (PCA rank, Glasso lambda, lookbacks)
- gating logic and parameters
- portfolio tilt/caps policies
- cost model realism
- investability filters (downstream universe masks)

Therefore:
- Keep contracts and runners clean and heavily validated
- Expect risk/signals/portfolio code to be iterative and designed for easy swaps

---

## 7) The data engine interface to downstream (how we use what we already built)

This is the bridge. We do **not** need to redesign `data_engine`; we need a disciplined adapter.

### Canonical downstream input
`model_engine` and `backtest_engine` will consume (at minimum):

- `data_bundle$panel_adj_model`

Optional downstream diagnostics (for filtering/auditing only):
- `data_bundle$residual_jump_audit`
- `data_bundle$split_audit`
- `data_bundle$prefilter_audit`
- `data_bundle$meta`

### Why `panel_adj_model` is enough for core quant work
It already contains adjusted:

- `open`, `high`, `low`, `close`
- `turnover`, `qty`
- `asset_type`
- `symbol`, `refdate`
- `adjustment_state`

This is sufficient to build:
- price matrices
- return matrices
- liquidity masks
- execution price vectors
- as-of slices
- rolling windows

### Important downstream policy (already aligned with project direction)
Potentially “mad”/junk/uninvestable names (including some suspect adjustment states) should be handled by **downstream investability logic**, not by mutating `data_engine`.

That keeps the upstream pipeline faithful to source data + adjustments while allowing modeling-specific exclusions later.

---

## 8) Core design principle for the model engine: snapshot semantics

The `model_engine` is not a time-loop simulator. It is a **single-date snapshot estimator**.

### Snapshot definition
Given:
- `as_of_date = t`
- `data_bundle` (or `panel_adj_model`)
- `ModelSpec`
- optional `prev_target` / `current_portfolio_state` (for turnover-aware targeting later)

the model engine returns a **SnapshotArtifact** with:

- target weights (including cash)
- all intermediate diagnostics needed for debugging
- metadata (spec hash, windows used, warnings)

### Why snapshot-first is the right design
It gives:
- easier testing (pure-ish function)
- cleaner backtester integration
- easy UI usage later (query “state on date t”)
- controlled caching of expensive artifacts (future)

---

## 9) Model engine file-by-file plan (compressed version)

This section is the implementation spec Cursor should follow.

---

### `model_engine/R/00_contracts_and_spec.R`

**Purpose:** Define contracts, validators, enums, spec defaults, and shared structural helpers.

This file should contain:

#### A) ModelSpec default builder and validator
Functions:
- `me_spec_default()`
- `me_get_spec(overrides = NULL)`
- `me_validate_spec(spec)`

The spec must be a list with sublists (minimum):
- `data`
- `risk`
- `signals`
- `market_state`
- `gating`
- `portfolio`
- `meta`

#### B) Snapshot artifact contract
Functions:
- `me_validate_snapshot_artifact(x)`

Artifact must contain (minimum):
- `as_of_date`
- `tradable_symbols`
- `target_weights`
- `cash_weight`
- `risk`
- `signals`
- `market_state`
- `gating`
- `portfolio_diag`
- `meta`
- `warnings`

#### C) General validators/helpers
Examples:
- scalar numeric/int/logical validators
- simplex checks (weights sum to 1)
- named-vector checks
- matrix finite/symmetric checks

#### D) Spec hashing helper (optional but recommended now)
- `me_hash_spec(spec)` (deterministic serialization + digest)

> Keep this file mostly declarative and validation-oriented. Avoid quant math here.

---

### `model_engine/R/01_data_adapter.R`

**Purpose:** Wrap `panel_adj_model` into no-lookahead-safe access functions and matrix builders.

This is the most important guard layer for correctness.

#### Main constructor
- `me_make_data_adapter(panel_adj_model, aux = list())`

Returns a list of closures/functions (or a structured list) that provide controlled access to data.

#### Required responsibilities
1. Validate and canonicalize panel:
   - data.table conversion
   - sort by `symbol, refdate`
   - enforce required columns
   - dedupe checks
2. Provide calendar access
3. Provide as-of slicing
4. Build aligned price and return matrices
5. Build execution price vectors
6. Build basic liquidity/investability masks

#### Required functions exposed by adapter (exact names can vary, but behavior should match)
- `calendar()`
- `panel_upto(as_of_date)`
- `panel_window(start_date, end_date)` (optional helper)
- `symbols_at(as_of_date)` (symbols with a row on that date)
- `price_matrix(as_of_date, lookback, field = "close", symbols = NULL, strict = TRUE)`
- `returns_matrix(as_of_date, lookback, field = "close", method = "log", symbols = NULL, strict = TRUE)`
- `execution_price(exec_date, field = "open", symbols = NULL)`
- `investability_snapshot(as_of_date, lookback_liq = 63L, rules = NULL)`

#### No-lookahead guarantees (must be explicit in code)
- Every `*_matrix(..., as_of_date=t)` must only use rows with `refdate <= t`
- No helper may silently read beyond `as_of_date`
- Add defensive assertions in development mode (or always, if cheap enough)

#### Missing data policy (v1)
Start strict:
- if `strict = TRUE`, include only assets with sufficient contiguous observations in requested window
- do not impute missing returns initially

This will reduce hidden numerical issues in risk estimation.

---

### `model_engine/R/02_risk_engine.R`

**Purpose:** All risk estimation and baseline allocation logic (linear chain, one file).

This file should implement the “locked” risk backbone in stages.

#### Functions (grouped, but can remain in one file)
1. **Volatility estimation**
   - `me_estimate_vol(R_window, spec_risk_vol)`
   - supports `rolling_sd` and/or `ewma`
   - returns named vector `sigma_t`

2. **PCA factor model**
   - `me_fit_pca_factor(R_window, spec_pca)`
   - returns list with:
     - loadings `B`
     - factor returns `F`
     - explained variance info
     - any centering/scaling metadata

3. **Residualization**
   - `me_residualize_returns(R_window, pca_fit)`

4. **Residual covariance and Glasso**
   - `me_fit_residual_cov(E_window, spec_resid)`
   - computes sample covariance
   - applies Glasso if selected
   - returns:
     - residual covariance
     - precision
     - sparsity diagnostics
     - any numerical flags

5. **Factor covariance**
   - `me_factor_cov(F_window, spec_factor_cov)`

6. **Covariance assembly**
   - `me_assemble_total_cov(pca_fit, Sigma_f, Sigma_eps)`

7. **Numerical repairs / checks**
   - `me_cov_sanity(Sigma, repair = TRUE/FALSE, ...)`
   - symmetry, finite checks, PSD checks
   - if repaired, set flags (do not silently hide)

8. **HRP allocator**
   - `me_allocate_hrp(Sigma, spec_hrp)`
   - returns baseline risky weights

9. **Top-level risk pipeline**
   - `me_run_risk_engine(R_window, spec_risk)`
   - returns rich list artifact with:
     - `sigma_t`
     - `Sigma_total`
     - `Sigma_f`
     - `Sigma_eps`
     - `Theta_eps` (if available)
     - `w_hrp`
     - diagnostics

#### Design notes
- Keep the file linear; do not split unless it becomes unmanageable.
- Use clear internal subheaders.
- Preserve raw diagnostics because this is where numerical pathologies appear.

---

### `model_engine/R/03_signal_engine.R`

**Purpose:** Implement trend experts and score normalization in one place.

This file should contain:

#### A) Kalman trend score
- `me_signal_kalman(prices_window, sigma_t, spec_kalman)`

Output:
- named vector of bounded scores `s_kalman`
- diagnostics (coverage, slope raw values, failed fits count)

#### B) TSMOM trend score
- `me_signal_tsmom(R_window, sigma_t, spec_tsmom)`

Output:
- named vector of bounded scores `s_tsmom`
- diagnostics (coverage, horizon used, raw momentum values)

#### C) Score scaling/bounding helpers
- tanh bounding
- safe standardization helpers (if needed)
- NA handling policy (explicit)

#### D) Expert alignment / combination helper (pre-gating)
- `me_align_signal_vectors(...)`
- returns aligned named vectors on same symbol set

#### E) Top-level signal pipeline
- `me_run_signal_engine(prices_window, R_window, sigma_t, spec_signals)`

Returns:
- `scores` list (e.g., `kalman`, `tsmom`)
- diagnostics
- symbol coverage info

#### Design note
Do not mix market-state or gating here. Signals produce expert scores only.

---

### `model_engine/R/04_state_and_gating.R`

**Purpose:** Compute market-state features and map them to expert trust + cash mass.

This file keeps “state estimation” and “control policy” together because both are short and tightly linked.

#### A) Market-state features
Implement:
- `me_state_dispersion(r_t, spec_dispersion)`
- `me_state_mode_dominance(R_state_window, spec_eta)` → `eta_t`
- `me_state_vov(R_hist, spec_vov, sigma_history = NULL)` → `VoV_t`
- `me_build_market_state(R_window, risk_artifact, spec_market_state)`

Return:
- named numeric vector `m_t` (at least `disp`, `eta`, `VoV`)
- diagnostics (sample sizes, windows, fallbacks)

#### B) Gating logic (initial = softmax linear)
- `me_gating_softmax_linear(m_t, spec_gating)`

Returns:
- logits
- weights on experts + cash mass (e.g., `w_kalman`, `w_tsmom`, `w_cash`)
- `gross_exposure = 1 - w_cash`

#### C) Validation checks
- finite values
- simplex check
- named output consistency

#### D) Top-level function
- `me_run_state_and_gating(R_window, risk_artifact, spec_market_state, spec_gating)`

Returns:
- `market_state`
- `gating`

#### Design note
Keep gating transparent. Avoid hidden heuristics in v1.

---

### `model_engine/R/05_portfolio_constructor.R`

**Purpose:** Convert risk baseline + signals + gating into target weights (still no execution).

This file is where the portfolio proposal is assembled.

#### A) Score combination (gated)
- `me_combine_expert_scores(signal_artifact, gating_artifact, spec_portfolio)`
- returns combined score vector `S_t`

#### B) Score → tilt multipliers
- `me_score_to_tilt(S_t, spec_tilt)`
  - rank/zscore + bounded map (e.g., tanh)
  - output positive multipliers

#### C) Apply tilt to HRP baseline
- `me_apply_tilt_to_baseline(w_hrp, tilt_mult, spec_portfolio)`

#### D) Caps/bounds/normalization
- `me_apply_weight_caps(w, spec_caps)`
- optional iterative renormalization (simple and deterministic)

#### E) Cash integration
- use gating gross exposure `g_t`
- `w_final_risky = g_t * w_risky`
- `w_cash = 1 - g_t`

#### F) Portfolio diagnostics
Include:
- concentration (Herfindahl)
- top weights
- active deviation vs HRP baseline
- exposure stats
- flags/warnings

#### G) Top-level function
- `me_build_portfolio_target(risk_artifact, signal_artifact, state_gating_artifact, spec_portfolio, prev_target = NULL)`

Returns:
- target weight table/vector
- portfolio diagnostics

> Turnover-aware smoothing can be added later, but wire the function signature now (`prev_target` optional).

---

### `model_engine/R/06_snapshot_runner.R`

**Purpose:** Orchestrate the full one-date modeling pipeline and return a single SnapshotArtifact.

This is the main entrypoint for both manual runs and backtest integration.

#### Main entrypoint (recommended)
- `me_run_snapshot(data_bundle_or_panel, as_of_date, spec = NULL, prev_target = NULL, aux = list())`

#### Responsibilities (in order)
1. Build or reuse data adapter
2. Validate `as_of_date` exists in calendar (or choose nearest previous trading day if explicitly enabled)
3. Build downstream investability/universe mask (from spec rules)
4. Build price/returns windows
5. Run risk engine
6. Run signal engine
7. Run state + gating
8. Run portfolio constructor
9. Assemble SnapshotArtifact
10. Validate artifact contract
11. Return artifact

#### SnapshotArtifact schema (recommended shape)
Top-level list:
- `as_of_date`
- `symbols` / `tradable_symbols`
- `target_weights` (table: symbol, weight_target)
- `cash_weight`
- `risk` (artifact)
- `signals` (artifact)
- `market_state`
- `gating`
- `portfolio_diag`
- `meta` (spec hash, windows used, timestamps, warnings)
- `warnings` (character vector)

#### Important behavior (v1)
If insufficient history prevents a valid run:
- fail clearly (preferred), or
- return a valid “all cash” target with warning if spec allows fallback

Do not silently degrade into nonsense.

---

## 10) Backtest engine file-by-file plan (compressed version)

The backtester remains model-agnostic and consumes a strategy interface that can call `model_engine`.

---

### `backtest_engine/R/00_contracts_and_spec.R`

**Purpose:** Backtest spec, strategy contract, validation helpers.

#### A) BacktestSpec
Functions:
- `bt_spec_default()`
- `bt_get_spec(overrides = NULL)`
- `bt_validate_spec(spec)`

Minimum sub-specs:
- `schedule`
- `execution`
- `costs`
- `accounting`
- `analytics`
- `meta`

#### B) Strategy interface contract
Define expected callable shape (function or list contract), e.g.:

- `strategy_propose_target(as_of_date, current_state, data_context, strategy_spec)`

Return must include (minimum):
- `decision_date`
- `target_weights`
- `cash_weight`
- optional diagnostics/artifact IDs
- warnings/errors

#### C) Result artifact contracts
- `bt_validate_run_artifact(x)`
- `bt_validate_nav_series(x)` (optional helper)

---

### `backtest_engine/R/01_data_and_clock.R`

**Purpose:** Manage simulation calendar, decision dates, execution dates, and safe access to prices.

#### Responsibilities
1. Build backtest calendar from panel data
2. Generate rebalance schedule
3. Map decision date → execution date
4. Provide execution price vectors and mark-to-market prices (through adapter helpers)
5. Enforce no-lookahead boundary in simulation logic

#### Functions
- `bt_make_data_context(data_bundle_or_panel, aux = list())`
- `bt_calendar(data_ctx)`
- `bt_rebalance_dates(calendar, spec_schedule)`
- `bt_next_exec_date(calendar, decision_date, spec_execution)`
- `bt_get_exec_prices(data_ctx, exec_date, field = "open", symbols = NULL)`
- `bt_get_mark_prices(data_ctx, date, field = "close", symbols = NULL)`

#### Note
This file may internally reuse `model_engine`’s adapter semantics or instantiate the same style of adapter. Keep behavior consistent.

---

### `backtest_engine/R/02_execution_and_costs.R`

**Purpose:** Convert target weights into trades/fills and apply costs.

#### Responsibilities
1. Generate orders from current portfolio → target portfolio
2. Compute execution notionals using chosen price model
3. Apply transaction costs / slippage
4. Return fills + cost breakdown

#### Functions
- `bt_generate_orders(current_positions, current_nav, target_weights, exec_prices, spec_execution)`
- `bt_apply_costs(orders, exec_prices, spec_costs)`
- `bt_execute_rebalance(current_state, target, exec_prices, spec_execution, spec_costs)`

#### Initial assumptions (v1)
Keep simple and explicit:
- full fills (no partials)
- long-only
- next-open execution
- proportional fees/slippage
- cash absorbs residual

These assumptions can be upgraded later without changing the backtest runner contract.

---

### `backtest_engine/R/03_accounting.R`

**Purpose:** Portfolio state transitions, NAV, PnL, turnover, cash.

This file should be boring and deterministic.

#### Responsibilities
- maintain positions ledger (shares or weight/notional representation; choose one and be consistent)
- maintain cash
- compute NAV after fills
- compute NAV through time between rebalances
- compute turnover
- maintain per-step logs

#### Functions (illustrative)
- `bt_init_portfolio_state(initial_nav = 1.0, initial_cash = 1.0)`
- `bt_update_after_execution(state, fills, exec_prices, costs)`
- `bt_mark_to_market(state, mark_prices, date)`
- `bt_compute_turnover(pre_weights, post_target_weights)`
- `bt_position_weights(state, mark_prices)`

#### Representation recommendation (v1)
Use **shares + cash** internally (more realistic and less error-prone for execution simulation), even if targets are weights.

---

### `backtest_engine/R/04_runner.R`

**Purpose:** The walk-forward simulation loop (main backtest entrypoint).

This file should not contain quant math; it orchestrates time, strategy calls, execution, and accounting.

#### Main entrypoint
- `bt_run_backtest(data_bundle_or_panel, strategy_fn, strategy_spec, bt_spec = NULL)`

#### Core loop responsibilities
For each rebalance step:
1. Determine `decision_date`
2. Determine `execution_date`
3. Call strategy (`strategy_fn`) with data context and current state
4. Validate target
5. Fetch execution prices
6. Execute rebalance + costs
7. Update positions/cash
8. Mark to market over interval
9. Log artifacts (targets, fills, costs, NAV, diagnostics)

#### No-lookahead rule (must be enforced here too)
The strategy must be called with `decision_date`, and all model inputs must be restricted to `<= decision_date`.

Execution occurs only on `execution_date` and later.

#### Return object
A `BacktestRunArtifact` containing:
- NAV series
- rebalance log
- target log
- fills log
- cost log
- exposure / turnover series
- performance summary (or raw inputs for analytics file)

---

### `backtest_engine/R/05_analytics.R`

**Purpose:** Compute summary metrics and diagnostics from backtest outputs.

#### Metrics (v1)
- cumulative return
- CAGR (if date range sufficient)
- annualized volatility
- Sharpe (simple risk-free = 0 initially)
- max drawdown
- drawdown duration (optional)
- turnover stats
- cost drag summary
- exposure stats (avg cash/risky exposure)
- rebalance count / hit rate style diagnostics (optional)

#### Functions
- `bt_compute_performance(nav_series, spec_analytics)`
- `bt_compute_drawdowns(nav_series)`
- `bt_compute_backtest_summary(run_artifact, spec_analytics)`

Keep reporting numeric and simple. Fancy visualization can come later.

---

## 11) The most important correctness constraint: no-lookahead (detailed)

This needs to be treated as an engineering requirement, not a slogan.

### Date semantics (must be encoded explicitly)
Every rebalance step has at least:

- `decision_date`: last date whose data the model is allowed to inspect
- `execution_date`: date on which target is implemented (e.g., next trading day open)

Typical rule:
- decide using close data up to `t`
- execute at open on `t+1`

### Where no-lookahead is enforced
It must be enforced in **three places**:

1. **Model data adapter (`model_engine/01_data_adapter.R`)**
   - every as-of slice and matrix builder truncates at `decision_date`

2. **Backtest clock (`backtest_engine/01_data_and_clock.R`)**
   - controls decision/execution mapping
   - strategy never chooses its own future execution date silently

3. **Backtest runner (`backtest_engine/04_runner.R`)**
   - passes only `decision_date` to strategy
   - fetches execution prices from `execution_date`
   - assertions for date ordering

### Suggested runtime assertions (implement them)
- no model input row has `refdate > decision_date`
- `execution_date > decision_date` (for next-open convention)
- no PnL interval starts before execution
- all returned target symbols exist in executable universe at `execution_date` (or are zeroed with warning)

This is the kind of “defensive programming” that saved the data engine; we want the same posture here.

---

## 12) Modeling/backtesting universe policy (downstream, not in data engine)

This was a key project point and should be codified clearly.

### Upstream (`data_engine`)
Produces a broad adjusted panel (including names we may later avoid trading).

### Downstream (`model_engine` / `backtest_engine`)
Implements **investability filters** for strategy purposes, such as:
- minimum recent turnover
- minimum traded-days ratio
- minimum history length
- price sanity bounds
- optional exclusion of extreme suspect states (`adjustment_state`)
- optional blacklist table (manual)

### Why this split is correct
It avoids contaminating raw/adjusted data construction with strategy assumptions. Different strategies may choose different investability rules.

This keeps the whole system extensible.

---

## 13) Proposed staged implementation plan (what Cursor should build, in order)

This is the execution roadmap. The order matters.

---

### Phase 0 — Skeleton + contracts + adapters (must come first)

**Goal:** Establish stable interfaces before math and simulation complexity.

#### Deliverables
- `model_engine` file skeleton (all 7 files)
- `backtest_engine` file skeleton (all 6 files)
- spec defaults + validators (model + backtest)
- `model_engine` data adapter (`panel_adj_model` wrapper)
- `backtest_engine` data/clock helpers
- placeholder snapshot runner (returns equal-weight target)
- placeholder backtest runner (simulates equal-weight periodic rebalances)

#### Acceptance criteria
- Can run a dummy snapshot at a valid `as_of_date`
- Can run a dummy backtest (equal-weight, monthly rebalance)
- Contracts validate outputs
- no-lookahead assertions pass in basic tests

> Do not implement PCA/Glasso/Kalman yet in this phase.

---

### Phase 1 — Minimal real end-to-end strategy (simple but functional)

**Goal:** Prove the whole stack works end-to-end with a simple model.

#### Model engine (minimal)
- returns matrix builder
- volatility estimator (simple rolling or EWMA)
- sample covariance + HRP (can skip PCA+Glasso in this phase if needed)
- TSMOM signal only
- simple portfolio target (no gating, no cash)
- snapshot artifact with diagnostics

#### Backtest engine
- monthly rebalance clock
- next-open execution
- proportional costs/slippage
- accounting + NAV
- basic analytics

#### Acceptance criteria
- Backtest runs over a date range without manual patching
- Results are reproducible
- No-lookahead checks pass
- Metrics and logs look sane

This phase proves the architecture, not the final model sophistication.

---

### Phase 2 — Replace simple risk with locked risk backbone (PCA → Glasso → HRP)

**Goal:** Upgrade the risk engine without changing backtest runner behavior.

#### Deliverables
- PCA factor fit
- residualization
- Glasso residual precision/covariance
- covariance assembly
- numerical sanity checks/repairs
- HRP on assembled covariance
- risk diagnostics (eigen spectrum, sparsity, repair flags)

#### Acceptance criteria
- Covariance matrices finite and usable
- HRP weights valid
- Backtest still runs without changing core loop
- Diagnostics capture failures/pathologies explicitly

This phase validates the value of the model/backtest separation.

---

### Phase 3 — Full signal engine (Kalman + TSMOM), ungated combination first

**Goal:** Add both trend experts and align score contracts.

#### Deliverables
- Kalman trend score module
- TSMOM score module
- score alignment helpers
- ungated linear combination (fixed weights or spec-defined blend)
- signal diagnostics (coverage, score correlation)

#### Acceptance criteria
- Both signals produce bounded, aligned score vectors
- Snapshot runner supports switching experts on/off via spec
- Backtest runs with either expert or both

---

### Phase 4 — Market-state + gating + cash exposure

**Goal:** Introduce adaptive control (expert trust + risk-off cash mass).

#### Deliverables
- `disp`, `eta`, `VoV`
- softmax gating
- gating diagnostics (logits/weights over time)
- portfolio constructor uses `g_t = 1 - w_cash`
- cash weight explicitly integrated into targets and backtests

#### Acceptance criteria
- `w_k + w_m + w_cash == 1` (within tolerance)
- Backtest reflects time-varying cash exposure
- Gating behavior is inspectable and stable (no silent NA collapse)

---

### Phase 5 — Constraints and realism polish (targeted, not sprawling)

**Goal:** Improve execution realism and portfolio constraints without redesign.

#### Deliverables
- weight caps and floor handling
- turnover control/bands (simple deterministic version)
- optional liquidity trade cap (based on turnover proxy)
- cost model refinement
- richer analytics (turnover, cost drag, exposure stats)

#### Acceptance criteria
- Constraints are enforced deterministically
- Cost drag and turnover are measurable and traceable
- No hidden behavior changes in runner semantics

---

### Phase 6 — Optional artifact caching + service integration prep

**Goal:** Prepare for UI/API without prematurely forcing a server architecture.

#### Deliverables
- snapshot/backtest artifact save/load helpers
- spec hashing for reproducibility
- lightweight artifact metadata index (optional)
- stable return shapes for later Plumber endpoints

#### Note
Do this **after** local engines are stable.

---

## 14) Testing plan (serious, defensive, and aligned with how data engine was hardened)

We should keep the same mentality that improved the `data_engine`: test the failure modes where “insanity lives”.

### Model engine tests

#### A) Contract/spec validation tests
- invalid spec enum values fail
- missing spec sections fail
- malformed artifact fails validation
- invalid panel columns fail adapter creation

#### B) No-lookahead adapter tests
Use a tiny synthetic panel and verify:
- `price_matrix(as_of_date=t)` never includes `t+1`
- `returns_matrix` and investability snapshots respect `as_of_date`

#### C) Numerical sanity tests (risk)
- covariance symmetry/finite checks
- PSD/repair flag behavior
- HRP output sums to 1
- no negative weights if long-only

#### D) Signal tests
- Kalman and TSMOM score bounds
- NA handling when insufficient history
- alignment across symbol sets

#### E) Gating tests
- simplex weights sum to 1
- finite logits/weights
- cash mass within [0,1]

#### F) Snapshot regression tests (small frozen slice)
- fixed data slice + fixed spec → stable outputs within tolerance
- at least test selected diagnostics and weight vectors

---

### Backtest engine tests

#### A) Clock and date mapping tests
- monthly/weekly schedule generation
- correct decision → execution mapping
- handling end-of-series dates

#### B) Execution/cost tests
- order generation from current → target weights
- cost calculations on known examples
- full-fill behavior

#### C) Accounting tests
- positions + cash + NAV updates on toy prices
- turnover calculations
- mark-to-market logic

#### D) Integration tests
- dummy equal-weight strategy
- all-cash strategy
- invalid target strategy (should fail clearly)

#### E) Lookahead guard integration tests
- inject impossible future access and assert failure (or explicit guard trigger)

---

## 15) Coding conventions and implementation rules (for Cursor/Gemini)

This section exists to prevent generated code from drifting into unreadable over-abstraction.

### General coding style
- Use `data.table` where tabular operations benefit from it
- Keep function names explicit (`me_*`, `bt_*`)
- Prefer small helper functions inside the same file before creating new files
- Add clear section headers in each file
- Fail loudly on invalid inputs
- Return rich diagnostics instead of silently swallowing issues

### What NOT to do
- Do not create dozens of one-function files
- Do not introduce S4/R6/OOP frameworks unless there is a compelling need
- Do not build a generalized plugin system before the first end-to-end strategy works
- Do not hide no-lookahead assumptions in “magic helpers”
- Do not mix execution/accounting logic into `model_engine`
- Do not move strategy investability filtering into `data_engine`

### Preferred development rhythm
1. Implement contracts + minimal path
2. Add tests
3. Run on real panel slices
4. Inspect artifacts
5. Only then add sophistication

This prevents “architectural speculation” from outrunning working code.

---

## 16) Open questions (to leave configurable, not hard-coded)

These should exist as spec parameters or clearly marked TODOs, not buried constants.

### Modeling choices (expected to evolve)
- risk lookback length
- PCA rank `k`
- Glasso lambda and solver details
- volatility estimator choice (EWMA vs rolling)
- Kalman window and state params
- TSMOM horizon(s)
- gating parameters (`A`, `b`)
- tilt mapping strength
- caps and turnover policies

### Backtest choices (must be explicit)
- decision timing (close of t)
- execution timing (open of t+1)
- slippage model
- transaction cost rates
- rebalancing frequency
- initial NAV and cash handling
- treatment of unavailable execution prices

The key point: **parameterize these in specs**, don’t hardcode them into engine internals.

---

## 17) Definition of done for this next dev cycle (realistic version)

The next cycle should be considered successful when all of the following are true:

1. `model_engine` can produce a validated `SnapshotArtifact` on arbitrary valid dates using `panel_adj_model`
2. `backtest_engine` can run a walk-forward backtest with a strategy function using no-lookahead-safe data access
3. The architecture supports swapping risk/signal components without changing backtest core loop
4. Investability filters are downstream and configurable
5. Outputs include sufficient diagnostics to debug failures/pathologies
6. Codebase remains compressed and readable (few files, linear flow, no over-sharding)

This is a better success criterion than “implement every fancy component immediately.”

---

## 18) Recommended first Cursor prompt (execution-safe, not too large)

Use Cursor to generate code in **phases**, not one giant pass.

### First prompt target (Phase 0 + minimal Phase 1 spine)
Ask Cursor/Gemini to implement:

- `model_engine`:
  - `00_contracts_and_spec.R`
  - `01_data_adapter.R`
  - `06_snapshot_runner.R` (with dummy equal-weight target first)
- `backtest_engine`:
  - `00_contracts_and_spec.R`
  - `01_data_and_clock.R`
  - `03_accounting.R`
  - `04_runner.R`
  - `05_analytics.R`
- basic tests for contracts, no-lookahead, and dummy backtest

Then, in the next pass, add:
- risk engine (`02_risk_engine.R`)
- signal engine (`03_signal_engine.R`)
- portfolio constructor (`05_portfolio_constructor.R`)
- integration into snapshot runner

This sequencing keeps generation grounded.

---

## 19) Final implementation philosophy (short version, to keep pinned)

- **Upstream (`data_engine`) is for data correctness and adjustments**
- **`model_engine` is for one-date quant state + target proposal**
- **`backtest_engine` is for simulation and accounting**
- **No-lookahead is enforced by adapters + clock + runner**
- **Keep the code linear and compressed**
- **Add complexity only when it pays rent**

This preserves both the mathematical structure of the project and the human readability needed to actually maintain it.

