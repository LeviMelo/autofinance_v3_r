CAME — Causal Architecture Model Engine (hard restart)

This folder contains a from-scratch refactor aligned to architecture.md, with minimal operational trims:
- Signal scalarization omega updates use a 1-day-ahead cross-sectional proxy target (still causal), configurable via spec$signals$scalarization$omega_target_horizon.
- Forecast uncertainty uses diagonal component error covariance (Ω initialized/maintained diagonal), which is permitted by the architecture.

No silent fallbacks are used. Cold-start behavior is enforced by requiring sufficient matured history before fitting forecast models; otherwise the runner errors unless strict=FALSE (not recommended).

Entry point:
- came_run_snapshot(data_bundle_or_panel, as_of_date, spec=NULL, state=NULL, prev_target=NULL)

All functions are prefixed with 'came_' to avoid collisions with legacy model_engine code.
