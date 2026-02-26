# 08_forecast.R — Forecast engine (architecture §12)
# 5-component mixture with:
#   - rolling EW-weighted ridge per component (matured labels only)
#   - global softmax gating pi_t from m_t
#   - bounded confidence multipliers kappa_{i,c}
#   - overlap-aware stagger-bucket component error states (H buckets)
#   - diagonal Omega (component error covariance) allowed by architecture
#   - reliability score rho_rel and effective moments (mu_eff, s_eff)

# --- component feature subsets (explicit by patterns) ----------------------

.came_comp_cols <- function(cols, comp) {
  comp <- as.integer(comp)

  # Component 1: temporal continuation
  if (comp == 1L) {
    return(unique(c(
      grep("^f_mom_h", cols, value = TRUE),
      grep("^f_resid_mom_h", cols, value = TRUE),
      grep("^f_kal_", cols, value = TRUE),
      "f_facproj",
      grep("^f_l", cols, value = TRUE), # liquidity logs/z/illiq
      grep("^f_liq_x_", cols, value = TRUE) # liquidity interactions
    )))
  }

  # Component 2: structural continuation / peer confirmation
  if (comp == 2L) {
    return(unique(c(
      grep("^f_peer_", cols, value = TRUE),
      grep("^f_shr_", cols, value = TRUE),
      grep("^f_sgn_", cols, value = TRUE),
      grep("^f_clz_", cols, value = TRUE),
      grep("^f_clctr_", cols, value = TRUE),
      "f_deg", "f_node_stab"
    )))
  }

  # Component 3: neighborhood mean-reversion risk (required)
  if (comp == 3L) {
    return(unique(c(
      grep("^f_rel_", cols, value = TRUE),
      grep("^f_ten_", cols, value = TRUE),
      "f_deg", "f_node_stab",
      grep("^f_mismatch_", cols, value = TRUE) # local mismatch often drives reversion
    )))
  }

  # Component 4: PCA–graph structural dislocation
  if (comp == 4L) {
    return(unique(c(
      "f_cf1", "f_cf2",
      "f_facproj",
      "f_deg", "f_node_stab",
      grep("^f_state_", cols, value = TRUE)
    )))
  }

  # Component 5: liquidity/friction-conditioned correction
  if (comp == 5L) {
    return(unique(c(
      grep("^f_l", cols, value = TRUE),
      grep("^f_liq_x_", cols, value = TRUE),
      grep("^f_peer_l", cols, value = TRUE),
      grep("^f_mismatch_", cols, value = TRUE),
      "f_active"
    )))
  }

  cols
}

# --- weighted ridge --------------------------------------------------------

.came_weighted_ridge <- function(X, y, w, lambda) {
  X <- as.matrix(X)
  y <- as.numeric(y)
  w <- as.numeric(w)

  X[!is.finite(X)] <- 0
  y[!is.finite(y)] <- 0
  w[!is.finite(w) | w < 0] <- 0
  if (sum(w) <= 0) w <- rep(1, length(y))

  # standardize X columns
  mu <- colMeans(X)
  sdv <- apply(X, 2, sd)
  sdv[!is.finite(sdv) | sdv <= 1e-8] <- 1
  Xs <- scale(X, center = mu, scale = sdv)
  Xs[!is.finite(Xs)] <- 0

  y0 <- weighted.mean(y, w)
  yc <- y - y0

  sw <- sqrt(w)
  Xw <- Xs * sw
  yw <- yc * sw

  p <- ncol(Xw)
  XtX <- crossprod(Xw) + lambda * diag(p)
  Xty <- crossprod(Xw, yw)

  b_std <- tryCatch(solve(XtX, Xty), error = function(e) NULL)
  if (is.null(b_std)) stop(came_error("forecast_ridge_solve_failed", "Ridge solve failed (ill-conditioned)"))

  b <- as.numeric(b_std) / sdv
  names(b) <- colnames(X)
  intercept <- y0 - sum(b * mu)

  list(beta = b, intercept = intercept, cols = colnames(X))
}

.came_predict <- function(X, model) {
  came_assert(!is.null(model$beta), "forecast_model_beta", "Model missing beta")
  cols <- intersect(names(model$beta), colnames(X))
  came_assert(length(cols) > 0, "forecast_predict_cols", "No overlapping columns between model and X")
  pred <- as.vector(as.matrix(X[, cols, drop = FALSE]) %*% model$beta[cols]) + model$intercept
  pred[!is.finite(pred)] <- 0
  pred
}

# --- training panel builder (strict on feature schema) ---------------------

came_build_training_panel <- function(history_snapshots, R_history, H, required_cols = NULL) {
  came_assert(
    is.matrix(R_history) && !is.null(rownames(R_history)),
    "forecast_R_history", "R_history must be matrix with rownames (Date strings)"
  )

  if (is.null(history_snapshots) || length(history_snapshots) < (H + 5L)) {
    return(list(X = NULL, y = NULL, age = NULL, asset = NULL, date = NULL, n = 0L))
  }

  dates_R <- as.Date(rownames(R_history))
  came_assert(!any(is.na(dates_R)), "forecast_R_dates", "R_history rownames must be Date-coercible")

  # Determine schema from required_cols (preferred) or from most recent snapshot
  if (is.null(required_cols)) {
    required_cols <- colnames(as.matrix(history_snapshots[[length(history_snapshots)]]$X))
  }
  came_assert(!is.null(required_cols) && length(required_cols) > 0, "forecast_required_cols", "No feature schema provided")

  rows_X <- list()
  rows_y <- c()
  rows_age <- c()
  rows_asset <- c()
  rows_date <- c()

  for (snap in history_snapshots) {
    d <- as.Date(snap$date)
    ix <- match(d, dates_R)
    if (is.na(ix)) next
    if (ix + H > nrow(R_history)) next # label not matured

    Xs <- as.matrix(snap$X)
    if (is.null(rownames(Xs)) || nrow(Xs) == 0) next

    # strict schema enforcement
    came_assert(
      identical(colnames(Xs), required_cols), "forecast_schema_drift",
      "Feature schema drift detected across history snapshots (colnames not identical). Rebuild history."
    )

    syms <- rownames(Xs)
    # y_{i,d}^{(H)} = sum_{h=1..H} r_{i, ix+h}
    for (sym in syms) {
      if (!(sym %in% colnames(R_history))) next
      y <- sum(R_history[(ix + 1):(ix + H), sym], na.rm = TRUE)
      if (!is.finite(y)) next

      rows_X[[length(rows_X) + 1L]] <- Xs[sym, , drop = TRUE]
      rows_y <- c(rows_y, y)
      rows_asset <- c(rows_asset, sym)
      rows_date <- c(rows_date, as.character(d))

      age <- (nrow(R_history) - (ix + H))
      rows_age <- c(rows_age, age)
    }
  }

  if (length(rows_y) < 100) {
    return(list(X = NULL, y = NULL, age = NULL, asset = NULL, date = NULL, n = 0L))
  }

  X_panel <- do.call(rbind, rows_X)
  colnames(X_panel) <- required_cols
  X_panel[!is.finite(X_panel)] <- 0

  list(X = X_panel, y = rows_y, age = rows_age, asset = rows_asset, date = rows_date, n = length(rows_y))
}

# --- kappa and reliability --------------------------------------------------

.came_kappa_matrix <- function(syms, C, X_now, node_stab, struct_diag, kmin, kmax) {
  # bounded multipliers around 1; component-specific tilts
  liq_z <- if ("f_ltv_z" %in% colnames(X_now)) X_now[, "f_ltv_z"] else rep(0, length(syms))
  illiq_z <- if ("f_illiq_z" %in% colnames(X_now)) X_now[, "f_illiq_z"] else rep(0, length(syms))
  liq_z[!is.finite(liq_z)] <- 0
  illiq_z[!is.finite(illiq_z)] <- 0

  eto <- struct_diag$eto %||% 0
  chi <- struct_diag$chi %||% 0

  ns <- node_stab[syms]
  ns[!is.finite(ns)] <- 1

  # component-specific scores (deterministic, non-placeholder)
  # Intuition:
  #  c1 continuation: likes liquidity, dislikes illiq, dislikes shock
  #  c2 structural confirmation: likes stability and density/low eto
  #  c3 mean reversion: rises with dislocation + instability (we proxy via eto/chi)
  #  c4 pca-graph: likes stable clusters and moderate liquidity (proxy via ns, liq)
  #  c5 friction correction: higher when illiq is high (so the correction component is "active")
  z1 <- 0.6 * liq_z - 0.6 * illiq_z + 0.3 * ns - 0.3 * chi
  z2 <- 0.3 * liq_z - 0.2 * illiq_z + 0.8 * ns - 0.6 * eto - 0.3 * chi
  z3 <- -0.1 * liq_z + 0.2 * illiq_z + 0.2 * eto + 0.4 * chi
  z4 <- 0.2 * liq_z - 0.2 * illiq_z + 0.5 * ns - 0.2 * chi
  z5 <- -0.2 * liq_z + 0.8 * illiq_z - 0.2 * ns + 0.2 * chi

  Z <- cbind(z1, z2, z3, z4, z5)
  if (C != 5L) {
    # If C differs, recycle first column pattern safely (strictly deterministic)
    Z <- Z[, rep(1, C), drop = FALSE]
  }

  raw <- 1 / (1 + exp(-Z))
  kappa <- kmin + (kmax - kmin) * raw
  dimnames(kappa) <- list(syms, paste0("C", seq_len(C)))
  kappa
}

.came_rho_rel <- function(liq_z, illiq_z, node_stab, eto, chi, a) {
  z <- a["intercept"] +
    a["liq_z"] * (liq_z %||% 0) +
    a["illiq_z"] * (illiq_z %||% 0) +
    a["node_stab"] * (node_stab %||% 1) +
    a["eto"] * (eto %||% 0) +
    a["chi"] * (chi %||% 0)
  r <- 1 / (1 + exp(-z))
  r <- pmin(1, pmax(0.05, r))
  r
}

# --- main forecast update ---------------------------------------------------

came_forecast_update <- function(X_now, m_t, struct_diag, node_stab, history_snapshots, R_history, state, spec) {
  C <- as.integer(spec$forecast$n_components %||% 5L)
  H <- as.integer(spec$forecast$H %||% 21L)
  came_assert(C >= 1, "forecast_C", "n_components must be >= 1")
  came_assert(H >= 1, "forecast_H", "H must be >= 1")

  ridge_lambda <- spec$forecast$ridge_lambda %||% 0.10
  ew_lambda <- spec$forecast$ew_lambda %||% 0.99
  refit_every <- as.integer(spec$forecast$refit_every %||% 5L)
  lambda_err <- spec$forecast$lambda_err %||% 0.97

  syms <- rownames(X_now)
  came_assert(length(syms) >= 3, "forecast_syms", "X_now must have >=3 rows and rownames as symbols")

  st <- state
  st$forecast$step <- as.integer((st$forecast$step %||% 0L) + 1L)

  cold_policy <- spec$forecast$cold_start_policy %||% "error"
  allow_skip <- (!isTRUE(spec$meta$strict %||% TRUE)) || identical(cold_policy, "skip")

  # gating pi_t (architecture §12.4)
  A_pi <- spec$gating$A_pi
  b_pi <- spec$gating$b_pi
  came_assert(is.matrix(A_pi) && nrow(A_pi) == C, "forecast_A_pi", "A_pi dims mismatch")
  mtv <- as.numeric(m_t)
  if (length(mtv) < ncol(A_pi)) mtv <- c(mtv, rep(0, ncol(A_pi) - length(mtv)))
  if (length(mtv) > ncol(A_pi)) mtv <- mtv[seq_len(ncol(A_pi))]
  pi <- came_softmax(A_pi %*% mtv + b_pi, temperature = spec$gating$temperature %||% 1.0)
  names(pi) <- paste0("C", seq_len(C))

  # training panel (matured labels only)
  panel <- came_build_training_panel(history_snapshots, R_history, H, required_cols = colnames(X_now))

  models <- st$forecast$models
  should_refit <- is.null(models) || (st$forecast$step %% refit_every == 0L)

  if (should_refit) {
    came_assert(panel$n > 0, "forecast_no_panel", "Insufficient matured history to fit forecast models (cold start).")
    w <- (ew_lambda^panel$age)

    models <- vector("list", C)
    names(models) <- paste0("C", seq_len(C))

    for (c in seq_len(C)) {
      cols <- .came_comp_cols(colnames(panel$X), c)
      cols <- intersect(cols, colnames(panel$X))
      came_assert(length(cols) > 0, "forecast_no_features", paste("No features for component", c))
      models[[c]] <- .came_weighted_ridge(panel$X[, cols, drop = FALSE], panel$y, w, ridge_lambda)
    }

    st$forecast$models <- models
  } else {
    came_assert(!is.null(models) && length(models) == C, "forecast_models_missing", "Models missing but refit disabled")
  }

  # component means mu^{(c)}_{i,t}
  comp_mu <- matrix(0, length(syms), C, dimnames = list(syms, paste0("C", seq_len(C))))
  for (c in seq_len(C)) {
    cols <- models[[c]]$cols
    comp_mu[, c] <- .came_predict(X_now[, cols, drop = FALSE], models[[c]])
  }

  # kappa_{i,c,t} and q_{i,c,t} (architecture §12.5)
  kmin <- spec$forecast$kappa_min %||% 0.7
  kmax <- spec$forecast$kappa_max %||% 1.3
  kappa <- .came_kappa_matrix(syms, C, X_now, node_stab, struct_diag, kmin, kmax)
  q_raw <- sweep(kappa, 2, pi, "*")
  q <- q_raw / pmax(rowSums(q_raw), 1e-12)

  mu_hat <- rowSums(q * comp_mu)
  names(mu_hat) <- syms

  # --- store today's component means for future matured error updates
  if (is.null(st$forecast$hist) || !is.list(st$forecast$hist)) st$forecast$hist <- list()
  today_date <- as.Date(tail(rownames(R_history), 1))
  st$forecast$hist[[length(st$forecast$hist) + 1L]] <- list(date = today_date, comp_mu = comp_mu)

  # retain limited buffer
  keep_hist <- as.integer(H + 60L)
  if (length(st$forecast$hist) > keep_hist) st$forecast$hist <- tail(st$forecast$hist, keep_hist)

  # --- stagger-bucket error states (architecture §12.6)
  if (is.null(st$forecast$error_buckets)) {
    st$forecast$error_buckets <- lapply(seq_len(C), function(.) rep(1e-4, H))
    names(st$forecast$error_buckets) <- paste0("C", seq_len(C))
  }

  # Determine matured trading-day index (NOT calendar subtraction)
  dates_R <- as.Date(rownames(R_history))
  came_assert(!any(is.na(dates_R)), "forecast_R_dates2", "R_history rownames must be Date-coercible")

  matured_ix <- nrow(R_history) - H
  if (matured_ix >= 1 && matured_ix + H <= nrow(R_history)) {
    matured_date <- dates_R[matured_ix]
    b <- ((matured_ix - 1) %% H) + 1L

    hist_dates <- vapply(st$forecast$hist, function(h) as.character(h$date), character(1))
    idx_m <- match(as.character(matured_date), hist_dates)

    if (is.finite(idx_m) && !is.na(idx_m)) {
      comp_mu_m <- st$forecast$hist[[idx_m]]$comp_mu
      # realized y^(H) at matured_ix
      common_assets <- intersect(rownames(comp_mu_m), colnames(R_history))
      if (length(common_assets) >= 3) {
        y_m <- rowSums(R_history[(matured_ix + 1):(matured_ix + H), common_assets, drop = FALSE], na.rm = TRUE)
        y_m[!is.finite(y_m)] <- 0

        for (c in seq_len(C)) {
          pred <- comp_mu_m[common_assets, c]
          e2 <- (y_m - pred)^2
          pool_e2 <- stats::median(e2, na.rm = TRUE)
          if (is.finite(pool_e2)) {
            key <- paste0("C", c)
            st$forecast$error_buckets[[key]][b] <- lambda_err * st$forecast$error_buckets[[key]][b] + (1 - lambda_err) * pool_e2
          }
        }
      }
    }
  }

  # Omega diagonal from buckets (architecture §12.7)
  comp_sigma <- sapply(st$forecast$error_buckets, function(v) sqrt(stats::median(v, na.rm = TRUE)))
  comp_sigma[!is.finite(comp_sigma) | comp_sigma <= 0] <- 0.01

  # combined uncertainty s^(H) (architecture §12.7)
  sigma_hat <- setNames(rep(0, length(syms)), syms)
  for (i in seq_along(syms)) {
    sigma_hat[i] <- sqrt(sum((q[i, ]^2) * (comp_sigma^2)))
  }
  sigma_hat[!is.finite(sigma_hat) | sigma_hat <= 0] <- 0.01

  # reliability (architecture §12.8)
  liq_z <- if ("f_ltv_z" %in% colnames(X_now)) X_now[, "f_ltv_z"] else rep(0, length(syms))
  illiq_z <- if ("f_illiq_z" %in% colnames(X_now)) X_now[, "f_illiq_z"] else rep(0, length(syms))
  liq_z[!is.finite(liq_z)] <- 0
  illiq_z[!is.finite(illiq_z)] <- 0

  eto <- struct_diag$eto %||% 0
  chi <- struct_diag$chi %||% 0
  ns <- node_stab[syms]
  ns[!is.finite(ns)] <- 1

  a <- spec$reliability$a
  rho <- setNames(rep(0.8, length(syms)), syms)
  for (i in seq_along(syms)) {
    rho[i] <- .came_rho_rel(liq_z[i], illiq_z[i], ns[i], eto, chi, a)
  }

  mu_eff <- rho * mu_hat
  s_eff <- sigma_hat / sqrt(pmax(rho, spec$reliability$eps %||% 1e-6))

  list(
    forecast = list(
      mu_hat = mu_hat,
      mu_eff = mu_eff,
      sigma_hat = sigma_hat,
      s_eff = s_eff,
      pi = pi,
      kappa = kappa,
      q = q,
      comp_mu = comp_mu,
      comp_sigma = comp_sigma,
      rho = rho
    ),
    state_out = st,
    diag = list(refit = should_refit, panel_n = panel$n, matured_ix = matured_ix)
  )
}
