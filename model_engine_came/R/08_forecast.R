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

  rows_X <- vector("list", length(history_snapshots))
  rows_y <- vector("list", length(history_snapshots))
  rows_age <- vector("list", length(history_snapshots))
  rows_asset <- vector("list", length(history_snapshots))
  rows_date <- vector("list", length(history_snapshots))
  n_blocks <- 0L
  syms_R <- colnames(R_history)

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

    syms <- intersect(rownames(Xs), syms_R)
    if (length(syms) == 0L) next

    y_block <- colSums(R_history[(ix + 1):(ix + H), syms, drop = FALSE], na.rm = TRUE)
    keep <- is.finite(y_block)
    if (!any(keep)) next

    syms_keep <- syms[keep]
    y_keep <- as.numeric(y_block[keep])

    n_blocks <- n_blocks + 1L
    rows_X[[n_blocks]] <- Xs[syms_keep, , drop = FALSE]
    rows_y[[n_blocks]] <- y_keep
    rows_asset[[n_blocks]] <- syms_keep
    rows_date[[n_blocks]] <- rep(as.character(d), length(syms_keep))
    rows_age[[n_blocks]] <- rep(nrow(R_history) - (ix + H), length(syms_keep))
  }

  if (n_blocks < 1L) {
    return(list(X = NULL, y = NULL, age = NULL, asset = NULL, date = NULL, n = 0L))
  }

  rows_X <- rows_X[seq_len(n_blocks)]
  rows_y <- unlist(rows_y[seq_len(n_blocks)], use.names = FALSE)
  rows_age <- unlist(rows_age[seq_len(n_blocks)], use.names = FALSE)
  rows_asset <- unlist(rows_asset[seq_len(n_blocks)], use.names = FALSE)
  rows_date <- unlist(rows_date[seq_len(n_blocks)], use.names = FALSE)

  if (length(rows_y) < 100L) {
    return(list(X = NULL, y = NULL, age = NULL, asset = NULL, date = NULL, n = 0L))
  }

  X_panel <- do.call(rbind, rows_X)
  X_panel <- as.matrix(X_panel)
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

.came_rho_rel_vec <- function(liq_z, illiq_z, node_stab, eto, chi, a) {
  a0 <- as.numeric(a["intercept"])
  a1 <- as.numeric(a["liq_z"])
  a2 <- as.numeric(a["illiq_z"])
  a3 <- as.numeric(a["node_stab"])
  a4 <- as.numeric(a["eto"])
  a5 <- as.numeric(a["chi"])
  if (!is.finite(a0)) a0 <- 0
  if (!is.finite(a1)) a1 <- 0
  if (!is.finite(a2)) a2 <- 0
  if (!is.finite(a3)) a3 <- 0
  if (!is.finite(a4)) a4 <- 0
  if (!is.finite(a5)) a5 <- 0

  lz <- as.numeric(liq_z)
  iz <- as.numeric(illiq_z)
  ns <- as.numeric(node_stab)
  lz[!is.finite(lz)] <- 0
  iz[!is.finite(iz)] <- 0
  ns[!is.finite(ns)] <- 1
  eto <- if (is.finite(eto)) eto else 0
  chi <- if (is.finite(chi)) chi else 0

  z <- a0 + a1 * lz + a2 * iz + a3 * ns + a4 * eto + a5 * chi
  r <- 1 / (1 + exp(-z))
  r <- pmin(1, pmax(0.05, r))
  r[!is.finite(r)] <- 0.8
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
  mt_order <- c("disp", "eta", "VoV", "dens", "eto", "chi", "liq_med_logv", "liq_med_logntr", "liq_frac_active")
  mtv <- as.numeric(m_t[mt_order])
  mtv[is.na(mtv) | !is.finite(mtv)] <- 0
  if (length(mtv) < ncol(A_pi)) mtv <- c(mtv, rep(0, ncol(A_pi) - length(mtv)))
  if (length(mtv) > ncol(A_pi)) mtv <- mtv[seq_len(ncol(A_pi))]
  pi <- came_softmax(A_pi %*% mtv + b_pi, temperature = spec$gating$temperature %||% 1.0)
  names(pi) <- paste0("C", seq_len(C))

  models <- st$forecast$models
  light_update <- isTRUE(spec$compute$light_update %||% FALSE)
  should_refit <- is.null(models) || (st$forecast$step %% refit_every == 0L)
  # Light updates may skip expensive refits only after models already exist.
  # On first bootstrap, refit must remain enabled.
  if (light_update && !is.null(models)) should_refit <- FALSE
  panel <- if (should_refit) {
    came_build_training_panel(history_snapshots, R_history, H, required_cols = colnames(X_now))
  } else {
    list(X = NULL, y = NULL, age = NULL, asset = NULL, date = NULL, n = 0L)
  }

  # ---- cold start handling ----
  if (is.null(models) && should_refit && (panel$n %||% 0L) == 0L) {
    if (!allow_skip) {
      came_stop("forecast_no_panel", "Insufficient matured history to fit forecast models (cold start).")
    }

    # Neutral forecasts (mu=0), but we still compute pi/kappa/q and maintain error buckets.
    comp_mu <- matrix(0, length(syms), C, dimnames = list(syms, paste0("C", seq_len(C))))

    # Keep per-day history so later matured error updates have something to reference
    if (is.null(st$forecast$hist) || !is.list(st$forecast$hist)) st$forecast$hist <- list()
    today_date <- as.Date(tail(rownames(R_history), 1))
    st$forecast$hist[[length(st$forecast$hist) + 1L]] <- list(
      date = today_date,
      step = st$forecast$step,
      comp_mu = comp_mu
    )
    keep_hist <- as.integer(H + 60L)
    if (length(st$forecast$hist) > keep_hist) st$forecast$hist <- tail(st$forecast$hist, keep_hist)

    # buckets init
    if (is.null(st$forecast$error_buckets)) {
      st$forecast$error_buckets <- lapply(seq_len(C), function(.) rep(1e-4, H))
      names(st$forecast$error_buckets) <- paste0("C", seq_len(C))
    }

    comp_sigma <- sapply(st$forecast$error_buckets, function(v) sqrt(stats::median(v, na.rm = TRUE)))
    comp_sigma[!is.finite(comp_sigma) | comp_sigma <= 0] <- 0.01

    # kappa and q (still computed)
    kmin <- spec$forecast$kappa_min %||% 0.7
    kmax <- spec$forecast$kappa_max %||% 1.3
    kappa <- .came_kappa_matrix(syms, C, X_now, node_stab, struct_diag, kmin, kmax)

    q_raw <- sweep(kappa, 2, pi, "*")
    q <- q_raw / pmax(rowSums(q_raw), 1e-12)

    mu_hat <- setNames(rep(0, length(syms)), syms)

    c_eff <- min(ncol(q), length(comp_sigma))
    came_assert(c_eff >= 1L, "forecast_sigma_dims", "Invalid component dimensions for sigma_hat (cold start)")
    sigma_hat <- sqrt(as.numeric((q[, seq_len(c_eff), drop = FALSE]^2) %*% (comp_sigma[seq_len(c_eff)]^2)))
    names(sigma_hat) <- syms
    sigma_hat[!is.finite(sigma_hat) | sigma_hat <= 0] <- 0.01

    # reliability
    liq_z <- if ("f_ltv_z" %in% colnames(X_now)) X_now[, "f_ltv_z"] else rep(0, length(syms))
    illiq_z <- if ("f_illiq_z" %in% colnames(X_now)) X_now[, "f_illiq_z"] else rep(0, length(syms))
    liq_z[!is.finite(liq_z)] <- 0
    illiq_z[!is.finite(illiq_z)] <- 0

    eto0 <- struct_diag$eto %||% 0
    chi0 <- struct_diag$chi %||% 0
    ns <- node_stab[syms]
    ns[!is.finite(ns)] <- 1

    a <- spec$reliability$a
    rho <- .came_rho_rel_vec(liq_z, illiq_z, ns, eto0, chi0, a)
    names(rho) <- syms

    mu_eff <- rho * mu_hat
    s_eff <- sigma_hat / sqrt(pmax(rho, spec$reliability$eps %||% 1e-6))

    return(list(
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
      diag = list(refit = FALSE, panel_n = 0L, matured_ix = nrow(R_history) - H, cold_start = TRUE)
    ))
  }

  # ---- normal refit / reuse ----
  if (should_refit) {
    came_assert(panel$n > 0, "forecast_no_panel", "Insufficient matured history to fit forecast models (cold start).")
    w <- (ew_lambda^panel$age)

    fit_one <- function(c) {
      cols <- .came_comp_cols(colnames(panel$X), c)
      cols <- intersect(cols, colnames(panel$X))
      came_assert(length(cols) > 0, "forecast_no_features", paste("No features for component", c))
      .came_weighted_ridge(panel$X[, cols, drop = FALSE], panel$y, w, ridge_lambda)
    }

    use_parallel <- isTRUE(spec$compute$parallel_components %||% FALSE) &&
      (.Platform$OS.type != "windows") &&
      (C >= 2L)
    if (use_parallel) {
      ncores <- as.integer(spec$compute$parallel_cores %||% 2L)
      if (!is.finite(ncores) || ncores < 1L) ncores <- 2L
      models <- parallel::mclapply(seq_len(C), fit_one, mc.cores = ncores)
    } else {
      models <- lapply(seq_len(C), fit_one)
    }
    names(models) <- paste0("C", seq_len(C))
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

  c_mu <- min(ncol(q), ncol(comp_mu))
  came_assert(c_mu >= 1L, "forecast_mu_dims", "Invalid component dimensions for mu_hat")
  q_use <- q[, seq_len(c_mu), drop = FALSE]
  comp_mu_use <- comp_mu[, seq_len(c_mu), drop = FALSE]
  mu_hat <- rowSums(q_use * comp_mu_use)
  names(mu_hat) <- syms

  # --- store today's component means for future matured error updates
  if (is.null(st$forecast$hist) || !is.list(st$forecast$hist)) st$forecast$hist <- list()
  today_date <- as.Date(tail(rownames(R_history), 1))
  st$forecast$hist[[length(st$forecast$hist) + 1L]] <- list(date = today_date, step = st$forecast$step, comp_mu = comp_mu)

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

    # IMPORTANT: bucket index must advance over time; matured_ix is constant when R_history is a rolling fixed window.
    matured_step <- st$forecast$step - H
    if (matured_step >= 1L) {
      b <- ((matured_step - 1L) %% H) + 1L

      # Prefer matching by step (more robust), fallback to date
      hist_steps <- vapply(st$forecast$hist, function(h) h$step %||% NA_integer_, integer(1))
      idx_m <- match(matured_step, hist_steps)

      if (is.na(idx_m)) {
        hist_dates <- vapply(st$forecast$hist, function(h) as.character(h$date), character(1))
        idx_m <- match(as.character(matured_date), hist_dates)
      }

      if (is.finite(idx_m) && !is.na(idx_m)) {
        comp_mu_m <- st$forecast$hist[[idx_m]]$comp_mu

        common_assets <- intersect(rownames(comp_mu_m), colnames(R_history))
        if (length(common_assets) >= 3) {
          y_m <- rowSums(R_history[(matured_ix + 1):(matured_ix + H), common_assets, drop = FALSE], na.rm = TRUE)
          y_m[!is.finite(y_m)] <- 0

          pred_mat <- as.matrix(comp_mu_m[common_assets, , drop = FALSE])
          c_err <- min(ncol(pred_mat), C)
          if (c_err >= 1L) {
            pred_mat <- pred_mat[, seq_len(c_err), drop = FALSE]
            y_aligned <- y_m[rownames(pred_mat)]
            y_aligned[!is.finite(y_aligned)] <- 0
            e2_mat <- sweep(pred_mat, 1, y_aligned, FUN = "-")^2
            pool_e2 <- apply(e2_mat, 2, stats::median, na.rm = TRUE)
          } else {
            pool_e2 <- numeric(0)
          }

          for (c in seq_len(c_err)) {
            pe <- pool_e2[c]
            if (is.finite(pe)) {
              key <- paste0("C", c)
              st$forecast$error_buckets[[key]][b] <- lambda_err * st$forecast$error_buckets[[key]][b] + (1 - lambda_err) * pe
            }
          }
        }
      }
    }
  }

  # Omega diagonal from buckets (architecture §12.7)
  comp_sigma <- sapply(st$forecast$error_buckets, function(v) sqrt(stats::median(v, na.rm = TRUE)))
  comp_sigma[!is.finite(comp_sigma) | comp_sigma <= 0] <- 0.01

  # combined uncertainty s^(H) (architecture §12.7)
  c_eff <- min(ncol(q), length(comp_sigma))
  came_assert(c_eff >= 1L, "forecast_sigma_dims", "Invalid component dimensions for sigma_hat")
  sigma_hat <- sqrt(as.numeric((q[, seq_len(c_eff), drop = FALSE]^2) %*% (comp_sigma[seq_len(c_eff)]^2)))
  names(sigma_hat) <- syms
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
  rho <- .came_rho_rel_vec(liq_z, illiq_z, ns, eto, chi, a)
  names(rho) <- syms

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
