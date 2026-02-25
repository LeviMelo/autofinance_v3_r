# 08_forecast.R — 5-component panel forecast, gating, confidence, error buckets, reliability

# Component feature subsets (regex-based, architecture-aligned)
.came_comp_cols <- function(cols, comp) {
  comp <- as.integer(comp)
  if (comp == 1L) return(grep("^f_(mom_h|resid_mom_h|kal_|facproj|s_).|^f_l", cols, value=TRUE))
  if (comp == 2L) return(grep("^f_(peer_|shr_|clz_|deg|sgn_).", cols, value=TRUE))
  if (comp == 3L) return(grep("^f_(rel_|ten_|clz_|deg).", cols, value=TRUE))
  if (comp == 4L) return(grep("^f_(cf1|cf2|factor_exposure|idio|vol).|^f_facproj", cols, value=TRUE))
  if (comp == 5L) return(grep("^f_(ltv|lnt|illiq|avg_trade|implied_px|ltv_z|lnt_z)", cols, value=TRUE))
  cols
}

.came_weighted_ridge <- function(X, y, w, lambda) {
  # minimize sum w_i (y - Xb)^2 + lambda||b||^2 ; returns beta, intercept, scale params
  X <- as.matrix(X); y <- as.numeric(y)
  X[!is.finite(X)] <- 0; y[!is.finite(y)] <- 0
  w <- as.numeric(w); w[!is.finite(w) | w < 0] <- 0
  if (sum(w) <= 0) w <- rep(1, length(y))

  # standardize
  mu <- colMeans(X)
  sdv <- apply(X, 2, sd); sdv[sdv <= 1e-8 | !is.finite(sdv)] <- 1
  Xs <- scale(X, center = mu, scale = sdv); Xs[!is.finite(Xs)] <- 0

  y0 <- weighted.mean(y, w)
  yc <- y - y0

  # apply weights
  sw <- sqrt(w)
  Xw <- Xs * sw
  yw <- yc * sw

  p <- ncol(Xw)
  XtX <- crossprod(Xw) + lambda * diag(p)
  Xty <- crossprod(Xw, yw)
  b_std <- tryCatch(solve(XtX, Xty), error = function(e) rep(0, p))
  b <- as.numeric(b_std) / sdv
  names(b) <- colnames(X)
  intercept <- y0 - sum(b * mu)
  list(beta=b, intercept=intercept, mu=mu, sd=sdv)
}

.came_predict <- function(X, model) {
  if (is.null(model) || is.null(model$beta)) return(rep(0, nrow(X)))
  cols <- intersect(names(model$beta), colnames(X))
  if (length(cols) == 0) return(rep(0, nrow(X)))
  pred <- as.vector(as.matrix(X[, cols, drop=FALSE]) %*% model$beta[cols]) + model$intercept
  pred[!is.finite(pred)] <- 0
  pred
}

came_build_training_panel <- function(history_snapshots, R_history, H) {
  # history_snapshots: list(date, X matrix)
  # R_history: matrix of daily returns with rownames=Date strings and colnames=symbols
  came_assert(is.matrix(R_history) && !is.null(rownames(R_history)),
              "forecast_R_history", "R_history must be matrix with rownames as dates")
  if (is.null(history_snapshots) || length(history_snapshots) < (H + 5L)) {
    return(list(X=NULL, y=NULL, w=NULL, asset=NULL, date=NULL, n=0L))
  }

  rows_X <- list(); rows_y <- c(); rows_w <- c(); rows_asset <- c(); rows_date <- c()
  dates_R <- as.Date(rownames(R_history))

  for (snap in history_snapshots) {
    d <- as.Date(snap$date)
    ix <- match(d, dates_R)
    if (!is.finite(ix) || is.na(ix)) next
    if (ix + H > nrow(R_history)) next  # label not matured yet
    X <- as.matrix(snap$X)
    if (is.null(rownames(X)) || nrow(X) == 0) next
    syms <- rownames(X)

    # label y_{i,d}^(H) = sum_{h=1..H} r_{i, ix+h}
    for (sym in syms) {
      if (!(sym %in% colnames(R_history))) next
      y <- sum(R_history[(ix+1):(ix+H), sym], na.rm = TRUE)
      if (!is.finite(y)) next
      rows_X[[length(rows_X)+1L]] <- X[sym, , drop=TRUE]
      rows_y <- c(rows_y, y)
      rows_asset <- c(rows_asset, sym)
      rows_date <- c(rows_date, as.character(d))
      # EW weight by recency: w = ew_lambda^(age)
      age <- (nrow(R_history) - (ix + H))
      rows_w <- c(rows_w, age)
    }
  }

  if (length(rows_y) < 50) return(list(X=NULL, y=NULL, w=NULL, asset=NULL, date=NULL, n=0L))
  X_panel <- do.call(rbind, rows_X)
  colnames(X_panel) <- colnames(as.matrix(history_snapshots[[length(history_snapshots)]]$X))
  X_panel[!is.finite(X_panel)] <- 0
  list(X = X_panel, y = rows_y, age = rows_w, asset = rows_asset, date = rows_date, n=length(rows_y))
}

.came_kappa <- function(liq_z, illiq_z, node_stab, eto, chi, kmin, kmax) {
  # deterministic, bounded around 1
  z <- 0.0 +
    0.6 * (liq_z %||% 0) +
    (-0.7) * (illiq_z %||% 0) +
    0.5 * (node_stab %||% 1) +
    (-0.5) * (eto %||% 0) +
    (-0.3) * (chi %||% 0)
  raw <- 1 / (1 + exp(-z))
  kmin + (kmax - kmin) * raw
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

came_forecast_update <- function(X_now, m_t, struct_diag, node_stab, history_snapshots, R_history,
                                 state, spec) {
  C <- as.integer(spec$forecast$n_components %||% 5L)
  H <- as.integer(spec$forecast$H %||% 21L)
  ridge_lambda <- spec$forecast$ridge_lambda %||% 0.10
  ew_lambda <- spec$forecast$ew_lambda %||% 0.99
  refit_every <- as.integer(spec$forecast$refit_every %||% 5L)

  syms <- rownames(X_now)
  came_assert(length(syms) > 0, "forecast_X_now", "X_now must have rownames symbols")

  st <- state
  st$forecast$step <- as.integer((st$forecast$step %||% 0L) + 1L)

  # training panel
  panel <- came_build_training_panel(history_snapshots, R_history, H)

  # gating weights π_t
  A_pi <- spec$gating$A_pi
  b_pi <- spec$gating$b_pi
  came_assert(is.matrix(A_pi) && nrow(A_pi) == C, "forecast_A_pi", "A_pi dims mismatch")
  mtv <- as.numeric(m_t)
  if (length(mtv) < ncol(A_pi)) mtv <- c(mtv, rep(0, ncol(A_pi) - length(mtv)))
  if (length(mtv) > ncol(A_pi)) mtv <- mtv[seq_len(ncol(A_pi))]
  pi <- came_softmax(A_pi %*% mtv + b_pi, temperature = spec$gating$temperature %||% 1.0)
  names(pi) <- paste0("C", seq_len(C))

  # Fit / reuse models
  models <- st$forecast$models
  should_refit <- is.null(models) || (st$forecast$step %% refit_every == 0L)
  if (should_refit) {
    came_assert(panel$n > 0, "forecast_no_panel", "Insufficient matured history to fit forecast models (cold start).")
    # convert age to EW weights
    w <- (ew_lambda ^ panel$age)
    models <- vector("list", C)
    names(models) <- paste0("C", seq_len(C))
    for (c in seq_len(C)) {
      cols <- .came_comp_cols(colnames(panel$X), c)
      cols <- intersect(cols, colnames(panel$X))
      came_assert(length(cols) > 0, "forecast_no_features", paste("No features for component", c))
      models[[c]] <- .came_weighted_ridge(panel$X[, cols, drop=FALSE], panel$y, w, ridge_lambda)
      models[[c]]$cols <- cols
    }
    st$forecast$models <- models
  } else {
    came_assert(!is.null(models) && length(models) == C, "forecast_models_missing", "Models missing but refit disabled")
  }

  # component means
  comp_mu <- matrix(0, length(syms), C, dimnames = list(syms, paste0("C", seq_len(C))))
  for (c in seq_len(C)) {
    cols <- models[[c]]$cols
    comp_mu[, c] <- .came_predict(X_now[, cols, drop=FALSE], models[[c]])
  }

  # per-asset bounded kappa and normalized q_{i,c}
  kmin <- spec$forecast$kappa_min %||% 0.7
  kmax <- spec$forecast$kappa_max %||% 1.3

  # covariates from X_now (liquidity z, illiq)
  liq_z <- if ("f_ltv_z" %in% colnames(X_now)) X_now[, "f_ltv_z"] else rep(0, length(syms))
  illiq_z <- if ("f_illiq" %in% colnames(X_now)) scale(X_now[, "f_illiq"])[,1] else rep(0, length(syms))
  liq_z[!is.finite(liq_z)] <- 0; illiq_z[!is.finite(illiq_z)] <- 0

  eto <- struct_diag$eto %||% 0
  chi <- struct_diag$chi %||% 0
  ns <- node_stab[syms]; ns[!is.finite(ns)] <- 1

  kappa <- matrix(1, length(syms), C, dimnames=list(syms, paste0("C",seq_len(C))))
  for (i in seq_along(syms)) {
    ki <- .came_kappa(liq_z[i], illiq_z[i], ns[i], eto, chi, kmin, kmax)
    # trimmed choice: κ is per-asset per-component but seeded equal; component-specific extension can be added later
    kappa[i, ] <- ki
  }
  q_raw <- sweep(kappa, 2, pi, "*")
  q <- q_raw / pmax(rowSums(q_raw), 1e-12)

  mu_hat <- rowSums(q * comp_mu)
  names(mu_hat) <- syms

  # forecast history ring buffer: store comp_mu for maturity update
  if (is.null(st$forecast$hist) || !is.list(st$forecast$hist)) st$forecast$hist <- list()
  st$forecast$hist[[length(st$forecast$hist) + 1L]] <- list(date = as.Date(tail(rownames(R_history), 1)), comp_mu = comp_mu)
  # retain last (H + 30)
  if (length(st$forecast$hist) > (H + 30L)) st$forecast$hist <- tail(st$forecast$hist, H + 30L)

  # update error buckets using matured date: take entry dated (today - H)
  lambda_err <- spec$forecast$lambda_err %||% 0.97
  if (is.null(st$forecast$error_buckets)) {
    st$forecast$error_buckets <- lapply(seq_len(C), function(.) rep(1e-4, H))
    names(st$forecast$error_buckets) <- paste0("C", seq_len(C))
  }
  # find matured hist entry (by position, aligned to H within retained buffer)
  # We use date alignment against R_history for correctness.
  today <- as.Date(tail(rownames(R_history), 1))
  matured_date <- today - H
  hist_dates <- vapply(st$forecast$hist, function(h) as.character(h$date), character(1))
  idx_m <- match(as.character(matured_date), hist_dates)

  if (is.finite(idx_m) && !is.na(idx_m)) {
    comp_mu_m <- st$forecast$hist[[idx_m]]$comp_mu
    # actual y^(H) for matured_date
    dates_R <- as.Date(rownames(R_history))
    ix <- match(matured_date, dates_R)
    if (is.finite(ix) && !is.na(ix) && ix + H <= nrow(R_history)) {
      y_m <- rowSums(R_history[(ix+1):(ix+H), syms, drop=FALSE], na.rm=TRUE)
      y_m[!is.finite(y_m)] <- 0
      b <- ((ix - 1) %% H) + 1L
      for (c in seq_len(C)) {
        e2 <- (y_m - comp_mu_m[, c])^2
        e2 <- median(e2, na.rm=TRUE)
        if (is.finite(e2)) {
          key <- paste0("C", c)
          st$forecast$error_buckets[[key]][b] <- lambda_err * st$forecast$error_buckets[[key]][b] + (1 - lambda_err) * e2
        }
      }
    }
  }

  # component uncertainty (diagonal Ω)
  sig_c <- sapply(st$forecast$error_buckets, function(v) sqrt(median(v, na.rm=TRUE)))
  sig_c[!is.finite(sig_c) | sig_c <= 0] <- 0.01
  sigma_hat <- setNames(rep(0, length(syms)), syms)
  for (i in seq_along(syms)) {
    sigma_hat[i] <- sqrt(sum((q[i, ]^2) * (sig_c^2)))
  }
  sigma_hat[!is.finite(sigma_hat) | sigma_hat <= 0] <- 0.01

  # reliability
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
      q = q,
      kappa = kappa,
      comp_mu = comp_mu,
      rho = rho,
      comp_sigma = sig_c
    ),
    state_out = st,
    diag = list(refit = should_refit, panel_n = panel$n)
  )
}
