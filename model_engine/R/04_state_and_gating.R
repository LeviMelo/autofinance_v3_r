#' @title Model Engine — State and Gating
#' @description Market-state features and softmax gating for expert mixture.

# ── Cross-sectional dispersion ────────────────────────────────────────────────

#' @export
me_state_dispersion <- function(r_t) {
    if (length(r_t) < 2) {
        return(0)
    }
    r <- r_t[is.finite(r_t)]
    if (length(r) < 2) {
        return(0)
    }
    sd(r, na.rm = TRUE)
}

# ── Market-mode dominance (eta) ───────────────────────────────────────────────

#' @export
me_state_eta <- function(R_window) {
    if (is.null(R_window) || nrow(R_window) < 5 || ncol(R_window) < 2) {
        return(0)
    }

    R_clean <- R_window
    R_clean[!is.finite(R_clean)] <- 0

    cor_mat <- tryCatch(cor(R_clean, use = "pairwise.complete.obs"),
        error = function(e) NULL
    )
    if (is.null(cor_mat)) {
        return(0)
    }
    cor_mat[!is.finite(cor_mat)] <- 0
    diag(cor_mat) <- 1

    evals <- tryCatch(eigen(cor_mat, symmetric = TRUE, only.values = TRUE)$values,
        error = function(e) NULL
    )
    if (is.null(evals) || length(evals) == 0) {
        return(0)
    }

    p <- length(evals)
    lambda_1 <- max(evals)
    eta <- lambda_1 / p
    if (!is.finite(eta)) 0 else eta
}

# ── Vol-of-vol proxy ─────────────────────────────────────────────────────────

#' @export
me_state_vov <- function(R_window, vol_lookback = 21L) {
    if (is.null(R_window) || nrow(R_window) < (vol_lookback + 5) || ncol(R_window) < 2) {
        return(0)
    }

    Tn <- nrow(R_window)
    n_days <- Tn - vol_lookback + 1
    if (n_days < 3) {
        return(0)
    }

    # Rolling per-asset volatilities, then daily median
    med_vols <- rep(NA_real_, n_days)
    for (tau in seq_len(n_days)) {
        idx <- tau:(tau + vol_lookback - 1)
        block <- R_window[idx, , drop = FALSE]
        asset_vols <- apply(block, 2, sd, na.rm = TRUE)
        asset_vols <- asset_vols[is.finite(asset_vols) & asset_vols > 0]
        if (length(asset_vols) > 0) {
            med_vols[tau] <- median(asset_vols)
        }
    }

    med_vols <- med_vols[is.finite(med_vols) & med_vols > 0]
    if (length(med_vols) < 3) {
        return(0)
    }

    # VoV = sd of log-changes in median vol
    log_vol <- log(med_vols)
    d_log_vol <- diff(log_vol)
    d_log_vol <- d_log_vol[is.finite(d_log_vol)]
    if (length(d_log_vol) < 2) {
        return(0)
    }

    vov <- sd(d_log_vol)
    if (!is.finite(vov)) 0 else vov
}

# ── Build market state vector ─────────────────────────────────────────────────

#' @export
me_build_market_state <- function(R_disp, R_eta, R_vov,
                                  spec_market_state, vol_lookback = 21L) {
    # Cross-sectional dispersion from most recent day's returns
    if (nrow(R_disp) > 0) {
        r_today <- R_disp[nrow(R_disp), ]
        disp <- me_state_dispersion(r_today)
    } else {
        disp <- 0
    }

    # Eta from correlation eigenvalues
    eta <- me_state_eta(R_eta)

    # Vol-of-vol
    vov <- me_state_vov(R_vov, vol_lookback)

    m_t <- c(disp = disp, eta = eta, VoV = vov)
    m_t
}

# ── Softmax gating ────────────────────────────────────────────────────────────

#' @export
me_softmax_gating <- function(m_t, spec_gating) {
    W <- spec_gating$W
    w0 <- spec_gating$w0
    tau <- spec_gating$temperature %||% 1.0

    if (is.null(W)) W <- matrix(0, 3, 3)
    if (is.null(w0)) w0 <- c(0, 0, -1)

    # Logits: W %*% m_t + w0
    logits <- as.vector(W %*% m_t) + w0

    # Softmax with temperature
    logits_scaled <- logits / tau
    logits_scaled <- logits_scaled - max(logits_scaled) # numerical stability
    exp_logits <- exp(logits_scaled)
    pi_t <- exp_logits / sum(exp_logits)

    names(pi_t) <- c("kalman", "tsmom", "cash")
    pi_t
}

# ── Full state and gating orchestrator ────────────────────────────────────────

#' @export
me_run_state_and_gating <- function(R_disp, R_eta, R_vov,
                                    risk_artifact,
                                    spec_market_state, spec_gating,
                                    vol_lookback = 21L) {
    m_t <- me_build_market_state(
        R_disp, R_eta, R_vov,
        spec_market_state, vol_lookback
    )

    pi_t <- me_softmax_gating(m_t, spec_gating)

    w_kalman <- pi_t["kalman"]
    w_tsmom <- pi_t["tsmom"]
    w_cash <- pi_t["cash"]
    gross_exposure <- 1 - w_cash

    list(
        market_state = m_t,
        gating = list(
            w_kalman        = unname(w_kalman),
            w_tsmom         = unname(w_tsmom),
            w_cash          = unname(w_cash),
            gross_exposure  = unname(gross_exposure),
            softmax_weights = pi_t,
            logits          = NULL # can store if debug needed
        ),
        diag = list(
            dispersion = m_t["disp"],
            eta        = m_t["eta"],
            vov        = m_t["VoV"]
        )
    )
}
