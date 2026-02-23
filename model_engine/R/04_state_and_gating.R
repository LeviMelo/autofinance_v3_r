#' @title State and Gating
#' @description Compute market-state features and map them to expert trust and cash mass.

#' @export
me_state_dispersion <- function(r_t) {
    if (length(r_t) < 2) {
        return(0)
    }
    sd(r_t, na.rm = TRUE)
}

#' @export
me_state_mode_dominance <- function(R_state_window) {
    if (nrow(R_state_window) < 10) {
        return(0.5)
    }
    keep <- colSums(is.na(R_state_window)) == 0
    R_clean <- R_state_window[, keep, drop = FALSE]

    if (ncol(R_clean) < 2) {
        return(0.5)
    }

    pca <- tryCatch(prcomp(R_clean, center = TRUE, scale. = TRUE), error = function(e) NULL)
    if (is.null(pca)) {
        return(0.5)
    }

    vars <- pca$sdev^2
    vars[1] / sum(vars)
}

#' @export
me_state_vov <- function(R_hist, vol_lookback = 63L) {
    # True VoV: median market vol series standard deviation
    # R_hist must span (vov_lookback + vol_lookback - 1) days ideally

    n_days <- nrow(R_hist)
    if (n_days <= vol_lookback) {
        return(0)
    }

    # Rolling vol estimates per asset
    v_tau <- numeric(n_days - vol_lookback + 1)

    for (i in seq_along(v_tau)) {
        # window for t
        start_idx <- i
        end_idx <- i + vol_lookback - 1

        R_sub <- R_hist[start_idx:end_idx, , drop = FALSE]
        # Keep strictly valid columns inside this rolling slice
        keep <- colSums(is.na(R_sub)) == 0
        if (sum(keep) < 2) {
            v_tau[i] <- 0
            next
        }

        R_clean <- R_sub[, keep, drop = FALSE]
        # Volatility per asset in cross section
        vols <- apply(R_clean, 2, sd, na.rm = TRUE) * sqrt(252)
        # Market vol proxy on this day
        v_tau[i] <- median(vols, na.rm = TRUE)
    }

    # Return standard deviation of the median market vols over the state window
    sd(v_tau, na.rm = TRUE)
}

#' @export
me_build_market_state <- function(R_disp, R_eta, R_vov, spec_market_state, vol_lookback) {
    r_t <- tail(R_disp, 1)[1, ]
    res <- c(
        disp = me_state_dispersion(r_t),
        eta = me_state_mode_dominance(R_eta),
        VoV = me_state_vov(R_vov, vol_lookback)
    )
    # Ensure exact dimensions
    if (is.na(res["VoV"])) res["VoV"] <- 0
    res
}

#' @export
me_gating_softmax_linear <- function(m_t, spec_gating) {
    w0 <- spec_gating$w0 %||% c(kalman = 0, tsmom = 0, cash = -1)

    if (is.null(spec_gating$W)) {
        logits <- w0
    } else {
        # Safe alignment using exact parameter mapping established in validation
        logits <- w0 + as.vector(spec_gating$W %*% m_t[colnames(spec_gating$W)])
        names(logits) <- names(w0)
    }

    exp_logits <- exp(logits - max(logits))
    probs <- exp_logits / sum(exp_logits)

    list(
        w_kalman = probs["kalman"],
        w_tsmom = probs["tsmom"],
        w_cash = probs["cash"],
        gross_exposure = 1.0 - probs["cash"],
        logits = logits
    )
}

#' @export
me_run_state_and_gating <- function(R_disp, R_eta, R_vov, risk_artifact, spec_market_state, spec_gating, vol_lookback = 63L) {
    # Heuristic diagnostics for default-path risk in state features
    eta_keep_cols <- if (!is.null(R_eta) && nrow(R_eta) > 0 && ncol(R_eta) > 0) {
        sum(colSums(is.na(R_eta)) == 0)
    } else {
        0L
    }

    vov_n_days <- if (!is.null(R_vov)) nrow(R_vov) else 0L
    disp_n_assets <- if (!is.null(R_disp)) ncol(R_disp) else 0L

    eta_likely_defaulted <- (is.null(R_eta) || nrow(R_eta) < 10 || eta_keep_cols < 2)
    vov_likely_defaulted <- (is.null(R_vov) || vov_n_days <= vol_lookback)
    disp_likely_degenerate <- (is.null(R_disp) || nrow(R_disp) < 1 || disp_n_assets < 2)

    m_t <- me_build_market_state(R_disp, R_eta, R_vov, spec_market_state, vol_lookback)
    gating <- me_gating_softmax_linear(m_t, spec_gating)

    W <- spec_gating$W
    gating_is_static <- is.null(W) || (is.matrix(W) && all(W == 0))

    list(
        market_state = m_t,
        gating = gating,
        diag = list(
            disp_likely_degenerate = disp_likely_degenerate,
            eta_likely_defaulted = eta_likely_defaulted,
            vov_likely_defaulted = vov_likely_defaulted,
            gating_is_static = gating_is_static,
            # Explicit placeholder status
            risk_artifact_used = FALSE
        )
    )
}
