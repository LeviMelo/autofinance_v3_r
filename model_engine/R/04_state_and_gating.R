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
me_state_vov <- function(R_hist) {
    if (nrow(R_hist) < 20) {
        return(0)
    }
    keep <- colSums(is.na(R_hist)) == 0
    R_clean <- R_hist[, keep, drop = FALSE]
    if (ncol(R_clean) == 0) {
        return(0)
    }

    roll_vols <- apply(R_clean, 1, function(x) sd(x, na.rm = TRUE))
    v <- sd(roll_vols, na.rm = TRUE) / (mean(roll_vols, na.rm = TRUE) + 1e-6)
    if (is.na(v)) {
        return(0)
    }
    v
}

#' @export
me_build_market_state <- function(R_disp, R_eta, R_vov) {
    r_t <- tail(R_disp, 1)[1, ]
    c(
        disp = me_state_dispersion(r_t),
        eta = me_state_mode_dominance(R_eta),
        VoV = me_state_vov(R_vov)
    )
}

#' @export
me_gating_softmax_linear <- function(m_t, spec_gating) {
    w0 <- spec_gating$w0 %||% c(kalman = 0, tsmom = 0, cash = -1)

    if (is.null(spec_gating$W)) {
        logits <- w0
    } else {
        logits <- w0 + as.vector(spec_gating$W %*% m_t)
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
me_run_state_and_gating <- function(R_disp, R_eta, R_vov, risk_artifact, spec_gating) {
    m_t <- me_build_market_state(R_disp, R_eta, R_vov)
    gating <- me_gating_softmax_linear(m_t, spec_gating)

    list(market_state = m_t, gating = gating)
}
