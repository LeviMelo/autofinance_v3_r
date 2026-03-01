#' @title Model Engine — State and Gating
#' @description Architecture §11.5, §12.4: Market state builder, 5-component softmax gating,
#' optimizer control maps. Preserves legacy 3-way gating as backward compat.

# ── Cross-sectional dispersion ────────────────────────────────────────────────

#' @export
me_state_dispersion <- function(R_window, spec_disp = list()) {
    lookback <- spec_disp$lookback %||% 63L
    Tn <- nrow(R_window)
    n <- ncol(R_window)
    if (Tn < 2 || n < 2) {
        return(0)
    }
    use <- max(1, Tn - lookback + 1):Tn
    cs_sd <- apply(R_window[use, , drop = FALSE], 1, sd, na.rm = TRUE)
    mean(cs_sd, na.rm = TRUE)
}

#' @export
me_state_eta <- function(R_window, spec_eta = list()) {
    lookback <- spec_eta$lookback %||% 126L
    Tn <- nrow(R_window)
    n <- ncol(R_window)
    if (Tn < 2 || n < 2) {
        return(0)
    }
    use <- max(1, Tn - lookback + 1):Tn
    R_sub <- R_window[use, , drop = FALSE]
    avg_ret <- colMeans(R_sub, na.rm = TRUE)
    pos_frac <- mean(avg_ret > 0, na.rm = TRUE)
    2 * pos_frac - 1
}

#' @export
me_state_vov <- function(R_window, spec_vov = list()) {
    lookback <- spec_vov$lookback %||% 63L
    vol_lookback <- spec_vov$vol_lookback %||% 21L
    Tn <- nrow(R_window)
    n <- ncol(R_window)
    if (Tn < 2 || n < 2) {
        return(0)
    }
    use <- max(1, Tn - lookback + 1):Tn
    R_sub <- R_window[use, , drop = FALSE]
    Tn_sub <- nrow(R_sub)
    if (Tn_sub < vol_lookback + 2) {
        return(0)
    }
    rolling_vol <- sapply(seq(vol_lookback + 1, Tn_sub), function(i) {
        mean(apply(R_sub[(i - vol_lookback):(i - 1), , drop = FALSE], 2, sd, na.rm = TRUE), na.rm = TRUE)
    })
    if (length(rolling_vol) < 2) {
        return(0)
    }
    sd(rolling_vol, na.rm = TRUE)
}

# ── Global state vector m_t (§11.5, extended) ────────────────────────────────

#' @export
me_build_market_state_vector <- function(R_window, spec_ms, graph_diag = NULL,
                                         liquidity_state = NULL) {
    disp <- me_state_dispersion(R_window, spec_ms$dispersion %||% list())
    eta <- me_state_eta(R_window, spec_ms$eta %||% list())
    vov <- me_state_vov(R_window, spec_ms$vov %||% list())

    m_t <- c(disp = disp, eta = eta, VoV = vov)

    # Graph diagnostics in m_t
    if (!is.null(graph_diag)) {
        m_t <- c(m_t,
            dens = graph_diag$density %||% 0,
            eto = graph_diag$edge_turnover %||% 0,
            chi = graph_diag$chi_t %||% 0
        )
    } else {
        m_t <- c(m_t, dens = 0, eto = 0, chi = 0)
    }

    # Global liquidity state
    if (!is.null(liquidity_state)) {
        m_t <- c(m_t,
            liq_avg = liquidity_state$avg %||% 0,
            liq_disp = liquidity_state$dispersion %||% 0,
            liq_trend = liquidity_state$trend %||% 0
        )
    } else {
        m_t <- c(m_t, liq_avg = 0, liq_disp = 0, liq_trend = 0)
    }

    m_t[!is.finite(m_t)] <- 0
    m_t
}

# ── Legacy 3-way softmax gating (backward compat) ────────────────────────────

#' @export
me_softmax_gating <- function(state_vec, spec_gating) {
    expert_names <- c("kalman", "tsmom", "cash")
    state_names <- c("disp", "eta", "VoV")

    W <- spec_gating$W
    if (is.null(W) || !is.matrix(W)) {
        W <- matrix(0,
            nrow = 3, ncol = 3,
            dimnames = list(expert_names, state_names)
        )
    }

    w0 <- spec_gating$w0
    if (is.null(w0)) {
        w0 <- c(kalman = 0, tsmom = 0, cash = -1)
    }
    # Ensure w0 is named
    if (is.null(names(w0))) names(w0) <- expert_names

    temperature <- spec_gating$temperature %||% 1.0

    sx <- state_vec[state_names]
    sx[!is.finite(sx)] <- 0

    logits <- as.vector(W %*% sx) + w0
    logits <- logits / temperature
    names(logits) <- expert_names
    logits <- logits - max(logits)
    e <- exp(logits)
    probs <- e / sum(e)
    probs[!is.finite(probs)] <- 1 / 3
    names(probs) <- expert_names

    list(
        w_kalman = as.numeric(probs["kalman"]),
        w_tsmom = as.numeric(probs["tsmom"]),
        w_cash = as.numeric(probs["cash"]),
        gross_exposure = as.numeric(1 - probs["cash"]),
        state_vec = state_vec,
        logits_raw = as.numeric(logits)
    )
}

# ── Architecture 5-component softmax gating (§12.4) ──────────────────────────

#' @export
me_softmax_gating_5c <- function(m_t, spec_gating) {
    A_pi <- spec_gating$A_pi
    b_pi <- spec_gating$b_pi
    n_comp <- spec_gating$n_components %||% 5L
    temperature <- spec_gating$temperature %||% 1.0

    if (is.null(A_pi) || is.null(b_pi)) {
        pi_t <- setNames(rep(1 / n_comp, n_comp), paste0("C", seq_len(n_comp)))
        return(list(pi_t = pi_t, method = "uniform_default"))
    }

    # Pad/trim m_t
    n_state <- ncol(A_pi)
    if (length(m_t) < n_state) {
        m_t <- c(m_t, rep(0, n_state - length(m_t)))
    } else if (length(m_t) > n_state) {
        m_t <- m_t[seq_len(n_state)]
    }

    pi_t <- me_forecast_softmax_weights(m_t, A_pi, b_pi, temperature)

    list(pi_t = pi_t, m_t = m_t, method = "architecture_5c_softmax")
}

# ── Optimizer control maps (§12.8) ───────────────────────────────────────────

#' @export
me_optimizer_controls <- function(m_t, spec_portfolio) {
    # γ_t = exp(θ_γ' m_t), τ_t = exp(θ_τ' m_t), ρ_gross = σ(θ_gross' m_t)
    # Initial: configurable static values (not learned)

    gamma_base <- spec_portfolio$gamma %||% 1.0
    tau_base <- spec_portfolio$turnover_penalty %||% 0.0
    gross_base <- 1 - (spec_portfolio$cash_target %||% 0.15)

    # Simple state-adaptive scaling
    vov <- m_t["VoV"] %||% 0
    disp <- m_t["disp"] %||% 0

    gamma_t <- gamma_base * exp(0.5 * vov) # higher VoV → more risk aversion
    tau_t <- tau_base * (1 + 0.2 * disp) # higher dispersion → more turnover penalty
    rho_gross <- gross_base / (1 + exp(-2 * m_t["eta"])) # bullish → more exposure

    gamma_t <- max(0.1, min(10, gamma_t))
    tau_t <- max(0, min(0.1, tau_t))
    rho_gross <- max(0.3, min(0.95, rho_gross))

    if (!is.finite(gamma_t)) gamma_t <- gamma_base
    if (!is.finite(tau_t)) tau_t <- tau_base
    if (!is.finite(rho_gross)) rho_gross <- gross_base

    list(
        gamma_t = gamma_t,
        tau_t = tau_t,
        rho_gross = rho_gross
    )
}
