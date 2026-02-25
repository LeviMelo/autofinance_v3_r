#' @title Backtest Engine — Execution and Costs
#' @description Convert target weights into trades, simulate fills, apply costs.

# ── Compute target shares ────────────────────────────────────────────────────

#' @export
bt_compute_target_shares <- function(target_weights, nav, exec_prices) {
    if (nrow(target_weights) == 0) {
        return(setNames(numeric(0), character(0)))
    }

    syms <- target_weights$symbol
    w <- target_weights$weight_target
    names(w) <- syms

    shares <- rep(0, length(syms))
    names(shares) <- syms

    for (s in syms) {
        p <- exec_prices[s]
        if (is.na(p) || !is.finite(p) || p <= 0) next
        notional <- w[s] * nav
        shares[s] <- floor(notional / p) # whole shares
    }
    shares
}

# ── Generate orders ──────────────────────────────────────────────────────────

#' @export
bt_generate_orders <- function(current_positions, target_shares) {
    all_syms <- unique(c(names(current_positions), names(target_shares)))
    if (length(all_syms) == 0) {
        return(data.frame(
            symbol = character(0), shares_delta = numeric(0),
            direction = character(0), stringsAsFactors = FALSE
        ))
    }

    cur <- rep(0, length(all_syms))
    names(cur) <- all_syms
    tgt <- rep(0, length(all_syms))
    names(tgt) <- all_syms

    found_cur <- intersect(names(current_positions), all_syms)
    cur[found_cur] <- current_positions[found_cur]
    found_tgt <- intersect(names(target_shares), all_syms)
    tgt[found_tgt] <- target_shares[found_tgt]

    delta <- tgt - cur
    active <- delta != 0

    if (!any(active)) {
        return(data.frame(
            symbol = character(0), shares_delta = numeric(0),
            direction = character(0), stringsAsFactors = FALSE
        ))
    }

    data.frame(
        symbol = all_syms[active],
        shares_delta = unname(delta[active]),
        direction = ifelse(delta[active] > 0, "buy", "sell"),
        stringsAsFactors = FALSE
    )
}

# ── Simulate fills ────────────────────────────────────────────────────────────

#' @export
bt_simulate_fills <- function(orders, exec_prices) {
    if (nrow(orders) == 0) {
        return(data.frame(
            symbol = character(0), shares_filled = numeric(0),
            fill_price = numeric(0), notional = numeric(0),
            stringsAsFactors = FALSE
        ))
    }

    fills <- orders
    fills$fill_price <- exec_prices[fills$symbol]
    fills$shares_filled <- fills$shares_delta # full fill
    fills$notional <- fills$shares_filled * fills$fill_price

    # Handle missing prices: zero-fill
    bad <- is.na(fills$fill_price) | !is.finite(fills$fill_price) | fills$fill_price <= 0
    if (any(bad)) {
        fills$shares_filled[bad] <- 0
        fills$fill_price[bad] <- NA_real_
        fills$notional[bad] <- 0
    }

    fills[, c("symbol", "shares_filled", "fill_price", "notional", "direction")]
}

# ── Compute costs ─────────────────────────────────────────────────────────────

#' @export
bt_compute_costs <- function(fills, spec_costs) {
    fee_rate <- spec_costs$fee_rate %||% 0.0003
    slippage_rate <- spec_costs$slippage_rate %||% 0.001

    if (nrow(fills) == 0) {
        return(list(
            total_cost = 0, fee_total = 0, slippage_total = 0,
            per_symbol = data.frame(
                symbol = character(0),
                fee = numeric(0),
                slippage = numeric(0)
            )
        ))
    }

    abs_notional <- abs(fills$notional)
    abs_notional[!is.finite(abs_notional)] <- 0

    fees <- abs_notional * fee_rate
    slippage <- abs_notional * slippage_rate

    list(
        total_cost = sum(fees + slippage),
        fee_total = sum(fees),
        slippage_total = sum(slippage),
        per_symbol = data.frame(
            symbol = fills$symbol,
            fee = fees,
            slippage = slippage,
            stringsAsFactors = FALSE
        )
    )
}

# ── Full execution orchestrator ───────────────────────────────────────────────

#' @export
bt_execute_rebalance <- function(portfolio_state, proposal, exec_prices,
                                 spec_execution, spec_costs) {
    target_shares <- bt_compute_target_shares(
        proposal$target_weights, portfolio_state$nav, exec_prices
    )

    orders <- bt_generate_orders(portfolio_state$positions, target_shares)
    fills <- bt_simulate_fills(orders, exec_prices)
    costs <- bt_compute_costs(fills, spec_costs)

    # Compute cash delta: sells add cash, buys consume cash
    cash_delta <- -sum(fills$notional, na.rm = TRUE) - costs$total_cost

    list(
        decision_date  = proposal$decision_date,
        execution_date = NA, # filled by runner
        orders         = orders,
        fills          = fills,
        costs          = costs,
        cash_delta     = cash_delta,
        target_shares  = target_shares,
        warnings       = character(0)
    )
}
