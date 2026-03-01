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
# Supports optional locked_symbols: those symbols must not be traded even if targeted.

#' @export
bt_execute_rebalance <- function(portfolio_state, proposal, exec_prices,
                                 spec_execution, spec_costs) {
    fee_rate <- spec_costs$fee_rate %||% 0.0003
    slip_rate <- spec_costs$slippage_rate %||% 0.0010
    cost_rate <- fee_rate + slip_rate

    # 1) initial target shares
    target_shares <- bt_compute_target_shares(
        proposal$target_weights, portfolio_state$nav, exec_prices
    )

    # 2) Apply trade locks
    locked <- proposal$locked_symbols %||% character(0)
    if (length(locked) > 0) {
        locked <- unique(as.character(locked))
        cur <- portfolio_state$positions
        for (s in locked) {
            if (s %in% names(cur)) target_shares[s] <- cur[s]
        }
    }

    # 3) Build orders and estimate cash impact
    orders <- bt_generate_orders(portfolio_state$positions, target_shares)
    if (nrow(orders) > 0) {
        px <- exec_prices[orders$symbol]
        px[!is.finite(px) | px <= 0] <- NA_real_
        notional <- orders$shares_delta * px
        notional[!is.finite(notional)] <- 0

        buy_notional <- sum(pmax(notional, 0), na.rm = TRUE)
        sell_notional <- sum(pmax(-notional, 0), na.rm = TRUE)

        # Costs apply to absolute traded notional
        est_cost <- cost_rate * sum(abs(notional), na.rm = TRUE)

        cash_after <- portfolio_state$cash + sell_notional - buy_notional - est_cost

        # 4) If would go negative, scale DOWN buys
        if (is.finite(cash_after) && cash_after < 0 && buy_notional > 0) {
            # cash available for buys after accounting for sells and estimated sell costs
            # conservative: reserve cost_rate on sells too
            cash_avail <- portfolio_state$cash +
                sell_notional - cost_rate * sell_notional

            # max buy notional allowed including buy-side costs
            max_buy <- cash_avail / (1 + cost_rate)
            max_buy <- max(0, max_buy)

            scale <- max_buy / buy_notional
            scale <- max(0, min(1, scale))

            # scale only BUY orders (shares_delta > 0)
            buy_idx <- which(orders$shares_delta > 0)
            if (length(buy_idx) > 0 && scale < 1) {
                # adjust target_shares for buy symbols
                for (i in buy_idx) {
                    sym <- orders$symbol[i]
                    # reduce towards current position by scaling delta
                    cur_pos <- portfolio_state$positions[sym] %||% 0
                    tgt <- target_shares[sym] %||% 0
                    delta <- tgt - cur_pos
                    new_tgt <- cur_pos + floor(delta * scale)
                    target_shares[sym] <- new_tgt
                }
            }

            # rebuild orders after scaling
            orders <- bt_generate_orders(portfolio_state$positions, target_shares)
        }
    }

    # 5) Simulate fills + true costs
    fills <- bt_simulate_fills(orders, exec_prices)
    costs <- bt_compute_costs(fills, spec_costs)

    cash_delta <- -sum(fills$notional, na.rm = TRUE) - costs$total_cost

    list(
        decision_date  = proposal$decision_date,
        execution_date = NA,
        orders         = orders,
        fills          = fills,
        costs          = costs,
        cash_delta     = cash_delta,
        target_shares  = target_shares,
        warnings       = character(0)
    )
}
