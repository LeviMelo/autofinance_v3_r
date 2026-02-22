#' @title Execution and Costs
#' @description Convert target weights into trades/fills and apply costs.

#' @export
bt_generate_orders <- function(current_positions, current_nav, target_weights, exec_prices, spec_execution) {
  # Merge current vs target to find deltas
  tgt <- target_weights
  if (nrow(tgt) == 0) tgt <- data.frame(symbol = character(0), weight_target = numeric(0))

  tgt_notional <- tgt$weight_target * current_nav
  names(tgt_notional) <- tgt$symbol

  # Ensure all necessary exec prices are present
  syms <- unique(c(current_positions$symbol, tgt$symbol))
  prices <- exec_prices[syms]
  # Fill NA prices with last known if necessary/available, but for simplicity here we assume valid exec
  prices[is.na(prices)] <- 0

  current_qty <- rep(0, length(syms))
  names(current_qty) <- syms
  current_qty[current_positions$symbol] <- current_positions$qty

  current_notional <- current_qty * prices

  tgt_notional_full <- rep(0, length(syms))
  names(tgt_notional_full) <- syms
  tgt_notional_full[names(tgt_notional)] <- tgt_notional

  delta_notional <- tgt_notional_full - current_notional

  nonzero <- abs(delta_notional) > 1e-4

  delta_notional <- delta_notional[nonzero]
  syms_trade <- names(delta_notional)
  prices_trade <- prices[syms_trade]

  tgt_qty <- tgt_notional_full[syms_trade] / prices_trade
  tgt_qty[is.nan(tgt_qty) | is.infinite(tgt_qty)] <- 0

  if (length(syms_trade) == 0) {
    return(data.frame(symbol = character(0), tgt_qty = numeric(0), price = numeric(0)))
  }

  data.frame(
    symbol = syms_trade,
    tgt_qty = tgt_qty,
    price = prices_trade,
    trade_qty = tgt_qty - current_qty[syms_trade],
    trade_notional = delta_notional,
    stringsAsFactors = FALSE
  )
}

#' @export
bt_apply_costs <- function(orders, exec_prices, spec_costs) {
  if (nrow(orders) == 0) {
    return(0.0)
  }

  fee_rate <- spec_costs$fee_rate %||% 0.0003
  slip_rate <- spec_costs$slippage_rate %||% 0.0010

  total_trade_notional <- sum(abs(orders$trade_notional), na.rm = TRUE)
  total_trade_notional * (fee_rate + slip_rate)
}

#' @export
bt_execute_rebalance <- function(current_state, target, exec_prices, spec_execution, spec_costs) {
  # This function exists to provide detailed logged output
  # actual state update happens in bt_update_after_execution
  list(
    state = current_state,
    fills = target, # minimal representation
    costs = 0.0 # actual compute is external
  )
}
