# 02_state.R — universe mapping Π_t, recursive state schema, VoV recursion

came_pi_vector <- function(v, new_univ, init_val = 0) {
  out <- setNames(rep(init_val, length(new_univ)), new_univ)
  if (!is.null(v) && length(v) > 0) {
    common <- intersect(names(v), new_univ)
    if (length(common) > 0) out[common] <- v[common]
  }
  out
}

came_pi_matrix <- function(M, new_univ, init_diag = 1e-4) {
  p <- length(new_univ)
  out <- matrix(0, p, p, dimnames = list(new_univ, new_univ))
  if (!is.null(M) && is.matrix(M)) {
    rn <- rownames(M) %||% colnames(M)
    if (!is.null(rn)) {
      common <- intersect(rn, new_univ)
      if (length(common) > 0) out[common, common] <- M[common, common, drop = FALSE]
    }
  }
  diag(out) <- ifelse(diag(out) == 0, init_diag, diag(out))
  came_symmetrize(out)
}

came_pi_mask <- function(M_prev, new_univ) {
  p <- length(new_univ)
  out <- matrix(FALSE, p, p, dimnames = list(new_univ, new_univ))
  if (!is.null(M_prev) && is.matrix(M_prev)) {
    rn <- rownames(M_prev) %||% colnames(M_prev)
    if (!is.null(rn)) {
      common <- intersect(rn, new_univ)
      if (length(common) > 0) out[common, common] <- (M_prev[common, common, drop = FALSE] != 0)
    }
  }
  diag(out) <- FALSE
  out
}

came_pi_list <- function(lst, new_univ, init_fn = function() NULL) {
  out <- setNames(vector("list", length(new_univ)), new_univ)
  if (is.null(lst) || !is.list(lst)) {
    for (nm in new_univ) out[[nm]] <- init_fn()
    return(out)
  }
  common <- intersect(names(lst), new_univ)
  for (nm in common) out[[nm]] <- lst[[nm]]
  for (nm in setdiff(new_univ, common)) out[[nm]] <- init_fn()
  out
}

# Canonical recursive state schema aligned to architecture §4.1
came_state_init <- function() {
  list(
    risk = list(
      sigma2 = NULL,
      sigma2_hist = NULL,
      Sigma_f = NULL,
      S_e = NULL,
      B_prev = NULL,
      bar_sigma_prev = NULL,
      vov2 = NULL
    ),
    structure = list(
      P_bar = NULL,
      M_prev = NULL,
      edge_stab = NULL,
      node_stab = NULL,
      labels = NULL
    ),
    signals = list(
      kalman = NULL,
      omega = NULL
    ),
    forecast = list(
      models = NULL,
      error_buckets = NULL,
      hist = NULL,
      step = 0L
    ),
    history = list(
      schema = "came_history_v1",
      snapshots = list()
    ),
    prev_target = NULL
  )
}

# Apply Π_t to state objects that are universe-indexed.
# Call this once at the beginning of a snapshot run (before any updates).
came_state_pi <- function(state, new_univ) {
  st <- state %||% came_state_init()

  # risk sigma2
  if (!is.null(st$risk$sigma2)) {
    init <- stats::median(st$risk$sigma2[is.finite(st$risk$sigma2) & st$risk$sigma2 > 0], na.rm = TRUE)
    if (!is.finite(init) || init <= 0) init <- 1e-4
    st$risk$sigma2 <- came_pi_vector(st$risk$sigma2, new_univ, init_val = init)
  }

  # S_e
  if (!is.null(st$risk$S_e)) {
    initd <- stats::median(diag(st$risk$S_e), na.rm = TRUE)
    if (!is.finite(initd) || initd <= 0) initd <- 1e-4
    st$risk$S_e <- came_pi_matrix(st$risk$S_e, new_univ, init_diag = initd)
  }

  # structure P_bar and M_prev and edge stability
  if (!is.null(st$structure$P_bar)) st$structure$P_bar <- came_pi_matrix(st$structure$P_bar, new_univ, init_diag = 0)
  if (!is.null(st$structure$M_prev)) st$structure$M_prev <- came_pi_mask(st$structure$M_prev, new_univ)
  if (!is.null(st$structure$edge_stab)) st$structure$edge_stab <- came_pi_matrix(st$structure$edge_stab, new_univ, init_diag = 0.5)

  # node stability vector
  if (!is.null(st$structure$node_stab)) {
    st$structure$node_stab <- came_pi_vector(st$structure$node_stab, new_univ, init_val = 1.0)
  }

  # signals: Kalman list is per-asset
  if (!is.null(st$signals$kalman)) {
    st$signals$kalman <- came_pi_list(st$signals$kalman, new_univ, init_fn = function() NULL)
  }

  st
}

came_history_append <- function(state, as_of_date, X, keep = 300L) {
  st <- state
  if (is.null(st$history) || !is.list(st$history$snapshots)) {
    st$history <- list(schema = "came_history_v1", snapshots = list())
  }

  snaps <- st$history$snapshots
  snaps[[length(snaps) + 1L]] <- list(date = as.Date(as_of_date), X = X)

  # dedupe by date, keep last
  dates <- vapply(snaps, function(s) as.character(s$date), character(1))
  if (anyDuplicated(dates)) {
    keep_idx <- !duplicated(dates, fromLast = TRUE)
    snaps <- snaps[keep_idx]
  }
  if (length(snaps) > keep) snaps <- tail(snaps, keep)

  st$history$snapshots <- snaps
  st
}

came_extract_prev_weights <- function(prev_target_df) {
  if (is.null(prev_target_df)) {
    return(NULL)
  }
  if (is.data.frame(prev_target_df) && all(c("symbol", "weight_target") %in% names(prev_target_df))) {
    w <- setNames(prev_target_df$weight_target, prev_target_df$symbol)
    w[!is.finite(w)] <- 0
    return(w)
  }
  NULL
}

# VoV recursion (architecture §11.5.3)
came_update_vov <- function(state, sigma2_t, lambda_vov = 0.97) {
  st <- state
  sigma <- sqrt(as.numeric(sigma2_t))
  sigma <- sigma[is.finite(sigma) & sigma > 0]
  bar_sigma <- if (length(sigma) > 0) stats::median(sigma) else 1e-4

  prev_bar <- st$risk$bar_sigma_prev %||% bar_sigma
  if (!is.finite(prev_bar) || prev_bar <= 0) prev_bar <- bar_sigma

  dlog <- log(bar_sigma) - log(prev_bar)
  if (!is.finite(dlog)) dlog <- 0

  vov2_prev <- st$risk$vov2 %||% 0
  if (!is.finite(vov2_prev) || vov2_prev < 0) vov2_prev <- 0

  lam <- lambda_vov
  if (!is.finite(lam) || lam <= 0 || lam >= 1) lam <- 0.97

  vov2 <- lam * vov2_prev + (1 - lam) * (dlog^2)
  if (!is.finite(vov2) || vov2 < 0) vov2 <- 0

  st$risk$bar_sigma_prev <- bar_sigma
  st$risk$vov2 <- vov2

  list(state = st, VoV = sqrt(vov2))
}
