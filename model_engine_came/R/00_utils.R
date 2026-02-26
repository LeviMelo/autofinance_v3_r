# CAME — Causal Architecture Model Engine
# 00_utils.R — utilities, assertions, error handling, numerics

`%||%` <- function(a, b) if (is.null(a)) b else a

came_require <- function(pkgs) {
  for (pkg in pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf("CAME requires package '%s' but it is not installed.", pkg), call. = FALSE)
    }
  }
  invisible(TRUE)
}

# ---- typed conditions ----

came_error <- function(code, message, data = NULL) {
  structure(
    list(code = as.character(code), message = as.character(message), data = data),
    class = c("came_error", "error", "condition")
  )
}

came_warning <- function(code, message, data = NULL) {
  structure(
    list(code = as.character(code), message = as.character(message), data = data),
    class = c("came_warning", "warning", "condition")
  )
}

came_stop <- function(code, message, data = NULL) {
  stop(came_error(code, message, data = data))
}

came_warn <- function(code, message, data = NULL) {
  warning(came_warning(code, message, data = data), call. = FALSE)
}

# ---- assertions ----

came_assert <- function(ok, code, message, data = NULL) {
  if (!isTRUE(ok)) came_stop(code, message, data = data)
  invisible(TRUE)
}

came_assert_scalar_numeric <- function(x, code, message) {
  came_assert(is.numeric(x) && length(x) == 1L && is.finite(x), code, message)
  invisible(TRUE)
}

came_assert_named <- function(x, code, message) {
  came_assert(!is.null(x), code, message)
  came_assert(length(x) == length(names(x)) && all(nzchar(names(x))), code, message)
  invisible(TRUE)
}

came_assert_square_named_matrix <- function(M, code, message) {
  came_assert(is.matrix(M), code, message)
  came_assert(nrow(M) == ncol(M), code, message)
  rn <- rownames(M)
  cn <- colnames(M)
  came_assert(!is.null(rn) && !is.null(cn), code, message)
  came_assert(identical(rn, cn), code, message)
  invisible(TRUE)
}

came_assert_date <- function(x, code, message) {
  d <- tryCatch(as.Date(x), error = function(e) NA)
  came_assert(!is.na(d), code, message)
  invisible(TRUE)
}

# ---- stage runner (no silent fallbacks) ----
# Captures warnings (optional) but never continues after an error.

came_run_stage <- function(stage, expr, warnings_acc = NULL) {
  withCallingHandlers(
    tryCatch(expr, error = function(e) {
      if (inherits(e, "came_error")) stop(e)
      came_stop(paste0(stage, "_error"), paste0("[", stage, "] ", conditionMessage(e)))
    }),
    warning = function(w) {
      if (!is.null(warnings_acc) && is.function(warnings_acc)) {
        warnings_acc(stage, conditionMessage(w))
      }
      invokeRestart("muffleWarning")
    }
  )
}

# ---- numerics ----

came_sigmoid <- function(z) {
  z <- as.numeric(z)
  z[!is.finite(z)] <- 0
  1 / (1 + exp(-z))
}

came_softmax <- function(z, temperature = 1.0) {
  z <- as.numeric(z)
  z[!is.finite(z)] <- 0
  tau <- temperature %||% 1.0
  if (!is.finite(tau) || tau <= 0) tau <- 1.0
  z <- z / tau
  z <- z - max(z)
  e <- exp(z)
  p <- e / sum(e)
  p[!is.finite(p)] <- 1 / length(p)
  p
}

came_symmetrize <- function(M) {
  (M + t(M)) / 2
}

came_near_psd <- function(S, eps = 1e-10) {
  came_require("Matrix")
  came_assert(is.matrix(S), "near_psd_type", "S must be a matrix")
  S <- came_symmetrize(S)
  diag(S) <- diag(S) + eps
  as.matrix(Matrix::nearPD(S, corr = FALSE)$mat)
}

came_hash <- function(x) {
  came_require("digest")
  digest::digest(x)
}

# ---- small helpers ----

came_rank01 <- function(x) {
  x <- as.numeric(x)
  x[!is.finite(x)] <- NA_real_
  r <- rank(x, ties.method = "average", na.last = "keep")
  out <- r / (sum(is.finite(x)) + 1)
  out[!is.finite(out)] <- 0.5
  out
}

came_quantile_safe <- function(x, p, default = NA_real_) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (length(x) < 2) {
    return(default)
  }
  as.numeric(stats::quantile(x, probs = p, na.rm = TRUE, names = FALSE, type = 7))
}

came_solve_lsap_max <- function(score_mat) {
  # Solve linear sum assignment maximizing score_mat.
  # Compatible with clue versions that require nonnegative entries
  # and may or may not support solve_LSAP(..., maximum=TRUE).

  came_require("clue")

  S <- as.matrix(score_mat)
  S[!is.finite(S)] <- 0
  S[S < 0] <- 0

  lsap <- clue::solve_LSAP

  # Newer clue supports maximum=TRUE
  if ("maximum" %in% names(formals(lsap))) {
    return(as.integer(lsap(S, maximum = TRUE)))
  }

  # Older clue: convert to nonnegative minimization cost
  m <- max(S)
  cost <- m - S
  cost[!is.finite(cost)] <- m
  cost[cost < 0] <- 0

  as.integer(lsap(cost))
}
