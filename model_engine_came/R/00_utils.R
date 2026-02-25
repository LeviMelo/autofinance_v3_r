# CAME — Causal Architecture Model Engine
# 00_utils.R — utilities, assertions, error handling, stage runner

`%||%` <- function(a, b) if (is.null(a)) b else a

came_require <- function(pkgs) {
  for (pkg in pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf("CAME requires package '%s' but it is not installed.", pkg), call. = FALSE)
    }
  }
  invisible(TRUE)
}

# ---- conditions ----

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

came_assert <- function(ok, code, message, data = NULL) {
  if (!isTRUE(ok)) stop(came_error(code, message, data = data))
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
  rn <- rownames(M); cn <- colnames(M)
  came_assert(!is.null(rn) && !is.null(cn), code, message)
  came_assert(identical(rn, cn), code, message)
  invisible(TRUE)
}

# ---- stage runner (no silent fallbacks) ----

came_run_stage <- function(stage, expr, warnings_acc = NULL) {
  withCallingHandlers(
    tryCatch(expr, error = function(e) {
      if (inherits(e, "came_error")) stop(e)
      stop(came_error(paste0(stage, "_error"), conditionMessage(e)))
    }),
    warning = function(w) {
      if (!is.null(warnings_acc) && is.function(warnings_acc)) {
        warnings_acc(stage, conditionMessage(w))
      }
      invokeRestart("muffleWarning")
    }
  )
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
  came_require(c("Matrix"))
  S <- came_symmetrize(S)
  # add small ridge to diagonal for numerical stability
  diag(S) <- diag(S) + eps
  as.matrix(Matrix::nearPD(S, corr = FALSE)$mat)
}

came_hash <- function(x) {
  came_require("digest")
  digest::digest(x)
}
