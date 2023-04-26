#' Determine whether `covr` is currently running
#'
h_covr_active <- function() {
  identical(Sys.getenv("R_COVR"), "true")
}

#' Remove `covr` traces from an expression
#'
#' '`covr` interferes with expressions (often function bodies) as a primary
#' interface of this package. To work with `covr`, we need to strip out the
#' code that `covr` injects into the package before using these expressions
#' for internal logic such as writing model files.
#'
#' This method is non-exhaustive, covering only a subset of `covr`'s tracing
#' behaviors necessary for this package.
#'
#' @param expr (`language`)\cr an R expression or call to strip of `covr` trace
#'   counters.
#'
#' @return A transformed expression, with calls to `covr:::count` removed.
#'
h_covr_detrace <- function(expr) {
  if (!h_covr_active()) {
    return(expr)
  }

  if (is.function(expr)) {
    body(expr) <- h_covr_detrace(body(expr))
    return(expr)
  }

  is_covr_trace <- function(x) {
    # matches `if (TRUE) { covr:::count(<trace>); <expr> }` (see covr:::trace_calls)
    is.call(x) &&
    x[[1]] == "if" &&
    x[[2]] == quote(TRUE) &&
    x[[3]][[1]] == "{" &&
    length(x[[3]] >= 3) &&
    is.call(x[[3]][[2]]) &&
    x[[3]][[2]][[1]] == quote(covr:::count)
  }

  detrace_call <- function(x) {
    x[[3]][[3]]
  }

  detrace <- function(x) {
    if (is_covr_trace(x)) x <- detrace_call(x)
    if (is.call(x)) x[-1] <- lapply(x[-1], detrace)
    x
  }

  detrace(expr)
}
