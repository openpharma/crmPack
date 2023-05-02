#' Helpers for stripping expressions of `covr`-inserted trace code
#'
#' Workarounds to allow the package to continue to work while running `covr`
#' with minimal changes to the package code.
#'
#' @details
#' When using `covr`, the source code for package objects are modified to add
#' callbacks for each expression to log its execution. Given an arbitrary
#' expression, such as:
#'
#'     expr
#'
#' The code will be modified before executing any package code to look like:
#'
#'     if (TRUE) {
#'       covr:::count("file.R:1:2:3:4:5:6:7:8")
#'       expr
#'     }
#'
#' These functions are used for stripping expressions of this code so that the
#' package continues to work as intended while running tests as part of running
#' `covr` to calculate package coverage.
#'
#' This method is non-exhaustive, covering only a subset of `covr`'s tracing
#' behaviors necessary for this package.
#'
#' @param expr (`language`)\cr an R expression or call to test or strip of
#'   `covr` trace counters.
#'
#' @return A logical value or transformed expression with calls to
#'   `covr:::count` removed.
#'
#' @name h_covr_helpers
#' @keywords internal
#'
NULL

#' @describeIn h_covr_helpers
#' Determine whether `covr` is currently running
h_covr_active <- function() {
  identical(Sys.getenv("R_COVR"), "true")
}

#' @describeIn h_covr_helpers
#' Remove `covr` traces from an expression
h_covr_detrace <- function(expr) {
  if (!h_covr_active()) {
    return(expr)
  }

  if (is.function(expr)) {
    body(expr) <- h_covr_detrace(body(expr))
    return(expr)
  }

  detrace <- function(x) {
    x <- h_covr_detrace_call(x)
    if (is.call(x)) x[-1] <- lapply(x[-1], h_covr_detrace)
    x
  }

  detrace(expr)
}

#' @describeIn h_covr_helpers
#' Determine whether the current expression is a `covr`-modified expression
h_is_covr_trace <- function(expr) {
  # Matches `if (TRUE) { covr:::count(<trace>); <expr> }` (see covr:::trace_calls).
  is.call(expr) &&
    expr[[1]] == "if" &&
    expr[[2]] == quote(TRUE) &&
    expr[[3]][[1]] == "{" &&
    length(expr[[3]] >= 3) &&
    is.call(expr[[3]][[2]]) &&
    expr[[3]][[2]][[1]] == call(":::", as.symbol("covr"), as.symbol("count"))
}

#' @describeIn h_covr_helpers
#' Extract the original expression from a `covr`-modified expression
h_covr_detrace_call <- function(expr) {
  if (h_is_covr_trace(expr)) expr[[3]][[3]] else expr
}
