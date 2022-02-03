#' Internal Helper Functions for Validation of [`Stopping`] Objects
#'
#' @description `r lifecycle::badge("stable")`
#'
#' These functions are only used internally to validate the format of an input
#' [`Stopping`] or inherited classes and therefore not exported.
#'
#' @name validate_stopping
#' @param object (`Stopping`)\cr object to validate.
#' @return A `character` vector with the validation failure messages,
#'   or `TRUE` in case validation passes.
NULL

#' @describeIn validate_stopping validates that the [`StoppingMTDCV`] object
#'   contains valid probability target and percentage threshold.
validate_stopping_mtd_cv <- function(object) {
  o <- Validate()
  o$check(
    is.probability(object@target, bounds = FALSE),
    "target must be probability > 0 and < 1"
  )
  o$check(
    is.probability(object@thresh_cv / 100, bounds = FALSE),
    "thresh_cv must be percentage > 0"
  )
  o$result()
}