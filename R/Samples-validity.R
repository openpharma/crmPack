#' @include McmcOptions-methods.R
NULL

# v_samples ----

#' Internal Helper Functions for Validation of [`Samples`] Objects
#'
#' @description `r lifecycle::badge("stable")`
#'
#' These functions are only used internally to validate the format of an input
#' [`Samples`] or inherited classes and therefore not exported.
#'
#' @name v_samples_objects
#' @param object (`Samples`)\cr object to validate.
#' @return A `character` vector with the validation failure messages, or `TRUE`
#'   in case validation passes.
NULL

#' @describeIn v_samples_objects validates that the [`Samples`] object contains
#'   valid `data` slot.
v_samples <- function(object) {
  v <- Validate()
  v$check(
    all(sapply(object@data, NROW) == size(object@options)),
    "Every element in data must be of the same length (no. of rows) as the sample size was"
  )
  v$check(
    all(sapply(object@data, test_numeric, finite = TRUE, any.missing = FALSE)),
    "Every element in data must be a finite object of type integer or double"
  )
  v$result()
}
