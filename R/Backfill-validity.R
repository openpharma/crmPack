# Opening ----

#' Internal Helper Functions for Validation of [`Opening`] Objects
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' These functions are only used internally to validate the format of an input
#' [`Opening`] or inherited classes and therefore not exported.
#'
#' @name v_opening
#' @param object (`Opening`)\cr object to validate.
#' @return A `character` vector with the validation failure messages,
#'   or `TRUE` in case validation passes.
NULL

#' @describeIn v_opening validates that the [`OpeningMinDose`] object
#'   contains valid `min_dose` slot.
v_opening_min_dose <- function(object) {
  v <- Validate()
  v$check(
    test_number(object@min_dose, lower = 0),
    "min_dose needs to be a non-negative numeric scalar"
  )
  v$result()
}
