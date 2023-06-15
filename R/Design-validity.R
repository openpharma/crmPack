# Design ----

#' Internal Helper Functions for Validation of [`RuleDesign`] Objects
#'
#' @description `r lifecycle::badge("stable")`
#'
#' These functions are only used internally to validate the format of an input
#' [`RuleDesign`] or inherited classes and therefore not exported.
#'
#' @name v_design
#' @param object (`RuleDesign`)\cr object to validate.
#' @return A `character` vector with the validation failure messages,
#'   or `TRUE` in case validation passes.
NULL

#' @describeIn v_design validates that the [`RuleDesign`] object
#'   contains valid `startingDose`.
v_rule_design <- function(object) {
  v <- Validate()
  v$check(
    test_number(object@startingDose, finite = TRUE),
    "startingDose must be a number"
  )
  v$check(
    test_subset(object@startingDose, choices = object@data@doseGrid, empty.ok = FALSE),
    "startingDose must be included in data@doseGrid"
  )
  v$result()
}
