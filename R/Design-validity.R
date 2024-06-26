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

#' Internal Helper Functions for Validation of [`RuleDesignOrdinal`] Objects
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' These functions are only used internally to validate the format of an input
#' [`RuleDesignOrdinal`] or inherited classes and therefore not exported.
#'
#' @name v_design
#' @param object (`RuleDesignOrdinal`)\cr object to validate.
#' @return A `character` vector with the validation failure messages,
#'   or `TRUE` in case validation passes.
NULL

#' @describeIn v_design validates that the [`RuleDesignOrdinal`] object
#'   contains valid `starting_dose`.
v_rule_design_ordinal <- function(object) {
  v <- Validate()
  v$check(
    test_number(object@starting_dose, finite = TRUE),
    "starting_dose must be a number"
  )
  v$check(
    test_subset(object@starting_dose, choices = object@data@doseGrid, empty.ok = FALSE),
    "starting_dose must be included in data@doseGrid"
  )
  v$result()
}


#' @describeIn v_design validates that the [`DesignGrouped`] object
#'   contains valid flags.
v_design_grouped <- function(object) {
  v <- Validate()
  v$check(
    test_flag(object@first_cohort_mono_only),
    "first_cohort_mono_only must be a flag"
  )
  v$check(
    test_flag(object@same_dose_for_all),
    "same_dose_for_all must be a flag"
  )
  v$check(
    test_flag(object@same_dose_for_all),
    "same_dose_for_start must be a flag"
  )
  v$result()
}
