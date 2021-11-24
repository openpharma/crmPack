#' Internal Helper Functions for Validation of [`GeneralData`] Objects
#'
#' @description `r lifecycle::badge("stable")`
#'
#' These functions are only used internally to validate the format of an input
#' [`GeneralData`] or inherited classes and therefore not exported.
#'
#' @name validate_data_objects
#' @param object (`GeneralData`)\cr object to validate.
#' @return A character vector with the validation failure messages,
#'   or `TRUE` in case validation passes.
NULL

#' @describeIn validate_data_objects validates that the [`GeneralData`] 
#'   object contains unique `ID`, non-negative `cohort` indices and
#'   `ID` and `cohort` vectors which are of the same length `nObs`.
validate_subjects <- function(object) {
  o <- Validate()
  o$check(
    test_int(object@nObs), 
    "nObs must be of type integer of length 1"
  )
  o$check(
    test_integer(object@ID, len = object@nObs, any.missing = FALSE, unique = TRUE),
    "ID must be of type integer and length nObs and unique"
  )
  o$check(
    test_integer(object@cohort, len = object@nObs, any.missing = FALSE, lower = 0L),
    "cohort must be of type integer and length nObs and non-negative"
  )
  o$check(
    !is.unsorted(object@cohort, strictly = FALSE), 
    "cohort indices must be sorted"
  )
  o$result()
}

#' @describeIn validate_data_objects helper function which verifies whether
#'   the `dose` values are unique in each and every different `cohort`.
#' @param dose (`numeric`)\cr dose values.
#' @param cohort (`integer`)\cr cohort indices parallel to `doses`.
#' @return `TRUE` if `dose` is unique per `cohort`, otherwise `FALSE`.
h_doses_unique_per_cohort <- function(dose, cohort) {
  num_doses_per_cohort <- tapply(
    X = dose, 
    INDEX = cohort, 
    FUN = function(d) length(unique(d))
  )
  all(num_doses_per_cohort == 1L)
}

#' @describeIn validate_data_objects validates that the [`Data`] object contains
#'   valid elements with respect to their types, dependency and length.
validate_data <- function(object) {
  o <- Validate()
  o$check(
    test_double(object@x, len = object@nObs, any.missing = FALSE),
    "Doses vector x must be of type double and length nObs"
  )
  o$check(
    test_integer(object@y, lower = 0, upper = 1, len = object@nObs, any.missing = FALSE),
    "DLT vector y must be nObs long and contain 0 or 1 integers only"
  )
  o$check(
    test_double(object@doseGrid, len = object@nGrid, any.missing = FALSE, sorted = TRUE, unique = TRUE),
    "doseGrid must be of type double and length nGrid and contain unique, sorted values"
  )
  o$check(
    test_int(object@nGrid),
    "Number of dose grid values nGrid must be scalar integer"
  )
  o$check(
    test_integer(object@xLevel, len = object@nObs, any.missing = FALSE),
    "Levels xLevel for the doses the patients have been given must be of type integer and length nObs"
  )
  o$check(
    test_flag(object@placebo), 
    "The placebo flag must be scalar logical"
  )
  o$check(
    test_subset(object@x, object@doseGrid),
    "Dose values in x must be from doseGrid"
  )
  o$check(
    h_all_equivalent(object@x, object@doseGrid[object@xLevel]),
    "x must be equivalent to doseGrid[xLevel] (up to numerical tolerance)"
  )
  if (object@placebo) {
    is_placebo <- object@x == object@doseGrid[1]
    o$check(
      test_set_equal(object@cohort, object@cohort[!is_placebo]),
      "A cohort with only placebo is not allowed"
    )
    o$check(
      h_doses_unique_per_cohort(dose = object@x[!is_placebo], cohort = object@cohort[!is_placebo]),
      "There must be only one dose level, other than placebo, per cohort"
    )
  }
  o$result()
}
