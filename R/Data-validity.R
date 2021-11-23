#' Internal Helper Functions for Validation of `GeneralData` and `Data` Objects.
#'
#' @description `r lifecycle::badge("stable")`
#'
#' These functions are only used internally to validate the format of an input
#' [`GeneralData-class`] and [`Data-class`] object and therefore not exported.
#'
#' @name validate_data
#' @param object (`GeneralData`)\cr object to validate.
#' @return A character vector with the validation failure messages,
#' or `TRUE` in case validation passes.
NULL

#' @describeIn validate_data helper function.
#' It returns `TRUE` if `dose` is unique per `cohort`.
#' @param dose (`numeric`)\cr vector of doses.
#' @param cohort (`integer`)\cr vector of cohorts corresponding to `doses`.
#'
h_is_dose_unique <- function(dose, cohort) {
  all(tapply(dose, cohort, function(d) length(unique(d)) == 1L))
}

#' @describeIn validate_data validates that the [`GeneralData-class`] object contains
#'   unique `ID`, non-negative `cohort` indices and
#'   `ID` and `cohort` vectors are of the same length equal to `nObs`.
#'
validate_subjects <- function(object) {
  o <- Validate()

  o$check(test_int(object@nObs), "nObs must be of type integer of length 1")

  o$check(
    test_integer(object@ID, len = object@nObs),
    "ID must be of type integer and length nObs"
  )
  o$check(all(!duplicated(object@ID)), "IDs must be unique")

  o$check(
    test_integer(object@cohort, len = object@nObs),
    "cohort must be of type integer and length nObs"
  )
  o$check(all(object@cohort >= 0), "cohort indices must be non-negative")
  o$check(!is.unsorted(object@cohort, strictly = FALSE), "cohort indices must be sorted")

  o$result()
}

#' @describeIn validate_data validates that the [`Data-class`] object contains
#' valid elements with respect to their types, dependency and length.
#'
validate_data <- function(object) {
  o <- Validate()

  o$check(
    test_double(object@x, len = object@nObs),
    "Doses vector x must be of type double and length nObs"
  )

  o$check(
    test_integer(object@y, lower = 0, upper = 1, len = object@nObs),
    "DLT vector y must be of type integer of lenght nObs and can only take 0 or 1 values"
  )

  o$check(
    test_double(object@doseGrid, len = object@nGrid),
    "Possible doses vector doseGrid must be of type double and length nGrid"
  )

  o$check(
    test_int(object@nGrid),
    "Number of gridpoints nGrid must be of type integer"
  )

  o$check(
    test_integer(object@xLevel, len = object@nObs),
    "Levels xLevel for the doses the patients have been given must be of type integer
     and length nObs"
  )

  o$check(test_flag(object@placebo), "The placebo flag must be of type logical")

  o$check(
    !is.unsorted(object@doseGrid, strictly = TRUE),
    "doseGrid must be sorted and without duplicate values"
  )

  o$check(
    test_subset(object@x, object@doseGrid),
    "Dose values in x must be from doseGrid"
  )

  x_eq_grid_xLev <- all.equal(
    object@x,
    object@doseGrid[object@xLevel],
    tolerance = 1e-10,
    check.names = FALSE,
    check.attributes = FALSE
  )

  o$check(
    test_flag(x_eq_grid_xLev) && x_eq_grid_xLev == TRUE,
    "x must be equal to doseGrid[xLevel] (tolerance 1e-10)"
  )

  if (object@placebo) {
    plcb <- object@x == object@doseGrid[1] # indicator of placebo
    o$check(
      test_set_equal(object@cohort, object@cohort[!plcb]),
      "A cohort with only placebo is not allowed"
    )
  } else {
    plcb <- FALSE
  }

  o$check(
    h_is_dose_unique(dose = object@x[!plcb], cohort = object@cohort[!plcb]),
    "There must be only one dose level, other than placebo, per cohort"
  )

  o$result()
}
