#' Internal Helper Functions for Validation of `GeneralData` Objects
#'
#' These functions are only used internally to validate the format of an input
#' [`GeneralData`] object and therefore not exported.
#'
#' @name validate_data
#' @param object (`GeneralData`)\cr object to validate.
#' @return A character vector with the validation failure messages,
#' or `TRUE` in case validation passes.
NULL

#' @describeIn validate_data validates that the [`GeneralData`] object contains
#'   unique `ID`, non-negative `cohort` indices and
#'   `ID` and `cohort` vectors are of the same length equal to `nObs`.
#' @param object (`GeneralData`)\cr object to validate.
#'
validate_subjects <- function(object) {
  assert_class(object, "GeneralData")
  assert_integer(object@nObs, len = 1L)
  assert_integer(object@ID)
  assert_integer(object@cohort)
  
  o <- Validate()
  o$check(all(!duplicated(object@ID)), "IDs must be unique")
  o$check(all(object@cohort >= 0), "cohort indices must be non-negative")
  o$check(!is.unsorted(object@cohort, strictly = FALSE), "cohort indices must be sorted")
  o$check(
    length(object@ID) == object@nObs && length(object@cohort) == object@nObs,
    "ID and cohort must be of the same length equal to nObs"
  )

  o$result()
}
