#' Internal Helper Functions for Validation of `GeneralData` Objects.
#'
#' @description `r lifecycle::badge("stable")`
#' 
#' These functions are only used internally to validate the format of an input
#' [`GeneralData-class`] object and therefore not exported.
#'
#' @name validate_data
#' @param object (`GeneralData`)\cr object to validate.
#' @return A character vector with the validation failure messages,
#' or `TRUE` in case validation passes.
NULL

#' @describeIn validate_data validates that the [`GeneralData-class`] object contains
#'   unique `ID`, non-negative `cohort` indices and
#'   `ID` and `cohort` vectors are of the same length equal to `nObs`.
#'
validate_subjects <- function(object) {
  o <- Validate()
  
  o$check(is.integer(object@nObs), "nObs must be of type integer")
  o$check(length(object@nObs) == 1L, "nObs must be of length 1")
  
  o$check(is.integer(object@ID), "ID must be of type integer")
  o$check(all(!duplicated(object@ID)), "IDs must be unique")
  
  o$check(is.integer(object@cohort), "cohort must be of type integer")
  o$check(all(object@cohort >= 0), "cohort indices must be non-negative")
  o$check(!is.unsorted(object@cohort, strictly = FALSE), "cohort indices must be sorted")
  
  o$check(
    length(object@ID) == object@nObs && length(object@cohort) == object@nObs,
    "ID and cohort must be of the same length equal to nObs"
  )

  o$result()
}
