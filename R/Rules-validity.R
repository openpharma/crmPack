#' Internal Helper Functions for Validation of [`Increments`] Objects
#'
#' @description `r lifecycle::badge("stable")`
#'
#' These functions are only used internally to validate the format of an input
#' [`Increments`] or inherited classes and therefore not exported.
#'
#' @name validate_increments
#' @param object (`Increments`)\cr object to validate.
#' @return A `character` vector with the validation failure messages,
#'   or `TRUE` in case validation passes.
NULL

#' @describeIn validate_increments validates that the [`IncrementsNumDoseLevels`] object
#'   contains valid `maxLevels` and `basisLevel` option.
validate_increments_numdoselevels <- function(object) {
  o <- Validate()
  o$check(
    is.scalar(object@maxLevels) &&
    is.integer(object@maxLevels) &&
    object@maxLevels > 0,
    "maxLevels must be scalar positive integer"
  )
  o$check(
    is.scalar(object@basisLevel) &&
    object@basisLevel %in% c("last", "max"),
    "basisLevel must be either 'last' or 'max'"
  )
  o$result()
}

#' @describeIn validate_stopping validates that the [`IncrementsHSRBeta`]
#'  object contains valid probability target, threshold and shape parameters.
validate_increments_hsr_beta <- function(object) {
  o <- Validate()
  o$check(
    is.probability(object@target, bounds = FALSE),
    "target must be a probability"
  )
  o$check(
    is.probability(object@prob, bounds = FALSE),
    "prob must be a probability"
  )
  o$check(
    is.scalar(object@a) & is.numeric(object@a) && object@a > 0,
    "Beta distribution shape parameter a must be a positive scalar"
  )
  o$check(
    is.scalar(object@b) & is.numeric(object@b) && object@b > 0,
    "Beta distribution shape parameter b must be a positive scalar"
  )

  o$result()
}

#' @describeIn validate_stopping validates that the [`StoppingLowestDoseHSRBeta`]
#'  object contains valid probability target, threshold and shape parameters.
validate_stopping_lowest_dose_hsr_beta <- validate_increments_hsr_beta

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
