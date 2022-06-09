# nolint start
#' Internal Helper Functions for Validation of [`Increments`] Objects
#'
#' @description `r lifecycle::badge("stable")`
#'
#' These functions are only used internally to validate the format of an input
#' [`Increments`] or inherited classes and therefore not exported.
#'
#' @name v_increments
#' @param object (`Increments`)\cr object to validate.
#' @return A `character` vector with the validation failure messages,
#'   or `TRUE` in case validation passes.
NULL

#' @describeIn v_increments validates that the [`IncrementsNumDoseLevels`] object
#'   contains valid `maxLevels` and `basisLevel` option.
v_increments_numdoselevels <- function(object) {
  v <- Validate()
  v$check(
    is.scalar(object@maxLevels) &&
      is.integer(object@maxLevels) &&
      object@maxLevels > 0,
    "maxLevels must be scalar positive integer"
  )
  v$check(
    is.scalar(object@basisLevel) &&
      object@basisLevel %in% c("last", "max"),
    "basisLevel must be either 'last' or 'max'"
  )
  v$result()
}

#' @describeIn v_stopping validates that the [`IncrementsHSRBeta`]
#'  object contains valid probability target, threshold and shape parameters.
v_increments_hsr_beta <- function(object) {
  v <- Validate()
  v$check(
    is.probability(object@target, bounds = FALSE),
    "target must be a probability"
  )
  v$check(
    is.probability(object@prob, bounds = FALSE),
    "prob must be a probability"
  )
  v$check(
    is.scalar(object@a) & is.numeric(object@a) && object@a > 0,
    "Beta distribution shape parameter a must be a positive scalar"
  )
  v$check(
    is.scalar(object@b) & is.numeric(object@b) && object@b > 0,
    "Beta distribution shape parameter b must be a positive scalar"
  )

  v$result()
}

#' @describeIn v_stopping validates that the [`StoppingLowestDoseHSRBeta`]
#'  object contains valid probability target, threshold and shape parameters.
v_stopping_lowest_dose_hsr_beta <- v_increments_hsr_beta

#' Internal Helper Functions for Validation of [`Stopping`] Objects
#'
#' @description `r lifecycle::badge("stable")`
#'
#' These functions are only used internally to validate the format of an input
#' [`Stopping`] or inherited classes and therefore not exported.
#'
#' @name v_stopping
#' @param object (`Stopping`)\cr object to validate.
#' @return A `character` vector with the validation failure messages,
#'   or `TRUE` in case validation passes.
NULL

#' @describeIn v_stopping validates that the [`StoppingMTDCV`] object
#'   contains valid probability target and percentage threshold.
v_stopping_mtd_cv <- function(object) {
  v <- Validate()
  v$check(
    is.probability(object@target, bounds = FALSE),
    "target must be probability > 0 and < 1"
  )
  v$check(
    is.probability(object@thresh_cv / 100, bounds = FALSE),
    "thresh_cv must be percentage > 0"
  )
  v$result()
}

# nolint end

#' These functions are only used internally to validate the format of an input
#' [`NextBestNCRMLoss`] or inherited classes and therefore not exported.


#' @name v_NextBestNCRMLoss
#' @param object (`NextBestNCRMLoss`)\cr object to validate.
#' @return A `character` vector with the validation failure messages,
#'   or `TRUE` in case validation passes.
NULL

#' @describeIn v_NextBestNCRMLoss validates the [`.NextBestNCRMLoss`] object
#'   contains valid probability target and percentage threshold.
v_NextBestNCRMLoss <- function(object) {
  v <- Validate()

  v$check(
    is.probRange(object@target),
    "target has to be a probability range"
  )
  v$check(
    is.probRange(object@overdose),
    "overdose has to be a probability range"
  )
  if (object@unacceptable[1] != 1) {
    v$check(
      is.probRange(object@unacceptable),
      "unacceptable has to be a probability range"
    )
  }

#  v$check(
#   is.probability(object@maxOverdoseProb),
#    "maxOverdoseProb has to be a probability"
#  )

 # v$check(
 #   all(!(object@losses < 0)),
 #   "losses has to be a vector of non-negative elements"
 # )

  v$result()
}
