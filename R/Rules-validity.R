# NextBest ----

#' Internal Helper Functions for Validation of [`NextBest`] Objects
#'
#' @description `r lifecycle::badge("stable")`
#'
#' These functions are only used internally to validate the format of an input
#' [`NextBest`] or inherited classes and therefore not exported.
#'
#' @name v_next_best
#' @param object (`NextBest`)\cr object to validate.
#' @return A `character` vector with the validation failure messages,
#'   or `TRUE` in case validation passes.
NULL

#' @describeIn v_next_best validates that the [`NextBestMTD`] object
#'   contains valid `target` probability and `derive` function.
v_next_best_mtd <- function(object) {
  v <- Validate()
  v$check(
    test_probability(object@target, bounds_closed = FALSE),
    "target must be a probability value from (0, 1) interval"
  )
  v$check(
    formalArgs(object@derive) == "mtd_samples",
    "derive must have a single argument 'mtd_samples'"
  )
  v$result()
}

#' @describeIn v_next_best validates that the [`NextBestNCRM`] object
#'   contains valid `target` probability, `overdose` and `max_overdose_prob`
#'   probability ranges.
v_next_best_ncrm <- function(object) {
  v <- Validate()
  v$check(
    test_probability_range(object@target),
    "target has to be a probability range"
  )
  v$check(
    test_probability_range(object@overdose),
    "overdose has to be a probability range"
  )
  v$check(
    test_probability(object@max_overdose_prob, bounds_closed = FALSE),
    "max_overdose_prob must be a probability value from (0, 1) interval"
  )
  v$result()
}

#' @describeIn v_next_best validates that the [`NextBestNCRMLoss`] object
#'   contains valid objects.
v_next_best_ncrm_loss <- function(object) {
  v <- Validate()
  v$check(
    test_probability_range(object@target_int, bounds_closed = FALSE),
    "target_int has to be a probability range excluding 0 and 1"
  )
  is_overdose_int_valid <- test_probability_range(
    object@overdose_int,
    bounds_closed = c(FALSE, TRUE)
  )
  v$check(
    is_overdose_int_valid,
    "overdose_int has to be a probability range excluding 0"
  )
  is_unacceptable_int_valid <- test_probability_range(
    object@unacceptable_int,
    bounds_closed = c(FALSE, TRUE)
  )
  v$check(
    is_unacceptable_int_valid,
    "unacceptable_int has to be a probability range excluding 0"
  )
  if (is_overdose_int_valid && is_unacceptable_int_valid) {
    v$check(
      object@overdose_int[2] <= object@unacceptable_int[1],
      "lower bound of unacceptable_int has to be >= than upper bound of overdose_int"
    )
  }
  v$check(
    test_probability(object@max_overdose_prob, bounds_closed = FALSE),
    "max_overdose_prob must be a probability value from (0, 1) interval"
  )
  if (is_unacceptable_int_valid) {
    losses_len <- ifelse(all(object@unacceptable_int == c(1, 1)), 3L, 4L)
    v$check(
      test_numeric(object@losses, lower = 0, finite = TRUE, any.missing = FALSE, len = losses_len),
      "losses must be a vector of non-negative numbers of length 3 if unacceptable_int is c(1, 1), otherwise 4"
    )
  }
  v$result()
}

#' @describeIn v_next_best validates that the [`NextBestDualEndpoint`] object
#'   contains valid probability objects.
v_next_best_dual_endpoint <- function(object) {
  v <- Validate()
  v$check(
    test_flag(object@target_relative),
    "target_relative must be a flag"
  )
  if (isTRUE(object@target_relative)) {
    v$check(
      test_probability_range(object@target),
      "target has to be a probability range when target_relative is TRUE"
    )
  } else {
    v$check(
      test_numeric(object@target, any.missing = FALSE, len = 2, unique = TRUE, sorted = TRUE),
      "target must be a numeric range"
    )
  }
  v$check(
    test_probability_range(object@overdose),
    "overdose has to be a probability range"
  )
  v$check(
    test_probability(object@max_overdose_prob, bounds_closed = FALSE),
    "max_overdose_prob must be a probability value from (0, 1) interval"
  )
  v$check(
    test_probability(object@target_thresh),
    "target_thresh must be a probability value from [0, 1] interval"
  )
  v$result()
}

#' @describeIn v_next_best validates that the [`NextBestMinDist`] object
#'   contains valid `target` object.
v_next_best_min_dist <- function(object) {
  v <- Validate()
  v$check(
    test_probability(object@target, bounds_closed = FALSE),
    "target must be a probability value from (0, 1) interval"
  )
  v$result()
}

#' @describeIn v_next_best validates that the [`NextBestInfTheory`] object
#'   contains valid `target` and `asymmetry` objects.
v_next_best_inf_theory <- function(object) {
  v <- Validate()
  v$check(
    test_probability(object@target, bounds_closed = FALSE),
    "target must be a probability value from (0, 1) interval"
  )
  v$check(
    test_number(object@asymmetry, finite = TRUE) && object@asymmetry > 0 && object@asymmetry < 2,
    "asymmetry must be a number from (0, 2) interval"
  )
  v$result()
}

# Increments ----

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

#' @describeIn v_increments validates that the [`IncrementsHSRBeta`]
#'  object contains valid probability target, threshold and shape parameters.
v_increments_hsr_beta <- function(object) {
  v <- Validate()
  v$check(
    test_probability(object@target, bounds_closed = FALSE),
    "target must be a probability"
  )
  v$check(
    test_probability(object@prob, bounds_closed = FALSE),
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

# Stopping ----

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

#' @describeIn v_stopping validates that the [`StoppingLowestDoseHSRBeta`]
#'  object contains valid probability target, threshold and shape parameters.
v_stopping_lowest_dose_hsr_beta <- v_increments_hsr_beta

#' @describeIn v_stopping validates that the [`StoppingMTDCV`] object
#'   contains valid probability target and percentage threshold.
v_stopping_mtd_cv <- function(object) {
  v <- Validate()
  v$check(
    test_probability(object@target, bounds_closed = FALSE),
    "target must be probability value from (0, 1) interval"
  )
  v$check(
    test_probability(object@thresh_cv / 100, bounds_closed = FALSE),
    "thresh_cv must be percentage > 0"
  )
  v$result()
}
