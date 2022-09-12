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
    test_function(object@derive, nargs = 1),
    "derive must have a single argument"
  )
  v$check(
    test_number(object@derive(1:5)),
    "derive must accept numerical vector as an argument and return a number"
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
    test_probability_range(object@target, bounds_closed = FALSE),
    "target has to be a probability range excluding 0 and 1"
  )
  is_overdose_valid <- test_probability_range(
    object@overdose,
    bounds_closed = c(FALSE, TRUE)
  )
  v$check(
    is_overdose_valid,
    "overdose has to be a probability range excluding 0"
  )
  is_unacceptable_valid <- test_probability_range(
    object@unacceptable,
    bounds_closed = c(FALSE, TRUE)
  )
  v$check(
    is_unacceptable_valid,
    "unacceptable has to be a probability range excluding 0"
  )
  if (is_overdose_valid && is_unacceptable_valid) {
    v$check(
      object@overdose[2] <= object@unacceptable[1],
      "lower bound of unacceptable has to be >= than upper bound of overdose"
    )
  }
  v$check(
    test_probability(object@max_overdose_prob, bounds_closed = FALSE),
    "max_overdose_prob must be a probability value from (0, 1) interval"
  )
  if (is_unacceptable_valid) {
    losses_len <- ifelse(all(object@unacceptable == c(1, 1)), 3L, 4L)
    v$check(
      test_numeric(object@losses, lower = 0, finite = TRUE, any.missing = FALSE, len = losses_len),
      "losses must be a vector of non-negative numbers of length 3 if unacceptable is c(1, 1), otherwise 4"
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
    test_number(object@asymmetry, finite = TRUE) && h_in_range(object@asymmetry, c(0, 2), FALSE),
    "asymmetry must be a number from (0, 2) interval"
  )
  v$result()
}

#' @describeIn v_next_best validates that the [`NextBestTD`] object
#'   contains valid `prob_target_drt` and `prob_target_eot` probabilities.
v_next_best_td <- function(object) {
  v <- Validate()
  v$check(
    test_probability(object@prob_target_drt, bounds_closed = FALSE),
    "prob_target_drt must be a probability value from (0, 1) interval"
  )
  v$check(
    test_probability(object@prob_target_eot, bounds_closed = FALSE),
    "prob_target_eot must be a probability value from (0, 1) interval"
  )
  v$result()
}

#' @describeIn v_next_best validates that the [`NextBestTDsamples`] object
#'   contains valid `derive` function.
v_next_best_td_samples <- function(object) {
  v <- Validate()
  v$check(
    test_function(object@derive, nargs = 1),
    "derive must have a single argument"
  )
  v$check(
    test_number(object@derive(1:5)),
    "derive must accept numerical vector as an argument and return a number"
  )
  v$result()
}

#' @describeIn v_next_best validates that the [`NextBestMaxGainSamples`] object
#'   contains valid `derive` and `mg_derive` functions.
v_next_best_max_gain_samples <- function(object) {
  v <- Validate()
  v$check(
    test_function(object@derive, nargs = 1),
    "derive must have a single argument"
  )
  v$check(
    test_number(object@derive(1:5)),
    "derive must accept numerical vector as an argument and return a number"
  )
  v$check(
    test_function(object@mg_derive, nargs = 1),
    "mg_derive must have a single argument"
  )
  v$check(
    test_number(object@mg_derive(1:5)),
    "mg_derive must accept numerical vector as an argument and return a number"
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

#' @describeIn v_increments validates that the [`IncrementsRelative`] object
#'   contains valid `intervals` and `increments` parameters.
v_increments_relative <- function(object) {
  v <- Validate()
  v$check(
    test_numeric(
      object@intervals,
      lower = 0, finite = TRUE, any.missing = FALSE, unique = TRUE, sorted = TRUE
    ),
    "intervals has to be a numerical vector with unique, finite, non-negative and sorted non-missing values"
  )
  v$check(
    test_numeric(
      object@increments,
      finite = TRUE, any.missing = FALSE, len = length(object@intervals), unique = TRUE
    ),
    "increments has to be a numerical vector of the same length as `intervals` with finite values"
  )
  v$result()
}

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
