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

  is_overdose_ok <- test_probability_range(object@overdose, bounds_closed = TRUE)
  v$check(is_overdose_ok, "overdose has to be a probability range")

  is_unacceptable_ok <- test_probability_range(object@unacceptable, bounds_closed = TRUE)
  v$check(is_unacceptable_ok, "unacceptable has to be a probability range")

  if (is_overdose_ok && is_unacceptable_ok) {
    v$check(
      object@overdose[2] <= object@unacceptable[1],
      "lower bound of unacceptable has to be >= than upper bound of overdose"
    )
  }
  if (is_unacceptable_ok) {
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
      test_range(object@target),
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

#' @describeIn v_next_best validates that the [`NextBestProbMTDLTE`] object
#'   contains valid `target` probability and `method` string value.
v_next_best_prob_mtd_lte <- function(object) {
  v <- Validate()
  v$check(
    test_probability(object@target, bounds_closed = FALSE),
    "target must be a probability value from (0, 1) interval"
  )
  v$result()
}

#' @describeIn v_next_best validates that the [`NextBestProbMTDMinDist`] object
#'   contains valid `target` probability and `method` string value.
v_next_best_prob_mtd_min_dist <- function(object) {
  v <- Validate()
  v$check(
    test_probability(object@target, bounds_closed = FALSE),
    "target must be a probability value from (0, 1) interval"
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
    test_numeric(object@increments, finite = TRUE, any.missing = FALSE, len = length(object@intervals)),
    "increments has to be a numerical vector of the same length as `intervals` with finite values"
  )
  v$result()
}

#' @describeIn v_increments validates that the [`IncrementsRelativeParts`] object
#'   contains valid `dlt_start` and `clean_start` parameters.
v_increments_relative_parts <- function(object) {
  v <- Validate()
  is_dlt_start_ok <- test_int(object@dlt_start)
  v$check(is_dlt_start_ok, "dlt_start must be an integer number")
  if (is_dlt_start_ok) {
    v$check(
      test_int(object@clean_start, lower = object@dlt_start),
      "clean_start must be an integer number and it must be >= dlt_start"
    )
  }
  v$result()
}

#' @describeIn v_increments validates that the [`IncrementsRelativeDLT`] object
#'   contains valid `intervals` and `increments` parameters.
v_increments_relative_dlt <- function(object) {
  v <- Validate()
  v$check(
    test_integer(object@intervals, lower = 0, any.missing = FALSE, unique = TRUE, sorted = TRUE),
    "intervals has to be an integer vector with unique, finite, non-negative and sorted non-missing values"
  )
  v$check(
    test_numeric(object@increments, finite = TRUE, any.missing = FALSE, len = length(object@intervals)),
    "increments has to be a numerical vector of the same length as `intervals` with finite values"
  )
  v$result()
}

#' @describeIn v_increments validates that the [`IncrementsDoseLevels`] object
#'   contains valid `levels` and `basis_level` option.
v_increments_dose_levels <- function(object) {
  v <- Validate()
  v$check(
    test_int(object@levels, lower = .Machine$double.xmin),
    "levels must be scalar positive integer"
  )
  v$check(
    test_string(object@basis_level, pattern = "^last$|^max$"),
    "basis_level must be either 'last' or 'max'"
  )
  v$result()
}

#' @describeIn v_increments validates that the [`IncrementsHSRBeta`]
#'   object contains valid probability target, threshold and shape parameters.
v_increments_hsr_beta <- function(object) {
  v <- Validate()
  v$check(
    test_probability(object@target, bounds_closed = FALSE),
    "target must be a probability value from (0, 1) interval"
  )
  v$check(
    test_probability(object@prob, bounds_closed = FALSE),
    "prob must be a probability value from (0, 1) interval"
  )
  v$check(
    test_number(object@a, lower = .Machine$double.xmin, finite = TRUE),
    "Beta distribution shape parameter a must be a positive scalar"
  )
  v$check(
    test_number(object@b, lower = .Machine$double.xmin, finite = TRUE),
    "Beta distribution shape parameter b must be a positive scalar"
  )
  v$result()
}

#' @describeIn v_increments validates that the [`IncrementsMin`]
#'   object contains a list with `Increments` objects.
v_increments_min <- function(object) {
  v <- Validate()
  v$check(
    all(sapply(object@increments_list, test_class, "Increments")),
    "all elements in increments_list must be of Increments class"
  )
  v$result()
}

#' @describeIn v_increments validates the [`IncrementsMaxToxProb`]
v_increments_maxtoxprob <- function(object) {
  v <- Validate()
  v$check(
    test_probabilities(object@prob),
    "prob must be a vector of probabilities with minimum length 1 and no missing values"
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

#' @describeIn v_stopping validates that the [`StoppingCohortsNearDose`]
#'   object contains valid `nCohorts` and `percentage` parameters.
v_stopping_cohorts_near_dose <- function(object) {
  v <- Validate()
  v$check(
    test_int(object@nCohorts, lower = .Machine$double.xmin),
    "nCohorts must be positive integer scalar"
  )
  v$check(
    test_probability(object@percentage / 100),
    "percentage must be a number between 0 and 100"
  )
  v$result()
}

#' @describeIn v_stopping validates that the [`StoppingPatientsNearDose`]
#'   object contains valid `nPatients` and `percentage` parameters.
v_stopping_patients_near_dose <- function(object) {
  v <- Validate()
  v$check(
    test_int(object@nPatients, lower = .Machine$double.xmin),
    "nPatients must be positive integer scalar"
  )
  v$check(
    test_probability(object@percentage / 100),
    "percentage must be a number between 0 and 100"
  )
  v$result()
}

#' @describeIn v_stopping validates that the [`StoppingMinCohorts`]
#'   object contains valid `nCohorts` parameter.
v_stopping_min_cohorts <- function(object) {
  v <- Validate()
  v$check(
    test_int(object@nCohorts, lower = .Machine$double.xmin),
    "nCohorts must be positive integer scalar"
  )
  v$result()
}

#' @describeIn v_stopping validates that the [`StoppingMinPatients`]
#'   object contains valid `nPatients` parameter.
v_stopping_min_patients <- function(object) {
  v <- Validate()
  v$check(
    test_int(object@nPatients, lower = .Machine$double.xmin),
    "nPatients must be positive integer scalar"
  )
  v$result()
}

#' @describeIn v_stopping validates that the [`StoppingTargetProb`]
#'   object contains valid `target` and `prob` parameters.
v_stopping_target_prob <- function(object) {
  v <- Validate()
  v$check(
    test_probability_range(object@target),
    "target has to be a probability range"
  )
  v$check(
    test_probability(object@prob, bounds_closed = FALSE),
    "prob must be a probability value from (0, 1) interval"
  )
  v$result()
}

#' @describeIn v_stopping validates that the [`StoppingMTDdistribution`]
#'   object contains valid `target`, `thresh` and `prob` parameters.
v_stopping_mtd_distribution <- function(object) {
  v <- Validate()
  v$check(
    test_probability(object@target, bounds_closed = FALSE),
    "target must be a probability value from (0, 1) interval"
  )
  v$check(
    test_probability(object@thresh, bounds_closed = FALSE),
    "thresh must be a probability value from (0, 1) interval"
  )
  v$check(
    test_probability(object@prob, bounds_closed = FALSE),
    "prob must be a probability value from (0, 1) interval"
  )
  v$result()
}

#' @describeIn v_stopping validates that the [`StoppingMTDCV`] object
#'   contains valid probability target and percentage threshold.
v_stopping_mtd_cv <- function(object) {
  v <- Validate()
  v$check(
    test_probability(object@target, bounds_closed = FALSE),
    "target must be probability value from (0, 1) interval"
  )
  v$check(
    test_probability(object@thresh_cv / 100, bounds_closed = c(FALSE, TRUE)),
    "thresh_cv must be percentage > 0"
  )
  v$result()
}

#' @describeIn v_stopping validates that the [`StoppingTargetBiomarker`] object
#'   contains valid `target`, `is_relative` and `prob`slots.
v_stopping_target_biomarker <- function(object) {
  v <- Validate()
  v$check(
    test_flag(object@is_relative),
    "is_relative must be a flag"
  )
  if (isTRUE(object@is_relative)) {
    v$check(
      test_probability_range(object@target),
      "target has to be a probability range when is_relative flag is 'TRUE'"
    )
  } else {
    v$check(
      test_range(object@target, finite = TRUE),
      "target must be a numeric range"
    )
  }
  v$check(
    test_probability(object@prob, bounds_closed = FALSE),
    "prob must be a probability value from (0, 1) interval"
  )
  v$result()
}

#' @describeIn v_stopping validates that the [`StoppingList`] object
#'   contains valid `stop_list`, `summary` slots.
v_stopping_list <- function(object) {
  v <- Validate()
  v$check(
    all(sapply(object@stop_list, test_class, "Stopping")),
    "every stop_list element must be of class 'Stopping'"
  )
  is_summary_ok <- test_function(object@summary, nargs = 1)
  v$check(
    is_summary_ok,
    "summary must be a function that accepts a single argument, without ..."
  )
  if (is_summary_ok) {
    summary_res <- object@summary(
      rep(c(TRUE, FALSE), length.out = length(object@stop_list))
    )
    v$check(
      test_flag(summary_res),
      "summary must accept a logical vector of the same length as 'stop_list' and return a boolean value"
    )
  }
  v$result()
}

#' @describeIn v_stopping validates that the [`StoppingAll`] object
#'   contains valid `stop_list` slot.
v_stopping_all <- function(object) {
  v <- Validate()
  v$check(
    all(sapply(object@stop_list, test_class, "Stopping")),
    "every stop_list element must be of class 'Stopping'"
  )
  v$result()
}

#' @describeIn v_stopping validates that the [`StoppingTDCIRatio`] object
#'   contains valid `target_ratio` and  `prob_target` slots.
v_stopping_tdci_ratio <- function(object) {
  v <- Validate()
  v$check(
    test_number(object@target_ratio, lower = .Machine$double.xmin, finite = TRUE),
    "target_ratio must be a positive number"
  )
  v$check(
    test_probability(object@prob_target),
    "prob_target must be a probability value from [0, 1] interval"
  )
  v$result()
}

# CohortSize ----

#' Internal Helper Functions for Validation of [`CohortSize`] Objects
#'
#' @description `r lifecycle::badge("stable")`
#'
#' These functions are only used internally to validate the format of an input
#' [`CohortSize`] or inherited classes and therefore not exported.
#'
#' @name v_cohort_size
#' @param object (`CohortSize`)\cr object to validate.
#' @return A `character` vector with the validation failure messages,
#'   or `TRUE` in case validation passes.
NULL

#' @describeIn v_cohort_size validates that the [`CohortSizeRange`] object
#'   contains valid `intervals` and  `cohort_size` slots.
v_cohort_size_range <- function(object) {
  v <- Validate()
  v$check(
    test_numeric(
      object@intervals,
      lower = 0, finite = TRUE, any.missing = FALSE, min.len = 1, unique = TRUE, sorted = TRUE
    ),
    "intervals must be a numeric vector with non-negative, sorted (asc.) and unique values"
  )
  v$check(
    test_integer(
      object@cohort_size,
      lower = 0, any.missing = FALSE, len = length(object@intervals)
    ),
    "cohort_size must be an integer vector of the same length as intervals, containing non-negative values only"
  )
  v$result()
}

#' @describeIn v_cohort_size validates that the [`CohortSizeDLT`] object
#'   contains valid `intervals` and  `cohort_size` slots.
v_cohort_size_dlt <- function(object) {
  v <- Validate()
  v$check(
    test_integer(
      object@intervals,
      lower = 0, any.missing = FALSE, min.len = 1, unique = TRUE, sorted = TRUE
    ),
    "intervals must be an integer vector with non-negative, sorted (asc.) and unique values"
  )
  v$check(
    test_integer(
      object@cohort_size,
      lower = 0, any.missing = FALSE, len = length(object@intervals)
    ),
    "cohort_size must be an integer vector of the same length as intervals, containing non-negative values only"
  )
  v$result()
}

#' @describeIn v_cohort_size validates that the [`CohortSizeConst`] object
#'   contains valid `size` slot.
v_cohort_size_const <- function(object) {
  v <- Validate()
  v$check(
    test_int(object@size, lower = 0),
    "size needs to be a non-negative scalar"
  )
  v$result()
}

#' @describeIn v_cohort_size validates that the [`CohortSizeParts`] object
#'   contains valid `sizes` slot.
v_cohort_size_parts <- function(object) {
  v <- Validate()
  v$check(
    test_integer(object@cohort_sizes, lower = .Machine$double.xmin, any.missing = FALSE, len = 2),
    "cohort_sizes needs to be an integer vector of length 2 with all elements positive"
  )
  v$result()
}

#' @describeIn v_cohort_size validates that the [`CohortSizeMax`] object
#'   contains valid `cohort_sizes` slot.
v_cohort_size_max <- function(object) {
  v <- Validate()
  v$check(
    test_list(object@cohort_sizes, types = "CohortSize", any.missing = FALSE, min.len = 2, unique = TRUE),
    "cohort_sizes must be a list of CohortSize (unique) objects only and be of length >= 2"
  )
  v$result()
}

# SafetyWindow ----

#' Internal Helper Functions for Validation of [`SafetyWindow`] Objects
#'
#' @description `r lifecycle::badge("stable")`
#'
#' These functions are only used internally to validate the format of an input
#' [`SafetyWindow`] or inherited classes and therefore not exported.
#'
#' @name v_safety_window
#' @param object (`SafetyWindow`)\cr object to validate.
#' @return A `character` vector with the validation failure messages,
#'   or `TRUE` in case validation passes.
NULL

#' @describeIn v_safety_window validates that the [`SafetyWindowSize`] object
#'   contains valid slots.
v_safety_window_size <- function(object) {
  v <- Validate()
  v$check(
    test_list(object@gap, types = "integer", any.missing = FALSE, min.len = 1),
    "gap must be a list of length >= 1 with integer vectors only"
  )
  v$check(
    all(sapply(object@gap, test_integer, lower = 0, any.missing = FALSE, min.len = 1)),
    "every element in gap list has to be an integer vector with non-negative and non-missing values"
  )
  pg_len <- length(object@gap)
  v$check(
    test_integer(
      object@size,
      lower = .Machine$double.xmin, any.missing = FALSE, len = pg_len, unique = TRUE, sorted = TRUE
    ),
    "size has to be an integer vector, of the same length as gap, with positive, unique and sorted non-missing values"
  )
  v$check(
    test_int(object@follow, lower = .Machine$double.xmin),
    "follow has to be a positive integer number"
  )
  v$check(
    test_int(object@follow_min, lower = .Machine$double.xmin),
    "follow_min has to be a positive integer number"
  )
  v$result()
}

#' @describeIn v_safety_window validates that the [`SafetyWindowConst`] object
#'   contains valid slots.
v_safety_window_const <- function(object) {
  v <- Validate()
  v$check(
    test_integer(object@gap, lower = 0, any.missing = FALSE),
    "gap has to be an integer vector with non-negative and non-missing elements"
  )
  v$check(
    test_int(object@follow, lower = .Machine$double.xmin),
    "follow has to be a positive integer number"
  )
  v$check(
    test_int(object@follow_min, lower = .Machine$double.xmin),
    "follow_min has to be a positive integer number"
  )
  v$result()
}

#' @describeIn v_next_best validates that the [`NextBestOrdinal`] object
#'   contains valid `grade` and standard `NextBest` rule.
v_next_best_ordinal <- function(object) {
  v <- Validate()
  v$check(
    test_integer(object@grade, lower = 1),
    "grade must be a positive integer"
  )
  v$check(
    test_class(object@rule, "NextBest"),
    "rule must be a NextBest object"
  )
  v$result()
}

#' @describeIn v_increments validates that the [`IncrementsOrdinal`] object
#'   contains valid `grade` and standard `Increments` rule.
v_increments_ordinal <- function(object) {
  v <- Validate()
  v$check(
    test_integer(object@grade, lower = 1),
    "grade must be a positive integer"
  )
  v$check(
    test_class(object@rule, "Increments"),
    "rule must be a Increments object"
  )
  v$result()
}

#' @describeIn v_increments validates that the [`CohortSizeOrdinal`] object
#'   contains valid `grade` and standard `CohortSize` rule.
v_cohort_size_ordinal <- function(object) {
  v <- Validate()
  v$check(
    test_integer(object@grade, lower = 1),
    "grade must be a positive integer"
  )
  v$check(
    test_class(object@rule, "CohortSize"),
    "rule must be a CohortSize object"
  )
  v$result()
}
