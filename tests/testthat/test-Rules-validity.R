# NextBest ----

## v_next_best_mtd ----

test_that("v_next_best_mtd passes for valid object", {
  object <- h_next_best_mtd()
  expect_true(v_next_best_mtd(object))
})

test_that("v_next_best_mtd returns message for non-valid target", {
  err_msg <- "target must be a probability value from (0, 1) interval"
  object <- h_next_best_mtd()

  # Changing `target` so that it does not represent allowed probability value.
  object@target <- 1
  expect_equal(v_next_best_mtd(object), err_msg)

  # Changing `target` so that it is not a scalar.
  object@target <- c(0.5, 0.6)
  expect_equal(v_next_best_mtd(object), err_msg)
})

test_that("v_next_best_mtd returns message for non-valid derive", {
  object <- h_next_best_mtd()

  # Changing `derive` so that it has many arguments.
  object@derive <- function(x, y) 1L
  expect_equal(
    v_next_best_mtd(object),
    "derive must have a single argument"
  )

  # Changing `derive` so that it does not return a number.
  object@derive <- function(x) c(1, 2)
  expect_equal(
    v_next_best_mtd(object),
    "derive must accept numerical vector as an argument and return a number"
  )
})

## v_next_best_ncrm ----

test_that("v_next_best_ncrm passes for valid object", {
  object <- h_next_best_ncrm()
  expect_true(v_next_best_ncrm(object))
})

test_that("v_next_best_ncrm returns message for non-valid target", {
  err_msg <- "target has to be a probability range"
  object <- h_next_best_ncrm()

  # Changing `target` so that it is not an interval.
  object@target <- 0.6
  expect_equal(v_next_best_ncrm(object), err_msg)

  object@target <- c(0.5, 0.6, 0.8)
  expect_equal(v_next_best_ncrm(object), err_msg)

  # Changing `target` so that one bound is not a valid probability value.
  object@target <- c(0.4, 1.2)
  expect_equal(v_next_best_ncrm(object), err_msg)
})

test_that("v_next_best_ncrm returns message for non-valid overdose", {
  err_msg <- "overdose has to be a probability range"
  object <- h_next_best_ncrm()

  # Changing `overdose` so that it is not an interval.
  object@overdose <- 0.6
  expect_equal(v_next_best_ncrm(object), err_msg)

  object@overdose <- c(0.5, 0.6, 0.8)
  expect_equal(v_next_best_ncrm(object), err_msg)

  # Changing `overdose` so that one bound is not a valid probability value.
  object@overdose <- c(0.4, 1.2)
  expect_equal(v_next_best_ncrm(object), err_msg)
})

test_that("v_next_best_ncrm returns message for non-valid max_overdose_prob", {
  err_msg <- "max_overdose_prob must be a probability value from (0, 1) interval"
  object <- h_next_best_ncrm()

  # Changing `max_overdose_prob` so that it does not represent allowed probability value.
  object@max_overdose_prob <- 1
  expect_equal(v_next_best_ncrm(object), err_msg)

  # Changing `max_overdose_prob` so that it is not a scalar.
  object@max_overdose_prob <- c(0.5, 0.6)
  expect_equal(v_next_best_ncrm(object), err_msg)
})

## v_next_best_ncrm_loss ----

test_that("v_next_best_ncrm_loss passes for valid object", {
  object <- h_next_best_ncrm_loss()
  expect_true(v_next_best_ncrm_loss(object))
})

test_that("v_next_best_ncrm_loss returns message for non-valid target", {
  err_msg <- "target has to be a probability range excluding 0 and 1"
  object <- h_next_best_ncrm_loss()

  # Changing `target` so that it is not an interval.
  object@target <- 0.6
  expect_equal(v_next_best_ncrm_loss(object), err_msg)

  object@target <- c(0.5, 0.6, 0.8)
  expect_equal(v_next_best_ncrm_loss(object), err_msg)

  # Changing `target` so that one bound is not a valid probability value.
  object@target <- c(0.4, 1)
  expect_equal(v_next_best_ncrm_loss(object), err_msg)

  object@target <- c(0, 0.9)
  expect_equal(v_next_best_ncrm_loss(object), err_msg)
})

test_that("v_next_best_ncrm_loss returns message for non-valid overdose", {
  err_msg <- "overdose has to be a probability range excluding 0"
  object <- h_next_best_ncrm_loss()

  # Changing `overdose` so that it is not an interval.
  object@overdose <- 0.6
  expect_equal(v_next_best_ncrm_loss(object), err_msg)

  object@overdose <- c(0.5, 0.6, 0.8)
  expect_equal(v_next_best_ncrm_loss(object), err_msg)

  # Changing `overdose` so that one bound is not a valid probability value.
  object@overdose <- c(0, 0.3)
  expect_equal(v_next_best_ncrm_loss(object), err_msg)
})

test_that("v_next_best_ncrm_loss returns message for non-valid unacceptable", {
  err_msg <- "unacceptable has to be a probability range excluding 0"
  object <- h_next_best_ncrm_loss()

  # Changing `unacceptable` so that it is not an interval.
  object@unacceptable <- 0.6
  expect_equal(v_next_best_ncrm_loss(object), err_msg)

  object@unacceptable <- c(0.5, 0.6, 0.8)
  expect_equal(v_next_best_ncrm_loss(object), err_msg)

  # Changing `unacceptable` so that one bound is not a valid probability value.
  object@unacceptable <- c(0, 0.3)
  expect_equal(v_next_best_ncrm_loss(object), err_msg)
})

test_that("v_next_best_ncrm_loss returns message for wrong overdose-unacceptable relation", {
  object <- h_next_best_ncrm_loss()

  # Changing `unacceptable` so that `overdose[2]` > `unacceptable[1]`.
  object@unacceptable <- c(0.34, 0.5)
  expect_equal(
    v_next_best_ncrm_loss(object),
    "lower bound of unacceptable has to be >= than upper bound of overdose"
  )
})

test_that("v_next_best_ncrm_loss returns message for wrong losses", {
  err_msg <- "losses must be a vector of non-negative numbers of length 3 if unacceptable is c(1, 1), otherwise 4"
  object <- h_next_best_ncrm_loss()

  # Changing `losses` so that it contains negative values.
  object@losses <- c(1, 2, -4, 4)
  expect_equal(v_next_best_ncrm_loss(object), err_msg)

  # Changing `losses` so that it is of wrong length.
  object@losses <- c(1, 2, 4)
  expect_equal(v_next_best_ncrm_loss(object), err_msg)

  # Changing `losses` so that it is of wrong length.
  object@unacceptable <- c(1, 1)
  object@losses <- c(1, 2, 4, 6)
  expect_equal(v_next_best_ncrm_loss(object), err_msg)
})

## v_next_best_dual_endpoint ----

test_that("v_next_best_dual_endpoint passes for valid object", {
  object <- h_next_best_dual_endpoint()
  expect_true(v_next_best_dual_endpoint(object))
})

test_that("v_next_best_dual_endpoint returns message for non-valid target (relative)", {
  err_msg <- "target has to be a probability range when target_relative is TRUE"
  object <- h_next_best_dual_endpoint(target_relative = TRUE)

  # Changing `target` so that it is not an interval.
  object@target <- 0.6
  expect_equal(v_next_best_dual_endpoint(object), err_msg)

  object@target <- c(0.5, 0.6, 0.8)
  expect_equal(v_next_best_dual_endpoint(object), err_msg)

  # Changing `target` so that one bound is not a valid probability value.
  object@target <- c(0.4, 1.2)
  expect_equal(v_next_best_dual_endpoint(object), err_msg)
})

test_that("v_next_best_dual_endpoint returns message for non-valid target (absolute)", {
  err_msg <- "target must be a numeric range"
  object <- h_next_best_dual_endpoint(target_relative = FALSE)

  # Changing `target` so that it is not a numeric range.
  object@target <- 0.6
  expect_equal(v_next_best_dual_endpoint(object), err_msg)

  object@target <- c(1, 5, 7)
  expect_equal(v_next_best_dual_endpoint(object), err_msg)
})

test_that("v_next_best_dual_endpoint returns message for non-valid target_relative flag", {
  object <- h_next_best_dual_endpoint()

  # Changing `target_relative` so that it is not a flag.
  object@target_relative <- c(TRUE, FALSE)
  expect_equal(v_next_best_dual_endpoint(object), "target_relative must be a flag")
})

test_that("v_next_best_dual_endpoint returns message for non-valid overdose", {
  err_msg <- "overdose has to be a probability range"
  object <- h_next_best_dual_endpoint()

  # Changing `overdose` so that it is not an interval.
  object@overdose <- 0.6
  expect_equal(v_next_best_dual_endpoint(object), err_msg)

  object@overdose <- c(0.5, 0.6, 0.8)
  expect_equal(v_next_best_dual_endpoint(object), err_msg)

  # Changing `overdose` so that one bound is not a valid probability value.
  object@overdose <- c(0.4, 1.2)
  expect_equal(v_next_best_dual_endpoint(object), err_msg)
})

test_that("v_next_best_dual_endpoint returns message for non-valid max_overdose_prob", {
  err_msg <- "max_overdose_prob must be a probability value from (0, 1) interval"
  object <- h_next_best_dual_endpoint()

  # Changing `max_overdose_prob` so that it does not represent allowed probability value.
  object@max_overdose_prob <- 1
  expect_equal(v_next_best_dual_endpoint(object), err_msg)

  # Changing `max_overdose_prob` so that it is not a scalar.
  object@max_overdose_prob <- c(0.5, 0.6)
  expect_equal(v_next_best_dual_endpoint(object), err_msg)
})

test_that("v_next_best_dual_endpoint returns message for non-valid target_thresh", {
  err_msg <- "target_thresh must be a probability value from [0, 1] interval"
  object <- h_next_best_dual_endpoint()

  # Changing `target_thresh` so that it does not represent allowed probability value.
  object@target_thresh <- 2
  expect_equal(v_next_best_dual_endpoint(object), err_msg)

  # Changing `target_thresh` so that it is not a scalar.
  object@target_thresh <- c(0.5, 0.6)
  expect_equal(v_next_best_dual_endpoint(object), err_msg)
})

## v_next_best_min_dist ----

test_that("v_next_best_min_dist passes for valid object", {
  object <- NextBestMinDist(target = 0.3)
  expect_true(v_next_best_min_dist(object))
})

test_that("v_next_best_min_dist returns message for non-valid target", {
  err_msg <- "target must be a probability value from (0, 1) interval"
  object <- NextBestMinDist(target = 0.3)

  # Changing `target` so that it does not represent allowed probability value.
  object@target <- 1
  expect_equal(v_next_best_min_dist(object), err_msg)

  # Changing `target` so that it is not a scalar.
  object@target <- c(0.5, 0.6)
  expect_equal(v_next_best_min_dist(object), err_msg)
})

## v_next_best_inf_theory ----

test_that("v_next_best_inf_theory passes for valid object", {
  object <- NextBestInfTheory(target = 0.4, asymmetry = 1.5)
  expect_true(v_next_best_inf_theory(object))
})

test_that("v_next_best_inf_theory returns message for non-valid target", {
  err_msg <- "target must be a probability value from (0, 1) interval"
  object <- NextBestInfTheory(target = 0.4, asymmetry = 1.5)

  # Changing `target` so that it does not represent allowed probability value.
  object@target <- 1
  expect_equal(v_next_best_inf_theory(object), err_msg)

  # Changing `target` so that it is not a scalar.
  object@target <- c(0.5, 0.6)
  expect_equal(v_next_best_inf_theory(object), err_msg)
})

test_that("v_next_best_inf_theory returns message for non-valid asymmetry", {
  err_msg <- "asymmetry must be a number from (0, 2) interval"
  object <- NextBestInfTheory(target = 0.4, asymmetry = 1.5)

  # Changing `asymmetry` so that it is outside of (0, 2).
  object@asymmetry <- 5
  expect_equal(v_next_best_inf_theory(object), err_msg)

  # Changing `asymmetry` so that it is not a scalar.
  object@asymmetry <- c(1, 1.8)
  expect_equal(v_next_best_inf_theory(object), err_msg)
})

## v_next_best_td ----

test_that("v_next_best_td passes for valid object", {
  object <- NextBestTD(0.4, 0.35)
  expect_true(v_next_best_td(object))
})

test_that("v_next_best_td returns message for non-valid prob_target_drt", {
  err_msg <- "prob_target_drt must be a probability value from (0, 1) interval"
  object <- NextBestTD(0.4, 0.35)

  # Changing `prob_target_drt` so that it does not represent allowed probability value.
  object@prob_target_drt <- 1
  expect_equal(v_next_best_td(object), err_msg)

  # Changing `prob_target_drt` so that it is not a scalar.
  object@prob_target_drt <- c(0.5, 0.6)
  expect_equal(v_next_best_td(object), err_msg)
})

test_that("v_next_best_td returns message for non-valid prob_target_eot", {
  err_msg <- "prob_target_eot must be a probability value from (0, 1) interval"
  object <- NextBestTD(0.4, 0.35)

  # Changing `prob_target_eot` so that it does not represent allowed probability value.
  object@prob_target_eot <- 1
  expect_equal(v_next_best_td(object), err_msg)

  # Changing `prob_target_eot` so that it is not a scalar.
  object@prob_target_eot <- c(0.5, 0.6)
  expect_equal(v_next_best_td(object), err_msg)
})

## v_next_best_td_samples ----

test_that("v_next_best_td_samples passes for valid object", {
  object <- h_next_best_tdsamples()
  expect_true(v_next_best_td_samples(object))
})

test_that("v_next_best_td_samples returns message for non-valid derive", {
  object <- h_next_best_tdsamples()

  # Changing `derive` so that it has many arguments.
  object@derive <- function(x, y) 1L
  expect_equal(
    v_next_best_td_samples(object),
    "derive must have a single argument"
  )

  # Changing `derive` so that it does not return a number.
  object@derive <- function(x) c(1, 2)
  expect_equal(
    v_next_best_td_samples(object),
    "derive must accept numerical vector as an argument and return a number"
  )
})

## v_next_best_max_gain_samples ----

test_that("v_next_best_max_gain_samples passes for valid object", {
  object <- h_next_best_mgsamples()
  expect_true(v_next_best_max_gain_samples(object))
})

test_that("v_next_best_max_gain_samples returns message for non-valid derive", {
  object <- h_next_best_mgsamples()

  # Changing `derive` so that it has many arguments.
  object@derive <- function(x, y) 1L
  expect_equal(
    v_next_best_max_gain_samples(object),
    "derive must have a single argument"
  )

  # Changing `derive` so that it does not return a number.
  object@derive <- function(x) c(1, 2)
  expect_equal(
    v_next_best_max_gain_samples(object),
    "derive must accept numerical vector as an argument and return a number"
  )
})

test_that("v_next_best_max_gain_samples returns message for non-valid mg_derive", {
  object <- h_next_best_mgsamples()

  # Changing `mg_derive` so that it has many arguments.
  object@mg_derive <- function(x, y) 1L
  expect_equal(
    v_next_best_max_gain_samples(object),
    "mg_derive must have a single argument"
  )

  # Changing `mg_derive` so that it does not return a number.
  object@mg_derive <- function(x) c(1, 2)
  expect_equal(
    v_next_best_max_gain_samples(object),
    "mg_derive must accept numerical vector as an argument and return a number"
  )
})

# Increments ----

## v_increments_relative ----

test_that("v_increments_relative passes for valid object", {
  object <- IncrementsRelative(intervals = c(0, 2), increments = c(2, 1))
  expect_true(v_increments_relative(object))
})

test_that("v_increments_relative returns message for non-valid intervals", {
  err_msg <- "intervals has to be a numerical vector with unique, finite, non-negative and sorted non-missing values"
  object <- IncrementsRelative(intervals = c(0, 2, 3), increments = c(2, 1, 1.5))

  # Changing `intervals` so that it contains non-unique values.
  object@intervals <- c(1, 2, 2)
  expect_equal(v_increments_relative(object), err_msg)

  # Changing `intervals` so that it contains non-sorted values.
  object@intervals <- c(1, 3, 2)
  expect_equal(v_increments_relative(object), err_msg)

  # Changing `intervals` so that it contains missing, or infinite negative values.
  object@intervals <- c(-1, NA, 2, Inf)
  object@increments <- 1:4
  expect_equal(v_increments_relative(object), err_msg)
})

test_that("v_increments_relative returns message for non-valid increments", {
  err_msg <- "increments has to be a numerical vector of the same length as `intervals` with finite values"
  object <- IncrementsRelative(intervals = c(0, 2, 3), increments = c(2, 1, 1.5))

  # Changing `increments` so that it is of a length different than the length of `intervals`.
  object@increments <- c(1, 2, 3, 4)
  expect_equal(v_increments_relative(object), err_msg)

  # Changing `increments` so that it contains missing, or infinite values.
  object@increments <- c(NA, 2, Inf)
  expect_equal(v_increments_relative(object), err_msg)
})

## v_increments_relative_parts ----

test_that("v_increments_relative_parts passes for valid object", {
  object <- IncrementsRelativeParts(dlt_start = -1L, clean_start = 3L)
  expect_true(v_increments_relative_parts(object))
})

test_that("v_increments_relative_parts returns message for non-valid dlt_start", {
  err_msg <- "dlt_start must be an integer number"
  object <- IncrementsRelativeParts(dlt_start = -1L, clean_start = 3L)

  # Changing `dlt_start` so that it not a scalar.
  object@dlt_start <- c(1L, 2L)
  expect_equal(v_increments_relative_parts(object), err_msg)

  # Changing `dlt_start` so that it is a missing object.
  object@dlt_start <- NA_integer_
  expect_equal(v_increments_relative_parts(object), err_msg)
})

test_that("v_increments_relative_parts returns message for non-valid clean_start", {
  err_msg <- "clean_start must be an integer number and it must be >= dlt_start"
  object <- IncrementsRelativeParts(dlt_start = -1L, clean_start = 1L)

  # Changing `clean_start` so that it not a scalar.
  object@clean_start <- c(1L, 2L)
  expect_equal(v_increments_relative_parts(object), err_msg)

  # Changing `clean_start` so that it is a missing object.
  object@clean_start <- NA_integer_
  expect_equal(v_increments_relative_parts(object), err_msg)

  # Changing `clean_start` so that it is less than `dlt_start`.
  object@clean_start <- -2L
  expect_equal(v_increments_relative_parts(object), err_msg)
})

## v_increments_relative_dlt ----

test_that("v_increments_relative_dlt passes for valid object", {
  object <- IncrementsRelativeDLT(dlt_intervals = c(0, 2), increments = c(2, 1))
  expect_true(v_increments_relative_dlt(object))
})

test_that("v_increments_relative_dlt returns message for non-valid intervals", {
  err_msg <- "dlt_intervals has to be an integer vector with unique, finite, non-negative and sorted non-missing values"
  object <- IncrementsRelativeDLT(dlt_intervals = c(0, 2, 3), increments = c(2, 1, 1.5))

  # Changing `dlt_intervals` so that it contains non-unique values.
  object@dlt_intervals <- c(1L, 2L, 2L)
  expect_equal(v_increments_relative_dlt(object), err_msg)

  # Changing `dlt_intervals` so that it contains non-sorted values.
  object@dlt_intervals <- c(1L, 3L, 2L)
  expect_equal(v_increments_relative_dlt(object), err_msg)

  # Changing `dlt_intervals` so that it contains missing, or negative values.
  object@dlt_intervals <- c(-1L, NA_integer_, 2L)
  expect_equal(v_increments_relative_dlt(object), err_msg)
})

test_that("v_increments_relative_dlt returns message for non-valid increments", {
  err_msg <- "increments has to be a numerical vector of the same length as `dlt_intervals` with finite values"
  object <- IncrementsRelativeDLT(dlt_intervals = c(0, 2, 3), increments = c(2, 1, 1.5))

  # Changing `increments` so that it is of a length different than the length of `dlt_intervals`.
  object@increments <- c(1, 2, 3, 4)
  expect_equal(v_increments_relative_dlt(object), err_msg)

  # Changing `increments` so that it contains missing, or infinite values.
  object@increments <- c(NA, 2, Inf)
  expect_equal(v_increments_relative_dlt(object), err_msg)
})

## v_increments_num_dose_levels ----

test_that("v_increments_num_dose_levels passes for valid object", {
  object <- IncrementsNumDoseLevels()
  expect_true(v_increments_num_dose_levels(object))

  object <- IncrementsNumDoseLevels(max_levels = 1, basis_level = "last")
  expect_true(v_increments_num_dose_levels(object))

  object <- IncrementsNumDoseLevels(max_levels = 2, basis_level = "max")
  expect_true(v_increments_num_dose_levels(object))
})

test_that("v_increments_num_dose_levels returns message for non-valid max_levels", {
  err_msg <- "max_levels must be scalar positive integer"
  object <- IncrementsNumDoseLevels()

  # Changing `max_levels` so that it not a scalar.
  object@max_levels <- c(1L, 2L)
  expect_equal(v_increments_num_dose_levels(object), err_msg)

  # Changing `max_levels` so that it is a missing object.
  object@max_levels <- NA_integer_
  expect_equal(v_increments_num_dose_levels(object), err_msg)

  # Changing `max_levels` so that it is a negative value.
  object@max_levels <- -2L
  expect_equal(v_increments_num_dose_levels(object), err_msg)
})

test_that("v_increments_num_dose_levels returns message for non-valid basis_level", {
  err_msg <- "basis_level must be either 'last' or 'max'"
  object <- IncrementsNumDoseLevels()

  # Changing `basis_level` so that it is neither equal to 'last' nor 'max'
  object@basis_level <- "last "
  expect_equal(v_increments_num_dose_levels(object), err_msg)

  object@basis_level <- " max "
  expect_equal(v_increments_num_dose_levels(object), err_msg)

  object@basis_level <- c("last", "max")
  expect_equal(v_increments_num_dose_levels(object), err_msg)
})

## v_increments_hsr_beta ----

test_that("v_increments_hsr_beta passes for valid object", {
  object <- IncrementsHSRBeta(target = 0.3, prob = 0.95)
  expect_true(v_increments_hsr_beta(object))

  object <- IncrementsHSRBeta(target = 0.2, prob = 0.9, a = 7, b = 3)
  expect_true(v_increments_hsr_beta(object))
})

test_that("v_increments_hsr_beta returns expected messages for non-valid object probabilities", {
  err_msg <- c("target must be a probability", "prob must be a probability")
  object <- IncrementsHSRBeta()

  # Changing `target` and `prob` so that they are not a probability values.
  object@target <- -0.3
  object@prob <- 1.1
  expect_equal(v_increments_hsr_beta(object), err_msg)

  # Changing `target` and `prob` so that they are not scalars.
  object@target <- c(0.3, 0.4)
  object@prob <- c(0.9, 0.95)
  expect_equal(v_increments_hsr_beta(object), err_msg)
})

test_that("v_increments_hsr_beta returns expected messages for non-valid beta parameters", {
  err_msg <- c(
    "Beta distribution shape parameter a must be a positive scalar",
    "Beta distribution shape parameter b must be a positive scalar"
  )
  object <- IncrementsHSRBeta()

  # Changing `a` and `b` so that they are not a positive scalars.
  object@a <- -2
  object@b <- 0
  expect_equal(v_increments_hsr_beta(object), err_msg)

  object@a <- c(1, 2)
  object@b <- c(1, 2)
  expect_equal(v_increments_hsr_beta(object), err_msg)
})

## v_increments_min ----

test_that("v_increments_min passes for valid object", {
  object <- IncrementsMin(
    increments_list = list(
      IncrementsRelativeDLT(dlt_intervals = c(0L, 1L), increments = c(2, 1)),
      IncrementsRelative(intervals = c(0, 2), increments = c(2, 1))
    )
  )
  expect_true(v_increments_min(object))
})

test_that("v_increments_min returns expected messages for non-valid object", {
  err_msg <- "all elements in increments_list must be of Increments class"
  object <- IncrementsMin(
    increments_list = list(
      IncrementsRelativeDLT(dlt_intervals = c(0L, 1L), increments = c(2, 1)),
      IncrementsRelative(intervals = c(0, 2), increments = c(2, 1))
    )
  )

  # Changing `increments_list` so that is contains objects other than `Increments`.
  object@increments_list <- list(
    IncrementsRelativeDLT(dlt_intervals = c(0L, 1L), increments = c(2, 1)),
    intervals = c(0, 2),
    increments = c(2, 1)
  )
  expect_equal(v_increments_min(object), err_msg)
})

# Stopping ----

## v_stopping_cohorts_near_dose ----

test_that("v_stopping_cohorts_near_dose passes for valid object", {
  object <- StoppingCohortsNearDose()
  expect_true(v_stopping_cohorts_near_dose(object))

  object <- StoppingCohortsNearDose(nCohorts = 5L, percentage = 40)
  expect_true(v_stopping_cohorts_near_dose(object))
})

test_that("v_stopping_cohorts_near_dose returns message for non-valid nCohorts", {
  err_msg <- "nCohorts must be positive integer scalar"
  object <- StoppingCohortsNearDose()

  # Changing `nCohorts` so that it not a scalar.
  object@nCohorts <- c(1L, 2L)
  expect_equal(v_stopping_cohorts_near_dose(object), err_msg)

  # Changing `nCohorts` so that it is NA value.
  object@nCohorts <- NA_integer_
  expect_equal(v_stopping_cohorts_near_dose(object), err_msg)

  # Changing `nCohorts` so that it is not a positive value.
  object@nCohorts <- -2L
  expect_equal(v_stopping_cohorts_near_dose(object), err_msg)
})

test_that("v_stopping_cohorts_near_dose returns message for non-valid percentage", {
  err_msg <- "percentage must be a number between 0 and 100"
  object <- StoppingCohortsNearDose()

  # Changing `percentage` so that it not a scalar.
  object@percentage <- c(1L, 2L)
  expect_equal(v_stopping_cohorts_near_dose(object), err_msg)

  # Changing `percentage` so that it is NA value.
  object@percentage <- NA_integer_
  expect_equal(v_stopping_cohorts_near_dose(object), err_msg)

  # Changing `percentage` so that it is not a percentage.
  object@percentage <- -1
  expect_equal(v_stopping_cohorts_near_dose(object), err_msg)

  # Changing `percentage` so that it is not a percentage.
  object@percentage <- 101
  expect_equal(v_stopping_cohorts_near_dose(object), err_msg)
})

## v_stopping_patients_near_dose ----

test_that("v_stopping_patients_near_dose passes for valid object", {
  object <- StoppingPatientsNearDose(nPatients = 10L)
  expect_true(v_stopping_patients_near_dose(object))

  object <- StoppingPatientsNearDose(nPatients = 5L, percentage = 40)
  expect_true(v_stopping_patients_near_dose(object))
})

test_that("v_stopping_patients_near_dose returns message for non-valid nPatients", {
  err_msg <- "nPatients must be positive integer scalar"
  object <- StoppingPatientsNearDose(nPatients = 5L)

  # Changing `nPatients` so that it not a scalar.
  object@nPatients <- c(1L, 2L)
  expect_equal(v_stopping_patients_near_dose(object), err_msg)

  # Changing `nPatients` so that it is NA value.
  object@nPatients <- NA_integer_
  expect_equal(v_stopping_patients_near_dose(object), err_msg)

  # Changing `nPatients` so that it is not a positive value.
  object@nPatients <- -2L
  expect_equal(v_stopping_patients_near_dose(object), err_msg)
})

test_that("v_stopping_patients_near_dose returns message for non-valid percentage", {
  err_msg <- "percentage must be a number between 0 and 100"
  object <- StoppingPatientsNearDose(nPatients = 5L)

  # Changing `percentage` so that it not a scalar.
  object@percentage <- c(1L, 2L)
  expect_equal(v_stopping_patients_near_dose(object), err_msg)

  # Changing `percentage` so that it is NA value.
  object@percentage <- NA_integer_
  expect_equal(v_stopping_patients_near_dose(object), err_msg)

  # Changing `percentage` so that it is not a percentage.
  object@percentage <- -1
  expect_equal(v_stopping_patients_near_dose(object), err_msg)

  # Changing `percentage` so that it is not a percentage.
  object@percentage <- 101
  expect_equal(v_stopping_patients_near_dose(object), err_msg)
})

## v_stopping_min_cohorts ----

test_that("v_stopping_min_cohorts passes for valid object", {
  object <- StoppingMinCohorts(nCohorts = 5L)
  expect_true(v_stopping_min_cohorts(object))
})

test_that("v_stopping_min_cohorts returns message for non-valid nCohorts", {
  err_msg <- "nCohorts must be positive integer scalar"
  object <- StoppingMinCohorts(nCohorts = 5L)

  # Changing `nCohorts` so that it not a scalar.
  object@nCohorts <- c(1L, 2L)
  expect_equal(v_stopping_min_cohorts(object), err_msg)

  # Changing `nCohorts` so that it is NA value.
  object@nCohorts <- NA_integer_
  expect_equal(v_stopping_min_cohorts(object), err_msg)

  # Changing `nCohorts` so that it is not a positive value.
  object@nCohorts <- -2L
  expect_equal(v_stopping_min_cohorts(object), err_msg)
})

## v_stopping_min_patients ----

test_that("v_stopping_min_patients passes for valid object", {
  object <- StoppingMinPatients(nPatients = 5L)
  expect_true(v_stopping_min_patients(object))
})

test_that("v_stopping_min_patients returns message for non-valid nPatients", {
  err_msg <- "nPatients must be positive integer scalar"
  object <- StoppingMinPatients(nPatients = 5L)

  # Changing `nPatients` so that it not a scalar.
  object@nPatients <- c(1L, 2L)
  expect_equal(v_stopping_min_patients(object), err_msg)

  # Changing `nPatients` so that it is NA value.
  object@nPatients <- NA_integer_
  expect_equal(v_stopping_min_patients(object), err_msg)

  # Changing `nPatients` so that it is not a positive value.
  object@nPatients <- -2L
  expect_equal(v_stopping_min_patients(object), err_msg)
})

## v_stopping_target_prob ----

test_that("v_stopping_target_prob passes for valid object", {
  object <- StoppingTargetProb(target = c(0.2, 0.35), prob = 0.4)
  expect_true(v_stopping_target_prob(object))
})

test_that("v_stopping_target_prob returns message for non-valid target", {
  err_msg <- "target has to be a probability range"
  object <- StoppingTargetProb(target = c(0.2, 0.35), prob = 0.4)

  # Changing `target` so that it is not an interval.
  object@target <- 0.6
  expect_equal(v_stopping_target_prob(object), err_msg)

  object@target <- c(0.5, 0.6, 0.8)
  expect_equal(v_stopping_target_prob(object), err_msg)

  # Changing `target` so that one bound is not a valid probability value.
  object@target <- c(0.4, 1.2)
  expect_equal(v_stopping_target_prob(object), err_msg)
})

test_that("v_stopping_target_prob returns message for non-valid prob", {
  err_msg <- "prob must be a probability value from (0, 1) interval"
  object <- StoppingTargetProb(target = c(0.2, 0.35), prob = 0.4)

  # Changing `prob` so that it does not represent allowed probability value.
  object@prob <- 1
  expect_equal(v_stopping_target_prob(object), err_msg)

  # Changing `prob` so that it is not a scalar.
  object@prob <- c(0.5, 0.6)
  expect_equal(v_stopping_target_prob(object), err_msg)
})

## v_stopping_mtd_distribution ----

test_that("v_stopping_mtd_distribution passes for valid object", {
  object <- StoppingMTDdistribution(target = 0.33, thresh = 0.5, prob = 0.9)
  expect_true(v_stopping_mtd_distribution(object))
})

test_that("v_stopping_mtd_distribution returns message for non-valid target", {
  err_msg <- "target must be a probability value from (0, 1) interval"
  object <- StoppingMTDdistribution(target = 0.33, thresh = 0.5, prob = 0.9)

  # Changing `target` so that it does not represent allowed probability value.
  object@target <- 1
  expect_equal(v_stopping_mtd_distribution(object), err_msg)

  # Changing `target` so that it is not a scalar.
  object@target <- c(0.5, 0.6)
  expect_equal(v_stopping_mtd_distribution(object), err_msg)
})

test_that("v_stopping_mtd_distribution returns message for non-valid thresh", {
  err_msg <- "thresh must be a probability value from (0, 1) interval"
  object <- StoppingMTDdistribution(target = 0.33, thresh = 0.5, prob = 0.9)

  # Changing `thresh` so that it does not represent allowed probability value.
  object@thresh <- 1
  expect_equal(v_stopping_mtd_distribution(object), err_msg)

  # Changing `thresh` so that it is not a scalar.
  object@thresh <- c(0.5, 0.6)
  expect_equal(v_stopping_mtd_distribution(object), err_msg)
})

test_that("v_stopping_mtd_distribution returns message for non-valid prob", {
  err_msg <- "prob must be a probability value from (0, 1) interval"
  object <- StoppingMTDdistribution(target = 0.33, thresh = 0.5, prob = 0.9)

  # Changing `prob` so that it does not represent allowed probability value.
  object@prob <- 1
  expect_equal(v_stopping_mtd_distribution(object), err_msg)

  # Changing `prob` so that it is not a scalar.
  object@prob <- c(0.5, 0.6)
  expect_equal(v_stopping_mtd_distribution(object), err_msg)
})

## v_stopping_mtd_cv ----

test_that("v_stopping_mtd_cv passes for valid object", {
  object <- StoppingMTDCV(target = 0.3, thresh_cv = 30)
  expect_true(v_stopping_mtd_cv(object))
})

test_that("v_stopping_mtd_cv returns expected messages for non-valid object", {
  object <- StoppingMTDCV()
  object@target <- c(-0.3)
  object@thresh_cv <- c(-40)

  expect_equal(
    v_stopping_mtd_cv(object),
    c(
      "target must be probability value from (0, 1) interval",
      "thresh_cv must be percentage > 0"
    )
  )
})

test_that("v_stopping_mtd_cv returns expected messages for non-valid object", {
  object <- StoppingMTDCV()
  object@target <- c(3)
  object@thresh_cv <- c(0)

  expect_equal(
    v_stopping_mtd_cv(object),
    c(
      "target must be probability value from (0, 1) interval",
      "thresh_cv must be percentage > 0"
    )
  )
})

test_that("v_stopping_mtd_cv returns expected messages for non-valid object", {
  object <- StoppingMTDCV()
  object@target <- c(0.3, 0.35)
  object@thresh_cv <- c(30, 40)

  expect_equal(
    v_stopping_mtd_cv(object),
    c(
      "target must be probability value from (0, 1) interval",
      "thresh_cv must be percentage > 0"
    )
  )
})

## v_stopping_lowest_dose_hsr_beta ----

test_that("v_stopping_lowest_dose_hsr_beta passes for valid object", {
  object <- StoppingLowestDoseHSRBeta(target = 0.3, prob = 0.95)
  expect_true(v_stopping_lowest_dose_hsr_beta(object))
})

test_that("v_stopping_lowest_dose_hsr_beta passes for valid object", {
  object <- StoppingLowestDoseHSRBeta(target = 0.2, prob = 0.9, a = 7, b = 3)
  expect_true(v_stopping_lowest_dose_hsr_beta(object))
})

test_that("StoppingLowestDoseHSRBeta returns expected messages for non-valid object", {
  object <- StoppingLowestDoseHSRBeta()
  object@target <- -0.3
  object@prob <- 1.1
  object@a <- -2
  object@b <- 0

  expect_equal(
    v_stopping_lowest_dose_hsr_beta(object),
    c(
      "target must be a probability",
      "prob must be a probability",
      "Beta distribution shape parameter a must be a positive scalar",
      "Beta distribution shape parameter b must be a positive scalar"
    )
  )
})

test_that("StoppingLowestDoseHSRBeta returns expected messages for non-valid object", {
  object <- StoppingLowestDoseHSRBeta()
  object@target <- c(0.3, 0.4)
  object@prob <- c(0.9, 0.95)
  object@a <- c(1, 2)
  object@b <- c(1, 2)

  expect_equal(
    v_stopping_lowest_dose_hsr_beta(object),
    c(
      "target must be a probability",
      "prob must be a probability",
      "Beta distribution shape parameter a must be a positive scalar",
      "Beta distribution shape parameter b must be a positive scalar"
    )
  )
})
