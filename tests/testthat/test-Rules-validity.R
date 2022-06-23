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
  # Changing `derive` so that it does not have one `mtd_samples` argument.
  object@derive <- function(x) {
    mean(x)
  }
  expect_equal(
    v_next_best_mtd(object),
    "derive must have a single argument 'mtd_samples'"
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

test_that("NextBestNCRMLoss error: overdose has to be a probability range", {
  expect_error(
    NextBestNCRMLoss(
      target_int = c(0.2, 0.35),
      overdose_int = c(0.35, 0.6, 1),
      max_overdose_prob = 0.25,
      losses = c(1, 0, 1, 2)
    ),
    "overdose_int has to be a probability range"
  )
})

test_that("NextBestNCRMLoss error: maxOverdoseProb has to be a probability", {
  expect_error(
    NextBestNCRMLoss(
      target_int = c(0.2, 0.35),
      overdose_int = c(0.35, 0.6),
      max_overdose_prob = 1.25,
      losses = c(1, 0, 1, 2)
    ),
    "max_overdose_prob must be probability > 0 and < 1"
  )
})

test_that("NextBestNCRMLoss error: losses has to be a vector of non-negative elements", {
  expect_error(
    NextBestNCRMLoss(
      target_int = c(0.2, 0.35),
      overdose_int = c(0.35, 0.6),
      unacceptable_int = c(0.6, 1),
      max_overdose_prob = 0.25,
      losses = c(-1, 0, 1, 2)
    ),
    "object: 1: losses has to be a vector of 3 or 4 elements"
  )
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

# Increments ----

## v_increments_numdoselevels ----

test_that("v_increments_numdoselevels passes for valid object", {
  object <- IncrementsNumDoseLevels(maxLevels = 1, basisLevel = "last")
  expect_true(v_increments_numdoselevels(object))
})

test_that("v_increments_numdoselevels passes for valid object", {
  object <- IncrementsNumDoseLevels(maxLevels = 1, basisLevel = "max")
  expect_true(v_increments_numdoselevels(object))
})

test_that("v_increments_numdoselevels passes for valid object", {
  object <- IncrementsNumDoseLevels(maxLevels = 1)
  expect_true(v_increments_numdoselevels(object))
})

test_that("v_increments_numdoselevels returns expected messages for non-valid object", {
  object <- IncrementsNumDoseLevels()
  object@maxLevels <- c(-1L)
  object@basisLevel <- c("minGiven")

  expect_equal(
    v_increments_numdoselevels(object),
    c(
      "maxLevels must be scalar positive integer",
      "basisLevel must be either 'last' or 'max'"
    )
  )
})

## v_increments_hsr_beta ----

test_that("v_increments_hsr_beta passes for valid object", {
  object <- IncrementsHSRBeta(target = 0.3, prob = 0.95)
  expect_true(v_increments_hsr_beta(object))
})

test_that("v_increments_hsr_beta passes for valid object", {
  object <- IncrementsHSRBeta(target = 0.2, prob = 0.9, a = 7, b = 3)
  expect_true(v_increments_hsr_beta(object))
})

test_that("v_increments_hsr_beta returns expected messages for non-valid object", {
  object <- IncrementsHSRBeta()
  object@target <- -0.3
  object@prob <- 1.1
  object@a <- -2
  object@b <- 0

  expect_equal(
    v_increments_hsr_beta(object),
    c(
      "target must be a probability",
      "prob must be a probability",
      "Beta distribution shape parameter a must be a positive scalar",
      "Beta distribution shape parameter b must be a positive scalar"
    )
  )
})

test_that("v_increments_hsr_beta returns expected messages for non-valid object", {
  object <- IncrementsHSRBeta()
  object@target <- c(0.3, 0.4)
  object@prob <- c(0.9, 0.95)
  object@a <- c(1, 2)
  object@b <- c(1, 2)

  expect_equal(
    v_increments_hsr_beta(object),
    c(
      "target must be a probability",
      "prob must be a probability",
      "Beta distribution shape parameter a must be a positive scalar",
      "Beta distribution shape parameter b must be a positive scalar"
    )
  )
})

# Stopping ----

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
