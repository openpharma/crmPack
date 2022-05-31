# v_next_best_mtd ----

test_that("v_next_best_mtd passes for valid object", {
  object <- h_next_best_mtd()
  expect_true(v_next_best_mtd(object))
})

test_that("v_next_best_mtd returns message for non-valid target", {
  object <- h_next_best_mtd()
  # Changing `target` so that it does not represent a probability value.
  object@target <- 1.2

  expect_equal(
    v_next_best_mtd(object),
    "target must be probability > 0 and < 1"
  )
})

test_that("v_next_best_mtd returns message for non-valid derive", {
  object <- h_next_best_mtd()
  # Changing `derive` so that it does not have one `mtd_samples` argument.
  object@derive <- function(x) {
    mean(x)
  }

  expect_equal(
    v_next_best_mtd(object),
    "derive must have as single argument 'mtd_samples'"
  )
})

# v_next_best_ncrm ----

test_that("v_next_best_ncrm passes for valid object", {
  object <- h_next_best_ncrm()
  expect_true(v_next_best_ncrm(object))
})

test_that("v_next_best_ncrm returns message for non-valid target", {
  object <- h_next_best_ncrm()
  # Changing `target` so that it is not an interval.
  object@target <- 1.2
  expect_equal(v_next_best_ncrm(object), "target has to be a probability range")

  # Changing `target` so that the one bound is not a valid probability.
  object@target <- c(0.4, 2)
  expect_equal(v_next_best_ncrm(object), "target has to be a probability range")
})

test_that("v_next_best_ncrm returns message for non-valid overdose", {
  object <- h_next_best_ncrm()
  # Changing `overdose` so that it is not an interval.
  object@overdose <- 1.2
  expect_equal(v_next_best_ncrm(object), "overdose has to be a probability range")

  # Changing `overdose` so that the one bound is not a valid probability.
  object@overdose <- c(0.4, 2)
  expect_equal(v_next_best_ncrm(object), "overdose has to be a probability range")
})

test_that("h_next_best_ncrm returns message for non-valid max_overdose_prob", {
  object <- h_next_best_ncrm()
  # Changing `max_overdose_prob` so that it does not represent a probability value.
  object@max_overdose_prob <- 1.2

  expect_equal(
    v_next_best_ncrm(object),
    "max_overdose_prob must be probability > 0 and < 1"
  )
})

# v_increments_numdoselevels ----

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

# v_increments_hsr_beta ----

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

# v_stopping_mtd_cv ----

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
      "target must be probability > 0 and < 1",
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
      "target must be probability > 0 and < 1",
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
      "target must be probability > 0 and < 1",
      "thresh_cv must be percentage > 0"
    )
  )
})


# v_stopping_lowest_dose_hsr_beta ----

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
