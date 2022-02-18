# validate_increments_numdoselevels ----

test_that("validate_increments_numdoselevels passes for valid object", {
  object <- IncrementsNumDoseLevels(maxLevels = 1, basisLevel = "last")
  expect_true(validate_increments_numdoselevels(object))
})

test_that("validate_increments_numdoselevels passes for valid object", {
  object <- IncrementsNumDoseLevels(maxLevels = 1, basisLevel = "max")
  expect_true(validate_increments_numdoselevels(object))
})

test_that("validate_increments_numdoselevels passes for valid object", {
  object <- IncrementsNumDoseLevels(maxLevels = 1)
  expect_true(validate_increments_numdoselevels(object))
})

test_that("validate_increments_numdoselevels returns expected messages for non-valid object", {
  object <- IncrementsNumDoseLevels()
  object@maxLevels <- c(-1L)
  object@basisLevel <- c("minGiven")

  expect_equal(
    validate_increments_numdoselevels(object),
    c(
      "maxLevels must be scalar positive integer",
      "basisLevel must be either 'last' or 'max'"
    )
  )
})


# validate_increments_hsr_beta ----

test_that("validate_increments_hsr_beta passes for valid object", {
  object <- IncrementsHSRBeta(target = 0.3, prob = 0.95)
  expect_true(validate_increments_hsr_beta(object))
})

test_that("validate_increments_hsr_beta passes for valid object", {
  object <- IncrementsHSRBeta(target = 0.2, prob = 0.9, a = 7, b = 3)
  expect_true(validate_increments_hsr_beta(object))
})

test_that("validate_increments_hsr_beta returns expected messages for non-valid object", {
  object <- IncrementsHSRBeta()
  object@target <- -0.3
  object@prob <- 1.1
  object@a <- -2
  object@b <- 0

  expect_equal(
    validate_increments_hsr_beta(object),
    c(
      "target must be a probability",
      "prob must be a probability",
      "Beta distribution shape parameter a must be a positive scalar",
      "Beta distribution shape parameter b must be a positive scalar"
    )
  )
})

test_that("validate_increments_hsr_beta returns expected messages for non-valid object", {
  object <- IncrementsHSRBeta()
  object@target <- c(0.3, 0.4)
  object@prob <- c(0.9, 0.95)
  object@a <- c(1, 2)
  object@b <- c(1, 2)

  expect_equal(
    validate_increments_hsr_beta(object),
    c(
      "target must be a probability",
      "prob must be a probability",
      "Beta distribution shape parameter a must be a positive scalar",
      "Beta distribution shape parameter b must be a positive scalar"
    )
  )
})

# validate_stopping_mtd_cv ----

test_that("validate_stopping_mtd_cv passes for valid object", {
  object <- StoppingMTDCV(target = 0.3, thresh_cv = 30)
  expect_true(validate_stopping_mtd_cv(object))
})

test_that("validate_stopping_mtd_cv returns expected messages for non-valid object", {
  object <- StoppingMTDCV()
  object@target <- c(-0.3)
  object@thresh_cv <- c(-40)

  expect_equal(
    validate_stopping_mtd_cv(object),
    c(
      "target must be probability > 0 and < 1",
      "thresh_cv must be percentage > 0"
    )
  )
})

test_that("validate_stopping_mtd_cv returns expected messages for non-valid object", {
  object <- StoppingMTDCV()
  object@target <- c(3)
  object@thresh_cv <- c(0)

  expect_equal(
    validate_stopping_mtd_cv(object),
    c(
      "target must be probability > 0 and < 1",
      "thresh_cv must be percentage > 0"
    )
  )
})

test_that("validate_stopping_mtd_cv returns expected messages for non-valid object", {
  object <- StoppingMTDCV()
  object@target <- c(0.3, 0.35)
  object@thresh_cv <- c(30, 40)

  expect_equal(
    validate_stopping_mtd_cv(object),
    c(
      "target must be probability > 0 and < 1",
      "thresh_cv must be percentage > 0"
    )
  )
})


# validate_stopping_lowest_dose_hsr_beta ----

test_that("validate_stopping_lowest_dose_hsr_beta passes for valid object", {
  object <- StoppingLowestDoseHSRBeta(target = 0.3, prob = 0.95)
  expect_true(validate_stopping_lowest_dose_hsr_beta(object))
})

test_that("validate_stopping_lowest_dose_hsr_beta passes for valid object", {
  object <- StoppingLowestDoseHSRBeta(target = 0.2, prob = 0.9, a = 7, b = 3)
  expect_true(validate_stopping_lowest_dose_hsr_beta(object))
})

test_that("StoppingLowestDoseHSRBeta returns expected messages for non-valid object", {
  object <- StoppingLowestDoseHSRBeta()
  object@target <- -0.3
  object@prob <- 1.1
  object@a <- -2
  object@b <- 0

  expect_equal(
    validate_stopping_lowest_dose_hsr_beta(object),
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
    validate_stopping_lowest_dose_hsr_beta(object),
    c(
      "target must be a probability",
      "prob must be a probability",
      "Beta distribution shape parameter a must be a positive scalar",
      "Beta distribution shape parameter b must be a positive scalar"
    )
  )
})
