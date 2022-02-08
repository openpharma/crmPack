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
