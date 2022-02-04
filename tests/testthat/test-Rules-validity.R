# validate_increments_numdoselevels ----

test_that("validate_increments_numdoselevels passes for valid object", {
  object <- IncrementsNumDoseLevels(maxLevels = 1, basisLevel = "lastGiven")
  expect_true(validate_increments_numdoselevels(object))
})

test_that("validate_increments_numdoselevels passes for valid object", {
  object <- IncrementsNumDoseLevels(maxLevels = 1, basisLevel = "maxGiven")
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
      "basisLevel must be either 'lastGiven' or 'maxGiven'"
    )
  )
})