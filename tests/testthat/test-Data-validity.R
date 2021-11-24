# validate_subjects ----

test_that("validate_subjects passes for valid object", {
  object <- h_get_data()
  expect_true(validate_subjects(object))
})

test_that("validate_subjects returns expected messages for non-valid object", {
  object <- h_get_data()
  object@nObs <- 4L
  expect_equal(
    validate_subjects(object),
    c(
      "ID must be of type integer and length nObs and unique",
      "cohort must be of type integer and length nObs and non-negative"
    )
  )
})

# validate_data ----

test_that("validate_data passes for valid object", {
  object <- h_get_data()
  expect_true(validate_data(object))
})

test_that("validate_data returns expected error when only placebo in one cohort", {
  object <- h_get_data()
  
  # We assign only placebo to all cohort 3 patients.
  object@x[which(object@cohort == 3L)] <- object@doseGrid[1]

  expect_equal(
    validate_data(object),
    c(
      "x must be equivalent to doseGrid[xLevel] (up to numerical tolerance)",
      "A cohort with only placebo is not allowed"
    )
  )
})

test_that("validate_data returns expected error when multiple different doses in one cohort", {
  object <- h_get_data()
  
  # We assign multiple doses to cohort 1.
  cohort_1 <- which(object@cohort == 1L)
  object@x[cohort_1] <- sample(x = object@doseGrid[-1], size = length(cohort_1))

  expect_equal(
    validate_data(object),
    c(
      "x must be equivalent to doseGrid[xLevel] (up to numerical tolerance)",
      "There must be only one dose level, other than placebo, per cohort"
    )
  )
})

test_that("validate_data returns expected error when first xLevel does not match x", {
  object <- h_get_data()
  
  # We assign a wrong xLevel for the first patient.
  object@xLevel[1] <- object@xLevel[1] + 20L

  expect_equal(
    validate_data(object),
    c("x must be equivalent to doseGrid[xLevel] (up to numerical tolerance)")
  )
})
