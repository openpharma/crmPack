# h_doses_unique_per_cohort ----

test_that("h_doses_unique_per_cohort returns TRUE for unique doses in cohorts", {
  dose <- c(1.5, 2.5, 2.5, 3.5, 3.5)
  cohort <- c(1L, 2L, 2L, 3L, 3L)
  result <- expect_silent(h_doses_unique_per_cohort(dose, cohort))
  expect_true(result)
})

test_that("h_doses_unique_per_cohort returns FALSE for non-unique doses in cohorts", {
  dose <- c(1.5, 2.5, 12.5, 3.5, 3.5)
  cohort <- c(1L, 2L, 2L, 3L, 3L)
  result <- expect_silent(h_doses_unique_per_cohort(dose, cohort))
  expect_false(result)
})

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
      "cohort must be of type integer and length nObs and contain non-negative, sorted values"
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

# validate_data_dual ----

test_that("validate_data_dual passes for valid object", {
  object <- h_get_data_dual()
  expect_true(validate_data_dual(object))
})

test_that("validate_data_dual returns error for biomarker vector of wrong length", {
  object <- h_get_data_dual()

  # We assign biomarker vector of length different than object@nObs.
  object@w <- object@w[1:5]

  expect_equal(
    validate_data_dual(object),
    "Biomarker vector w must be of type double and length nObs"
  )
})
