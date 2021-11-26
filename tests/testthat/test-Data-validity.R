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

# validate_data_parts ----

test_that("validate_data_parts passes for valid object", {
  object <- h_get_data_parts()
  expect_true(validate_data_parts(object))
})

test_that("validate_data_parts returns error for vector part of wrong length and values", {
  object <- h_get_data_parts()

  # We assign vector part of length different than object@nObs.
  object@part <- 1:5

  expect_equal(
    validate_data_parts(object),
    "vector part must be nObs long and contain 1 or 2 integers only"
  )
})

test_that("validate_data_parts returns error for nextPart with wrong length, values and ordering", {
  object <- h_get_data_parts()

  # We assign vector nextPart of length different than 1,
  # with not sorted values not only from {1, 2}.
  object@nextPart <- c(1L, 3L, 3L, 2L)

  expect_equal(
    validate_data_parts(object),
    "nextPart must be integer scalar 1 or 2"
  )
})

test_that("validate_data_parts returns error for part1Ladder with not sorted and non-unique values", {
  object <- h_get_data_parts()

  # We assign vector part1Ladder with not sorted and non-unique values.
  object@part1Ladder <- c(200, 300, 50, 225, 225)

  expect_equal(
    validate_data_parts(object),
    "part1Ladder must be of type double and contain unique, sorted values"
  )
})

test_that("validate_data_parts returns error for part1Ladder with wrong values", {
  object <- h_get_data_parts()

  # We assign vector part1Ladder with values not from object@doseGrid.
  object@part1Ladder <- as.numeric(1:12)

  expect_equal(
    validate_data_parts(object),
    "part1Ladder must have all entries from doseGrid"
  )
})

# validate_data_mixture ----

test_that("validate_data_mixture passes for valid object", {
  object <- h_get_data_mixture()
  expect_true(validate_data_mixture(object))
})

test_that("validate_data_mixture returns error for vector xshare of wrong length", {
  object <- h_get_data_mixture()

  # We assign vector xshare of length different than object@nObsshare
  object@xshare <- c(100, 125)

  expect_equal(
    validate_data_mixture(object),
    "Dose vector xshare must be of type double and length nObsshare"
  )
})

test_that("validate_data_mixture returns error for vector xshare with wrong values", {
  object <- h_get_data_mixture()

  # We assign vector xshare with values not from object@doseGrid.
  object@xshare <- as.numeric(1:4)

  expect_equal(
    validate_data_mixture(object),
    "Dose values in xshare must be from doseGrid"
  )
})

test_that("validate_data_mixture returns error for vector yshare of wrong length and values", {
  object <- h_get_data_mixture()

  # We assign vector yshare of length different than object@nObsshare
  # and with values not from {0, 1}.
  object@yshare <- c(11:20)

  expect_equal(
    validate_data_mixture(object),
    "DLT vector yshare must be nObsshare long and contain 0 or 1 integers only"
  )
})

test_that("validate_data_mixture returns error for nObsshare of wrong length", {
  object <- h_get_data_mixture()

  # We assign nObsshare of length different than 1,
  object@nObsshare <- 1:5

  # validate_data_mixture will throw the error here, which will be originated by
  # test_* functions (used in validate_data_mixture) when the len parameter
  # (initiated to object@nObsshare) is not a scalar.
  expect_error(
    validate_data_mixture(object),
    regexp = "^Argument '[[:alnum:]]+' must have length 1$"
  )
})
