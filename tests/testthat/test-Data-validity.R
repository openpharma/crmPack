# h_doses_unique_per_cohort ----

test_that("h_doses_unique_per_cohort returns TRUE for unique doses", {
  dose <- c(1.5, 2.5, 2.5, 3.5, 3.5)
  cohort <- c(1L, 2L, 2L, 3L, 3L)
  result <- expect_silent(h_doses_unique_per_cohort(dose, cohort))
  expect_true(result)
})

test_that("h_doses_unique_per_cohort returns FALSE for non-unique doses", {
  dose <- c(1.5, 2.5, 12.5, 3.5, 3.5)
  cohort <- c(1L, 2L, 2L, 3L, 3L)
  result <- expect_silent(h_doses_unique_per_cohort(dose, cohort))
  expect_false(result)
})

# v_general_data ----

test_that("v_general_data passes for valid object", {
  object <- h_get_data()
  expect_true(v_general_data(object))
})

test_that("v_general_data returns expected messages for non-valid object", {
  object <- h_get_data()
  object@nObs <- 4L
  expect_equal(
    v_general_data(object),
    c(
      "ID must be of type integer and length nObs and unique",
      "cohort must be of type integer and length nObs and contain non-negative, sorted values" # nolintr
    )
  )
})

# v_data ----

test_that("v_data passes for valid object", {
  object <- h_get_data()
  expect_true(v_data(object))
})

test_that("v_data returns error when only placebo in cohort", {
  object <- h_get_data()

  # We assign only placebo to all cohort 3 patients.
  object@x[which(object@cohort == 3L)] <- object@doseGrid[1]

  expect_equal(
    v_data(object),
    c(
      "x must be equivalent to doseGrid[xLevel] (up to numerical tolerance)",
      "A cohort with only placebo is not allowed"
    )
  )
})

test_that("v_data: error when multiple different doses in a cohort", {
  object <- h_get_data()

  # We assign multiple doses to cohort 1.
  cohort_1 <- which(object@cohort == 1L)
  object@x[cohort_1] <- sample(x = object@doseGrid[-1], size = length(cohort_1))

  expect_equal(
    v_data(object),
    c(
      "x must be equivalent to doseGrid[xLevel] (up to numerical tolerance)",
      "There must be only one dose level, other than placebo, per cohort"
    )
  )
})

test_that("v_data returns error when first xLevel does not match x", {
  object <- h_get_data()

  # We assign a wrong xLevel for the first patient.
  object@xLevel[1] <- object@xLevel[1] + 20L

  expect_equal(
    v_data(object),
    c("x must be equivalent to doseGrid[xLevel] (up to numerical tolerance)")
  )
})

# v_data_dual ----

test_that("v_data_dual passes for valid object", {
  object <- h_get_data_dual()
  expect_true(v_data_dual(object))
})

test_that("v_data_dual returns error for biomarker of wrong length", {
  object <- h_get_data_dual()

  # We assign biomarker vector of length different than object@nObs.
  object@w <- object@w[1:5]

  expect_equal(
    v_data_dual(object),
    "Biomarker vector w must be of type double and length nObs"
  )
})

# v_data_parts ----

test_that("v_data_parts passes for valid object", {
  object <- h_get_data_parts()
  expect_true(v_data_parts(object))
})

test_that("v_data_parts: error when part of wrong length and values", {
  object <- h_get_data_parts()

  # We assign vector part of length different than object@nObs.
  object@part <- 1:5

  expect_equal(
    v_data_parts(object),
    "vector part must be nObs long and contain 1 or 2 integers only"
  )
})

test_that("v_data_parts: error when nextPart of wrong len/vals/order", {
  object <- h_get_data_parts()

  # We assign vector nextPart of length different than 1,
  # with not sorted values not only from {1, 2}.
  object@nextPart <- c(1L, 3L, 3L, 2L)

  expect_equal(
    v_data_parts(object),
    "nextPart must be integer scalar 1 or 2"
  )
})

test_that("v_data_parts: error when part1Ladder unsorted, non-unique", {
  object <- h_get_data_parts()

  # We assign vector part1Ladder with not sorted and non-unique values.
  object@part1Ladder <- c(200, 300, 50, 225, 225)

  expect_equal(
    v_data_parts(object),
    "part1Ladder must be of type double and contain unique, sorted values"
  )
})

test_that("v_data_parts: error when part1Ladder has wrong values", {
  object <- h_get_data_parts()

  # We assign vector part1Ladder with values not from object@doseGrid.
  object@part1Ladder <- as.numeric(1:12)

  expect_equal(
    v_data_parts(object),
    "part1Ladder must have all entries from doseGrid"
  )
})

# v_data_mixture ----

test_that("v_data_mixture passes for valid object", {
  object <- h_get_data_mixture()
  expect_true(v_data_mixture(object))
})

test_that("v_data_mixture: error when xshare is of wrong length", {
  object <- h_get_data_mixture()

  # We assign vector xshare of length different than object@nObsshare.
  object@xshare <- c(100, 125)

  expect_equal(
    v_data_mixture(object),
    "Dose vector xshare must be of type double and length nObsshare"
  )
})

test_that("v_data_mixture: error when xshare has wrong values", {
  object <- h_get_data_mixture()

  # We assign vector xshare with values not from object@doseGrid.
  object@xshare <- as.numeric(1:4)

  expect_equal(
    v_data_mixture(object),
    "Dose values in xshare must be from doseGrid"
  )
})

test_that("v_data_mixture: error for yshare of wrong length and vals.", {
  object <- h_get_data_mixture()

  # We assign vector yshare of length different than object@nObsshare
  # and with values not from {0, 1}.
  object@yshare <- c(11:20)

  expect_equal(
    v_data_mixture(object),
    "DLT vector yshare must be nObsshare long and contain 0 or 1 integers only"
  )
})

test_that("v_data_mixture: error for nObsshare of wrong length", {
  object <- h_get_data_mixture()

  # We assign nObsshare of length different than 1.
  object@nObsshare <- 1:5

  expect_equal(
    v_data_mixture(object),
    "nObsshare must be of type integer of length 1"
  )
})

# v_data_da ----

test_that("v_data_da passes for valid object", {
  object <- h_get_data_da()
  expect_true(v_data_da(object))
})

test_that("v_data_da: error for Tmax of wrong length, negative values", {
  object <- h_get_data_da()

  # We assign Tmax of wrong length and negative values.
  object@Tmax <- c(-10, -20)

  expect_equal(
    v_data_da(object),
    "DLT window Tmax must be of type double of length 1 and greater than 0"
  )
})

test_that("v_data_da: error for u of wrong length and values", {
  object <- h_get_data_da()

  # We assign vector u of length different than object@nObs
  # with some negative values and some other, greater than Tmax.
  object@u <- c(-1, -2, 100)

  expect_equal(
    v_data_da(object),
    "u must be of type double, nObs length, non-negative and not greater than Tmax" # nolintr
  )
})

test_that("v_data_da: error for t0 of wrong length, negative values", {
  object <- h_get_data_da()

  # We assign vector t0 of length different than object@nObs
  # and some negative, non-sorted values.
  object@t0 <- c(1, -2)

  expect_equal(
    v_data_da(object),
    "t0 must be of type double, nObs length, sorted non-negative"
  )
})

# v_data_ordinal ----

test_that("v_data_ordinal passes for valid object", {
  object <- h_get_data_ordinal()
  expect_true(v_data_ordinal(object))
})

test_that("v_data_ordinal correctly detects bad data", {
  object <- h_get_data_ordinal()
  object@x[2] <- NA
  expect_equal(
    v_data_ordinal(object),
    c(
      "Doses vector x must be of type double and length nObs",
      "Dose values in x must be from doseGrid",
      "x must be equivalent to doseGrid[xLevel] (up to numerical tolerance)"
    )
  )

  object <- h_get_data_ordinal()
  object@x[2] <- 5
  expect_equal(
    v_data_ordinal(object),
    c(
      "Dose values in x must be from doseGrid",
      "x must be equivalent to doseGrid[xLevel] (up to numerical tolerance)"
    )
  )

  expect_error(
    object@yCategories <- c("No tox" = 0, "AE" = 1, "DLT" = 2),
    "assignment of an object of class \"numeric\" is not valid for @'yCategories' in an object of class \"DataOrdinal\""
  )

  object <- h_get_data_ordinal()
  object@y[3] <- 3L
  expect_equal(
    v_data_ordinal(object),
    "DLT vector y must be nObs long and contain integers between 0 and k-1 only, where k is the length of the vector in the yCategories slot"
  )

  object <- h_get_data_ordinal()
  object@yCategories <- c("Good" = 0L, "Bad" = 1L, "Bad" = 2L)
  expect_equal(
    v_data_ordinal(object),
    "yCategory labels must be unique"
  )

  object <- h_get_data_ordinal()
  object@x <- c(10, 20, 30, 40, 50, 80, 50, 60, 60, 60)
  expect_equal(
    v_data_ordinal(object),
    c(
      "x must be equivalent to doseGrid[xLevel] (up to numerical tolerance)",
      "There must be only one dose level per cohort"
    )
  )

  object <- h_get_data_ordinal()
  object@placebo <- TRUE
  expect_equal(
    v_data_ordinal(object),
    "A cohort with only placebo is not allowed"
  )

  object <- h_get_data_ordinal()
  object@placebo <- TRUE
  object@x <- c(10, 20, 30, 40, 50, 80, 10, 60, 60, 60)
  object@xLevel <- as.integer(c(1, 2, 3, 4, 5, 8, 1, 6, 6, 6))
  object@cohort <- as.integer(c(1, 1, 2, 3, 4, 4, 4, 5, 5, 5))
  expect_equal(
    v_data_ordinal(object),
    "There must be only one dose level, other than placebo, per cohort"
  )
})
