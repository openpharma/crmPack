# Opening ----

## v_opening_min_dose ----

test_that("v_opening_min_dose passes for valid object", {
  object <- OpeningMinDose(min_dose = 0)
  expect_true(v_opening_min_dose(object))

  object <- OpeningMinDose(min_dose = 50)
  expect_true(v_opening_min_dose(object))
})

test_that("v_opening_min_dose returns message for non-valid min_dose", {
  err_msg <- "min_dose needs to be a non-negative numeric scalar"
  object <- OpeningMinDose(min_dose = 10)

  # Changing `min_dose` so that it is negative.
  object@min_dose <- -5
  expect_equal(v_opening_min_dose(object), err_msg)

  # Changing `min_dose` so that it is NA.
  object@min_dose <- NA_real_
  expect_equal(v_opening_min_dose(object), err_msg)

  # Changing `min_dose` so that it is not a scalar.
  object@min_dose <- c(1, 5)
  expect_equal(v_opening_min_dose(object), err_msg)
})

## v_opening_min_cohorts ----

test_that("v_opening_min_cohorts passes for valid object", {
  object <- OpeningMinCohorts(min_cohorts = 2)
  expect_true(v_opening_min_cohorts(object))

  object <- OpeningMinCohorts(min_cohorts = 5)
  expect_true(v_opening_min_cohorts(object))
})

test_that("v_opening_min_cohorts returns message for non-valid min_cohorts", {
  err_msg <- "min_cohorts needs to be a positive integer scalar"
  object <- OpeningMinCohorts(min_cohorts = 3)

  # Changing `min_cohorts` so that it is zero.
  object@min_cohorts <- 0L
  expect_equal(v_opening_min_cohorts(object), err_msg)

  # Changing `min_cohorts` so that it is negative.
  object@min_cohorts <- -1L
  expect_equal(v_opening_min_cohorts(object), err_msg)

  # Changing `min_cohorts` so that it is NA.
  object@min_cohorts <- NA_integer_
  expect_equal(v_opening_min_cohorts(object), err_msg)

  # Changing `min_cohorts` so that it is not a scalar.
  object@min_cohorts <- c(2L, 3L)
  expect_equal(v_opening_min_cohorts(object), err_msg)
})
