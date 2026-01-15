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
})

## v_opening_min_responses ----

test_that("v_opening_min_responses passes for valid object", {
  object <- OpeningMinResponses(min_responses = 1, include_lower_doses = FALSE)
  expect_true(v_opening_min_responses(object))

  object <- OpeningMinResponses(min_responses = 5, include_lower_doses = TRUE)
  expect_true(v_opening_min_responses(object))
})

test_that("v_opening_min_responses returns message for non-valid min_responses", {
  err_msg_responses <- "min_responses needs to be a positive integer scalar"
  object <- OpeningMinResponses(min_responses = 2)

  # Changing `min_responses` so that it is zero.
  object@min_responses <- 0L
  expect_equal(v_opening_min_responses(object), err_msg_responses)

  # Changing `min_responses` so that it is negative.
  object@min_responses <- -1L
  expect_equal(v_opening_min_responses(object), err_msg_responses)
})

test_that("v_opening_min_responses returns message for non-valid include_lower_doses", {
  err_msg_flag <- "include_lower_doses needs to be a logical flag"
  object <- OpeningMinResponses(min_responses = 2, include_lower_doses = FALSE)

  # Changing `include_lower_doses` so that it is NA.
  object@include_lower_doses <- NA
  expect_equal(v_opening_min_responses(object), err_msg_flag)

  # Changing `include_lower_doses` so that it is a vector.
  object@include_lower_doses <- c(TRUE, FALSE)
  expect_equal(v_opening_min_responses(object), err_msg_flag)

  # Changing `min_cohorts` so that it is NA.
  object@min_cohorts <- NA_integer_
  expect_equal(v_opening_min_cohorts(object), err_msg)

  # Changing `min_cohorts` so that it is not a scalar.
  object@min_cohorts <- c(2L, 3L)
  expect_equal(v_opening_min_cohorts(object), err_msg)
})

# Recruitment ----

## v_recruitment_ratio ----

test_that("v_recruitment_ratio passes for valid object", {
  object <- RecruitmentRatio(ratio = 0)
  expect_true(v_recruitment_ratio(object))

  object <- RecruitmentRatio(ratio = 0.5)
  expect_true(v_recruitment_ratio(object))

  object <- RecruitmentRatio(ratio = 1)
  expect_true(v_recruitment_ratio(object))

  object <- RecruitmentRatio(ratio = 2.5)
  expect_true(v_recruitment_ratio(object))
})

test_that("v_recruitment_ratio fails for negative ratio", {
  object <- RecruitmentRatio(ratio = 1)
  object@ratio <- -0.5
  err_msg <- "Ratio must be non-negative"
  expect_equal(v_recruitment_ratio(object), err_msg)
})

test_that("v_recruitment_ratio fails for non-scalar ratio", {
  object <- RecruitmentRatio(ratio = 1)
  object@ratio <- c(0.5, 1)
  err_msg <- "Ratio must be non-negative"
  expect_equal(v_recruitment_ratio(object), err_msg)
})

test_that("v_recruitment_ratio fails for NA ratio", {
  object <- RecruitmentRatio(ratio = 1)
  object@ratio <- NA_real_
  err_msg <- "Ratio must be non-negative"
  expect_equal(v_recruitment_ratio(object), err_msg)
})
