# Opening ----

## OpeningMinDose ----

test_that(".OpeningMinDose works as expected", {
  result <- expect_silent(.OpeningMinDose())
  expect_valid(result, "OpeningMinDose")
})

test_that("OpeningMinDose object can be created with user constructor", {
  result <- expect_silent(OpeningMinDose(min_dose = 10))
  expect_valid(result, "OpeningMinDose")
  expect_identical(result@min_dose, 10)
})

test_that("OpeningMinDose object can be created with default min_dose", {
  result <- expect_silent(OpeningMinDose())
  expect_valid(result, "OpeningMinDose")
  expect_identical(result@min_dose, 0)
})

test_that(".DefaultOpeningMinDose works as expected", {
  expect_equal(
    .DefaultOpeningMinDose(),
    OpeningMinDose(min_dose = 0)
  )
})

## OpeningMinCohorts ----

test_that(".OpeningMinCohorts works as expected", {
  result <- expect_silent(.OpeningMinCohorts())
  expect_valid(result, "OpeningMinCohorts")
})

test_that("OpeningMinCohorts object can be created with user constructor", {
  result <- expect_silent(OpeningMinCohorts(min_cohorts = 3))
  expect_valid(result, "OpeningMinCohorts")
  expect_identical(result@min_cohorts, 3L)
})

test_that("OpeningMinCohorts object can be created with default min_cohorts", {
  result <- expect_silent(OpeningMinCohorts())
  expect_valid(result, "OpeningMinCohorts")
  expect_identical(result@min_cohorts, 2L)
})

test_that(".DefaultOpeningMinCohorts works as expected", {
  expect_equal(
    .DefaultOpeningMinCohorts(),
    OpeningMinCohorts(min_cohorts = 2L)
  )
})

## OpeningNone ----

test_that(".OpeningNone works as expected", {
  result <- expect_silent(.OpeningNone())
  expect_valid(result, "OpeningNone")
})

test_that("OpeningNone object can be created with user constructor", {
  result <- expect_silent(OpeningNone())
  expect_valid(result, "OpeningNone")
})

test_that(".DefaultOpeningNone works as expected", {
  expect_equal(
    .DefaultOpeningNone(),
    OpeningNone()
  )
})

## OpeningMinResponses ----

test_that(".OpeningMinResponses works as expected", {
  result <- expect_silent(.OpeningMinResponses())
  expect_valid(result, "OpeningMinResponses")
})

test_that("OpeningMinResponses object can be created with user constructor", {
  result <- expect_silent(OpeningMinResponses(
    min_responses = 3,
    include_lower_doses = TRUE
  ))
  expect_valid(result, "OpeningMinResponses")
  expect_identical(result@min_responses, 3L)
  expect_identical(result@include_lower_doses, TRUE)
})

test_that("OpeningMinResponses object can be created with default parameters", {
  result <- expect_silent(OpeningMinResponses())
  expect_valid(result, "OpeningMinResponses")
  expect_identical(result@min_responses, 1L)
  expect_identical(result@include_lower_doses, FALSE)
})

test_that(".DefaultOpeningMinResponses works as expected", {
  expect_equal(
    .DefaultOpeningMinResponses(),
    OpeningMinResponses(min_responses = 1L, include_lower_doses = FALSE)
  )
})
