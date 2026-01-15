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
