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
