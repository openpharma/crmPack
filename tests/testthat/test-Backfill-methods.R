# openCohort ----

## OpeningMinDose ----

test_that("openCohort works as expected for OpeningMinDose", {
  data <- Data(
    x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
    y = c(0, 0, 0, 0, 0, 0, 1, 0),
    cohort = c(0, 1, 2, 3, 4, 5, 5, 5),
    doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2))
  )

  opening <- OpeningMinDose(min_dose = 10)

  # cohort dose (10) >= min_dose (10), should open
  expect_true(openCohort(opening, cohort = 5, data = data, dose = 20))

  # cohort dose (6) < min_dose (10), should not open
  expect_false(openCohort(opening, cohort = 4, data = data, dose = 20))
})

## OpeningMinCohorts ----

test_that("openCohort works as expected for OpeningMinCohorts when n_cohorts >= min_cohorts", {
  data <- Data(
    x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
    y = c(0, 0, 0, 0, 0, 0, 1, 0),
    cohort = c(1, 2, 3, 4, 5, 6, 6, 6),
    doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2))
  )

  opening <- OpeningMinCohorts(min_cohorts = 3)

  # Test: n_cohorts (6) >= min_cohorts (3), should open
  expect_true(openCohort(opening, cohort = 1, data = data, dose = 50))

  # Test with different min_cohorts
  opening_strict <- OpeningMinCohorts(min_cohorts = 6)
  expect_true(openCohort(opening_strict, cohort = 1, data = data, dose = 50))
})

test_that("openCohort returns FALSE when n_cohorts < min_cohorts for OpeningMinCohorts", {
  data <- Data(
    x = c(0.1, 0.5, 1.5),
    y = c(0, 0, 0),
    cohort = c(1, 2, 3),
    doseGrid = c(0.1, 0.5, 1.5, 3, 6, 10)
  )

  opening <- OpeningMinCohorts(min_cohorts = 5)

  # Test: n_cohorts (3) < min_cohorts (5), should not open
  expect_false(openCohort(opening, cohort = 1, data = data, dose = 50))
})
