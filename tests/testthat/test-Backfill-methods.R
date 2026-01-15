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

## OpeningNone ----

test_that("openCohort always returns FALSE for OpeningNone", {
  data <- Data(
    x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
    y = c(0, 0, 0, 0, 0, 0, 1, 0),
    cohort = c(1, 2, 3, 4, 5, 6, 6, 6),
    doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2))
  )

  opening <- OpeningNone()

  # OpeningNone always returns FALSE regardless of cohort or dose
  expect_false(openCohort(opening, cohort = 1, data = data, dose = 20))
  expect_false(openCohort(opening, cohort = 5, data = data, dose = 50))
  expect_false(openCohort(opening, cohort = 6, data = data, dose = 80))
})

## OpeningMinResponses ----

test_that("openCohort works for OpeningMinResponses with include_lower_doses = FALSE", {
  data <- Data(
    x = c(10, 10, 20, 20, 20, 30, 30, 30),
    y = c(0, 0, 0, 0, 0, 0, 0, 0),
    response = c(1, 1, 1, 1, 0, 1, 1, 0),
    cohort = c(1, 1, 2, 2, 2, 3, 3, 3),
    doseGrid = seq(10, 100, by = 10)
  )

  opening <- OpeningMinResponses(min_responses = 2, include_lower_doses = FALSE)

  # At dose 20, there are 2 responses, should open
  expect_true(openCohort(opening, cohort = 2, data = data, dose = 40))

  # At dose 10, there are 2 responses, should open
  expect_true(openCohort(opening, cohort = 1, data = data, dose = 40))

  # At dose 30, there are 2 responses, should open
  expect_true(openCohort(opening, cohort = 3, data = data, dose = 40))
})

test_that("openCohort works for OpeningMinResponses with include_lower_doses = TRUE", {
  data <- Data(
    x = c(10, 10, 20, 20, 20, 30, 30, 30),
    y = c(0, 0, 0, 0, 0, 0, 0, 0),
    response = c(0, 1, 0, 1, 0, 1, 1, 0),
    cohort = c(1, 1, 2, 2, 2, 3, 3, 3),
    doseGrid = seq(10, 100, by = 10)
  )

  opening <- OpeningMinResponses(min_responses = 2, include_lower_doses = TRUE)

  # At dose 20 with include_lower_doses:
  # Dose 10: 1 response, Dose 20: 1 response = 2 total >= 2, should open
  expect_true(openCohort(opening, cohort = 2, data = data, dose = 40))

  # At dose 10 with include_lower_doses:
  # Only dose 10: 1 response < 2, should not open
  expect_false(openCohort(opening, cohort = 1, data = data, dose = 40))

  # At dose 30 with include_lower_doses:
  # Dose 10: 1 response, Dose 20: 1 response, Dose 30: 2 responses = 4 total >= 2, should open
  expect_true(openCohort(opening, cohort = 3, data = data, dose = 40))
})

test_that("openCohort returns FALSE when cohort dose is NA for OpeningMinResponses", {
  data <- Data(
    x = c(10, 10, 20),
    y = c(0, 0, 0),
    response = c(1, 1, 1),
    cohort = c(1, 1, 2),
    doseGrid = seq(10, 100, by = 10)
  )

  opening <- OpeningMinResponses(min_responses = 1)

  # Cohort 5 has NA dose, should return FALSE
  expect_false(openCohort(opening, cohort = 5, data = data, dose = 30))
})
