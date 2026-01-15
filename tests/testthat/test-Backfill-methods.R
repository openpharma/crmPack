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

## OpeningAll ----

test_that("openCohort works correctly with OpeningAll (AND logic)", {
  data <- Data(
    x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
    y = c(0, 0, 0, 0, 0, 0, 1, 0),
    cohort = c(1, 2, 3, 4, 5, 6, 6, 6),
    doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2))
  )

  # Both criteria: dose >= 10 AND at least 5 cohorts treated
  opening1 <- OpeningMinDose(min_dose = 10)
  opening2 <- OpeningMinCohorts(min_cohorts = 5)
  opening_all <- OpeningAll(opening1, opening2)

  # Cohort 6 dose = 10 (>= 10: TRUE), max cohort = 6 (>= 5: TRUE), AND = TRUE
  expect_true(openCohort(opening_all, cohort = 6, data = data, dose = 20))

  # Cohort 4 dose = 3 (>= 10: FALSE), max cohort = 6 (>= 5: TRUE), AND = FALSE
  expect_false(openCohort(opening_all, cohort = 4, data = data, dose = 20))
})

test_that("openCohort works with & operator", {
  data <- Data(
    x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
    y = c(0, 0, 0, 0, 0, 0, 1, 0),
    cohort = c(1, 2, 3, 4, 5, 6, 6, 6),
    doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2))
  )

  opening1 <- OpeningMinDose(min_dose = 10)
  opening2 <- OpeningMinCohorts(min_cohorts = 5)
  opening_all <- opening1 & opening2

  expect_true(openCohort(opening_all, cohort = 6, data = data, dose = 20))
  expect_false(openCohort(opening_all, cohort = 4, data = data, dose = 20))
})

## OpeningAny ----

test_that("openCohort works correctly with OpeningAny (OR logic)", {
  data <- Data(
    x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
    y = c(0, 0, 0, 0, 0, 0, 1, 0),
    cohort = c(1, 2, 3, 4, 5, 6, 6, 6),
    doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2))
  )

  # Either criterion: dose >= 20 OR at least 5 cohorts treated
  opening1 <- OpeningMinDose(min_dose = 20)
  opening2 <- OpeningMinCohorts(min_cohorts = 5)
  opening_any <- OpeningAny(opening1, opening2)

  # Cohort 6 dose = 10 (>= 20: FALSE), max cohort = 6 (>= 5: TRUE), OR = TRUE
  expect_true(openCohort(opening_any, cohort = 6, data = data, dose = 20))

  # Cohort at dose 25 would be >= 20 (TRUE), so OR = TRUE
  expect_true(openCohort(opening_any, cohort = 1, data = data, dose = 25))
})

test_that("openCohort works with | operator", {
  data <- Data(
    x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
    y = c(0, 0, 0, 0, 0, 0, 1, 0),
    cohort = c(1, 2, 3, 4, 5, 6, 6, 6),
    doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2))
  )

  opening1 <- OpeningMinDose(min_dose = 20)
  opening2 <- OpeningMinCohorts(min_cohorts = 5)
  opening_any <- opening1 | opening2

  expect_true(openCohort(opening_any, cohort = 6, data = data, dose = 20))
  expect_true(openCohort(opening_any, cohort = 2, data = data, dose = 25))
})

# maxRecruits ----

## RecruitmentUnlimited ----

test_that("maxRecruits works for RecruitmentUnlimited with small cohort", {
  object <- RecruitmentUnlimited()
  result <- maxRecruits(object, active_cohort_size = 1L)
  expect_equal(result, 1e6L)
})

test_that("maxRecruits works for RecruitmentUnlimited with moderate cohort", {
  object <- RecruitmentUnlimited()
  result <- maxRecruits(object, active_cohort_size = 10L)
  expect_equal(result, 1e6L)
})

test_that("maxRecruits works for RecruitmentUnlimited with large cohort", {
  object <- RecruitmentUnlimited()
  result <- maxRecruits(object, active_cohort_size = 1000L)
  expect_equal(result, 1e6L)
})

## RecruitmentRatio ----

test_that("maxRecruits works for RecruitmentRatio with ratio = 0.5", {
  object <- RecruitmentRatio(ratio = 0.5)
  result <- maxRecruits(object, active_cohort_size = 10L)
  expect_equal(result, 5L)
})

test_that("maxRecruits works for RecruitmentRatio with ratio = 1", {
  object <- RecruitmentRatio(ratio = 1)
  result <- maxRecruits(object, active_cohort_size = 10L)
  expect_equal(result, 10L)
})

test_that("maxRecruits works for RecruitmentRatio with ratio = 2", {
  object <- RecruitmentRatio(ratio = 2)
  result <- maxRecruits(object, active_cohort_size = 10L)
  expect_equal(result, 20L)
})

test_that("maxRecruits applies ceiling correctly for RecruitmentRatio", {
  object <- RecruitmentRatio(ratio = 0.5)
  result <- maxRecruits(object, active_cohort_size = 7L)
  expect_equal(result, 4L)
})

test_that("maxRecruits handles ratio = 0 for RecruitmentRatio", {
  object <- RecruitmentRatio(ratio = 0)
  result <- maxRecruits(object, active_cohort_size = 10L)
  expect_equal(result, 0L)
})

test_that("maxRecruits handles small cohort size for RecruitmentRatio", {
  object <- RecruitmentRatio(ratio = 0.5)
  result <- maxRecruits(object, active_cohort_size = 1L)
  expect_equal(result, 1L)
})
