# Generic testing for existence of methods, return type and stability of output
# takes place in test-crmPackClass-methods.R.  This file tests only the
# CORRECTNESS of output.  For simplicity, use asis = FALSE throughout.

test_that("knit_print.StoppingCohortsNearDose works correctly", {
  expect_equal(
    knit_print(
      StoppingCohortsNearDose(
        nCohorts = 8,
        percentage = 25,
        report_label = "COHORT_COUNT"
      ),
      asis = FALSE
    ),
    "COHORT_COUNT: If 8 or more cohorts have been treated within 25% of the next best dose."
  )
  expect_equal(
    knit_print(
      StoppingCohortsNearDose(
        nCohorts = 3,
        percentage = 0
      ),
      asis = FALSE
    ),
    "If 3 or more cohorts have been treated at the next best dose."
  )
})

test_that("knit_print.StoppingPatientsNearDose works correctly", {
  expect_equal(
    knit_print(
      StoppingPatientsNearDose(
        nPatients = 8,
        percentage = 25,
        report_label = "PARTICIPANT_COUNT"
      ),
      asis = FALSE
    ),
    "PARTICIPANT_COUNT: If 8 or more participants have been treated within 25% of the next best dose."
  )
  expect_equal(
    knit_print(
      StoppingPatientsNearDose(
        nPatients = 10,
        percentage = 0
      ),
      asis = FALSE,
      label = "subjects"
    ),
    "If 10 or more subjects have been treated at the next best dose."
  )
})

test_that("knit_print.StoppingMissingDose works correctly", {
  expect_equal(
    knit_print(
      StoppingMissingDose(report_label = "MISSING_DOSE"),
      asis = FALSE
    ),
    "MISSING_DOSE: If the dose returned by <code>nextBest()</code> is <code>NA</code>, or if the trial includes a placebo dose, the placebo dose." #nolint
  )
  expect_equal(
    knit_print(
      StoppingMissingDose(),
      asis = FALSE
    ),
    "If the dose returned by <code>nextBest()</code> is <code>NA</code>, or if the trial includes a placebo dose, the placebo dose." #nolint
  )
})
