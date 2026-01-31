# Generic testing for existence of methods, return type and stability of output
# takes place in test-crmPackClass-methods.R.  This file tests only the
# CORRECTNESS of output.  For simplicity, use asis = FALSE throughout.

test_that("knit_print.Backfill works correctly", {
  x <- .DefaultBackfill()
  result <- knit_print(x, asis = FALSE)

  # Check that all components are present
  expect_true(grepl("**Cohort size**:", result, fixed = TRUE))
  expect_true(grepl("**Opening rule**:", result, fixed = TRUE))
  expect_true(grepl("**Recruitment**:", result, fixed = TRUE))
  expect_true(grepl(
    "**Total number of backfill patients**:",
    result,
    fixed = TRUE
  ))
  expect_true(grepl(
    "**Priority of higher vs. lower dose backfill cohorts**:",
    result,
    fixed = TRUE
  ))

  # Check default values
  expect_true(grepl("A constant size of 3 participants", result))
  expect_true(grepl("If the backfill cohort's dose is 0 or higher", result))
  expect_true(grepl(
    "Unlimited recruitment of backfill patients is allowed",
    result
  ))
  expect_true(grepl("Unlimited backfill patients", result))
  expect_true(grepl("highest dose", result))
})

test_that("knit_print.Backfill works with custom values", {
  x <- Backfill(
    cohort_size = CohortSizeConst(size = 5),
    opening = OpeningMinDose(min_dose = 10),
    recruitment = RecruitmentRatio(ratio = 0.5),
    max_size = 20L,
    priority = "lowest"
  )
  result <- knit_print(x, asis = FALSE)

  expect_true(grepl("A constant size of 5 participants", result))
  expect_true(grepl("If the backfill cohort's dose is 10 or higher", result))
  expect_true(grepl(
    "Backfill patients are recruited at a ratio of 0.5",
    result
  ))
  expect_true(grepl("20 backfill patients", result))
  expect_true(grepl("lowest dose", result))
})

test_that("knit_print.Backfill works with complex opening rules", {
  x <- Backfill(
    cohort_size = CohortSizeConst(size = 3),
    opening = OpeningAll(
      OpeningMinDose(min_dose = 5),
      OpeningMinCohorts(min_cohorts = 2L)
    ),
    recruitment = RecruitmentUnlimited(),
    max_size = 100L,
    priority = "random"
  )
  result <- knit_print(x, asis = FALSE)

  expect_true(grepl("**Opening rule**:", result, fixed = TRUE))
  expect_true(grepl("both of the following rules are satisfied", result))
  expect_true(grepl("If the backfill cohort's dose is 5 or higher", result))
  expect_true(grepl("If 2 or more cohorts have been treated", result))
  expect_true(grepl("100 backfill patients", result))
  expect_true(grepl("random dose", result))
})

test_that("knit_print.Backfill works correctly with OpeningNone", {
  x <- Backfill(
    cohort_size = CohortSizeConst(size = 2),
    opening = OpeningNone(),
    recruitment = RecruitmentRatio(ratio = 1),
    max_size = 50L,
    priority = "highest"
  )
  result <- knit_print(x, asis = FALSE)

  expect_match(result, "No backfill cohorts at all will be opened")
})
