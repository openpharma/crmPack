# Generic testing for existence of methods, return type and stability of output
# takes place in test-crmPackClass-methods.R.  This file tests only the
# CORRECTNESS of output.  For simplicity, use asis = FALSE throughout.

test_that("knit_print.RecruitmentUnlimited works correctly", {
  x <- .DefaultRecruitmentUnlimited()
  expect_equal(
    knit_print(x, asis = FALSE),
    "Unlimited recruitment of backfill patients is allowed.\n\n"
  )
})

test_that("knit_print.RecruitmentRatio works correctly", {
  x <- .DefaultRecruitmentRatio()
  expect_equal(
    knit_print(x, asis = FALSE),
    "Backfill patients are recruited at a ratio of 1 per patient in the main trial cohort.\n\n"
  )

  x <- RecruitmentRatio(ratio = 0.5)
  expect_equal(
    knit_print(x, asis = FALSE),
    "Backfill patients are recruited at a ratio of 0.5 per patient in the main trial cohort.\n\n"
  )

  x <- RecruitmentRatio(ratio = 2)
  expect_equal(
    knit_print(x, asis = FALSE),
    "Backfill patients are recruited at a ratio of 2 per patient in the main trial cohort.\n\n"
  )
})
