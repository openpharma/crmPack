test_that("knit_print.OpeningMinDose works correctly", {
  x <- .DefaultOpeningMinDose()
  expect_equal(
    knit_print(x, asis = FALSE),
    "If the backfill cohort's dose is 0 or higher.\n\n"
  )

  x <- OpeningMinDose(min_dose = 10)
  expect_equal(
    knit_print(x, asis = FALSE),
    "If the backfill cohort's dose is 10 or higher.\n\n"
  )
})

test_that("knit_print.OpeningMinCohorts works correctly", {
  x <- .DefaultOpeningMinCohorts()
  expect_equal(
    knit_print(x, asis = FALSE),
    "If 2 or more cohorts have been treated in total.\n\n"
  )

  x <- OpeningMinCohorts(min_cohorts = 5L)
  expect_equal(
    knit_print(x, asis = FALSE),
    "If 5 or more cohorts have been treated in total.\n\n"
  )
})

test_that("knit_print.OpeningNone works correctly", {
  x <- .DefaultOpeningNone()
  expect_equal(
    knit_print(x, asis = FALSE),
    "No backfill cohorts at all will be opened.\n\n"
  )
})

test_that("knit_print.OpeningMinResponses works correctly", {
  x <- .DefaultOpeningMinResponses()
  expect_equal(
    knit_print(x, asis = FALSE),
    "If 1 or more responses have been observed at this dose.\n\n"
  )

  x <- OpeningMinResponses(min_responses = 2L, include_lower_doses = FALSE)
  expect_equal(
    knit_print(x, asis = FALSE),
    "If 2 or more responses have been observed at this dose.\n\n"
  )

  x <- OpeningMinResponses(min_responses = 3L, include_lower_doses = TRUE)
  expect_equal(
    knit_print(x, asis = FALSE),
    "If 3 or more responses have been observed at this dose or lower.\n\n"
  )
})

test_that("knit_print.OpeningAll works correctly", {
  x <- .DefaultOpeningAll()
  expect_equal(
    knit_print(x, asis = FALSE),
    paste0(
      "If both of the following rules are satisfied:\n\n",
      "-  If the backfill cohort's dose is 0 or higher.\n\n\n",
      "-  If the backfill cohort's dose is 0 or higher.\n\n\n\n"
    )
  )

  x <- OpeningAll(
    OpeningMinDose(min_dose = 5),
    OpeningMinCohorts(min_cohorts = 3L)
  )
  expect_equal(
    knit_print(x, asis = FALSE),
    paste0(
      "If both of the following rules are satisfied:\n\n",
      "-  If the backfill cohort's dose is 5 or higher.\n\n\n",
      "-  If 3 or more cohorts have been treated in total.\n\n\n\n"
    )
  )

  x <- OpeningAll(
    OpeningMinDose(min_dose = 5),
    OpeningMinCohorts(min_cohorts = 3L),
    OpeningMinResponses(min_responses = 2L)
  )
  expect_equal(
    knit_print(x, asis = FALSE),
    paste0(
      "If all of the following rules are satisfied:\n\n",
      "-  If the backfill cohort's dose is 5 or higher.\n\n\n",
      "-  If 3 or more cohorts have been treated in total.\n\n\n",
      "-  If 2 or more responses have been observed at this dose.\n\n\n\n"
    )
  )

  x <- OpeningAll(OpeningMinDose(min_dose = 5))
  expect_equal(
    knit_print(x, asis = FALSE),
    paste0(
      "If this following rule is satisfied:\n\n",
      "-  If the backfill cohort's dose is 5 or higher.\n\n\n\n"
    )
  )
})

test_that("knit_print.OpeningAny works correctly", {
  x <- .DefaultOpeningAny()
  expect_equal(
    knit_print(x, asis = FALSE),
    paste0(
      "If either of the following rules are satisfied:\n\n",
      "-  If the backfill cohort's dose is 0 or higher.\n\n\n",
      "-  If the backfill cohort's dose is 0 or higher.\n\n\n\n"
    )
  )

  x <- OpeningAny(
    OpeningMinDose(min_dose = 5),
    OpeningMinCohorts(min_cohorts = 3L)
  )
  expect_equal(
    knit_print(x, asis = FALSE),
    paste0(
      "If either of the following rules are satisfied:\n\n",
      "-  If the backfill cohort's dose is 5 or higher.\n\n\n",
      "-  If 3 or more cohorts have been treated in total.\n\n\n\n"
    )
  )

  x <- OpeningAny(
    OpeningMinDose(min_dose = 5),
    OpeningMinCohorts(min_cohorts = 3L),
    OpeningMinResponses(min_responses = 2L)
  )
  expect_equal(
    knit_print(x, asis = FALSE),
    paste0(
      "If any of the following rules are satisfied:\n\n",
      "-  If the backfill cohort's dose is 5 or higher.\n\n\n",
      "-  If 3 or more cohorts have been treated in total.\n\n\n",
      "-  If 2 or more responses have been observed at this dose.\n\n\n\n"
    )
  )

  x <- OpeningAny(OpeningNone())
  expect_equal(
    knit_print(x, asis = FALSE),
    paste0(
      "If this following rule is satisfied:\n\n",
      "-  No backfill cohorts at all will be opened.\n\n\n\n"
    )
  )
})

test_that("knit_print.OpeningList works correctly", {
  x <- .DefaultOpeningList()
  expect_equal(
    knit_print(x, asis = FALSE),
    paste0(
      "The following opening rules apply:\n\n",
      "-  If the backfill cohort's dose is 0 or higher.\n\n\n",
      "-  If the backfill cohort's dose is 0 or higher.\n\n\n\n"
    )
  )

  expect_equal(
    knit_print(x, asis = FALSE, indent = 1L),
    paste0(
      "The following opening rules apply:\n\n",
      "    -  If the backfill cohort's dose is 0 or higher.\n\n\n",
      "    -  If the backfill cohort's dose is 0 or higher.\n\n\n\n"
    )
  )

  x <- OpeningList(
    OpeningMinDose(min_dose = 5),
    OpeningMinCohorts(min_cohorts = 3L)
  )
  expect_equal(
    knit_print(x, asis = FALSE),
    paste0(
      "The following opening rules apply:\n\n",
      "-  If the backfill cohort's dose is 5 or higher.\n\n\n",
      "-  If 3 or more cohorts have been treated in total.\n\n\n\n"
    )
  )
})
