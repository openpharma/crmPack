# Generic testing for existence of methods, return type and stability of output
# takes place in test-crmPackClass-methods.R.  This file tests only the
# CORRECTNESS of output.  For simplicity, use asis = FALSE throughout.

test_that("knit_print.SafetyWindowConst works correctly", {
  x <- .DefaultSafetyWindowConst()

  expect_equal(
    knit_print(x, asis = FALSE),
    paste0(
      "To protect the welfare of individual participants, the rate of enrolment ",
      "within each cohort will be restricted.\n\nFor all cohorts:\n\n- The gap ",
      "between consecutive enrolments should always be at least 7 days.\n\n",
      "Before the next cohort can open, all participants in the current cohort ",
      "must have been followed up for at least 7 days and at least one ",
      "participant must have been followed up for at least 14 days.\n\n"
    )
  )

  x <- SafetyWindowConst(
    gap = c(7, 5, 3),
    follow = 7,
    follow_min = 14
  )

  expect_equal(
    knit_print(x, asis = FALSE),
    paste0(
      "To protect the welfare of individual participants, the rate of enrolment ",
      "within each cohort will be restricted.\n\nFor all cohorts:\n\n",
      "-  The gap between the enrolment of the first and the second participants ",
      "in the cohort should be at least 7 days\n\n",
      "-  The gap between the enrolment of the second and the third participants ",
      "in the cohort should be at least 5 days\n",
      "- The gap between all subsequent participants should be at least 3 days\n\n",
      "Before the next cohort can open, all participants in the current cohort ",
      "must have been followed up for at least 7 days and at least one ",
      "participant must have been followed up for at least 14 days.\n\n"
    )
  )
  expect_equal(
    knit_print(
      x,
      asis = FALSE,
      time_unit = "hour",
      label = "volunteer",
      ordinals = c("premier", "deuxieme", "troisieme")
    ),
    paste0(
      "To protect the welfare of individual volunteers, the rate of enrolment ",
      "within each cohort will be restricted.\n\nFor all cohorts:\n\n",
      "-  The gap between the enrolment of the premier and the deuxieme volunteers ",
      "in the cohort should be at least 7 hours\n\n",
      "-  The gap between the enrolment of the deuxieme and the troisieme volunteers ",
      "in the cohort should be at least 5 hours\n",
      "- The gap between all subsequent volunteers should be at least 3 hours\n\n",
      "Before the next cohort can open, all volunteers in the current cohort ",
      "must have been followed up for at least 7 hours and at least one ",
      "volunteer must have been followed up for at least 14 hours.\n\n"
    )
  )
})

test_that("knit_print.SafetyWindowConst fails gracefully with bad input", {
  x <- SafetyWindowConst(
    gap = c(7, 5, 3),
    follow = 7,
    follow_min = 14
  )

  expect_error(
    knit_print(x, label = FALSE),
    "Assertion on 'x' failed: Must be of type 'character', not 'logical'."
  )
  expect_error(
    knit_print(x, time_unit = FALSE),
    "Assertion on 'time_unit' failed: Must be of type 'character', not 'logical'\\."
  )
  expect_error(
    knit_print(x, ordinals = 1:10),
    "Assertion on 'ordinals' failed: Must be of type 'character', not 'integer'\\."
  )
  expect_error(
    knit_print(x, ordinals = c("tooShort")),
    "Assertion on 'ordinals' failed: Must have length >= 2, but has length 1\\."
  )
})

test_that("knit_print.SafetyWindowSize works correctly", {
  x <- .DefaultSafetyWindowSize()

  expect_equal(
    knit_print(x, asis = FALSE),
    paste0(
      "To protect the welfare of individual participants, the rate of enrolment ",
      "within each cohort will be restricted.\n\n## For cohort sizes of less ",
      "than 4\n\n-  The gap between the enrolment of the first and the second ",
      "participants in the cohort should be at least 7 days\n- The gap between ",
      "all subsequent participants should be at least 3 days\n\n\n## For ",
      "cohort sizes of 4 or more\n\n-  The gap between the enrolment of the ",
      "first and the second participants in the cohort should be at least 9 ",
      "days\n- The gap between all subsequent participants should be at least ",
      "5 days\n\nFor all cohorts, before the next cohort can open, all ",
      "participants in the current cohort must have been followed up for at ",
      "least 7 days and at least one participant must have been followed up ",
      "for at least 14 days.\n\n"
    )
  )
})

test_that("knit_print.SafetyWindowSize fails gracefully with bad input", {
  x <- SafetyWindowSize(
    gap = list(c(7, 3), c(9, 5, 3), c(13, 10, 7, 5)),
    size = c(1, 4, 8),
    follow = 7,
    follow_min = 14
  )

  expect_error(
    knit_print(x, label = FALSE),
    "Assertion on 'x' failed: Must be of type 'character', not 'logical'."
  )
  expect_error(
    knit_print(x, time_unit = FALSE),
    "Assertion on 'time_unit' failed: Must be of type 'character', not 'logical'."
  )
  expect_error(
    knit_print(x, ordinals = 1:10),
    "Assertion on 'ordinals' failed: Must be of type 'character', not 'integer'."
  )
  expect_error(
    knit_print(x, ordinals = c("tooShort")),
    "Assertion on 'ordinals' failed: Must have length >= 2, but has length 1."
  )
  expect_error(
    knit_print(x, level = -1),
    "Assertion on 'level' failed: Must be of type 'integer', not 'double'."
  )
  expect_error(
    knit_print(x, level = -1L),
    "Assertion on 'level' failed: Element 1 is not >= 1."
  )
})
