# Generic testing for existence of methods, return type and stability of output
# takes place in test-crmPackClass-methods.R.  Correctness of output for
# idividual slots is tested in class-specific tests elsewhere. This file tests
# only the correct action of method-specific arguments.  For simplicity,
# use asis = FALSE throughout.

test_that("v_starting_dose works correctly", {
  expect_error(
    StartingDose(-3),
    "starting_dose must be a non-negative, finite number"
  )
  expect_error(
    StartingDose(Inf),
    "starting_dose must be a non-negative, finite number"
  )
  expect_error(
    StartingDose(1:2),
    "starting_dose must be a non-negative, finite number"
  )
})

test_that("h_markdown_header works correctly", {
  expect_equal(
    h_markdown_header("Test header"),
    "\n## Test header\n\n"
  )
  expect_equal(
    h_markdown_header("Test header", level = 3L),
    "\n### Test header\n\n"
  )
})

test_that("h_markdown_header fails gracefully with bad input", {
  expect_error(
    h_markdown_header(NA),
    "Assertion on 'text' failed\\: Contains missing values \\(element 1\\)\\."
  )
  expect_error(
    h_markdown_header(4),
    "Assertion on 'text' failed\\: Must be of type 'character', not 'double'\\."
  )
  expect_error(
    h_markdown_header(c("Heading 1", "Heading 2")),
    "Assertion on 'text' failed\\: Must have length 1, but has length 2\\."
  )
  expect_error(
    h_markdown_header("Title", 3),
    "Assertion on 'level' failed\\: Must be of type 'integer', not 'double'\\."
  )
  expect_error(
    h_markdown_header("Title", 999L),
    "Assertion on 'level' failed\\: Element 1 is not <= 6\\."
  )
  expect_error(
    h_markdown_header("Title", 0L),
    "Assertion on 'level' failed\\: Element 1 is not >= 1\\."
  )
  expect_error(
    h_markdown_header("Title", 1L:3L),
    "Assertion on 'level' failed\\: Must have length 1, but has length 3\\."
  )
})

test_that("h_prepare_section_labels works correctly", {
  x <- .DefaultRuleDesign()
  names <- slotNames(x)

  expect_equal(
    h_prepare_section_labels(x, names),
    names
  )
  expect_equal(
    h_prepare_section_labels(x, names[-1]),
    names[-1]
  )
  expect_equal(
    h_prepare_section_labels(x, c(names, "extraSlot")),
    c(names, "extraSlot")
  )
  expect_equal(
    h_prepare_section_labels(x, names, "extraSlot"),
    names
  )
  expect_equal(
    h_prepare_section_labels(x, names, c("extraSlot", "data")),
    names
  )

  names <- c(
    "nextBest" = "Dose recommendation",
    "cohort_size" = "Cohort size",
    "data" = "Observed data",
    "startingDose" = "Starting dose"
  )
  expectedNames <- names
  expectedNames["data"] <- "Custom label"
  expect_equal(
    h_prepare_section_labels(
      x,
      names,
      c("data" = "Custom label")
    ),
    expectedNames
  )
})

test_that("knit_print-StartingDose works correctly", {
  expect_equal(
    knit_print(StartingDose(10), asis = FALSE),
    "The starting dose is 10.\n\n"
  )
  expect_equal(
    knit_print(StartingDose(10), units = "mg/dL", asis = FALSE),
    "The starting dose is 10 mg/dL.\n\n"
  )
})

test_that("knit_print-RuleDesign works correctly", {
  x <- knit_print(.DefaultRuleDesign(), asis = TRUE)
  expectedSections <- c(
    "Dose recommendation",
    "Cohort size",
    "Observed data",
    "Starting dose"
  )

  # Section headers exist
  expect_true(
    all(stringr::str_detect(x, paste0("### ", expectedSections)))
  )
})
