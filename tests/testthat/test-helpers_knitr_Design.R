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
    h_markdown_header("Title", 999L),
    "Assertion on 'level' failed\\: Element 1 is not <= 6\\."
  )
  expect_error(
    h_markdown_header("Title", 0L),
    "Assertion on 'level' failed\\: Element 1 is not >= 1\\."
  )
  expect_error(
    h_markdown_header("Title", 1L:3L),
    "Assertion on 'level' failed\\: Must have length 1."
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
  x <- knit_print(.DefaultRuleDesign(), asis = FALSE)
  expectedSections <- c(
    "Dose recommendation",
    "Cohort size",
    "Observed data",
    "Starting dose"
  )

  # Section headers exist
  expect_true(stringr::str_detect(x, "## Design"))
  expect_true(
    all(stringr::str_detect(x, paste0("### ", expectedSections)))
  )
  # Initial label counts
  expect_equal(
    stringr::str_count(x, c("toxicities", "toxicity", "participants")),
    c(1, 1, 3)
  )
  # Custom label counts
  expect_equal(
    stringr::str_count(
      knit_print(
        .DefaultRuleDesign(),
        asis = FALSE,
        label = "subject",
        tox_label = "DLAE"
      ),
      # Account for word boundaries
      c("DLAEs\\b", "DLAE\\b", "subjects\\b")
    ),
    c(1, 1, 3)
  )
})

test_that("knit_print-Design works correctly", {
  x <- knit_print(.DefaultDesign(), asis = FALSE)
  expectedSections <- c(
    "nextBest" = "Dose recommendation",
    "cohort_size" = "Cohort size",
    "data" = "Observed data",
    "startingDose" = "Starting dose",
    "increments" = "Escalation rule",
    "stopping" = "Stopping rule",
    "model" = "Dose toxicity model",
    "pl_cohort_size" = "Use of placebo",
    "backfill" = "Backfill cohorts"
  )

  # Section headers exist
  expect_true(stringr::str_detect(x, "## Design"))
  expect_true(all(stringr::str_detect(x, paste0("### ", expectedSections))))
  expect_true(stringr::str_detect(
    x,
    "No backfill cohorts at all will be opened"
  ))

  # Initial label counts
  expected <- c(
    "toxicities" = 2,
    "toxicity" = 6,
    "participants" = 2,
    "patients" = 1,
    "subjects" = 0,
    "cohorts" = 4,
    "DLAE\\b" = 0,
    "DLAEs\\b" = 0
  )
  actual <- stringr::str_count(x, names(expected))
  names(actual) <- names(expected)
  expect_equal(actual, expected)

  # Custom label counts
  x <- knit_print(
    .DefaultDesign(),
    asis = FALSE,
    label = "subject",
    tox_label = "DLAE"
  )
  expected <- c(
    "toxicities" = 0,
    "toxicity" = 1,
    "participants" = 0,
    "patients" = 1,
    "subjects" = 2,
    "cohorts" = 4,
    "DLAE\\b" = 5,
    "DLAEs\\b" = 2
  )
  actual <- stringr::str_count(x, names(expected))
  names(actual) <- names(expected)
  expect_equal(actual, expected)
})

test_that("knit_print-Design works correctly with backfill", {
  x <- .DefaultDesign()
  x@backfill <- Backfill(
    cohort_size = CohortSizeConst(size = 4),
    opening = OpeningMinDose(min_dose = 10),
    recruitment = RecruitmentRatio(ratio = 1),
    total_size = 50L,
    priority = "highest"
  )
  result <- knit_print(x, asis = FALSE)

  expect_match(
    result,
    "**Cohort size**: A constant size of 4 participants",
    fixed = TRUE
  )
  expect_match(
    result,
    "**Opening rule**: If the backfill cohort's dose is 10 or higher",
    fixed = TRUE
  )
  expect_match(
    result,
    "**Recruitment**: Backfill patients are recruited at a ratio of 1",
    fixed = TRUE
  )
  expect_match(
    result,
    "**Total number of backfill patients**: 50 backfill patients",
    fixed = TRUE
  )
  expect_match(
    result,
    "**Priority of higher vs. lower dose backfill cohorts**: highest dose",
    fixed = TRUE
  )
})

test_that("knit_print-DualDesign works correctly", {
  x <- knit_print(.DefaultDualDesign(), asis = FALSE)
  expectedSections <- c(
    "nextBest" = "Dose recommendation",
    "cohort_size" = "Cohort size",
    "data" = "Observed data",
    "startingDose" = "Starting dose",
    "increments" = "Escalation rule",
    "stopping" = "Stopping rule",
    "model" = "Dose-toxicity and dose-biomarker models",
    "pl_cohort_size" = "Use of placebo"
  )

  # Section headers exist
  expect_true(stringr::str_detect(x, "## Design"))
  expect_true(all(stringr::str_detect(x, paste0("### ", expectedSections))))
  # Initial label counts
  expected <- c(
    "toxicities" = 2,
    "toxicity" = 4,
    "participants" = 2,
    "patients" = 1,
    "subjects" = 0,
    "cohort" = 3,
    "Cohort" = 3,
    "cohorts" = 0,
    "DLAE\\b" = 0,
    "DLAEs\\b" = 0,
    "biomarker" = 8,
    "Biomarker" = 1
  )
  actual <- stringr::str_count(x, names(expected))
  names(actual) <- names(expected)
  expect_equal(actual, expected)

  # Custom label counts
  x <- knit_print(
    .DefaultDualDesign(),
    asis = TRUE,
    label = "subject",
    tox_label = "DLAE"
  )
  expected <- c(
    "toxicities" = 0,
    "toxicity" = 0,
    "participants" = 0,
    "patients" = 1,
    "subjects" = 2,
    "cohort" = 3,
    "Cohort" = 3,
    "cohorts" = 0,
    "DLAE\\b" = 4,
    "DLAEs\\b" = 2,
    "biomarker" = 8,
    "Biomarker" = 1
  )
  actual <- stringr::str_count(x, names(expected))
  names(actual) <- names(expected)
  expect_equal(actual, expected)
})

test_that("knit_print-DADesign works correctly", {
  x <- knit_print(.DefaultDADesign(), asis = FALSE)
  expectedSections <- c(
    "nextBest" = "Dose recommendation",
    "cohort_size" = "Cohort size",
    "data" = "Observed data",
    "startingDose" = "Starting dose",
    "increments" = "Escalation rule",
    "stopping" = "Stopping rule",
    "model" = "Dose toxicity model",
    "pl_cohort_size" = "Use of placebo"
  )

  # Section headers exist
  expect_true(stringr::str_detect(x, "## Design"))
  expect_true(all(stringr::str_detect(x, paste0("### ", expectedSections))))
  # Initial label counts
  expected <- c(
    "toxicities" = 2,
    "toxicity" = 6,
    "participants" = 6,
    "participant\\b" = 1,
    "patients" = 1,
    "subjects" = 0,
    "cohort\\b" = 7,
    "Cohort" = 3,
    "cohorts" = 1,
    "DLAE\\b" = 0,
    "DLAEs\\b" = 0
  )
  actual <- stringr::str_count(x, names(expected))
  names(actual) <- names(expected)
  expect_equal(actual, expected)

  # Custom label counts
  x <- knit_print(
    .DefaultDADesign(),
    asis = TRUE,
    label = "subject",
    tox_label = "DLAE"
  )
  expected <- c(
    "toxicities" = 0,
    "toxicity" = 1,
    "participants" = 0,
    "patients" = 1,
    "subjects" = 6,
    "cohort" = 8,
    "Cohort" = 3,
    "cohorts" = 1,
    "DLAE\\b" = 5,
    "DLAEs\\b" = 2
  )
  actual <- stringr::str_count(x, names(expected))
  names(actual) <- names(expected)
  expect_equal(actual, expected)
})

test_that("knit_print-DTDDesign works correctly", {
  x <- knit_print(.DefaultTDDesign(), asis = FALSE)
  expectedSections <- c(
    "nextBest" = "Dose recommendation",
    "cohort_size" = "Cohort size",
    "data" = "Observed data",
    "startingDose" = "Starting dose",
    "increments" = "Escalation rule",
    "stopping" = "Stopping rule",
    "model" = "Dose toxicity model",
    "pl_cohort_size" = "Use of placebo"
  )

  # Section headers exist
  expect_true(stringr::str_detect(x, "## Design"))
  expect_true(all(stringr::str_detect(x, paste0("### ", expectedSections))))
  # Initial label counts
  expected <- c(
    "toxicities" = 0,
    "toxicity" = 3,
    "participants" = 3,
    "participant\\b" = 0,
    "patients" = 1,
    "subjects" = 0,
    "cohort\\b" = 1,
    "Cohort" = 1,
    "cohorts" = 0,
    "DLAE\\b" = 1,
    "DLAEs\\b" = 2
  )
  actual <- stringr::str_count(x, names(expected))
  names(actual) <- names(expected)
  expect_equal(actual, expected)

  # Custom label counts
  x <- knit_print(
    .DefaultTDDesign(),
    asis = TRUE,
    label = "subject",
    tox_label = "toxicity"
  )
  expected <- c(
    "toxicities" = 2,
    "toxicity" = 4,
    "participants" = 0,
    "patients" = 1,
    "subjects" = 3,
    "cohort" = 1,
    "Cohort" = 1,
    "cohorts" = 0,
    "DLAE\\b" = 0,
    "DLAEs\\b" = 0
  )
  actual <- stringr::str_count(x, names(expected))
  names(actual) <- names(expected)
  expect_equal(actual, expected)
})

test_that("knit_print-DualResponsesDesign works correctly", {
  x <- knit_print(.DefaultDualResponsesDesign(), asis = FALSE)
  expectedSections <- c(
    "nextBest" = "Dose recommendation",
    "cohort_size" = "Cohort size",
    "data" = "Observed data",
    "startingDose" = "Starting dose",
    "increments" = "Escalation rule",
    "stopping" = "Stopping rule",
    "model" = "Dose-toxicity model",
    "pl_cohort_size" = "Use of placebo",
    "eff_model" = "Dose-efficacy model"
  )

  # Section headers exist
  expect_true(stringr::str_detect(x, "## Design"))
  expect_true(all(stringr::str_detect(x, paste0("### ", expectedSections))))
  # Initial label counts
  expected <- c(
    "toxicities" = 0,
    "toxicity" = 3,
    "participants\\b" = 4,
    "participants'" = 1,
    "participant\\b" = 1,
    "patients" = 1,
    "subjects" = 0,
    "cohort\\b" = 1,
    "Cohort" = 1,
    "cohorts" = 0,
    "DLAE\\b" = 2,
    "DLAEs\\b" = 2
  )
  actual <- stringr::str_count(x, names(expected))
  names(actual) <- names(expected)
  expect_equal(actual, expected)

  # Custom label counts
  x <- knit_print(
    .DefaultDualResponsesDesign(),
    asis = TRUE,
    label = "subject",
    tox_label = "toxicity",
    eff_label = "CRP"
  )
  expected <- c(
    "toxicities" = 2,
    "toxicity" = 5,
    "participants\\b" = 0,
    "participants'\\b" = 0,
    "participant\\b" = 0,
    "subjects\\b" = 4,
    "subjects\\." = 1,
    "subjects'" = 1,
    "subject\\b" = 1,
    "patients" = 1,
    "subjects" = 4,
    "cohort\\b" = 1,
    "Cohort" = 1,
    "cohorts" = 0,
    "DLAE\\b" = 0,
    "DLAEs\\b" = 0
  )
  actual <- stringr::str_count(x, names(expected))
  names(actual) <- names(expected)
  expect_equal(actual, expected)
})

test_that("knit_print-DualResponsesSamplesDesign works correctly", {
  x <- knit_print(.DefaultDualResponsesSamplesDesign(), asis = FALSE)
  expectedSections <- c(
    "nextBest" = "Dose recommendation",
    "cohort_size" = "Cohort size",
    "data" = "Observed data",
    "startingDose" = "Starting dose",
    "increments" = "Escalation rule",
    "stopping" = "Stopping rule",
    "model" = "Dose-toxicity model",
    "pl_cohort_size" = "Use of placebo",
    "eff_model" = "Dose-efficacy model"
  )

  # Section headers exist
  expect_true(stringr::str_detect(x, "## Design"))
  expect_true(all(stringr::str_detect(x, paste0("### ", expectedSections))))
  # Initial label counts
  expected <- c(
    "toxicities" = 0,
    "toxicity" = 3,
    "participants\\b" = 4,
    "participants'" = 1,
    "participant\\b" = 1,
    "patients" = 1,
    "subjects" = 0,
    "cohort\\b" = 1,
    "Cohort" = 1,
    "cohorts" = 0,
    "DLAE\\b" = 2,
    "DLAEs\\b" = 2
  )
  actual <- stringr::str_count(x, names(expected))
  names(actual) <- names(expected)
  expect_equal(actual, expected)

  # Custom label counts
  x <- knit_print(
    .DefaultDualResponsesSamplesDesign(),
    asis = TRUE,
    label = "subject",
    tox_label = "toxicity",
    eff_label = "CRP"
  )
  expected <- c(
    "toxicities" = 2,
    "toxicity" = 5,
    "participants\\b" = 0,
    "participants'\\b" = 0,
    "participant\\b" = 0,
    "subjects\\b" = 4,
    "subjects\\." = 1,
    "subjects'" = 1,
    "subject\\b" = 1,
    "patients" = 1,
    "subjects" = 4,
    "cohort\\b" = 1,
    "Cohort" = 1,
    "cohorts" = 0,
    "DLAE\\b" = 0,
    "DLAEs\\b" = 0
  )
  actual <- stringr::str_count(x, names(expected))
  names(actual) <- names(expected)
  expect_equal(actual, expected)
})

test_that("knit_print-RuleDesignOrdinal works correctly", {
  x <- knit_print(.DefaultRuleDesignOrdinal(), asis = FALSE)
  expectedSections <- c(
    "nextBest" = "Dose recommendation",
    "cohort_size" = "Cohort size",
    "data" = "Observed data",
    "startingDose" = "Starting dose"
  )

  # Section headers exist
  expect_true(stringr::str_detect(x, "## Design"))
  expect_true(all(stringr::str_detect(x, paste0("### ", expectedSections))))
  # Initial label counts
  expected <- c(
    "toxicities" = 0,
    "toxicity" = 4,
    "participants\\b" = 2,
    "participant\\b" = 0,
    "patients" = 0,
    "subjects" = 0,
    "cohort\\b" = 2,
    "Cohort" = 1,
    "cohorts" = 0,
    "DLAE\\b" = 0,
    "DLAEs\\b" = 0
  )
  actual <- stringr::str_count(x, names(expected))
  names(actual) <- names(expected)
  expect_equal(actual, expected)

  # Custom label counts
  x <- knit_print(
    .DefaultRuleDesignOrdinal(),
    asis = TRUE,
    label = "subject",
    tox_label = "DLAE",
    eff_label = "CRP"
  )
  expected <- c(
    "toxicities" = 0,
    "toxicity" = 0,
    "participants\\b" = 0,
    "participant\\b" = 0,
    "patients" = 0,
    "subjects" = 2,
    "cohort\\b" = 2,
    "Cohort" = 1,
    "cohorts" = 0,
    "DLAE\\b" = 4,
    "DLAEs\\b" = 0
  )
  actual <- stringr::str_count(x, names(expected))
  names(actual) <- names(expected)
  expect_equal(actual, expected)
})
