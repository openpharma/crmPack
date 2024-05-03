# Generic testing for existence of methods, return type and stability of output
# takes place in test-crmPackClass-methods.R.  This file tests only the
# CORRECTNESS of output.  For simplicity, use asis = FALSE throughout.

test_that("knit_print.StoppingOrdinal works correctly", {
  x <- .DefaultStoppingOrdinal()
  x@report_label <- "ORDINAL"
  expect_equal(
    knit_print(x, asis = FALSE),
    paste0(
      "ORDINAL: Based on a toxicity grade of 1: P(0.2 ≤ prob(DLE | NBD) ≤ 0.35)",
      " ≥ 0.6: If the probability of toxicity at the next best dose is in the ",
      "range [0.20, 0.35] is at least 0.60.\n\n"
    )
  )
  x@report_label <- NA_character_
  expect_equal(
    knit_print(x, asis = FALSE),
    paste0(
      "Based on a toxicity grade of 1: P(0.2 ≤ prob(DLE | NBD) ≤ 0.35)",
      " ≥ 0.6: If the probability of toxicity at the next best dose is in the ",
      "range [0.20, 0.35] is at least 0.60.\n\n"
    )
  )
})

test_that("knit_print.StoppingAll works correctly", {
  x <- .DefaultStoppingAll()
  x@report_label <- "LIST_ALL"
  expect_equal(
    knit_print(x, asis = FALSE),
    paste0(
      "LIST_ALL: If all of the following rules are `TRUE`:\n\n",
      "-  ≥ 3 cohorts dosed: If 3 or more cohorts have been treated.\n\n\n",
      "-  P(0.2 ≤ prob(DLE | NBD) ≤ 0.35) ≥ 0.5: If the probability of ",
      "toxicity at the next best dose is in the range [0.20, 0.35] is at ",
      "least 0.50.\n\n\n",
      "-  ≥ 20 patients dosed: If 20 or more participants have been treated.\n\n\n\n"
    )
  )
  x@report_label <- NA_character_
  expect_equal(
    knit_print(x, asis = FALSE),
    paste0(
      "If all of the following rules are `TRUE`:\n\n",
      "-  ≥ 3 cohorts dosed: If 3 or more cohorts have been treated.\n\n\n",
      "-  P(0.2 ≤ prob(DLE | NBD) ≤ 0.35) ≥ 0.5: If the probability of ",
      "toxicity at the next best dose is in the range [0.20, 0.35] is at ",
      "least 0.50.\n\n\n",
      "-  ≥ 20 patients dosed: If 20 or more participants have been treated.\n\n\n\n"
    )
  )
})

test_that("knit_print.StoppingAny works correctly", {
  x <- .DefaultStoppingAny()
  x@report_label <- "LIST_ANY"
  expect_equal(
    knit_print(x, asis = FALSE),
    paste0(
      "LIST_ANY: If any of the following rules are `TRUE`:\n\n",
      "-  ≥ 3 cohorts dosed: If 3 or more cohorts have been treated.\n\n\n",
      "-  P(0.2 ≤ prob(DLE | NBD) ≤ 0.35) ≥ 0.5: If the probability of ",
      "toxicity at the next best dose is in the range [0.20, 0.35] is at ",
      "least 0.50.\n\n\n",
      "-  ≥ 20 patients dosed: If 20 or more participants have been treated.\n\n\n\n"
    )
  )
  x@report_label <- NA_character_
  expect_equal(
    knit_print(x, asis = FALSE),
    paste0(
      "If any of the following rules are `TRUE`:\n\n",
      "-  ≥ 3 cohorts dosed: If 3 or more cohorts have been treated.\n\n\n",
      "-  P(0.2 ≤ prob(DLE | NBD) ≤ 0.35) ≥ 0.5: If the probability of ",
      "toxicity at the next best dose is in the range [0.20, 0.35] is at ",
      "least 0.50.\n\n\n",
      "-  ≥ 20 patients dosed: If 20 or more participants have been treated.\n\n\n\n"
    )
  )
})

test_that("knit_print.StoppingList works correctly", {
  x <- .DefaultStoppingList()
  x@report_label <- "LIST"
  expect_equal(
    knit_print(x, asis = FALSE),
    paste0(
      "LIST: If the result of applying the summary function to the following ",
      "rules is `TRUE`:\n\n",
      "-  ≥ 3 cohorts dosed: If 3 or more cohorts have been treated.\n\n\n",
      "-  P(0.2 ≤ prob(DLE | NBD) ≤ 0.35) ≥ 0.5: If the probability of ",
      "toxicity at the next best dose is in the range [0.20, 0.35] is at ",
      "least 0.50.\n\n\n",
      "-  ≥ 20 patients dosed: If 20 or more participants have been treated.\n\n\n\n"
    )
  )
  x@report_label <- NA_character_
  expect_equal(
    knit_print(x, asis = FALSE),
    paste0(
      "If the result of applying the summary function to the following ",
      "rules is `TRUE`:\n\n",
      "-  ≥ 3 cohorts dosed: If 3 or more cohorts have been treated.\n\n\n",
      "-  P(0.2 ≤ prob(DLE | NBD) ≤ 0.35) ≥ 0.5: If the probability of ",
      "toxicity at the next best dose is in the range [0.20, 0.35] is at ",
      "least 0.50.\n\n\n",
      "-  ≥ 20 patients dosed: If 20 or more participants have been treated.\n\n\n\n"
    )
  )
})

test_that("knit_print.StoppingMaxGainCIRatio works correctly", {
  expect_equal(
    knit_print(
      StoppingMaxGainCIRatio(target_ratio = 4, prob_target = 0.3, report_label = "MAXGAINCI_RATIO"),
      asis = FALSE
    ),
    paste0(
      "MAXGAINCI_RATIO: If the ratio of the upper to the lower limit of the ",
      "posterior 95% credible interval for the probability of toxicity at the ",
      "target dose (the smaller of the MTD for 30% target and GStar) ",
      "is less than or equal to 4.\n\n"
    )
  )
  expect_equal(
    knit_print(
      StoppingMaxGainCIRatio(target_ratio = 4, prob_target = 0.3),
      asis = FALSE,
      dose_label = "the MTD"
    ),
    paste0(
      "GStar 4 for 0.3 target prob: If the ratio of the upper to the lower ",
      "limit of the posterior 95% credible interval for the probability ",
      "of toxicity at the target dose (the smaller of the MTD for 30% target ",
      "and GStar) is less than or equal to 4.\n\n"
    )
  )
})

test_that("knit_print.StoppingTDCIRatio works correctly", {
  expect_equal(
    knit_print(
      StoppingTDCIRatio(target_ratio = 4, prob_target = 0.3, report_label = "TDCI_RATIO"),
      asis = FALSE
    ),
    paste0(
      "TDCI_RATIO: If, at the next best dose, the ratio of the upper to the lower limit of the ",
      "posterior 95% credible interval for toxicity (targetting 30%) is less ",
      "than or equal to 4.\n\n"
    )
  )
  expect_equal(
    knit_print(
      StoppingTDCIRatio(target_ratio = 4, prob_target = 0.3),
      asis = FALSE,
      dose_label = "the MTD"
    ),
    paste0(
      "TD 4 for 0.3 target prob: If, at the MTD, the ratio of the upper to the lower limit of the ",
      "posterior 95% credible interval for toxicity (targetting 30%) is less ",
      "than or equal to 4.\n\n"
    )
  )
})

test_that("knit_print.StoppingTargetBiomarker works correctly", {
  expect_equal(
    knit_print(
      StoppingTargetBiomarker(target = c(0.9, 1.0), prob = 0.5, report_label = "BIOMARKER"),
      asis = FALSE
    ),
    paste0(
      "BIOMARKER: If, at the next best dose, the posterior probability that the ",
      "target biomarker is in the range (0.90, 1.00), relative to the maximum ",
      "value of the target biomarker, is 50% or more.\n\n"
    )
  )
  expect_equal(
    knit_print(
      StoppingTargetBiomarker(target = c(300, 500), is_relative = FALSE, prob = 0.75),
      asis = FALSE,
      dose_label = "the MTD",
      biomarker_label = "hs-CRP"
    ),
    paste0(
      "P(300 ≤ Biomarker ≤ 500) ≥ 0.75 (absolute): If, at the MTD, the ",
      "posterior probability that hs-CRP is in the range (300.00, 500.00) ",
      "is 75% or more.\n\n"
    )
  )
})

test_that("knit_print.StoppingLowestDoseHSRBeta works correctly", {
  expect_equal(
    knit_print(
      StoppingLowestDoseHSRBeta(target = 0.33, a = 1, b = 1, report_label = "HSR"),
      asis = FALSE
    ),
    paste0(
      "HSR: If, using a Hard Stopping Rule with a prior of Beta(1, 1), the ",
      "lowest dose in the dose grid has a posterior probability of toxicity of ",
      "95% or more.\n\n"
    )
  )
  expect_equal(
    knit_print(
      StoppingLowestDoseHSRBeta(target = 0.33, a = 3, b = 2, prob = 0.5),
      asis = FALSE
    ),
    paste0(
      "Pβ(lowest dose > P(DLE) = 0.33) > 0.5: If, using a Hard Stopping Rule ",
      "with a prior of Beta(3, 2), the lowest dose in the dose grid has a ",
      "posterior probability of toxicity of 50% or more.\n\n"
    )
  )
})

test_that("knit_print.StoppingMTDCV works correctly", {
  expect_equal(
    knit_print(
      StoppingMTDCV(target = 0.33, thresh = 0.5, report_label = "MTD_CV"),
      asis = FALSE
    ),
    paste0(
      "MTD_CV: If the posterior estimate of the robust coefficient of ",
      "variation of the MTD (targetting 33%), is than or equal to 50%.\n\n"
    )
  )
  expect_equal(
    knit_print(
      StoppingMTDCV(target = 0.33, thresh = 0.5),
      asis = FALSE
    ),
    paste0(
      "CV(MTD) > 0.33: If the posterior estimate of the robust coefficient ",
      "of variation of the MTD (targetting 33%), is than or equal to 50%.\n\n"
    )
  )
})

test_that("knit_print.StoppingMTDdistribution works correctly", {
  expect_equal(
    knit_print(
      StoppingMTDdistribution(target = 0.33, thresh = 0.5, prob = 0.9, report_label = "MTD_DIST"),
      asis = FALSE
    ),
    paste0(
      "MTD_DIST: If the mean posterior probability of toxicity at 50% of the ",
      "next best dose is at least 0.90.\n\n"
    )
  )
  expect_equal(
    knit_print(
      StoppingMTDdistribution(target = 0.33, thresh = 0.5, prob = 0.9),
      asis = FALSE
    ),
    paste0(
      "P(MTD > 0.5 * NBD | P(DLE) = 0.33) ≥ 0.9: If the mean posterior ",
      "probability of toxicity at 50% of the next best dose is at least 0.90.\n\n"
    )
  )
})

test_that("knit_print.StoppingHighestDose works correctly", {
  expect_equal(
    knit_print(
      StoppingHighestDose(report_label = "HIGHEST_DOSE"),
      asis = FALSE
    ),
    "HIGHEST_DOSE: If the next best dose is the highest dose in the dose grid.\n\n"
  )
  expect_equal(
    knit_print(
      StoppingHighestDose(),
      asis = FALSE
    ),
    "NBD is the highest dose: If the next best dose is the highest dose in the dose grid.\n\n"
  )
})

test_that("knit_print.StoppingSpecificDose works correctly", {
  expect_equal(
    knit_print(
      StoppingSpecificDose(
        dose = 100,
        rule = StoppingTargetProb(target = c(0.1, 0.25), prob = 0.6),
        report_label = "TARGET_PROB"
      ),
      asis = FALSE
    ),
    paste0(
      "TARGET_PROB: If the probability of toxicity at 100 is in the range ",
      "[0.10, 0.25] is at least 0.60.\n\n"
    )
  )
  expect_equal(
    knit_print(
      StoppingSpecificDose(
        dose = 100,
        rule = StoppingTargetProb(target = c(0.1, 0.25), prob = 0.6)
      ),
      asis = FALSE,
      tox_label = "a DLAE",
      dose_label = "100 mg",
      fmt_string = "%sIf the probability of %s at %s is in the range [%5.3f, %5.3f] is at least %5.3f.\n\n"
    ),
    paste0(
      "Dose 100 used for testing a stopping rule: If the probability of a ",
      "DLAE at 100 mg is in the range [0.100, 0.250] is at least 0.600.\n\n"
    )
  )
})

test_that("knit_print.StoppingTargetProb works correctly", {
  expect_equal(
    knit_print(
      StoppingTargetProb(target = c(0.25, 0.6), prob = 0.75, report_label = "TARGET_PROB"),
      asis = FALSE
    ),
    paste0(
      "TARGET_PROB: If the probability of toxicity at the next best dose is in ",
      "the range [0.25, 0.60] is at least 0.75.\n\n"
    )
  )
  expect_equal(
    knit_print(
      StoppingTargetProb(target = c(0.25, 0.6), prob = 0.75),
      asis = FALSE,
      dose_label = "the MTD",
      tox_label = "a DLAE",
      fmt_string = "%sIf the probability of %s at %s is in the range [%5.3f, %5.3f] is at least %5.3f."
    ),
    paste0(
      "P(0.25 ≤ prob(DLE | NBD) ≤ 0.6) ≥ 0.75: If the probability of a DLAE at ",
      "the MTD is in the range [0.250, 0.600] is at least 0.750."
    )
  )
})

test_that("knit_print.StoppingMinPatients works correctly", {
  expect_equal(
    knit_print(
      StoppingMinPatients(nPatients = 8, report_label = "PARTICIPANT_COUNT"),
      asis = FALSE
    ),
    "PARTICIPANT_COUNT: If 8 or more participants have been treated.\n\n"
  )
  expect_equal(
    knit_print(
      StoppingMinPatients(nPatients = 10),
      asis = FALSE,
      label = "subjects"
    ),
    "≥ 10 patients dosed: If 10 or more subjects have been treated.\n\n"
  )
})

test_that("knit_print.StoppingMinCohorts works correctly", {
  expect_equal(
    knit_print(
      StoppingMinCohorts(nCohorts = 8, report_label = "COHORT_COUNT"),
      asis = FALSE
    ),
    "COHORT_COUNT: If 8 or more cohorts have been treated.\n\n"
  )
  expect_equal(
    knit_print(
      StoppingMinCohorts(nCohorts = 3),
      asis = FALSE
    ),
    "≥ 3 cohorts dosed: If 3 or more cohorts have been treated.\n\n"
  )
})

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
    "COHORT_COUNT: If 8 or more cohorts have been treated within 25% of the next best dose.\n\n"
  )
  expect_equal(
    knit_print(
      StoppingCohortsNearDose(
        nCohorts = 3,
        percentage = 0
      ),
      asis = FALSE
    ),
    paste0(
      "≥ 3 cohorts dosed in 0 % dose range around NBD: If 3 or more cohorts ",
      "have been treated at the next best dose.\n\n"
    )
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
    paste0(
      "PARTICIPANT_COUNT: If 8 or more participants have been treated within ",
      "25% of the next best dose.\n\n"
    )
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
    paste0(
      "≥ 10 patients dosed in 0 % dose range around NBD: If 10 or more ",
      "subjects have been treated at the next best dose.\n\n"
    )
  )
})

test_that("knit_print.StoppingMissingDose works correctly", {
  expect_equal(
    knit_print(
      StoppingMissingDose(report_label = "MISSING_DOSE"),
      asis = FALSE
    ),
    paste0(
      "MISSING_DOSE: If the dose returned by <code>nextBest()</code> is ",
      "<code>NA</code>, or if the trial includes a placebo dose, the placebo dose.\n\n"
    )
  )
  expect_equal(
    knit_print(
      StoppingMissingDose(),
      asis = FALSE
    ),
    paste0(
      "Stopped because of missing dose: If the dose returned by ",
      "<code>nextBest()</code> is <code>NA</code>, or if the trial includes a ",
      "placebo dose, the placebo dose.\n\n"
    )
  )
})
