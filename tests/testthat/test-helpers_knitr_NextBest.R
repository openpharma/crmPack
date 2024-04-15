# Generic testing for existence of methods, return type and stability of output
# takes place in test-crmPackClass-methods.R.  This file tests only the
# CORRECTNESS of output.  For simplicity, use asis = FALSE throughout.

test_that("knit_print.NextBestMTD works correctly", {
  x <- .DefaultNextBestMTD()
  expect_equal(
    knit_print(x, asis = FALSE),
    paste0(
      "The dose recommended for the next cohort will be the one which is both ",
      "eligible and which has the smallest absolute difference between the ",
      "25th centile of the posterior distribution of the probability of ",
      "toxicity and the target toxicity rate [0.33]."
    )
  )
  x1 <- NextBestMTD(target = 0.25, derive = x@derive)
  expect_equal(
    knit_print(x1, asis = FALSE, tox_label = "DLT", target_label = "a custom target"),
    paste0(
      "The dose recommended for the next cohort will be the one which is both ",
      "eligible and which has the smallest absolute difference between a ",
      "custom target of the posterior distribution of the probability of ",
      "DLT and the target DLT rate [0.25]."
    )
  )
})

test_that("knit_print.NextBestNCRM works correctly", {
  x <- .DefaultNextBestNCRM()
  expect_equal(
    knit_print(x, asis = FALSE),
    paste0(
      "The dose recommended for the next cohort will be chosen in the ",
      "following way.  First, doses that are ineligible according to the ",
      "increments rule will be discarded.  Next, any dose for which the mean ",
      "posterior probability of toxicity being in the overdose range - ",
      "(0.35, 1] - is 0.25 or more will also be discarded.  Finally, the dose ",
      "amongst those remaining which has the highest chance that the mean ",
      "posterior probability of toxicity is in the target toxicity range of ",
      "0.2 to 0.35 (inclusive) will be selected."
    )
  )
  expect_equal(
    knit_print(x, asis = FALSE, tox_label = "DLT"),
    paste0(
      "The dose recommended for the next cohort will be chosen in the ",
      "following way.  First, doses that are ineligible according to the ",
      "increments rule will be discarded.  Next, any dose for which the mean ",
      "posterior probability of DLT being in the overdose range - ",
      "(0.35, 1] - is 0.25 or more will also be discarded.  Finally, the dose ",
      "amongst those remaining which has the highest chance that the mean ",
      "posterior probability of DLT is in the target DLT range of ",
      "0.2 to 0.35 (inclusive) will be selected."
    )
  )
})

test_that("knit_print.NextBestThreePlusThree works correctly", {
  x <- .DefaultNextBestThreePlusThree()
  expect_equal(
    knit_print(x, asis = FALSE),
    paste0(
      "The dose recommended for the next cohort will be chosen using the ",
      "\"Three Plus Three\" rule.\n\n- If no toxicities have been reported at ",
      "the current dose level, escalate by one dose level.\n- If the observed ",
      "toxicity rate at the current dose level is exactly 1/3 and no more than ",
      "three participants treated at the current dose level are evaluable, ",
      "remain at the current dose level.\n- Otherwise, recommend that the ",
      "trial stops and identify the MTD as dose level immediately below the ",
      "current one."
    )
  )
  expect_equal(
    knit_print(x, asis = FALSE, tox_label = "DLT", participant_label = "subject"),
    paste0(
      "The dose recommended for the next cohort will be chosen using the ",
      "\"Three Plus Three\" rule.\n\n- If no DLTs have been reported at ",
      "the current dose level, escalate by one dose level.\n- If the observed ",
      "DLT rate at the current dose level is exactly 1/3 and no more than ",
      "three subjects treated at the current dose level are evaluable, ",
      "remain at the current dose level.\n- Otherwise, recommend that the ",
      "trial stops and identify the MTD as dose level immediately below the ",
      "current one."
    )
  )
})


test_that("knit_print.NextBestDualEndpoint works correctly", {
  x <- .DefaultNextBestDualEndpoint()
  expect_equal(
    knit_print(x, asis = FALSE),
    paste0(
      "The dose recommended for the next cohort will be chosen in the ",
      "following way.  First, doses that are ineligible according to the ",
      "increments rule will be discarded.  Next, any dose for which the mean ",
      "posterior probability of toxicity being in the overdose range ",
      "- (0.35, 1] - is 0.25 or more will also be discarded.  Finally, the ",
      "dose amongst those remaining which has the highest chance that the mean ",
      "posterior probability that the biomarker is in the target range for ",
      "the biomarker, which is 200 to 300 (inclusive), will be selected, provided ",
      "that this probability exceeds 0.01.  If no dose meets this threshold, ",
      "then the highest eligible dose will be selected."
    )
  )
  expect_equal(
    knit_print(x, asis = FALSE, tox_label = "DLT", biomarker_units = "IU/mL", biomarker_label = "the custom label"),
    paste0(
      "The dose recommended for the next cohort will be chosen in the ",
      "following way.  First, doses that are ineligible according to the ",
      "increments rule will be discarded.  Next, any dose for which the mean ",
      "posterior probability of DLT being in the overdose range ",
      "- (0.35, 1] - is 0.25 or more will also be discarded.  Finally, the ",
      "dose amongst those remaining which has the highest chance that the mean ",
      "posterior probability that the custom label is in the target range for ",
      "the custom label, which is 200IU/mL to 300IU/mL (inclusive), will be selected, provided ",
      "that this probability exceeds 0.01.  If no dose meets this threshold, ",
      "then the highest eligible dose will be selected."
    )
  )
})

