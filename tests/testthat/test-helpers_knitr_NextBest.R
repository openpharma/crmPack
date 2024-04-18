# Generic testing for existence of methods, return type and stability of output
# takes place in test-crmPackClass-methods.R.  This file tests only the
# CORRECTNESS of output.  For simplicity, use asis = FALSE throughout.

test_that("knit_print.NextBestMinDist works correctly", {
  x <- .DefaultNextBestMinDist()
  expect_equal(
    knit_print(x, asis = FALSE),
    paste0(
      "The dose recommended for the next cohort will be the one which is both ",
      "eligible and which has the smallest absolute difference between its ",
      "mean posterior estimate of the probability of ",
      "toxicity and the target toxicity rate [0.3]."
    )
  )
  x1 <- NextBestMinDist(target = 0.25)
  expect_equal(
    knit_print(x1, asis = FALSE, tox_label = "DLT"),
    paste0(
      "The dose recommended for the next cohort will be the one which is both ",
      "eligible and which has the smallest absolute difference between its ",
      "mean posterior estimate of the probability of DLT and the target DLT ",
      "rate [0.25]."
    )
  )
})

test_that("knit_print.NextBestMTD works correctly", {
  x <- .DefaultNextBestMTD()
  expect_equal(
    knit_print(x, asis = FALSE),
    paste0(
      "The dose level recommended for the next cohort will be selected as ",
      "follows:\n\n-  First, the 25th centile of the posterior distribution of ",
      "toxicity will be calculated for all dose levels that are eligible ",
      "according to the  Increments rule.\n-  Next, the \"target dose\" ",
      "(which may not be part of the dose grid) for which the 25th centile of ",
      "the posterior distribution of toxicity is exactly equal to the target ",
      "rate of 0.33 will be determined.\n- Finally, the dose level whose ",
      "absolute distance from the target dose is smallest will be selected as ",
      "the recommended dose for the next cohort\n\n"
    )
  )
  x1 <- NextBestMTD(target = 0.25, derive = x@derive)
  expect_equal(
    knit_print(
      x1,
      asis = FALSE,
      tox_label = "DLT",
      target_label = "a custom measure"
    ),
    paste0(
      "The dose level recommended for the next cohort will be selected as ",
      "follows:\n\n-  First, a custom measure of the posterior distribution of ",
      "DLT will be calculated for all dose levels that are eligible ",
      "according to the  Increments rule.\n-  Next, the \"target dose\" ",
      "(which may not be part of the dose grid) for which a custom measure of ",
      "the posterior distribution of DLT is exactly equal to the target ",
      "rate of 0.25 will be determined.\n- Finally, the dose level whose ",
      "absolute distance from the target dose is smallest will be selected as ",
      "the recommended dose for the next cohort\n\n"
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
    knit_print(
      x,
      asis = FALSE,
      tox_label = "DLT",
      biomarker_units = "IU/mL",
      biomarker_label = "the custom label"
    ),
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

test_that("knit_print.NextBestInfTheory works correctly", {
  x <- .DefaultNextBestInfTheory()
  expect_equal(
    knit_print(x, asis = FALSE),
    paste0(
      "The recommended dose for the next cohort will be chosen using the ",
      "complex infinite bounds penalisation (CIBP) criterion of [Mozgunov & ",
      "Jaki (2019)](https://doi.org/10.1002/sim.8450).  Let\n\n$$ ",
      "\\delta(\\hat{p}_d, \\gamma) = \\frac{(\\hat{p}_d - ",
      "\\gamma)^2}{\\hat{p}_d^a \\cdot (1 - \\hat{p}_d)^{2 - a}} $$\n\nwhere ",
      "a is the non-centrality parameter with a value of 1.2, &gamma; is the ",
      "target toxicity rate with a value of 0.33 and $\\hat{p}_d$ is the mean ",
      "posterior estimate of the probability of toxicity at dose level d.",
      "\n\nThe recommended dose for the next cohort will be the value of d ",
      "that minimises $\\delta(\\hat{p}_d, \\gamma)$."
    )
  )
  expect_equal(
    knit_print(
      x,
      asis = FALSE,
      tox_label = "DLT",
      citation_text = "@MOZGUNOV",
      citation_link = ""
    ),
    paste0(
      "The recommended dose for the next cohort will be chosen using the ",
      "complex infinite bounds penalisation (CIBP) criterion of [@MOZGUNOV].  ",
      "Let\n\n$$ ",
      "\\delta(\\hat{p}_d, \\gamma) = \\frac{(\\hat{p}_d - ",
      "\\gamma)^2}{\\hat{p}_d^a \\cdot (1 - \\hat{p}_d)^{2 - a}} $$\n\nwhere ",
      "a is the non-centrality parameter with a value of 1.2, &gamma; is the ",
      "target DLT rate with a value of 0.33 and $\\hat{p}_d$ is the mean ",
      "posterior estimate of the probability of DLT at dose level d.",
      "\n\nThe recommended dose for the next cohort will be the value of d ",
      "that minimises $\\delta(\\hat{p}_d, \\gamma)$."
    )
  )
})

test_that("knit_print.NextBestTD works correctly", {
  x <- .DefaultNextBestTD()
  expect_equal(
    knit_print(x, asis = FALSE),
    paste0(
      "The dose recommended for the next cohort will be the one which is both ",
      "eligible and which is the highest dose in the dose grid strictly less ",
      "than the dose (which may not be in the dose grid) that has a posterior ",
      "plug-in estimate of the probability of toxicity exactly equal to the ",
      "target toxicity rate, either during [0.35] or at the end of the trial ",
      "[0.3]."
    )
  )
  expect_equal(
    knit_print(
      x,
      asis = FALSE,
      tox_label = "DLT"
    ),
    paste0(
      paste0(
        "The dose recommended for the next cohort will be the one which is both ",
        "eligible and which is the highest dose in the dose grid strictly less ",
        "than the dose (which may not be in the dose grid) that has a posterior ",
        "plug-in estimate of the probability of DLT exactly equal to the ",
        "target DLT rate, either during [0.35] or at the end of the trial ",
        "[0.3]."
      )
    )
  )
})

test_that("knit_print.NextBestMaxGain works correctly", {
  x <- .DefaultNextBestMaxGain()
  expect_equal(
    knit_print(x, asis = FALSE),
    paste0(
      "The dose recommended for the next cohort will be the one which is ",
      "closest to Gstar, the dose that maximises the gain for probability of ",
      "toxicity exactly equal to the target toxicity rate, either during ",
      "[0.35] or at the end of the trial [0.3]."
    )
  )
  expect_equal(
    knit_print(
      x,
      asis = FALSE,
      tox_label = "DLT"
    ),
    paste0(
      paste0(
        "The dose recommended for the next cohort will be the one which is ",
        "closest to Gstar, the dose that maximises the gain for probability of ",
        "DLT exactly equal to the target DLT rate, either during ",
        "[0.35] or at the end of the trial [0.3]."
      )
    )
  )
})

test_that("knit_print.NextBestProbMTDLTE works correctly", {
  x <- .DefaultNextBestProbMTDLTE()
  expect_equal(
    knit_print(x, asis = FALSE),
    paste0(
      "The dose recommended for the next cohort will be the dose level with ",
      "the highest probability of being the highest dose with an estimated ",
      "probability of toxicity less than or equal to 0.3."
    )
  )
  expect_equal(
    knit_print(
      x,
      asis = FALSE,
      tox_label = "DLT"
    ),
    paste0(
      paste0(
        "The dose recommended for the next cohort will be the dose level with ",
        "the highest probability of being the highest dose with an estimated ",
        "probability of DLT less than or equal to 0.3."
      )
    )
  )
})

test_that("knit_print.NextBestProbMTDMinDist works correctly", {
  x <- .DefaultNextBestProbMTDMinDist()
  expect_equal(
    knit_print(x, asis = FALSE),
    paste0(
      "The dose recommended for the next cohort will be the dose level with ",
      "the highest probability of being the highest dose with an estimated ",
      "probability of toxicity closest to 0.3."
    )
  )
  expect_equal(
    knit_print(
      x,
      asis = FALSE,
      tox_label = "DLT"
    ),
    paste0(
      paste0(
        "The dose recommended for the next cohort will be the dose level with ",
        "the highest probability of being the highest dose with an estimated ",
        "probability of DLT closest to 0.3."
      )
    )
  )
})

test_that("knit_print.NextBestNCRMLoss works correctly", {
  x <- .DefaultNextBestNCRMLoss()
  expect_equal(
    knit_print(x, asis = FALSE),
    paste0(
      "The dose recommended for the next cohort will be chosen in the ",
      "following way:\n\n-  First, the chance that the probability of toxicity ",
      "falls into each of the underdose, target , overdose and unacceptable ",
      "dose ranges is calculated for element of the dose grid.\n-  Next, the ",
      "loss associated with each dose is calculated by multiplying these ",
      "probabilities by the corresponding loss coefficient and summing the ",
      "result.\n-  Then ineligible doses, and those with a probability of ",
      "being in the overdose or unaccaptable ranges that is greater than 0.25, ",
      "are discarded.\n-  Finally, the dose level with the smallest loss is ",
      "selected as the recommended dose for the next cohort.\n\nToxicity ranges ",
      "and loss coefficients are given in the following table:\n\n",
      "<table class=\"table table-striped table-hover table-condensed\" ",
      "style=\"margin-left: auto; margin-right: auto;\">\n <thead>\n  <tr>\n   ",
      "<th style=\"text-align:left;\"> Range </th>\n   ",
      "<th style=\"text-align:right;\"> Lower </th>\n   ",
      "<th style=\"text-align:right;\"> Upper </th>\n   ",
      "<th style=\"text-align:right;\"> Loss Coefficient </th>\n  ",
      "</tr>\n </thead>\n<tbody>\n  <tr>\n   ",
      "<td style=\"text-align:left;\"> Underdose </td>\n   ",
      "<td style=\"text-align:right;\"> 0.00 </td>\n   ",
      "<td style=\"text-align:right;\"> 0.20 </td>\n   ",
      "<td style=\"text-align:right;\"> 1 </td>\n  </tr>\n  <tr>\n   ",
      "<td style=\"text-align:left;\"> Target </td>\n   ",
      "<td style=\"text-align:right;\"> 0.20 </td>\n   ",
      "<td style=\"text-align:right;\"> 0.35 </td>\n   ",
      "<td style=\"text-align:right;\"> 0 </td>\n  </tr>\n  <tr>\n   ",
      "<td style=\"text-align:left;\"> Overdose </td>\n   ",
      "<td style=\"text-align:right;\"> 0.35 </td>\n   ",
      "<td style=\"text-align:right;\"> 0.60 </td>\n   ",
      "<td style=\"text-align:right;\"> 1 </td>\n  </tr>\n  <tr>\n   ",
      "<td style=\"text-align:left;\"> Unacceptable </td>\n   ",
      "<td style=\"text-align:right;\"> 0.60 </td>\n   ",
      "<td style=\"text-align:right;\"> 1.00 </td>\n   ",
      "<td style=\"text-align:right;\"> 2 </td>\n  </tr>\n</tbody>\n</table>"
    )
  )
  expect_equal(
    knit_print(
      .DefaultNextBestNCRMLoss(),
      tox_label = "DLT",
      format_func = function(x) x,
      asis = FALSE
    ),
    paste0(
      "The dose recommended for the next cohort will be chosen in the following ",
      "way:\n\n-  First, the chance that the probability of DLT falls into each ",
      "of the underdose, target , overdose and unacceptable dose ranges is ",
      "calculated for element of the dose grid.\n-  Next, the loss associated ",
      "with each dose is calculated by multiplying these probabilities by the ",
      "corresponding loss coefficient and summing the result.\n-  Then ",
      "ineligible doses, and those with a probability of being in the overdose ",
      "or unaccaptable ranges that is greater than 0.25, are discarded.\n-  ",
      "Finally, the dose level with the smallest loss is selected as the ",
      "recommended dose for the next cohort.\n\nDLT ranges and loss coefficients ",
      "are given in the following table:\n\n",
      "<table>\n <thead>\n  <tr>\n   ",
      "<th style=\"text-align:left;\"> Range </th>\n   ",
      "<th style=\"text-align:right;\"> Lower </th>\n   ",
      "<th style=\"text-align:right;\"> Upper </th>\n   ",
      "<th style=\"text-align:right;\"> Loss Coefficient </th>\n  </tr>\n ",
      "</thead>\n<tbody>\n  <tr>\n   ",
      "<td style=\"text-align:left;\"> Underdose </td>\n   ",
      "<td style=\"text-align:right;\"> 0.00 </td>\n   ",
      "<td style=\"text-align:right;\"> 0.20 </td>\n   ",
      "<td style=\"text-align:right;\"> 1 </td>\n  </tr>\n  <tr>\n   ",
      "<td style=\"text-align:left;\"> Target </td>\n   ",
      "<td style=\"text-align:right;\"> 0.20 </td>\n   ",
      "<td style=\"text-align:right;\"> 0.35 </td>\n   ",
      "<td style=\"text-align:right;\"> 0 </td>\n  </tr>\n  <tr>\n   ",
      "<td style=\"text-align:left;\"> Overdose </td>\n   ",
      "<td style=\"text-align:right;\"> 0.35 </td>\n   ",
      "<td style=\"text-align:right;\"> 0.60 </td>\n   ",
      "<td style=\"text-align:right;\"> 1 </td>\n  </tr>\n  <tr>\n   ",
      "<td style=\"text-align:left;\"> Unacceptable </td>\n   ",
      "<td style=\"text-align:right;\"> 0.60 </td>\n   ",
      "<td style=\"text-align:right;\"> 1.00 </td>\n   ",
      "<td style=\"text-align:right;\"> 2 </td>\n  </tr>\n</tbody>\n</table>"
    )
  )
})

test_that("knit_print.NextBestTD works correctly", {
  x <- .DefaultNextBestTDsamples()
  expect_equal(
    knit_print(x, asis = FALSE),
    paste0(
      "The dose recommended for the next cohort will be the one which is both ",
      "eligible and which is the highest dose in the dose grid strictly less ",
      "than the dose (which may not be in the dose grid) that has a full Bayes ",
      "posterior estimate of the probability of toxicity exactly equal to the ",
      "target toxicity rate, either during [0.35] or at the end of the trial ",
      "[0.3]."
    )
  )
  expect_equal(
    knit_print(
      x,
      asis = FALSE,
      tox_label = "DLT"
    ),
    paste0(
      paste0(
        "The dose recommended for the next cohort will be the one which is both ",
        "eligible and which is the highest dose in the dose grid strictly less ",
        "than the dose (which may not be in the dose grid) that has a full Bayes ",
        "posterior estimate of the probability of DLT exactly equal to the ",
        "target DLT rate, either during [0.35] or at the end of the trial ",
        "[0.3]."
      )
    )
  )
})

test_that("knit_print.NextBestMaxGainSamples works correctly", {
  x <- .DefaultNextBestMaxGainSamples()
  expect_equal(
    knit_print(x, asis = FALSE),
    paste0(
      "The dose recommended for the next cohort will be the one which is ",
      "closest to Gstar, the dose for which the full Bayes posterior estimate ",
      "of the probability of toxicity maximises the gain relative to the ",
      "target toxicity rate, either during [0.35] or at the end of the trial [0.3]."
    )
  )
  expect_equal(
    knit_print(
      x,
      asis = FALSE,
      tox_label = "DLT"
    ),
    paste0(
      paste0(
        "The dose recommended for the next cohort will be the one which is ",
        "closest to Gstar, the dose for which the full Bayes posterior estimate ",
        "of the probability of DLT maximises the gain relative to the target ",
        "DLT rate, either during [0.35] or at the end of the trial [0.3]."
      )
    )
  )
})

test_that("knit_print.NextBestOrdinal works correctly", {
  x <- .DefaultNextBestOrdinal()
  expect_equal(
    knit_print(x, asis = FALSE),
    paste0(
      "Based on a toxicity grade of 1: The dose level recommended for the ",
      "next cohort will be selected as follows:\n\n-  First, the 25th centile ",
      "of the posterior distribution of toxicity will be calculated for all ",
      "dose levels that are eligible according to the  Increments rule.\n",
      "-  Next, the \"target dose\" (which may not be part of the dose grid) ",
      "for which the 25th centile of the posterior distribution of toxicity is ",
      "exactly equal to the target rate of 0.25 will be determined.\n",
      "- Finally, the dose level whose absolute distance from the target dose ",
      "is smallest will be selected as the recommended dose for the next cohort\n\n"
    )
  )
  x1 <- NextBestOrdinal(grade = 2L, rule = NextBestMTD(target = 0.25, derive = x@rule@derive))
  expect_equal(
    knit_print(
      x1,
      asis = FALSE,
      tox_label = "DLT",
      target_label = "a custom measure"
    ),
    paste0(
      "Based on a toxicity grade of 2: The dose level recommended for the ",
      "next cohort will be selected as follows:\n\n-  First, a custom ",
      "measure of the posterior distribution of DLT will be calculated for all ",
      "dose levels that are eligible according to the  Increments rule.\n",
      "-  Next, the \"target dose\" (which may not be part of the dose grid) ",
      "for which a custom measure of the posterior distribution of DLT is ",
      "exactly equal to the target rate of 0.25 will be determined.\n",
      "- Finally, the dose level whose absolute distance from the target dose ",
      "is smallest will be selected as the recommended dose for the next cohort\n\n"
    )
  )
})
