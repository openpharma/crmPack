# NextBest ----

## NextBestMTD ----

test_that(".NextBestMTD works as expected", {
  result <- expect_silent(.NextBestMTD())
  expect_valid(result, "NextBestMTD")
})

test_that("NextBestMTD object can be created with user constructor", {
  result <- expect_silent(
    NextBestMTD(0.4, function(mtd_samples) mean(mtd_samples))
  )
  expect_valid(result, "NextBestMTD")
  expect_identical(result@target, 0.4)
  expect_identical(result@derive(c(1:5)), 3) # nolintr
})

## NextBestNCRM ----

test_that(".NextBestNCRM works as expected", {
  result <- expect_silent(.NextBestNCRM())
  expect_valid(result, "NextBestNCRM")
})

test_that("NextBestNCRM object can be created with user constructor", {
  result <- expect_silent(
    NextBestNCRM(c(0.5, 0.8), c(0.4, 1), 0.3)
  )
  expect_valid(result, "NextBestNCRM")
  expect_identical(result@target, c(0.5, 0.8))
  expect_identical(result@overdose, c(0.4, 1))
  expect_identical(result@max_overdose_prob, 0.3)
})

## NextBestNCRMLoss ----

test_that(".NextBestNCRMLoss works as expected", {
  result <- expect_silent(.NextBestNCRMLoss())
  expect_valid(result, "NextBestNCRMLoss")
})

test_that("NextBestNCRMLoss object can be created with user constructor", {
  result <- expect_silent(
    NextBestNCRMLoss(c(0.2, 0.35), c(0.35, 0.6), c(0.6, 1), 0.25, c(1, 0, 1, 2))
  )
  expect_valid(result, "NextBestNCRMLoss")
  expect_identical(result@target, c(0.2, 0.35))
  expect_identical(result@overdose, c(0.35, 0.6))
  expect_identical(result@unacceptable, c(0.6, 1))
  expect_identical(result@max_overdose_prob, 0.25)
  expect_identical(result@losses, c(1, 0, 1, 2))
})

## NextBestThreePlusThree ----

test_that(".NextBestThreePlusThree works as expected", {
  result <- expect_silent(.NextBestThreePlusThree())
  expect_valid(result, "NextBestThreePlusThree")
})

test_that("NextBestThreePlusThree object can be created with user constructor", {
  result <- expect_silent(NextBestThreePlusThree())
  expect_valid(result, "NextBestThreePlusThree")
})

## NextBestDualEndpoint ----

test_that(".NextBestDualEndpoint works as expected", {
  result <- expect_silent(.NextBestDualEndpoint())
  expect_valid(result, "NextBestDualEndpoint")
})

test_that("NextBestDualEndpoint object can be created with user constructor", {
  result <- expect_silent(
    NextBestDualEndpoint(c(0.5, 0.8), c(0.4, 1), 0.3)
  )
  expect_valid(result, "NextBestDualEndpoint")
  expect_identical(result@target, c(0.5, 0.8))
  expect_identical(result@overdose, c(0.4, 1))
  expect_identical(result@target_relative, TRUE)
  expect_identical(result@target_thresh, 0.01)
})

test_that("NextBestDualEndpoint object can be created with user constructor 2", {
  result <- expect_silent(
    NextBestDualEndpoint(c(0.5, 0.8), c(0.4, 1), 0.3, FALSE, 0.05)
  )
  expect_valid(result, "NextBestDualEndpoint")
  expect_identical(result@target, c(0.5, 0.8))
  expect_identical(result@overdose, c(0.4, 1))
  expect_identical(result@target_relative, FALSE)
  expect_identical(result@target_thresh, 0.05)
})

## NextBestMinDist ----

test_that("NextBestMinDist works as expected", {
  result <- expect_silent(.NextBestMinDist())
  expect_valid(result, "NextBestMinDist")
})

test_that("NextBestMinDist object can be created with user constructor", {
  result <- expect_silent(
    NextBestMinDist(0.3)
  )
  expect_valid(result, "NextBestMinDist")
  expect_identical(result@target, 0.3)
})

## NextBestInfTheory ----

test_that(".NextBestInfTheory works as expected", {
  result <- expect_silent(.NextBestInfTheory())
  expect_valid(result, "NextBestInfTheory")
})

test_that("NextBestInfTheory object can be created with user constructor", {
  result <- expect_silent(
    NextBestInfTheory(0.4, 1.5)
  )
  expect_valid(result, "NextBestInfTheory")
  expect_identical(result@target, 0.4)
  expect_identical(result@asymmetry, 1.5)
})

## NextBestTD ----

test_that(".NextBestTD works as expected", {
  result <- expect_silent(.NextBestTD())
  expect_valid(result, "NextBestTD")
})

test_that("NextBestTD object can be created with user constructor", {
  result <- expect_silent(
    NextBestTD(0.4, 0.35)
  )
  expect_valid(result, "NextBestTD")
  expect_identical(result@prob_target_drt, 0.4)
  expect_identical(result@prob_target_eot, 0.35)
})

## NextBestTDsamples ----

test_that(".NextBestTDsamples works as expected", {
  result <- expect_silent(.NextBestTDsamples())
  expect_valid(result, "NextBestTDsamples")
})

test_that("NextBestTDsamples object can be created with user constructor", {
  result <- expect_silent(
    NextBestTDsamples(0.4, 0.35, function(x) mean(x))
  )
  expect_valid(result, "NextBestTDsamples")
  expect_identical(result@prob_target_drt, 0.4)
  expect_identical(result@prob_target_eot, 0.35)
  expect_identical(result@derive(c(1:5)), 3) # nolintr
})

## NextBestMaxGain ----

test_that(".NextBestMaxGain works as expected", {
  result <- expect_silent(.NextBestMaxGain())
  expect_valid(result, "NextBestMaxGain")
})

test_that("NextBestMaxGain object can be created with user constructor", {
  result <- expect_silent(
    NextBestMaxGain(0.4, 0.35)
  )
  expect_valid(result, "NextBestMaxGain")
  expect_identical(result@prob_target_drt, 0.4)
  expect_identical(result@prob_target_eot, 0.35)
})

## NextBestMaxGainSamples ----

test_that(".NextBestMaxGainSamples works as expected", {
  result <- expect_silent(.NextBestMaxGainSamples())
  expect_valid(result, "NextBestMaxGainSamples")
})

test_that("NextBestMaxGainSamples object can be created with user constructor", {
  result <- expect_silent(
    NextBestMaxGainSamples(0.4, 0.35, function(x) mean(x), function(x) sum(x))
  )
  expect_valid(result, "NextBestMaxGainSamples")
  expect_identical(result@prob_target_drt, 0.4)
  expect_identical(result@prob_target_eot, 0.35)
  expect_identical(result@derive(c(1:5)), 3) # nolintr
  expect_identical(result@mg_derive(c(1:5)), 15L) # nolintr
})

# Increments ----

## IncrementsRelative ----

test_that(".IncrementsRelative works as expected", {
  result <- expect_silent(.IncrementsRelative())
  expect_valid(result, "IncrementsRelative")
})

test_that("IncrementsRelative object can be created with user constructor", {
  result <- expect_silent(
    IncrementsRelative(c(0, 2, 3), c(2, 1, 1.5))
  )
  expect_valid(result, "IncrementsRelative")
  expect_identical(result@intervals, c(0, 2, 3))
  expect_identical(result@increments, c(2, 1, 1.5))
})

## IncrementsRelativeParts ----

test_that("IncrementsRelativeParts works as expected", {
  result <- expect_silent(.IncrementsRelativeParts())
  expect_valid(result, "IncrementsRelativeParts")
})

test_that("IncrementsRelativeParts object can be created with user constructor", {
  result <- expect_silent(
    IncrementsRelativeParts(-1, 3)
  )
  expect_valid(result, "IncrementsRelativeParts")
  expect_identical(result@dlt_start, -1L)
  expect_identical(result@clean_start, 3L)
})

## IncrementsRelativeDLT ----

test_that(".IncrementsRelativeDLT works as expected", {
  result <- expect_silent(.IncrementsRelativeDLT())
  expect_valid(result, "IncrementsRelativeDLT")
})

test_that("IncrementsRelativeDLT object can be created with user constructor", {
  result <- expect_silent(
    IncrementsRelativeDLT(c(0, 2, 3), c(2, 1, 1.5))
  )
  expect_valid(result, "IncrementsRelativeDLT")
  expect_identical(result@dlt_intervals, c(0L, 2L, 3L))
  expect_identical(result@increments, c(2, 1, 1.5))
})

## IncrementsRelativeDLTCurrent ----

test_that("IncrementsRelativeDLTCurrent works as expected", {
  result <- expect_silent(.IncrementsRelativeDLTCurrent())
  expect_valid(result, "IncrementsRelativeDLTCurrent")
})

test_that("IncrementsRelativeDLTCurrent object can be created with user constructor", {
  result <- expect_silent(
    IncrementsRelativeDLTCurrent(c(0, 2, 3), c(2, 1, 1.5))
  )
  expect_valid(result, "IncrementsRelativeDLTCurrent")
  expect_identical(result@dlt_intervals, c(0L, 2L, 3L))
  expect_identical(result@increments, c(2, 1, 1.5))
})

## IncrementsNumDoseLevels ----

test_that(".IncrementsNumDoseLevels works as expected", {
  result <- expect_silent(.IncrementsNumDoseLevels())
  expect_valid(result, "IncrementsNumDoseLevels")
})

test_that("IncrementsNumDoseLevels object can be created with user constructor (default)", {
  result <- expect_silent(IncrementsNumDoseLevels())
  expect_valid(result, "IncrementsNumDoseLevels")
  expect_identical(result@max_levels, 1L)
  expect_identical(result@basis_level, "last")
})

test_that("IncrementsNumDoseLevels object can be created with user constructor", {
  result <- expect_silent(IncrementsNumDoseLevels(5L, "max"))
  expect_valid(result, "IncrementsNumDoseLevels")
  expect_identical(result@max_levels, 5L)
  expect_identical(result@basis_level, "max")
})

## IncrementsHSRBeta ----

test_that(".IncrementsHSRBeta works as expected", {
  result <- expect_silent(.IncrementsHSRBeta())
  expect_valid(result, "IncrementsHSRBeta")
})

test_that("IncrementsHSRBeta object can be created with user constructor (default)", {
  result <- expect_silent(IncrementsHSRBeta())
  expect_valid(result, "IncrementsHSRBeta")
  expect_identical(result@target, 0.3)
  expect_identical(result@prob, 0.95)
  expect_identical(result@a, 1)
  expect_identical(result@b, 1)
})

test_that("IncrementsHSRBeta object can be created with user constructor", {
  result <- expect_silent(IncrementsHSRBeta(0.5, 0.6, 2, 4))
  expect_valid(result, "IncrementsHSRBeta")
  expect_identical(result@target, 0.5)
  expect_identical(result@prob, 0.6)
  expect_identical(result@a, 2)
  expect_identical(result@b, 4)
})

## IncrementsMin ----

test_that(".IncrementsMin works as expected", {
  result <- expect_silent(.IncrementsMin())
  expect_valid(result, "IncrementsMin")
})

test_that("IncrementsMin object can be created with user constructor", {
  increments_list <- list(
    IncrementsRelativeDLT(dlt_intervals = c(0L, 1L), increments = c(2, 1)),
    IncrementsRelative(intervals = c(0, 2), increments = c(2, 1))
  )
  result <- expect_silent(IncrementsMin(increments_list))

  expect_valid(result, "IncrementsMin")
  expect_identical(result@increments_list, increments_list)
})

# Stopping ----

## StoppingCohortsNearDose ----

test_that(".StoppingCohortsNearDose works as expected", {
  result <- expect_silent(.StoppingCohortsNearDose())
  expect_valid(result, "StoppingCohortsNearDose")
})

test_that("StoppingCohortsNearDose object can be created with user constructor (default)", {
  result <- expect_silent(StoppingCohortsNearDose())
  expect_valid(result, "StoppingCohortsNearDose")
  expect_identical(result@nCohorts, 2L)
  expect_identical(result@percentage, 50)
})

test_that("StoppingCohortsNearDose object can be created with user constructor", {
  result <- expect_silent(StoppingCohortsNearDose(5L, 40))
  expect_valid(result, "StoppingCohortsNearDose")
  expect_identical(result@nCohorts, 5L)
  expect_identical(result@percentage, 40)
})

## StoppingPatientsNearDose ----

test_that(".StoppingPatientsNearDose works as expected", {
  result <- expect_silent(.StoppingPatientsNearDose())
  expect_valid(result, "StoppingPatientsNearDose")
})

test_that("StoppingPatientsNearDose object can be created with user constructor (default)", {
  result <- expect_silent(StoppingPatientsNearDose(20L))
  expect_valid(result, "StoppingPatientsNearDose")
  expect_identical(result@nPatients, 20L)
  expect_identical(result@percentage, 50)
})

test_that("StoppingPatientsNearDose object can be created with user constructor", {
  result <- expect_silent(StoppingPatientsNearDose(5L, 40))
  expect_valid(result, "StoppingPatientsNearDose")
  expect_identical(result@nPatients, 5L)
  expect_identical(result@percentage, 40)
})

## StoppingMinCohorts ----

test_that(".StoppingMinCohorts works as expected", {
  result <- expect_silent(.StoppingMinCohorts())
  expect_valid(result, "StoppingMinCohorts")
})

test_that("StoppingMinCohorts object can be created with user constructor (default)", {
  result <- expect_silent(StoppingMinCohorts())
  expect_valid(result, "StoppingMinCohorts")
  expect_identical(result@nCohorts, 2L)
})

test_that("StoppingMinCohorts object can be created with user constructor", {
  result <- expect_silent(StoppingMinCohorts(5L))
  expect_valid(result, "StoppingMinCohorts")
  expect_identical(result@nCohorts, 5L)
})

## StoppingMTDCV ----

test_that(".StoppingMTDCV works as expected", {
  result <- expect_silent(.StoppingMTDCV())
  expect_valid(result, "StoppingMTDCV")
})

test_that("StoppingMTDCV object can be created with user constructor (default)", {
  result <- expect_silent(
    StoppingMTDCV(target = 0.3,
                  thresh_cv = 40)
    )
  expect_valid(result, "StoppingMTDCV")
  expect_identical(result@target, 0.3)
  expect_identical(result@thresh_cv, 40) # nolintr
})

test_that("StoppingMTDCV object can be created with user constructor", {
  result <- expect_silent(
    StoppingMTDCV(target = 0.35,
                  thresh_cv = 30)
  )
  expect_valid(result, "StoppingMTDCV")
  expect_identical(result@target, 0.35)
  expect_identical(result@thresh_cv, 30) # nolintr
})

## StoppingLowestDoseHSRBeta ----

test_that(".StoppingLowestDoseHSRBeta works as expected", {
  result <- expect_silent(.StoppingLowestDoseHSRBeta())
  expect_valid(result, "StoppingLowestDoseHSRBeta")
})

test_that("StoppingLowestDoseHSRBeta object can be created with user constructor (default)", {
  result <- expect_silent(
    StoppingLowestDoseHSRBeta(target = 0.3,
                              prob = 0.95,
                              a = 1,
                              b = 1
                              )
  )
  expect_valid(result, "StoppingLowestDoseHSRBeta")
  expect_identical(result@target, 0.3)
  expect_identical(result@prob, 0.95)
  expect_identical(result@a, 1)
  expect_identical(result@b, 1) # nolintr
})

test_that("StoppingLowestDoseHSRBeta object can be created with user constructor", {
  result <- expect_silent(
    StoppingLowestDoseHSRBeta(target = 0.35,
                              prob = 0.90,
                              a = 0.5,
                              b = 0.5
    )
  )
  expect_valid(result, "StoppingLowestDoseHSRBeta")
  expect_identical(result@target, 0.35)
  expect_identical(result@prob, 0.90)
  expect_identical(result@a, 0.5)
  expect_identical(result@b, 0.5) # nolintr
})

## StoppingSpecificDose ----

test_that(".StoppingSpecificDose works as expected", {
  result <- expect_silent(.StoppingSpecificDose(dose = 80))
  expect_valid(result, "StoppingSpecificDose")
})

test_that("StoppingSpecificDose object can be created with user constructor", {
  result <- expect_silent(
    StoppingSpecificDose(rule = StoppingTargetProb(target = c(0, 0.3), prob = 0.8),
                         dose = 80
    )
  )
  expect_valid(result, "StoppingSpecificDose")
  expect_valid(result@rule, "StoppingTargetProb")
  expect_identical(result@rule@target, c(0, 0.3))
  expect_identical(result@rule@prob, 0.8)
  expect_identical(result@dose, 80) # nolintr
})
