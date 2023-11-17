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

test_that(".DefaultNextBestDualEndpoint works as expected", {
  expect_equal(
    .DefaultNextBestDualEndpoint(),
    NextBestDualEndpoint(
      target = c(200, 300),
      overdose = c(0.35, 1),
      max_overdose_prob = 0.25,
      target_relative = FALSE
    )
  )
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

test_that(".DefaultNextBestMaxGainSamples works as expected", {
  result <- .DefaultNextBestMaxGainSamples()
  expect_valid(result, "NextBestMaxGainSamples")
  expect_identical(result@prob_target_drt, 0.35)
  expect_identical(result@prob_target_eot, 0.3)
  expect_identical(result@derive(c(1:5)), 2.2) # nolintr
  expect_identical(result@mg_derive(c(1:5)), 3.0) # nolintr
})

## NextBestProbMTDLTE ----

test_that(".NextBestProbMTDLTE works as expected", {
  result <- expect_silent(.NextBestProbMTDLTE())
  expect_valid(result, "NextBestProbMTDLTE")
})

test_that("NextBestProbMTDLTE object can be created with user constructor", {
  result <- expect_silent(
    NextBestProbMTDLTE(0.4)
  )
  expect_valid(result, "NextBestProbMTDLTE")
  expect_identical(result@target, 0.4)
})

test_that(".DefaultNextBestProbMTDLTE works as expected", {
  result <- expect_silent(.DefaultNextBestProbMTDLTE())
  expect_valid(result, "NextBestProbMTDLTE")
})

## NextBestProbMTDMinDist ----

test_that(".NextBestProbMTDMinDist works as expected", {
  result <- expect_silent(.NextBestProbMTDMinDist())
  expect_valid(result, "NextBestProbMTDMinDist")
})

test_that("NextBestProbMTDMinDist object can be created with user constructor", {
  result <- expect_silent(NextBestProbMTDMinDist(0.4))
  expect_valid(result, "NextBestProbMTDMinDist")
  expect_identical(result@target, 0.4)
})

test_that(".DefaultNextBestProbMTDMinDist works as expected", {
  result <- expect_silent(.DefaultNextBestProbMTDMinDist())
  expect_valid(result, "NextBestProbMTDMinDist")
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

test_that(".DefaultIncrementsRelative works as expected", {
  expect_equal(
    .DefaultIncrementsRelative(),
    IncrementsRelative(intervals = c(0, 20), increments = c(1, 0.33))
  )
})

## IncrementsRelativeParts ----

test_that(".IncrementsRelativeParts works as expected", {
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

test_that(".DefaultIncrementsRelativeParts works as expected", {
  expect_equal(
    .DefaultIncrementsRelativeParts(),
    IncrementsRelativeParts(dlt_start = 0, clean_start = 1)
  )
})

test_that(".DefaultIncrementsRelativeParts works as expected", {
  expect_equal(
    .DefaultIncrementsRelativeParts(),
    IncrementsRelativeParts(dlt_start = 0, clean_start = 1)
  )
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
  expect_identical(result@intervals, c(0L, 2L, 3L))
  expect_identical(result@increments, c(2, 1, 1.5))
})

test_that(".DefaultIncrementsRelativeDLT works as expected", {
  expect_equal(
    .DefaultIncrementsRelativeDLT(),
    IncrementsRelativeDLT(intervals = c(0, 1, 3), increments = c(1, 0.33, 0.2))
  )
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
  expect_identical(result@intervals, c(0L, 2L, 3L))
  expect_identical(result@increments, c(2, 1, 1.5))
})

test_that(".DefaultIncrementsRelativeDLTCurrent works as expected", {
  expect_equal(
    .DefaultIncrementsRelativeDLTCurrent(),
    IncrementsRelativeDLTCurrent(intervals = c(0, 1, 3), increments = c(1, 0.33, 0.2))
  )
})

## IncrementsDoseLevels ----

test_that(".IncrementsDoseLevels works as expected", {
  result <- expect_silent(.IncrementsDoseLevels())
  expect_valid(result, "IncrementsDoseLevels")
})

test_that("IncrementsDoseLevels object can be created with user constructor (default)", {
  result <- expect_silent(IncrementsDoseLevels())
  expect_valid(result, "IncrementsDoseLevels")
  expect_identical(result@levels, 1L)
  expect_identical(result@basis_level, "last")
})

test_that("IncrementsDoseLevels object can be created with user constructor", {
  result <- expect_silent(IncrementsDoseLevels(5L, "max"))
  expect_valid(result, "IncrementsDoseLevels")
  expect_identical(result@levels, 5L)
  expect_identical(result@basis_level, "max")
})

test_that(".DefaultIncrementsDoseLevels works as expected", {
  expect_equal(
    .DefaultIncrementsDoseLevels(),
    IncrementsDoseLevels(levels = 2, basis_level = "last")
  )
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

test_that(".DefaultIncrementsHSRBeta works as expected", {
  expect_equal(
    .DefaultIncrementsHSRBeta(),
    IncrementsHSRBeta(target = 0.3, prob = 0.95)
  )
})

## IncrementsMin ----

test_that(".IncrementsMin works as expected", {
  result <- expect_silent(.IncrementsMin())
  expect_valid(result, "IncrementsMin")
})

test_that("IncrementsMin object can be created with user constructor", {
  increments_list <- list(
    IncrementsRelativeDLT(intervals = c(0L, 1L), increments = c(2, 1)),
    IncrementsRelative(intervals = c(0, 2), increments = c(2, 1))
  )
  result <- expect_silent(IncrementsMin(increments_list))

  expect_valid(result, "IncrementsMin")
  expect_identical(result@increments_list, increments_list)
})

test_that(".DefaultIncrementsMin works as expected", {
  expect_equal(
    .DefaultIncrementsMin(),
    IncrementsMin(
      increments_list = list(
        IncrementsRelativeDLT(
          intervals = c(0, 1, 3),
          increments = c(1, 0.33, 0.2)
        ),
        IncrementsRelative(
          intervals = c(0, 20),
          increments = c(1, 0.33)
        )
      )
    )
  )
})

# Stopping ----

## StoppingMissingDose ----

test_that(".StoppingMissingDose works as expected", {
  result <- expect_silent(.StoppingMissingDose())
  expect_valid(result, "StoppingMissingDose")
})

test_that(".DefaultStoppingMissingDose works as expected", {
  expect_equal(
    .DefaultStoppingMissingDose(),
    StoppingMissingDose()
  )
})

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
  expect_identical(result@report_label, NA_character_)
})

test_that("StoppingCohortsNearDose object can be created with user constructor", {
  result <- expect_silent(StoppingCohortsNearDose(5L, 40, "custom label"))
  expect_valid(result, "StoppingCohortsNearDose")
  expect_identical(result@nCohorts, 5L)
  expect_identical(result@percentage, 40)
  expect_identical(result@report_label, "custom label")
})

test_that("StoppingCohortsNearDose replaces empty label with correct default label", {
  result <- expect_silent(StoppingCohortsNearDose(5L, 40, character(0)))
  expect_identical(result@report_label, "≥ 5 cohorts dosed in 40 % dose range around NBD")
})

test_that(".DefaultStoppingCohortsNearDose works as expected", {
  expect_equal(
    .DefaultStoppingCohortsNearDose(),
    StoppingCohortsNearDose(nCohorts = 3, percentage = 0.2, report_label = NA_character_)
  )
})

## StoppingPatientsNearDose ----

test_that(".StoppingPatientsNearDose works as expected", {
  result <- expect_silent(.StoppingPatientsNearDose())
  expect_valid(result, "StoppingPatientsNearDose")
})

test_that("StoppingPatientsNearDose object can be created with user constructor (default)", {
  result <- expect_silent(StoppingPatientsNearDose())
  expect_valid(result, "StoppingPatientsNearDose")
  expect_identical(result@nPatients, 10L)
  expect_identical(result@percentage, 50)
  expect_identical(result@report_label, NA_character_)
})

test_that("StoppingPatientsNearDose object can be created with user constructor", {
  result <- expect_silent(StoppingPatientsNearDose(5L, 40, "custom_label"))
  expect_valid(result, "StoppingPatientsNearDose")
  expect_identical(result@nPatients, 5L)
  expect_identical(result@percentage, 40)
  expect_identical(result@report_label, "custom_label")
})

test_that("StoppingPatientsNearDose replaces empty label with correct default label", {
  result <- expect_silent(StoppingPatientsNearDose(5L, 40, character(0)))
  expect_identical(result@report_label, "≥ 5 patients dosed in 40 % dose range around NBD")
})

test_that(".DefaultStoppingPatientsNearDose works as expected", {
  expect_equal(
    .DefaultStoppingPatientsNearDose(),
    StoppingPatientsNearDose(nPatients = 9, percentage = 20, report_label = NA_character_)
  )
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
  expect_identical(result@report_label, NA_character_)
})

test_that("StoppingMinCohorts object can be created with user constructor", {
  result <- expect_silent(StoppingMinCohorts(5L, "custom label"))
  expect_valid(result, "StoppingMinCohorts")
  expect_identical(result@nCohorts, 5L)
  expect_identical(result@report_label, "custom label")
})

test_that("StoppingMinCohorts replaces empty label with correct default label", {
  result <- expect_silent(StoppingMinCohorts(5L, character(0)))
  expect_identical(result@report_label, "≥ 5 cohorts dosed")
})

test_that(".DefaultStoppingMinCohorts works as expected", {
  expect_equal(
    .DefaultStoppingMinCohorts(),
    StoppingMinCohorts(
      nCohorts = 6,
      report_label = NA_character_
    )
  )
})

## StoppingMinPatients ----

test_that(".StoppingMinPatients works as expected", {
  result <- expect_silent(.StoppingMinPatients())
  expect_valid(result, "StoppingMinPatients")
})

test_that("StoppingMinPatients object can be created with user constructor (default)", {
  result <- expect_silent(StoppingMinPatients())
  expect_valid(result, "StoppingMinPatients")
  expect_identical(result@nPatients, 20L)
  expect_identical(result@report_label, NA_character_)
})

test_that("StoppingMinPatients object can be created with user constructor", {
  result <- expect_silent(StoppingMinPatients(5L, "custom label"))
  expect_valid(result, "StoppingMinPatients")
  expect_identical(result@nPatients, 5L)
  expect_identical(result@report_label, "custom label")
})

test_that("StoppingMinPatients replaces empty label with correct default label", {
  result <- expect_silent(StoppingMinPatients(5L, character(0)))
  expect_identical(result@report_label, "≥ 5 patients dosed")
})

test_that(".DefaultStoppingMinPatients works as expected", {
  expect_equal(
    .DefaultStoppingMinPatients(),
    StoppingMinPatients(nPatients = 20, report_label = NA_character_)
  )
})

## StoppingTargetProb ----

test_that(".StoppingTargetProb works as expected", {
  result <- expect_silent(.StoppingTargetProb())
  expect_valid(result, "StoppingTargetProb")
})

test_that("StoppingTargetProb object can be created with user constructor (default)", {
  result <- expect_silent(StoppingTargetProb())
  expect_valid(result, "StoppingTargetProb")
  expect_identical(result@target, c(0.2, 0.35))
  expect_identical(result@prob, 0.4)
  expect_identical(result@report_label, NA_character_)
})

test_that("StoppingTargetProb object can be created with user constructor", {
  result <- expect_silent(StoppingTargetProb(c(0.3, 0.45), 0.5, "custom label"))
  expect_valid(result, "StoppingTargetProb")
  expect_identical(result@target, c(0.3, 0.45))
  expect_identical(result@prob, 0.5)
  expect_identical(result@report_label, "custom label")
})

test_that("StoppingTargetProb replaces empty label with correct default label", {
  result <- expect_silent(StoppingTargetProb(c(0.3, 0.45), 0.5, character(0)))
  expect_identical(result@report_label, "P(0.3 ≤ prob(DLE | NBD) ≤ 0.45) ≥ 0.5")
})

test_that(".DefaultStoppingTargetProb works as expected", {
  expect_equal(
    .DefaultStoppingTargetProb(),
    StoppingTargetProb(target = c(0.2, 0.35), prob = 0.5, report_label = NA_character_)
  )
})

## StoppingMTDdistribution ----

test_that(".StoppingMTDdistribution works as expected", {
  result <- expect_silent(.StoppingMTDdistribution())
  expect_valid(result, "StoppingMTDdistribution")
})

test_that("StoppingMTDdistribution object can be created with user constructor (default)", {
  result <- expect_silent(
    StoppingMTDdistribution()
  )
  expect_valid(result, "StoppingMTDdistribution")
  expect_identical(result@target, 0.33)
  expect_identical(result@thresh, 0.5)
  expect_identical(result@prob, 0.9)
  expect_identical(result@report_label, NA_character_)
})

test_that("StoppingMTDdistribution object can be created with user constructor", {
  result <- expect_silent(
    StoppingMTDdistribution(0.4, 0.4, 0.8, "custom label")
  )
  expect_valid(result, "StoppingMTDdistribution")
  expect_identical(result@target, 0.4)
  expect_identical(result@thresh, 0.4)
  expect_identical(result@prob, 0.8)
  expect_identical(result@report_label, "custom label")
})

test_that("StoppingMTDdistribution replaces empty label with correct default label", {
  result <- expect_silent(StoppingMTDdistribution(0.4, 0.4, 0.8, character(0)))
  expect_identical(result@report_label, "P(MTD > 0.4 * NBD | P(DLE) = 0.4) ≥ 0.8")
})

test_that(".DefaultStoppingMTDdistribution works as expected", {
  expect_equal(
    .DefaultStoppingMTDdistribution(),
    StoppingMTDdistribution(target = 0.33, thresh = 0.5, prob = 0.9, report_label = NA_character_)
  )
})

## StoppingMTDCV ----

test_that(".StoppingMTDCV works as expected", {
  result <- expect_silent(.StoppingMTDCV())
  expect_valid(result, "StoppingMTDCV")
})

test_that("StoppingMTDCV object can be created with user constructor (default)", {
  result <- expect_silent(StoppingMTDCV())
  expect_valid(result, "StoppingMTDCV")
  expect_identical(result@target, 0.3)
  expect_identical(result@thresh_cv, 40)
  expect_identical(result@report_label, NA_character_)
})

test_that("StoppingMTDCV object can be created with user constructor", {
  result <- expect_silent(StoppingMTDCV(0.4, 70, "custom label"))
  expect_valid(result, "StoppingMTDCV")
  expect_identical(result@target, 0.4)
  expect_identical(result@thresh_cv, 70)
  expect_identical(result@report_label, "custom label")
})

test_that("StoppingMTDCV replaces empty label with correct default label", {
  result <- expect_silent(StoppingMTDCV(0.4, 70, character(0)))
  expect_identical(result@report_label, "CV(MTD) > 0.4")
})


test_that(".DefaultStoppingMTDCV works as expected", {
  expect_equal(
    .DefaultStoppingMTDCV(),
    StoppingMTDCV(target = 0.3, thresh_cv = 40, report_label = NA_character_)
  )
})

## StoppingLowestDoseHSRBeta

test_that(".StoppingLowestDoseHSRBeta works as expected", {
  result <- expect_silent(.StoppingLowestDoseHSRBeta())
  expect_valid(result, "StoppingLowestDoseHSRBeta")
})

test_that("StoppingLowestDoseHSRBeta object can be created with user constructor (default)", {
  result <- expect_silent(StoppingLowestDoseHSRBeta())
  expect_valid(result, "StoppingLowestDoseHSRBeta")
  expect_identical(result@target, 0.3)
  expect_identical(result@prob, 0.95)
  expect_identical(result@a, 1)
  expect_identical(result@b, 1)
  expect_identical(result@report_label, NA_character_)
})

test_that("StoppingLowestDoseHSRBeta object can be created with user constructor", {
  result <- expect_silent(StoppingLowestDoseHSRBeta(0.25, 0.82, 5, 2, "custom label"))
  expect_valid(result, "StoppingLowestDoseHSRBeta")
  expect_identical(result@target, 0.25)
  expect_identical(result@prob, 0.82)
  expect_identical(result@a, 5)
  expect_identical(result@b, 2)
  expect_identical(result@report_label, "custom label")
})

test_that("StoppingLowestDoseHSRBeta replaces empty label with correct default label", {
  result <- expect_silent(StoppingLowestDoseHSRBeta(0.25, 0.82, 5, 2, character(0)))
  expect_identical(result@report_label, "Pβ(lowest dose > P(DLE) = 0.25) > 0.82")
})

test_that(".DefaultStoppingLowestDoseHSRBeta works as expected", {
  expect_equal(
    .DefaultStoppingLowestDoseHSRBeta(),
    StoppingLowestDoseHSRBeta(
      target = 0.3,
      prob = 0.95,
      a = 1,
      b = 1,
      report_label = NA_character_
    )
  )
})

## StoppingTargetBiomarker

test_that(".StoppingTargetBiomarker works as expected", {
  result <- expect_silent(.StoppingTargetBiomarker())
  expect_valid(result, "StoppingTargetBiomarker")
})

test_that("StoppingTargetBiomarker object can be created with user constructor (default)", {
  result <- expect_silent(StoppingTargetBiomarker())
  expect_valid(result, "StoppingTargetBiomarker")
  expect_identical(result@target, c(0.9, 1))
  expect_identical(result@is_relative, TRUE)
  expect_identical(result@prob, 0.3)
})

test_that("StoppingTargetBiomarker object can be created with user constructor", {
  result <- expect_silent(StoppingTargetBiomarker(c(0.85, 1), 0.4, FALSE, "custom label"))
  expect_valid(result, "StoppingTargetBiomarker")
  expect_identical(result@target, c(0.85, 1))
  expect_identical(result@is_relative, FALSE)
  expect_identical(result@prob, 0.4)
  expect_identical(result@report_label, "custom label")
})

test_that("StoppingTargetBiomarker replaces empty label with correct default label", {
  result <- expect_silent(StoppingTargetBiomarker(c(0.85, 1), 0.4, FALSE, character(0)))
  expect_identical(result@report_label, "P(0.85 ≤ Biomarker ≤ 1) ≥ 0.4 (absolute)")
})

test_that(".DefaultStoppingTargetBiomarker works as expected", {
  expect_equal(
    .DefaultStoppingTargetBiomarker(),
    StoppingTargetBiomarker(
      target = c(0.9, 1),
      prob = 0.5,
      report_label = NA_character_
    )
  )
})

## StoppingSpecificDose ----

test_that(".StoppingSpecificDose works as expected", {
  result <- expect_silent(.StoppingSpecificDose(dose = positive_number(80)))
  expect_valid(result, "StoppingSpecificDose")
})

test_that("StoppingSpecificDose object can be created with user constructor (default)", {
  result <- expect_silent(
    StoppingSpecificDose()
  )
  expect_valid(result, "StoppingSpecificDose")
  expect_valid(result@rule, "StoppingTargetProb")
  expect_identical(result@rule@target, c(0, 0.3))
  expect_identical(result@rule@prob, 0.8)
  expect_identical(result@dose@.Data, 80)
  expect_identical(result@report_label, NA_character_)
})

test_that("StoppingSpecificDose object can be created with user constructor", {
  result <- expect_silent(
    StoppingSpecificDose(
      rule = StoppingTargetProb(target = c(0, 0.3), prob = 0.8),
      dose = 80,
      report_label = "custom label"
    )
  )
  expect_valid(result, "StoppingSpecificDose")
  expect_valid(result@rule, "StoppingTargetProb")
  expect_identical(result@rule@target, c(0, 0.3))
  expect_identical(result@rule@prob, 0.8)
  expect_identical(result@dose@.Data, 80)
  expect_identical(result@report_label, "custom label")
})


test_that("StoppingSpecificDose replaces empty label with correct default label", {
  result <- expect_silent(StoppingSpecificDose(
    rule = StoppingTargetProb(target = c(0.1, 0.2), prob = 0.7),
    dose = 75,
    character(0)
  ))
  expect_identical(result@report_label, "Dose 75 used for testing a stopping rule")
})

test_that(".DefaultStoppingSpecificDose works as expected", {
  expect_equal(
    .DefaultStoppingSpecificDose(),
    StoppingSpecificDose(
      rule = StoppingTargetProb(target = c(0, 0.3), prob = 0.8),
      dose = 80,
      report_label = NA_character_
    )
  )
})

## StoppingHighestDose ----

test_that(".StoppingHighestDose works as expected", {
  result <- expect_silent(.StoppingHighestDose())
  expect_valid(result, "StoppingHighestDose")
})

test_that("StoppingHighestDose object can be created with user constructor (default)", {
  result <- expect_silent(StoppingHighestDose())
  expect_valid(result, "StoppingHighestDose")
  expect_identical(result@report_label, NA_character_)
})

test_that("StoppingHighestDose object can be created with user constructor", {
  result <- expect_silent(StoppingHighestDose("custom label"))
  expect_valid(result, "StoppingHighestDose")
  expect_identical(result@report_label, "custom label")
})

test_that("StoppingHighestDose replaces empty label with correct default label", {
  result <- expect_silent(StoppingHighestDose(character(0)))
  expect_identical(result@report_label, "NBD is the highest dose")
})

test_that(".DefaultStoppingHighestDose works as expected", {
  expect_equal(
    .DefaultStoppingHighestDose(),
    StoppingHighestDose()
  )
})

## StoppingList ----

test_that(".StoppingList works as expected", {
  result <- expect_silent(.StoppingList())
  expect_valid(result, "StoppingList")
})

test_that("StoppingList object can be created with user constructor", {
  stop_list <- list(
    StoppingMinCohorts(nCohorts = 5),
    StoppingTargetProb(target = c(0.2, 0.45), prob = 0.6),
    StoppingMinPatients(nPatients = 30)
  )
  result <- expect_silent(
    StoppingList(stop_list = stop_list, summary = all)
  )
  expect_valid(result, "StoppingList")
  expect_identical(result@stop_list, stop_list)
  expect_identical(result@summary, all)
})

test_that(".DefaultStoppingList works as expected", {
  expect_equal(
    .DefaultStoppingList(),
    StoppingList(
      stop_list = c(
        StoppingMinCohorts(nCohorts = 3),
        StoppingTargetProb(target = c(0.2, 0.35), prob = 0.5),
        StoppingMinPatients(nPatients = 20)
      ),
      summary = any
    )
  )
})

## StoppingAll ----

test_that(".StoppingAll works as expected", {
  result <- expect_silent(.StoppingAll())
  expect_valid(result, "StoppingAll")
})

test_that("StoppingAll object can be created with user constructor", {
  stop_list <- list(
    StoppingMinCohorts(nCohorts = 5),
    StoppingTargetProb(target = c(0.2, 0.45), prob = 0.6),
    StoppingMinPatients(nPatients = 30)
  )
  result <- expect_silent(
    StoppingAll(stop_list = stop_list, report_label = "custom label")
  )
  expect_valid(result, "StoppingAll")
  expect_identical(result@stop_list, stop_list)
  expect_identical(result@report_label, "custom label")
})

## StoppingAny ----

test_that(".StoppingAny works as expected", {
  result <- expect_silent(.StoppingAny())
  expect_valid(result, "StoppingAny")
})

test_that("StoppingAny object can be created with user constructor", {
  stop_list <- list(
    StoppingMinCohorts(nCohorts = 5),
    StoppingTargetProb(target = c(0.2, 0.45), prob = 0.6),
    StoppingMinPatients(nPatients = 30)
  )
  result <- expect_silent(
    StoppingAny(stop_list = stop_list, report_label = "custom label")
  )
  expect_valid(result, "StoppingAny")
  expect_identical(result@stop_list, stop_list)
  expect_identical(result@report_label, "custom label")
})

## StoppingTDCIRatio ----

test_that(".StoppingTDCIRatio works as expected", {
  result <- expect_silent(.StoppingTDCIRatio())
  expect_valid(result, "StoppingTDCIRatio")
})

test_that("StoppingTDCIRatio object can be created with user constructor (default)", {
  result <- expect_silent(StoppingTDCIRatio())
  expect_valid(result, "StoppingTDCIRatio")
  expect_identical(result@target_ratio, 5)
  expect_identical(result@prob_target, 0.3)
  expect_identical(result@report_label, NA_character_)
})

test_that("StoppingTDCIRatio object can be created with user constructor", {
  result <- expect_silent(StoppingTDCIRatio(6, 0.5, "custom label"))
  expect_valid(result, "StoppingTDCIRatio")
  expect_identical(result@target_ratio, 6)
  expect_identical(result@prob_target, 0.5)
  expect_identical(result@report_label, "custom label")
})

test_that("StoppingTDCIRatio replaces empty label with correct default label", {
  result <- expect_silent(StoppingTDCIRatio(6, 0.5, character(0)))
  expect_identical(result@report_label, "TD 6 for 0.5 target prob")
})

test_that(".DefaultStoppingTDCIRatio works as expected", {
  expect_equal(
    .DefaultStoppingTDCIRatio(),
    StoppingTDCIRatio(target_ratio = 5, prob_target = 0.3, report_label = NA_character_)
  )
})

## StoppingMaxGainCIRatio ----

test_that(".StoppingMaxGainCIRatio works as expected", {
  result <- expect_silent(.StoppingMaxGainCIRatio())
  expect_valid(result, "StoppingMaxGainCIRatio")
})

test_that("StoppingMaxGainCIRatio object can be created with user constructor (default)", {
  result <- expect_silent(StoppingMaxGainCIRatio())
  expect_valid(result, "StoppingMaxGainCIRatio")
  expect_identical(result@target_ratio, 5)
  expect_identical(result@prob_target, 0.3)
  expect_identical(result@report_label, NA_character_)
})

test_that("StoppingMaxGainCIRatio object can be created with user constructor", {
  result <- expect_silent(StoppingMaxGainCIRatio(6, 0.5, "custom label"))
  expect_valid(result, "StoppingMaxGainCIRatio")
  expect_identical(result@target_ratio, 6)
  expect_identical(result@prob_target, 0.5)
  expect_identical(result@report_label, "custom label")
})

test_that("StoppingMaxGainCIRatio replaces empty label with correct default label", {
  result <- expect_silent(StoppingMaxGainCIRatio(6, 0.5, character(0)))
  expect_identical(result@report_label, "GStar 6 for 0.5 target prob")
})

test_that(".DefaultStoppingMaxGainCIRatio works as expected", {
  expect_equal(
    .DefaultStoppingMaxGainCIRatio(),
    StoppingMaxGainCIRatio(target_ratio = 5, prob_target = 0.3, report_label = NA_character_)
  )
})

# CohortSize ----

## CohortSizeRange ----

test_that(".CohortSizeRange works as expected", {
  result <- expect_silent(.CohortSizeRange())
  expect_valid(result, "CohortSizeRange")
})

test_that("CohortSizeRange object can be created with user constructor", {
  result <- expect_silent(CohortSizeRange(c(0, 30, 50), c(20, 60, 90)))
  expect_valid(result, "CohortSizeRange")
  expect_identical(result@intervals, c(0, 30, 50))
  expect_identical(result@cohort_size, c(20L, 60L, 90L))
})

test_that(".DefaultCohortSizeRange works as expected", {
  expect_equal(
    .DefaultCohortSizeRange(),
    CohortSizeRange(intervals = c(0, 30), cohort_size = c(1, 3))
  )
})

## CohortSizeDLT ----

test_that(".CohortSizeDLT works as expected", {
  result <- expect_silent(.CohortSizeDLT())
  expect_valid(result, "CohortSizeDLT")
})

test_that("CohortSizeDLT object can be created with user constructor", {
  result <- expect_silent(CohortSizeDLT(c(0, 1, 2), c(20, 60, 90)))
  expect_valid(result, "CohortSizeDLT")
  expect_identical(result@intervals, c(0L, 1L, 2L))
  expect_identical(result@cohort_size, c(20L, 60L, 90L))
})

test_that(".DefaultCohortSizeDLT works as expected", {
  expect_equal(
    .DefaultCohortSizeDLT(),
    CohortSizeDLT(intervals = c(0, 1), cohort_size = c(1, 3))
  )
})

## CohortSizeConst ----

test_that(".CohortSizeConst works as expected", {
  result <- expect_silent(.CohortSizeConst())
  expect_valid(result, "CohortSizeConst")
})

test_that("CohortSizeConst object can be created with user constructor", {
  result <- expect_silent(CohortSizeConst(5))
  expect_valid(result, "CohortSizeConst")
  expect_identical(result@size, 5L)
})

test_that(".DefaultCohortSizeConst works as expected", {
  expect_equal(
    .DefaultCohortSizeConst(),
    CohortSizeConst(size = 3)
  )
})

## CohortSizeParts ----

test_that(".CohortSizeParts works as expected", {
  result <- expect_silent(.CohortSizeParts())
  expect_valid(result, "CohortSizeParts")
})

test_that("CohortSizeParts object can be created with user constructor", {
  result <- expect_silent(CohortSizeParts(c(1, 4)))
  expect_valid(result, "CohortSizeParts")
  expect_identical(result@cohort_sizes, c(1L, 4L))
})

## CohortSizeMax ----

test_that(".CohortSizeMax works as expected", {
  result <- expect_silent(.CohortSizeMax())
  expect_valid(result, "CohortSizeMax")
})

test_that("CohortSizeMax object can be created with user constructor", {
  cohort_sizes <- h_cohort_sizes()
  result <- expect_silent(CohortSizeMax(cohort_sizes = cohort_sizes))
  expect_valid(result, "CohortSizeMax")
  expect_identical(result@cohort_sizes, cohort_sizes)

  cohort_sizes <- h_cohort_sizes(three_rules = TRUE)
  result <- expect_silent(CohortSizeMax(cohort_sizes = cohort_sizes))
  expect_valid(result, "CohortSizeMax")
  expect_identical(result@cohort_sizes, cohort_sizes)
})

test_that(".DefaultCohortSizeMax works as expected", {
  expect_equal(
    .DefaultCohortSizeMax(),
    CohortSizeMax(
      cohort_sizes = list(
        CohortSizeRange(intervals = c(0, 10), cohort_size = c(1, 3)),
        CohortSizeDLT(intervals = c(0, 1), cohort_size = c(1, 3))
      )
    )
  )
})

## CohortSizeMin ----

test_that(".CohortSizeMin works as expected", {
  result <- expect_silent(.CohortSizeMin())
  expect_valid(result, "CohortSizeMin")
})

test_that("CohortSizeMin object can be created with user constructor", {
  cohort_sizes <- h_cohort_sizes()
  result <- expect_silent(CohortSizeMin(cohort_sizes = cohort_sizes))
  expect_valid(result, "CohortSizeMin")
  expect_identical(result@cohort_sizes, cohort_sizes)

  cohort_sizes <- h_cohort_sizes(three_rules = TRUE)
  result <- expect_silent(CohortSizeMin(cohort_sizes = cohort_sizes))
  expect_valid(result, "CohortSizeMin")
  expect_identical(result@cohort_sizes, cohort_sizes)
})

test_that(".DefaultCohortSizeMain works as expected", {
  expect_equal(
    .DefaultCohortSizeMin(),
    CohortSizeMin(
      cohort_sizes = list(
        CohortSizeRange(intervals = c(0, 10), cohort_size = c(1, 3)),
        CohortSizeDLT(intervals = c(0, 1), cohort_size = c(1, 3))
      )
    )
  )
})

# SafetyWindow ----

## SafetyWindowSize ----

test_that(".SafetyWindowSize works as expected", {
  result <- expect_silent(.SafetyWindowSize())
  expect_valid(result, "SafetyWindowSize")
})

test_that("SafetyWindowSize object can be created with user constructor", {
  result <- expect_silent(
    SafetyWindowSize(list(c(8, 3), c(9, 4)), c(1, 5), 8, 15)
  )
  expect_valid(result, "SafetyWindowSize")
  expect_identical(result@gap, list(c(8L, 3L), c(9L, 4L)))
  expect_identical(result@size, c(1L, 5L))
  expect_identical(result@follow, 8L)
  expect_identical(result@follow_min, 15L)
})

## SafetyWindowConst ----

test_that(".SafetyWindowConst works as expected", {
  result <- expect_silent(.SafetyWindowConst())
  expect_valid(result, "SafetyWindowConst")
})

test_that("SafetyWindowConst object can be created with user constructor", {
  result <- expect_silent(SafetyWindowConst(8, 2, 18))
  expect_valid(result, "SafetyWindowConst")
  expect_identical(result@gap, 8L)
  expect_identical(result@follow, 2L)
  expect_identical(result@follow_min, 18L)
})
