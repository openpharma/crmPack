# NextBest ----

## NextBestMTD ----

test_that(".NextBestMTD works as expected", {
  result <- expect_silent(.NextBestMTD())
  expect_valid(result, "NextBestMTD")
})

test_that("NextBestMTD object can be created with user constructor", {
  result <- expect_silent(
    NextBestMTD(
      target = 0.4,
      derive = function(mtd_samples) {
        mean(mtd_samples)
      }
    )
  )
  expect_valid(result, "NextBestMTD")
  expect_equal(result@target, 0.4)
  expect_equal(result@derive(c(1:5)), 3) # nolintr
})

## NextBestNCRM ----

test_that(".NextBestNCRM works as expected", {
  result <- expect_silent(.NextBestNCRM())
  expect_valid(result, "NextBestNCRM")
})

test_that("NextBestNCRM object can be created with user constructor", {
  result <- expect_silent(
    NextBestNCRM(
      target = c(0.5, 0.8),
      overdose = c(0.4, 1),
      max_overdose_prob = 0.3
    )
  )
  expect_valid(result, "NextBestNCRM")
  expect_equal(result@target, c(0.5, 0.8))
  expect_equal(result@overdose, c(0.4, 1))
  expect_equal(result@max_overdose_prob, 0.3)
})

## NextBestNCRMLoss ----

test_that(".NextBestNCRMLoss works as expected", {
  result <- expect_silent(.NextBestNCRMLoss())
  expect_valid(result, "NextBestNCRMLoss")
})

test_that("NextBestNCRMLoss object can be created with user constructor", {
  result <- expect_silent(
    NextBestNCRMLoss(
      target = c(0.2, 0.35),
      overdose = c(0.35, 0.6),
      unacceptable = c(0.6, 1),
      max_overdose_prob = 0.25,
      losses = c(1, 0, 1, 2)
    )
  )
  expect_equal(result@target, c(0.2, 0.35))
  expect_equal(result@overdose, c(0.35, 0.6))
  expect_equal(result@unacceptable, c(0.6, 1))
  expect_equal(result@max_overdose_prob, 0.25)
  expect_equal(result@losses, c(1, 0, 1, 2))
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
    NextBestDualEndpoint(
      target = c(0.5, 0.8),
      overdose = c(0.4, 1),
      max_overdose_prob = 0.3
    )
  )
  expect_valid(result, "NextBestDualEndpoint")
  expect_equal(result@target, c(0.5, 0.8))
  expect_equal(result@overdose, c(0.4, 1))
  expect_equal(result@target_relative, TRUE)
  expect_equal(result@target_thresh, 0.01)
})

test_that("NextBestDualEndpoint object can be created with user constructor 2", {
  result <- expect_silent(
    NextBestDualEndpoint(
      target = c(0.5, 0.8),
      target_relative = FALSE,
      overdose = c(0.4, 1),
      max_overdose_prob = 0.3,
      target_thresh = 0.05
    )
  )
  expect_valid(result, "NextBestDualEndpoint")
  expect_equal(result@target, c(0.5, 0.8))
  expect_equal(result@overdose, c(0.4, 1))
  expect_equal(result@target_relative, FALSE)
  expect_equal(result@target_thresh, 0.05)
})

## NextBestMinDist ----

test_that("NextBestMinDist works as expected", {
  result <- expect_silent(.NextBestMinDist())
  expect_valid(result, "NextBestMinDist")
})

test_that("NextBestMinDist object can be created with user constructor", {
  result <- expect_silent(
    NextBestMinDist(
      target = 0.3
    )
  )
  expect_valid(result, "NextBestMinDist")
  expect_equal(result@target, 0.3)
})

## NextBestInfTheory ----

test_that(".NextBestInfTheory works as expected", {
  result <- expect_silent(.NextBestInfTheory())
  expect_valid(result, "NextBestInfTheory")
})

test_that("NextBestInfTheory object can be created with user constructor", {
  result <- expect_silent(
    NextBestInfTheory(
      target = 0.4,
      asymmetry = 1.5
    )
  )
  expect_valid(result, "NextBestInfTheory")
  expect_equal(result@target, 0.4)
  expect_equal(result@asymmetry, 1.5)
})

## NextBestTDsamples ----

test_that(".NextBestTDsamples works as expected", {
  result <- expect_silent(.NextBestTDsamples())
  expect_valid(result, "NextBestTDsamples")
})

test_that("NextBestTDsamples object can be created with user constructor", {
  result <- expect_silent(
    NextBestTDsamples(
      prob_target_drt = 0.4,
      prob_target_eot = 0.35,
      derive = function(x) {
        mean(x)
      }
    )
  )
  expect_valid(result, "NextBestTDsamples")
  expect_equal(result@prob_target_drt, 0.4)
  expect_equal(result@prob_target_eot, 0.35)
  expect_equal(result@derive(c(1:5)), 3) # nolintr
})

# Increments ----

## IncrementsRelativeDLTCurrent-class ----

test_that(".IncrementsRelativeDLTCurrent works as expected", {
  result <- expect_silent(.IncrementsRelativeDLTCurrent())
  expect_valid(result, "IncrementsRelativeDLTCurrent")
})

## IncrementsRelativeDLTCurrent-constructor ----

test_that("IncrementsRelativeDLTCurrent object can be created with user constructor", {
  result <- expect_silent(IncrementsRelativeDLTCurrent())
  expect_valid(result, "IncrementsRelativeDLTCurrent")
})

test_that("IncrementsRelativeDLTCurrent fails if DLTintervals is not integer", {
  expect_error(
    IncrementsRelativeDLTCurrent(
      DLTintervals = c(0, 0.6, 1),
      increments = c(1, 0.50)
    ),
    "elements 2 of vector are not integers!"
  )
})

test_that("IncrementsRelativeDLTCurrent fails if DLTintervals is not sorted and unique", {
  expect_error(
    IncrementsRelativeDLTCurrent(
      DLTintervals = c(1, 0),
      increments = c(1, 3)
    ),
    "DLTintervals has to be sorted and have unique values"
  )
})

test_that("IncrementsRelativeDLTCurrent warns if increments does not have same length as intervals", {
  expect_error(
    IncrementsRelativeDLTCurrent(
      DLTintervals = c(0, 1),
      increments = c(1, 3, 100)
    ),
    "increments must have same length as DLTintervals"
  )
})

## IncrementsNumDoseLevels-class ----

test_that(".IncrementsNumDoseLevels works as expected", {
  result <- expect_silent(.IncrementsNumDoseLevels())
  expect_valid(result, "IncrementsNumDoseLevels")
})

## IncrementsNumDoseLevels-constructor ----

test_that("IncrementsNumDoseLevels object can be created with user constructor", {
  result <- expect_silent(IncrementsNumDoseLevels())
  expect_valid(result, "IncrementsNumDoseLevels")
})

## IncrementsHSRBeta-class ----

test_that(".IncrementsHSRBeta works as expected", {
  result <- expect_silent(.IncrementsHSRBeta())
  expect_valid(result, "IncrementsHSRBeta")
})

## IncrementsHSRBeta-constructor ----

test_that("IncrementsHSRBeta object can be created with user constructor", {
  result <- expect_silent(IncrementsHSRBeta())
  expect_valid(result, "IncrementsHSRBeta")
})

# Stopping ----

## StoppingMTDCV-class ----

test_that(".StoppingMTDCV works as expected", {
  result <- expect_silent(.StoppingMTDCV())
  expect_valid(result, "StoppingMTDCV")
})

## StoppingMTDCV-constructor ----

test_that("StoppingMTDCV object can be created with user constructor", {
  result <- expect_silent(StoppingMTDCV())
  expect_valid(result, "StoppingMTDCV")
})

## StoppingLowestDoseHSRBeta-class ----

test_that(".StoppingLowestDoseHSRBeta works as expected", {
  result <- expect_silent(.StoppingLowestDoseHSRBeta())
  expect_valid(result, "StoppingLowestDoseHSRBeta")
})

## StoppingLowestDoseHSRBeta-constructor ----

test_that("StoppingLowestDoseHSRBeta object can be created with user constructor", {
  result <- expect_silent(StoppingLowestDoseHSRBeta())
  expect_valid(result, "StoppingLowestDoseHSRBeta")
})
