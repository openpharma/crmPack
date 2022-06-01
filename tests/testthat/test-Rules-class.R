# nextBest ----

## NextBestMTD ----

test_that(".NextBestMTD works as expected", {
  result <- expect_silent(.NextBestMTD())
  expect_valid(result, "NextBestMTD")
})

test_that("NextBestMTD object can be created with user constructor", {
  result <- expect_silent(
    NextBestMTD(
      target = 0.3,
      derive = function(mtd_samples) {
        mean(mtd_samples)
      }
    )
  )
  expect_valid(result, "NextBestMTD")
})

## NextBestNCRM ----

test_that(".NextBestNCRM works as expected", {
  result <- expect_silent(.NextBestNCRM())
  expect_valid(result, "NextBestNCRM")
})

test_that("NextBestNCRM object can be created with user constructor", {
  result <- expect_silent(
    NextBestNCRM(
      target = c(0.2, 0.35),
      overdose = c(0.35, 1),
      max_overdose_prob = 0.25
    )
  )
  expect_valid(result, "NextBestNCRM")
})

## NextBestInfTheory ----

test_that(".NextBestInfTheory works as expected", {
  result <- expect_silent(.NextBestInfTheory())
  expect_valid(result, "NextBestInfTheory")
})

test_that("NextBestNCRM object can be created with user constructor", {
  expect_silent(NextBestInfTheory(target = 0.25, asymmetry = 0.1))
})

# IncrementsRelativeDLTCurrent-class ----

test_that(".IncrementsRelativeDLTCurrent works as expected", {
  result <- expect_silent(.IncrementsRelativeDLTCurrent())
  expect_valid(result, "IncrementsRelativeDLTCurrent")
})

# IncrementsRelativeDLTCurrent-constructor ----

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

# IncrementsNumDoseLevels-class ----

test_that(".IncrementsNumDoseLevels works as expected", {
  result <- expect_silent(.IncrementsNumDoseLevels())
  expect_valid(result, "IncrementsNumDoseLevels")
})

# IncrementsNumDoseLevels-constructor ----

test_that("IncrementsNumDoseLevels object can be created with user constructor", {
  result <- expect_silent(IncrementsNumDoseLevels())
  expect_valid(result, "IncrementsNumDoseLevels")
})



# IncrementsHSRBeta-class ----

test_that(".IncrementsHSRBeta works as expected", {
  result <- expect_silent(.IncrementsHSRBeta())
  expect_valid(result, "IncrementsHSRBeta")
})

# IncrementsHSRBeta-constructor ----

test_that("IncrementsHSRBeta object can be created with user constructor", {
  result <- expect_silent(IncrementsHSRBeta())
  expect_valid(result, "IncrementsHSRBeta")
})



# StoppingMTDCV-class ----

test_that(".StoppingMTDCV works as expected", {
  result <- expect_silent(.StoppingMTDCV())
  expect_valid(result, "StoppingMTDCV")
})

# StoppingMTDCV-constructor ----

test_that("StoppingMTDCV object can be created with user constructor", {
  result <- expect_silent(StoppingMTDCV())
  expect_valid(result, "StoppingMTDCV")
})


# StoppingLowestDoseHSRBeta-class ----

test_that(".StoppingLowestDoseHSRBeta works as expected", {
  result <- expect_silent(.StoppingLowestDoseHSRBeta())
  expect_valid(result, "StoppingLowestDoseHSRBeta")
})

# StoppingLowestDoseHSRBeta-constructor ----

test_that("StoppingLowestDoseHSRBeta object can be created with user constructor", {
  result <- expect_silent(StoppingLowestDoseHSRBeta())
  expect_valid(result, "StoppingLowestDoseHSRBeta")
})
