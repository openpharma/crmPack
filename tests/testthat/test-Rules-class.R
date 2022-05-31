# NextBestMTD ----

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

test_that("NextBestMTD throws the error for non valid target", {
  expect_error(
    NextBestMTD(
      target = 1.2,
      derive = function(mtd_samples) {
        mean(mtd_samples)
      }
    ),
    ".* target must be probability > 0 and < 1"
  )
})

test_that("NextBestMTD throws the error for non valid argument of derive", {
  expect_error(
    NextBestMTD(
      target = 0.4,
      derive = function(x) {
        mean(mtd_samples)
      }
    ),
    ".* derive must have as single argument 'mtd_samples'"
  )
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


# NextBestNCRMLoss-class ----

test_that("NextBestNCRMLoss error: overdose has to be a probability range", {
  expect_error(
    NextBestNCRMLoss(
      target = c(0.2, 0.35),
      overdose = c(0.35, 0.6, 1),
      maxOverdoseProb = 0.25,
      losses = c(1, 0, 1, 2)
    ),
    "overdose has to be a probability range"
  )
})

test_that("NextBestNCRMLoss error: maxOverdoseProb has to be a probability", {
  expect_error(
    NextBestNCRMLoss(
      target = c(0.2, 0.35),
      overdose = c(0.35, 0.6, 1),
      maxOverdoseProb = 1.25,
      losses = c(1, 0, 1, 2)
    ),
    "maxOverdoseProb has to be a probability"
  )
})

test_that("NextBestNCRMLoss error: losses has to be a vector of non-negative elements", {
  expect_error(
    NextBestNCRMLoss(
      target = c(0.2, 0.35),
      overdose = c(0.35, 0.6),
      unacceptable = c(0.6, 1),
      maxOverdoseProb = 0.25,
      losses = c(-1, 0, 1, 2)
    ),
    "losses has to be a vector of non-negative elements"
  )
})
