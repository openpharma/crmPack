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
