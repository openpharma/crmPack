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
  expect_warning(
    expect_error(
      IncrementsRelativeDLTCurrent(
        DLTintervals = c(1, 0),
        increments = c(1, 3)
      ),
      "intervals has to be sorted and have unique values"
    ),
    "intervals has to be sorted and have unique values"
  )
})

test_that("IncrementsRelativeDLTCurrent warns if increments does not have same length as intervals", {
  expect_warning(
    IncrementsRelativeDLTCurrent(
      DLTintervals = c(0, 1),
      increments = c(1, 3, 100)
    ),
    "increments must have same length as intervals"
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
