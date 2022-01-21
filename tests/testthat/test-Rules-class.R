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
