# IncrementsRelativeDLTCumulative-class ----

test_that(".IncrementsRelativeDLTCumulative works as expected", {
  result <- expect_silent(.IncrementsRelativeDLTCumulative())
  expect_valid(result, "IncrementsRelativeDLTCumulative")
})

# IncrementsRelativeDLTCumulative-constructor ----

test_that("IncrementsRelativeDLTCumulative object can be created with user constructor", {
  result <- expect_silent(IncrementsRelativeDLTCumulative())
  expect_valid(result, "IncrementsRelativeDLTCumulative")
})
