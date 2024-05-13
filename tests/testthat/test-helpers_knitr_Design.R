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
      "toxicity and the target toxicity rate [0.3].\n\n"
    )
  )
  x1 <- NextBestMinDist(target = 0.25)
  expect_equal(
    knit_print(x1, asis = FALSE, tox_label = "DLT"),
    paste0(
      "The dose recommended for the next cohort will be the one which is both ",
      "eligible and which has the smallest absolute difference between its ",
      "mean posterior estimate of the probability of DLT and the target DLT ",
      "rate [0.25].\n\n"
    )
  )
})
