# Extracted from test-Recommendation-class.R:225

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "crmPack", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
result <- expect_silent(.DefaultRecommendationAllocation())
expect_valid(result, "RecommendationAllocation")
testthat::expect_error(
    RecommendationAllocation(value = -1),
    "invalid class"
  )
testthat::expect_error(
    RecommendationAllocation(value = 1, details = list()),
    "invalid class"
  )
testthat::expect_error(
    RecommendationAllocation(
      value = 3,
      details = tibble::tibble(
        Dose = c(        1.5,   2.5,   3.5,   4.5,   6.0,   7.0),
        BadCol = c(0.075, 0.373, 0.281, 0.081, 0.038, 0.152)
      )
    ),
    "details must be a tibble with columns Dose and Allocation"
  )
