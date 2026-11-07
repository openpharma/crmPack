# Extracted from test-Data-methods.R:58

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "crmPack", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
data <- h_get_data()
result <- plot(data, mark_response = TRUE, mark_backfill = TRUE)
expect_doppel(
    "Plot of Data with response and backfill markers",
    result
  )
