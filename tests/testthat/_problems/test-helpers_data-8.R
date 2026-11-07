# Extracted from test-helpers_data.R:8

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "crmPack", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
data <- h_get_data()
result <- h_plot_data_dataordinal(x = data)
expect_doppel(
    "Helper result of data plot",
    result
  )
