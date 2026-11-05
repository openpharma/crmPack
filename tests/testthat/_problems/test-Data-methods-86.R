# Extracted from test-Data-methods.R:86

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "crmPack", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
data <- h_get_data_da()
result <- plot(data)
expect_doppel("Plot of DataDA with placebo", result)
