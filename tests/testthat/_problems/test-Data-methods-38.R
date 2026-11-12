# Extracted from test-Data-methods.R:38

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "crmPack", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
data <- h_get_data()
result <- plot(data, blind = TRUE)
expect_doppel("Plot of Data with placebo and blinding", result)
