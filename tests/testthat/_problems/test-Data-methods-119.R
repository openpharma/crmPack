# Extracted from test-Data-methods.R:119

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "crmPack", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
data <- h_get_data()
result <- plot(data, blind = TRUE, legend = FALSE)
expect_doppel("plot-DataOrdinal-placebo-blinding-nolegend", result)
