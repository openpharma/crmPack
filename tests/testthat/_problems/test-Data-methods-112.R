# Extracted from test-Data-methods.R:112

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "crmPack", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
data <- h_get_data_ordinal()
result <- plot(data, blind = TRUE)
expect_doppel("plot-DataOrdinal-placebo-blinding", result)
