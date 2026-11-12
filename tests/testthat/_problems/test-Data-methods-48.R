# Extracted from test-Data-methods.R:48

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "crmPack", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
data <- h_get_data()
result <- plot(data, blind = TRUE, legend = FALSE)
expect_doppel(
    "Plot of Data with placebo, blinding and no legend",
    result
  )
