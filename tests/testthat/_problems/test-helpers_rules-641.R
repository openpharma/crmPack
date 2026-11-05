# Extracted from test-helpers_rules.R:641

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "crmPack", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
data <- h_get_data(empty = TRUE, placebo = FALSE)
prob_dlt <- c(
    0.11,
    0.22,
    0.31,
    0.37,
    0.43,
    0.47,
    0.5,
    0.53,
    0.55,
    0.57,
    0.59,
    0.6
  )
result <- h_next_best_td_plot(0.33, 80, 0.27, 70, data, prob_dlt, 200, 75)
expect_doppel("h_next_best_td_plot", result)
