# Extracted from test-helpers_rules.R:722

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "crmPack", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
result <- h_next_best_mgsamples_plot(
    0.45,
    104,
    0.4,
    83,
    200,
    c(100, 300, 250, 125, 100, 175, 50, 300, 300, 150),
    75,
    80,
    c(50, 320)
  )
expect_doppel("h_next_best_mgsamples_plot", result)
