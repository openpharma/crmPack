# Extracted from test-helpers_rules.R:737

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "crmPack", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
result <- h_next_best_mgsamples_plot(
    0.35,
    67.5,
    0.3,
    53,
    125,
    c(300, 225, 50, 175, 75, 125, 25, 125, 125, 250),
    50,
    Inf,
    c(25, 300)
  )
expect_doppel("h_next_best_mgsamples_plot_nodoselim", result)
