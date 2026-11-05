# Extracted from test-helpers_rules.R:602

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "crmPack", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
result <- h_next_best_tdsamples_plot(
    1:100,
    50:150,
    100,
    120,
    c(25, 300),
    h_next_best_tdsamples(),
    75,
    60
  )
suppressWarnings({
    expect_doppel("h_next_best_tdsamples_plot", result)
  })
