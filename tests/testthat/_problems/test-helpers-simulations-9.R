# Extracted from test-helpers-simulations.R:9

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "crmPack", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
set.seed(504409)
plot <- h_barplot_percentages(
    rpois(n = 100, lambda = 2),
    "test"
  )
expect_doppel("barplot-percentages", plot)
