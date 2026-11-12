# Extracted from test-helpers_rules.R:705

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "crmPack", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
data <- h_get_data_dual(placebo = FALSE)
model_dlt <- h_get_logistic_indep_beta()
model_eff <- h_get_eff_log_log(const = 0)
result <- h_next_best_mg_plot(
    0.35,
    52.3,
    0.3,
    42.7,
    79.8,
    0.63,
    50,
    Inf,
    data,
    model_dlt,
    model_eff
  )
expect_doppel("h_next_best_mg_plot_nodoselim", result)
