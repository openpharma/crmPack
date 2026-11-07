# Extracted from test-Rules-methods.R:1140

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "crmPack", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
skip_on_cran_but_not_ci()

# test -------------------------------------------------------------------------
data <- h_get_data(placebo = FALSE)
model <- h_get_logistic_indep_beta()
nb_td <- NextBestTD(prob_target_drt = 0.45, prob_target_eot = 0.4)
result <- nextBest(nb_td, Inf, model = model, data = data)
expected <- list(
    next_dose_drt = 75,
    prob_target_drt = 0.45,
    dose_target_drt = 75.82941,
    next_dose_eot = 50,
    prob_target_eot = 0.4,
    dose_target_eot = 63.21009,
    ci_dose_target_eot = c(20.38729, 195.98072),
    ci_ratio_dose_target_eot = 9.612886
  )
expect_identical(result[names(expected)], expected, tolerance = 10e-7)
expect_doppel("Plot of nextBest-NextBestTD_nodoselim", result$plot)
