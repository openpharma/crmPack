# Extracted from test-Rules-methods.R:1225

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "crmPack", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
skip_on_cran_but_not_ci()

# test -------------------------------------------------------------------------
data <- h_get_data_dual(placebo = FALSE)
model_dlt <- h_get_logistic_indep_beta()
model_eff <- h_get_eff_log_log(const = 5)
nb_mg <- NextBestMaxGain(prob_target_drt = 0.35, prob_target_eot = 0.3)
result <- nextBest(
    nb_mg,
    Inf,
    model = model_dlt,
    data = data,
    model_eff = model_eff
  )
expected <- list(
    next_dose = 50,
    prob_target_drt = 0.35,
    dose_target_drt = 52.28128,
    next_dose_drt = 50,
    prob_target_eot = 0.3,
    dose_target_eot = 42.68131,
    next_dose_eot = 25,
    dose_max_gain = 83.96469,
    next_dose_max_gain = 75,
    ci_dose_target_eot = c(11.06619, 164.61798),
    ci_ratio_dose_target_eot = 14.87575,
    ci_dose_max_gain = c(23.09875, 305.21431),
    ci_ratio_dose_max_gain = 13.21345
  )
expect_identical(result[names(expected)], expected, tolerance = 10e-7)
expect_doppel("Plot of nextBest-NextBestMaxGain_nodoselim", result$plot)
