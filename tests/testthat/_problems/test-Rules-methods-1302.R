# Extracted from test-Rules-methods.R:1302

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
samples_dlt <- h_as_samples(
    list(phi1 = c(-4.03, -4.48, -4.07, -4.37), phi2 = c(1.45, 0.86, 0.56, 0.42))
  )
samples_eff <- h_as_samples(
    list(
      theta1 = c(-2.93, -0.54, 0.01, -2.42),
      theta2 = c(3.41, 0.61, 0.58, 1.35),
      nu = c(2.14, 4.63, 0.83, 2.98)
    )
  )
nb_mgs <- h_next_best_mgsamples()
result <- nextBest(
    nb_mgs,
    49,
    samples_dlt,
    model_dlt,
    data,
    model_eff,
    samples_eff
  )
expected <- list(
    next_dose = 25,
    prob_target_drt = 0.45,
    dose_target_drt = 131.8022,
    next_dose_drt = 25,
    prob_target_eot = 0.4,
    dose_target_eot = 103.9855,
    next_dose_eot = 25,
    dose_max_gain = 125,
    next_dose_max_gain = 25,
    ci_dose_target_eot = c(103.9855, 103.9855),
    ci_ratio_dose_target_eot = 1,
    ci_dose_max_gain = c(30.625, 288.750),
    ci_ratio_dose_max_gain = 9.428571
  )
expect_identical(result[names(expected)], expected, tolerance = 10e-7)
expect_doppel("Plot of nextBest-NextBestMaxGainSamples", result$plot)
