# Extracted from test-Rules-methods1.R:959

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
result <- recommend(
    nb_mgs,
    49,
    samples_dlt,
    model_dlt,
    data,
    model_eff,
    samples_eff
  )
expect_identical(result@value, 25)
