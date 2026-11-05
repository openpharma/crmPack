# Extracted from test-Rules-methods1.R:1048

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "crmPack", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
skip_on_cran_but_not_ci()

# test -------------------------------------------------------------------------
data <- h_get_data_dual(placebo = TRUE)
model_dlt <- h_get_logistic_indep_beta()
model_eff <- h_get_eff_log_log(const = 5)
samples_dlt <- h_as_samples(
    list(
      phi1 = c(-4.03, -4.48, -4.07, -4.37, -4.5),
      phi2 = c(1.45, 0.86, 0.56, 0.42, 0.6)
    )
  )
samples_eff <- h_as_samples(
    list(
      theta1 = c(-2.93, -0.54, 0.01, -2.42, -1.5),
      theta2 = c(3.41, 0.61, 0.58, 1.35, 2),
      nu = c(2.14, 4.63, 0.83, 2.98, 1.6)
    )
  )
nb_mgs <- h_next_best_mgsamples(td = 0.5, te = 0.45, p = 0.25, p_gstar = 0.3)
result <- recommend(
    nb_mgs,
    60,
    samples_dlt,
    model_dlt,
    data,
    model_eff,
    samples_eff
  )
expect_identical(result@value, 50)
