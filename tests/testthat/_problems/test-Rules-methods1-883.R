# Extracted from test-Rules-methods1.R:883

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
result <- recommend(
    nb_mg,
    Inf,
    model = model_dlt,
    data = data,
    model_eff = model_eff
  )
expect_identical(result@value, 50)
