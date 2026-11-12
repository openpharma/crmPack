# Extracted from test-Rules-methods1.R:914

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
nb_mg <- NextBestMaxGain(prob_target_drt = 0.45, prob_target_eot = 0.4)
result <- recommend(
    nb_mg,
    150,
    model = model_dlt,
    data = data,
    model_eff = model_eff
  )
expect_identical(result@value, 75)
