# Extracted from test-Rules-methods1.R:848

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "crmPack", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
skip_on_cran_but_not_ci()

# test -------------------------------------------------------------------------
data <- h_get_data(placebo = FALSE)
model <- h_get_logistic_indep_beta()
nb_td <- NextBestTD(prob_target_drt = 0.25, prob_target_eot = 0.2)
result <- recommend(nb_td, 70, model = model, data = data)
expect_identical(result@value, 25)
