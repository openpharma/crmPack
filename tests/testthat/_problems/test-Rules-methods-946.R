# Extracted from test-Rules-methods.R:946

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "crmPack", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
skip_on_cran_but_not_ci()

# test -------------------------------------------------------------------------
data <- h_get_data(placebo = FALSE)
model <- h_get_logistic_log_normal()
samples <- h_as_samples(
    list(alpha0 = c(-1.8, -3.8, -2.2, -1.6), alpha1 = c(1.7, 3.3, 5.1, 2.2))
  )
nb_md <- NextBestMinDist(target = 0.3)
result <- nextBest(nb_md, Inf, samples, model, data)
expect_identical(result$value, 75)
expect_snap(result$probs)
expect_doppel("Plot nextBest-NextBestMinDist w/o doselimit", result$plot)
