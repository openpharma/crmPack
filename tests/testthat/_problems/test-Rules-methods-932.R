# Extracted from test-Rules-methods.R:932

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "crmPack", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
skip_on_cran_but_not_ci()

# test -------------------------------------------------------------------------
data <- h_get_data(placebo = TRUE)
model <- h_get_logistic_log_normal()
samples <- h_as_samples(
    list(
      alpha0 = c(-0.38, -0.13, 1.43, 2.57),
      alpha1 = c(1.67, 1.3, 1.77, 2.51)
    )
  )
nb_md <- NextBestMinDist(target = 0.1)
result <- nextBest(nb_md, 40, samples, model, data)
expect_identical(result$value, 25)
expect_snap(result$probs)
expect_doppel("Plot of nextBest-NextBestMinDist with placebo", result$plot)
