# Extracted from test-Rules-methods.R:239

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
nb_ncrm <- NextBestNCRM(
    target = c(0.2, 0.35),
    overdose = c(0.35, 1),
    max_overdose_prob = 0.25
  )
result <- nextBest(
    nb_ncrm,
    Inf,
    samples,
    model,
    data,
    prob_plot_type = "bar"
  )
expect_identical(result$value, 75)
expect_snap(result$probs)
expect_doppel("Plot of nextBest-NextBestNCRM without doselimit", result$plot)
