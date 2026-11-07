# Extracted from test-Rules-methods.R:899

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "crmPack", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
skip_on_cran_but_not_ci()

# test -------------------------------------------------------------------------
data <- h_get_data_dual(placebo = FALSE)
model <- h_get_dual_endpoint_rw()
samples <- h_samples_dual_endpoint_rw()
nb_de <- NextBestDualEndpoint(
    target = c(0.9, 1),
    overdose = c(0.65, 1),
    max_overdose_prob = 0.55,
    target_relative = FALSE
  )
result <- nextBest(
    nb_de,
    Inf,
    samples,
    model,
    data,
    prob_plot_type = "bar"
  )
expect_identical(result$value, 100)
expect_snap(result$probs)
expect_doppel("nextBest-NextBestDualEndpoint_atgt_nodlim", result$plot)
