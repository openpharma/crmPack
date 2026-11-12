# Extracted from test-helpers_rules.R:585

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "crmPack", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
prob_mat <- matrix(
    c(
      0.084,
      0.024,
      0.156,
      0.172,
      0.024,
      1,
      0.284,
      0.122,
      0.348,
      0.336,
      0.138,
      0.22,
      0.632,
      0.854,
      0.496,
      0.492,
      0.838,
      0.42
    ),
    byrow = FALSE,
    ncol = 3,
    dimnames = list(NULL, c("underdosing", "target", "overdose"))
  )
posterior_loss <- c(0.856, 1.182, 0.746, 0.738, 1.152, 1)
result <- h_next_best_ncrm_loss_plot(
    prob_mat,
    posterior_loss,
    0.9,
    seq(10, by = 5, length.out = 6),
    5,
    Inf,
    25,
    FALSE,
    prob_plot_type = "bar"
  )
expect_doppel("h_next_best_ncrm_loss_plot_nodoselim", result$plot_joint)
