# Extracted from test-helpers_rules.R:531

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "crmPack", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
prob_mat <- matrix(
    c(
      0.61,
      0.084,
      0.024,
      0.982,
      0.244,
      0.024,
      0.326,
      0.284,
      0.138,
      0.016,
      0.338,
      0.154,
      0.058,
      0.492,
      0.548,
      0.002,
      0.378,
      0.544,
      0.006,
      0.14,
      0.29,
      0.17,
      0.04,
      0.278
    ),
    byrow = FALSE,
    ncol = 4,
    dimnames = list(
      NULL,
      c("underdosing", "target", "excessive", "unacceptable")
    )
  )
posterior_loss <- c(0.68, 0.856, 1.152, 0.984, 0.702, 1.124)
result <- h_next_best_ncrm_loss_plot(
    prob_mat,
    posterior_loss,
    0.9,
    seq(10, by = 5, length.out = 6),
    5,
    20,
    15,
    TRUE,
    prob_plot_type = "bar"
  )
expect_doppel("h_next_best_ncrm_loss_plot_unacpt", result$plot_joint)
