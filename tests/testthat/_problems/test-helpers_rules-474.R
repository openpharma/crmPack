# Extracted from test-helpers_rules.R:474

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
    20,
    15,
    FALSE,
    prob_plot_type = "bar"
  )
overdose_layers <- result$plots_single$plot1$layers
expect_identical(
    unname(vapply(overdose_layers, function(x) class(x$geom)[1L], character(1L))),
    c("GeomHline", "GeomVline", "GeomBar")
  )
target_layers <- result$plots_single$plot2$layers
expect_identical(
    unname(vapply(target_layers, function(x) class(x$geom)[1L], character(1L))),
    c("GeomVline", "GeomVline", "GeomBar")
  )
loss_layers <- result$plots_single$plot_loss$layers
expect_identical(loss_layers[[2L]]$aes_params$colour, "blue")
expect_identical(loss_layers[[2L]]$aes_params$fill, "blue")
loss_plot_data <- ggplot_build(result$plots_single$plot_loss)$data
expect_equal(loss_plot_data[[2L]]$y - max(loss_plot_data[[1L]]$y), 0.3)
expect_doppel("h_next_best_ncrm_loss_plot", result$plot_joint)
