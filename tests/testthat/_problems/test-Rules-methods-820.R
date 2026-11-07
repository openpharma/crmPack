# Extracted from test-Rules-methods.R:820

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
    overdose = c(0.45, 1),
    max_overdose_prob = 0.25
  )
result <- nextBest(
    nb_de,
    133,
    samples,
    model,
    data,
    prob_plot_type = "bar"
  )
expect_identical(result$value, 25)
expect_snap(result$probs)
overdose_layers <- result$singlePlots$plot1$layers
expect_identical(
    unname(vapply(overdose_layers, function(x) class(x$geom)[1L], character(1L))),
    c("GeomHline", "GeomVline", "GeomBar")
  )
target_layers <- result$singlePlots$plot2$layers
expect_identical(
    unname(vapply(target_layers, function(x) class(x$geom)[1L], character(1L))),
    c("GeomVline", "GeomVline", "GeomBar", "GeomPoint")
  )
expect_identical(target_layers[[4L]]$aes_params$colour, "blue")
expect_identical(target_layers[[4L]]$aes_params$fill, "blue")
selected_bar <- target_layers[[3L]]$data$y[
    target_layers[[3L]]$data$Dose == result$value
  ]
expect_equal(target_layers[[4L]]$data$y - selected_bar, 10)
expect_doppel("Plot of nextBest-NextBestDualEndpoint", result$plot)
