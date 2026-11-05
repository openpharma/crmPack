# Extracted from test-Rules-methods.R:212

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
    45,
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
expect_equal(
    result$singlePlots$plot1$coordinates$limits$y,
    result$singlePlots$plot2$coordinates$limits$y
  )
triangle_result <- nextBest(
    nb_ncrm,
    45,
    samples,
    model,
    data,
    prob_plot_type = "bar",
    safe_dose_marker = "triangle"
  )
triangle_layers <- triangle_result$singlePlots$plot1$layers
expect_identical(
    unname(vapply(triangle_layers, function(x) class(x$geom)[1L], character(1L))),
    c("GeomHline", "GeomBar", "GeomPoint")
  )
expect_identical(triangle_layers[[3L]]$aes_params$colour, "red")
expect_identical(triangle_layers[[3L]]$aes_params$fill, "red")
expect_equal(triangle_layers[[3L]]$data$x, 25)
expect_error(
    nextBest(nb_ncrm, 45, samples, model, data, safe_dose_marker = "invalid"),
    "should be one of"
  )
expect_doppel("Plot of nextBest-NextBestNCRM", result$plot)
