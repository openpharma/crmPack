# Extracted from test-Rules-methods.R:303

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "crmPack", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
skip_on_cran_but_not_ci()

# test -------------------------------------------------------------------------
data <- Data(
    doseGrid = c(0.001, 0.01, 0.05, 0.15, 0.5, 1, 2, 4, 8, 16)
  )
model <- h_get_logistic_log_normal()
samples <- h_as_samples(
    list(alpha0 = c(-1.8, -3.8, -2.2, -1.6), alpha1 = c(1.7, 3.3, 5.1, 2.2))
  )
nb_ncrm <- NextBestNCRM(
    target = c(0.2, 0.35),
    overdose = c(0.35, 1),
    max_overdose_prob = 0.25
  )
result_lollipop <- nextBest(nb_ncrm, 16, samples, model, data)
result_bar <- nextBest(
    nb_ncrm,
    16,
    samples,
    model,
    data,
    prob_plot_type = "bar"
  )
expect_identical(
    unname(vapply(
      result_lollipop$singlePlots$plot1$layers,
      function(layer) class(layer$geom)[1L],
      character(1L)
    )),
    c("GeomHline", "GeomVline", "GeomSegment", "GeomPoint")
  )
expect_identical(
    unname(vapply(
      result_lollipop$singlePlots$plot2$layers,
      function(layer) class(layer$geom)[1L],
      character(1L)
    )),
    c("GeomVline", "GeomVline", "GeomSegment", "GeomPoint", "GeomPoint")
  )
expect_identical(
    unname(vapply(
      result_bar$singlePlots$plot1$layers,
      function(layer) class(layer$geom)[1L],
      character(1L)
    )),
    c("GeomHline", "GeomVline", "GeomBar")
  )
expect_identical(
    unname(vapply(
      result_bar$singlePlots$plot2$layers,
      function(layer) class(layer$geom)[1L],
      character(1L)
    )),
    c("GeomVline", "GeomVline", "GeomBar", "GeomPoint")
  )
expect_identical(result_bar$value, result_lollipop$value)
expect_equal(result_bar$probs, result_lollipop$probs)
expect_doppel(
    "plot-nextbest-nextbestncrm-lollipop-uneven-dose-grid",
    result_lollipop$plot
  )
