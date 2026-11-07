# Extracted from test-Rules-methods.R:346

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
result_linear <- nextBest(nb_ncrm, 16, samples, model, data)
result_log <- nextBest(
    nb_ncrm,
    16,
    samples,
    model,
    data,
    dose_scale = "log"
  )
target_scale <- result_log$singlePlots$plot1$scales$get_scales("x")
overdose_scale <- result_log$singlePlots$plot2$scales$get_scales("x")
uses_log_scale <- all(vapply(
    list(target_scale, overdose_scale),
    function(scale) {
      !is.null(scale) && grepl("^log", scale$trans$name)
    },
    logical(1L)
  ))
expect_identical(result_log$value, result_linear$value)
expect_equal(result_log$probs, result_linear$probs)
expect_true(uses_log_scale)
expect_doppel("plot-nextbest-nextbestncrm-log-dose-scale", result_log$plot)
