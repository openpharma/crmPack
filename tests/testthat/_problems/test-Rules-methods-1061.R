# Extracted from test-Rules-methods.R:1061

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "crmPack", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
skip_on_cran_but_not_ci()

# test -------------------------------------------------------------------------
data <- h_get_data(placebo = FALSE)
model <- h_get_logistic_indep_beta()
samples <- h_as_samples(
    list(
      phi1 = c(
        -6.99,
        -6.99,
        -8.58,
        -8.62,
        -8.62,
        -8.62,
        -8.62,
        -8.23,
        -8.71,
        -8.71
      ),
      phi2 = c(1.69, 1.69, 1.26, 1.72, 1.72, 1.72, 1.72, 1.78, 1.74, 1.74)
    )
  )
nb_tds <- h_next_best_tdsamples()
result <- nextBest(nb_tds, Inf, samples, model, data)
expected <- list(
    next_dose_drt = 100,
    prob_target_drt = 0.45,
    dose_target_drt = 120.4065,
    next_dose_eot = 100,
    prob_target_eot = 0.4,
    dose_target_eot = 107.1014,
    ci_dose_target_eot = c(49.21382, 535.88506),
    ci_ratio_dose_target_eot = 10.88891
  )
expect_identical(result[names(expected)], expected, tolerance = 10e-7)
suppressWarnings({
    expect_doppel("Plot of nextBest-NextBestTDsamples_nodoselim", result$plot)
  })
