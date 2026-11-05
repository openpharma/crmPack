# Extracted from test-Rules-methods1.R:783

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
result <- recommend(nb_tds, Inf, samples, model, data)
expect_identical(result@value, 100)
