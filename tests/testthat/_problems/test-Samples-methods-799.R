# Extracted from test-Samples-methods.R:799

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "crmPack", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
skip_on_cran_but_not_ci()

# test -------------------------------------------------------------------------
data <- DataDual(
    x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10, 20, 20, 20, 40, 40, 40, 50, 50, 50),
    y = c(0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1),
    w = c(
      0.31,
      0.42,
      0.59,
      0.45,
      0.6,
      0.7,
      0.55,
      0.6,
      0.52,
      0.54,
      0.56,
      0.43,
      0.41,
      0.39,
      0.34,
      0.38,
      0.21
    ),
    doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2)),
    ID = 1L:17L,
    cohort = as.integer(c(1:5, rep(6:9, each = 3)))
  )
model <- DualEndpointRW(
    mean = c(0, 1),
    cov = matrix(c(1, 0, 0, 1), nrow = 2),
    sigma2betaW = 0.01,
    sigma2W = c(a = 0.1, b = 0.1),
    rho = c(a = 1, b = 1),
    rw1 = TRUE
  )
options <- McmcOptions(
    burnin = 100,
    step = 2,
    samples = 2000,
    rng_kind = "Mersenne-Twister",
    rng_seed = 393015
  )
samples <- mcmc(data, model, options)
actual <- plot(x = samples, y = model, data = data)
expect_doppel("plot-Samples-DataDual", actual)
