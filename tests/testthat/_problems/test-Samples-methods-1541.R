# Extracted from test-Samples-methods.R:1541

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "crmPack", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
skip_on_cran_but_not_ci()

# test -------------------------------------------------------------------------
data <- DataDual(
    x = c(25, 50, 25, 50, 75, 300, 250, 150),
    y = c(0, 0, 0, 0, 0, 1, 1, 0),
    w = c(0.31, 0.42, 0.59, 0.45, 0.6, 0.7, 0.6, 0.52),
    doseGrid = seq(25, 300, 25),
    placebo = FALSE,
    ID = 1L:8L,
    cohort = 1L:8L
  )
model <- Effloglog(
    eff = c(1.223, 2.513),
    eff_dose = c(25, 300),
    nu = c(a = 1, b = 0.025),
    data = data
  )
options <- McmcOptions(
    burnin = 100,
    step = 2,
    samples = 200,
    rng_kind = "Mersenne-Twister",
    rng_seed = 565409
  )
samples <- mcmc(data = data, model = model, options = options)
actual <- plot(x = samples, y = model, data = data)
expect_doppel("plot-Samples-ModelEffloglog", actual)
