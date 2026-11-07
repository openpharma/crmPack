# Extracted from test-Samples-methods.R:1391

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "crmPack", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
skip_on_cran_but_not_ci()

# test -------------------------------------------------------------------------
data <- Data(
    x = c(25, 50, 50, 75, 150, 200, 225, 300),
    y = c(0, 0, 0, 0, 1, 1, 1, 1),
    doseGrid = seq(from = 25, to = 300, by = 25),
    ID = 1L:8L,
    cohort = as.integer(c(1, 2, 2, 3:7))
  )
model <- LogisticIndepBeta(
    binDLE = c(1.05, 1.8),
    DLEweights = c(3, 3),
    DLEdose = c(25, 300),
    data = data
  )
options <- McmcOptions(
    burnin = 500,
    step = 2,
    samples = 5000,
    rng_kind = "Mersenne-Twister",
    rng_seed = 565409
  )
samples <- mcmc(data = data, model = model, options = options)
actual <- plot(x = samples, y = model, data = data)
expect_doppel("plot-Samples-ModelTox", actual)
