# Extracted from test-Samples-methods.R:1651

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
DLEmodel <- LogisticIndepBeta(
    binDLE = c(1.05, 1.8),
    DLEweights = c(3, 3),
    DLEdose = c(25, 300),
    data = data
  )
Effmodel <- Effloglog(
    eff = c(1.223, 2.513),
    eff_dose = c(25, 300),
    nu = c(a = 1, b = 0.025),
    data = data,
    const = 0
  )
data1 <- Data(
    x = data@x,
    y = data@y,
    doseGrid = data@doseGrid,
    ID = 1L:8L,
    cohort = 1L:8L
  )
optionsDLE <- McmcOptions(
    burnin = 1000,
    step = 2,
    samples = 10000,
    rng_kind = "Mersenne-Twister",
    rng_seed = 114810
  )
optionsTox <- McmcOptions(
    burnin = 1000,
    step = 2,
    samples = 10000,
    rng_kind = "Mersenne-Twister",
    rng_seed = 265310
  )
DLEsamples <- mcmc(data = data1, model = DLEmodel, options = optionsDLE)
Effsamples <- mcmc(data = data, model = Effmodel, options = optionsTox)
actual <- plotGain(
    DLEmodel = DLEmodel,
    DLEsamples = DLEsamples,
    Effmodel = Effmodel,
    Effsamples = Effsamples,
    data = data
  )
expect_doppel("plotGain-ModelTox-Samples-ModelEff-Samples", actual)
