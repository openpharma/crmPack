# Extracted from test-Samples-methods.R:1414

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "crmPack", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
skip_on_cran_but_not_ci()

# test -------------------------------------------------------------------------
mcmcOptions <- McmcOptions(samples = 3)
samples <- Samples(
    data = list(
      alpha0 = -1:1,
      delta0 = c(0, 1, -1),
      alpha1 = -1:1,
      delta1 = c(-1, 0, 2)
    ),
    options = mcmcOptions
  )
model <- .DefaultLogisticLogNormalGrouped()
emptyData <- Data(doseGrid = seq(0.5, 10, by = 0.1))
result <- expect_silent(plot(samples, model, emptyData, group = "combo"))
expect_doppel("plot-Samples-LogisticLogNormalGrouped", result)
