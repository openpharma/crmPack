# Extracted from test-Samples-methods.R:1588

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "crmPack", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
skip_on_cran_but_not_ci()

# test -------------------------------------------------------------------------
data <- Data(
    x = c(25, 50, 50, 75, 100, 100, 225, 300),
    y = c(0, 0, 0, 0, 1, 1, 1, 1),
    doseGrid = seq(25, 300, 25),
    ID = 1L:8L,
    cohort = 1L:8L
  )
model <- LogisticIndepBeta(
    binDLE = c(1.05, 1.8),
    DLEweights = c(3, 3),
    DLEdose = c(25, 300),
    data = data
  )
actual <- plot(y = model, x = data)
expect_doppel("plot-Samples-ModelEffNoSamples", actual)
