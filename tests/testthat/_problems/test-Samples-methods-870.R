# Extracted from test-Samples-methods.R:870

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "crmPack", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
skip_on_cran_but_not_ci()

# test -------------------------------------------------------------------------
data <- DataDual(
    x = c(25, 50, 50, 75, 100, 100, 225, 300),
    y = c(0, 0, 0, 0, 1, 1, 1, 1),
    w = c(0.31, 0.42, 0.59, 0.45, 0.6, 0.7, 0.6, 0.52),
    doseGrid = seq(25, 300, 25),
    placebo = FALSE,
    ID = 1L:8L,
    cohort = as.integer(c(1, 2, 2, 3, 4, 4, 5, 6))
  )
Effmodel <- Effloglog(
    eff = c(1.223, 2.513),
    eff_dose = c(25, 300),
    nu = c(a = 1, b = 0.025),
    data = data
  )
result <- plot(x = data, y = Effmodel)
expect_doppel("samples-plot-modeleff-nosamples", result)
