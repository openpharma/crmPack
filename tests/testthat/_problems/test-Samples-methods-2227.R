# Extracted from test-Samples-methods.R:2227

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "crmPack", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
skip_on_cran_but_not_ci()

# test -------------------------------------------------------------------------
data <- Data(
    x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
    y = c(0, 0, 0, 0, 0, 0, 1, 0),
    ID = 1L:8L,
    cohort = c(0, 1, 2, 3, 4, 5, 5, 5),
    doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2))
  )
model <- LogisticLogNormal(
    mean = c(-0.85, 1),
    cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
    ref_dose = 56
  )
options <- McmcOptions(
    burnin = 100,
    step = 2,
    samples = 2000,
    rng_seed = 544914,
    rng_kind = "Mersenne-Twister"
  )
samples <- mcmc(data, model, options)
actual <- approximate(
    object = samples,
    model = model,
    data = data,
    control = list(threshold.stop = 0.1, max.time = 1, maxit = 1)
  )
expect_equal(length(actual), 2)
expect_set_equal(names(actual), c("model", "plot"))
for (slot_name in slotNames(actual$model)) {
    if (!is.function(slot(actual$model, slot_name))) {
      expect_snap(slot(actual$model, slot_name))
    }
  }
expect_doppel("approximate-Samples", actual$plot)
