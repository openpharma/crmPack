# Extracted from test-Samples-methods.R:2087

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "crmPack", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
skip_on_cran_but_not_ci()

# test -------------------------------------------------------------------------
skip_on_cran()
data <- DataDA(
    x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
    y = c(0, 0, 1, 1, 0, 0, 1, 0),
    doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2)),
    u = c(42, 30, 15, 5, 20, 25, 30, 60),
    t0 = c(0, 15, 30, 40, 55, 70, 75, 85),
    Tmax = 60,
    ID = 1L:8L,
    cohort = 1L:8L
  )
npiece_ <- 10
lambda_prior <- function(k) {
    npiece_ / (data@Tmax * (npiece_ - k + 0.5))
  }
model <- DALogisticLogNormal(
    mean = c(-0.85, 1),
    cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
    ref_dose = 56,
    npiece = npiece_,
    l = as.numeric(t(apply(
      as.matrix(c(1:npiece_), 1, npiece_),
      2,
      lambda_prior
    ))),
    c_par = 2
  )
options <- McmcOptions(
    burnin = 100,
    step = 2,
    samples = 1000,
    rng_kind = "Mersenne-Twister",
    rng_seed = 552914
  )
samples <- mcmc(data, model, options)
actual <- plot(samples, model, data)
expect_doppel("plot-Samples-DALogisticLogNormal", actual)
