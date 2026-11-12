# Extracted from test-Rules-methods.R:1544

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "crmPack", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
skip_on_cran_but_not_ci()

# test -------------------------------------------------------------------------
ordinal_data <- .DefaultDataOrdinal()
ordinal_model <- .DefaultLogisticLogNormalOrdinal()
samples <- mcmc(
    ordinal_data,
    ordinal_model,
    McmcOptions(
      rng_kind = "Mersenne-Twister",
      rng_seed = 215614
    )
  )
next_best <- .DefaultNextBestOrdinal()
bad_data <- .DefaultData()
actual <- nextBest(next_best, Inf, samples, ordinal_model, ordinal_data)
expect_doppel("nextBest-NextBestOrdinal", actual$plot)
