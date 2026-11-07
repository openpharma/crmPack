# Extracted from test-Rules-methods1.R:271

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "crmPack", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
skip_on_cran_but_not_ci()

# test -------------------------------------------------------------------------
set.seed(123)
data <- h_get_data_combo()
model <- h_get_two_drugs_combo()
samples <- mcmc(
    data,
    model,
    h_get_mcmc_options(samples = 10, burnin = 10)
  )
nb_ncrm <- NextBestNCRM(
    target = c(0.2, 0.35),
    overdose = c(0.35, 1),
    max_overdose_prob = 0.25
  )
increments <- IncrementsComboCartesian(
    drug1 = IncrementsRelative(0, 0.5),
    drug2 = IncrementsRelative(0, 1)
  )
doselimit <- maxDose(increments, data)
doselimit[3, 2] <- 20
expected <- nextBest(
    nb_ncrm,
    doselimit,
    samples,
    model,
    data
  )
actual <- recommend(
    nb_ncrm,
    doselimit,
    samples,
    model,
    data
  )
