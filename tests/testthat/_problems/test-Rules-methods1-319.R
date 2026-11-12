# Extracted from test-Rules-methods1.R:319

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "crmPack", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
skip_on_cran_but_not_ci()

# test -------------------------------------------------------------------------
set.seed(123)
data <- DataCombo(
    doseGrid = list(compound_a = c(10, 20, 30), compound_b = c(20, 40)),
    x = cbind(
      compound_a = c(10, 10, 10, 20, 20, 20),
      compound_b = c(20, 20, 20, 20, 20, 20)
    ),
    y = c(0L, 0L, 1L, 0L, 0L, 0L),
    ID = 1L:6L,
    cohort = c(1L, 1L, 1L, 2L, 2L, 2L)
  )
model <- TwoDrugsCombo(list(
    compound_a = .DefaultLogisticLogNormal(),
    compound_b = .DefaultLogisticLogNormal()
  ))
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
result <- recommend(
    nb_ncrm,
    maxDose(
      IncrementsComboCartesian(
        drug1 = IncrementsRelative(0, 0.5),
        drug2 = IncrementsRelative(0, 1)
      ),
      data
    ),
    samples,
    model,
    data
  )
