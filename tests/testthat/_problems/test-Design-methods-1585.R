# Extracted from test-Design-methods.R:1585

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "crmPack", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
skip_on_cran_but_not_ci()
options(testthat.progress.max_fails = 0)

# test -------------------------------------------------------------------------
mono_arm <- Design(
    model = .LogisticNormal(),
    nextBest = NextBestNCRM(
      target = c(0.3, 0.6),
      overdose = c(0.6, 1),
      max_overdose_prob = 0.7
    ),
    # With a custom label that we can check below.
    stopping = StoppingMinPatients(nPatients = 20, report_label = "my label"),
    increments = IncrementsDoseLevels(levels = 5),
    cohort_size = CohortSizeConst(3),
    data = Data(doseGrid = 1:100),
    startingDose = 10
  )
combo_arm <- .Design(
    mono_arm,
    # Such that we stop after the first cohort.
    stopping = StoppingMinPatients(nPatients = 1)
  )
object <- DesignGrouped(
    model = LogisticLogNormalGrouped(
      mean = rep(-1, 4),
      cov = diag(5, 4),
      ref_dose = 1
    ),
    mono = mono_arm,
    combo = combo_arm,
    same_dose_for_all = FALSE,
    first_cohort_mono_only = FALSE,
    stop_mono_with_combo = TRUE
  )
my_truth <- function(x) plogis(-4 + 0.2 * log(x / 0.1))
my_combo_truth <- function(x) plogis(-4 + 0.5 * log(x / 0.1))
result <- expect_silent(simulate(
    object,
    nsim = 2,
    seed = 123,
    truth = my_truth,
    combo_truth = my_combo_truth,
    mcmcOptions = h_get_mcmc_options()
  ))
