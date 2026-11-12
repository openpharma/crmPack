# Extracted from test-Design-methods.R:1853

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "crmPack", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
skip_on_cran_but_not_ci()
options(testthat.progress.max_fails = 0)

# test -------------------------------------------------------------------------
skip_on_cran()
object <- DesignGrouped(
    model = LogisticLogNormalGrouped(
      mean = rep(-1, 4),
      cov = diag(5, 4),
      ref_dose = 1
    ),
    mono = Design(
      model = .LogisticNormal(),
      nextBest = NextBestNCRM(
        target = c(0.3, 0.6),
        overdose = c(0.6, 1),
        max_overdose_prob = 0.7
      ),
      stopping = StoppingMinPatients(nPatients = 9),
      increments = IncrementsDoseLevels(levels = 5),
      # Make sure that cohort size changes across doses.
      cohort_size = CohortSizeRange(
        intervals = c(0, 5, 10),
        cohort_size = c(1, 2, 3)
      ),
      data = Data(doseGrid = 1:100),
      startingDose = 1
    ),
    same_dose_for_all = TRUE,
    first_cohort_mono_only = TRUE
  )
my_truth <- stats::stepfun(x = c(0, 5, 10), y = c(0, 0, 0.5, 0.99))
my_combo_truth <- my_truth
result <- expect_silent(simulate(
    object,
    nsim = 2,
    seed = 123,
    truth = my_truth,
    combo_truth = my_combo_truth,
    mcmcOptions = h_get_mcmc_options()
  ))
