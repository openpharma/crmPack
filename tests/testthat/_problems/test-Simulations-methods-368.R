# Extracted from test-Simulations-methods.R:368

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "crmPack", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
skip_on_cran_but_not_ci()
testthat::local_mocked_bindings(
  .DefaultSimulations = function(...) {
    readRDS(testthat::test_path("fixtures", "default_simulations.Rds"))
  }
)
testthat::local_mocked_bindings(
  .DefaultDualSimulations = function(...) {
    readRDS(testthat::test_path("fixtures", "default_dual_simulations.Rds"))
  }
)
testthat::local_mocked_bindings(
  .DefaultPseudoSimulations = function(...) {
    readRDS(testthat::test_path("fixtures", "default_pseudo_simulations.Rds"))
  }
)
testthat::local_mocked_bindings(
  .DefaultPseudoDualSimulations = function(...) {
    readRDS(testthat::test_path(
      "fixtures",
      "default_pseudo_dual_simulations.Rds"
    ))
  }
)
testthat::local_mocked_bindings(
  .DefaultPseudoDualFlexiSimulations = function(...) {
    readRDS(testthat::test_path(
      "fixtures",
      "default_pseudo_dual_flexi_simulations.Rds"
    ))
  }
)

# test -------------------------------------------------------------------------
skip_on_cran()
design <- .DefaultDesignCombo()
design@stopping <- StoppingMinPatients(nPatients = 60)
design@nextBest <- NextBestNCRM(
    target = c(0.1, 0.6),
    overdose = c(0.99, 1),
    max_overdose_prob = 0.999
  )
true_tox_combo <- function(dose) {
    plogis(-7 + 0.05 * dose[1] + 0.04 * dose[2] + 0.0005 * dose[1] * dose[2])
  }
sims <- simulate(
    design,
    truth = true_tox_combo,
    nsim = 20,
    seed = 819,
    mcmcOptions = h_get_mcmc_options(samples = 8),
    parallel = FALSE
  )
n_unique_combos <- sapply(sims@data, function(d) nrow(unique(d@x)))
expect_true(any(n_unique_combos > 1L))
result_2d <- plot(sims, type = "trajectory2D")
expect_s3_class(result_2d, "ggplot")
expect_doppel("plot_comboSims_trajectory2D_20sims", result_2d)
