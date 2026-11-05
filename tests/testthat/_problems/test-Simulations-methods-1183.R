# Extracted from test-Simulations-methods.R:1183

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
sim_summary <- new(
    "SimulationsSummary",
    n_obs = as.integer(rep(
      c(3, 6, 9, 12, 15, 18, 21, 24, 27, 30, 33, 36, 39, 42, 45, 48, 51, 54),
      times = c(2, 3, 2, 4, 3, 6, 5, 13, 8, 8, 14, 10, 8, 3, 2, 5, 1, 1)
    )),
    dose_selected = rep(
      c(0.001, 0.01, 0.05, 0.15, 0.5, 1, 2, 4, 8, 16),
      times = c(2, 5, 9, 18, 24, 19, 12, 7, 3, 1)
    ),
    prop_dlts = c(
      rep(0, 4),
      rep(1 / 12, 10),
      rep(2 / 12, 17),
      rep(3 / 12, 24),
      rep(4 / 12, 20),
      rep(5 / 12, 14),
      rep(6 / 12, 8),
      rep(7 / 12, 3)
    ),
    n_above_target = as.integer(rep(c(0, 3, 6), times = c(91, 7, 2))),
    placebo = FALSE
  )
summary_dashboard <- plot(
    sim_summary,
    type = c("nObs", "doseSelected", "propDLTs", "nAboveTarget")
  )
expect_s3_class(summary_dashboard, "gtable")
expect_doppel("plot-simulations-summary-dashboard", summary_dashboard)
