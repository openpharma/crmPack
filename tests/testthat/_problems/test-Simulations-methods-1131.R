# Extracted from test-Simulations-methods.R:1131

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
    prop_dlts = rep(
      c(0, 1 / 12, 2 / 12, 3 / 12, 4 / 12, 5 / 12),
      times = c(3, 16, 28, 25, 18, 10)
    ),
    placebo = FALSE
  )
dlt_plot <- plot(sim_summary, type = "propDLTs")
expect_s3_class(dlt_plot$layers[[1L]]$stat, "StatBin")
expect_doppel("plot-simulations-summary-prop-dlts-representative", dlt_plot)
