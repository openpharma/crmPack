# Extracted from test-Simulations-methods.R:1149

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
pseudo_summary <- new(
    "PseudoSimulationsSummary",
    prop_dle = rep(
      c(0, 1 / 8, 2 / 8, 3 / 8, 4 / 8, 5 / 8, 6 / 8),
      times = c(2, 10, 24, 30, 20, 10, 4)
    )
  )
dle_plot <- plot(pseudo_summary, type = "propDLE")
expect_s3_class(dle_plot$layers[[1L]]$stat, "StatBin")
expect_doppel(
    "plot-pseudo-simulations-summary-prop-dle-representative",
    dle_plot
  )
