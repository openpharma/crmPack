# Extracted from test-Simulations-methods.R:1107

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
    dose_selected = rep(
      c(1, 3, 5, 10, 15, 20, 25),
      times = c(2, 8, 20, 35, 22, 10, 3)
    ),
    placebo = FALSE
  )
mtd_plot <- plot(sim_summary, type = "doseSelected")
expect_true(mtd_plot$scales$get_scales("x")$is_discrete())
expect_equal(mtd_plot$theme$axis.text.x$angle, 45)
expect_doppel(
    "plot-simulations-summary-dose-selected-representative",
    mtd_plot
  )
