# Extracted from test-Simulations-methods.R:1832

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
pseudo_dual_sims <- .DefaultPseudoDualSimulations()
pseudo_dual_summary <- summary(
    pseudo_dual_sims,
    trueDLE = function(x) plogis(-3 + 0.05 * x),
    trueEff = function(x) 0.2 + 0.004 * x
  )
expect_error(
    plot(pseudo_dual_summary, type = "invalid_type"),
    "should be one of"
  )
expect_error(
    plot(pseudo_dual_summary, type = character(0)),
    "must be of length"
  )
result_single <- plot(pseudo_dual_summary, type = "meanEffFit")
expect_s3_class(result_single, "ggplot")
expect_doppel("plot_pseudoDualSimsSummary_meanEffFit_single", result_single)
