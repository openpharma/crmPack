# Extracted from test-Simulations-methods.R:67

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
mySims <- .DefaultSimulations()
result <- plot(mySims)
expect_s3_class(result, "gtable")
expect_equal(dim(result), c(2, 1))
expect_true(all(c("heights", "widths", "grobs") %in% names(result)))
result_trajectory <- plot(mySims, type = "trajectory")
expect_s3_class(result_trajectory, "ggplot")
expect_equal(result_trajectory$labels$x, "Patient")
expect_equal(result_trajectory$labels$y, "Dose Level")
expect_equal(
    result_trajectory$scales$get_scales("y")$breaks,
    mySims@data[[1L]]@doseGrid
  )
expect_doppel("plot_generalSims_trajectory", result_trajectory)
