# Extracted from test-Simulations-methods.R:1304

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
mySims <- .DefaultDualSimulations()
myTruthTox <- function(dose) pnorm((dose - 60) / 10)
myTruthBio <- function(dose) {
    pmax(0.1, pmin(0.95, 0.2 + 0.6 * (dose / 100)^0.5))
  }
simSummary <- summary(
    mySims,
    trueTox = myTruthTox,
    trueBiomarker = myTruthBio
  )
result_mean_bio_fit <- plot(simSummary, type = "meanBiomarkerFit")
expect_s3_class(result_mean_bio_fit, "ggplot")
expect_doppel("plot_dualSimsSummary_meanBiomarkerFit", result_mean_bio_fit)
