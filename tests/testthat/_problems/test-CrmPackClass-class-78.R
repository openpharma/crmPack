# Extracted from test-CrmPackClass-class.R:78

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "crmPack", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
testthat::local_mocked_bindings(
  .DefaultDASimulations = function(...) {
    readRDS(testthat::test_path("fixtures", "default_da_simulations.Rds"))
  }
)
testthat::local_mocked_bindings(
  .DefaultSimulations = function(...) {
    readRDS(testthat::test_path("fixtures", "default_simulations.Rds"))
  }
)
testthat::local_mocked_bindings(
  .DefaultDualSimulations = function(...) {
    readRDS(testthat::test_path(
      "fixtures",
      "default_dual_simulations.Rds"
    ))
  }
)

# test -------------------------------------------------------------------------
skip_on_cran()
crmPack_class_list <- getClasses(asNamespace("crmPack"))
exclusions <- c(
    "ArmCondition",
    "CohortSize",
    "ComboSimulationsSummary",
    "CrmPackClass",
    "DualEndpoint",
    "DualSimulationsSummary",
    "GeneralData",
    "GeneralModel",
    "GeneralSimulationsSummary",
    "HierarchicalSimulationsSummary",
    "Increments",
    "ModelEff",
    "ModelPseudo",
    "ModelTox",
    "NextBest",
    "Opening",
    "positive_number",
    "PseudoSimulations",
    "PseudoDualSimulations",
    "PseudoDualSimulationsSummary",
    "PseudoDualFlexiSimulations",
    "PseudoFlexiSimulations",
    "PseudoSimulationsSummary",
    "Recruitment",
    "SimulationsSummary",
    "Report",
    "SafetyWindow",
    "Stopping",
    "Validate"
  )
crmPack_class_list <- setdiff(crmPack_class_list, exclusions)
for (cls in crmPack_class_list) {
    if (!isClassUnion(cls)) {
      constructor_name <- paste0(".Default", cls)
      if (exists(constructor_name, mode = "function")) {
        expect_true(is(
          do.call(paste0(".Default", cls), list()),
          "CrmPackClass"
        ))
      } else {
        fail(paste0("No default constructor for ", cls))
      }
    }
  }
