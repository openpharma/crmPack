# Extracted from test-CrmPackClass-methods.R:153

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
crmpack_class_list <- getClasses(asNamespace("crmPack"))
some_elements_length_zero <- c("RuleDesign")
exclusions <- c(
    "ArmCondition",
    "Backfill",
    "CohortSize",
    "ComboSimulationsSummary",
    "CrmPackClass",
    "DualEndpoint",
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
    "Validate",
    # The following classes have no constructors
    "DualSimulationsSummary"
  )
crmpack_class_list <- setdiff(crmpack_class_list, exclusions)
for (cls in crmpack_class_list) {
    if (!isClassUnion(cls)) {
      constructor_name <- paste0(".Default", cls)
      if (exists(constructor_name, mode = "function")) {
        tryCatch(
          {
            x <- do.call(paste0(".Default", cls), list())
            result <- x %>% tidy()
            if (is.list(result)) {
              if (cls %in% some_elements_length_zero) {
                expect_true(length(result) > 0)
              } else {
                expect_true(all(sapply(result, function(x) length(x) > 0)))
              }
            } else {
              expect_true(length(result) > 0)
            }
          },
          error = function(e) {
            fail(paste0("Unable to tidy ", cls, " objects: ", e))
          }
        )
      } else {
        print(paste0("No default constructor for ", cls))
      }
    }
  }
