# nolint start

# This code mocks functions that have long execution times so that unit tests
# complete more quickly.  Initial tests suggest that the mocks need to be defined
# in the file in which the tests are executed.  `source`ing the mocks does not
# work.
#
# The persistent objects that are loaded are created by
# /testthat/fixtures/make_persistent_objects_for_mocked_constructors.R.
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
  .DefaultDualSimulationsSummary = function(...) {
    readRDS(testthat::test_path(
      "fixtures",
      "default_dual_simulations_summary.Rds"
    ))
  }
)
# End of mocks

test_that("CrmPackClass correctly identifies crmPack classes", {
  crmPack_class_list <- getClasses(asNamespace("crmPack"))
  exclusions <- c(
    "CohortSize",
    "CrmPackClass",
    "DualEndpoint",
    "GeneralData",
    "GeneralModel",
    "GeneralSimulationsSummary",
    "Increments",
    "ModelEff",
    "ModelPseudo",
    "ModelTox",
    "NextBest",
    "positive_number",
    "PseudoSimulations",
    "PseudoDualSimulations",
    "PseudoDualSimulationsSummary",
    "PseudoDualFlexiSimulations",
    "PseudoFlexiSimulations",
    "PseudoSimulationsSummary",
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
})

test_that("CrmPackClass does not identify random non-crmPack classes", {
  non_crmPack_object_list <- list(
    tibble::tibble(),
    glm(mpg ~ wt + gear, data = mtcars)
  )

  for (obj in non_crmPack_object_list) {
    expect_false(is(obj, "CrmPackClass"))
  }
})

test_that("virtual CrmPackClass classes throw expected error when default constructor called", {
  exception_class_list <- c(
    "CohortSize",
    "CrmPackClass",
    "DualEndpoint",
    "GeneralData",
    "GeneralModel",
    "GeneralSimulationsSummary",
    "Increments",
    "ModelEff",
    "ModelPseudo",
    "ModelTox",
    "NextBest",
    "positive_number",
    "PseudoSimulations",
    "PseudoDualSimulations",
    "PseudoDualSimulationsSummary",
    "PseudoDualFlexiSimulations",
    "PseudoFlexiSimulations",
    "PseudoSimulationsSummary",
    "SimulationsSummary",
    "Report",
    "SafetyWindow",
    "Stopping",
    "Validate"
  )
  for (cls in exception_class_list) {
    constructor_name <- paste0(".Default", cls)
    expect_error(do.call(paste0(".Default", !!cls), list()))
  }
})

# nolint end
