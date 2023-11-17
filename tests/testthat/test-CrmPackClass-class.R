# nolint start

test_that("CrmPackClass correctly identifies crmPack classes", {
  crmPack_class_list <- getClasses(asNamespace("crmPack"))
  exclusions <- c(
    "CohortSize", "CrmPackClass", "DualEndpoint", "GeneralData", "GeneralModel",
    "GeneralSimulationsSummary", "Increments", "ModelEff", "ModelPseudo",
    "ModelTox", "NextBest", "positive_number", "PseudoSimulations",
    "PseudoDualSimulations", "PseudoDualSimulationsSummary",
    "PseudoDualFlexiSimulations", "PseudoFlexiSimulations",
    "PseudoSimulationsSummary", "SimulationsSummary", "Report", "SafetyWindow",
    "Stopping", "Validate"
  )
  crmPack_class_list <- setdiff(crmPack_class_list, exclusions)

  for (cls in crmPack_class_list) {
    if (!isClassUnion(cls)) {
      constructor_name <- paste0(".Default", cls)
      if (exists(constructor_name, mode = "function")) {
        expect_true(is(do.call(paste0(".Default", cls), list()), "CrmPackClass"))
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
    "CohortSize", "CrmPackClass", "DualEndpoint", "GeneralData", "GeneralModel",
    "GeneralSimulationsSummary", "Increments", "ModelEff", "ModelPseudo",
    "ModelTox", "NextBest", "positive_number", "PseudoSimulations",
    "PseudoDualSimulations", "PseudoDualSimulationsSummary",
    "PseudoDualFlexiSimulations", "PseudoFlexiSimulations",
    "PseudoSimulationsSummary", "SimulationsSummary", "Report", "SafetyWindow",
    "Stopping", "Validate"
  )
  for (cls in exception_class_list) {
    constructor_name <- paste0(".Default", cls)
    expect_error(do.call(paste0(".Default", !!cls), list()))
  }
})

# nolint end
