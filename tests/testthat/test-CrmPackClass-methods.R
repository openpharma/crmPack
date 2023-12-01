#' @include "logger.R"

test_that("tidy methods exist for all relevant classes", {
  crmpack_class_list <- getClasses(asNamespace("crmPack"))
  exclusions <- c(
    "CohortSize", "CrmPackClass", "DualEndpoint", "GeneralData", "GeneralModel",
    "GeneralSimulationsSummary", "Increments", "ModelEff", "ModelPseudo",
    "ModelTox", "NextBest", "positive_number", "PseudoSimulations",
    "PseudoDualSimulations", "PseudoDualSimulationsSummary",
    "PseudoDualFlexiSimulations", "PseudoFlexiSimulations",
    "PseudoSimulationsSummary", "SimulationsSummary", "Report", "SafetyWindow",
    "Stopping", "Validate",
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
            result <- x |> tidy()
            expect_equal(class(result)[1], paste0("tbl_", cls))
          },
          error = function(e) fail(paste0("Unable to tidy ", cls, " objects: ", e))
        )
      } else {
        print(paste0("No default constructor for ", cls))
      }
    }
  }
})

# Related: https://github.com/Roche/crmPack/issues/759
test_that("tidy methods return non-empty value for all classes", {
  crmpack_class_list <- getClasses(asNamespace("crmPack"))
  # The default constructors of the following classes correctly return a list
  # with some elements of length zero
  some_elements_length_zero <- c("RuleDesign")
  exclusions <- c(
    "CohortSize", "CrmPackClass", "DualEndpoint", "GeneralData", "GeneralModel",
    "GeneralSimulationsSummary", "Increments", "ModelEff", "ModelPseudo",
    "ModelTox", "NextBest", "positive_number", "PseudoSimulations",
    "PseudoDualSimulations", "PseudoDualSimulationsSummary",
    "PseudoDualFlexiSimulations", "PseudoFlexiSimulations",
    "PseudoSimulationsSummary", "SimulationsSummary", "Report", "SafetyWindow",
    "Stopping", "Validate",
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
            result <- x |> tidy()
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
          error = function(e) fail(paste0("Unable to tidy ", cls, " objects: ", e))
        )
      } else {
        print(paste0("No default constructor for ", cls))
      }
    }
  }
})

