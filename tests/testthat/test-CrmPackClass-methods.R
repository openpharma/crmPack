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
  crmpack_class_list <- setdiff(crmPack_class_list, exclusions)

  for (cls in crmpack_class_list) {
    if (!isClassUnion(cls)) {
      constructor_name <- paste0(".Default", cls)
      if (exists(constructor_name, mode = "function")) {
        tryCatch({
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
