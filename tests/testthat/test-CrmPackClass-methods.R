test_that("tidy methods exist for all relevant classes", {
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
        tryCatch({
          x <- do.call(paste0(".Default", cls), list())
          result <- x |> tidy()
          expect_class(result, c(paste0("tbl_", cls), "tbl_df", "tbl", "data.frame"))
        },
        error = function(e) fail(paste0("Unable to tidy ", cls, " objects:", geterrmessage()))
        )
      } else {
        print(paste0("No default constructor for ", cls))
      }
    }
  }
})
