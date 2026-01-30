#' @include "logger.R"

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
  .DefaultDualSimulations = function(...) {
    readRDS(testthat::test_path(
      "fixtures",
      "default_dual_simulations.Rds"
    ))
  }
)
# End of mocks

test_that("tidy methods exist for all relevant classes", {
  crmpack_class_list <- getClasses(asNamespace("crmPack"))
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
            expect_equal(class(result)[1], paste0("tbl_", cls))
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
})

# Related: https://github.com/openpharma/crmPack/issues/759
test_that("tidy methods return non-empty value for all classes", {
  crmpack_class_list <- getClasses(asNamespace("crmPack"))
  # The default constructors of the following classes correctly return a list
  # with some elements of length zero
  some_elements_length_zero <- c("RuleDesign")
  exclusions <- c(
    "Backfill",
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
})
