# Extracted from test-helpers_knitr.R:178

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "crmPack", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
skip_on_cran_but_not_ci()
library(knitr)
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
h_custom_method_exists <- function(generic, obj) {
  # See https://stackoverflow.com/questions/42738851/r-how-to-find-what-s3-method-will-be-called-on-an-object
  generic_name <- deparse(substitute(generic))
  f <- X <- function(x, obj) UseMethod("X")
  for (m in methods(generic_name)) {
    assign(sub(generic_name, "X", m, fixed = TRUE), "body<-"(f, value = m))
  }
  method_name <- X(obj)
  (method_name != paste0(generic_name, ".default"))
}
crmpack_class_list <- getClasses(asNamespace("crmPack"))
exclusions <- c(
  "ArmCondition",
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

# test -------------------------------------------------------------------------
skip_on_cran()
for (cls in crmpack_class_list) {
    if (!isClassUnion(cls)) {
      startTime <- Sys.time()

      obj <- do.call(paste0(".Default", cls), list())

      endTime <- Sys.time()
      if (unclass(endTime - startTime) > 2) {
        print(paste0("Long initialisation for ", cls))
      }

      # If the default knit_print method has been overridden, test it
      if (h_custom_method_exists(knit_print, obj)) {
        # Default behaviour
        rv <- knit_print(obj)
        if (is.null(rv)) {
          print(paste0("knit_print(obj) returns NULL for class ", cls, "."))
        }
        expect_class(rv, "knit_asis")

        # Explicit behaviours
        rv <- knit_print(obj, asis = TRUE)
        if (is.null(rv)) {
          print(paste0(
            "knit_print(obj, asis = TRUE) returns NULL for class ",
            cls,
            "."
          ))
        }
        expect_class(rv, "knit_asis")
        rv <- knit_print(obj, asis = FALSE)
        if (is.null(rv)) {
          print(paste0(
            "knit_print(obj, asis = FALSE) returns NULL for class ",
            cls,
            "."
          ))
        }
        # Most objects return a character, but not all.  For example,
        # CohortSizeDLT returns a knitr_table
        if ("knit_asis" %in% class(rv)) {
          print(cls)
        }
        expect_true(!("knit_asis" %in% class(rv)))

        # Invalid value
        errorThrown <- FALSE
        tryCatch(
          {
            knit_print(obj, asis = "badValue")
          },
          error = function(e) errorThrown <<- TRUE
        )
        if (!errorThrown) {
          print(paste0("No error thrown for ", cls, "."))
        }
        expect_error(knit_print(obj, asis = "badValue"))
      }
    }
  }
