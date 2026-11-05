# Extracted from test-helpers_knitr.R:164

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
      # If the default knit_print method has been overridden, test it
      if (
        h_custom_method_exists(
          knit_print,
          do.call(paste0(".Default", cls), list())
        )
      ) {
        outFileName <- paste0("knit_print_", cls, ".html")
        # with_file guarantees that the test file will be deleted automatically
        # once the snapshot has been compared with the previous version, which
        # can be found in /_snaps/<Platform>/helpers_knitr
        withr::with_file(
          test_path("fixtures", outFileName),
          {
            # Code run in the template does not contribute to test coverage
            tryCatch(
              {
                rmarkdown::render(
                  input = test_path("fixtures", "knit_print_template.Rmd"),
                  params = list("class_name" = cls),
                  output_file = outFileName,
                  output_dir = test_path("fixtures"),
                  quiet = TRUE
                )
                expect_snap_file(test_path("fixtures", outFileName))
              },
              error = function(e) {
                ignored_errors <- c(
                  "RStudio not running",
                  "This {rstudioapi} function is not currently implemented for VSCode."
                )
                if (!any(sapply(ignored_errors, grepl, e$message, fixed = TRUE))) {
                  warning(
                    paste0("Error for class ", cls, ": "),
                    geterrmessage()
                  )
                }
              }
            )
          }
        )
      }
    } else {
      warning(paste0("No default constructor for ", cls))
    }
  }
