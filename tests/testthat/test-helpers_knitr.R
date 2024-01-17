library(knitr)

test_that("knit_print methods exist for all relevant classes", {
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

  # See https://stackoverflow.com/questions/42738851/r-how-to-find-what-s3-method-will-be-called-on-an-object
  findMethod <- function(generic, ...) {
    ch <- deparse(substitute(generic))
    f <- X <- function(x, ...) UseMethod("X")
    for(m in methods(ch)) assign(sub(ch, "X", m, fixed = TRUE), "body<-"(f, value = m))
    X(...)
  }

  for (cls in crmpack_class_list) {
    if (!isClassUnion(cls)) {
      constructor_name <- paste0(".Default", cls)
      if (exists(constructor_name, mode = "function")) {
        tryCatch(
          {
            x <- do.call(paste0(".Default", cls), list())

            result <- findMethod(knit_print, x)
            # TODO Remove if (...) { warning() } else { once all methods have been implemented
            if (result == "knit_print.default") {
              warning(paste0("No knit_print method for ", cls, " objects"))
            } else {
              expect_equal(result, paste0("knit_print.", cls))
            }
          },
          error = function(e) fail(paste0("Error locating knit_print method for ", cls, " objects: ", e))
        )
      } else {
        warning(paste0("No default constructor for ", cls))
      }
    }
  }
})

#  CohortSize ---

#  CohortSizeConst

test_that("knit_print.CohortSizeConst works correctly", {
  x <- CohortSizeConst(3)
  rv <-  knit_print(x)
  expect_equal(rv, "A constant size of 3 participants", ignore_attr = TRUE)
  expect_class(rv, "knit_asis")

  x <- CohortSizeConst(2)
  rv <-  knit_print(x, label = "subject")
  expect_equal(rv, "A constant size of 2 subjects", ignore_attr = TRUE)

  x <- CohortSizeConst(1)
  rv <-  knit_print(x, label = "subject")
  expect_equal(rv, "A constant size of 1 subject", ignore_attr = TRUE)

  x <- CohortSizeConst(3)
  rv <- knit_print(x, asis = FALSE)
  expect_equal(rv, "A constant size of 3 participants")
  expect_class(rv, "character")
})

# CohortSizeRange

test_that("knit_print.CohortSizeConst works correctly", {
})

