library(knitr)
if (!testthat::is_checking()) {
  devtools::load_all()
}

test_that("knit_print methods exist for all relevant classes and produce consistent output", {
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
  identifyMethod <- function(generic, ...) {
    ch <- deparse(substitute(generic))
    f <- X <- function(x, ...) UseMethod("X")
    for (m in methods(ch)) assign(sub(ch, "X", m, fixed = TRUE), "body<-"(f, value = m))
    X(...)
  }

  for (cls in crmpack_class_list) {
    if (!isClassUnion(cls)) {
      # Obtain the corresponding knit_print method...
      methodName <- identifyMethod(
        knit_print,
        do.call(paste0(".Default", cls), list())
      )
      # ... and if the default has been overridden, test it
      if (methodName != "knit_print.default") {
        outFileName <- paste0("knit_print_", cls, ".html")
        # with_file guarantees that the test file will be deleted automatically
        # once the snapshot has been compared with the previous version, which
        # can be found in /_snaps/helpers_knitr
        withr::with_file(
          outFileName,
          {
            rmarkdown::render(
              input = test_path("fixtures", "knit_print_template.Rmd"),
              params = list("class_name" = cls),
              output_file = outFileName,
              output_dir = test_path("fixtures")
            )
            expect_snapshot_file(test_path("fixtures", outFileName))
          }
        )
      }
    } else {
      warning(paste0("No default constructor for ", cls))
    }
  }
})

#  CohortSize ---

#  CohortSizeConst

test_that("knit_print.CohortSizeConst works correctly", {
  x <- CohortSizeConst(3)
  rv <- knit_print(x)
  expect_equal(rv, "A constant size of 3 participants.", ignore_attr = TRUE)
  expect_class(rv, "knit_asis")

  x <- CohortSizeConst(2)
  rv <- knit_print(x, label = "subject")
  expect_equal(rv, "A constant size of 2 subjects.", ignore_attr = TRUE)

  x <- CohortSizeConst(1)
  rv <- knit_print(x, label = "subject")
  expect_equal(rv, "A constant size of 1 subject.", ignore_attr = TRUE)

  x <- CohortSizeConst(3)
  rv <- knit_print(x, asis = FALSE)
  expect_equal(rv, "A constant size of 3 participants.")
  expect_class(rv, "character")
})

#  CohortSizeParts

test_that("knit_print.CohortSizeParts works correctly", {
  x <- CohortSizeParts(c(1, 3))
  rv <- knit_print(x)
  expect_equal(rv, "A size of 1 participant in the first part and 3 participants in the second.", ignore_attr = TRUE)
  expect_class(rv, "knit_asis")

  x <- CohortSizeParts(c(1, 3))
  rv <- knit_print(x, label = "subject")
  expect_equal(rv, "A size of 1 subject in the first part and 3 subjects in the second.", ignore_attr = TRUE)

  x <- CohortSizeParts(c(1, 3))
  rv <- knit_print(x, label = "subject")
  expect_equal(rv, "A size of 1 subject in the first part and 3 subjects in the second.", ignore_attr = TRUE)

  x <- CohortSizeParts(c(1, 3))
  rv <- knit_print(x, asis = FALSE)
  expect_equal(rv, "A size of 1 participant in the first part and 3 participants in the second.")
  expect_class(rv, "character")
})

