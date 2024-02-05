library(knitr)
# if (!testthat::is_checking()) {
#   devtools::load_all()
# }

# h_custom_method_exists could be removed once all necessary knit_print methods
# have been defined

#' Test if a Class-Specific S3 Generic Exists
#'
#' @param generic (`name`)\cr The unquoted name of the generic, which must exist
#' @param obj (`ANY`)\cr An S3 or S4 object
#' @return TRUE if S3 method `<method_name>.<class_name>` exists, FALSE otherwise
#' @example h_custom_method_exists(knit_print, "CohortSizeConst") # TRUE
#' @example h_custom_method_exists(knot_print, "Validate") # FALSE
#' @internal
h_custom_method_exists <- function(generic, obj) {
  # See https://stackoverflow.com/questions/42738851/r-how-to-find-what-s3-method-will-be-called-on-an-object
  generic_name <- deparse(substitute(generic))
  f <- X <- function(x, obj) UseMethod("X")
  for (m in methods(generic_name)) assign(sub(generic_name, "X", m, fixed = TRUE), "body<-"(f, value = m))
  method_name <- X(obj)
  return(method_name != paste0(generic_name, ".default"))
}

test_that("h_custom_method_exists works correctly", {
  withr::with_environment(
    .GlobalEnv,
    {
      foo <<- function(x, ...) UseMethod("foo")
      bar <<- NA
      class(bar) <- "bar"
      baz <<- NA
      class(baz) <- "baz"
      foo.default <<- function(x, ...) "I don't know what to do"
      foo.bar <<- function(x, ...) "I am bar"
      withr::defer(rm(foo, bar, baz, foo.default, foo.bar, envir = .GlobalEnv))

      expect_true(h_custom_method_exists(foo, bar))
      expect_false(h_custom_method_exists(foo, baz))
    }
  )
})

test_that("Global environment is clean after testing h_custom_method_exists", {
  expect_false(exists("foo"))
  expect_false(exists("bar"))
  expect_false(exists("baz"))
  expect_false(exists("foo.bar"))
  expect_false(exists("foo.default"))
})

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

test_that("knit_print methods exist for all relevant classes and produce consistent output", {
  for (cls in crmpack_class_list) {
    if (!isClassUnion(cls)) {
      # If the default knit_print method has been overridden, test it
      if (h_custom_method_exists(knit_print, do.call(paste0(".Default", cls), list()))) {
        outFileName <- paste0("knit_print_", cls, ".html")
        # with_file guarantees that the test file will be deleted automatically
        # once the snapshot has been compared with the previous version, which
        # can be found in /_snaps/helpers_knitr
        withr::with_file(
          test_path("fixtures", outFileName),
          {
            # Code run in the template does not contribute to test coverage
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

test_that("asis parameter works correctly for all implemented methods", {
  for (cls in crmpack_class_list) {
    if (!isClassUnion(cls)) {
      obj <- do.call(paste0(".Default", cls), list())
      # If the default knit_print method has been overridden, test it
      if (h_custom_method_exists(knit_print, obj)) {
        rv <- knit_print(obj)
        expect_class(rv, "knit_asis")
        rv <- knit_print(obj, asis = TRUE)
        expect_class(rv, "knit_asis")
        rv <- knit_print(obj, asis = FALSE)
        # Most objects return a character, but not all.  For example,
        # CohortSizeDLT returns a knitr_table
        if ("knit_asis" %in% class(rv)) print(cls)
        expect_true(!("knit_asis" %in% class(rv)))
      }
    }
  }
})

#  CohortSize ---

#  CohortSizeConst

test_that("knit_print.CohortSizeConst works correctly", {
  x <- CohortSizeConst(3)
  rv <- knit_print(x)
  expect_equal(rv, "A constant size of 3 participants.", ignore_attr = TRUE)

  x <- CohortSizeConst(2)
  rv <- knit_print(x, label = "subject")
  expect_equal(rv, "A constant size of 2 subjects.", ignore_attr = TRUE)

  x <- CohortSizeConst(1)
  rv <- knit_print(x, label = "subject")
  expect_equal(rv, "A constant size of 1 subject.", ignore_attr = TRUE)

  x <- CohortSizeConst(3)
  rv <- knit_print(x, asis = FALSE)
  expect_equal(rv, "A constant size of 3 participants.")
})

#  CohortSizeParts

test_that("knit_print.CohortSizeParts works correctly", {
  x <- CohortSizeParts(c(1, 3))
  rv <- knit_print(x)
  expect_equal(rv, "A size of 1 participant in the first part and 3 participants in the second.", ignore_attr = TRUE)

  x <- CohortSizeParts(c(1, 3))
  rv <- knit_print(x, label = "subject")
  expect_equal(rv, "A size of 1 subject in the first part and 3 subjects in the second.", ignore_attr = TRUE)

  x <- CohortSizeParts(c(1, 3))
  rv <- knit_print(x, label = "subject")
  expect_equal(rv, "A size of 1 subject in the first part and 3 subjects in the second.", ignore_attr = TRUE)

  x <- CohortSizeParts(c(1, 3))
  rv <- knit_print(x, asis = FALSE)
  expect_equal(rv, "A size of 1 participant in the first part and 3 participants in the second.")
})
