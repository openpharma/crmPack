pkg_name <- "crmPack"
if (requireNamespace("testthat", quietly = TRUE)) {
  library(testthat)
  if (testthat:::on_ci()) {
    reporter <- MultiReporter$new(list(
      CheckReporter$new(),
      JunitReporter$new(file = "junit-result.xml")
    ))
    test_results <- test_check(pkg_name, reporter = reporter)
    saveRDS(test_results, "unit_testing_results.rds")
  } else {
    test_check(pkg_name)
  }
}
