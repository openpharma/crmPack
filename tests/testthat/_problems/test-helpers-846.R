# Extracted from test-helpers.R:846

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "crmPack", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
result <- gridExtra::arrangeGrob(grid::rectGrob(), grid::rectGrob())
assert_class(result, "gtable")
expect_doppel("plot-gtable", plot(result))
