# Extracted from test-logger.R:38

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "crmPack", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
is_current_verbose <- is_logging_enabled()
disable_logging()
expect_true(h_test_logging_disabled())
enable_logging()
expect_true(h_test_logging_enabled())
