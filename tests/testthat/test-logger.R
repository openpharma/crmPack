# enable_logging ----

test_that("enable_logging works as expected", {
  is_current_verbose <- is_logging_enabled()

  disable_logging()
  enable_logging()
  expect_true(h_test_logging_enabled())

  # Restore settings.
  if (!is_current_verbose) {
    disable_logging()
  }
})

# disable_logging ----

test_that("disable_logging works as expected", {
  is_current_verbose <- is_logging_enabled()

  disable_logging()
  expect_true(h_test_logging_disabled())

  # Restore settings.
  if (is_current_verbose) {
    enable_logging()
  }
})

# is_logging_enabled ----

test_that("is_logging_enabled works as expected", {
  is_current_verbose <- is_logging_enabled()

  disable_logging()
  expect_true(h_test_logging_disabled())
  enable_logging()
  expect_true(h_test_logging_enabled())

  # Restore settings.
  if (!is_current_verbose) {
    disable_logging()
  }
})

# log_trace ----

test_that("log_trace works as expected", {
  is_current_verbose <- is_logging_enabled()

  enable_logging()
  expect_true(h_test_logging_enabled())

  # Restore settings.
  if (!is_current_verbose) {
    disable_logging()
  }
})
