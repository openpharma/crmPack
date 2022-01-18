# verbose_logging ----

test_that("verbose_logging works as expected", {
  is_current_verbose <- is_logging_verbose()

  disable_logging()
  verbose_logging()
  expect_true(h_test_verbose())

  # Restore settings.
  if (!is_current_verbose) {
    disable_logging()
  }
})

# disable_logging ----

test_that("disable_logging works as expected", {
  is_current_verbose <- is_logging_verbose()

  disable_logging()
  expect_true(h_test_no_verbose())

  # Restore settings.
  if (is_current_verbose) {
    verbose_logging()
  }
})

# is_logging_verbose ----

test_that("is_logging_verbose works as expected", {
  is_current_verbose <- is_logging_verbose()

  disable_logging()
  expect_true(h_test_no_verbose())
  verbose_logging()
  expect_true(h_test_verbose())

  # Restore settings.
  if (!is_current_verbose) {
    disable_logging()
  }
})

# log_trace ----

test_that("log_trace works as expected", {
  is_current_verbose <- is_logging_verbose()

  verbose_logging()
  expect_true(h_test_verbose())

  # Restore settings.
  if (!is_current_verbose) {
    disable_logging()
  }
})
