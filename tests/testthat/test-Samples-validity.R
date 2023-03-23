# v_samples ----

test_that("v_samples passes for valid object (1 scalar parameter)", {
  data <- list(param_a = 1:10)
  options <- McmcOptions(samples = 10)
  object <- Samples(data, options)
  expect_true(v_samples(object))
})

test_that("v_samples passes for valid object (2 scalar parameters)", {
  data <- list(param_a = 1:10, param_b = 21:30)
  options <- McmcOptions(samples = 10)
  object <- Samples(data, options)
  expect_true(v_samples(object))
})

test_that("v_samples passes for valid object (1 vector-valued and 1 scalar parameters)", {
  data <- list(param_a = matrix(1:12, nrow = 4), param_b = 2:5)
  options <- McmcOptions(samples = 4)
  object <- Samples(data, options)
  expect_true(v_samples(object))
})

test_that("v_samples returns message for discrepancy in sample size lengths", {
  err_msg <- "Every element in data must be of the same length (no. of rows) as the sample size was"
  data <- list(param_a = 1:10, param_b = 21:30)
  options <- McmcOptions(samples = 10)
  object <- Samples(data, options)

  # Changing `data$param_b` so that its length is different than 10.
  object@data$param_b <- 1:4
  expect_equal(v_samples(object), err_msg)

  # Changing also `data$param_a` so that its length is different than 10.
  object@data$param_a <- 1:4
  expect_equal(v_samples(object), err_msg)
})

test_that("v_samples returns message for discrepancy in sample size lengths (matrix)", {
  err_msg <- "Every element in data must be of the same length (no. of rows) as the sample size was"
  data <- list(param_a = matrix(1:12, nrow = 4), param_b = 2:5)
  options <- McmcOptions(samples = 4)
  object <- Samples(data, options)

  # Changing `data$param_a` so that its number of rows is different than 4.
  object@data$param_a <- matrix(1:12, nrow = 6)
  expect_equal(v_samples(object), err_msg)
})

test_that("v_samples returns message for non-numeric samples", {
  err_msg <- "Every element in data must be a finite object of type integer or double"
  data <- list(param_a = 1:10, param_b = 21:30)
  options <- McmcOptions(samples = 10)
  object <- Samples(data, options)

  # Changing `data$param_a` so that it is not a numeric object.
  object@data$param_a <- letters[1:10]
  expect_equal(v_samples(object), err_msg)
})
