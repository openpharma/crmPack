# h_get_min_inf_beta ----

test_that("h_get_min_inf_beta works as expected with p < q", {
  result <- expect_silent(h_get_min_inf_beta(0.2, 0.5))
  expected <- list(a = 2.322, b = 1)
  expect_equal(result, expected, tolerance = 1e-4)
})

test_that("h_get_min_inf_beta works as expected with p > q", {
  result <- expect_silent(h_get_min_inf_beta(0.7, 0.1))
  expected <- list(a = 1, b = 11.4272)
  expect_equal(result, expected, tolerance = 1e-4)
})

test_that("h_get_min_inf_beta works as expected with p = q", {
  result <- expect_silent(h_get_min_inf_beta(0.1, 0.1))
  expected <- list(a = 1, b = 1)
  expect_identical(result, expected)
})

# h_get_quantiles_start_values ----

test_that("h_get_quantiles_start_values works with NULL parstart", {
  dosegrid <- c(1, 3, 5, 10)
  refDose <- 5
  median <- c(0.1, 0.2, 0.3, 0.4)

  result <- h_get_quantiles_start_values(
    parstart = NULL,
    median = median,
    dosegrid = dosegrid,
    refDose = refDose,
    logNormal = FALSE
  )
  expected <- c(
    meanAlpha = -0.92619,
    meanBeta = 0.79288,
    sdAlpha = 1,
    sdBeta = 1,
    correlation = 0
  )
  expect_equal(result, expected, tolerance = 1e-4)
})

test_that("h_get_quantiles_start_values works with logNormal = TRUE", {
  dosegrid <- c(1, 3, 5, 10)
  refDose <- 5
  median <- c(0.1, 0.2, 0.3, 0.4)

  result <- h_get_quantiles_start_values(
    parstart = NULL,
    median = median,
    dosegrid = dosegrid,
    refDose = refDose,
    logNormal = TRUE
  )

  expected <- c(
    meanAlpha = -0.92619,
    meanBeta = -0.23208,
    sdAlpha = 1,
    sdBeta = 1,
    correlation = 0
  )
  expect_equal(result, expected, tolerance = 1e-4)
})

test_that("h_get_quantiles_start_values works with provided parstart", {
  parstart <- c(1, 2, 3, 4, 5)

  result <- h_get_quantiles_start_values(
    parstart = parstart,
    median = c(0.1, 0.2),
    dosegrid = c(1, 3),
    refDose = 2,
    logNormal = FALSE
  )

  expect_identical(result, parstart)
})

# h_quantiles_target_function ----

test_that("h_quantiles_target_function returns a function", {
  dosegrid <- c(1, 3, 5)
  refDose <- 3
  lower <- c(0.05, 0.1, 0.15)
  median <- c(0.1, 0.2, 0.3)
  upper <- c(0.15, 0.3, 0.45)

  target_fn <- h_quantiles_target_function(
    dosegrid = dosegrid,
    refDose = refDose,
    lower = lower,
    median = median,
    upper = upper,
    level = 0.95,
    logNormal = FALSE,
    seed = 12345
  )

  expect_function(target_fn)

  # Test that the function works with valid parameters
  param <- c(
    meanAlpha = -2,
    meanBeta = 1,
    sdAlpha = 1,
    sdBeta = 1,
    correlation = 0
  )
  result <- target_fn(param)

  expect_type(result, "double")
  expect_length(result, 1)
  expect_true(result >= 0) # Distance should be non-negative

  # Check attributes
  expect_true(all(c("mean", "cov", "quantiles") %in% names(attributes(result))))
  expect_length(attr(result, "mean"), 2)
  expect_equal(dim(attr(result, "cov")), c(2, 2))
  expect_equal(dim(attr(result, "quantiles")), c(3, 3))
})

# Quantiles2LogisticNormal ----

test_that("Quantiles2LogisticNormal works with basic inputs", {
  skip_on_cran_but_not_ci()

  dosegrid <- c(1, 3, 5, 10)
  refDose <- 5
  lower <- c(0.05, 0.1, 0.15, 0.2)
  median <- c(0.1, 0.2, 0.3, 0.4)
  upper <- c(0.15, 0.3, 0.45, 0.6)

  result <- Quantiles2LogisticNormal(
    dosegrid = dosegrid,
    refDose = refDose,
    lower = lower,
    median = median,
    upper = upper,
    seed = 12345,
    control = list(maxit = 10, max.time = 1) # Shorter for testing
  )

  expect_type(result, "list")
  expect_named(
    result,
    c("model", "parameters", "quantiles", "required", "distance")
  )

  # Check model
  expect_s4_class(result$model, "LogisticNormal")

  # Check parameters
  expect_type(result$parameters, "double")
  expect_length(result$parameters, 5)

  # Check quantiles
  expect_equal(dim(result$quantiles), c(4, 3))
  expect_equal(colnames(result$quantiles), c("lower", "median", "upper"))

  # Check required
  expect_equal(dim(result$required), c(4, 3))
  expect_equal(colnames(result$required), c("lower", "median", "upper"))

  # Check distance
  expect_type(result$distance, "double")
  expect_length(result$distance, 1)
  expect_true(result$distance >= 0)
})

test_that("Quantiles2LogisticNormal works with logNormal = TRUE", {
  skip_on_cran_but_not_ci()

  dosegrid <- c(1, 3, 5)
  refDose <- 3
  lower <- c(0.05, 0.1, 0.15)
  median <- c(0.1, 0.2, 0.3)
  upper <- c(0.15, 0.3, 0.45)

  result <- Quantiles2LogisticNormal(
    dosegrid = dosegrid,
    refDose = refDose,
    lower = lower,
    median = median,
    upper = upper,
    logNormal = TRUE,
    seed = 12345,
    control = list(maxit = 10, max.time = 1)
  )

  expect_s4_class(result$model, "LogisticLogNormal")
})

# MinimalInformative ----

test_that("MinimalInformative works with basic inputs", {
  skip_on_cran_but_not_ci()

  dosegrid <- c(1, 3, 5, 10)
  refDose <- 5

  result <- MinimalInformative(
    dosegrid = dosegrid,
    refDose = refDose,
    control = list(maxit = 10, max.time = 1) # Shorter for testing
  )

  expect_type(result, "list")
  expect_named(
    result,
    c("model", "parameters", "quantiles", "required", "distance")
  )
  expect_s4_class(result$model, "LogisticNormal")
})

test_that("MinimalInformative works with logNormal = TRUE", {
  skip_on_cran_but_not_ci()

  dosegrid <- c(1, 3, 5)
  refDose <- 3

  result <- MinimalInformative(
    dosegrid = dosegrid,
    refDose = refDose,
    logNormal = TRUE,
    threshmin = 0.1,
    threshmax = 0.4,
    probmin = 0.1,
    probmax = 0.1,
    control = list(maxit = 10, max.time = 1)
  )
  expect_s4_class(result$model, "LogisticLogNormal")
})
