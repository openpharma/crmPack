# AllModels-class ----

test_that(".AllModels works as expected", {
  result <- expect_silent(.AllModels(datanames = "x"))
  expect_valid(result, "AllModels")
})

# GeneralModel-class ----

test_that(".GeneralModel works as expected", {
  # nolint start
  result <- expect_silent(
    .GeneralModel(
      datamodel = function(x) {},
      priormodel = function(x) {},
      modelspecs = function(x) {},
      init = function(x) {},
      sample = "param1",
      datanames = "x"
    )
  )
  # nolint end
  expect_valid(result, "GeneralModel")
})

# Model-class ----

test_that(".Model works as expected", {
  # nolint start
  result <- expect_silent(
    .Model(
      dose = function(prob, param1) {},
      prob = function(dose, param1) {},
      datamodel = function(x) {},
      priormodel = function(x) {},
      modelspecs = function(x) {},
      init = function(x) {},
      sample = "param1",
      datanames = "x"
    )
  )
  # nolint end
  expect_valid(result, "Model")
})

# LogisticNormal ----

## constructor ----

test_that("LogisticNormal object can be created with user constructor", {
  result <- expect_silent(
    LogisticNormal(
      mean = c(1, 5),
      cov = diag(4, ncol = 2, nrow = 2),
      refDose = 2
    )
  )
  expect_valid(result, "LogisticNormal")
})

## mcmc ----

test_that("MCMC computes correct values for LogisticNormal model", {
  data <- h_get_data()
  model <- h_get_logistic_normal()
  options <- h_get_mcmc_options(small = TRUE, fixed = TRUE)

  result <- mcmc(data = data, model = model, options = options)
  expect_equal(
    result@data,
    list(
      alpha0 = c(-1.955379, -1.955379, -1.955379, -2.325551),
      alpha1 = c(1.450219, 1.450219, 1.450219, 1.059415)
    ),
    tolerance = 1e-06
  )
})

## dose ----

test_that("dose computes correct values for LogisticNormal model", {
  data <- h_get_data()
  model <- h_get_logistic_normal()
  options <- h_get_mcmc_options(small = TRUE, fixed = TRUE)

  samples <- mcmc(data = data, model = model, options = options)
  result <- dose(0.4, model, samples)
  expect_equal(
    result,
    c(145.5861, 145.5861, 145.5861, 306.2573),
    tolerance = 1e-04
  )
})

## prob ----

test_that("prob computes correct values for LogisticNormal model", {
  data <- h_get_data()
  model <- h_get_logistic_normal()
  options <- h_get_mcmc_options(small = TRUE, fixed = TRUE)

  samples <- mcmc(data = data, model = model, options = options)
  result <- prob(145, model, samples)
  expect_equal(
    result,
    c(0.3985968, 0.3985968, 0.3985968, 0.2319058),
    tolerance = 1e-06
  )
})

# LogisticLogNormal ----

## constructor ----

test_that("LogisticLogNormal object can be created with user constructor", {
  result <- expect_silent(
    LogisticLogNormal(
      mean = c(1, 5),
      cov = diag(4, ncol = 2, nrow = 2),
      refDose = 2
    )
  )
  expect_valid(result, "LogisticLogNormal")
})

## mcmc ----

test_that("MCMC computes correct values for LogisticLogNormal model", {
  data <- h_get_data()
  model <- h_get_logistic_log_normal()
  options <- h_get_mcmc_options(small = TRUE, fixed = TRUE)

  result <- mcmc(data = data, model = model, options = options)
  expect_equal(
    result@data,
    list(
      alpha0 = c(-1.296799, -1.296799, -1.296799, -1.680008),
      alpha1 = c(0.975694, 0.975694, 0.975694, 0.651047)
    ),
    tolerance = 1e-06
  )
})

## dose ----

test_that("dose computes correct values for LogisticLogNormal model", {
  data <- h_get_data()
  model <- h_get_logistic_log_normal()
  options <- h_get_mcmc_options(small = TRUE, fixed = TRUE)

  samples <- mcmc(data = data, model = model, options = options)
  result <- dose(0.4, model, samples)
  expect_equal(
    result,
    c(124.6565, 124.6565, 124.6565, 354.1443),
    tolerance = 1e-04
  )
})

## prob ----

test_that("prob computes correct values for LogisticLogNormal model", {
  data <- h_get_data()
  model <- h_get_logistic_log_normal()
  options <- h_get_mcmc_options(small = TRUE, fixed = TRUE)

  samples <- mcmc(data = data, model = model, options = options)
  result <- prob(0.4, model, samples)
  expect_equal(
    result,
    c(0.002453566, 0.002453566, 0.002453566, 0.007974759),
    tolerance = 1e-06
  )
})
