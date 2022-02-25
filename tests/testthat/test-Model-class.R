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

# ModelLogNormal-class ----

test_that(".ModelLogNormal works as expected", {
  # nolint start
  result <- expect_silent(
    .ModelLogNormal(
      params = ModelParamsNormal(mean = c(0, 2), cov = diag(2)),
      ref_dose = 1,
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
  expect_valid(result, "ModelLogNormal")
})

# ModelLogNormal-constructor ----

test_that("ModelLogNormal object can be created with user constructor", {
  result <- expect_silent(
    ModelLogNormal(
      mean = c(1, 5),
      cov = diag(4, ncol = 2, nrow = 2),
      ref_dose = 2
    )
  )
  expect_valid(result, "ModelLogNormal")
})

# LogisticNormal ----

## constructor ----

test_that("LogisticNormal object can be created with user constructor", {
  result <- expect_silent(
    LogisticNormal(
      mean = c(1, 5),
      cov = diag(4, ncol = 2, nrow = 2),
      ref_dose = 2
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

# LogisticLogNormal ----

## constructor ----

test_that("LogisticLogNormal object can be created with user constructor", {
  result <- expect_silent(
    LogisticLogNormal(
      mean = c(1, 5),
      cov = diag(4, ncol = 2, nrow = 2),
      ref_dose = 2
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

# LogisticLogNormalSub ----

## constructor ----

test_that("LogisticLogNormalSub object can be created with user constructor", {
  result <- expect_silent(
    LogisticLogNormalSub(
      mean = c(1, 5),
      cov = diag(4, ncol = 2, nrow = 2),
      ref_dose = 2
    )
  )
  expect_valid(result, "LogisticLogNormalSub")
})

## mcmc ----

test_that("MCMC computes correct values for LogisticLogNormalSub model", {
  data <- h_get_data()
  model <- h_get_logistic_log_normal_sub()
  options <- h_get_mcmc_options(small = TRUE, fixed = TRUE)

  result <- mcmc(data = data, model = model, options = options)
  expect_equal(
    result@data,
    list(
      alpha0 = c(-2.965073, -2.965073, -2.965073, -2.965073),
      alpha1 = c(0.00974706, 0.00974706, 0.00974706, 0.00974706)
    ),
    tolerance = 1e-06
  )
})

# ProbitLogNormal ----

## constructor ----

test_that("ProbitLogNormal object can be created with user constructor", {
  result <- expect_silent(
    ProbitLogNormal(
      mean = c(1, 5),
      cov = diag(4, ncol = 2, nrow = 2),
      ref_dose = 2
    )
  )
  expect_valid(result, "ProbitLogNormal")
})

## mcmc ----

test_that("MCMC computes correct values for ProbitLogNormal model", {
  data <- h_get_data()
  model <- h_get_probit_log_normal()
  options <- h_get_mcmc_options(small = TRUE, fixed = TRUE)

  result <- mcmc(data = data, model = model, options = options)
  expect_equal(
    result@data,
    list(
      alpha0 = c(-1.709947, -1.709947, -1.709947, -1.988543),
      alpha1 = c(0.4101207, 0.4101207, 0.4101207, 0.3056156)
    ),
    tolerance = 1e-06
  )
})

# ProbitLogNormalRel ----

## constructor ----

test_that("ProbitLogNormalRel object can be created with user constructor", {
  result <- expect_silent(
    ProbitLogNormalRel(
      mean = c(1, 5),
      cov = diag(4, ncol = 2, nrow = 2),
      ref_dose = 2
    )
  )
  expect_valid(result, "ProbitLogNormalRel")
})

## mcmc ----

test_that("MCMC computes correct values for ProbitLogNormalRel model", {
  data <- h_get_data()
  model <- h_get_probit_log_normal_rel()
  options <- h_get_mcmc_options(small = TRUE, fixed = TRUE)

  result <- mcmc(data = data, model = model, options = options)
  expect_equal(
    result@data,
    list(
      alpha0 = c(-0.8353588, -0.8353588, -0.8353588, -0.8353588),
      alpha1 = c(0.02201234, 0.02201234, 0.02201234, 0.02201234)
    ),
    tolerance = 1e-06
  )
})

# LogisticKadane ----

## constructor ----

test_that("LogisticKadane object can be created with user constructor", {
  result <- expect_silent(
    LogisticKadane(
      theta = 0.33,
      xmin = 1,
      xmax = 200
    )
  )
  expect_valid(result, "LogisticKadane")
})

# LogisticNormalMixture ----

## constructor ----

test_that("LogisticNormalMixture object can be created with user constructor", {
  result <- expect_silent(
    LogisticNormalMixture(
      comp1 = ModelParamsNormal(mean = c(0, 3), cov = diag(2)),
      comp2 = ModelParamsNormal(mean = c(-1, 6), cov = c(2, 4) * diag(2)),
      weightpar = c(a = 1, b = 5),
      ref_dose = 2
    )
  )
  expect_valid(result, "LogisticNormalMixture")
})
