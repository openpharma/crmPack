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
  expect_snapshot(result@data)
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
  expect_snapshot(result@data)
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
  expect_snapshot(result@data)
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
  expect_snapshot(result@data)
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
  expect_snapshot(result@data)
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

## mcmc ----

test_that("MCMC computes correct values for LogisticKadane model", {
  data <- h_get_data()
  model <- h_get_logistic_kadane()
  options <- h_get_mcmc_options(small = TRUE, fixed = TRUE)

  result <- mcmc(data = data, model = model, options = options)
  expect_snapshot(result@data)
})

# LogisticKadaneBetaGamma ----

## constructor ----

test_that("LogisticKadaneBetaGamma object can be created with user constructor", {
  result <- expect_silent(
    LogisticKadaneBetaGamma(
      theta = 0.3,
      xmin = 0,
      xmax = 7,
      alpha = 1,
      beta = 19,
      shape = 0.5625,
      rate = 0.125
    )
  )
  expect_valid(result, "LogisticKadaneBetaGamma")
})

## mcmc ----

test_that("MCMC computes correct values for LogisticKadaneBetaGamma model", {
  data <- h_get_data_no_plcb_k()
  model <- h_get_logistic_kadane_beta_gam()
  options <- h_get_mcmc_options(small = TRUE, fixed = TRUE)

  result <- mcmc(data = data, model = model, options = options)
  expect_snapshot(result@data)
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

## mcmc ----

test_that("MCMC computes correct values for LogisticNormalMixture model", {
  data <- h_get_data_mixture()
  model <- h_get_logistic_normal_mix()
  options <- h_get_mcmc_options(small = TRUE, fixed = TRUE)

  result <- mcmc(data = data, model = model, options = options)
  expect_snapshot(result@data)
})

# LogisticNormalFixedMixture ----

## constructor ----

test_that("LogisticNormalFixedMixture object can be created with user constructor", {
  result <- expect_silent(
    LogisticNormalFixedMixture(
      components = list(
        comp1 = ModelParamsNormal(
          mean = c(-0.85, 1),
          cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2)
        ),
        comp2 = ModelParamsNormal(
          mean = c(1, 1.5),
          cov = matrix(c(1.2, -0.45, -0.45, 0.6), nrow = 2)
        )
      ),
      weights = c(0.3, 0.7),
      ref_dose = 50
    )
  )
  expect_valid(result, "LogisticNormalFixedMixture")
})

## mcmc ----

test_that("MCMC computes correct values for LogisticNormalFixedMixture model", {
  data <- h_get_data_mixture()
  model <- h_get_logistic_normal_fixed_mix()
  options <- h_get_mcmc_options(small = TRUE, fixed = TRUE)

  result <- mcmc(data = data, model = model, options = options)
  expect_snapshot(result@data)
})

# LogisticLogNormalMixture ----

## constructor ----

test_that("LogisticLogNormalMixture object can be created with user constructor", {
  result <- expect_silent(
    LogisticLogNormalMixture(
      mean = c(0, 1),
      cov = diag(2),
      share_weight = 0.1,
      ref_dose = 1
    )
  )
  expect_valid(result, "LogisticLogNormalMixture")
})

## mcmc ----

test_that("MCMC computes correct values for LogisticLogNormalMixture model", {
  data <- h_get_data_mixture()
  model <- h_get_logistic_log_normal_mix()
  options <- h_get_mcmc_options(small = TRUE, fixed = TRUE)

  result <- mcmc(data = data, model = model, options = options)
  expect_snapshot(result@data)
})

# DualEndpoint ----

## constructor ----

test_that("DualEndpoint object can be created with user constructor (fixed params)", {
  result <- expect_silent(h_get_dual_endpoint())
  expect_valid(result, "DualEndpoint")
})

test_that("DualEndpoint object can be created with user constructor", {
  result <- expect_silent(h_get_dual_endpoint(fixed = FALSE))
  expect_valid(result, "DualEndpoint")
})

# DualEndpointRW ----

## constructor ----

test_that("DualEndpointRW object can be created with user constructor (fixed params)", {
  result <- expect_silent(h_get_dual_endpoint_rw())
  expect_valid(result, "DualEndpointRW")
})

test_that("DualEndpointRW object can be created with user constructor", {
  result <- expect_silent(h_get_dual_endpoint_rw(fixed = FALSE))
  expect_valid(result, "DualEndpointRW")
})

## mcmc ----

test_that("MCMC computes correct values for DualEndpointRW model (fixed params)", {
  data <- h_get_data_dual()
  model <- h_get_dual_endpoint_rw()
  options <- h_get_mcmc_options(small = TRUE, fixed = TRUE)

  result <- mcmc(data = data, model = model, options = options)
  expect_snapshot(result@data)
})

test_that("MCMC computes correct values for DualEndpointRW model", {
  data <- h_get_data_dual()
  model <- h_get_dual_endpoint_rw(fixed = FALSE)
  options <- h_get_mcmc_options(small = TRUE, fixed = TRUE)

  result <- mcmc(data = data, model = model, options = options)
  expect_snapshot(result@data)
})

test_that("MCMC computes correct values for DualEndpointRW model with RW2", {
  data <- h_get_data_dual()
  model <- h_get_dual_endpoint_rw(rw1 = FALSE)
  options <- h_get_mcmc_options(small = TRUE, fixed = TRUE)

  result <- mcmc(data = data, model = model, options = options)
  expect_snapshot(result@data)
})

test_that("MCMC computes correct values for DualEndpointRW model (fixed params) with RW2", {
  data <- h_get_data_dual()
  model <- h_get_dual_endpoint_rw(rw1 = FALSE, fixed = FALSE)
  options <- h_get_mcmc_options(small = TRUE, fixed = TRUE)

  result <- mcmc(data = data, model = model, options = options)
  expect_snapshot(result@data)
})

# DualEndpointBeta ----

## constructor ----

test_that("DualEndpointBeta object can be created with user constructor (fixed params)", {
  result <- expect_silent(h_get_dual_endpoint_beta())
  expect_valid(result, "DualEndpointBeta")
})

test_that("DualEndpointBeta object can be created with user constructor", {
  result <- expect_silent(h_get_dual_endpoint_beta(fixed = FALSE))
  expect_valid(result, "DualEndpointBeta")
})

## mcmc ----

test_that("MCMC computes correct values for DualEndpointBeta model with fixed parameters", {
  data <- h_get_data_dual()
  model <- h_get_dual_endpoint_beta()
  options <- h_get_mcmc_options(small = TRUE, fixed = TRUE)

  result <- mcmc(data = data, model = model, options = options)
  expect_snapshot(result@data)
})

test_that("MCMC computes correct values for DualEndpointBeta model", {
  data <- h_get_data_dual()
  model <- h_get_dual_endpoint_beta(fixed = FALSE)
  options <- h_get_mcmc_options(small = TRUE, fixed = TRUE)

  result <- mcmc(data = data, model = model, options = options)
  expect_snapshot(result@data)
})

test_that("MCMC throws the error when ref_dose_beta is not greater than max dose in grid)", {
  data <- h_get_data_dual()
  model <- h_get_dual_endpoint_beta()
  options <- h_get_mcmc_options(small = TRUE, fixed = TRUE)

  model@ref_dose_beta <- data@doseGrid[data@nGrid] - 1
  expect_error(
    mcmc(data = data, model = model, options = options),
    "Assertion on 'model@ref_dose_beta > data@doseGrid\\[data@nGrid\\]' failed: Must be TRUE."
  )
})

# DualEndpointEmax ----

## constructor ----

test_that("DualEndpointEmax object can be created with user constructor (fixed params)", {
  result <- expect_silent(h_get_dual_endpoint_emax())
  expect_valid(result, "DualEndpointEmax")
})

test_that("DualEndpointEmax object can be created with user constructor", {
  result <- expect_silent(h_get_dual_endpoint_emax(fixed = FALSE))
  expect_valid(result, "DualEndpointEmax")
})

## mcmc ----

test_that("MCMC computes correct values for DualEndpointEmax model with fixed parameters", {
  data <- h_get_data_dual()
  model <- h_get_dual_endpoint_emax()
  options <- h_get_mcmc_options(small = TRUE, fixed = TRUE)

  result <- mcmc(data = data, model = model, options = options)
  expect_snapshot(result@data)
})

test_that("MCMC computes correct values for DualEndpointEmax model", {
  data <- h_get_data_dual()
  model <- h_get_dual_endpoint_emax(fixed = FALSE)
  options <- h_get_mcmc_options(small = TRUE, fixed = TRUE)

  result <- mcmc(data = data, model = model, options = options)
  expect_snapshot(result@data)
})
