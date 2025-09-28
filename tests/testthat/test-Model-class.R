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
      datanames = "x",
      datanames_prior = "x1"
    )
  )
  # nolint end
  expect_valid(result, "GeneralModel")
})

# ModelLogNormal-class ----

test_that(".ModelLogNormal works as expected", {
  # nolint start
  result <- expect_silent(
    .ModelLogNormal(
      params = ModelParamsNormal(mean = c(0, 2), cov = diag(2)),
      ref_dose = positive_number(1),
      datamodel = function(x) {},
      priormodel = function(x) {},
      modelspecs = function(x) {},
      init = function(x) {},
      sample = "param1",
      datanames = "x",
      datanames_prior = "x1"
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

test_that(".DefaultLogisticNormal works as expected", {
  expect_equal(
    .DefaultLogisticNormal(),
    LogisticNormal(
      mean = c(-0.85, 1),
      cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2)
    )
  )
})

## mcmc ----

test_that("MCMC computes correct values for LogisticNormal model", {
  data <- h_get_data()
  model <- h_get_logistic_normal()
  options <- h_get_mcmc_options()

  result <- mcmc(data = data, model = model, options = options)
  expect_snapshot(result@data)
})

test_that("MCMC computes correct values for LogisticNormal model and empty data", {
  data <- h_get_data(empty = TRUE)
  model <- h_get_logistic_normal()
  options <- h_get_mcmc_options()

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

test_that(".DefaultLogisticLogNormal works as expected", {
  expect_equal(
    .DefaultLogisticLogNormal(),
    LogisticLogNormal(
      mean = c(-0.85, 1),
      cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
      ref_dose = 50
    )
  )
})

## mcmc ----

test_that("MCMC computes correct values for LogisticLogNormal model", {
  data <- h_get_data()
  model <- h_get_logistic_log_normal()
  options <- h_get_mcmc_options()

  result <- mcmc(data = data, model = model, options = options)
  expect_snapshot(result@data)
})

test_that("MCMC computes correct values for LogisticLogNormal model and empty data", {
  data <- h_get_data(empty = TRUE)
  model <- h_get_logistic_log_normal()
  options <- h_get_mcmc_options()

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

test_that(".DefaultLogisticLogNormalSub works correctly", {
  expect_equal(
    .DefaultLogisticLogNormalSub(),
    LogisticLogNormalSub(
      mean = c(-0.85, 1),
      cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
      ref_dose = 50
    )
  )
})

## mcmc ----

test_that("MCMC computes correct values for LogisticLogNormalSub model", {
  data <- h_get_data()
  model <- h_get_logistic_log_normal_sub()
  options <- h_get_mcmc_options()

  result <- mcmc(data = data, model = model, options = options)
  expect_snapshot(result@data)
})

test_that("MCMC computes correct values for LogisticLogNormalSub model and empty data", {
  data <- h_get_data(empty = TRUE)
  model <- h_get_logistic_log_normal_sub()
  options <- h_get_mcmc_options()

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

test_that(".DefaultProbitLogNormal works correctly", {
  expect_equal(
    .DefaultProbitLogNormal(),
    ProbitLogNormal(
      mean = c(-0.85, 1),
      cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
      ref_dose = 7.2
    )
  )
})

## mcmc ----

test_that("MCMC computes correct values for ProbitLogNormal model", {
  data <- h_get_data()
  model <- h_get_probit_log_normal()
  options <- h_get_mcmc_options()

  result <- mcmc(data = data, model = model, options = options)
  expect_snapshot(result@data)
})

test_that("MCMC computes correct values for ProbitLogNormal model and empty data", {
  data <- h_get_data(empty = TRUE)
  model <- h_get_probit_log_normal()
  options <- h_get_mcmc_options()

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

test_that(".DefaultProbitLogNormalRel works correctly", {
  expect_equal(
    .DefaultProbitLogNormalRel(),
    ProbitLogNormalRel(
      mean = c(-0.85, 1),
      cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2)
    )
  )
})

## mcmc ----

test_that("MCMC computes correct values for ProbitLogNormalRel model", {
  data <- h_get_data()
  model <- h_get_probit_log_normal_rel()
  options <- h_get_mcmc_options()

  result <- mcmc(data = data, model = model, options = options)
  expect_snapshot(result@data)
})

test_that("MCMC computes correct values for ProbitLogNormalRel model and empty data", {
  data <- h_get_data(empty = TRUE)
  model <- h_get_probit_log_normal_rel()
  options <- h_get_mcmc_options()

  result <- mcmc(data = data, model = model, options = options)
  expect_snapshot(result@data)
})

# LogisticLogNormalGrouped ----

## constructor ----

test_that("LogisticLogNormalGrouped object can be created with user constructor", {
  result <- expect_silent(
    LogisticLogNormalGrouped(
      mean = 1:4,
      cov = diag(1:4, 4),
      ref_dose = 2
    )
  )
  expect_valid(result, "LogisticLogNormalGrouped")
})

test_that(".DefaultLogisticLogNormalGrouped works as expected", {
  expect_valid(
    .DefaultLogisticLogNormalGrouped(),
    "LogisticLogNormalGrouped"
  )
})

## mcmc ----

test_that("MCMC computes correct values for LogisticLogNormalGrouped model", {
  data <- h_get_data_grouped()
  model <- .DefaultLogisticLogNormalGrouped()
  options <- h_get_mcmc_options()

  result <- mcmc(data = data, model = model, options = options)
  expect_snapshot(result@data)
})

test_that("MCMC computes correct values for LogisticLogNormalGrouped model and empty data", {
  data <- h_get_data_grouped(empty = TRUE)
  model <- .DefaultLogisticLogNormalGrouped()
  options <- h_get_mcmc_options()

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

test_that(".DefaultLogisticKadane works correctly", {
  expect_equal(
    .DefaultLogisticKadane(),
    LogisticKadane(theta = 0.33, xmin = 1, xmax = 200)
  )
})

## mcmc ----

test_that("MCMC computes correct values for LogisticKadane model", {
  data <- h_get_data()
  model <- h_get_logistic_kadane()
  options <- h_get_mcmc_options()

  result <- mcmc(data = data, model = model, options = options)
  expect_snapshot(result@data)
})

test_that("MCMC computes correct values for LogisticKadane model and empty data", {
  data <- h_get_data(empty = TRUE)
  model <- h_get_logistic_kadane()
  options <- h_get_mcmc_options()

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

test_that(".DefaultLogisticKadaneBetaGamma works correctly", {
  expect_equal(
    .DefaultLogisticKadaneBetaGamma(),
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
})

## mcmc ----

test_that("MCMC computes correct values for LogisticKadaneBetaGamma model", {
  data <- h_get_data_2()
  model <- h_get_logistic_kadane_beta_gam()
  options <- h_get_mcmc_options()

  result <- mcmc(data = data, model = model, options = options)
  expect_snapshot(result@data)
})

test_that("MCMC computes correct values for LogisticKadaneBetaGamma model and empty data", {
  data <- h_get_data(empty = TRUE)
  model <- h_get_logistic_kadane_beta_gam()
  options <- h_get_mcmc_options()

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


test_that(".DefaultLogisticNormalMixture works correctly", {
  expect_equal(
    .DefaultLogisticNormalMixture(),
    LogisticNormalMixture(
      comp1 = ModelParamsNormal(
        mean = c(-0.85, 1),
        cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2)
      ),
      comp2 = ModelParamsNormal(
        mean = c(1, 1.5),
        cov = matrix(c(1.2, -0.45, -0.45, 0.6), nrow = 2)
      ),
      weightpar = c(a = 1, b = 1),
      ref_dose = 50
    )
  )
})
## mcmc ----

test_that("MCMC computes correct values for LogisticNormalMixture model", {
  data <- h_get_data_mixture()
  model <- h_get_logistic_normal_mix()
  options <- h_get_mcmc_options()

  result <- mcmc(data = data, model = model, options = options)
  expect_snapshot(result@data)
})

test_that("MCMC computes correct values for LogisticNormalMixture model and empty data", {
  data <- h_get_data_mixture(empty = TRUE)
  model <- h_get_logistic_normal_mix()
  options <- h_get_mcmc_options()

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

test_that(".DefaultLogisticNormalFixedMixture works as expected", {
  expect_equal(
    .DefaultLogisticNormalFixedMixture(),
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
})

## mcmc ----

test_that("MCMC computes correct values for LogisticNormalFixedMixture model", {
  data <- h_get_data_mixture()
  model <- h_get_logistic_normal_fixed_mix()
  model_log_normal <- h_get_logistic_normal_fixed_mix(log_normal = TRUE)
  options <- h_get_mcmc_options()

  result <- mcmc(data = data, model = model, options = options)
  result_log_normal <- mcmc(
    data = data,
    model = model_log_normal,
    options = options
  )
  expect_snapshot(result@data)
  expect_snapshot(result_log_normal@data)
})

test_that("MCMC computes correct values for LogisticNormalFixedMixture model and empty data", {
  data <- h_get_data_mixture(empty = TRUE)
  model <- h_get_logistic_normal_fixed_mix()
  model_log_normal <- h_get_logistic_normal_fixed_mix(log_normal = TRUE)
  options <- h_get_mcmc_options()

  result <- mcmc(data = data, model = model, options = options)
  result_log_normal <- mcmc(
    data = data,
    model = model_log_normal,
    options = options
  )
  expect_snapshot(result@data)
  expect_snapshot(result_log_normal@data)
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

test_that(".DefaultLogisticLogNormalMixture works as expected", {
  test_obj <-
    expect_equal(
      .DefaultLogisticLogNormalMixture(),
      LogisticLogNormalMixture(
        share_weight = 0.1,
        mean = c(-0.85, 1),
        cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
        ref_dose = 50
      )
    )
})

## mcmc ----

test_that("MCMC computes correct values for LogisticLogNormalMixture model", {
  data <- h_get_data_mixture()
  model <- h_get_logistic_log_normal_mix()
  options <- h_get_mcmc_options()

  result <- mcmc(data = data, model = model, options = options)
  expect_snapshot(result@data)
})

test_that("MCMC computes correct values for LogisticLogNormalMixture model and empty data", {
  data <- h_get_data_mixture(empty = TRUE)
  model <- h_get_logistic_log_normal_mix()
  options <- h_get_mcmc_options()

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

test_that(".DefaultDualEndpointRW works correctly", {
  expect_equal(
    .DefaultDualEndpointRW(),
    DualEndpointRW(
      mean = c(0, 1),
      cov = matrix(c(1, 0, 0, 1), nrow = 2),
      sigma2W = c(a = 0.1, b = 0.1),
      rho = c(a = 1, b = 1),
      sigma2betaW = 0.01,
      rw1 = TRUE
    )
  )
})

## mcmc ----

test_that("MCMC computes correct values for DualEndpointRW model (fixed params)", {
  data <- h_get_data_dual()
  model <- h_get_dual_endpoint_rw()
  model_log_dose <- h_get_dual_endpoint_rw(use_log_dose = TRUE)
  options <- h_get_mcmc_options()

  result <- mcmc(data = data, model = model, options = options)
  result_log_dose <- mcmc(
    data = data,
    model = model_log_dose,
    options = options
  )
  expect_snapshot(result@data)
  expect_snapshot(result_log_dose@data)
})

test_that("MCMC computes correct values for DualEndpointRW model", {
  data <- h_get_data_dual()
  model <- h_get_dual_endpoint_rw(fixed = FALSE)
  model_log_dose <- h_get_dual_endpoint_rw(use_log_dose = TRUE, fixed = FALSE)
  options <- h_get_mcmc_options()

  result <- mcmc(data = data, model = model, options = options)
  result_log_dose <- mcmc(
    data = data,
    model = model_log_dose,
    options = options
  )
  expect_snapshot(result@data)
  expect_snapshot(result_log_dose@data)
})

test_that("MCMC computes correct values for DualEndpointRW model with RW2", {
  data <- h_get_data_dual()
  model <- h_get_dual_endpoint_rw(rw1 = FALSE)
  model_log_dose <- h_get_dual_endpoint_rw(use_log_dose = TRUE, rw1 = FALSE)
  options <- h_get_mcmc_options()

  result <- mcmc(data = data, model = model, options = options)
  result_log_dose <- mcmc(
    data = data,
    model = model_log_dose,
    options = options
  )
  expect_snapshot(result@data)
  expect_snapshot(result_log_dose@data)
})

test_that("MCMC computes correct values for DualEndpointRW model (fixed params) with RW2", {
  data <- h_get_data_dual()
  model <- h_get_dual_endpoint_rw(rw1 = FALSE, fixed = FALSE)
  model_log_dose <- h_get_dual_endpoint_rw(
    use_log_dose = TRUE,
    rw1 = FALSE,
    fixed = FALSE
  )
  options <- h_get_mcmc_options()

  result <- mcmc(data = data, model = model, options = options)
  result_log_dose <- mcmc(
    data = data,
    model = model_log_dose,
    options = options
  )
  expect_snapshot(result@data)
  expect_snapshot(result_log_dose@data)
})

test_that("MCMC computes correct values for DualEndpointRW model (fixed params, empty data)", {
  data <- h_get_data_dual(empty = TRUE)
  model <- h_get_dual_endpoint_rw()
  model_log_dose <- h_get_dual_endpoint_rw(use_log_dose = TRUE)
  options <- h_get_mcmc_options()

  result <- mcmc(data = data, model = model, options = options)
  result_log_dose <- mcmc(
    data = data,
    model = model_log_dose,
    options = options
  )
  expect_snapshot(result@data)
  expect_snapshot(result_log_dose@data)
})

test_that("MCMC computes correct values for DualEndpointRW model (empty data)", {
  data <- h_get_data_dual(empty = TRUE)
  model <- h_get_dual_endpoint_rw(fixed = FALSE)
  model_log_dose <- h_get_dual_endpoint_rw(use_log_dose = TRUE, fixed = FALSE)
  options <- h_get_mcmc_options()

  result <- mcmc(data = data, model = model, options = options)
  result_log_dose <- mcmc(
    data = data,
    model = model_log_dose,
    options = options
  )
  expect_snapshot(result@data)
  expect_snapshot(result_log_dose@data)
})

test_that("MCMC computes correct values for DualEndpointRW model with RW2 (empty data)", {
  data <- h_get_data_dual(empty = TRUE)
  model <- h_get_dual_endpoint_rw(rw1 = FALSE)
  model_log_dose <- h_get_dual_endpoint_rw(use_log_dose = TRUE, rw1 = FALSE)
  options <- h_get_mcmc_options()

  result <- mcmc(data = data, model = model, options = options)
  result_log_dose <- mcmc(
    data = data,
    model = model_log_dose,
    options = options
  )
  expect_snapshot(result@data)
  expect_snapshot(result_log_dose@data)
})

test_that("MCMC computes correct values for DualEndpointRW model (fixed params, empty data) with RW2", {
  data <- h_get_data_dual(empty = TRUE)
  model <- h_get_dual_endpoint_rw(rw1 = FALSE, fixed = FALSE)
  model_log_dose <- h_get_dual_endpoint_rw(
    use_log_dose = TRUE,
    rw1 = FALSE,
    fixed = FALSE
  )
  options <- h_get_mcmc_options()

  result <- mcmc(data = data, model = model, options = options)
  result_log_dose <- mcmc(
    data = data,
    model = model_log_dose,
    options = options
  )
  expect_snapshot(result@data)
  expect_snapshot(result_log_dose@data)
})

test_that("MCMC throws the error for DualEndpointRW model when 'nGrid == 1' for RW 1", {
  data <- h_get_data_dual(empty = TRUE)
  model <- h_get_dual_endpoint_rw(rw1 = TRUE)
  options <- h_get_mcmc_options()

  data@nGrid <- 1L
  data@doseGrid <- data@doseGrid[1]

  expect_error(
    mcmc(data = data, model = model, options = options),
    "Assertion on 'data@nGrid >= 2' failed: Must be TRUE"
  )
})

test_that("MCMC throws the error for DualEndpointRW model when 'nGrid <= 2' for RW 2", {
  data <- h_get_data_dual(empty = TRUE)
  model <- h_get_dual_endpoint_rw(rw1 = FALSE)
  options <- h_get_mcmc_options()

  data@nGrid <- 1L
  data@doseGrid <- data@doseGrid[1]

  expect_error(
    mcmc(data = data, model = model, options = options),
    "Assertion on 'data@nGrid >= 3' failed: Must be TRUE"
  )

  data@nGrid <- 2L
  data@doseGrid <- data@doseGrid[1:2]

  expect_error(
    mcmc(data = data, model = model, options = options),
    "Assertion on 'data@nGrid >= 3' failed: Must be TRUE"
  )
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

test_that(".DefaultDualEndpointBeta works as expected", {
  expect_equal(
    .DefaultDualEndpointBeta(),
    DualEndpointBeta(
      mean = c(0, 1),
      cov = matrix(c(1, 0, 0, 1), nrow = 2),
      ref_dose = 10,
      use_log_dose = TRUE,
      sigma2W = c(a = 0.1, b = 0.1),
      rho = c(a = 1, b = 1),
      E0 = c(0, 100),
      Emax = c(0, 500),
      delta1 = c(0, 5),
      mode = c(1, 15),
      ref_dose_beta = 1000
    )
  )
})

## mcmc ----

test_that("MCMC computes correct values for DualEndpointBeta model with fixed parameters", {
  data <- h_get_data_dual()
  model <- h_get_dual_endpoint_beta()
  model_log_dose <- h_get_dual_endpoint_beta(use_log_dose = TRUE)
  options <- h_get_mcmc_options()

  result <- mcmc(data = data, model = model, options = options)
  result_log_dose <- mcmc(
    data = data,
    model = model_log_dose,
    options = options
  )
  expect_snapshot(result@data)
  expect_snapshot(result_log_dose@data)
})

test_that("MCMC computes correct values for DualEndpointBeta model", {
  data <- h_get_data_dual()
  model <- h_get_dual_endpoint_beta(fixed = FALSE)
  model_log_dose <- h_get_dual_endpoint_beta(use_log_dose = TRUE, fixed = FALSE)
  options <- h_get_mcmc_options()

  result <- mcmc(data = data, model = model, options = options)
  result_log_dose <- mcmc(
    data = data,
    model = model_log_dose,
    options = options
  )
  expect_snapshot(result@data)
  expect_snapshot(result_log_dose@data)
})

test_that("MCMC throws the error for DualEndpointBeta model when 'ref_dose_beta <= max(doseGrid)'", {
  data <- h_get_data_dual()
  model <- h_get_dual_endpoint_beta()
  options <- h_get_mcmc_options()

  model@ref_dose_beta <- positive_number(data@doseGrid[data@nGrid] - 1)
  expect_error(
    mcmc(data = data, model = model, options = options),
    "Assertion on 'model@ref_dose_beta > data@doseGrid\\[data@nGrid\\]' failed: Must be TRUE."
  )
})

test_that("MCMC throws the error for DualEndpointBeta model when 'nGrid == 0'", {
  data <- DataDual()
  model <- h_get_dual_endpoint_beta()
  options <- h_get_mcmc_options()

  expect_error(
    mcmc(data = data, model = model, options = options),
    "Assertion on 'data@nGrid >= 1' failed: Must be TRUE"
  )
})

test_that("MCMC computes correct values for DualEndpointBeta model with fixed parameters (empty data)", {
  data <- h_get_data_dual(empty = TRUE)
  model <- h_get_dual_endpoint_beta()
  model_log_dose <- h_get_dual_endpoint_beta(use_log_dose = TRUE)
  options <- h_get_mcmc_options()

  result <- mcmc(data = data, model = model, options = options)
  result_log_dose <- mcmc(
    data = data,
    model = model_log_dose,
    options = options
  )
  expect_snapshot(result@data)
  expect_snapshot(result_log_dose@data)
})

test_that("MCMC computes correct values for DualEndpointBeta model (empty data)", {
  data <- h_get_data_dual(empty = TRUE)
  model <- h_get_dual_endpoint_beta(fixed = FALSE)
  model_log_dose <- h_get_dual_endpoint_beta(use_log_dose = TRUE, fixed = FALSE)
  options <- h_get_mcmc_options()

  result <- mcmc(data = data, model = model, options = options)
  result_log_dose <- mcmc(
    data = data,
    model = model_log_dose,
    options = options
  )
  expect_snapshot(result@data)
  expect_snapshot(result_log_dose@data)
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

test_that(".DefaultDualEndpointEmax works correctly", {
  expect_equal(
    .DefaultDualEndpointEmax(),
    DualEndpointEmax(
      mean = c(0, 1),
      cov = matrix(c(1, 0, 0, 1), nrow = 2),
      sigma2W = c(a = 0.1, b = 0.1),
      rho = c(a = 1, b = 1),
      E0 = c(0, 100),
      Emax = c(0, 500),
      ED50 = c(10, 200),
      ref_dose_emax = 1000
    )
  )
})

## mcmc ----

test_that("MCMC computes correct values for DualEndpointEmax model with fixed parameters", {
  data <- h_get_data_dual()
  model <- h_get_dual_endpoint_emax()
  model_log_dose <- h_get_dual_endpoint_emax(use_log_dose = TRUE)
  options <- h_get_mcmc_options()

  result <- mcmc(data = data, model = model, options = options)
  result_log_dose <- mcmc(
    data = data,
    model = model_log_dose,
    options = options
  )
  expect_snapshot(result@data)
  expect_snapshot(result_log_dose@data)
})

test_that("MCMC computes correct values for DualEndpointEmax model", {
  data <- h_get_data_dual()
  model <- h_get_dual_endpoint_emax(fixed = FALSE)
  model_log_dose <- h_get_dual_endpoint_emax(use_log_dose = TRUE, fixed = FALSE)
  options <- h_get_mcmc_options()

  result <- mcmc(data = data, model = model, options = options)
  result_log_dose <- mcmc(
    data = data,
    model = model_log_dose,
    options = options
  )
  expect_snapshot(result@data)
  expect_snapshot(result_log_dose@data)
})

test_that("MCMC computes correct values for DualEndpointEmax model with fixed parameters (empty data)", {
  data <- h_get_data_dual(empty = TRUE)
  model <- h_get_dual_endpoint_emax()
  model_log_dose <- h_get_dual_endpoint_emax(use_log_dose = TRUE)
  options <- h_get_mcmc_options()

  result <- mcmc(data = data, model = model, options = options)
  result_log_dose <- mcmc(
    data = data,
    model = model_log_dose,
    options = options
  )
  expect_snapshot(result@data)
  expect_snapshot(result_log_dose@data)
})

test_that("MCMC computes correct values for DualEndpointEmax model (empty data)", {
  data <- h_get_data_dual(empty = TRUE)
  model <- h_get_dual_endpoint_emax(fixed = FALSE)
  model_log_dose <- h_get_dual_endpoint_emax(use_log_dose = TRUE, fixed = FALSE)
  options <- h_get_mcmc_options()

  result <- mcmc(data = data, model = model, options = options)
  result_log_dose <- mcmc(
    data = data,
    model = model_log_dose,
    options = options
  )
  expect_snapshot(result@data)
  expect_snapshot(result_log_dose@data)
})

test_that("MCMC throws the error for DualEndpointEmax model when 'nGrid == 0'", {
  data <- DataDual()
  model <- h_get_dual_endpoint_emax()
  options <- h_get_mcmc_options()

  expect_error(
    mcmc(data = data, model = model, options = options),
    "Assertion on 'data@nGrid >= 1' failed: Must be TRUE"
  )
})

# LogisticIndepBeta ----

## constructor ----

test_that("LogisticIndepBeta object can be created with user constructor (empty data)", {
  result <- expect_silent(h_get_logistic_indep_beta(emptydata = TRUE))
  expect_valid(result, "LogisticIndepBeta")
})

test_that("LogisticIndepBeta object can be created with user constructor", {
  result <- expect_silent(h_get_logistic_indep_beta(emptydata = FALSE))
  expect_valid(result, "LogisticIndepBeta")
})

## mcmc ----

test_that("MCMC computes correct values for LogisticIndepBeta model", {
  model <- h_get_logistic_indep_beta(emptydata = FALSE)
  options <- h_get_mcmc_options()

  set.seed(10)
  result <- mcmc(data = model@data, model = model, options = options)
  expect_snapshot(result@data)
})

test_that("MCMC computes correct values for LogisticIndepBeta model (empty data)", {
  model <- h_get_logistic_indep_beta(emptydata = TRUE)
  options <- h_get_mcmc_options()

  set.seed(10)
  result <- mcmc(data = model@data, model = model, options = options)
  expect_snapshot(result@data)
})

# Effloglog ----

## constructor ----

test_that("Effloglog object can be created with user constructor (empty data)", {
  result <- expect_silent(h_get_eff_log_log(emptydata = TRUE))
  expect_valid(result, "Effloglog")
})

test_that("Effloglog object can be created with user constructor", {
  result <- expect_silent(h_get_eff_log_log(emptydata = FALSE))
  expect_valid(result, "Effloglog")
})

## mcmc ----

test_that("MCMC computes correct values for Effloglog model", {
  model <- h_get_eff_log_log(emptydata = FALSE)
  options <- h_get_mcmc_options()

  set.seed(10)
  result <- mcmc(data = model@data, model = model, options = options)
  expect_snapshot(result@data)
})

test_that("MCMC computes correct values for Effloglog model (empty data)", {
  model <- h_get_eff_log_log(emptydata = TRUE)
  options <- h_get_mcmc_options()

  set.seed(10)
  result <- mcmc(data = model@data, model = model, options = options)
  expect_snapshot(result@data)
})

# EffFlexi ----

## constructor ----

test_that("EffFlexi object can be created with user constructor", {
  result <- expect_silent(h_get_eff_flexi())
  expect_valid(result, "EffFlexi")
})

test_that("EffFlexi object can be created with user constructor (RW2)", {
  result <- expect_silent(h_get_eff_flexi(rw1 = FALSE))
  expect_valid(result, "EffFlexi")
})

test_that("EffFlexi object can be created with user constructor (empty data)", {
  result <- expect_silent(h_get_eff_flexi(emptydata = TRUE))
  expect_valid(result, "EffFlexi")
})

test_that("EffFlexi object can be created with user constructor (empty data, RW2)", {
  result <- expect_silent(h_get_eff_flexi(emptydata = TRUE, rw1 = FALSE))
  expect_valid(result, "EffFlexi")
})

## mcmc ----

test_that("MCMC computes correct values for EffFlexi model", {
  model <- h_get_eff_flexi()
  options <- h_get_mcmc_options()

  result <- mcmc(data = model@data, model = model, options = options)
  expect_snapshot(result@data)
})

test_that("MCMC computes correct values for EffFlexi model (RW2)", {
  model <- h_get_eff_flexi(rw1 = FALSE)
  options <- h_get_mcmc_options()

  result <- mcmc(data = model@data, model = model, options = options)
  expect_snapshot(result@data)
})

test_that("MCMC computes correct values for EffFlexi model (empty data)", {
  model <- h_get_eff_flexi(emptydata = TRUE)
  options <- h_get_mcmc_options()

  result <- mcmc(data = model@data, model = model, options = options)
  expect_snapshot(result@data)
})

test_that("MCMC computes correct values for EffFlexi model (empty data, RW2)", {
  model <- h_get_eff_flexi(emptydata = TRUE, rw1 = FALSE)
  options <- h_get_mcmc_options()

  result <- mcmc(data = model@data, model = model, options = options)
  expect_snapshot(result@data)
})

# DALogisticLogNormal ----

## constructor ----

test_that("DALogisticLogNormal object can be created with user constructor", {
  result <- expect_silent(
    DALogisticLogNormal(
      mean = c(0, 1),
      cov = diag(2),
      ref_dose = 1,
      npiece = 3,
      l = c(0.5, 0.5, 0.5),
      c_par = 2
    )
  )
  expect_valid(result, "DALogisticLogNormal")
})

test_that(".DefaultDALogisticLogNormal works as expected", {
  npiece <- 10
  Tmax <- 60

  lambda_prior <- function(k) {
    npiece / (Tmax * (npiece - k + 0.5))
  }

  test_obj <-
    expect_equal(
      .DefaultDALogisticLogNormal(),
      DALogisticLogNormal(
        mean = c(-0.85, 1),
        cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
        ref_dose = 56,
        npiece = npiece,
        l = as.numeric(t(apply(
          as.matrix(c(1:npiece), 1, npiece),
          2,
          lambda_prior
        ))),
        c_par = 2
      )
    )
})
## mcmc ----

test_that("MCMC computes correct values for DALogisticLogNormal model", {
  data <- h_get_data_da()
  model <- h_get_da_logistic_log_normal()
  options <- h_get_mcmc_options()

  result <- mcmc(data = data, model = model, options = options)
  expect_snapshot(result@data)
})

test_that("MCMC computes correct values for DALogisticLogNormal model and empty data", {
  data <- h_get_data_da(empty = TRUE)
  model <- h_get_da_logistic_log_normal()
  options <- h_get_mcmc_options()

  result <- mcmc(data = data, model = model, options = options)
  expect_snapshot(result@data)
})

# TITELogisticLogNormal ----

## constructor ----

test_that("TITELogisticLogNormal object can be created with user constructor (linear weight)", {
  result <- expect_silent(h_get_tite_logistic_log_normal("linear"))
  expect_valid(result, "TITELogisticLogNormal")
})

test_that("TITELogisticLogNormal object can be created with user constructor (adaptive weight)", {
  result <- expect_silent(h_get_tite_logistic_log_normal("adaptive"))
  expect_valid(result, "TITELogisticLogNormal")
})

test_that(".DefaultTITELogisticLogNormal object can be created with user constructor (linear weight)", {
  expect_equal(
    .DefaultTITELogisticLogNormal(),
    TITELogisticLogNormal(
      mean = c(0, 1),
      cov = diag(2),
      ref_dose = 1,
      weight_method = "linear"
    )
  )
})

## mcmc ----

test_that("MCMC computes correct values for TITELogisticLogNormal model (linear)", {
  data <- h_get_data_da()
  model <- h_get_tite_logistic_log_normal("linear")
  options <- h_get_mcmc_options()

  result <- mcmc(data = data, model = model, options = options)
  expect_snapshot(result@data)
})

test_that("MCMC computes correct values for TITELogisticLogNormal model (linear, data 2)", {
  data <- h_get_data_da_2()
  model <- h_get_tite_logistic_log_normal("linear")
  options <- h_get_mcmc_options()

  result <- mcmc(data = data, model = model, options = options)
  expect_snapshot(result@data)
})

test_that("MCMC computes correct values for TITELogisticLogNormal model (linear, empty data)", {
  data <- h_get_data_da(empty = TRUE)
  model <- h_get_tite_logistic_log_normal("linear")
  options <- h_get_mcmc_options()

  result <- mcmc(data = data, model = model, options = options)
  expect_snapshot(result@data)
})

test_that("MCMC computes correct values for TITELogisticLogNormal model (adaptive)", {
  data <- h_get_data_da()
  model <- h_get_tite_logistic_log_normal("adaptive")
  options <- h_get_mcmc_options()

  result <- mcmc(data = data, model = model, options = options)
  expect_snapshot(result@data)
})

test_that("MCMC computes correct values for TITELogisticLogNormal model (adaptive, data 2)", {
  data <- h_get_data_da_2()
  model <- h_get_tite_logistic_log_normal("adaptive")
  options <- h_get_mcmc_options()

  result <- mcmc(data = data, model = model, options = options)
  expect_snapshot(result@data)
})

test_that("MCMC computes correct values for TITELogisticLogNormal model (adaptive, empty data)", {
  data <- h_get_data_da(empty = TRUE)
  model <- h_get_tite_logistic_log_normal("adaptive")
  options <- h_get_mcmc_options()

  result <- mcmc(data = data, model = model, options = options)
  expect_snapshot(result@data)
})

# OneParLogNormalPrior ----

## constructor ----

test_that("OneParLogNormalPrior object can be created with user constructor", {
  result <- expect_silent(
    OneParLogNormalPrior(
      skel_probs = c(0.1, 0.3, 0.5, 0.7, 0.9),
      dose_grid = 1:5,
      sigma2 = 2
    )
  )
  expect_valid(result, "OneParLogNormalPrior")
  expect_identical(result@skel_probs, c(0.1, 0.3, 0.5, 0.7, 0.9))
  expect_identical(result@sigma2, 2)
  expect_identical(
    result@skel_fun(c(1, 1.5, 3, 3.7, 5)),
    c(0.10, 0.20, 0.50, 0.64, 0.90)
  )
  expect_identical(
    result@skel_fun_inv(c(0.10, 0.20, 0.50, 0.64, 0.90)),
    c(1, 1.5, 3, 3.7, 5)
  )
})

test_that("OneParLogNormalPrior throws the error when dose_grid and skel_probs have diff. lengths", {
  result <- expect_error(
    OneParLogNormalPrior(
      skel_probs = c(0.1, 0.3, 0.5, 0.7, 0.9),
      dose_grid = 1:6,
      sigma2 = 2
    ),
    "Assertion on 'dose_grid' failed: Must have length 5, but has length 6."
  )
})

test_that("OneParLogNormalPrior throws the error for not unique or not sorted dose_grid", {
  result <- expect_error(
    OneParLogNormalPrior(
      skel_probs = c(0.1, 0.3, 0.5, 0.7, 0.9),
      dose_grid = c(1, 3, 4, 5, 5),
      sigma2 = 2
    ),
    "Assertion on 'dose_grid' failed: Contains duplicated values, position 5."
  )
  result <- expect_error(
    OneParLogNormalPrior(
      skel_probs = c(0.1, 0.3, 0.5, 0.7, 0.9),
      dose_grid = c(2, 1, 3, 4, 5),
      sigma2 = 2
    ),
    "Assertion on 'dose_grid' failed: Must be sorted"
  )
})

test_that("OneParLogNormalPrior throws the error for not a probability values in skel_probs", {
  result <- expect_error(
    OneParLogNormalPrior(
      skel_probs = c(0.1, 0.3, 0.5, 0.7, 1.1),
      dose_grid = 1:5,
      sigma2 = 2
    ),
    "Assertion on 'skel_probs' failed: Probability must be within \\[0, 1\\] bounds but it is not"
  )
})

test_that("OneParLogNormalPrior throws the error for not unique or not sorted skel_probs", {
  result <- expect_error(
    OneParLogNormalPrior(
      skel_probs = c(0.1, 0.2, 0.2, 0.3, 0.4),
      dose_grid = 1:5,
      sigma2 = 2
    ),
    "Assertion on 'skel_probs' failed: Contains duplicated values, position 3."
  )
  result <- expect_error(
    OneParLogNormalPrior(
      skel_probs = c(0.3, 0.1, 0.5, 0.7, 0.9),
      dose_grid = 1:5,
      sigma2 = 2
    ),
    "Assertion on 'skel_probs' failed: Must be sorted"
  )
})

## mcmc ----

test_that("MCMC computes correct values for OneParLogNormalPrior model", {
  data <- h_get_data(placebo = FALSE)
  model <- h_get_one_par_log_normal_prior()
  options <- h_get_mcmc_options()

  result <- mcmc(data = data, model = model, options = options)
  expect_snapshot(result@data)
})

test_that("MCMC computes correct values for OneParLogNormalPrior model and empty data", {
  data <- h_get_data(empty = TRUE, placebo = FALSE)
  model <- h_get_one_par_log_normal_prior()
  options <- h_get_mcmc_options()

  result <- mcmc(data = data, model = model, options = options)
  expect_snapshot(result@data)
})

test_that("MCMC throws the error for OneParLogNormalPrior model when 'xLevel' does not match 'skel_probs'", {
  data <- h_get_data(placebo = FALSE)
  model <- h_get_one_par_log_normal_prior()
  model@skel_probs <- model@skel_probs[-1]
  options <- h_get_mcmc_options()

  expect_error(
    mcmc(data = data, model = model, options = options, from_prior = FALSE),
    "Assertion on 'length\\(model@skel_probs\\) == data@nGrid' failed: Must be TRUE."
  )
})

test_that("No NA is returned in dose calculations for OneParLogNormalPrior model", {
  model <- h_get_one_par_log_normal_prior()
  calc_dose <- doseFunction(model, alpha = 1)

  expect_false(is.na(calc_dose(0.95)))
})

# OneParExpPrior ----

## constructor ----

test_that("OneParExpPrior object can be created with user constructor", {
  result <- expect_silent(
    OneParExpPrior(
      skel_probs = c(0.1, 0.3, 0.5, 0.7, 0.9),
      dose_grid = 1:5,
      lambda = 2
    )
  )
  expect_valid(result, "OneParExpPrior")
  expect_identical(result@skel_probs, c(0.1, 0.3, 0.5, 0.7, 0.9))
  expect_identical(result@lambda, 2)
  expect_identical(
    result@skel_fun(c(1, 1.5, 3, 3.7, 5)),
    c(0.10, 0.20, 0.50, 0.64, 0.90)
  )
  expect_identical(
    result@skel_fun_inv(c(0.10, 0.20, 0.50, 0.64, 0.90)),
    c(1, 1.5, 3, 3.7, 5)
  )
})

test_that("OneParExpPrior throws the error when dose_grid and skel_probs have diff. lengths", {
  result <- expect_error(
    OneParExpPrior(
      skel_probs = c(0.1, 0.3, 0.5, 0.7, 0.9),
      dose_grid = 1:6,
      lambda = 2
    ),
    "Assertion on 'dose_grid' failed: Must have length 5, but has length 6."
  )
})

test_that("OneParExpPrior throws the error for not unique or not sorted dose_grid", {
  result <- expect_error(
    OneParExpPrior(
      skel_probs = c(0.1, 0.3, 0.5, 0.7, 0.9),
      dose_grid = c(1, 3, 4, 5, 5),
      lambda = 2
    ),
    "Assertion on 'dose_grid' failed: Contains duplicated values, position 5."
  )
  result <- expect_error(
    OneParExpPrior(
      skel_probs = c(0.1, 0.3, 0.5, 0.7, 0.9),
      dose_grid = c(2, 1, 3, 4, 5),
      lambda = 2
    ),
    "Assertion on 'dose_grid' failed: Must be sorted"
  )
})

test_that("OneParExpPrior throws the error for not a probability values in skel_probs", {
  result <- expect_error(
    OneParExpPrior(
      skel_probs = c(0.1, 0.3, 0.5, 0.7, 1.1),
      dose_grid = 1:5,
      lambda = 2
    ),
    "Assertion on 'skel_probs' failed: Probability must be within \\[0, 1\\] bounds but it is not"
  )
})

test_that("OneParExpPrior throws the error for not unique or not sorted skel_probs", {
  result <- expect_error(
    OneParExpPrior(
      skel_probs = c(0.1, 0.2, 0.2, 0.3, 0.4),
      dose_grid = 1:5,
      lambda = 2
    ),
    "Assertion on 'skel_probs' failed: Contains duplicated values, position 3."
  )
  result <- expect_error(
    OneParExpPrior(
      skel_probs = c(0.3, 0.1, 0.5, 0.7, 0.9),
      dose_grid = 1:5,
      lambda = 2
    ),
    "Assertion on 'skel_probs' failed: Must be sorted"
  )
})

## mcmc ----

test_that("MCMC computes correct values for OneParExpPrior model", {
  data <- h_get_data(placebo = FALSE)
  model <- h_get_one_par_exp_prior()
  options <- h_get_mcmc_options()

  result <- mcmc(data = data, model = model, options = options)
  expect_snapshot(result@data)
})

test_that("MCMC computes correct values for OneParExpPrior model and empty data", {
  data <- h_get_data(empty = TRUE, placebo = FALSE)
  model <- h_get_one_par_exp_prior()
  options <- h_get_mcmc_options()

  result <- mcmc(data = data, model = model, options = options)
  expect_snapshot(result@data)
})

test_that("MCMC throws the error for OneParExpPrior model when 'xLevel' does not match 'skel_probs'", {
  data <- h_get_data(placebo = FALSE)
  model <- h_get_one_par_exp_prior()
  model@skel_probs <- model@skel_probs[-1]
  options <- h_get_mcmc_options()

  expect_error(
    mcmc(data = data, model = model, options = options, from_prior = FALSE),
    "Assertion on 'length\\(model@skel_probs\\) == data@nGrid' failed: Must be TRUE."
  )
})

test_that("No NA is returned in dose calculations for OneParExpPrior model", {
  model <- h_get_one_par_exp_prior()
  calc_dose <- doseFunction(model, theta = 1)

  expect_false(is.na(calc_dose(0.95)))
})

# FractionalCRM ----

## constructor ----

test_that("FractionalCRM object can be created with user constructor", {
  result <- expect_silent(
    FractionalCRM(
      skel_probs = c(0.1, 0.2, 0.3, 0.4),
      dose_grid = c(10, 30, 50, 100),
      sigma2 = 2
    )
  )
  expect_valid(result, "FractionalCRM")
})

test_that(".DefaultFractionalCRM works correctly", {
  expect_equal(
    .DefaultFractionalCRM(),
    FractionalCRM(
      skel_probs = c(0.1, 0.2, 0.3, 0.4),
      dose_grid = c(10, 30, 50, 100),
      sigma2 = 2
    )
  )
})

## mcmc ----

test_that("MCMC computes correct values for FractionalCRM model", {
  data <- h_get_data_da(placebo = FALSE)
  model <- h_get_fractional_crm()
  options <- h_get_mcmc_options()

  result <- mcmc(data = data, model = model, options = options)
  expect_snapshot(result@data)
})

test_that("MCMC computes correct values for FractionalCRM model and empty data", {
  data <- h_get_data_da(empty = TRUE, placebo = FALSE)
  model <- h_get_fractional_crm()
  options <- h_get_mcmc_options()

  result <- mcmc(data = data, model = model, options = options)
  expect_snapshot(result@data)
})

test_that("MCMC throws the error for FractionalCRM model when 'xLevel' does not match 'skel_probs'", {
  data <- h_get_data(placebo = FALSE)
  model <- h_get_fractional_crm()
  model@skel_probs <- model@skel_probs[-1]
  options <- h_get_mcmc_options()

  expect_error(
    mcmc(data = data, model = model, options = options, from_prior = FALSE),
    "Assertion on 'length\\(model@skel_probs\\) == data@nGrid' failed: Must be TRUE."
  )
})
