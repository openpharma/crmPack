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
      dose = function(x, param1) {},
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
      ref_dose = positive_number(1),
      dose = function(x, param1) {},
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

test_that("MCMC computes correct values for LogisticNormal model and empty data", {
  empty_data <- h_get_data(empty = TRUE)
  model <- h_get_logistic_normal()
  options <- h_get_mcmc_options(small = TRUE, fixed = TRUE)

  result <- mcmc(data = empty_data, model = model, options = options)
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

test_that("MCMC computes correct values for LogisticLogNormal model and empty data", {
  empty_data <- h_get_data(empty = TRUE)
  model <- h_get_logistic_log_normal()
  options <- h_get_mcmc_options(small = TRUE, fixed = TRUE)

  result <- mcmc(data = empty_data, model = model, options = options)
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

test_that("MCMC computes correct values for LogisticLogNormalSub model and empty data", {
  empty_data <- h_get_data(empty = TRUE)
  model <- h_get_logistic_log_normal_sub()
  options <- h_get_mcmc_options(small = TRUE, fixed = TRUE)

  result <- mcmc(data = empty_data, model = model, options = options)
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

test_that("MCMC computes correct values for ProbitLogNormal model and empty data", {
  empty_data <- h_get_data(empty = TRUE)
  model <- h_get_probit_log_normal()
  options <- h_get_mcmc_options(small = TRUE, fixed = TRUE)

  result <- mcmc(data = empty_data, model = model, options = options)
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

test_that("MCMC computes correct values for ProbitLogNormalRel model and empty data", {
  empty_data <- h_get_data(empty = TRUE)
  model <- h_get_probit_log_normal_rel()
  options <- h_get_mcmc_options(small = TRUE, fixed = TRUE)

  result <- mcmc(data = empty_data, model = model, options = options)
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

test_that("MCMC computes correct values for LogisticKadane model and empty data", {
  empty_data <- h_get_data(empty = TRUE)
  model <- h_get_logistic_kadane()
  options <- h_get_mcmc_options(small = TRUE, fixed = TRUE)

  result <- mcmc(data = empty_data, model = model, options = options)
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
  data <- h_get_data_2()
  model <- h_get_logistic_kadane_beta_gam()
  options <- h_get_mcmc_options(small = TRUE, fixed = TRUE)

  result <- mcmc(data = data, model = model, options = options)
  expect_snapshot(result@data)
})

test_that("MCMC computes correct values for LogisticKadaneBetaGamma model and empty data", {
  empty_data <- h_get_data(empty = TRUE)
  model <- h_get_logistic_kadane_beta_gam()
  options <- h_get_mcmc_options(small = TRUE, fixed = TRUE)

  result <- mcmc(data = empty_data, model = model, options = options)
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

test_that("MCMC computes correct values for LogisticNormalMixture model and empty data", {
  empty_data <- h_get_data_mixture(empty = TRUE)
  model <- h_get_logistic_normal_mix()
  options <- h_get_mcmc_options(small = TRUE, fixed = TRUE)

  result <- mcmc(data = empty_data, model = model, options = options)
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
  model_log_normal <- h_get_logistic_normal_fixed_mix(log_normal = TRUE)
  options <- h_get_mcmc_options(small = TRUE, fixed = TRUE)

  result <- mcmc(data = data, model = model, options = options)
  result_log_normal <- mcmc(data = data, model = model_log_normal, options = options)
  expect_snapshot(result@data)
  expect_snapshot(result_log_normal@data)
})

test_that("MCMC computes correct values for LogisticNormalFixedMixture model and empty data", {
  empty_data <- h_get_data_mixture(empty = TRUE)
  model <- h_get_logistic_normal_fixed_mix()
  model_log_normal <- h_get_logistic_normal_fixed_mix(log_normal = TRUE)
  options <- h_get_mcmc_options(small = TRUE, fixed = TRUE)

  result <- mcmc(data = empty_data, model = model, options = options)
  result_log_normal <- mcmc(data = empty_data, model = model_log_normal, options = options)
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

## mcmc ----

test_that("MCMC computes correct values for LogisticLogNormalMixture model", {
  data <- h_get_data_mixture()
  model <- h_get_logistic_log_normal_mix()
  options <- h_get_mcmc_options(small = TRUE, fixed = TRUE)

  result <- mcmc(data = data, model = model, options = options)
  expect_snapshot(result@data)
})

test_that("MCMC computes correct values for LogisticLogNormalMixture model and empty data", {
  empty_data <- h_get_data_mixture(empty = TRUE)
  model <- h_get_logistic_log_normal_mix()
  options <- h_get_mcmc_options(small = TRUE, fixed = TRUE)

  result <- mcmc(data = empty_data, model = model, options = options)
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
  model_log_dose <- h_get_dual_endpoint_rw(use_log_dose = TRUE)
  options <- h_get_mcmc_options(small = TRUE, fixed = TRUE)

  result <- mcmc(data = data, model = model, options = options)
  result_log_dose <- mcmc(data = data, model = model_log_dose, options = options)
  expect_snapshot(result@data)
  expect_snapshot(result_log_dose@data)
})

test_that("MCMC computes correct values for DualEndpointRW model", {
  data <- h_get_data_dual()
  model <- h_get_dual_endpoint_rw(fixed = FALSE)
  model_log_dose <- h_get_dual_endpoint_rw(use_log_dose = TRUE, fixed = FALSE)
  options <- h_get_mcmc_options(small = TRUE, fixed = TRUE)

  result <- mcmc(data = data, model = model, options = options)
  result_log_dose <- mcmc(data = data, model = model_log_dose, options = options)
  expect_snapshot(result@data)
  expect_snapshot(result_log_dose@data)
})

test_that("MCMC computes correct values for DualEndpointRW model with RW2", {
  data <- h_get_data_dual()
  model <- h_get_dual_endpoint_rw(rw1 = FALSE)
  model_log_dose <- h_get_dual_endpoint_rw(use_log_dose = TRUE, rw1 = FALSE)
  options <- h_get_mcmc_options(small = TRUE, fixed = TRUE)

  result <- mcmc(data = data, model = model, options = options)
  result_log_dose <- mcmc(data = data, model = model_log_dose, options = options)
  expect_snapshot(result@data)
  expect_snapshot(result_log_dose@data)
})

test_that("MCMC computes correct values for DualEndpointRW model (fixed params) with RW2", {
  data <- h_get_data_dual()
  model <- h_get_dual_endpoint_rw(rw1 = FALSE, fixed = FALSE)
  model_log_dose <- h_get_dual_endpoint_rw(use_log_dose = TRUE, rw1 = FALSE, fixed = FALSE)
  options <- h_get_mcmc_options(small = TRUE, fixed = TRUE)

  result <- mcmc(data = data, model = model, options = options)
  result_log_dose <- mcmc(data = data, model = model_log_dose, options = options)
  expect_snapshot(result@data)
  expect_snapshot(result_log_dose@data)
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
  model_log_dose <- h_get_dual_endpoint_beta(use_log_dose = TRUE)
  options <- h_get_mcmc_options(small = TRUE, fixed = TRUE)

  result <- mcmc(data = data, model = model, options = options)
  result_log_dose <- mcmc(data = data, model = model_log_dose, options = options)
  expect_snapshot(result@data)
  expect_snapshot(result_log_dose@data)
})

test_that("MCMC computes correct values for DualEndpointBeta model", {
  data <- h_get_data_dual()
  model <- h_get_dual_endpoint_beta(fixed = FALSE)
  model_log_dose <- h_get_dual_endpoint_beta(use_log_dose = TRUE, fixed = FALSE)
  options <- h_get_mcmc_options(small = TRUE, fixed = TRUE)

  result <- mcmc(data = data, model = model, options = options)
  result_log_dose <- mcmc(data = data, model = model_log_dose, options = options)
  expect_snapshot(result@data)
  expect_snapshot(result_log_dose@data)
})

test_that("MCMC throws the error for DualEndpointBeta model when 'ref_dose_beta <= max(doseGrid)'", {
  data <- h_get_data_dual()
  model <- h_get_dual_endpoint_beta()
  options <- h_get_mcmc_options(small = TRUE, fixed = TRUE)

  model@ref_dose_beta <- positive_number(data@doseGrid[data@nGrid] - 1)
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
  model_log_dose <- h_get_dual_endpoint_emax(use_log_dose = TRUE)
  options <- h_get_mcmc_options(small = TRUE, fixed = TRUE)

  result <- mcmc(data = data, model = model, options = options)
  result_log_dose <- mcmc(data = data, model = model_log_dose, options = options)
  expect_snapshot(result@data)
  expect_snapshot(result_log_dose@data)
})

test_that("MCMC computes correct values for DualEndpointEmax model", {
  data <- h_get_data_dual()
  model <- h_get_dual_endpoint_emax(fixed = FALSE)
  model_log_dose <- h_get_dual_endpoint_emax(use_log_dose = TRUE, fixed = FALSE)
  options <- h_get_mcmc_options(small = TRUE, fixed = TRUE)

  result <- mcmc(data = data, model = model, options = options)
  result_log_dose <- mcmc(data = data, model = model_log_dose, options = options)
  expect_snapshot(result@data)
  expect_snapshot(result_log_dose@data)
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
  options <- h_get_mcmc_options(small = TRUE, fixed = TRUE)

  set.seed(10)
  result <- mcmc(data = model@data, model = model, options = options)
  expect_snapshot(result@data)
})

test_that("MCMC computes correct values for LogisticIndepBeta model (empty data)", {
  model <- h_get_logistic_indep_beta(emptydata = TRUE)
  options <- h_get_mcmc_options(small = TRUE, fixed = TRUE)

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
  options <- h_get_mcmc_options(small = TRUE, fixed = TRUE)

  set.seed(10)
  result <- mcmc(data = model@data, model = model, options = options)
  expect_snapshot(result@data)
})

test_that("MCMC computes correct values for Effloglog model (empty data)", {
  model <- h_get_eff_log_log(emptydata = TRUE)
  options <- h_get_mcmc_options(small = TRUE, fixed = TRUE)

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
  options <- h_get_mcmc_options(small = TRUE, fixed = TRUE)

  result <- mcmc(data = model@data, model = model, options = options)
  expect_snapshot(result@data)
})

test_that("MCMC computes correct values for EffFlexi model (RW2)", {
  model <- h_get_eff_flexi(rw1 = FALSE)
  options <- h_get_mcmc_options(small = TRUE, fixed = TRUE)

  result <- mcmc(data = model@data, model = model, options = options)
  expect_snapshot(result@data)
})

test_that("MCMC computes correct values for EffFlexi model (empty data)", {
  model <- h_get_eff_flexi(emptydata = TRUE)
  options <- h_get_mcmc_options(small = TRUE, fixed = TRUE)

  result <- mcmc(data = model@data, model = model, options = options)
  expect_snapshot(result@data)
})

test_that("MCMC computes correct values for EffFlexi model (empty data, RW2)", {
  model <- h_get_eff_flexi(emptydata = TRUE, rw1 = FALSE)
  options <- h_get_mcmc_options(small = TRUE, fixed = TRUE)

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

## mcmc ----

test_that("MCMC computes correct values for DALogisticLogNormal model", {
  data <- h_get_data_da()
  model <- h_get_da_logistic_log_normal()
  options <- h_get_mcmc_options(small = TRUE, fixed = TRUE)

  result <- mcmc(data = data, model = model, options = options)
  expect_snapshot(result@data)
})

test_that("MCMC computes correct values for DALogisticLogNormal model and empty data", {
  empty_data <- h_get_data_da(empty = TRUE)
  model <- h_get_da_logistic_log_normal()
  options <- h_get_mcmc_options(small = TRUE, fixed = TRUE)

  result <- mcmc(data = empty_data, model = model, options = options)
  expect_snapshot(result@data)
})
