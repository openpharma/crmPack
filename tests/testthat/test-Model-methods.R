# doseFunction ----

## GeneralModel ----

test_that("doseFunction-GeneralModel returns correct dose function", {
  model <- h_get_logistic_log_normal()
  samples <- h_as_samples(list(alpha0 = 1, alpha1 = 2), burnin = 10000, fixed = FALSE)
  dose_args <- c("x", "model", "samples")

  dose_fun <- doseFunction(model, alpha0 = 1, alpha1 = 2)
  dose_fun_dose_args <- as.character(body(dose_fun)[[2]][-1])
  dose_fun_env <- environment(dose_fun)

  expect_function(dose_fun, args = "x", nargs = 1, null.ok = FALSE)
  expect_equal(dose_fun_dose_args, dose_args)
  expect_subset(
    setdiff(dose_fun_dose_args, "x"),
    ls(envir = dose_fun_env)
  )
  expect_identical(dose_fun_env[["model"]], model)
  expect_identical(dose_fun_env[["samples"]], samples)
})

test_that("doseFunction-GeneralModel throws the error when valid params are not provided", {
  model <- h_get_logistic_log_normal()

  expect_error(
    doseFunction(model),
    "Assertion on .* failed: Must be a subset of \\{'alpha0','alpha1'\\}, not empty.$"
  )
  expect_error(
    doseFunction(model, wrong = 1, alpha1 = 2),
    "Assertion on .* failed: Must be a subset of \\{'alpha0','alpha1'\\}, but .* \\{'wrong'\\}.$"
  )
})

## ModelPseudo ----

test_that("doseFunction-ModelPseudo returns correct dose function", {
  model <- h_get_logistic_indep_beta()
  samples <- h_as_samples(list(phi1 = 35, phi2 = 5), burnin = 10000, fixed = FALSE)
  dose_args <- c("x", "model", "samples")

  dose_fun <- doseFunction(model, phi1 = 35, phi2 = 5)
  dose_fun_dose_args <- as.character(body(dose_fun)[[2]][-1])
  dose_fun_env <- environment(dose_fun)

  expect_function(dose_fun, args = "x", nargs = 1, null.ok = FALSE)
  expect_equal(dose_fun_dose_args, dose_args)
  expect_subset(
    setdiff(dose_fun_dose_args, "x"),
    ls(envir = dose_fun_env)
  )
  expect_identical(dose_fun_env[["model"]], model)
  expect_identical(dose_fun_env[["samples"]], samples)
})

test_that("doseFunction-ModelPseudo throws the error when no params are provided", {
  model <- h_get_logistic_indep_beta()

  expect_error(
    doseFunction(model),
    "Assertion on .* failed: Must be of type 'character', not 'NULL'.$"
  )
})

# probFunction ----

## GeneralModel ----

test_that("probFunction-GeneralModel returns correct prob function", {
  model <- h_get_logistic_log_normal()
  samples <- h_as_samples(list(alpha0 = 1, alpha1 = 2), burnin = 10000, fixed = FALSE)
  prob_args <- c("dose", "model", "samples")

  prob_fun <- probFunction(model, alpha0 = 1, alpha1 = 2)
  prob_fun_prob_args <- as.character(body(prob_fun)[[2]][-1])
  prob_fun_env <- environment(prob_fun)

  expect_function(prob_fun, args = "dose", nargs = 1, null.ok = FALSE)
  expect_equal(prob_fun_prob_args, prob_args)
  expect_subset(
    setdiff(prob_fun_prob_args, "dose"),
    ls(envir = prob_fun_env)
  )
  expect_identical(prob_fun_env[["model"]], model)
  expect_identical(prob_fun_env[["samples"]], samples)
})

test_that("probFunction-GeneralModel throws the error when valid params are not provided", {
  model <- h_get_logistic_log_normal()

  expect_error(
    probFunction(model),
    "Assertion on .* failed: Must be a subset of \\{'alpha0','alpha1'\\}, not empty.$"
  )
  expect_error(
    probFunction(model, wrong = 1, alpha1 = 2),
    "Assertion on .* failed: Must be a subset of \\{'alpha0','alpha1'\\}, but .* \\{'wrong'\\}.$"
  )
})

## ModelTox ----

test_that("probFunction-ModelTox returns correct prob function", {
  model <- h_get_logistic_indep_beta()
  samples <- h_as_samples(list(phi1 = 35, phi2 = 5), burnin = 10000, fixed = FALSE)
  prob_args <- c("dose", "model", "samples")

  prob_fun <- probFunction(model, phi1 = 35, phi2 = 5)
  prob_fun_prob_args <- as.character(body(prob_fun)[[2]][-1])
  prob_fun_env <- environment(prob_fun)

  expect_function(prob_fun, args = "dose", nargs = 1, null.ok = FALSE)
  expect_equal(prob_fun_prob_args, prob_args)
  expect_subset(
    setdiff(prob_fun_prob_args, "dose"),
    ls(envir = prob_fun_env)
  )
  expect_identical(prob_fun_env[["model"]], model)
  expect_identical(prob_fun_env[["samples"]], samples)
})

test_that("probFunction-ModelTox throws the error when no params are provided", {
  model <- h_get_logistic_indep_beta()

  expect_error(
    probFunction(model),
    "Assertion on .* failed: Must be of type 'character', not 'NULL'.$"
  )
})

# efficacyFunction ----

## ModelEff ----

test_that("efficacyFunction-ModelEff returns correct efficacy function", {
  model <- h_get_eff_log_log()
  samples <- h_as_samples(list(theta1 = -4.8, theta2 = 3.7), burnin = 10000, fixed = FALSE)
  eff_args <- c("dose", "model", "samples")

  eff_fun <- efficacyFunction(model, theta1 = -4.8, theta2 = 3.7)
  eff_fun_eff_args <- as.character(body(eff_fun)[[2]][-1])
  eff_fun_env <- environment(eff_fun)

  expect_function(eff_fun, args = "dose", nargs = 1, null.ok = FALSE)
  expect_equal(eff_fun_eff_args, eff_args)
  expect_subset(
    setdiff(eff_fun_eff_args, "dose"),
    ls(envir = eff_fun_env)
  )
  expect_identical(eff_fun_env[["model"]], model)
  expect_identical(eff_fun_env[["samples"]], samples)
})

test_that("efficacyFunction-ModelEff throws the error when no params are provided", {
  model <- h_get_eff_log_log()

  expect_error(
    efficacyFunction(model),
    "Assertion on .* failed: Must be of type 'character', not 'NULL'.$"
  )
})

# dose ----

## LogisticNormal ----

test_that("dose-LogisticNormal works as expected", {
  model <- h_get_logistic_normal()
  samples <- h_as_samples(list(alpha0 = c(0, -1, 1, 2), alpha1 = c(0, 2, 1, -1)))

  result <- dose(0.4, model, samples)
  expect_snapshot(result)
})

test_that("dose-LogisticNormal works as expected for scalar samples", {
  model <- h_get_logistic_normal()
  samples <- h_as_samples(list(alpha0 = 5, alpha1 = 10))

  result <- dose(c(0.3, 0.6), model, samples)
  expect_snapshot(result)
})

test_that("dose-LogisticNormal throws the error when x is not a valid scalar", {
  model <- h_get_logistic_normal()
  samples <- h_as_samples(list(alpha0 = c(5, 6), alpha1 = c(10, 11)))
  expect_error(
    dose(c(0.4, 0.6), model, samples),
    "Assertion on 'x' failed: Must have length 1."
  )
  expect_error(
    dose(2, model, samples),
    "Assertion on 'x' failed: Element 1 is not <= 1."
  )
  expect_error(
    dose(-2, model, samples),
    "Assertion on 'x' failed: Element 1 is not >= 0."
  )
})

## LogisticLogNormal ----

test_that("dose-LogisticLogNormal works as expected", {
  model <- h_get_logistic_log_normal()
  samples <- h_as_samples(list(alpha0 = c(0, -1, 1, 2), alpha1 = c(0, 2, 1, -1)))

  result <- dose(0.4, model, samples)
  expect_snapshot(result)
})

test_that("dose-LogisticLogNormal works as expected for scalar samples", {
  model <- h_get_logistic_log_normal()
  samples <- h_as_samples(list(alpha0 = 5, alpha1 = 10))

  result <- dose(c(0.3, 0.7), model, samples)
  expect_snapshot(result)
})

test_that("dose-LogisticLogNormal throws the error when x is not a valid scalar", {
  model <- h_get_logistic_log_normal()
  samples <- h_as_samples(list(alpha0 = c(5, 6), alpha1 = c(10, 11)))

  expect_error(
    dose(c(40, 50), model, samples),
    "Assertion on 'x' failed: Must have length 1."
  )
  expect_error(
    dose(2, model, samples),
    "Assertion on 'x' failed: Element 1 is not <= 1."
  )
  expect_error(
    dose(-2, model, samples),
    "Assertion on 'x' failed: Element 1 is not >= 0."
  )
})

## LogisticLogNormalSub ----

test_that("dose-LogisticLogNormalSub works as expected", {
  model <- h_get_logistic_log_normal_sub()
  samples <- h_as_samples(list(alpha0 = c(0, -1, 1, 2), alpha1 = c(0, 2, 1, -1)))

  result <- dose(0.4, model, samples)
  expect_snapshot(result)
})

test_that("dose-LogisticLogNormalSub works as expected for scalar samples", {
  model <- h_get_logistic_log_normal_sub()
  samples <- h_as_samples(list(alpha0 = 5, alpha1 = 10))

  result <- dose(c(0.3, 0.7), model, samples)
  expect_snapshot(result)
})

test_that("dose-LogisticLogNormalSub throws the error when x is not a valid scalar", {
  model <- h_get_logistic_log_normal_sub()
  samples <- h_as_samples(list(alpha0 = c(5, 6), alpha1 = c(10, 11)))

  expect_error(
    dose(c(40, 50), model, samples),
    "Assertion on 'x' failed: Must have length 1."
  )
  expect_error(
    dose(2, model, samples),
    "Assertion on 'x' failed: Element 1 is not <= 1."
  )
  expect_error(
    dose(-2, model, samples),
    "Assertion on 'x' failed: Element 1 is not >= 0."
  )
})

## ProbitLogNormal ----

test_that("dose-ProbitLogNormal works as expected", {
  model <- h_get_probit_log_normal()
  samples <- h_as_samples(list(alpha0 = c(0, -1, 1, 2), alpha1 = c(0, 2, 1, -1)))

  result <- dose(0.4, model, samples)
  expect_snapshot(result)
})

test_that("dose-ProbitLogNormal works as expected for scalar samples", {
  model <- h_get_probit_log_normal()
  samples <- h_as_samples(list(alpha0 = 5, alpha1 = 10))

  result <- dose(c(0.3, 0.7), model, samples)
  expect_snapshot(result)
})

test_that("dose-ProbitLogNormal throws the error when x is not a valid scalar", {
  model <- h_get_probit_log_normal()
  samples <- h_as_samples(list(alpha0 = c(5, 6), alpha1 = c(10, 11)))

  expect_error(
    dose(c(40, 50), model, samples),
    "Assertion on 'x' failed: Must have length 1."
  )
  expect_error(
    dose(2, model, samples),
    "Assertion on 'x' failed: Element 1 is not <= 1."
  )
  expect_error(
    dose(-2, model, samples),
    "Assertion on 'x' failed: Element 1 is not >= 0."
  )
})

## ProbitLogNormalRel ----

test_that("dose-ProbitLogNormalRel works as expected", {
  model <- h_get_probit_log_normal_rel()
  samples <- h_as_samples(list(alpha0 = c(0, -1, 1, 2), alpha1 = c(0, 2, 1, -1)))

  result <- dose(0.4, model, samples)
  expect_snapshot(result)
})

test_that("dose-ProbitLogNormalRel works as expected for scalar samples", {
  model <- h_get_probit_log_normal_rel()
  samples <- h_as_samples(list(alpha0 = 5, alpha1 = 10))

  result <- dose(c(0.3, 0.7), model, samples)
  expect_snapshot(result)
})

test_that("dose-ProbitLogNormalRel throws the error when x is not a valid scalar", {
  model <- h_get_probit_log_normal_rel()
  samples <- h_as_samples(list(alpha0 = c(5, 6), alpha1 = c(10, 11)))

  expect_error(
    dose(c(40, 50), model, samples),
    "Assertion on 'x' failed: Must have length 1."
  )
  expect_error(
    dose(2, model, samples),
    "Assertion on 'x' failed: Element 1 is not <= 1."
  )
  expect_error(
    dose(-2, model, samples),
    "Assertion on 'x' failed: Element 1 is not >= 0."
  )
})

## LogisticKadane ----

test_that("dose-LogisticKadane works as expected", {
  model <- h_get_logistic_kadane()
  samples <- h_as_samples(list(rho0 = c(0.1, 0.2, 0.3), gamma = c(10, 40, 80)))

  result <- dose(0.2, model, samples)
  expect_snapshot(result)
})

test_that("dose-LogisticKadane works as expected for scalar samples", {
  model <- h_get_logistic_kadane()
  samples <- h_as_samples(list(rho0 = 0.15, gamma = 50))

  result <- dose(c(0.3, 0.7), model, samples)
  expect_snapshot(result)
})

test_that("dose-LogisticKadane throws the error when x is not a valid scalar", {
  model <- h_get_logistic_kadane()
  samples <- h_as_samples(list(rho0 = c(0.1, 0.2), gamma = c(10, 40)))

  expect_error(
    dose(c(40, 50), model, samples),
    "Assertion on 'x' failed: Must have length 1."
  )
  expect_error(
    dose(2, model, samples),
    "Assertion on 'x' failed: Element 1 is not <= 1."
  )
  expect_error(
    dose(-2, model, samples),
    "Assertion on 'x' failed: Element 1 is not >= 0."
  )
})


## LogisticKadaneBetaGamma ----

test_that("dose-LogisticKadaneBetaGamma works as expected", {
  model <- h_get_logistic_kadane_beta_gam()
  samples <- h_as_samples(
    list(rho0 = c(0.05, 0.1, 0.15), gamma = c(3, 7, 10))
  )

  result <- dose(0.2, model, samples)
  expect_snapshot(result)
})

test_that("dose-LogisticKadaneBetaGamma works as expected for scalar samples", {
  model <- h_get_logistic_kadane_beta_gam()
  samples <- h_as_samples(list(rho0 = 0.15, gamma = 50))

  result <- dose(c(0.3, 0.7), model, samples)
  expect_snapshot(result)
})

test_that("dose-LogisticKadaneBetaGamma throws the error when x is not a valid scalar", {
  model <- h_get_logistic_kadane_beta_gam()
  samples <- h_as_samples(list(rho0 = c(0.1, 0.2), gamma = c(10, 40)))

  expect_error(
    dose(c(40, 50), model, samples),
    "Assertion on 'x' failed: Must have length 1."
  )
  expect_error(
    dose(2, model, samples),
    "Assertion on 'x' failed: Element 1 is not <= 1."
  )
  expect_error(
    dose(-2, model, samples),
    "Assertion on 'x' failed: Element 1 is not >= 0."
  )
})

## LogisticNormalMixture ----

test_that("dose-LogisticNormalMixture works as expected", {
  model <- h_get_logistic_normal_mix()
  samples <- h_as_samples(list(alpha0 = c(0, -1, 1, 2), alpha1 = c(0, 2, 1, -1)))

  result <- dose(0.2, model, samples)
  expect_snapshot(result)
})

test_that("dose-LogisticNormalMixture works as expected for scalar samples", {
  model <- h_get_logistic_normal_mix()
  samples <- h_as_samples(list(alpha0 = 5, alpha1 = 10))

  result <- dose(c(0.3, 0.7), model, samples)
  expect_snapshot(result)
})

test_that("dose-LogisticNormalMixture throws the error when x is not a valid scalar", {
  model <- h_get_logistic_normal_mix()
  samples <- h_as_samples(list(alpha0 = c(5, 6), alpha1 = c(10, 11)))

  expect_error(
    dose(c(40, 50), model, samples),
    "Assertion on 'x' failed: Must have length 1."
  )
  expect_error(
    dose(2, model, samples),
    "Assertion on 'x' failed: Element 1 is not <= 1."
  )
  expect_error(
    dose(-2, model, samples),
    "Assertion on 'x' failed: Element 1 is not >= 0."
  )
})

## LogisticNormalFixedMixture ----

test_that("dose-LogisticNormalFixedMixture works as expected", {
  model <- h_get_logistic_normal_fixed_mix()
  samples <- h_as_samples(list(alpha0 = c(0, -1, 1, 2), alpha1 = c(0, 2, 1, -1)))

  result <- dose(0.2, model, samples)
  expect_snapshot(result)
})

test_that("dose-LogisticNormalFixedMixture works as expected for scalar samples", {
  model <- h_get_logistic_normal_fixed_mix()
  samples <- h_as_samples(list(alpha0 = 5, alpha1 = 10))

  result <- dose(c(0.3, 0.7), model, samples)
  expect_snapshot(result)
})

test_that("dose-LogisticNormalFixedMixture throws the error when x is not a valid scalar", {
  model <- h_get_logistic_normal_fixed_mix()
  samples <- h_as_samples(list(alpha0 = c(5, 6), alpha1 = c(10, 11)))

  expect_error(
    dose(c(40, 50), model, samples),
    "Assertion on 'x' failed: Must have length 1."
  )
  expect_error(
    dose(2, model, samples),
    "Assertion on 'x' failed: Element 1 is not <= 1."
  )
  expect_error(
    dose(-2, model, samples),
    "Assertion on 'x' failed: Element 1 is not >= 0."
  )
})

## LogisticLogNormalMixture ----

test_that("dose-LogisticLogNormalMixture is not implemented", {
  model <- h_get_logistic_log_normal_mix()
  samples <- h_as_samples(list(alpha0 = c(0, -1, 1, 2), alpha1 = c(0, 2, 1, -1)))
  expect_error(
    dose(2, model, samples),
    "not implemented"
  )
})

## DualEndpoint ----

test_that("dose-DualEndpoint works as expected", {
  model <- h_get_dual_endpoint()
  model_log_dose <- h_get_dual_endpoint(use_log_dose = TRUE)
  betaZ <- matrix(c(0.4, -0.2, 0.5, 0.9, -1.3, 0.1, 0.24, -1.03), ncol = 2) # nolintr
  samples <- h_as_samples(list(betaZ = betaZ))

  result <- dose(0.2, model, samples)
  result_log_dose <- dose(0.2, model_log_dose, samples)
  expect_false(identical(result, result_log_dose))
  expect_snapshot(result)
  expect_snapshot(result_log_dose)
})

test_that("dose-DualEndpoint works as expected for scalar samples", {
  model <- h_get_dual_endpoint()
  model_log_dose <- h_get_dual_endpoint(use_log_dose = TRUE)
  samples <- h_as_samples(list(betaZ = matrix(c(0.4, -0.2), ncol = 2)))

  result <- dose(c(0.3, 0.7), model, samples)
  result_log_dose <- dose(c(0.3, 0.7), model_log_dose, samples)
  expect_false(identical(result, result_log_dose))
  expect_snapshot(result)
  expect_snapshot(result_log_dose)
})

test_that("dose-DualEndpoint throws the error when x is not a valid scalar", {
  model <- h_get_dual_endpoint()
  samples <- h_as_samples(list(betaZ = matrix(c(0.4, -0.2, 0.5, 0.9), ncol = 2)))

  expect_error(
    dose(c(40, 50), model, samples),
    "Assertion on 'x' failed: Must have length 1."
  )
  expect_error(
    dose(2, model, samples),
    "Assertion on 'x' failed: Element 1 is not <= 1."
  )
  expect_error(
    dose(-2, model, samples),
    "Assertion on 'x' failed: Element 1 is not >= 0."
  )
})

## LogisticIndepBeta ----

test_that("dose-LogisticIndepBeta works as expected", {
  dlt_model_emptydat <- h_get_logistic_indep_beta(emptydata = TRUE)
  dlt_model <- h_get_logistic_indep_beta(emptydata = FALSE)

  samples <- h_as_samples(
    list(
      phi1 = seq(from = -1.96, to = 1.96, length = 5),
      phi2 = seq(from = -1.96, to = 1.96, length = 5)
    )
  )

  result_emptydat <- dose(0.45, dlt_model_emptydat, samples)
  result <- dose(0.45, dlt_model, samples)

  expect_snapshot(result_emptydat)
  expect_snapshot(result)
})

test_that("dose-LogisticIndepBeta works as expected for scalar samples", {
  dlt_model <- h_get_logistic_indep_beta()
  samples <- h_as_samples(list(phi1 = -1, phi2 = 1))

  result <- dose(c(0.45, 0.7), dlt_model, samples)
  expect_snapshot(result)
})

test_that("dose-LogisticIndepBeta throws the error when x is not a valid scalar", {
  dlt_model <- h_get_logistic_indep_beta()
  samples <- h_as_samples(list(phi1 = c(-1, 0.5), phi2 = c(1, 0.6)))

  expect_error(
    dose(c(40, 50), dlt_model, samples),
    "Assertion on 'x' failed: Must have length 1."
  )
  expect_error(
    dose(2, dlt_model, samples),
    "Assertion on 'x' failed: Element 1 is not <= 1."
  )
  expect_error(
    dose(-2, dlt_model, samples),
    "Assertion on 'x' failed: Element 1 is not >= 0."
  )
})

## LogisticIndepBeta-noSamples ----

test_that("dose-LogisticIndepBeta-noSamples works as expected", {
  dlt_model_emptydat <- h_get_logistic_indep_beta(emptydata = TRUE)
  dlt_model <- h_get_logistic_indep_beta(emptydata = FALSE)

  result_emptydat <- dose(c(0.45, 0.55), dlt_model_emptydat)
  result <- dose(c(0.45, 0.55), dlt_model)

  expect_snapshot(result_emptydat)
  expect_snapshot(result)
})

test_that("dose-LogisticIndepBeta-noSamples throws the error when x is not a valid scalar", {
  dlt_model <- h_get_logistic_indep_beta()

  expect_error(
    dose(2, dlt_model),
    "Assertion on 'x' failed: Element 1 is not <= 1."
  )
  expect_error(
    dose(-2, dlt_model),
    "Assertion on 'x' failed: Element 1 is not >= 0."
  )
})

## OneParExpNormalPrior ----

test_that("dose-OneParExpNormalPrior works as expected", {
  model <- h_get_one_par_exp_normal_prior()
  samples <- h_as_samples(list(alpha = c(0, 0.5, 1, 2)))

  result <- dose(0.4, model, samples)
  expect_snapshot(result)
})

test_that("dose-OneParExpNormalPrior works as expected for scalar samples", {
  model <- h_get_one_par_exp_normal_prior()
  samples <- h_as_samples(list(alpha = 1))

  result <- dose(c(0.3, 0.7), model, samples)
  expect_snapshot(result)
})

test_that("dose-OneParExpNormalPrior throws the error when x is not a valid scalar", {
  model <- h_get_one_par_exp_normal_prior()
  samples <- h_as_samples(list(alpha = c(1, 2)))

  expect_error(
    dose(c(40, 50), model, samples),
    "Assertion on 'x' failed: Must have length 1."
  )
  expect_error(
    dose(2, model, samples),
    "Assertion on 'x' failed: Element 1 is not <= 1."
  )
  expect_error(
    dose(-2, model, samples),
    "Assertion on 'x' failed: Element 1 is not >= 0."
  )
})

# prob ----

## LogisticNormal ----

test_that("prob-LogisticNormal works as expected", {
  model <- h_get_logistic_normal()
  samples <- h_as_samples(list(alpha0 = c(0, -1, 1, 2), alpha1 = c(0, 2, 1, -1)))

  result <- prob(60, model, samples)
  expect_snapshot(result)
})

test_that("prob-LogisticNormal works as expected for scalar samples", {
  model <- h_get_logistic_normal()
  samples <- h_as_samples(list(alpha0 = 5, alpha1 = 10))

  result <- prob(c(20, 60), model, samples)
  expect_snapshot(result)
})

test_that("prob-LogisticNormal throws the error when dose is not a valid scalar", {
  model <- h_get_logistic_normal()
  samples <- h_as_samples(list(alpha0 = c(5, 6), alpha1 = c(10, 11)))

  expect_error(
    prob(c(40, 50), model, samples),
    "Assertion on 'dose' failed: Must have length 1."
  )
  expect_error(
    prob(-3, model, samples),
    "Assertion on 'dose' failed: Element 1 is not >= 0."
  )
})

## LogisticLogNormal ----

test_that("prob-LogisticLogNormal works as expected", {
  model <- h_get_logistic_log_normal()
  samples <- h_as_samples(list(alpha0 = c(0, -1, 1, 2), alpha1 = c(0, 2, 1, -1)))

  result <- prob(60, model, samples)
  expect_snapshot(result)
})

test_that("prob-LogisticLogNormal works as expected for scalar samples", {
  model <- h_get_logistic_log_normal()
  samples <- h_as_samples(list(alpha0 = 5, alpha1 = 10))

  result <- prob(c(20, 60), model, samples)
  expect_snapshot(result)
})

test_that("prob-LogisticLogNormal throws the error when dose is not a valid scalar", {
  model <- h_get_logistic_log_normal()
  samples <- h_as_samples(list(alpha0 = c(5, 6), alpha1 = c(10, 11)))

  expect_error(
    prob(c(40, 50), model, samples),
    "Assertion on 'dose' failed: Must have length 1."
  )
  expect_error(
    prob(-3, model, samples),
    "Assertion on 'dose' failed: Element 1 is not >= 0."
  )
})

## LogisticLogNormalSub ----

test_that("prob-LogisticLogNormalSub works as expected", {
  model <- h_get_logistic_log_normal_sub()
  samples <- h_as_samples(list(alpha0 = c(0, -1, 1, 2), alpha1 = c(0, 2, 1, -1)))

  result <- prob(4, model, samples)
  expect_snapshot(result)
})

test_that("prob-LogisticLogNormalSub works as expected for scalar samples", {
  model <- h_get_logistic_log_normal_sub()
  samples <- h_as_samples(list(alpha0 = 2, alpha1 = 0.5))

  result <- prob(c(4, 10), model, samples)
  expect_snapshot(result)
})

test_that("prob-LogisticLogNormalSub throws the error when dose is not a valid scalar", {
  model <- h_get_logistic_log_normal_sub()
  samples <- h_as_samples(list(alpha0 = c(5, 6), alpha1 = c(10, 11)))

  expect_error(
    prob(c(40, 50), model, samples),
    "Assertion on 'dose' failed: Must have length 1."
  )
  expect_error(
    prob(-3, model, samples),
    "Assertion on 'dose' failed: Element 1 is not >= 0."
  )
})

## ProbitLogNormal ----

test_that("prob-ProbitLogNormal works as expected", {
  model <- h_get_probit_log_normal()
  samples <- h_as_samples(list(alpha0 = c(0, -1, 1, 2), alpha1 = c(0, 2, 1, -1)))

  result <- prob(4, model, samples)
  expect_snapshot(result)
})

test_that("prob-ProbitLogNormal works as expected for scalar samples", {
  model <- h_get_probit_log_normal()
  samples <- h_as_samples(list(alpha0 = 2, alpha1 = 0.5))

  result <- prob(c(4, 10), model, samples)
  expect_snapshot(result)
})

test_that("prob-ProbitLogNormal throws the error when dose is not a valid scalar", {
  model <- h_get_probit_log_normal()
  samples <- h_as_samples(list(alpha0 = c(5, 6), alpha1 = c(10, 11)))

  expect_error(
    prob(c(40, 50), model, samples),
    "Assertion on 'dose' failed: Must have length 1."
  )
  expect_error(
    prob(-3, model, samples),
    "Assertion on 'dose' failed: Element 1 is not >= 0."
  )
})

## ProbitLogNormalRel ----

test_that("prob-ProbitLogNormalRel works as expected", {
  model <- h_get_probit_log_normal_rel()
  samples <- h_as_samples(list(alpha0 = c(0, -1, 1, 2), alpha1 = c(0, 2, 1, -1)))

  result <- prob(4, model, samples)
  expect_snapshot(result)
})

test_that("prob-ProbitLogNormalRel works as expected for scalar samples", {
  model <- h_get_probit_log_normal_rel()
  samples <- h_as_samples(list(alpha0 = 2, alpha1 = 0.5))

  result <- prob(c(2, 2.5), model, samples)
  expect_snapshot(result)
})

test_that("prob-ProbitLogNormalRel throws the error when dose is not a valid scalar", {
  model <- h_get_probit_log_normal_rel()
  samples <- h_as_samples(list(alpha0 = c(5, 6), alpha1 = c(10, 11)))

  expect_error(
    prob(c(40, 50), model, samples),
    "Assertion on 'dose' failed: Must have length 1."
  )
  expect_error(
    prob(-3, model, samples),
    "Assertion on 'dose' failed: Element 1 is not >= 0."
  )
})

## LogisticKadane ----

test_that("prob-LogisticKadane works as expected", {
  model <- h_get_logistic_kadane()
  samples <- h_as_samples(list(rho0 = c(0.1, 0.2, 0.3), gamma = c(10, 40, 80)))

  result <- prob(4, model, samples)
  expect_snapshot(result)
})

test_that("prob-LogisticKadane works as expected for scalar samples", {
  model <- h_get_logistic_kadane()
  samples <- h_as_samples(list(rho0 = 0.15, gamma = 50))

  result <- prob(c(2, 2.5), model, samples)
  expect_snapshot(result)
})

test_that("prob-LogisticKadane throws the error when dose is not a valid scalar", {
  model <- h_get_logistic_kadane()
  samples <- h_as_samples(list(rho0 = c(0.1, 0.2), gamma = c(10, 40)))

  expect_error(
    prob(c(40, 50), model, samples),
    "Assertion on 'dose' failed: Must have length 1."
  )
  expect_error(
    prob(-3, model, samples),
    "Assertion on 'dose' failed: Element 1 is not >= 0."
  )
})

## LogisticNormalMixture ----

test_that("prob-LogisticNormalMixture works as expected", {
  model <- h_get_logistic_normal_mix()
  samples <- h_as_samples(list(alpha0 = c(0, -1, 1, 2), alpha1 = c(0, 2, 1, -1)))

  result <- prob(60, model, samples)
  expect_snapshot(result)
})

test_that("prob-LogisticNormalMixture works as expected for scalar samples", {
  model <- h_get_logistic_normal_mix()
  samples <- h_as_samples(list(alpha0 = 5, alpha1 = 10))

  result <- prob(c(1, 1.5), model, samples)
  expect_snapshot(result)
})

test_that("prob-LogisticNormalMixture throws the error when dose is not a valid scalar", {
  model <- h_get_logistic_normal_mix()
  samples <- h_as_samples(list(alpha0 = c(5, 6), alpha1 = c(10, 11)))

  expect_error(
    prob(c(40, 50), model, samples),
    "Assertion on 'dose' failed: Must have length 1."
  )
  expect_error(
    prob(-3, model, samples),
    "Assertion on 'dose' failed: Element 1 is not >= 0."
  )
})

## LogisticNormalFixedMixture ----

test_that("prob-LogisticNormalFixedMixture works as expected", {
  model <- h_get_logistic_normal_fixed_mix()
  samples <- h_as_samples(list(alpha0 = c(0, -1, 1, 2), alpha1 = c(0, 2, 1, -1)))

  result <- prob(60, model, samples)
  expect_snapshot(result)
})

test_that("prob-LogisticNormalFixedMixture works as expected for scalar samples", {
  model <- h_get_logistic_normal_fixed_mix()
  samples <- h_as_samples(list(alpha0 = 5, alpha1 = 10))

  result <- prob(c(30, 45), model, samples)
  expect_snapshot(result)
})

test_that("prob-LogisticNormalFixedMixture throws the error when dose is not a valid scalar", {
  model <- h_get_logistic_normal_fixed_mix()
  samples <- h_as_samples(list(alpha0 = c(5, 6), alpha1 = c(10, 11)))

  expect_error(
    prob(c(40, 50), model, samples),
    "Assertion on 'dose' failed: Must have length 1."
  )
  expect_error(
    prob(-3, model, samples),
    "Assertion on 'dose' failed: Element 1 is not >= 0."
  )
})

## LogisticLogNormalMixture ----

test_that("prob-LogisticLogNormalMixture works as expected", {
  model <- h_get_logistic_log_normal_mix()
  samples <- h_as_samples(
    list(
      alpha0 = matrix(
        c(-0.93, -0.67, -0.94, -0.67, -2.37, -1.28, -2.37, -1.08),
        ncol = 2,
        byrow = TRUE
      ),
      alpha1 = matrix(
        c(0.45, 0.75, 0.45, 0.75, 0.4, 1.18, 0.4, 0.63),
        ncol = 2,
        byrow = TRUE
      ),
      comp = c(1, 1, 1, 1)
    )
  )

  result <- prob(60, model, samples)
  expect_snapshot(result)
})

test_that("prob-LogisticLogNormalMixture works as expected for single samples", {
  model <- h_get_logistic_log_normal_mix()
  samples <- h_as_samples(
    list(
      alpha0 = matrix(c(-0.93, -0.67), ncol = 2),
      alpha1 = matrix(c(0.45, 0.75), ncol = 2),
      comp = 1
    )
  )
  result <- prob(c(1, 1.5), model, samples)
  expect_snapshot(result)
})

test_that("prob-LogisticLogNormalMixture throws the error when dose is not a valid scalar", {
  model <- h_get_logistic_log_normal_mix()
  samples <- h_as_samples(
    list(
      alpha0 = matrix(
        c(-0.93, -0.67, -0.94, -0.67, -2.37, -1.28, -2.37, -1.08),
        ncol = 2,
        byrow = TRUE
      ),
      alpha1 = matrix(
        c(0.45, 0.75, 0.45, 0.75, 0.4, 1.18, 0.4, 0.63),
        ncol = 2,
        byrow = TRUE
      ),
      comp = c(1, 1, 1, 1)
    )
  )

  expect_error(
    prob(c(40, 50), model, samples),
    "Assertion on 'dose' failed: Must have length 1."
  )
  expect_error(
    prob(-3, model, samples),
    "Assertion on 'dose' failed: Element 1 is not >= 0."
  )
})

## DualEndpoint ----

test_that("prob-DualEndpoint works as expected", {
  model <- h_get_dual_endpoint()
  model_log_dose <- h_get_dual_endpoint(use_log_dose = TRUE)
  betaZ <- matrix(c(0.4, -0.6, 0.5, 0.09, -0.3, 0.1, 0.24, -1.03), ncol = 2) # nolintr
  samples <- h_as_samples(list(betaZ = betaZ))

  result <- prob(5, model, samples)
  result_log_dose <- prob(5, model_log_dose, samples)
  expect_false(identical(result, result_log_dose))
  expect_snapshot(result)
  expect_snapshot(result_log_dose)
})

test_that("prob-DualEndpoint works as expected for scalar samples", {
  model <- h_get_dual_endpoint()
  model_log_dose <- h_get_dual_endpoint(use_log_dose = TRUE)
  samples <- h_as_samples(list(betaZ = matrix(c(0.4, -0.6), ncol = 2)))

  result <- prob(c(5, 8), model, samples)
  result_log_dose <- prob(c(5, 8), model_log_dose, samples)
  expect_false(identical(result, result_log_dose))
  expect_snapshot(result)
  expect_snapshot(result_log_dose)
})

test_that("prob-DualEndpoint throws the error when dose is not a valid scalar", {
  model <- h_get_dual_endpoint()
  samples <- h_as_samples(list(betaZ = matrix(c(0.4, -0.2, 0.5, 0.9), ncol = 2)))

  expect_error(
    prob(c(40, 50), model, samples),
    "Assertion on 'dose' failed: Must have length 1."
  )
  expect_error(
    prob(-3, model, samples),
    "Assertion on 'dose' failed: Element 1 is not >= 0."
  )
})

## LogisticIndepBeta ----

test_that("prob-LogisticIndepBeta works as expected", {
  dlt_model_emptydat <- h_get_logistic_indep_beta(emptydata = TRUE)
  dlt_model <- h_get_logistic_indep_beta(emptydata = FALSE)

  samples <- h_as_samples(
    list(
      phi1 = seq(from = -1.96, to = 1.96, length = 5),
      phi2 = seq(from = -1.96, to = 1.96, length = 5)
    )
  )

  result_emptydat <- prob(100, dlt_model_emptydat, samples)
  result <- prob(100, dlt_model, samples)

  expect_snapshot(result_emptydat)
  expect_snapshot(result)
})

test_that("prob-LogisticIndepBeta works as expected for scalar samples", {
  dlt_model <- h_get_logistic_indep_beta()
  samples <- h_as_samples(list(phi1 = -1, phi2 = 1))

  result <- prob(c(6, 15), dlt_model, samples)
  expect_snapshot(result)
})

test_that("prob-LogisticIndepBeta throws the error when dose is not a valid scalar", {
  dlt_model <- h_get_logistic_indep_beta()
  samples <- h_as_samples(list(phi1 = c(-1, 0.5), phi2 = c(1, 0.6)))

  expect_error(
    prob(c(40, 50), dlt_model, samples),
    "Assertion on 'dose' failed: Must have length 1."
  )
  expect_error(
    prob(-3, dlt_model, samples),
    "Assertion on 'dose' failed: Element 1 is not >= 0."
  )
})

## LogisticIndepBeta-noSamples ----

test_that("prob-LogisticIndepBeta-noSamples works as expected", {
  dlt_model_emptydat <- h_get_logistic_indep_beta(emptydata = TRUE)
  dlt_model <- h_get_logistic_indep_beta(emptydata = FALSE)

  result_emptydat <- prob(c(500, 1000), dlt_model_emptydat)
  result <- prob(c(500, 1000), dlt_model)

  expect_snapshot(result_emptydat)
  expect_snapshot(result)
})

test_that("prob-LogisticIndepBeta-noSamples throws the error when dose is not a valid scalar", {
  dlt_model <- h_get_logistic_indep_beta()

  expect_error(
    prob(-3, dlt_model),
    "Assertion on 'dose' failed: Element 1 is not >= 0."
  )
})

## OneParExpNormalPrior ----

test_that("prob-OneParExpNormalPrior works as expected", {
  model <- h_get_one_par_exp_normal_prior()
  samples <- h_as_samples(list(alpha = c(0, 0.5, 1, 2)))

  result <- prob(60, model, samples)
  expect_snapshot(result)
})

test_that("prob-OneParExpNormalPrior works as expected for scalar samples", {
  model <- h_get_one_par_exp_normal_prior()
  samples <- h_as_samples(list(alpha = 1))

  result <- prob(c(20, 60), model, samples)
  expect_snapshot(result)
})

test_that("prob-OneParExpNormalPrior throws the error when dose is not a valid scalar", {
  model <- h_get_one_par_exp_normal_prior()
  samples <- h_as_samples(list(alpha = c(1, 2)))

  expect_error(
    prob(c(40, 50), model, samples),
    "Assertion on 'dose' failed: Must have length 1."
  )
  expect_error(
    prob(-3, model, samples),
    "Assertion on 'dose' failed: Element 1 is not >= 0."
  )
})

# biomarker ----

## DualEndpoint ----

test_that("biomarker-DualEndpoint works as expected", {
  beta_w <- matrix(c(0.54, 0.61, 0.44, 0.62, 0.66, 0.41, 0.7, 0.56), nrow = 4)
  model <- h_get_dual_endpoint()
  samples <- h_as_samples(list(betaW = beta_w))

  result <- biomarker(xLevel = 2L, model, samples)
  expect_identical(result, beta_w[, 2])
})

test_that("biomarker-DualEndpoint works as expected for xLevel vector", {
  beta_w <- matrix(c(0.54, 0.61, 0.44, 0.62, 0.66, 0.41, 0.7, 0.56), nrow = 4)
  model <- h_get_dual_endpoint()
  samples <- h_as_samples(list(betaW = beta_w))

  result <- biomarker(xLevel = 1:2, model, samples)
  expect_identical(result, beta_w)
})

test_that("biomarker-DualEndpoint throws the error when xLevel is not valid", {
  beta_w <- matrix(c(0.54, 0.61, 0.44, 0.62, 0.66, 0.41, 0.7, 0.56), nrow = 4)
  model <- h_get_dual_endpoint()
  samples <- h_as_samples(list(betaW = beta_w))

  expect_error(
    biomarker(xLevel = 1.5, model, samples),
    "unable to find an inherited method for function 'biomarker' *"
  )
})

# gain ----

## ModelTox-ModelEff ----

test_that("gain-ModelTox-ModelEff works as expected", {
  model_dle <- h_get_logistic_indep_beta(emptydata = TRUE)
  samples_dle <- h_as_samples(
    list(phi1 = c(1.72, -1.45, -4.52, -1.54), phi2 = c(0.17, 0.79, -0.11, 0.06))
  )
  model_eff <- h_get_eff_log_log(emptydata = TRUE)
  samples_eff <- h_as_samples(
    list(
      theta1 = c(-1.08, -0.87, -1.91, -1.51),
      theta2 = c(1.93, 1.51, 2, 1.73),
      nu = c(6.48, 63.36, 2.14, 20.75)
    )
  )

  result <- gain(dose = 75, model_dle, samples_dle, model_eff, samples_eff)
  expect_snapshot(result)
})

test_that("gain-ModelTox-ModelEff works as expected for scalar samples", {
  model_dle <- h_get_logistic_indep_beta(emptydata = TRUE)
  samples_dle <- h_as_samples(list(phi1 = 1.72, phi2 = 0.17))
  model_eff <- h_get_eff_log_log(emptydata = TRUE)
  samples_eff <- h_as_samples(list(theta1 = -1.08, theta2 = 1.93, nu = 6.48))

  result <- gain(dose = c(50, 75), model_dle, samples_dle, model_eff, samples_eff)
  expect_equal(result, c(0.1325413, 0.1388810), tolerance = 10e-7)
})

test_that("gain-ModelTox-ModelEff throws the error when dose is not a valid scalar", {
  model_dle <- h_get_logistic_indep_beta(emptydata = TRUE)
  samples_dle <- h_as_samples(list(phi1 = c(1.72, -1.45), phi2 = c(0.17, 0.79)))
  model_eff <- h_get_eff_log_log(emptydata = TRUE)
  samples_eff <- h_as_samples(
    list(theta1 = c(-1.08, -0.87), theta2 = c(1.93, 1.51), nu = c(6.48, 63.36))
  )

  expect_error(
    gain(dose = c(50, 75), model_dle, samples_dle, model_eff, samples_eff),
    "Assertion on 'dose' failed: Must have length 1, but has length 2."
  )
})

## ModelTox-Effloglog-noSamples ----

test_that("gain-ModelTox-Effloglog-noSamples works as expected", {
  model_dle <- h_get_logistic_indep_beta(emptydata = FALSE)
  model_eff <- h_get_eff_log_log(emptydata = TRUE)

  result <- gain(dose = 75, model_dle = model_dle, model_eff = model_eff)
  expect_equal(result, 1.034771, tolerance = 10e-7)
})

test_that("gain-ModelTox-Effloglog-noSamples works as expected for vector dose", {
  model_dle <- h_get_logistic_indep_beta(emptydata = FALSE)
  model_eff <- h_get_eff_log_log(emptydata = TRUE)

  result <- gain(dose = c(50, 75), model_dle = model_dle, model_eff = model_eff)
  expect_equal(result, c(1.090325, 1.034771), tolerance = 10e-7)
})
