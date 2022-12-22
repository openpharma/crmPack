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

test_that("doseFunction-GeneralModel returns correct dose function for matrix param", {
  model <- h_get_logistic_log_normal_mix()
  params <- list(
    alpha0 = matrix(c(-0.94, -0.94, -2.37, -2.37, -0.67, -0.67, -1.28, -1.08), nrow = 4),
    alpha1 = matrix(c(0.45, 0.45, 0.40, 0.40, 0.75, 0.75, 1.18, 0.63), nrow = 4),
    comp = c(1, 1, 1, 1)
  )
  samples <- h_as_samples(params, burnin = 10000, fixed = FALSE)
  dose_args <- c("x", "model", "samples")

  dose_fun <- doseFunction(model, alpha0 = params$alpha0, alpha1 = params$alpha1, comp = params$comp)
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

test_that("probFunction-GeneralModel returns correct prob function for matrix param", {
  model <- h_get_logistic_log_normal_mix()
  params <- list(
    alpha0 = matrix(c(-0.94, -0.94, -2.37, -2.37, -0.67, -0.67, -1.28, -1.08), nrow = 4),
    alpha1 = matrix(c(0.45, 0.45, 0.40, 0.40, 0.75, 0.75, 1.18, 0.63), nrow = 4),
    comp = c(1, 1, 1, 1)
  )
  samples <- h_as_samples(params, burnin = 10000, fixed = FALSE)
  prob_args <- c("dose", "model", "samples")

  prob_fun <- probFunction(model, alpha0 = params$alpha0, alpha1 = params$alpha1, comp = params$comp)
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
  expect_equal(result, c(0, 67.30876, 12.26265, 554.17921), tolerance = 1e-7)
})

test_that("dose-LogisticNormal works as expected for scalar samples", {
  model <- h_get_logistic_normal()
  samples <- h_as_samples(list(alpha0 = 5, alpha1 = 10))

  result <- dose(c(0.3, 0.6), model, samples)
  expect_equal(result, c(27.86282, 31.581441), tolerance = 1e-7)
})

test_that("dose-LogisticNormal works as expected for vectors", {
  model <- h_get_logistic_normal()
  samples <- h_as_samples(list(alpha0 = c(5, 6), alpha1 = c(10, 11)))

  result <- dose(c(0.4, 0.6), model, samples)
  expect_equal(result, c(29.12149, 30.06702), tolerance = 1e-7)
})

test_that("dose-LogisticNormal throws the error when x and samples lengths differ", {
  model <- h_get_logistic_normal()
  samples <- h_as_samples(list(alpha0 = c(5, 6), alpha1 = c(10, 11)))
  expect_error(
    dose(c(0.4, 0.6, 0.5), model, samples),
    "Assertion on 'x' failed: x is of length 3 which is not allowed; the allowed lengths are: 1 or 2."
  )
  expect_error(
    dose(2, model, samples),
    "Assertion on 'x' failed: Probability must be within \\[0, 1\\] bounds but it is not."
  )
  expect_error(
    dose(-2, model, samples),
    "Assertion on 'x' failed: Probability must be within \\[0, 1\\] bounds but it is not."
  )
})

## LogisticLogNormal ----

test_that("dose-LogisticLogNormal works as expected", {
  model <- h_get_logistic_log_normal()
  samples <- h_as_samples(list(alpha0 = c(0, -1, 1, 2), alpha1 = c(0, 2, 1, -1)))

  result <- dose(0.4, model, samples)
  expect_equal(result, c(0, 67.30876, 12.26265, 554.17921), tolerance = 1e-7)
})

test_that("dose-LogisticLogNormal works as expected for scalar samples", {
  model <- h_get_logistic_log_normal()
  samples <- h_as_samples(list(alpha0 = 5, alpha1 = 10))

  result <- dose(c(0.3, 0.7), model, samples)
  expect_equal(result, c(27.86282, 33.00809), tolerance = 1e-7)
})

test_that("dose-LogisticLogNormal works as expected for vectors", {
  model <- h_get_logistic_log_normal()
  samples <- h_as_samples(list(alpha0 = c(5, 6), alpha1 = c(10, 11)))

  result <- dose(c(0.4, 0.75), model, samples)
  expect_equal(result, c(29.12149, 32.02261), tolerance = 1e-7)
})

test_that("dose-LogisticLogNormal throws the error when x is not valid", {
  model <- h_get_logistic_log_normal()
  samples <- h_as_samples(list(alpha0 = c(5, 6), alpha1 = c(10, 11)))

  expect_error(
    dose(c(0.4, 0.6, 0.5), model, samples),
    "Assertion on 'x' failed: x is of length 3 which is not allowed; the allowed lengths are: 1 or 2."
  )
  expect_error(
    dose(2, model, samples),
    "Assertion on 'x' failed: Probability must be within \\[0, 1\\] bounds but it is not."
  )
  expect_error(
    dose(-2, model, samples),
    "Assertion on 'x' failed: Probability must be within \\[0, 1\\] bounds but it is not."
  )
})

## LogisticLogNormalSub ----

test_that("dose-LogisticLogNormalSub works as expected", {
  model <- h_get_logistic_log_normal_sub()
  samples <- h_as_samples(list(alpha0 = c(0, -1, 1, 2), alpha1 = c(0, 2, 1, -1)))

  result <- dose(0.4, model, samples)
  expect_equal(result, c(-Inf, 2.2972674, 0.5945349, 4.4054651), tolerance = 1e-7)
})

test_that("dose-LogisticLogNormalSub works as expected for scalar samples", {
  model <- h_get_logistic_log_normal_sub()
  samples <- h_as_samples(list(alpha0 = 5, alpha1 = 10))

  result <- dose(c(0.3, 0.7), model, samples)
  expect_equal(result, c(1.41527, 1.58473), tolerance = 1e-6)
})

test_that("dose-LogisticLogNormalSub works as expected for vectors", {
  model <- h_get_logistic_log_normal_sub()
  samples <- h_as_samples(list(alpha0 = c(5, 6), alpha1 = c(10, 11)))

  result <- dose(c(0.3, 0.7), model, samples)
  expect_equal(result, c(1.415270, 1.531573), tolerance = 1e-6)
})

test_that("dose-LogisticLogNormalSub throws the error when x is not valid", {
  model <- h_get_logistic_log_normal_sub()
  samples <- h_as_samples(list(alpha0 = c(5, 6), alpha1 = c(10, 11)))

  expect_error(
    dose(c(0.4, 0.6, 0.5), model, samples),
    "Assertion on 'x' failed: x is of length 3 which is not allowed; the allowed lengths are: 1 or 2."
  )
  expect_error(
    dose(2, model, samples),
    "Assertion on 'x' failed: Probability must be within \\[0, 1\\] bounds but it is not."
  )
  expect_error(
    dose(-2, model, samples),
    "Assertion on 'x' failed: Probability must be within \\[0, 1\\] bounds but it is not."
  )
})

## ProbitLogNormal ----

test_that("dose-ProbitLogNormal works as expected", {
  model <- h_get_probit_log_normal()
  samples <- h_as_samples(list(alpha0 = c(0, -1, 1, 2), alpha1 = c(0, 2, 1, -1)))

  result <- dose(0.4, model, samples)
  expect_equal(result, c(0, 10.458421, 2.055942, 68.540727), tolerance = 1e-7)
})

test_that("dose-ProbitLogNormal works as expected for scalar samples", {
  model <- h_get_probit_log_normal()
  samples <- h_as_samples(list(alpha0 = 5, alpha1 = 10))

  result <- dose(c(0.3, 0.7), model, samples)
  expect_equal(result, c(4.143915, 4.602138), tolerance = 1e-7)
})

test_that("dose-ProbitLogNormal works as expected for vectors", {
  model <- h_get_probit_log_normal()
  samples <- h_as_samples(list(alpha0 = c(5, 6), alpha1 = c(10, 11)))

  result <- dose(c(0.3, 0.7), model, samples)
  expect_equal(result, c(4.143915, 4.376719), tolerance = 1e-7)
})

test_that("dose-ProbitLogNormal throws the error when x is not valid", {
  model <- h_get_probit_log_normal()
  samples <- h_as_samples(list(alpha0 = c(5, 6), alpha1 = c(10, 11)))

  expect_error(
    dose(c(0.4, 0.6, 0.5), model, samples),
    "Assertion on 'x' failed: x is of length 3 which is not allowed; the allowed lengths are: 1 or 2."
  )
  expect_error(
    dose(2, model, samples),
    "Assertion on 'x' failed: Probability must be within \\[0, 1\\] bounds but it is not."
  )
  expect_error(
    dose(-2, model, samples),
    "Assertion on 'x' failed: Probability must be within \\[0, 1\\] bounds but it is not."
  )
})

## ProbitLogNormalRel ----

test_that("dose-ProbitLogNormalRel works as expected", {
  model <- h_get_probit_log_normal_rel()
  samples <- h_as_samples(list(alpha0 = c(0, -1, 1, 2), alpha1 = c(0, 2, 1, -1)))

  result <- dose(0.4, model, samples)
  expect_equal(result, c(-Inf, 0.7466529, -2.5066942, 4.5066942), tolerance = 1e-7)
})

test_that("dose-ProbitLogNormalRel works as expected for scalar samples", {
  model <- h_get_probit_log_normal_rel()
  samples <- h_as_samples(list(alpha0 = 5, alpha1 = 10))

  result <- dose(c(0.3, 0.7), model, samples)
  expect_equal(result, c(-1.1048801, -0.8951199), tolerance = 1e-7)
})

test_that("dose-ProbitLogNormalRel works as expected for vectors", {
  model <- h_get_probit_log_normal_rel()
  samples <- h_as_samples(list(alpha0 = c(5, 6), alpha1 = c(10, 11)))

  result <- dose(c(0.3, 0.7), model, samples)
  expect_equal(result, c(-1.1048801, -0.9955635), tolerance = 1e-7)
})

test_that("dose-ProbitLogNormalRel throws the error when x is not valid", {
  model <- h_get_probit_log_normal_rel()
  samples <- h_as_samples(list(alpha0 = c(5, 6), alpha1 = c(10, 11)))

  expect_error(
    dose(c(0.4, 0.6, 0.5), model, samples),
    "Assertion on 'x' failed: x is of length 3 which is not allowed; the allowed lengths are: 1 or 2."
  )
  expect_error(
    dose(2, model, samples),
    "Assertion on 'x' failed: Probability must be within \\[0, 1\\] bounds but it is not."
  )
  expect_error(
    dose(-2, model, samples),
    "Assertion on 'x' failed: Probability must be within \\[0, 1\\] bounds but it is not."
  )
})

## LogisticKadane ----

test_that("dose-LogisticKadane works as expected", {
  model <- h_get_logistic_kadane()
  samples <- h_as_samples(list(rho0 = c(0.1, 0.2, 0.3), gamma = c(10, 40, 80)))

  result <- dose(0.2, model, samples)
  expect_equal(result, c(5.901396, 1, -305.087742), tolerance = 1e-7)
})

test_that("dose-LogisticKadane works as expected for scalar samples", {
  model <- h_get_logistic_kadane()
  samples <- h_as_samples(list(rho0 = 0.15, gamma = 50))

  result <- dose(c(0.3, 0.7), model, samples)
  expect_equal(result, c(43.3589, 124.2571), tolerance = 1e-7)
})

test_that("dose-LogisticKadane works as expected for vectors", {
  model <- h_get_logistic_kadane()
  samples <- h_as_samples(list(rho0 = c(0.1, 0.2), gamma = c(10, 40)))

  result <- dose(c(0.3, 0.7), model, samples)
  expect_equal(result, c(9.159179, 129.460259), tolerance = 1e-7)
})

test_that("dose-LogisticKadane throws the error when x is not valid", {
  model <- h_get_logistic_kadane()
  samples <- h_as_samples(list(rho0 = c(0.1, 0.2), gamma = c(10, 40)))

  expect_error(
    dose(c(0.4, 0.6, 0.5), model, samples),
    "Assertion on 'x' failed: x is of length 3 which is not allowed; the allowed lengths are: 1 or 2."
  )
  expect_error(
    dose(2, model, samples),
    "Assertion on 'x' failed: Probability must be within \\[0, 1\\] bounds but it is not."
  )
  expect_error(
    dose(-2, model, samples),
    "Assertion on 'x' failed: Probability must be within \\[0, 1\\] bounds but it is not."
  )
})

## LogisticKadaneBetaGamma ----

test_that("dose-LogisticKadaneBetaGamma works as expected", {
  model <- h_get_logistic_kadane_beta_gam()
  samples <- h_as_samples(
    list(rho0 = c(0.05, 0.1, 0.15), gamma = c(3, 7, 10))
  )

  result <- dose(0.2, model, samples)
  expect_equal(result, c(2.228955, 4.205052, 3.925453), tolerance = 1e-7)
})

test_that("dose-LogisticKadaneBetaGamma works as expected for scalar samples", {
  model <- h_get_logistic_kadane_beta_gam()
  samples <- h_as_samples(list(rho0 = 0.15, gamma = 50))

  result <- dose(c(0.3, 0.7), model, samples)
  expect_equal(result, c(50, 145.4914), tolerance = 1e-6)
})

test_that("dose-LogisticKadaneBetaGamma works as expected for vectors", {
  model <- h_get_logistic_kadane_beta_gam()
  samples <- h_as_samples(list(rho0 = c(0.1, 0.2), gamma = c(10, 40)))

  result <- dose(c(0.3, 0.7), model, samples)
  expect_equal(result, c(10, 165.7593), tolerance = 1e-7)
})

test_that("dose-LogisticKadaneBetaGamma throws the error when x is not valid", {
  model <- h_get_logistic_kadane_beta_gam()
  samples <- h_as_samples(list(rho0 = c(0.1, 0.2), gamma = c(10, 40)))

  expect_error(
    dose(c(0.4, 0.6, 0.5), model, samples),
    "Assertion on 'x' failed: x is of length 3 which is not allowed; the allowed lengths are: 1 or 2."
  )
  expect_error(
    dose(2, model, samples),
    "Assertion on 'x' failed: Probability must be within \\[0, 1\\] bounds but it is not."
  )
  expect_error(
    dose(-2, model, samples),
    "Assertion on 'x' failed: Probability must be within \\[0, 1\\] bounds but it is not."
  )
})

## LogisticNormalMixture ----

test_that("dose-LogisticNormalMixture works as expected", {
  model <- h_get_logistic_normal_mix()
  samples <- h_as_samples(list(alpha0 = c(0, -1, 1, 2), alpha1 = c(0, 2, 1, -1)))

  result <- dose(0.2, model, samples)
  expect_equal(result, c(0, 1.6487213, 0.1839397, 59.1124488), tolerance = 1e-7)
})

test_that("dose-LogisticNormalMixture works as expected for scalar samples", {
  model <- h_get_logistic_normal_mix()
  samples <- h_as_samples(list(alpha0 = 5, alpha1 = 10))

  result <- dose(c(0.3, 0.7), model, samples)
  expect_equal(result, c(1.114513, 1.320324), tolerance = 1e-6)
})

test_that("dose-LogisticNormalMixture works as expected for vectors", {
  model <- h_get_logistic_normal_mix()
  samples <- h_as_samples(list(alpha0 = c(5, 6), alpha1 = c(10, 11)))

  result <- dose(c(0.3, 0.7), model, samples)
  expect_equal(result, c(1.114513, 1.251972), tolerance = 1e-6)
})

test_that("dose-LogisticNormalMixture throws the error when x is not valid", {
  model <- h_get_logistic_normal_mix()
  samples <- h_as_samples(list(alpha0 = c(5, 6), alpha1 = c(10, 11)))

  expect_error(
    dose(c(0.4, 0.6, 0.5), model, samples),
    "Assertion on 'x' failed: x is of length 3 which is not allowed; the allowed lengths are: 1 or 2."
  )
  expect_error(
    dose(2, model, samples),
    "Assertion on 'x' failed: Probability must be within \\[0, 1\\] bounds but it is not."
  )
  expect_error(
    dose(-2, model, samples),
    "Assertion on 'x' failed: Probability must be within \\[0, 1\\] bounds but it is not."
  )
})

## LogisticNormalFixedMixture ----

test_that("dose-LogisticNormalFixedMixture works as expected", {
  model <- h_get_logistic_normal_fixed_mix()
  samples <- h_as_samples(list(alpha0 = c(0, -1, 1, 2), alpha1 = c(0, 2, 1, -1)))

  result <- dose(0.2, model, samples)
  expect_equal(result, c(0, 41.218032, 4.598493, 1477.811220), tolerance = 1e-7)
})

test_that("dose-LogisticNormalFixedMixture works as expected for scalar samples", {
  model <- h_get_logistic_normal_fixed_mix()
  samples <- h_as_samples(list(alpha0 = 5, alpha1 = 10))

  result <- dose(c(0.3, 0.7), model, samples)
  expect_equal(result, c(27.86282, 33.00809), tolerance = 1e-7)
})

test_that("dose-LogisticNormalFixedMixture works as expected for vectors", {
  model <- h_get_logistic_normal_fixed_mix()
  samples <- h_as_samples(list(alpha0 = c(5, 6), alpha1 = c(10, 11)))

  result <- dose(c(0.3, 0.7), model, samples)
  expect_equal(result, c(27.86282, 31.29929), tolerance = 1e-7)
})

test_that("dose-LogisticNormalFixedMixture throws the error when x is not valid", {
  model <- h_get_logistic_normal_fixed_mix()
  samples <- h_as_samples(list(alpha0 = c(5, 6), alpha1 = c(10, 11)))

  expect_error(
    dose(c(0.4, 0.6, 0.5), model, samples),
    "Assertion on 'x' failed: x is of length 3 which is not allowed; the allowed lengths are: 1 or 2."
  )
  expect_error(
    dose(2, model, samples),
    "Assertion on 'x' failed: Probability must be within \\[0, 1\\] bounds but it is not."
  )
  expect_error(
    dose(-2, model, samples),
    "Assertion on 'x' failed: Probability must be within \\[0, 1\\] bounds but it is not."
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
  expect_equal(result, c(1.910187, -12.832425, -11.180177, 3.381789), tolerance = 1e-7)
  expect_equal(result_log_dose, c(5.197825875, 0.003269673, 0.007469395, 10.848660131), tolerance = 1e-7)
})

test_that("dose-DualEndpoint works as expected for scalar samples", {
  model <- h_get_dual_endpoint()
  model_log_dose <- h_get_dual_endpoint(use_log_dose = TRUE)
  samples <- h_as_samples(list(betaZ = matrix(c(0.4, -0.2), ncol = 2)))

  result <- dose(c(0.3, 0.7), model, samples)
  result_log_dose <- dose(c(0.3, 0.7), model_log_dose, samples)
  expect_false(identical(result, result_log_dose))
  expect_equal(result, c(9.244005, -1.244005), tolerance = 1e-7)
  expect_equal(result_log_dose, c(203.394968, 1.073736), tolerance = 1e-7)
})

test_that("dose-DualEndpoint works as expected for vectors", {
  model <- h_get_dual_endpoint()
  model_log_dose <- h_get_dual_endpoint(use_log_dose = TRUE)
  samples <- h_as_samples(list(betaZ = matrix(c(0.4, -0.2, 0.5, 0.9), ncol = 2)))

  result <- dose(c(0.3, 0.7), model, samples)
  result_log_dose <- dose(c(0.3, 0.7), model_log_dose, samples)
  expect_false(identical(result, result_log_dose))
  expect_equal(result, c(-3.697602, 1.609779), tolerance = 1e-7)
  expect_equal(result_log_dose, c(0.3148516, 4.4728985), tolerance = 1e-7)
})

test_that("dose-DualEndpoint throws the error when x is not valid", {
  model <- h_get_dual_endpoint()
  samples <- h_as_samples(list(betaZ = matrix(c(0.4, -0.2, 0.5, 0.9), ncol = 2)))

  expect_error(
    dose(c(0.4, 0.6, 0.5), model, samples),
    "Assertion on 'x' failed: x is of length 3 which is not allowed; the allowed lengths are: 1 or 2."
  )
  expect_error(
    dose(2, model, samples),
    "Assertion on 'x' failed: Probability must be within \\[0, 1\\] bounds but it is not."
  )
  expect_error(
    dose(-2, model, samples),
    "Assertion on 'x' failed: Probability must be within \\[0, 1\\] bounds but it is not."
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
  result_expected <- c(0.4075397, 0.4514756, 0, 0.2997621, 0.3320788)
  expect_equal(result_emptydat, result_expected, tolerance = 1e-7)
  expect_equal(result, result_expected, tolerance = 1e-7)
})

test_that("dose-LogisticIndepBeta works as expected for scalar samples", {
  dlt_model <- h_get_logistic_indep_beta()
  samples <- h_as_samples(list(phi1 = -1, phi2 = 1))

  result <- dose(c(0.45, 0.7), dlt_model, samples)
  expect_equal(result, c(2.224049, 6.342658), tolerance = 1e-7)
})

test_that("dose-LogisticIndepBeta works as expected for vectors", {
  dlt_model <- h_get_logistic_indep_beta()
  samples <- h_as_samples(list(phi1 = c(-1, 0.5), phi2 = c(1, 0.6)))

  result <- dose(c(0.45, 0.7), dlt_model, samples)
  expect_equal(result, c(2.224049, 1.783950), tolerance = 1e-6)
})

test_that("dose-LogisticIndepBeta throws the error when x is not valid", {
  dlt_model <- h_get_logistic_indep_beta()
  samples <- h_as_samples(list(phi1 = c(-1, 0.5), phi2 = c(1, 0.6)))

  expect_error(
    dose(c(0.4, 0.6, 0.5), dlt_model, samples),
    "Assertion on 'x' failed: x is of length 3 which is not allowed; the allowed lengths are: 1 or 2."
  )
  expect_error(
    dose(2, dlt_model, samples),
    "Assertion on 'x' failed: Probability must be within \\[0, 1\\] bounds but it is not."
  )
  expect_error(
    dose(-2, dlt_model, samples),
    "Assertion on 'x' failed: Probability must be within \\[0, 1\\] bounds but it is not."
  )
})

## LogisticIndepBeta-noSamples ----

test_that("dose-LogisticIndepBeta-noSamples works as expected", {
  dlt_model_emptydat <- h_get_logistic_indep_beta(emptydata = TRUE)
  dlt_model <- h_get_logistic_indep_beta(emptydata = FALSE)

  result_emptydat <- dose(c(0.45, 0.55), dlt_model_emptydat)
  result <- dose(c(0.45, 0.55), dlt_model)
  expect_equal(result_emptydat, c(68.96623, 182.55643), tolerance = 1e-7)
  expect_equal(result, c(75.82941, 108.33195), tolerance = 1e-7)
})

test_that("dose-LogisticIndepBeta-noSamples throws the error when x is not valid", {
  dlt_model <- h_get_logistic_indep_beta()
  expect_error(
    dose(-2, dlt_model),
    "Assertion on 'x' failed: Probability must be within \\[0, 1\\] bounds but it is not."
  )
})

## OneParLogNormalPrior ----

test_that("dose-OneParLogNormalPrior works as expected", {
  model <- h_get_one_par_log_normal_prior()
  samples <- h_as_samples(list(alpha = c(0, 0.5, 1, 2)))

  result <- dose(0.4, model, samples)
  expect_equal(result, c(5.125000, 7.512509, 9.440417, 11.771394), tolerance = 1e-7)
})

test_that("dose-OneParLogNormalPrior works as expected for scalar samples", {
  model <- h_get_one_par_log_normal_prior()
  samples <- h_as_samples(list(alpha = 1))

  result <- dose(c(0.3, 0.7), model, samples)
  expect_equal(result, c(8.454708, 11.684171), tolerance = 1e-7)
})

test_that("dose-OneParLogNormalPrior works as expected for vectors", {
  model <- h_get_one_par_log_normal_prior()
  samples <- h_as_samples(list(alpha = c(1, 2)))

  result <- dose(c(0.3, 0.7), model, samples)
  expect_equal(result, c(8.454708, 12), tolerance = 1e-7)
})

test_that("dose-OneParLogNormalPrior throws the error when x is not valid", {
  model <- h_get_one_par_log_normal_prior()
  samples <- h_as_samples(list(alpha = c(1, 2)))

  expect_error(
    dose(c(0.4, 0.6, 0.5), model, samples),
    "Assertion on 'x' failed: x is of length 3 which is not allowed; the allowed lengths are: 1 or 2."
  )
  expect_error(
    dose(2, model, samples),
    "Assertion on 'x' failed: Probability must be within \\[0, 1\\] bounds but it is not."
  )
  expect_error(
    dose(-2, model, samples),
    "Assertion on 'x' failed: Probability must be within \\[0, 1\\] bounds but it is not."
  )
})

## OneParExpPrior ----

test_that("dose-OneParExpPrior works as expected", {
  model <- h_get_one_par_exp_prior()
  samples <- h_as_samples(list(theta = c(0.001, 0.5, 1, 2)))

  result <- dose(0.4, model, samples)
  expect_equal(result, c(1, 1.825000, 5.125000, 8.321264), tolerance = 1e-7)
})

test_that("dose-OneParExpPrior works as expected for scalar samples", {
  model <- h_get_one_par_exp_prior()
  samples <- h_as_samples(list(theta = 1))

  result <- dose(c(0.3, 0.7), model, samples)
  expect_equal(result, c(3.75, 9.25), tolerance = 1e-7)
})

test_that("dose-OneParExpPrior works as expected for vectors", {
  model <- h_get_one_par_exp_prior()
  samples <- h_as_samples(list(theta = c(1, 2)))

  result <- dose(c(0.3, 0.7), model, samples)
  expect_equal(result, c(3.75, 11.12908), tolerance = 1e-6)
})

test_that("dose-OneParExpPrior throws the error when x is not valid", {
  model <- h_get_one_par_exp_prior()
  samples <- h_as_samples(list(theta = c(1, 2)))

  expect_error(
    dose(c(0.4, 0.6, 0.5), model, samples),
    "Assertion on 'x' failed: x is of length 3 which is not allowed; the allowed lengths are: 1 or 2."
  )
  expect_error(
    dose(2, model, samples),
    "Assertion on 'x' failed: Probability must be within \\[0, 1\\] bounds but it is not."
  )
  expect_error(
    dose(-2, model, samples),
    "Assertion on 'x' failed: Probability must be within \\[0, 1\\] bounds but it is not."
  )
})

# prob ----

## LogisticNormal ----

test_that("prob-LogisticNormal works as expected", {
  model <- h_get_logistic_normal()
  samples <- h_as_samples(list(alpha0 = c(0, -1, 1, 2), alpha1 = c(0, 2, 1, -1)))

  result <- prob(60, model, samples)
  expect_equal(result, c(0.5, 0.3462969, 0.7653650, 0.8602873), tolerance = 1e-7)
})

test_that("prob-LogisticNormal works as expected for scalar samples", {
  model <- h_get_logistic_normal()
  samples <- h_as_samples(list(alpha0 = 5, alpha1 = 10))

  result <- prob(c(20, 60), model, samples)
  expect_equal(result, c(0.01532378, 0.99891297), tolerance = 1e-7)
})

test_that("prob-LogisticNormal works as expected for vectors", {
  model <- h_get_logistic_normal()
  samples <- h_as_samples(list(alpha0 = c(5, 6), alpha1 = c(10, 11)))

  result <- prob(c(20, 60), model, samples)
  expect_equal(result, c(0.01532378, 0.99966650), tolerance = 1e-7)
})

test_that("prob-LogisticNormal throws the error when dose is not valid", {
  model <- h_get_logistic_normal()
  samples <- h_as_samples(list(alpha0 = c(5, 6), alpha1 = c(10, 11)))

  expect_error(
    prob(c(40, 50, 90), model, samples),
    "Assertion on 'dose' failed: x is of length 3 which is not allowed; the allowed lengths are: 1 or 2."
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
  expect_equal(result, c(0.5, 0.3462969, 0.7653650, 0.8602873), tolerance = 1e-7)
})

test_that("prob-LogisticLogNormal works as expected for scalar samples", {
  model <- h_get_logistic_log_normal()
  samples <- h_as_samples(list(alpha0 = 5, alpha1 = 10))

  result <- prob(c(26, 35), model, samples)
  expect_equal(result, c(0.1766422, 0.8074073), tolerance = 1e-7)
})

test_that("prob-LogisticLogNormal works as expected for vectors", {
  model <- h_get_logistic_log_normal()
  samples <- h_as_samples(list(alpha0 = c(5, 6), alpha1 = c(10, 11)))

  result <- prob(c(26, 35), model, samples)
  expect_equal(result, c(0.1766422, 0.8886055), tolerance = 1e-7)
})

test_that("prob-LogisticLogNormal throws the error when dose is not valid", {
  model <- h_get_logistic_log_normal()
  samples <- h_as_samples(list(alpha0 = c(5, 6), alpha1 = c(10, 11)))

  expect_error(
    prob(c(40, 50, 90), model, samples),
    "Assertion on 'dose' failed: x is of length 3 which is not allowed; the allowed lengths are: 1 or 2."
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
  expect_equal(result, c(0.5, 0.9525741, 0.9525741, 0.5), tolerance = 1e-7)
})

test_that("prob-LogisticLogNormalSub works as expected for scalar samples", {
  model <- h_get_logistic_log_normal_sub()
  samples <- h_as_samples(list(alpha0 = 2, alpha1 = 0.5))

  result <- prob(c(3, 7), model, samples)
  expect_equal(result, c(0.9241418, 0.9890131), tolerance = 1e-7)
})

test_that("prob-LogisticLogNormalSub works as expected for vectors", {
  model <- h_get_logistic_log_normal_sub()
  samples <- h_as_samples(list(alpha0 = c(-3, -5), alpha1 = c(2, 4)))

  result <- prob(c(3, 5), model, samples)
  expect_equal(result, c(0.2689414, 0.9990889), tolerance = 1e-7)
})

test_that("prob-LogisticLogNormalSub throws the error when dose is not valid", {
  model <- h_get_logistic_log_normal_sub()
  samples <- h_as_samples(list(alpha0 = c(5, 6), alpha1 = c(10, 11)))

  expect_error(
    prob(c(40, 50, 90), model, samples),
    "Assertion on 'dose' failed: x is of length 3 which is not allowed; the allowed lengths are: 1 or 2."
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
  expect_equal(result, c(0.5, 0.01479359, 0.65990847, 0.99517026), tolerance = 1e-7)
})

test_that("prob-ProbitLogNormal works as expected for scalar samples", {
  model <- h_get_probit_log_normal()
  samples <- h_as_samples(list(alpha0 = 2, alpha1 = 0.5))

  result <- prob(c(4, 10), model, samples)
  expect_equal(result, c(0.9560059, 0.9847775), tolerance = 1e-7)
})

test_that("prob-ProbitLogNormal works as expected for vectors", {
  model <- h_get_probit_log_normal()
  samples <- h_as_samples(list(alpha0 = c(5, 3), alpha1 = c(10, 7)))

  result <- prob(c(4, 5), model, samples)
  expect_equal(result, c(0.1900080, 0.6727423), tolerance = 1e-7)
})

test_that("prob-ProbitLogNormal throws the error when dose is not valid", {
  model <- h_get_probit_log_normal()
  samples <- h_as_samples(list(alpha0 = c(5, 6), alpha1 = c(10, 11)))

  expect_error(
    prob(c(40, 50, 90), model, samples),
    "Assertion on 'dose' failed: x is of length 3 which is not allowed; the allowed lengths are: 1 or 2."
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
  expect_equal(result, c(0.5, 0.9986501, 0.9986501, 0.5), tolerance = 1e-7)
})

test_that("prob-ProbitLogNormalRel works as expected for scalar samples", {
  model <- h_get_probit_log_normal_rel()
  samples <- h_as_samples(list(alpha0 = 2, alpha1 = -0.5))

  result <- prob(c(2, 2.5), model, samples)
  expect_equal(result, c(0.9331928, 0.9154343), tolerance = 1e-7)
})

test_that("prob-ProbitLogNormalRel works as expected for vectors", {
  model <- h_get_probit_log_normal_rel()
  samples <- h_as_samples(list(alpha0 = c(-10, -9), alpha1 = c(10, 8)))

  result <- prob(c(2, 2.5), model, samples)
  expect_equal(result, c(0.5, 0.8413447), tolerance = 1e-7)
})

test_that("prob-ProbitLogNormalRel throws the error when dose is not valid", {
  model <- h_get_probit_log_normal_rel()
  samples <- h_as_samples(list(alpha0 = c(5, 6), alpha1 = c(10, 11)))

  expect_error(
    prob(c(40, 50, 90), model, samples),
    "Assertion on 'dose' failed: x is of length 3 which is not allowed; the allowed lengths are: 1 or 2."
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
  expect_equal(result, c(0.1543506, 0.2084767, 0.3011106), tolerance = 1e-6)
})

test_that("prob-LogisticKadane works as expected for scalar samples", {
  model <- h_get_logistic_kadane()
  samples <- h_as_samples(list(rho0 = 0.15, gamma = 30))

  result <- prob(c(2, 15), model, samples)
  expect_equal(result, c(0.1545688, 0.2245944), tolerance = 1e-6)
})

test_that("prob-LogisticKadane works as expected for vectors", {
  model <- h_get_logistic_kadane()
  samples <- h_as_samples(list(rho0 = c(0.15, 0.3), gamma = c(30, 20)))

  result <- prob(c(2, 15), model, samples)
  expect_equal(result, c(0.1545688, 0.3219568), tolerance = 1e-7)
})

test_that("prob-LogisticKadane throws the error when dose is not valid", {
  model <- h_get_logistic_kadane()
  samples <- h_as_samples(list(rho0 = c(0.1, 0.2), gamma = c(10, 40)))

  expect_error(
    prob(c(40, 50, 90), model, samples),
    "Assertion on 'dose' failed: x is of length 3 which is not allowed; the allowed lengths are: 1 or 2."
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
  expect_equal(result, c(0.5, 0.9969888, 0.9878859, 0.1976262), tolerance = 1e-7)
})

test_that("prob-LogisticNormalMixture works as expected for scalar samples", {
  model <- h_get_logistic_normal_mix()
  samples <- h_as_samples(list(alpha0 = 5, alpha1 = 10))

  result <- prob(c(1, 1.5), model, samples)
  expect_equal(result, c(0.1265878, 0.8931358), tolerance = 1e-7)
})

test_that("prob-LogisticNormalMixture works as expected for vectors", {
  model <- h_get_logistic_normal_mix()
  samples <- h_as_samples(list(alpha0 = c(5, 6), alpha1 = c(10, 11)))

  result <- prob(c(1, 1.5), model, samples)
  expect_equal(result, c(0.1265878, 0.9445642), tolerance = 1e-7)
})

test_that("prob-LogisticNormalMixture throws the error when dose is not valid", {
  model <- h_get_logistic_normal_mix()
  samples <- h_as_samples(list(alpha0 = c(5, 6), alpha1 = c(10, 11)))

  expect_error(
    prob(c(40, 50, 90), model, samples),
    "Assertion on 'dose' failed: x is of length 3 which is not allowed; the allowed lengths are: 1 or 2."
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
  expect_equal(result, c(0.5, 0.3462969, 0.7653650, 0.8602873), tolerance = 1e-7)
})

test_that("prob-LogisticNormalFixedMixture works as expected for scalar samples", {
  model <- h_get_logistic_normal_fixed_mix()
  samples <- h_as_samples(list(alpha0 = 5, alpha1 = 10))

  result <- prob(c(30, 45), model, samples)
  expect_equal(result, c(0.4729623, 0.9810421), tolerance = 1e-7)
})

test_that("prob-LogisticNormalFixedMixture works as expected for vectors", {
  model <- h_get_logistic_normal_fixed_mix()
  samples <- h_as_samples(list(alpha0 = c(5, 6), alpha1 = c(10, 11)))

  result <- prob(c(30, 45), model, samples)
  expect_equal(result, c(0.4729623, 0.9921630), tolerance = 1e-7)
})

test_that("prob-LogisticNormalFixedMixture throws the error when dose is not valid", {
  model <- h_get_logistic_normal_fixed_mix()
  samples <- h_as_samples(list(alpha0 = c(5, 6), alpha1 = c(10, 11)))

  expect_error(
    prob(c(40, 50, 90), model, samples),
    "Assertion on 'dose' failed: x is of length 3 which is not allowed; the allowed lengths are: 1 or 2."
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
  expect_equal(result, c(0.6748043, 0.6726061, 0.2901927, 0.2901927), tolerance = 1e-7)
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
  expect_equal(result, c(0.2474127, 0.2829247), tolerance = 1e-7)
})

test_that("prob-LogisticLogNormalMixture works as expected for vectorized dose-samples", {
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
  result <- prob(c(1, 1.5, 3, 6), model, samples)
  expect_equal(result, c(0.2474127, 0.2809003, 0.1098043, 0.1399769), tolerance = 1e-6)
})

test_that("prob-LogisticLogNormalMixture throws the error when dose is not valid", {
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
    "Assertion on 'dose' failed: x is of length 2 which is not allowed; the allowed lengths are: 1 or 4."
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
  expect_equal(result, c(0.363169349, 0.363169349, 0.864333939, 0.006477572), tolerance = 1e-7)
  expect_equal(result_log_dose, c(0.5497829, 0.3055966, 0.7642097, 0.1966136), tolerance = 1e-7)
})

test_that("prob-DualEndpoint works as expected for scalar samples", {
  model <- h_get_dual_endpoint()
  model_log_dose <- h_get_dual_endpoint(use_log_dose = TRUE)
  samples <- h_as_samples(list(betaZ = matrix(c(0.4, -0.6), ncol = 2)))

  result <- prob(c(5, 8), model, samples)
  result_log_dose <- prob(c(5, 8), model_log_dose, samples)
  expect_false(identical(result, result_log_dose))
  expect_equal(result, c(0.13566606, 0.02275013), tolerance = 1e-7)
  expect_equal(result_log_dose, c(0.4404713, 0.3329519), tolerance = 1e-7)
})

test_that("prob-DualEndpoint works as expected for vectorized dose-samples", {
  model <- h_get_dual_endpoint()
  model_log_dose <- h_get_dual_endpoint(use_log_dose = TRUE)
  samples <- h_as_samples(list(betaZ = matrix(c(0.4, -0.2, 0.5, 0.9), ncol = 2)))

  result <- prob(c(5, 8), model, samples)
  result_log_dose <- prob(c(5, 8), model_log_dose, samples)
  expect_false(identical(result, result_log_dose))
  expect_equal(result, c(0.9505285, 0.9996631), tolerance = 1e-7)
  expect_equal(result_log_dose, c(0.8045939, 0.8526035), tolerance = 1e-7)
})

test_that("prob-DualEndpoint throws the error when dose is not valid", {
  model <- h_get_dual_endpoint()
  samples <- h_as_samples(list(betaZ = matrix(c(0.4, -0.2, 0.5, 0.9), ncol = 2)))

  expect_error(
    prob(c(40, 50, 90), model, samples),
    "Assertion on 'dose' failed: x is of length 3 which is not allowed; the allowed lengths are: 1 or 2."
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

  result_emptydat <- prob(20, dlt_model_emptydat, samples)
  result <- prob(20, dlt_model, samples)
  result_expected <- c(0.0003968183, 0.0195350305, 0.5, 0.9804649695, 0.9996031817)
  expect_equal(result_emptydat, result_expected, tolerance = 1e-7)
  expect_equal(result, result_expected, tolerance = 1e-7)
})

test_that("prob-LogisticIndepBeta works as expected for scalar samples", {
  dlt_model <- h_get_logistic_indep_beta()
  samples <- h_as_samples(list(phi1 = -1, phi2 = 1))

  result <- prob(c(6, 15), dlt_model, samples)
  expect_equal(result, c(0.6882090, 0.8465832), tolerance = 1e-7)
})

test_that("prob-LogisticIndepBeta works as expected for vectors", {
  dlt_model <- h_get_logistic_indep_beta()
  samples <- h_as_samples(list(phi1 = c(-1, 0.5), phi2 = c(1, 0.6)))

  result <- prob(c(6, 15), dlt_model, samples)
  expect_equal(result, c(0.6882090, 0.8932932), tolerance = 1e-7)
})

test_that("prob-LogisticIndepBeta throws the error when dose is not valid", {
  dlt_model <- h_get_logistic_indep_beta()
  samples <- h_as_samples(list(phi1 = c(-1, 0.5), phi2 = c(1, 0.6)))

  expect_error(
    prob(c(40, 50, 90), dlt_model, samples),
    "Assertion on 'dose' failed: x is of length 3 which is not allowed; the allowed lengths are: 1 or 2."
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

  result_emptydat <- prob(300, dlt_model_emptydat)
  result <- prob(300, dlt_model)
  expect_equal(result_emptydat, 0.6, tolerance = 1e-7)
  expect_equal(result, 0.7935871, tolerance = 1e-7)
})

test_that("prob-LogisticIndepBeta-noSamples works as expected for vector dose", {
  dlt_model_emptydat <- h_get_logistic_indep_beta(emptydata = TRUE)
  dlt_model <- h_get_logistic_indep_beta(emptydata = FALSE)

  result_emptydat <- prob(c(500, 1000), dlt_model_emptydat)
  result <- prob(c(500, 1000), dlt_model)
  expect_equal(result_emptydat, c(0.6493251, 0.7113300), tolerance = 1e-7)
  expect_equal(result, c(0.8722965, 0.9371023), tolerance = 1e-7)
})

test_that("prob-LogisticIndepBeta-noSamples throws the error when dose is not valid", {
  dlt_model <- h_get_logistic_indep_beta()
  expect_error(
    prob(-3, dlt_model),
    "Assertion on 'dose' failed: Element 1 is not >= 0."
  )
})

## OneParLogNormalPrior ----

test_that("prob-OneParLogNormalPrior works as expected", {
  model <- h_get_one_par_log_normal_prior()
  samples <- h_as_samples(list(alpha = c(0, 0.5, 1, 2)))

  result <- prob(60, model, samples)
  expect_equal(result, c(0.9, 0.8405405, 0.7509625, 0.4590874), tolerance = 1e-7)
})

test_that("prob-OneParLogNormalPrior works as expected for scalar samples", {
  model <- h_get_one_par_log_normal_prior()
  samples <- h_as_samples(list(alpha = 1))

  result <- prob(c(10, 20, 80), model, samples)
  expect_equal(result, c(0.4650659, 0.7509625, 0.7509625), tolerance = 1e-7)
})

test_that("prob-OneParLogNormalPrior works as expected for vectors", {
  model <- h_get_one_par_log_normal_prior()
  samples <- h_as_samples(list(alpha = c(1, 2)))

  result <- prob(c(12, 10), model, samples)
  expect_equal(result, c(0.7509625, 0.1247989), tolerance = 1e-7)
})

test_that("prob-OneParLogNormalPrior throws the error when dose is not valid", {
  model <- h_get_one_par_log_normal_prior()
  samples <- h_as_samples(list(alpha = c(1, 2)))

  expect_error(
    prob(c(40, 50, 90), model, samples),
    "Assertion on 'dose' failed: x is of length 3 which is not allowed; the allowed lengths are: 1 or 2."
  )
  expect_error(
    prob(-3, model, samples),
    "Assertion on 'dose' failed: Element 1 is not >= 0."
  )
})

## OneParExpPrior ----

test_that("prob-OneParExpPrior works as expected", {
  model <- h_get_one_par_exp_prior()
  samples <- h_as_samples(list(theta = c(0, 0.5, 1, 2)))

  result <- prob(60, model, samples)
  expect_equal(result, c(1, 0.9486833, 0.9, 0.8100000), tolerance = 1e-7)
})

test_that("prob-OneParExpPrior works as expected for scalar samples", {
  model <- h_get_one_par_exp_prior()
  samples <- h_as_samples(list(theta = 1))

  result <- prob(c(12, 10), model, samples)
  expect_equal(result, c(0.9, 0.7545455), tolerance = 1e-7)
})

test_that("prob-OneParExpPrior works as expected for vectors", {
  model <- h_get_one_par_exp_prior()
  samples <- h_as_samples(list(theta = c(1, 2)))

  result <- prob(c(12, 10), model, samples)
  expect_equal(result, c(0.9, 0.5693388), tolerance = 1e-7)
})

test_that("prob-OneParExpPrior throws the error when dose is not valid", {
  model <- h_get_one_par_exp_prior()
  samples <- h_as_samples(list(theta = c(1, 2)))

  expect_error(
    prob(c(40, 50, 90), model, samples),
    "Assertion on 'dose' failed: x is of length 3 which is not allowed; the allowed lengths are: 1 or 2."
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
  expect_error(
    biomarker(xLevel = 3L, model, samples),
    "Assertion on 'xLevel' failed: Element 1 is not <= 2."
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
  expect_equal(result, c(0.1388810, 0.1662916, 1.0205899, 0.8068247), tolerance = 1e-7)
})

test_that("gain-ModelTox-ModelEff works as expected for scalar samples", {
  model_dle <- h_get_logistic_indep_beta(emptydata = TRUE)
  samples_dle <- h_as_samples(list(phi1 = 1.72, phi2 = 0.17))
  model_eff <- h_get_eff_log_log(emptydata = TRUE)
  samples_eff <- h_as_samples(list(theta1 = -1.08, theta2 = 1.93, nu = 6.48))

  result <- gain(dose = c(50, 175), model_dle, samples_dle, model_eff, samples_eff)
  expect_equal(result, c(0.1325413, 0.1449772), tolerance = 1e-7)
})

test_that("gain-ModelTox-ModelEff works as expected for vectors", {
  model_dle <- h_get_logistic_indep_beta(emptydata = TRUE)
  samples_dle <- h_as_samples(list(phi1 = c(1.72, -1.45), phi2 = c(0.17, 0.79)))
  model_eff <- h_get_eff_log_log(emptydata = TRUE)
  samples_eff <- h_as_samples(
    list(theta1 = c(-1.08, -0.87), theta2 = c(1.93, 1.51), nu = c(6.48, 63.36))
  )

  result <- gain(dose = c(50, 175), model_dle, samples_dle, model_eff, samples_eff)
  expect_equal(result, c(0.1325413, 0.1083962), tolerance = 1e-6)
})

test_that("gain-ModelTox-ModelEff throws the error when dose is not of valid length", {
  model_dle <- h_get_logistic_indep_beta(emptydata = TRUE)
  samples_dle <- h_as_samples(list(phi1 = c(1.72, -1.45), phi2 = c(0.17, 0.79)))
  model_eff <- h_get_eff_log_log(emptydata = TRUE)
  samples_eff <- h_as_samples(
    list(theta1 = c(-1.08, -0.87), theta2 = c(1.93, 1.51), nu = c(6.48, 63.36))
  )

  expect_error(
    gain(dose = c(50, 75, 125), model_dle, samples_dle, model_eff, samples_eff),
    "Assertion on 'dose' failed: x is of length 3 which is not allowed; the allowed lengths are: 1 or 2."
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

# update ----

## ModelPseudo ----

test_that("update-ModelPseudo works as expected for LogisticIndepBeta", {
  model <- h_get_logistic_indep_beta(emptydata = TRUE)
  new_data <- h_get_data()

  result <- update(object = model, data = new_data)
  model@phi1 <- -5.090751
  model@phi2 <- 0.933697
  model@Pcov[] <- matrix(c(9.455109, -2.023160, -2.023160, 0.452532), nrow = 2)
  model@data <- new_data
  expect_equal(result, model, tolerance = 10e-8)
})

test_that("update-ModelPseudo works as expected for Effloglog", {
  model <- h_get_eff_log_log(emptydata = TRUE)
  new_data <- h_get_data_dual()

  result <- update(object = model, data = new_data)
  expect_snapshot(result)
})

test_that("update-ModelPseudo works as expected for EffFlexi", {
  model <- h_get_eff_flexi(emptydata = TRUE)
  new_data <- h_get_data_dual()

  result <- update(object = model, data = new_data)
  expect_snapshot(result)
})

test_that("update-ModelPseudo throws the error when data is not an object of Data class", {
  model <- h_get_logistic_indep_beta(emptydata = TRUE)
  new_data <- h_get_data()
  new_data <- h_slots(new_data, names = slotNames(new_data)) # A list.

  expect_error(
    update(object = model, data = new_data),
    "Assertion on 'data' failed: Must inherit from class 'Data' *"
  )
})
