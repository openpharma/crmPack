# doseFunction ----

## GeneralModel ----

test_that("doseFunction-GeneralModel returns correct dose function", {
  model <- h_get_logistic_log_normal()
  samples <- Samples(data = list(alpha0 = 1, alpha1 = 2), options = McmcOptions(samples = 1))
  dose_args <- c("prob", "model", "samples")

  dose_fun <- doseFunction(model, alpha0 = 1, alpha1 = 2)
  dose_fun_dose_args <- as.character(body(dose_fun)[[2]][-1])
  dose_fun_env <- environment(dose_fun)

  expect_function(dose_fun, args = "prob", nargs = 1, null.ok = FALSE)
  expect_equal(dose_fun_dose_args, dose_args)
  expect_subset(
    setdiff(dose_fun_dose_args, "prob"),
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
    "Assertion on .* failed: Must be a subset of \\{'alpha0','alpha1'\\}, but is \\{'wrong','alpha1'\\}.$"
  )
})

## ModelTox ----

test_that("doseFunction-ModelTox returns correct dose function", {
  model <- h_get_logistic_indep_beta()
  samples <- Samples(data = list(phi1 = 35, phi2 = 5), options = McmcOptions(samples = 1))
  dose_args <- c("prob", "model", "samples")

  dose_fun <- doseFunction(model, phi1 = 35, phi2 = 5)
  dose_fun_dose_args <- as.character(body(dose_fun)[[2]][-1])
  dose_fun_env <- environment(dose_fun)

  expect_function(dose_fun, args = "prob", nargs = 1, null.ok = FALSE)
  expect_equal(dose_fun_dose_args, dose_args)
  expect_subset(
    setdiff(dose_fun_dose_args, "prob"),
    ls(envir = dose_fun_env)
  )
  expect_identical(dose_fun_env[["model"]], model)
  expect_identical(dose_fun_env[["samples"]], samples)
})

test_that("doseFunction-ModelTox throws the error when no params are provided", {
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
  samples <- Samples(data = list(alpha0 = 1, alpha1 = 2), options = McmcOptions(samples = 1))
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
    "Assertion on .* failed: Must be a subset of \\{'alpha0','alpha1'\\}, but is \\{'wrong','alpha1'\\}.$"
  )
})

## ModelTox ----

test_that("probFunction-ModelTox returns correct prob function", {
  model <- h_get_logistic_indep_beta()
  samples <- Samples(data = list(phi1 = 35, phi2 = 5), options = McmcOptions(samples = 1))
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

# dose ----

## LogisticNormal ----

test_that("dose-LogisticNormal works as expected", {
  data <- h_get_data()
  model <- h_get_logistic_normal()
  samples <- h_as_samples(list(alpha0 = c(0, -1, 1, 2), alpha1 = c(0, 2, 1, -1)))

  result <- dose(0.4, model, samples)
  expect_equal(
    result,
    c(0.00000, 67.30876, 12.26265, 554.17921),
    tolerance = 1e-05
  )
})

test_that("dose-LogisticNormal works as expected for scalar samples", {
  data <- h_get_data()
  model <- h_get_logistic_normal()
  samples <- h_as_samples(list(alpha0 = 5, alpha1 = 10))

  result <- dose(c(0.3, 0.6), model, samples)
  expect_equal(result, c(27.86282, 31.58144), tolerance = 1e-05)
})

test_that("dose-LogisticNormal throws the error when prob is not a valid scalar", {
  data <- h_get_data()
  model <- h_get_logistic_normal()
  samples <- h_as_samples(list(alpha0 = c(5, 6), alpha1 = c(10, 11)))
  expect_error(
    dose(prob = c(0.4, 0.6), model = model, samples = samples),
    "Assertion on 'prob' failed: Must have length 1."
  )
  expect_error(
    dose(prob = 2, model = model, samples = samples),
    "Assertion on 'prob' failed: Element 1 is not <= 1."
  )
  expect_error(
    dose(prob = -2, model = model, samples = samples),
    "Assertion on 'prob' failed: Element 1 is not >= 0."
  )
})

## LogisticLogNormal ----

test_that("dose-LogisticLogNormal works as expected", {
  data <- h_get_data()
  model <- h_get_logistic_log_normal()
  samples <- h_as_samples(list(alpha0 = c(0, -1, 1, 2), alpha1 = c(0, 2, 1, -1)))

  result <- dose(0.4, model, samples)
  expect_equal(
    result,
    c(0.00000, 67.30876, 12.26265, 554.17921),
    tolerance = 1e-05
  )
})

test_that("dose-LogisticLogNormal works as expected for scalar samples", {
  data <- h_get_data()
  model <- h_get_logistic_log_normal()
  samples <- h_as_samples(list(alpha0 = 5, alpha1 = 10))

  result <- dose(c(0.3, 0.7), model, samples)
  expect_equal(result, c(27.86282, 33.00809), tolerance = 1e-05)
})

test_that("dose-LogisticLogNormal throws the error when prob is not a valid scalar", {
  data <- h_get_data()
  model <- h_get_logistic_log_normal()
  samples <- h_as_samples(list(alpha0 = c(5, 6), alpha1 = c(10, 11)))

  expect_error(
    dose(prob = c(40, 50), model = model, samples = samples),
    "Assertion on 'prob' failed: Must have length 1."
  )
  expect_error(
    dose(prob = 2, model = model, samples = samples),
    "Assertion on 'prob' failed: Element 1 is not <= 1."
  )
  expect_error(
    dose(prob = -2, model = model, samples = samples),
    "Assertion on 'prob' failed: Element 1 is not >= 0."
  )
})

## LogisticLogNormalSub ----

test_that("dose-LogisticLogNormalSub works as expected", {
  data <- h_get_data()
  model <- h_get_logistic_log_normal_sub()
  samples <- h_as_samples(list(alpha0 = c(0, -1, 1, 2), alpha1 = c(0, 2, 1, -1)))

  result <- dose(0.4, model, samples)
  expect_equal(
    result,
    c(-Inf, 2.2972674, 0.5945349, 4.4054651),
    tolerance = 1e-05
  )
})

test_that("dose-LogisticLogNormalSub works as expected for scalar samples", {
  data <- h_get_data()
  model <- h_get_logistic_log_normal_sub()
  samples <- h_as_samples(list(alpha0 = 5, alpha1 = 10))

  result <- dose(c(0.3, 0.7), model, samples)
  expect_equal(result, c(1.41527, 1.58473), tolerance = 1e-05)
})

test_that("dose-LogisticLogNormalSub throws the error when prob is not a valid scalar", {
  data <- h_get_data()
  model <- h_get_logistic_log_normal_sub()
  samples <- h_as_samples(list(alpha0 = c(5, 6), alpha1 = c(10, 11)))

  expect_error(
    dose(prob = c(40, 50), model = model, samples = samples),
    "Assertion on 'prob' failed: Must have length 1."
  )
  expect_error(
    dose(prob = 2, model = model, samples = samples),
    "Assertion on 'prob' failed: Element 1 is not <= 1."
  )
  expect_error(
    dose(prob = -2, model = model, samples = samples),
    "Assertion on 'prob' failed: Element 1 is not >= 0."
  )
})

## ProbitLogNormal ----

test_that("dose-ProbitLogNormal works as expected", {
  data <- h_get_data()
  model <- h_get_probit_log_normal()
  samples <- h_as_samples(list(alpha0 = c(0, -1, 1, 2), alpha1 = c(0, 2, 1, -1)))

  result <- dose(0.4, model, samples)
  expect_equal(
    result,
    c(0, 10.458421, 2.055942, 68.540727),
    tolerance = 1e-05
  )
})

test_that("dose-ProbitLogNormal works as expected for scalar samples", {
  data <- h_get_data()
  model <- h_get_probit_log_normal()
  samples <- h_as_samples(list(alpha0 = 5, alpha1 = 10))

  result <- dose(c(0.3, 0.7), model, samples)
  expect_equal(result, c(4.143915, 4.602138), tolerance = 1e-05)
})

test_that("dose-ProbitLogNormal throws the error when prob is not a valid scalar", {
  data <- h_get_data()
  model <- h_get_probit_log_normal()
  samples <- h_as_samples(list(alpha0 = c(5, 6), alpha1 = c(10, 11)))

  expect_error(
    dose(prob = c(40, 50), model = model, samples = samples),
    "Assertion on 'prob' failed: Must have length 1."
  )
  expect_error(
    dose(prob = 2, model = model, samples = samples),
    "Assertion on 'prob' failed: Element 1 is not <= 1."
  )
  expect_error(
    dose(prob = -2, model = model, samples = samples),
    "Assertion on 'prob' failed: Element 1 is not >= 0."
  )
})

## ProbitLogNormalRel ----

test_that("dose-ProbitLogNormalRel works as expected", {
  data <- h_get_data()
  model <- h_get_probit_log_normal_rel()
  samples <- h_as_samples(list(alpha0 = c(0, -1, 1, 2), alpha1 = c(0, 2, 1, -1)))

  result <- dose(0.4, model, samples)
  expect_equal(
    result,
    c(-Inf, 0.7466529, -2.5066942, 4.5066942),
    tolerance = 1e-05
  )
})

test_that("dose-ProbitLogNormalRel works as expected for scalar samples", {
  data <- h_get_data()
  model <- h_get_probit_log_normal_rel()
  samples <- h_as_samples(list(alpha0 = 5, alpha1 = 10))

  result <- dose(c(0.3, 0.7), model, samples)
  expect_equal(result, c(-1.1048801, -0.8951199), tolerance = 1e-05)
})

test_that("dose-ProbitLogNormalRel throws the error when prob is not a valid scalar", {
  data <- h_get_data()
  model <- h_get_probit_log_normal_rel()
  samples <- h_as_samples(list(alpha0 = c(5, 6), alpha1 = c(10, 11)))

  expect_error(
    dose(prob = c(40, 50), model = model, samples = samples),
    "Assertion on 'prob' failed: Must have length 1."
  )
  expect_error(
    dose(prob = 2, model = model, samples = samples),
    "Assertion on 'prob' failed: Element 1 is not <= 1."
  )
  expect_error(
    dose(prob = -2, model = model, samples = samples),
    "Assertion on 'prob' failed: Element 1 is not >= 0."
  )
})

## LogisticIndepBeta ----

test_that("dose-LogisticIndepBeta works as expected", {
  dlt_model <- h_get_logistic_indep_beta()
  samples <- h_as_samples(
    list(
      phi1 = seq(from = -1.96, to = 1.96, length = 5),
      phi2 = seq(from = -1.96, to = 1.96, length = 5)
    )
  )

  result <- dose(prob = 0.45, model = dlt_model, samples = samples)
  expected <- c(0.4075, 0.4515, 0, 0.2998, 0.3321)
  expect_equal(result, expected, tolerance = 0.0001)
})

test_that("dose-LogisticIndepBeta works as expected for scalar samples", {
  dlt_model <- h_get_logistic_indep_beta()
  samples <- h_as_samples(list(phi1 = -1, phi2 = 1))

  result <- dose(prob = c(0.45, 0.7), model = dlt_model, samples = samples)
  expect_equal(result, c(2.224049, 6.342658), tolerance = 1e-05)
})

test_that("dose-LogisticIndepBeta throws the error when prob is not a valid scalar", {
  dlt_model <- h_get_logistic_indep_beta()
  samples <- h_as_samples(list(phi1 = c(-1, 0.5), phi2 = c(1, 0.6)))

  expect_error(
    dose(prob = c(40, 50), model = dlt_model, samples = samples),
    "Assertion on 'prob' failed: Must have length 1."
  )
  expect_error(
    dose(prob = 2, model = dlt_model, samples = samples),
    "Assertion on 'prob' failed: Element 1 is not <= 1."
  )
  expect_error(
    dose(prob = -2, model = dlt_model, samples = samples),
    "Assertion on 'prob' failed: Element 1 is not >= 0."
  )
})

## LogisticIndepBeta-noSamples ----

test_that("dose-LogisticIndepBeta-noSamples works as expected", {
  dlt_model <- h_get_logistic_indep_beta()

  result <- dose(prob = c(0.45, 0.55), model = dlt_model)
  expect_equal(result, expected = c(188.1673, 289.2156), tolerance = 0.0001)
})

test_that("dose-LogisticIndepBeta-noSamples throws the error when prob is not a valid scalar", {
  dlt_model <- h_get_logistic_indep_beta()

  expect_error(
    dose(prob = 2, model = dlt_model),
    "Assertion on 'prob' failed: Element 1 is not <= 1."
  )
  expect_error(
    dose(prob = -2, model = dlt_model),
    "Assertion on 'prob' failed: Element 1 is not >= 0."
  )
})

# prob ----

## LogisticNormal ----

test_that("prob-LogisticNormal works as expected", {
  data <- h_get_data()
  model <- h_get_logistic_normal()
  samples <- h_as_samples(list(alpha0 = c(0, -1, 1, 2), alpha1 = c(0, 2, 1, -1)))

  result <- prob(60, model, samples)
  expect_equal(
    result,
    c(0.5, 0.346297, 0.765365, 0.860287),
    tolerance = 1e-06
  )
})

test_that("prob-LogisticNormal works as expected for scalar samples", {
  data <- h_get_data()
  model <- h_get_logistic_normal()
  samples <- h_as_samples(list(alpha0 = 5, alpha1 = 10))

  result <- prob(c(20, 60), model, samples)
  expect_equal(result, c(0.01532378, 0.99891297), tolerance = 1e-05)
})

test_that("prob-LogisticNormal throws the error when dose is not a valid scalar", {
  data <- h_get_data()
  model <- h_get_logistic_normal()
  samples <- h_as_samples(list(alpha0 = c(5, 6), alpha1 = c(10, 11)))

  expect_error(
    prob(dose = c(40, 50), model = model, samples = samples),
    "Assertion on 'dose' failed: Must have length 1."
  )
  expect_error(
    prob(dose = -3, model = model, samples = samples),
    "Assertion on 'dose' failed: Element 1 is not >= 0."
  )
})

## LogisticLogNormal ----

test_that("prob-LogisticLogNormal works as expected", {
  data <- h_get_data()
  model <- h_get_logistic_log_normal()
  samples <- Samples(
    data = list(
      alpha0 = c(0, -1, 1, 2),
      alpha1 = c(0, 2, 1, -1)
    ),
    options = h_get_mcmc_options(small = TRUE, fixed = TRUE)
  )

  result <- prob(60, model, samples)
  expect_equal(
    result,
    c(0.5000000, 0.3462969, 0.7653650, 0.8602873),
    tolerance = 1e-06
  )
})

test_that("prob-LogisticLogNormal works as expected for scalar samples", {
  data <- h_get_data()
  model <- h_get_logistic_log_normal()
  samples <- h_as_samples(list(alpha0 = 5, alpha1 = 10))

  result <- prob(c(20, 60), model, samples)
  expect_equal(result, c(0.01532378, 0.99891297), tolerance = 1e-05)
})

test_that("prob-LogisticLogNormal throws the error when dose is not a valid scalar", {
  data <- h_get_data()
  model <- h_get_logistic_log_normal()
  samples <- h_as_samples(list(alpha0 = c(5, 6), alpha1 = c(10, 11)))

  expect_error(
    prob(dose = c(40, 50), model = model, samples = samples),
    "Assertion on 'dose' failed: Must have length 1."
  )
  expect_error(
    prob(dose = -3, model = model, samples = samples),
    "Assertion on 'dose' failed: Element 1 is not >= 0."
  )
})

## LogisticLogNormalSub ----

test_that("prob-LogisticLogNormalSub works as expected", {
  data <- h_get_data()
  model <- h_get_logistic_log_normal_sub()
  samples <- h_as_samples(list(alpha0 = c(0, -1, 1, 2), alpha1 = c(0, 2, 1, -1)))

  result <- prob(4, model, samples)
  expect_equal(
    result,
    c(0.5, 0.9525741, 0.9525741, 0.5),
    tolerance = 1e-06
  )
})

test_that("prob-LogisticLogNormalSub works as expected for scalar samples", {
  data <- h_get_data()
  model <- h_get_logistic_log_normal_sub()
  samples <- h_as_samples(list(alpha0 = 2, alpha1 = 0.5))

  result <- prob(c(4, 10), model, samples)
  expect_equal(result, c(0.9525741, 0.9975274), tolerance = 1e-05)
})

test_that("prob-LogisticLogNormalSub throws the error when dose is not a valid scalar", {
  data <- h_get_data()
  model <- h_get_logistic_log_normal_sub()
  samples <- h_as_samples(list(alpha0 = c(5, 6), alpha1 = c(10, 11)))

  expect_error(
    prob(dose = c(40, 50), model = model, samples = samples),
    "Assertion on 'dose' failed: Must have length 1."
  )
  expect_error(
    prob(dose = -3, model = model, samples = samples),
    "Assertion on 'dose' failed: Element 1 is not >= 0."
  )
})

## ProbitLogNormal ----

test_that("prob-ProbitLogNormal works as expected", {
  data <- h_get_data()
  model <- h_get_probit_log_normal()
  samples <- h_as_samples(list(alpha0 = c(0, -1, 1, 2), alpha1 = c(0, 2, 1, -1)))

  result <- prob(4, model, samples)
  expect_equal(
    result,
    c(0.5, 0.01479359, 0.65990847, 0.99517026),
    tolerance = 1e-06
  )
})

test_that("prob-ProbitLogNormal works as expected for scalar samples", {
  data <- h_get_data()
  model <- h_get_probit_log_normal()
  samples <- h_as_samples(list(alpha0 = 2, alpha1 = 0.5))

  result <- prob(c(4, 10), model, samples)
  expect_equal(result, c(0.9560059, 0.9847775), tolerance = 1e-05)
})

test_that("prob-ProbitLogNormal throws the error when dose is not a valid scalar", {
  data <- h_get_data()
  model <- h_get_probit_log_normal()
  samples <- h_as_samples(list(alpha0 = c(5, 6), alpha1 = c(10, 11)))

  expect_error(
    prob(dose = c(40, 50), model = model, samples = samples),
    "Assertion on 'dose' failed: Must have length 1."
  )
  expect_error(
    prob(dose = -3, model = model, samples = samples),
    "Assertion on 'dose' failed: Element 1 is not >= 0."
  )
})

## ProbitLogNormalRel ----

test_that("prob-ProbitLogNormalRel works as expected", {
  data <- h_get_data()
  model <- h_get_probit_log_normal_rel()
  samples <- h_as_samples(list(alpha0 = c(0, -1, 1, 2), alpha1 = c(0, 2, 1, -1)))

  result <- prob(4, model, samples)
  expect_equal(
    result,
    c(0.5, 0.9986501, 0.9986501, 0.5),
    tolerance = 1e-06
  )
})

test_that("prob-ProbitLogNormalRel works as expected for scalar samples", {
  data <- h_get_data()
  model <- h_get_probit_log_normal_rel()
  samples <- h_as_samples(list(alpha0 = 2, alpha1 = 0.5))

  result <- prob(c(2, 2.5), model, samples)
  expect_equal(result, c(0.9937903, 0.9956676), tolerance = 1e-05)
})

test_that("prob-ProbitLogNormalRel throws the error when dose is not a valid scalar", {
  data <- h_get_data()
  model <- h_get_probit_log_normal_rel()
  samples <- h_as_samples(list(alpha0 = c(5, 6), alpha1 = c(10, 11)))

  expect_error(
    prob(dose = c(40, 50), model = model, samples = samples),
    "Assertion on 'dose' failed: Must have length 1."
  )
  expect_error(
    prob(dose = -3, model = model, samples = samples),
    "Assertion on 'dose' failed: Element 1 is not >= 0."
  )
})

## LogisticIndepBeta ----

test_that("prob-LogisticIndepBeta works as expected", {
  dlt_model <- h_get_logistic_indep_beta()
  samples <- h_as_samples(
    list(
      phi1 = seq(from = -1.96, to = 1.96, length = 5),
      phi2 = seq(from = -1.96, to = 1.96, length = 5)
    )
  )

  result <- prob(dose = 100, model = dlt_model, samples = samples)
  expected <- c(1.7e-05, 0.004098, 0.5, 0.995902, 0.999983)
  expect_equal(result, expected, tolerance = 0.000001)
})

test_that("prob-LogisticIndepBeta works as expected for scalar samples", {
  dlt_model <- h_get_logistic_indep_beta()
  samples <- h_as_samples(list(phi1 = -1, phi2 = 1))

  result <- prob(dose = c(6, 15), model = dlt_model, samples = samples)
  expect_equal(result, c(0.6882090, 0.8465832), tolerance = 1e-05)
})

test_that("prob-LogisticIndepBeta throws the error when dose is not a valid scalar", {
  dlt_model <- h_get_logistic_indep_beta()
  samples <- h_as_samples(list(phi1 = c(-1, 0.5), phi2 = c(1, 0.6)))

  expect_error(
    prob(dose = c(40, 50), model = dlt_model, samples = samples),
    "Assertion on 'dose' failed: Must have length 1."
  )
  expect_error(
    prob(dose = -3, model = dlt_model, samples = samples),
    "Assertion on 'dose' failed: Element 1 is not >= 0."
  )
})

## LogisticIndepBeta-noSamples ----

test_that("prob-LogisticIndepBeta-noSamples works as expected", {
  dlt_model <- h_get_logistic_indep_beta()

  result <- prob(dose = c(500, 1000), model = dlt_model)
  expect_equal(result, expected = c(0.6708009, 0.7955970), tolerance = 0.000001)
})

test_that("prob-LogisticIndepBeta-noSamples throws the error when dose is not a valid scalar", {
  dlt_model <- h_get_logistic_indep_beta()

  expect_error(
    prob(dose = -3, model = dlt_model),
    "Assertion on 'dose' failed: Element 1 is not >= 0."
  )
})
