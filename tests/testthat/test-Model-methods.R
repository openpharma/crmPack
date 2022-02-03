# dose ----

## LogisticNormal ----

test_that("dose computes correct values for LogisticNormal model", {
  data <- h_get_data()
  model <- h_get_logistic_normal()
  samples <- Samples(
    data = list(
      alpha0 = c(0, -1, 1, 2),
      alpha1 = c(0, 2, 1, -1)
    ),
    options = h_get_mcmc_options(small = TRUE, fixed = TRUE)
  )

  result <- dose(0.4, model, samples)
  expect_equal(
    result,
    c(0.00000, 67.30876, 12.26265, 554.17921),
    tolerance = 1e-05
  )
})

## LogisticLogNormal ----

test_that("dose computes correct values for LogisticLogNormal model", {
  data <- h_get_data()
  model <- h_get_logistic_log_normal()
  samples <- Samples(
    data = list(
      alpha0 = c(0, -1, 1, 2),
      alpha1 = c(0, 2, 1, -1)
    ),
    options = h_get_mcmc_options(small = TRUE, fixed = TRUE)
  )

  result <- dose(0.4, model, samples)
  expect_equal(
    result,
    c(0.00000, 67.30876, 12.26265, 554.17921),
    tolerance = 1e-05
  )
})

## LogisticLogNormalSub ----

test_that("dose computes correct values for LogisticLogNormalSub model", {
  data <- h_get_data()
  model <- h_get_logistic_log_normal_sub()
  samples <- Samples(
    data = list(
      alpha0 = c(0, -1, 1, 2),
      alpha1 = c(0, 2, 1, -1)
    ),
    options = h_get_mcmc_options(small = TRUE, fixed = TRUE)
  )

  result <- dose(0.4, model, samples)
  expect_equal(
    result,
    c(-Inf, 2.2972674, 0.5945349, 4.4054651),
    tolerance = 1e-05
  )
})

## ProbitLogNormal ----

test_that("dose computes correct values for ProbitLogNormal model", {
  data <- h_get_data()
  model <- h_get_probit_log_normal()
  samples <- Samples(
    data = list(
      alpha0 = c(0, -1, 1, 2),
      alpha1 = c(0, 2, 1, -1)
    ),
    options = h_get_mcmc_options(small = TRUE, fixed = TRUE)
  )

  result <- dose(0.4, model, samples)
  expect_equal(
    result,
    c(0, 10.458421, 2.055942, 68.540727),
    tolerance = 1e-05
  )
})

## ProbitLogNormalRel ----

test_that("dose computes correct values for ProbitLogNormalRel model", {
  data <- h_get_data()
  model <- h_get_probit_log_normal_rel()
  samples <- Samples(
    data = list(
      alpha0 = c(0, -1, 1, 2),
      alpha1 = c(0, 2, 1, -1)
    ),
    options = h_get_mcmc_options(small = TRUE, fixed = TRUE)
  )

  result <- dose(0.4, model, samples)
  expect_equal(
    result,
    c(-Inf, 0.7466529, -2.5066942, 4.5066942),
    tolerance = 1e-05
  )
})

## ModelTox ----

test_that("dose-ModelTox works as expected", {
  dlt_model <- LogisticIndepBeta(
    binDLE = c(1.05, 1.8),
    DLEweights = c(3, 3),
    DLEdose = c(25, 300),
    data = h_get_data()
  )
  samples <- Samples(
    data = list(
      phi1 = seq(from = -1.96, to = 1.96, length = 5),
      phi2 = seq(from = -1.96, to = 1.96, length = 5)
    ),
    options = McmcOptions(burnin = 2L, step = 1L, samples = 5L)
  )
  result <- dose(prob = 0.45, model = dlt_model, samples = samples)
  expected <- c(0.4075, 0.4515, 0, 0.2998, 0.3321)

  expect_equal(result, expected, tolerance = 0.0001)
})

## ModelTox_noSamples ----

test_that("dose-ModelTox works as expected when no samples", {
  dlt_model <- LogisticIndepBeta(
    binDLE = c(1.05, 1.8),
    DLEweights = c(3, 3),
    DLEdose = c(25, 300),
    data = h_get_data()
  )
  result <- dose(prob = c(0.45, 0.55), model = dlt_model)

  expect_equal(result, expected = c(188.1673, 289.2156), tolerance = 0.0001)
})

# prob ----

## Model ----

test_that("prob-Model works as expected", {
  model <- LogisticLogNormal(
    mean = c(-0.85, 1),
    cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
    ref_dose = 56
  )
  samples <- Samples(
    data = list(
      alpha0 = seq(from = -1.96, to = 1.96, length = 5),
      alpha1 = seq(from = -1.96, to = 1.96, length = 5)
    ),
    options = McmcOptions(burnin = 2L, step = 1L, samples = 5L)
  )
  result <- prob(dose = 50, model = model, samples = samples)
  expected <- c(0.1496, 0.2955, 0.5, 0.7045, 0.8504)

  expect_equal(result, expected, tolerance = 0.0001)
})

test_that("prob-Model throws the error when dose is not a scalar", {
  model <- LogisticLogNormal(
    mean = c(-0.85, 1),
    cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
    ref_dose = 56
  )
  samples <- Samples(
    data = list(
      alpha0 = seq(from = -1.96, to = 1.96, length = 5),
      alpha1 = seq(from = -1.96, to = 1.96, length = 5)
    ),
    options = McmcOptions(burnin = 2L, step = 1L, samples = 5L)
  )
  expect_error(
    prob(dose = c(40, 50), model = model, samples = samples),
    "Assertion on 'dose' failed: Must have length 1."
  )
})

## LogisticNormal ----

test_that("prob computes correct values for LogisticNormal model", {
  data <- h_get_data()
  model <- h_get_logistic_normal()
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
    c(0.5, 0.346297, 0.765365, 0.860287),
    tolerance = 1e-06
  )
})

## LogisticLogNormal ----

test_that("prob computes correct values for LogisticLogNormal model", {
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

## LogisticLogNormalSub ----

test_that("prob computes correct values for LogisticLogNormalSub model", {
  data <- h_get_data()
  model <- h_get_logistic_log_normal_sub()
  samples <- Samples(
    data = list(
      alpha0 = c(0, -1, 1, 2),
      alpha1 = c(0, 2, 1, -1)
    ),
    options = h_get_mcmc_options(small = TRUE, fixed = TRUE)
  )

  result <- prob(4, model, samples)
  expect_equal(
    result,
    c(0.5, 0.9525741, 0.9525741, 0.5),
    tolerance = 1e-06
  )
})

## ProbitLogNormal ----

test_that("prob computes correct values for ProbitLogNormal model", {
  data <- h_get_data()
  model <- h_get_probit_log_normal()
  samples <- Samples(
    data = list(
      alpha0 = c(0, -1, 1, 2),
      alpha1 = c(0, 2, 1, -1)
    ),
    options = h_get_mcmc_options(small = TRUE, fixed = TRUE)
  )

  result <- prob(4, model, samples)
  expect_equal(
    result,
    c(0.5, 0.01479359, 0.65990847, 0.99517026),
    tolerance = 1e-06
  )
})

## ProbitLogNormalRel ----

test_that("prob computes correct values for ProbitLogNormalRel model", {
  data <- h_get_data()
  model <- h_get_probit_log_normal_rel()
  samples <- Samples(
    data = list(
      alpha0 = c(0, -1, 1, 2),
      alpha1 = c(0, 2, 1, -1)
    ),
    options = h_get_mcmc_options(small = TRUE, fixed = TRUE)
  )

  result <- prob(4, model, samples)
  expect_equal(
    result,
    c(0.5, 0.9986501, 0.9986501, 0.5),
    tolerance = 1e-06
  )
})

## ModelTox ----

test_that("prob-ModelTox works as expected", {
  dlt_model <- LogisticIndepBeta(
    binDLE = c(1.05, 1.8),
    DLEweights = c(3, 3),
    DLEdose = c(25, 300),
    data = h_get_data_dual()
  )
  samples <- Samples(
    data = list(
      phi1 = seq(from = -1.96, to = 1.96, length = 5),
      phi2 = seq(from = -1.96, to = 1.96, length = 5)
    ),
    options = McmcOptions(burnin = 2L, step = 1L, samples = 5L)
  )
  result <- prob(dose = 100, model = dlt_model, samples = samples)
  expected <- c(1.7e-05, 0.004098, 0.5, 0.995902, 0.999983)

  expect_equal(result, expected, tolerance = 0.000001)
})

## ModelTox_noSamples ----

test_that("prob-ModelTox works as expected when no samples", {
  dlt_model <- LogisticIndepBeta(
    binDLE = c(1.05, 1.8),
    DLEweights = c(3, 3),
    DLEdose = c(25, 300),
    data = h_get_data_dual()
  )
  result <- prob(dose = c(500, 1000), model = dlt_model)

  expect_equal(result, expected = c(0.6708009, 0.7955970), tolerance = 0.000001)
})
