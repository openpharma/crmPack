# mcmc-GeneralData ----

test_that("mcmc-GeneralData works as expected", {
  data <- h_get_data()
  model <- h_get_logistic_log_normal()
  options <- McmcOptions(burnin = 100, step = 2, samples = 1000)

  result <- mcmc(data = data, model = model, options = options)

  expect_true(all(slotNames(result) == c("data", "options")))
  expect_identical(result@options, options)
  expect_true(all(names(result@data) == c("alpha0", "alpha1")))
  expect_numeric(result@data$alpha0, len = 1000, any.missing = FALSE, finite = TRUE)
  expect_numeric(result@data$alpha1, len = 1000, any.missing = FALSE, finite = TRUE)
})

test_that("mcmc-GeneralData gets random results", {
  data <- h_get_data()
  model <- h_get_logistic_log_normal()
  options <- McmcOptions(burnin = 100, step = 2, samples = 1000)

  result_1 <- mcmc(data = data, model = model, options = options)
  result_2 <- mcmc(data = data, model = model, options = options)

  # Should differ due to randomness.
  expect_false(all(result_1@data$alpha0 == result_2@data$alpha0))
  expect_false(all(result_1@data$alpha1 == result_2@data$alpha1))
})

test_that("mcmc-GeneralData respects fixed RNG settings", {
  data <- h_get_data()
  model <- h_get_logistic_log_normal()
  options <- McmcOptions(
    burnin = 100,
    step = 2,
    samples = 1000,
    rng_kind = "Mersenne-Twister",
    rng_seed = 1
  )

  result_1 <- mcmc(data = data, model = model, options = options)
  result_2 <- mcmc(data = data, model = model, options = options)

  # Must not differ due to fixed seed.
  expect_true(all(result_1@data$alpha0 == result_2@data$alpha0))
  expect_true(all(result_1@data$alpha1 == result_2@data$alpha1))
})

test_that("mcmc-GeneralData computes correct values for LogisticLogNormal model", {
  data <- h_get_data()
  model <- h_get_logistic_log_normal()
  options <- McmcOptions(
    burnin = 2,
    step = 2,
    samples = 4,
    rng_kind = "Mersenne-Twister",
    rng_seed = 1
  )
  
  result <- mcmc(data = data, model = model, options = options)
  
  expect_equal(
    result@data,
    list(
      alpha0 = c(-0.4327469, -0.6748706, -0.9834536, -0.3192341),
      alpha1 = c(0.7570043, 0.3950696, 0.5885619, 1.9444664)
    ),
    tolerance = 1e-07
  )
})
