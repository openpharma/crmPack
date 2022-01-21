# mcmc-GeneralData ----

test_that("mcmc-GeneralData works as expected", {
  data <- h_get_data()
  model <- h_get_logistic_log_normal()
  options <- h_get_mcmc_options()

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
  options <- h_get_mcmc_options()

  result_1 <- mcmc(data = data, model = model, options = options)
  result_2 <- mcmc(data = data, model = model, options = options)

  # Should differ due to randomness.
  expect_false(all(result_1@data$alpha0 == result_2@data$alpha0))
  expect_false(all(result_1@data$alpha1 == result_2@data$alpha1))
})

test_that("mcmc-GeneralData respects fixed RNG settings", {
  data <- h_get_data()
  model <- h_get_logistic_log_normal()
  options <- h_get_mcmc_options(fixed = TRUE)

  result_1 <- mcmc(data = data, model = model, options = options)
  result_2 <- mcmc(data = data, model = model, options = options)

  # Must not differ due to fixed seed.
  expect_true(all(result_1@data$alpha0 == result_2@data$alpha0))
  expect_true(all(result_1@data$alpha1 == result_2@data$alpha1))
})

test_that("mcmc-GeneralData computes correct values for LogisticNormal model", {
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

test_that("mcmc-GeneralData computes correct values for LogisticLogNormal model", {
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
