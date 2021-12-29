# dose-Model ----

test_that("dose-Model works as expected", {
  model <- LogisticLogNormal(
    mean = c(-0.85, 1),
    cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
    refDose = 56
  )
  samples <- Samples(
    data = list(
      alpha0 = seq(from = -1.96, to = 1.96, length = 5),
      alpha1 = seq(from = -1.96, to = 1.96, length = 5)
    ),
    options = McmcOptions(burnin = 2L, step = 1L, samples = 5L)
  )
  result <- dose(prob = 0.45, model = model, samples = samples)
  expected <- c(22.82, 25.28, 0, 16.79, 18.60)

  expect_equal(result, expected, tolerance = 0.001)
})

# dose-ModelTox ----

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

# dose-ModelTox_noSamples ----

test_that("dose-ModelTox works as expected when no samples", {
  dlt_model <- LogisticIndepBeta(
    binDLE = c(1.05, 1.8),
    DLEweights = c(3, 3),
    DLEdose = c(25, 300),
    data = h_get_data()
  )
  result <- dose(prob = 0.45, model = dlt_model)

  expect_equal(result, expected = 188.1673, tolerance = 0.0001)
})

# prob-Model ----

test_that("prob-Model works as expected", {
  model <- LogisticLogNormal(
    mean = c(-0.85, 1),
    cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
    refDose = 56
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

# prob-ModelTox ----

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

# prob-ModelTox_noSamples ----

test_that("prob-ModelTox works as expected when no samples", {
  dlt_model <- LogisticIndepBeta(
    binDLE = c(1.05, 1.8),
    DLEweights = c(3, 3),
    DLEdose = c(25, 300),
    data = h_get_data_dual()
  )
  result <- prob(dose = 1000, model = dlt_model)

  expect_equal(result, expected = 0.795597, tolerance = 0.000001)
})
