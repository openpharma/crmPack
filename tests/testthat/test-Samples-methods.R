# nolint start

# size ----

## Samples ----

test_that("size-Samples returns correct number of samples", {
  samples <- h_as_samples(list(alpha0 = c(0, -1, 1, 2), alpha1 = c(0, 2, 1, -1)))
  samples2 <- h_as_samples(
    list(alpha0 = seq(from = 1, length.out = 50), alpha1 = seq(from = 60, length.out = 50))
  )
  expect_identical(size(samples), 4L)
  expect_identical(size(samples2), 50L)
})

# names ----

## Samples ----

test_that("names-Samples returns correct names of the parameters", {
  samples <- h_as_samples(list(alpha0 = c(0, -1), alpha1 = c(2, 1), beta = c(4, 7)))
  samples2 <- h_as_samples(
    list(a = matrix(5:8, nrow = 2), z = matrix(1:4, nrow = 2))
  )
  expect_identical(names(samples), c("alpha0", "alpha1", "beta"))
  expect_identical(names(samples2), c("a", "z"))
})

# get ----

## Samples ----

test_that("get-Samples fails gracefully with bad input", {
  samples <- Samples(data = list(good = 1:3), options = McmcOptions(samples = 3))
  expect_error(
    get(samples, "bad"),
    "Assertion on 'pos' failed: Must be element of set \\{'good'\\}, but is 'bad'."
  )
  expect_error(
    get(samples, c("bad", "worse")),
    "Assertion on 'pos' failed: Must have length 1."
  )

  dualSamples <- Samples(data = list(good = matrix(1:6, ncol = 2)), options = McmcOptions(samples = 3))
  expect_error(
    get(dualSamples, "good", envir = "NotNumeric"),
    "Assertion on 'envir' failed: Must be of type 'integer', not 'character'."
  )
  expect_error(
    get(dualSamples, "good", envir = pi),
    "Assertion on 'envir' failed: Must be of type 'integer', not 'double'."
  )
  expect_error(
    get(dualSamples, "good", envir = 99L),
    "Assertion on 'envir' failed: Must be a subset of \\{'1','2'\\}, but has additional elements \\{'99'\\}"
  )
})

test_that("get-Samples returns correct values", {
  mcmcOptions <- McmcOptions(samples = 3)
  samples <- Samples(data = list(alpha0 = 1:3, alpha1 = 4:6), options = mcmcOptions)

  for (param in names(samples@data)) {
    expected <- data.frame(
      Iteration = as.integer(1:3),
      Chain = 1L,
      Parameter = as.factor(param),
      value = as.double(samples@data[[param]])
    )

    attr(expected, "description") <- param
    attr(expected, "nBurnin") <- mcmcOptions@burnin
    attr(expected, "nChains") <- 1L
    attr(expected, "nParameters") <- 1L
    attr(expected, "nThin") <- mcmcOptions@step
    attr(expected, "nIterations") <- mcmcOptions@iterations
    attr(expected, "parallel") <- FALSE

    expect_identical(get(samples, param), expected)
  }

  dualData <- DataDual(doseGrid = c(seq(from = 10, to = 80, by = 10)))
  dualModel <- DualEndpointRW(
    mean = c(0, 1),
    cov = matrix(c(1, 0, 0, 1), nrow = 2),
    sigma2betaW = 0.01,
    sigma2W = c(a = 0.1, b = 0.1),
    rho = c(a = 1, b = 1),
    rw1 = TRUE
  )
  mcmcOptions <- McmcOptions(burnin = 5, step = 2, samples = 2)
  set.seed(94)
  dualSamples <- mcmc(dualData, dualModel, mcmcOptions)
  for (param in c("betaZ")) { # names(dualSamples@data)) {
    actual <- get(dualSamples, param)
    assert_data_frame(actual)
    assert_set_equal(names(actual), c("Iteration", "Chain", "Parameter", "value"))
    expected <- data.frame(
      Iteration = rep(
        1:((mcmcOptions@iterations - mcmcOptions@burnin) / mcmcOptions@step),
        times = ncol(dualSamples@data[[param]])
      ),
      Chain = 1L,
      Parameter = as.factor(
        paste0(
          param,
          "[",
          rep(
            1:ncol(dualSamples@data[[param]]),
            each = (mcmcOptions@iterations - mcmcOptions@burnin) / mcmcOptions@step
          ),
          "]"
        )
      ),
      value = matrix(dualSamples@data[[param]], ncol = 1)
    )
    attr(expected, "description") <- paste0(
      param,
      "[",
      1:ncol(dualSamples@data[[param]]),
      "]"
    )
    attr(expected, "nBurnin") <- mcmcOptions@burnin
    attr(expected, "nChains") <- 1L
    attr(expected, "nParameters") <- as.integer(ncol(dualSamples@data[[param]]))
    attr(expected, "nThin") <- mcmcOptions@step
    attr(expected, "nIterations") <- mcmcOptions@iterations
    attr(expected, "parallel") <- FALSE
    expect_identical(actual, expected)
  }
})

# fit ----

## Samples-GeneralModel ----

test_that("fit-Samples fails gracefully with bad inputs", {
  mcmcOptions <- McmcOptions(samples = 3)
  samples <- Samples(data = list(alpha0 = 1:3, alpha1 = 4:6), options = mcmcOptions)
  model <- LogisticLogNormal(
    mean = c(-0.85, 1),
    cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
    ref_dose = 56
  )
  emptyData <- Data(doseGrid = seq(10, 80, 10))

  expect_error(
    fit(samples, model, emptyData, quantiles = c(0.025, 99)),
    "Assertion on 'quantiles' failed: Probability must be within \\[0, 1\\] bounds but it is not."
  )
  expect_error(
    fit(samples, model, emptyData, points = "A"),
    "Assertion on 'points' failed: Must be of type 'numeric', not 'character'."
  )
})

test_that("fit-Samples works correctly for tox-only models", {
  checkIt <- function(middleFunc = mean, lowerQuantile = 0.025, upperQuantile = 0.975, tolerance = 1e-06, seed) {
    mcmcOptions <- McmcOptions()
    sampleCount <- (mcmcOptions@iterations - mcmcOptions@burnin) / mcmcOptions@step

    model <- LogisticLogNormal(
      mean = c(-0.85, 1),
      cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
      ref_dose = 56
    )
    emptyData <- Data(doseGrid = seq(10, 80, 10))
    samples <- Samples(
      data = list(
        alpha0 = rnorm(sampleCount, mean = model@params@mean[1], sd = model@params@cov[1, 1]),
        alpha1 = rnorm(sampleCount, mean = model@params@mean[2], sd = model@params@cov[2, 2])
      ),
      mcmcOptions
    )
    actual <- fit(
      samples,
      model,
      emptyData,
      middle = middleFunc,
      quantiles = c(lowerQuantile, upperQuantile)
    )

    expected <- tibble::tibble(
      alpha0 = samples@data$alpha0,
      alpha1 = samples@data$alpha1
    ) %>%
      tidyr::expand(tidyr::nesting(alpha0, alpha1), dose = emptyData@doseGrid) %>%
      dplyr::mutate(
        Z = exp(alpha0 + alpha1 * log(dose / model@ref_dose)),
        Prob = Z / (1 + Z)
      ) %>%
      dplyr::group_by(dose) %>%
      dplyr::summarise(
        middle = middleFunc(Prob),
        lower = quantile(Prob, probs = lowerQuantile),
        upper = quantile(Prob, probs = upperQuantile),
        .groups = "drop"
      ) %>%
      as.data.frame()

    expect_equal(actual, expected, tolerance = 1e-06)
  }

  checkIt(seed = 123)
  checkIt(seed = 456, middleFunc = median)
  checkIt(seed = 789, lowerQuantile = 0.25, upperQuantile = 0.75)
})

## Samples-DataModel ----

test_that("fit-Samples works correctly for dual models", {
  # TODO: Check for numerical correctness
  dualData <- DataDual(
    ID = 1L:12L,
    cohort = c(6, 6, 6, 7, 7, 7, 8, 8, 8, 9, 9, 9),
    x = c(10, 10, 10, 20, 20, 20, 40, 40, 40, 50, 50, 50),
    y = c(0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1),
    w = c(0.7, 0.55, 0.6, 0.52, 0.54, 0.56, 0.43, 0.41, 0.39, 0.34, 0.38, 0.21),
    doseGrid = c(seq(from = 10, to = 80, by = 10))
  )

  model <- DualEndpointRW(
    mean = c(0, 1),
    cov = matrix(c(1, 0, 0, 1), nrow = 2),
    sigma2betaW = 0.01,
    sigma2W = c(a = 0.1, b = 0.1),
    rho = c(a = 1, b = 1),
    rw1 = TRUE
  )

  options <- McmcOptions(rng_kind = "Mersenne-Twister", rng_seed = 1234567)
  samples <- mcmc(dualData, model, options)

  actual <- fit(samples, model, dualData)

  expect_equal(class(actual), "data.frame")
  expect_setequal(names(actual), c("dose", "middle", "lower", "upper", "middleBiomarker", "lowerBiomarker", "upperBiomarker"))
  expect_snapshot(actual)
})

# approximate ----

## Samples-GeneralModel ----

test_that("Samples-approximate works correctly", {
  data <- Data(
    x = c(3, 6, 10, 10, 10),
    y = c(0, 0, 0, 1, 0),
    ID = 1L:5L,
    cohort = c(3, 4, 5, 5, 5),
    doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2))
  )

  model <- LogisticLogNormal(
    mean = c(-0.85, 1),
    cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
    ref_dose = 56
  )

  options <- McmcOptions(burnin = 100, step = 2, samples = 2000, rng_kind = "Mersenne-Twister", rng_seed = 303010)
  samples <- mcmc(data, model, options)

  posterior <- approximate(
    object = samples,
    model = model,
    data = data,
    logNormal = TRUE,
    control = list(threshold.stop = 0.1, max.time = 1, maxit = 1),
    verbose = FALSE
  )
  expect_snapshot_value(posterior, style = "serialize")

  model1 <- LogisticNormal(
    mean = c(-0.85, 1),
    cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
    ref_dose = 56
  )

  posterior1 <- approximate(
    object = samples,
    model = model1,
    data = data,
    logNormal = TRUE,
    control = list(threshold.stop = 0.1, max.time = 1, maxit = 1)
  )
  expect_snapshot_value(posterior1, style = "serialize")

  posterior2 <- approximate(
    object = samples,
    model = model1,
    data = data,
    logNormal = FALSE,
    control = list(threshold.stop = 0.1, max.time = 1, maxit = 1)
  )
  expect_snapshot_value(posterior2, style = "serialize")
})

# plot ----

## Samples-GeneralModel ----

test_that("plot-Samples fails gracefully with bad input", {
  data <- Data(
    x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
    y = c(0, 0, 0, 0, 0, 0, 1, 0),
    ID = 1L:8L,
    cohort = c(0, 1, 2, 3, 4, 5, 5, 5),
    doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2))
  )
  model <- LogisticLogNormal(
    mean = c(-0.85, 1),
    cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
    ref_dose = 56
  )
  options <- McmcOptions(burnin = 100, step = 2, samples = 2000, rng_kind = "Mersenne-Twister", rng_seed = 303010)
  samples <- mcmc(data, model, options)
  expect_error(
    plot(x = samples, y = model, data = data, showLegend = "NotLogical"),
    "Assertion on 'showLegend' failed: Must be of type 'logical', not 'character'."
  )
})

test_that("plot-Samples works correctly", {
  data <- Data(
    x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
    y = c(0, 0, 0, 0, 0, 0, 1, 0),
    ID = 1L:8L,
    cohort = c(0, 1, 2, 3, 4, 5, 5, 5),
    doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2))
  )
  model <- LogisticLogNormal(
    mean = c(-0.85, 1),
    cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
    ref_dose = 56
  )
  options <- McmcOptions(burnin = 100, step = 2, samples = 2000, rng_kind = "Mersenne-Twister", rng_seed = 303010)
  samples <- mcmc(data, model, options)

  actual <- plot(x = samples, y = model, data = data)
  vdiffr::expect_doppelganger("plot-Samples", actual)

  actual1 <- plot(x = samples, y = model, data = data, showLegend = FALSE)
  vdiffr::expect_doppelganger("plot-Samples_showLegend-FALSE", actual1)
})

test_that("plot-Samples-DualEndpoint fails gracefully with bad input", {
  data <- DataDual(
    x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10, 20, 20, 20, 40, 40, 40, 50, 50, 50),
    y = c(0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1),
    w = c(0.31, 0.42, 0.59, 0.45, 0.6, 0.7, 0.55, 0.6, 0.52, 0.54, 0.56, 0.43, 0.41, 0.39, 0.34, 0.38, 0.21),
    doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2)),
    ID = 1L:17L,
    cohort = as.integer(c(1:5, rep(6:9, each = 3)))
  )
  model <- DualEndpointRW(
    mean = c(0, 1),
    cov = matrix(c(1, 0, 0, 1), nrow = 2),
    sigma2betaW = 0.01,
    sigma2W = c(a = 0.1, b = 0.1),
    rho = c(a = 1, b = 1),
    rw1 = TRUE
  )
  options <- McmcOptions(burnin = 100, step = 2, samples = 2000, rng_kind = "Mersenne-Twister", rng_seed = 393015)
  samples <- mcmc(data, model, options)

  expect_error(
    plot(x = samples, y = model, data = data, extrapolate = "NotLogical"),
    "Assertion on 'extrapolate' failed: Must be of type 'logical', not 'character'."
  )
})

## Samples-DualEndpoint ----

test_that("plot-Samples-DualEndpoint works correctly", {
  data <- DataDual(
    x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10, 20, 20, 20, 40, 40, 40, 50, 50, 50),
    y = c(0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1),
    w = c(0.31, 0.42, 0.59, 0.45, 0.6, 0.7, 0.55, 0.6, 0.52, 0.54, 0.56, 0.43, 0.41, 0.39, 0.34, 0.38, 0.21),
    doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2)),
    ID = 1L:17L,
    cohort = as.integer(c(1:5, rep(6:9, each = 3)))
  )
  model <- DualEndpointRW(
    mean = c(0, 1),
    cov = matrix(c(1, 0, 0, 1), nrow = 2),
    sigma2betaW = 0.01,
    sigma2W = c(a = 0.1, b = 0.1),
    rho = c(a = 1, b = 1),
    rw1 = TRUE
  )
  options <- McmcOptions(burnin = 100, step = 2, samples = 2000, rng_kind = "Mersenne-Twister", rng_seed = 393015)
  samples <- mcmc(data, model, options)

  actual <- plot(x = samples, y = model, data = data)
  vdiffr::expect_doppelganger("plot-Samples-DataDual", actual)

  actual1 <- plot(x = samples, y = model, data = data, showLegend = FALSE)
  vdiffr::expect_doppelganger("plot-Samples-DataDual_showlegend-FALSE", actual1)
})

## Samples-LogisticIndepBeta ----

test_that("fit-Samples-LogisticIndepBeta fails gracefully with bad input", {
  data <- Data(
    ID = 1L:8L,
    cohort = as.integer(c(1, 2, 2, 3, 4, 5, 6, 7)),
    x = c(25, 50, 50, 75, 150, 200, 225, 300),
    y = c(0, 0, 0, 0, 1, 1, 1, 1),
    doseGrid = seq(from = 25, to = 300, by = 25)
  )
  model <- LogisticIndepBeta(binDLE = c(1.05, 1.8), DLEweights = c(3, 3), DLEdose = c(25, 300), data = data)
  options <- McmcOptions(burnin = 500, step = 2, samples = 2000, rng_kind = "Mersenne-Twister", rng_seed = 405017)
  samples <- mcmc(data, model, options)
  expect_error(
    fit(object = samples, model = model, data = data, points = "NotNumeric"),
    "Assertion on 'points' failed: Must be of type 'numeric', not 'character'."
  )
  expect_error(
    fit(object = samples, model = model, data = data, quantiles = c(0.1, 99)),
    "Assertion on 'quantiles' failed: Probability must be within \\[0, 1\\] bounds but it is not."
  )
  expect_error(
    fit(object = samples, model = model, data = data, quantiles = c(0.1, 0.2, 0.3)),
    "Assertion on 'quantiles' failed: Must have length 2, but has length 3."
  )
})

# fit ----

## Samples-LogisticIndepBeta ----

test_that("fit-Samples-LogisticIndepBeta works", {
  data <- Data(
    ID = 1L:8L,
    cohort = as.integer(c(1, 2, 2, 3, 4, 5, 6, 7)),
    x = c(25, 50, 50, 75, 150, 200, 225, 300),
    y = c(0, 0, 0, 0, 1, 1, 1, 1),
    doseGrid = seq(from = 25, to = 300, by = 25)
  )
  model <- LogisticIndepBeta(binDLE = c(1.05, 1.8), DLEweights = c(3, 3), DLEdose = c(25, 300), data = data)
  options <- McmcOptions(burnin = 500, step = 2, samples = 2000, rng_kind = "Mersenne-Twister", rng_seed = 405017)
  samples <- mcmc(data, model, options)

  actual <- fit(object = samples, model = model, data = data, quantiles = c(0.1, 0.9))
  expect_snapshot(actual)
})

## Samples-Effloglog ----

test_that("fit-Samples-Effloglog works correctly", {
  data <- DataDual(
    x = c(25, 50, 25, 50, 75, 300, 250, 150),
    y = c(0, 0, 0, 0, 0, 1, 1, 0),
    w = c(0.31, 0.42, 0.59, 0.45, 0.6, 0.7, 0.6, 0.52),
    doseGrid = seq(25, 300, 25),
    placebo = FALSE,
    ID = 1L:8L,
    cohort = 1L:8L
  )
  model <- Effloglog(c(1.223, 2.513), c(25, 300), nu = c(a = 1, b = 0.025), data = data, c = 0)
  options <- McmcOptions(burnin = 100, step = 2, samples = 200)
  samples <- mcmc(
    data = data,
    model = model,
    options = options,
    rng_kind = "Mersenne-Twister",
    rng_seed = 303012
  )
  actual <- fit(object = samples, model = model, data = data)
  expect_snapshot(actual)

  actual1 <- fit(object = samples, model = model, data = data, middle = median)
  expect_snapshot(actual1)
})

test_that("fit-Samples-Effloglog fails gracefully with bad input", {
  data <- DataDual(
    x = c(25, 50, 25, 50, 75, 300, 250, 150),
    y = c(0, 0, 0, 0, 0, 1, 1, 0),
    w = c(0.31, 0.42, 0.59, 0.45, 0.6, 0.7, 0.6, 0.52),
    doseGrid = seq(25, 300, 25),
    placebo = FALSE,
    ID = 1L:8L,
    cohort = 1L:8L
  )
  model <- Effloglog(c(1.223, 2.513), c(25, 300), nu = c(a = 1, b = 0.025), data = data, c = 0)
  options <- McmcOptions(burnin = 100, step = 2, samples = 200)
  samples <- mcmc(
    data = data,
    model = model,
    options = options,
    rng_kind = "Mersenne-Twister",
    rng_seed = 303012
  )
  expect_error(
    fit(object = samples, model = model, data = data, points = "NotNumeric"),
    "Assertion on 'points' failed: Must be of type 'numeric', not 'character'."
  )
  expect_error(
    fit(object = samples, model = model, data = data, quantiles = c(0.1, 99)),
    "Assertion on 'quantiles' failed: Probability must be within \\[0, 1\\] bounds but it is not."
  )
  expect_error(
    fit(object = samples, model = model, data = data, quantiles = c(0.1, 0.2, 0.3)),
    "Assertion on 'quantiles' failed: Must have length 2, but has length 3."
  )
})

## Samples-EffFlexi ----

test_that("fit-Samples-EffFlexi works correctly", {
  data <- DataDual(
    x = c(25, 50, 25, 50, 75, 300, 250, 150),
    y = c(0, 0, 0, 0, 0, 1, 1, 0),
    w = c(0.31, 0.42, 0.59, 0.45, 0.6, 0.7, 0.6, 0.52),
    doseGrid = seq(25, 300, 25),
    placebo = FALSE,
    ID = 1L:8L,
    cohort = 1L:8L
  )
  model <- EffFlexi(
    eff = c(1.223, 2.513), eff_dose = c(25, 300),
    sigma2W = c(a = 0.1, b = 0.1), sigma2betaW = c(a = 20, b = 50), rw1 = FALSE, data = data
  )
  options <- McmcOptions(
    burnin = 1000,
    step = 2,
    samples = 10000,
    rng_kind = "Mersenne-Twister",
    rng_seed = 574712
  )
  samples <- mcmc(data = data, model = model, options = options)

  actual <- fit(object = samples, model = model, data = data)
  expect_snapshot(actual)

  actual1 <- fit(object = samples, model = model, data = data, middle = median)
  expect_snapshot(actual1)
})

test_that("fit-Samples-EffFlexi fails gracefully with bad input", {
  data <- DataDual(
    x = c(25, 50, 25, 50, 75, 300, 250, 150),
    y = c(0, 0, 0, 0, 0, 1, 1, 0),
    w = c(0.31, 0.42, 0.59, 0.45, 0.6, 0.7, 0.6, 0.52),
    doseGrid = seq(25, 300, 25),
    placebo = FALSE,
    ID = 1L:8L,
    cohort = 1L:8L
  )
  model <- EffFlexi(
    eff = c(1.223, 2.513), eff_dose = c(25, 300),
    sigma2W = c(a = 0.1, b = 0.1), sigma2betaW = c(a = 20, b = 50), rw1 = FALSE, data = data
  )
  options <- McmcOptions(burnin = 100, step = 2, samples = 200)
  options <- McmcOptions(
    burnin = 100,
    step = 2,
    samples = 200,
    rng_kind = "Mersenne-Twister",
    rng_seed = 574712
  )
  samples <- mcmc(data = data, model = model, options = options)

  expect_error(
    fit(object = samples, model = model, data = data, points = "NotNumeric"),
    "Assertion on 'points' failed: Must be of type 'numeric', not 'character'."
  )
  expect_error(
    fit(object = samples, model = model, data = data, quantiles = c(0.1, 99)),
    "Assertion on 'quantiles' failed: Probability must be within \\[0, 1\\] bounds but it is not."
  )
  expect_error(
    fit(object = samples, model = model, data = data, quantiles = c(0.1, 0.2, 0.3)),
    "Assertion on 'quantiles' failed: Must have length 2, but has length 3."
  )
})

# fitGain ----

## Samples ----

test_that("fitGain-Samples works correctly", {
  data <- DataDual(
    x = c(25, 50, 25, 50, 75, 300, 250, 150),
    y = c(0, 0, 0, 0, 0, 1, 1, 0),
    w = c(0.31, 0.42, 0.59, 0.45, 0.6, 0.7, 0.6, 0.52),
    doseGrid = seq(25, 300, 25),
    placebo = FALSE,
    ID = 1L:8L,
    cohort = 1L:8L
  )
  DLEmodel <- LogisticIndepBeta(binDLE = c(1.05, 1.8), DLEweights = c(3, 3), DLEdose = c(25, 300), data = data)

  Effmodel <- Effloglog(c(1.223, 2.513), c(25, 300), nu = c(a = 1, b = 0.025), data = data, c = 0)
  options <- McmcOptions(
    burnin = 500,
    step = 2,
    samples = 5000,
    rng_kind = "Mersenne-Twister",
    rng_seed = 195612
  )

  data1 <- Data(
    x = data@x, y = data@y,
    doseGrid = data@doseGrid,
    ID = 1L:8L,
    cohort = 1L:8L
  )

  DLEsamples <- mcmc(data = data1, model = DLEmodel, options = options)
  Effsamples <- mcmc(data = data, model = Effmodel, options = options)

  actual <- fitGain(
    DLEmodel = DLEmodel, DLEsamples = DLEsamples,
    Effmodel = Effmodel, Effsamples = Effsamples, data = data
  )

  expect_snapshot(actual)
})

test_that("fitGain-Samples fails gracefully with bad input", {
  data <- DataDual(
    x = c(25, 50, 25, 50, 75, 300, 250, 150),
    y = c(0, 0, 0, 0, 0, 1, 1, 0),
    w = c(0.31, 0.42, 0.59, 0.45, 0.6, 0.7, 0.6, 0.52),
    doseGrid = seq(25, 300, 25),
    placebo = FALSE,
    ID = 1L:8L,
    cohort = 1L:8L
  )
  DLEmodel <- LogisticIndepBeta(binDLE = c(1.05, 1.8), DLEweights = c(3, 3), DLEdose = c(25, 300), data = data)

  Effmodel <- Effloglog(c(1.223, 2.513), c(25, 300), nu = c(a = 1, b = 0.025), data = data, c = 0)
  options <- McmcOptions(
    burnin = 500,
    step = 2,
    samples = 5000,
    rng_kind = "Mersenne-Twister",
    rng_seed = 195612
  )

  data1 <- Data(
    x = data@x, y = data@y,
    doseGrid = data@doseGrid,
    ID = 1L:8L,
    cohort = 1L:8L
  )

  DLEsamples <- mcmc(data = data1, model = DLEmodel, options = options)
  Effsamples <- mcmc(data = data, model = Effmodel, options = options)

  expect_error(
    fitGain(
      DLEmodel = DLEmodel, DLEsamples = DLEsamples,
      Effmodel = Effmodel, Effsamples = Effsamples, data = data, points = "NotNumeric"
    ),
    "Assertion on 'points' failed: Must be of type 'numeric', not 'character'."
  )
  expect_error(
    fitGain(
      DLEmodel = DLEmodel, DLEsamples = DLEsamples,
      Effmodel = Effmodel, Effsamples = Effsamples, data = data, quantiles = c(0.1, 99)
    ),
    "Assertion on 'quantiles' failed: Probability must be within \\[0, 1\\] bounds but it is not."
  )
  expect_error(
    fitGain(
      DLEmodel = DLEmodel, DLEsamples = DLEsamples,
      Effmodel = Effmodel, Effsamples = Effsamples, data = data, quantiles = c(0.1, 0.2, 0.3)
    ),
    "Assertion on 'quantiles' failed: Must have length 2, but has length 3."
  )
})

## Samples-DataDual ----

test_that("fitGain-Samples-ModelEff works correctly", {
  data <- DataDual(
    x = c(25, 50, 25, 50, 75, 300, 250, 150),
    y = c(0, 0, 0, 0, 0, 1, 1, 0),
    w = c(0.31, 0.42, 0.59, 0.45, 0.6, 0.7, 0.6, 0.52),
    doseGrid = seq(25, 300, 25),
    placebo = FALSE,
    ID = 1L:8L,
    cohort = 1L:8L
  )
  DLEmodel <- LogisticIndepBeta(binDLE = c(1.05, 1.8), DLEweights = c(3, 3), DLEdose = c(25, 300), data = data)

  Effmodel <- Effloglog(c(1.223, 2.513), c(25, 300), nu = c(a = 1, b = 0.025), data = data, c = 0)
  options <- McmcOptions(
    burnin = 500,
    step = 2,
    samples = 5000,
    rng_kind = "Mersenne-Twister",
    rng_seed = 431609
  )
  data1 <- Data(
    x = data@x,
    y = data@y,
    doseGrid = data@doseGrid,
    ID = 1L:8L,
    cohort = 1L:8L
  )

  DLEsamples <- mcmc(data = data1, model = DLEmodel, options = options)
  Effsamples <- mcmc(data = data, model = Effmodel, options = options)

  actual <- fitGain(
    DLEmodel = DLEmodel, DLEsamples = DLEsamples,
    Effmodel = Effmodel, Effsamples = Effsamples, data = data
  )

  expect_snapshot(actual)
})

test_that("fitGain-Samples-ModelEff fails gracefully with bad input", {
  data <- DataDual(
    x = c(25, 50, 25, 50, 75, 300, 250, 150),
    y = c(0, 0, 0, 0, 0, 1, 1, 0),
    w = c(0.31, 0.42, 0.59, 0.45, 0.6, 0.7, 0.6, 0.52),
    doseGrid = seq(25, 300, 25),
    placebo = FALSE,
    ID = 1L:8L,
    cohort = 1L:8L
  )
  DLEmodel <- LogisticIndepBeta(binDLE = c(1.05, 1.8), DLEweights = c(3, 3), DLEdose = c(25, 300), data = data)

  Effmodel <- Effloglog(c(1.223, 2.513), c(25, 300), nu = c(a = 1, b = 0.025), data = data, c = 0)
  options <- McmcOptions(
    burnin = 500,
    step = 2,
    samples = 5000,
    rng_kind = "Mersenne-Twister",
    rng_seed = 431609
  )
  data1 <- Data(
    x = data@x,
    y = data@y,
    doseGrid = data@doseGrid,
    ID = 1L:8L,
    cohort = 1L:8L
  )

  DLEsamples <- mcmc(data = data1, model = DLEmodel, options = options)
  Effsamples <- mcmc(data = data, model = Effmodel, options = options)

  expect_error(
    fitGain(
      DLEmodel = DLEmodel, DLEsamples = DLEsamples,
      Effmodel = Effmodel, Effsamples = Effsamples, data = data, points = "NotNumeric"
    ),
    "Assertion on 'points' failed: Must be of type 'numeric', not 'character'."
  )
  expect_error(
    fitGain(
      DLEmodel = DLEmodel, DLEsamples = DLEsamples,
      Effmodel = Effmodel, Effsamples = Effsamples, data = data, quantiles = c(0.1, 99)
    ),
    "Assertion on 'quantiles' failed: Probability must be within \\[0, 1\\] bounds but it is not."
  )
  expect_error(
    fitGain(
      DLEmodel = DLEmodel, DLEsamples = DLEsamples,
      Effmodel = Effmodel, Effsamples = Effsamples, data = data, quantiles = c(0.1, 0.2, 0.3)
    ),
    "Assertion on 'quantiles' failed: Must have length 2, but has length 3."
  )
})

# plot ----

## Samples-GeneralModel ----

test_that("Check that plot-Samples-ModelTox fails gracefully with bad input", {
  data <- Data(
    x = c(25, 50, 50, 75, 150, 200, 225, 300),
    y = c(0, 0, 0, 0, 1, 1, 1, 1),
    doseGrid = seq(from = 25, to = 300, by = 25),
    ID = 1L:8L,
    cohort = as.integer(c(1, 2, 2, 3:7))
  )
  model <- LogisticIndepBeta(binDLE = c(1.05, 1.8), DLEweights = c(3, 3), DLEdose = c(25, 300), data = data)
  options <- McmcOptions(burnin = 100, step = 2, samples = 200)
  samples <- mcmc(data = data, model = model, options = options)

  expect_error(
    plot(x = samples, y = model, data = data, showLegend = "NotLogical"),
    "Assertion on 'showLegend' failed: Must be of type 'logical', not 'character'."
  )
})

test_that("Check that plot-Samples-ModelTox works correctly", {
  data <- Data(
    x = c(25, 50, 50, 75, 150, 200, 225, 300),
    y = c(0, 0, 0, 0, 1, 1, 1, 1),
    doseGrid = seq(from = 25, to = 300, by = 25),
    ID = 1L:8L,
    cohort = as.integer(c(1, 2, 2, 3:7))
  )
  model <- LogisticIndepBeta(binDLE = c(1.05, 1.8), DLEweights = c(3, 3), DLEdose = c(25, 300), data = data)
  options <- McmcOptions(
    burnin = 500, step = 2,
    samples = 5000,
    rng_kind = "Mersenne-Twister",
    rng_seed = 565409
  )
  samples <- mcmc(data = data, model = model, options = options)

  actual <- plot(x = samples, y = model, data = data)
  vdiffr::expect_doppelganger("plot-Samples-ModelTox", actual)

  actual1 <- plot(x = samples, y = model, data = data, showLegend = FALSE)
  vdiffr::expect_doppelganger("plot-Samples-ModelTox_showlegend-FALSE", actual1)
})

## Samples-DataDual ----

test_that("Check that plot-Samples-ModelEff fails gracefully with bad input", {
  data <- DataDual(
    x = c(25, 50, 25, 50, 75, 300, 250, 150),
    y = c(0, 0, 0, 0, 0, 1, 1, 0),
    w = c(0.31, 0.42, 0.59, 0.45, 0.6, 0.7, 0.6, 0.52),
    doseGrid = seq(25, 300, 25),
    placebo = FALSE,
    ID = 1L:8L,
    cohort = 1L:8L
  )
  model <- Effloglog(eff = c(1.223, 2.513), eff_dose = c(25, 300), nu = c(a = 1, b = 0.025), data = data)
  options <- McmcOptions(
    burnin = 100, step = 2,
    samples = 200,
    rng_kind = "Mersenne-Twister",
    rng_seed = 565409
  )
  samples <- mcmc(data = data, model = model, options = options)

  expect_error(
    plot(x = samples, y = model, data = data, showLegend = "NotLogical"),
    "Assertion on 'showLegend' failed: Must be of type 'logical', not 'character'."
  )
})

test_that("Check that plot-Samples-ModelEff works correctly", {
  data <- DataDual(
    x = c(25, 50, 25, 50, 75, 300, 250, 150),
    y = c(0, 0, 0, 0, 0, 1, 1, 0),
    w = c(0.31, 0.42, 0.59, 0.45, 0.6, 0.7, 0.6, 0.52),
    doseGrid = seq(25, 300, 25),
    placebo = FALSE,
    ID = 1L:8L,
    cohort = 1L:8L
  )
  model <- Effloglog(eff = c(1.223, 2.513), eff_dose = c(25, 300), nu = c(a = 1, b = 0.025), data = data)
  options <- McmcOptions(
    burnin = 100, step = 2,
    samples = 200,
    rng_kind = "Mersenne-Twister",
    rng_seed = 565409
  )
  samples <- mcmc(data = data, model = model, options = options)

  actual <- plot(x = samples, y = model, data = data)
  vdiffr::expect_doppelganger("plot-Samples-ModelEff", actual)

  actual1 <- plot(x = samples, y = model, data = data, showLegend = FALSE)
  vdiffr::expect_doppelganger("plot-Samples-ModelEff_showlegend-FALSE", actual1)
})

## Samples-DataDual-ModelEffloglog ----

test_that("Check that plot-Samples-ModelEffloglog fails gracefully with bad input", {
  data <- DataDual(
    x = c(25, 50, 25, 50, 75, 300, 250, 150),
    y = c(0, 0, 0, 0, 0, 1, 1, 0),
    w = c(0.31, 0.42, 0.59, 0.45, 0.6, 0.7, 0.6, 0.52),
    doseGrid = seq(25, 300, 25),
    placebo = FALSE,
    ID = 1L:8L,
    cohort = 1L:8L
  )
  model <- Effloglog(eff = c(1.223, 2.513), eff_dose = c(25, 300), nu = c(a = 1, b = 0.025), data = data)
  options <- McmcOptions(
    burnin = 100, step = 2,
    samples = 200,
    rng_kind = "Mersenne-Twister",
    rng_seed = 565409
  )
  samples <- mcmc(data = data, model = model, options = options)

  expect_error(
    plot(x = samples, y = model, data = data, showLegend = "NotLogical"),
    "Assertion on 'showLegend' failed: Must be of type 'logical', not 'character'."
  )
})

test_that("Check that plot-Samples-ModelEffloglog works correctly", {
  data <- DataDual(
    x = c(25, 50, 25, 50, 75, 300, 250, 150),
    y = c(0, 0, 0, 0, 0, 1, 1, 0),
    w = c(0.31, 0.42, 0.59, 0.45, 0.6, 0.7, 0.6, 0.52),
    doseGrid = seq(25, 300, 25),
    placebo = FALSE,
    ID = 1L:8L,
    cohort = 1L:8L
  )
  model <- Effloglog(eff = c(1.223, 2.513), eff_dose = c(25, 300), nu = c(a = 1, b = 0.025), data = data)
  options <- McmcOptions(
    burnin = 100, step = 2,
    samples = 200,
    rng_kind = "Mersenne-Twister",
    rng_seed = 565409
  )
  samples <- mcmc(data = data, model = model, options = options)

  actual <- plot(x = samples, y = model, data = data)
  vdiffr::expect_doppelganger("plot-Samples-ModelEffloglog", actual)

  actual1 <- plot(x = samples, y = model, data = data, showLegend = FALSE)
  vdiffr::expect_doppelganger("plot-Samples-ModelEffloglog_showlegend-FALSE", actual1)
})

# plot

## Samples-GeneralModel-Missing----

test_that("Check that plot-Samples-ModelEffNoSamples fails gracefully with bad input", {
  data <- Data(
    x = c(25, 50, 50, 75, 100, 100, 225, 300),
    y = c(0, 0, 0, 0, 1, 1, 1, 1),
    doseGrid = seq(25, 300, 25),
    ID = 1L:8L,
    cohort = 1L:8L
  )
  model <- LogisticIndepBeta(binDLE = c(1.05, 1.8), DLEweights = c(3, 3), DLEdose = c(25, 300), data = data)

  expect_error(
    plot(y = model, x = data, showLegend = "NotLogical"),
    "Assertion on 'showLegend' failed: Must be of type 'logical', not 'character'."
  )
})

test_that("Check that plot-Samples-ModelEffNoSamples works correctly", {
  data <- Data(
    x = c(25, 50, 50, 75, 100, 100, 225, 300),
    y = c(0, 0, 0, 0, 1, 1, 1, 1),
    doseGrid = seq(25, 300, 25),
    ID = 1L:8L,
    cohort = 1L:8L
  )
  model <- LogisticIndepBeta(binDLE = c(1.05, 1.8), DLEweights = c(3, 3), DLEdose = c(25, 300), data = data)

  actual <- plot(y = model, x = data)
  vdiffr::expect_doppelganger("plot-Samples-ModelEffNoSamples", actual)

  actual1 <- plot(y = model, x = data, showLegend = FALSE)
  vdiffr::expect_doppelganger("plot-Samples-ModelEffNoSamples_showlegend-FALSE", actual1)
})

# plotGain ----

## ModelTox-Samples-ModelEff-Samples ----

test_that("plotGain-ModelTox-Samples-ModelEff-Samples works correctly", {
  data <- DataDual(
    x = c(25, 50, 25, 50, 75, 300, 250, 150),
    y = c(0, 0, 0, 0, 0, 1, 1, 0),
    w = c(0.31, 0.42, 0.59, 0.45, 0.6, 0.7, 0.6, 0.52),
    doseGrid = seq(25, 300, 25),
    placebo = FALSE,
    ID = 1L:8L,
    cohort = 1L:8L
  )
  DLEmodel <- LogisticIndepBeta(binDLE = c(1.05, 1.8), DLEweights = c(3, 3), DLEdose = c(25, 300), data = data)
  Effmodel <- Effloglog(eff = c(1.223, 2.513), eff_dose = c(25, 300), nu = c(a = 1, b = 0.025), data = data, const = 0)
  data1 <- Data(
    x = data@x,
    y = data@y,
    doseGrid = data@doseGrid,
    ID = 1L:8L,
    cohort = 1L:8L
  )
  optionsDLE <- McmcOptions(
    burnin = 1000,
    step = 2,
    samples = 10000,
    rng_kind = "Mersenne-Twister",
    rng_seed = 114810
  )
  optionsTox <- McmcOptions(
    burnin = 1000,
    step = 2,
    samples = 10000,
    rng_kind = "Mersenne-Twister",
    rng_seed = 265310
  )
  DLEsamples <- mcmc(data = data1, model = DLEmodel, options = optionsDLE)
  Effsamples <- mcmc(data = data, model = Effmodel, options = optionsTox)
  actual <- plotGain(
    DLEmodel = DLEmodel, DLEsamples = DLEsamples,
    Effmodel = Effmodel, Effsamples = Effsamples,
    data = data
  )
  vdiffr::expect_doppelganger("plotGain-ModelTox-Samples-ModelEff-Samples", actual)
})

## ModelTox-Missing-ModelEff-Missing ----

test_that("plotGain-ModelTox-Missing-ModelEff-Missing works correctly", {
  data <- DataDual(
    x = c(25, 50, 25, 50, 75, 300, 250, 150),
    y = c(0, 0, 0, 0, 0, 1, 1, 0),
    w = c(0.31, 0.42, 0.59, 0.45, 0.6, 0.7, 0.6, 0.52),
    doseGrid = seq(25, 300, 25),
    placebo = FALSE,
    ID = 1L:8L,
    cohort = 1L:8L
  )
  DLEmodel <- LogisticIndepBeta(binDLE = c(1.05, 1.8), DLEweights = c(3, 3), DLEdose = c(25, 300), data = data)
  Effmodel <- Effloglog(eff = c(1.223, 2.513), eff_dose = c(25, 300), nu = c(a = 1, b = 0.025), data = data)
  actual <- plotGain(
    DLEmodel = DLEmodel,
    Effmodel = Effmodel,
    data = data
  )
  vdiffr::expect_doppelganger("plotGain-ModelTox-Missing-ModelEff-Missing", actual)
})

# plotDualResponses ----

## Samples ----

test_that("plotDualResponses fails gracefully with bad arguments", {
  data <- DataDual(
    x = c(25, 50, 25, 50, 75, 300, 250, 150),
    y = c(0, 0, 0, 0, 0, 1, 1, 0),
    w = c(0.31, 0.42, 0.59, 0.45, 0.6, 0.7, 0.6, 0.52),
    doseGrid = seq(25, 300, 25),
    placebo = FALSE,
    ID = 1L:8L,
    cohort = 1L:8L
  )
  DLEmodel <- LogisticIndepBeta(binDLE = c(1.05, 1.8), DLEweights = c(3, 3), DLEdose = c(25, 300), data = data)
  Effmodel <- Effloglog(eff = c(1.223, 2.513), eff_dose = c(25, 300), nu = c(a = 1, b = 0.025), data = data)
  data1 <- Data(
    x = data@x, y = data@y,
    doseGrid = data@doseGrid,
    ID = 1L:8L,
    cohort = 1L:8L
  )
  optionsDLE <- McmcOptions(
    burnin = 1000,
    step = 2,
    samples = 10000,
    rng_kind = "Mersenne-Twister",
    rng_seed = 284211
  )
  optionsEff <- McmcOptions(
    burnin = 1000,
    step = 2,
    samples = 10000,
    rng_kind = "Mersenne-Twister",
    rng_seed = 374211
  )
  DLEsamples <- mcmc(data = data1, model = DLEmodel, options = optionsDLE)
  Effsamples <- mcmc(data = data, model = Effmodel, options = optionsEff)
  expect_error(
    plotDualResponses(
      DLEmodel = DLEmodel,
      DLEsamples = DLEsamples,
      Effmodel = Effmodel,
      Effsamples = Effsamples,
      data = data,
      extrapolate = "NotLogical"
    ),
    "Assertion on 'extrapolate' failed: Must be of type 'logical', not 'character'."
  )
  expect_error(
    plotDualResponses(
      DLEmodel = DLEmodel,
      DLEsamples = DLEsamples,
      Effmodel = Effmodel,
      Effsamples = Effsamples,
      data = data,
      showLegend = "NotLogical"
    ),
    "Assertion on 'showLegend' failed: Must be of type 'logical', not 'character'."
  )
})

test_that("plotDualResponses works correctly", {
  data <- DataDual(
    x = c(25, 50, 25, 50, 75, 300, 250, 150),
    y = c(0, 0, 0, 0, 0, 1, 1, 0),
    w = c(0.31, 0.42, 0.59, 0.45, 0.6, 0.7, 0.6, 0.52),
    doseGrid = seq(25, 300, 25),
    placebo = FALSE,
    ID = 1L:8L,
    cohort = 1L:8L
  )
  DLEmodel <- LogisticIndepBeta(binDLE = c(1.05, 1.8), DLEweights = c(3, 3), DLEdose = c(25, 300), data = data)
  Effmodel <- Effloglog(eff = c(1.223, 2.513), eff_dose = c(25, 300), nu = c(a = 1, b = 0.025), data = data)
  data1 <- Data(
    x = data@x, y = data@y,
    doseGrid = data@doseGrid,
    ID = 1L:8L,
    cohort = 1L:8L
  )
  optionsDLE <- McmcOptions(
    burnin = 1000,
    step = 2,
    samples = 10000,
    rng_kind = "Mersenne-Twister",
    rng_seed = 284211
  )
  optionsEff <- McmcOptions(
    burnin = 1000,
    step = 2,
    samples = 10000,
    rng_kind = "Mersenne-Twister",
    rng_seed = 374211
  )
  DLEsamples <- mcmc(data = data1, model = DLEmodel, options = optionsDLE)
  Effsamples <- mcmc(data = data, model = Effmodel, options = optionsEff)

  actual <- plotDualResponses(
    DLEmodel = DLEmodel,
    DLEsamples = DLEsamples,
    Effmodel = Effmodel,
    Effsamples = Effsamples,
    data = data
  )
  vdiffr::expect_doppelganger("plotDualResponses", actual)

  actual1 <- plotDualResponses(
    DLEmodel = DLEmodel,
    DLEsamples = DLEsamples,
    Effmodel = Effmodel,
    Effsamples = Effsamples,
    data = data,
    extrapolate = FALSE
  )
  vdiffr::expect_doppelganger("plotDualResponses_extrapolate-FALSE", actual1)

  actual2 <- plotDualResponses(
    DLEmodel = DLEmodel,
    DLEsamples = DLEsamples,
    Effmodel = Effmodel,
    Effsamples = Effsamples,
    data = data,
    showLegend = TRUE
  )
  vdiffr::expect_doppelganger("plotDualResponses_showlegend-TRUE", actual2)

  actual3 <- plotDualResponses(
    DLEmodel = DLEmodel,
    DLEsamples = DLEsamples,
    Effmodel = Effmodel,
    Effsamples = Effsamples,
    data = data,
    showLegend = TRUE,
    extrapolate = FALSE
  )
  vdiffr::expect_doppelganger("plotDualResponses_showlegend-TRUE_extrapolate-FALSE", actual3)
})

test_that("plotDualResponses-ModelTox-Missing-ModelEff-Missing works as expected", {
  data <- DataDual(
    x = c(25, 50, 25, 50, 75, 300, 250, 150),
    y = c(0, 0, 0, 0, 0, 1, 1, 0),
    w = c(0.31, 0.42, 0.59, 0.45, 0.6, 0.7, 0.6, 0.52),
    doseGrid = seq(25, 300, 25),
    placebo = FALSE,
    ID = 1L:8L,
    cohort = 1L:8L
  )
  DLEmodel <- LogisticIndepBeta(binDLE = c(1.05, 1.8), DLEweights = c(3, 3), DLEdose = c(25, 300), data = data)
  Effmodel <- Effloglog(eff = c(1.223, 2.513), eff_dose = c(25, 300), nu = c(a = 1, b = 0.025), data = data)
  actual <- plotDualResponses(
    DLEmodel = DLEmodel,
    Effmodel = Effmodel,
    data = data
  )
  vdiffr::expect_doppelganger("plotDualResponses-ModelTox-Missing-ModelEff-Missing", actual)
})

# fitPEM ----

## Samples-DALogisticLogNormal-DataDA ----

test_that("fitPEM-Samples-DALogisticLogNormal-DataDA fails gracefully with bad input", {
  data <- DataDA(
    x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
    y = c(0, 0, 1, 1, 0, 0, 1, 0),
    doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2)),
    u = c(42, 30, 15, 5, 20, 25, 30, 60),
    t0 = c(0, 15, 30, 40, 55, 70, 75, 85),
    Tmax = 60,
    ID = 1L:8L,
    cohort = as.integer(c(1:5, 6, 6, 6))
  )
  npiece_ <- 10
  lambda_prior <- function(k) {
    npiece_ / (data@Tmax * (npiece_ - k + 0.5))
  }
  model <- DALogisticLogNormal(
    mean = c(-0.85, 1),
    cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
    ref_dose = 56,
    npiece = npiece_,
    l = as.numeric(t(apply(as.matrix(c(1:npiece_), 1, npiece_), 2, lambda_prior))),
    c_par = 2
  )
  options <- McmcOptions(
    burnin = 500,
    step = 2,
    samples = 5000,
    rng_kind = "Mersenne-Twister",
    rng_seed = 225013
  )
  samples <- mcmc(data, model, options)

  expect_error(
    fitted <- fitPEM(samples, model, data, quantiles = c(35, 0.975)),
    "Assertion on 'quantiles' failed: Must be sorted."
  )
  expect_error(
    fitted <- fitPEM(samples, model, data, quantiles = c(0.025, 975)),
    "Assertion on 'quantiles' failed: Probability must be within \\[0, 1\\] bounds but it is not."
  )
  expect_error(
    fitted <- fitPEM(samples, model, data, quantiles = c(0.025, 0.6, 975)),
    "Assertion on 'quantiles' failed: Must have length 2, but has length 3."
  )
  expect_error(
    fitted <- fitPEM(samples, model, data, quantiles = c(0.025, 0.975), hazard = "NotLogical"),
    "Assertion on 'hazard' failed: Must be of type 'logical', not 'character'."
  )
})

## Samples-DALogisticLogNormal-DataDA ----

test_that("fitPEM-Samples-DALogisticLogNormal-DataDA works correctly", {
  data <- DataDA(
    x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
    y = c(0, 0, 1, 1, 0, 0, 1, 0),
    doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2)),
    u = c(42, 30, 15, 5, 20, 25, 30, 60),
    t0 = c(0, 15, 30, 40, 55, 70, 75, 85),
    Tmax = 60,
    ID = 1L:8L,
    cohort = as.integer(c(1:5, 6, 6, 6))
  )
  npiece_ <- 10
  lambda_prior <- function(k) {
    npiece_ / (data@Tmax * (npiece_ - k + 0.5))
  }
  model <- DALogisticLogNormal(
    mean = c(-0.85, 1),
    cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
    ref_dose = 56,
    npiece = npiece_,
    l = as.numeric(t(apply(as.matrix(c(1:npiece_), 1, npiece_), 2, lambda_prior))),
    c_par = 2
  )
  options <- McmcOptions(
    burnin = 500,
    step = 2,
    samples = 5000,
    rng_kind = "Mersenne-Twister",
    rng_seed = 225013
  )
  samples <- mcmc(data, model, options)

  actual <- fitPEM(samples, model, data)
  expect_snapshot(actual)

  actual1 <- fitPEM(samples, model, data, middle = median)
  expect_snapshot(actual1)

  actual2 <- fitPEM(samples, model, data, quantiles = c(0.2, 0.8))
  expect_snapshot(actual2)

  actual3 <- fitPEM(samples, model, data, hazard = TRUE)
  expect_snapshot(actual3)
})

test_that("plot-Samples-DALogisticNormal fails gracefully with bad input", {
  data <- DataDA(
    x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
    y = c(0, 0, 1, 1, 0, 0, 1, 0),
    doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2)),
    u = c(42, 30, 15, 5, 20, 25, 30, 60),
    t0 = c(0, 15, 30, 40, 55, 70, 75, 85),
    Tmax = 60,
    ID = 1L:8L,
    cohort = 1L:8L
  )
  npiece_ <- 10
  lambda_prior <- function(k) {
    npiece_ / (data@Tmax * (npiece_ - k + 0.5))
  }

  model <- DALogisticLogNormal(
    mean = c(-0.85, 1),
    cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
    ref_dose = 56,
    npiece = npiece_,
    l = as.numeric(t(apply(as.matrix(c(1:npiece_), 1, npiece_), 2, lambda_prior))),
    c_par = 2
  )
  options <- McmcOptions(
    burnin = 100,
    step = 2,
    samples = 1000,
    rng_kind = "Mersenne-Twister",
    rng_seed = 552914
  )
  samples <- mcmc(data, model, options)

  expect_error(
    plot(samples, model, data, showLegend = "NotLogical"),
    "Assertion on 'showLegend' failed: Must be of type 'logical', not 'character'."
  )
  expect_error(
    plot(samples, model, data, hazard = "NotLogical"),
    "Assertion on 'hazard' failed: Must be of type 'logical', not 'character'."
  )
})

# plot ----

## Samples-DALogisticLogNormal-DataDA

test_that("plot-Samples-DALogisticNormal works correctly", {
  data <- DataDA(
    x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
    y = c(0, 0, 1, 1, 0, 0, 1, 0),
    doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2)),
    u = c(42, 30, 15, 5, 20, 25, 30, 60),
    t0 = c(0, 15, 30, 40, 55, 70, 75, 85),
    Tmax = 60,
    ID = 1L:8L,
    cohort = 1L:8L
  )
  npiece_ <- 10
  lambda_prior <- function(k) {
    npiece_ / (data@Tmax * (npiece_ - k + 0.5))
  }

  model <- DALogisticLogNormal(
    mean = c(-0.85, 1),
    cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
    ref_dose = 56,
    npiece = npiece_,
    l = as.numeric(t(apply(as.matrix(c(1:npiece_), 1, npiece_), 2, lambda_prior))),
    c_par = 2
  )
  options <- McmcOptions(
    burnin = 100,
    step = 2,
    samples = 1000,
    rng_kind = "Mersenne-Twister",
    rng_seed = 552914
  )
  samples <- mcmc(data, model, options)

  actual <- plot(samples, model, data)
  vdiffr::expect_doppelganger("plot-Samples-DALogisticLogNormal", actual)

  actual1 <- plot(samples, model, data, hazard = TRUE)
  vdiffr::expect_doppelganger("plot-Samples-DALogisticLogNormal_hazard-TRUE", actual1)

  actual2 <- plot(samples, model, data, showLegend = FALSE)
  vdiffr::expect_doppelganger("plot-Samples-DALogisticLogNormal_showLegend-FALSE", actual2)

  actual3 <- plot(samples, model, data, showLegend = FALSE, hazard = TRUE)
  vdiffr::expect_doppelganger("plot-Samples-DALogisticLogNormal_hazard-TRUE_showLegend-FALSE", actual3)
})

# nolint end
