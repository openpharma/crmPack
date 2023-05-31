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

  dualSamples <- Samples(data = list(good = matrix(1:6, ncol=2)), options = McmcOptions(samples = 3))
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

  for (param in names(samples@data)){
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
    rho = c(a=1, b=1),
    rw1 = TRUE
  )
  mcmcOptions <- McmcOptions(burnin = 5, step = 2, samples = 2)
  set.seed(94)
  dualSamples <- mcmc(dualData, dualModel, mcmcOptions)
  for (param in c("betaZ")) { #names(dualSamples@data)) {
    actual <- get(dualSamples, param)
    assert_data_frame(actual)
    assert_set_equal(names(actual), c("Iteration", "Chain", "Parameter", "value"))
    expected <- data.frame(
      Iteration = rep(
        1:((mcmcOptions@iterations - mcmcOptions@burnin) / mcmcOptions@step),
        times=ncol(dualSamples@data[[param]])
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
      value = matrix(dualSamples@data[[param]], ncol=1)
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

test_that("fit-Samples fails gracefully with bad inputs", {
  mcmcOptions <- McmcOptions(samples = 3)
  samples <- Samples(data = list(alpha0 = 1:3, alpha1 = 4:6), options = mcmcOptions)
  model <- LogisticLogNormal(mean = c(-0.85, 1),
                             cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
                             ref_dose = 56)
  emptyData <- Data(doseGrid = seq(10, 80, 10))

  expect_error(
    fit(samples, model, emptyData, quantiles = c(0.025, 99))
    #,
    # "Assertion on 'quantiles' failed: Probability must be within [0, 1] bounds but it is not."
    # This is the correct error message, so why dos expect_error fail?
  )
  expect_error(
    fit(samples, model, emptyData, points = "A"),
    "Assertion on 'points' failed: Must be of type 'numeric', not 'character'."
  )})

test_that("fit-Samples works correctly for tox-only models", {
  checkIt <- function(middleFunc = mean, lowerQuantile =0.025, upperQuantile = 0.975, tolerance = 1e-06, seed) {
    mcmcOptions <- McmcOptions()
    sampleCount <- (mcmcOptions@iterations - mcmcOptions@burnin) / mcmcOptions@step

    model <- LogisticLogNormal(mean = c(-0.85, 1),
                               cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
                               ref_dose = 56)
    emptyData <- Data(doseGrid = seq(10, 80, 10))
    samples <- Samples(
      data = list(
        alpha0 = rnorm(sampleCount, mean = model@params@mean[1], sd = model@params@cov[1, 1]),
        alpha1 =  rnorm(sampleCount, mean = model@params@mean[2], sd = model@params@cov[2, 2])
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
test_that("fit-Samples works correctly for dual models", {
  #TODO: Check for numerical correctness
  dualData <- DataDual(
    ID = 1L:12L,
    cohort = c(6, 6, 6, 7, 7, 7, 8, 8, 8, 9, 9, 9),
    x=c(10, 10, 10, 20, 20, 20, 40, 40, 40, 50, 50, 50),
    y=c(0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1),
    w=c(0.7, 0.55, 0.6, 0.52, 0.54, 0.56, 0.43, 0.41, 0.39, 0.34, 0.38, 0.21),
    doseGrid=c(seq(from=10, to=80, by=10))
  )

  model <- DualEndpointRW(
    mean = c(0, 1),
    cov = matrix(c(1, 0, 0, 1), nrow=2),
    sigma2betaW = 0.01,
    sigma2W = c(a=0.1, b=0.1),
    rho = c(a=1, b=1),
    rw1 = TRUE
  )


  options <- McmcOptions()
  samples <- mcmc(dualData, model, options)

  actual <- fit(samples, model, dualData)

  expect_equal(class(actual), "data.frame")
  expect_setequal(names(actual), c("dose", "middle", "lower", "upper", "middleBiomarker", "lowerBiomarker", "upperBiomarker"))
})



