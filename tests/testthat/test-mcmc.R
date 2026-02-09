# JAGS ----

test_that("JAGS model compile works as expected for an example model", {
  # The example model here is DualEndpointRW.
  # The steps undertaken in this unit test correspond to the following commands
  # in (command line) JAGS:
  # jags
  # # and then in an interactive mode
  # model in "model.bug"
  # data in "data.R"
  # compile, nchains(1)
  # We use JAGS through the rjags R package, so that we do not have to invoke
  # a system command 'jags' with base::system().

  modfile <- test_path("_jags/model.bug")
  data <- list(
    betaZ_mean = c(0, 1),
    betaZ_prec = structure(c(1, 0, 0, 1), .Dim = c(2, 2)),
    doseGrid = c(
      0.001,
      25,
      50,
      75,
      100,
      125,
      150,
      175,
      200,
      225,
      250,
      275,
      300
    ),
    nGrid = 13,
    nObs = 12,
    precBetaW = 100,
    precW = 1,
    ref_dose = 2,
    rho = 0,
    use_log_dose = 0,
    w = c(13, 77, 86, 26, 27, 36, 37, 97, 21, 49, 87, 48),
    x = c(0.001, 25, 25, 25, 0.001, 50, 50, 50, 0.001, 100, 100, 100),
    xLevel = c(1, 2, 2, 2, 1, 3, 3, 3, 1, 5, 5, 5),
    y = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0)
  )

  p <- .Call("make_console", PACKAGE = "rjags")
  invisible(.Call("check_model", p, modfile, PACKAGE = "rjags"))
  varnames <- .Call("get_variable_names", p, PACKAGE = "rjags")
  varnames_expected <- c(
    "nObs",
    "stand_dose_temp",
    "x",
    "ref_dose",
    "stand_dose",
    "use_log_dose",
    "meanZ",
    "betaZ",
    "z",
    "y",
    "condMeanW",
    "betaW",
    "xLevel",
    "rho",
    "precW",
    "w",
    "condPrecW",
    "theta",
    "betaZ_mean",
    "betaZ_prec",
    "nGrid",
    "delta",
    "precBetaW",
    "doseGrid"
  )
  compile_output <- capture_output_lines(.Call(
    "compile",
    p,
    data,
    1L,
    TRUE,
    PACKAGE = "rjags"
  ))
  compile_output <- trimws(compile_output)
  compile_output_expected <- c(
    "Compiling model graph",
    "Resolving undeclared variables",
    "Allocating nodes",
    "Graph information:",
    ifelse(rjags::jags.version() >= numeric_version("5.0.0"),
           "Fully observed stochastic nodes: 24",
           "Observed stochastic nodes: 24"),
    "Unobserved stochastic nodes: 26",
    "Total graph size: 196",
    ""
  )

  expect_set_equal(varnames, varnames_expected)
  expect_identical(compile_output, compile_output_expected)
})

# mcmc-GeneralData ----

test_that("mcmc-GeneralData works as expected", {
  data <- h_get_data()
  model <- h_get_logistic_log_normal()
  options <- h_get_mcmc_options(samples = 100)

  result <- mcmc(data = data, model = model, options = options)

  expect_true(all(slotNames(result) == c("data", "options")))
  expect_identical(result@options, options)
  expect_identical(names(result), c("alpha0", "alpha1"))
  expect_numeric(
    result@data$alpha0,
    len = 100,
    any.missing = FALSE,
    finite = TRUE
  )
  expect_numeric(
    result@data$alpha1,
    len = 100,
    any.missing = FALSE,
    finite = TRUE
  )
})

test_that("mcmc-GeneralData gets random results", {
  data <- h_get_data()
  model <- h_get_logistic_log_normal()
  options <- h_get_mcmc_options(fixed = FALSE)

  result_1 <- mcmc(data = data, model = model, options = options)
  result_2 <- mcmc(data = data, model = model, options = options)

  # Should differ due to randomness.
  expect_false(all(result_1@data$alpha0 == result_2@data$alpha0))
  expect_false(all(result_1@data$alpha1 == result_2@data$alpha1))
})

test_that("mcmc-GeneralData respects fixed RNG settings", {
  data <- h_get_data()
  model <- h_get_logistic_log_normal()
  options <- h_get_mcmc_options()

  result_1 <- mcmc(data = data, model = model, options = options)
  result_2 <- mcmc(data = data, model = model, options = options)

  # Must not differ due to fixed seed.
  expect_true(all(result_1@data$alpha0 == result_2@data$alpha0))
  expect_true(all(result_1@data$alpha1 == result_2@data$alpha1))
})

test_that("mcmc-DataOrdinal returns a correctly named samples@data list", {
  ordinal_data <- .DefaultDataOrdinal()
  ordinal_model <- .DefaultLogisticLogNormalOrdinal()
  mcmc_options <- .DefaultMcmcOptions()

  samples <- mcmc(ordinal_data, ordinal_model, mcmc_options)
  expect_equal(names(samples@data), c("alpha1", "alpha2", "beta"))
})
