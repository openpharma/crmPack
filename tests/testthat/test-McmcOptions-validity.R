# validate_mcmc_options ----

test_that("validate_mcmc_options passes for valid object", {
  object <- McmcOptions()
  expect_true(validate_mcmc_options(object))
})

test_that("validate_mcmc_options returns error messages for non-valid object", {
  object <- McmcOptions()
  object@iterations <- c(-3L, 5L) # Should be a scalar greater than/equal to 1.
  object@burnin <- 20L # Should be lower than iterations.
  object@RNG <- list(some_type = "something", "1")
  expect_equal(
    validate_mcmc_options(object),
    c(
      "iterations must be integer scalar greater than or equal to 1",
      "burn-in must be lower than iterations",
      "RNG must be a named list with two elements",
      "RNG's elements must be named: .RNG.name, .RNG.seed",
      ".RNG.seed in RNG list must be integer scalar"
    )
  )
})

test_that("validate_mcmc_options returns error message for wrong step", {
  object <- McmcOptions()
  object@step <- 0L # Should be greater than or equal to 1.
  expect_equal(
    validate_mcmc_options(object),
    "step must be integer scalar greater than or equal to 1"
  )
})

test_that("validate_mcmc_options returns error message for wrong RNG name", {
  allowed_rng <- c(
    "base::Wichmann-Hill",
    "base::Marsaglia-Multicarry",
    "base::Super-Duper",
    "base::Mersenne-Twister"
  )
  object <- McmcOptions()
  object@RNG <- list(.RNG.name = "wrong_name", .RNG.seed = 3L)
  expect_equal(
    validate_mcmc_options(object),
    paste(
      ".RNG.name in RNG list must one of the following:",
      paste(allowed_RNGs, collapse = ", ")
    )
  )
})
