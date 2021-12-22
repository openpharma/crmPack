# validate_mcmc_options ----

test_that("validate_mcmc_options passes for valid object", {
  object <- McmcOptions()
  expect_true(validate_mcmc_options(object))
})

test_that("validate_mcmc_options returns error messages for non-valid object", {
  object <- McmcOptions()
  object@iterations <- c(-3L, 5L) # Should be a scalar greater than/equal to 1.
  object@burnin <- 20L # Should be lower than iterations.
  expect_equal(
    validate_mcmc_options(object),
    c(
      "iterations must be integer scalar greater than or equal to 1",
      "burn-in must be lower than iterations"
    )
  )
})

test_that("validate_mcmc_options returns error message wrong step", {
  object <- McmcOptions()
  object@step <- 0L # Should be greater than or equal to 1.
  expect_equal(
    validate_mcmc_options(object),
    "step must be integer scalar greater than or equal to 1"
  )
})
