# validate_mcmc_options ----

test_that("validate_mcmc_options passes for valid object", {
  object <- McmcOptions()
  expect_true(validate_mcmc_options(object))
})

test_that("validate_mcmc_options returns error messages for non-valid object", {
  allowed_rng_kinds <- c(
    "base::Wichmann-Hill",
    "base::Marsaglia-Multicarry",
    "base::Super-Duper",
    "base::Mersenne-Twister",
    NA_character_
  )

  object <- McmcOptions()
  object@iterations <- c(-3L, 5L) # Should be a scalar greater than/equal to 1.
  object@burnin <- 20L # Should be lower than iterations.
  object@rng_kind <- c("wrong", "base::Wichmann-Hill", NA_character_)
  object@rng_seed <- c(2L, 4L, NA_integer_) # Should be an integer scalar.

  expect_equal(
    validate_mcmc_options(object),
    c(
      "iterations must be integer scalar greater than or equal to 1",
      "burn-in must be lower than iterations",
      "rng_kind must be a single string",
      paste0(
        "rng_kind must one of the following: ",
        paste(allowed_rng_kinds, collapse = ", "),
        ". User specifies the rng_kind without `base::` prefix"
      ),
      "rng_seed must be an integer scalar"
    )
  )
})

test_that("validate_mcmc_options returns error messages non-valid RNG spec", {
  object <- McmcOptions()
  object@rng_kind <- NA_character_
  object@rng_seed <- 5L
  expect_equal(
    validate_mcmc_options(object),
    "rng_seed supplied but rng_kind not set"
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
