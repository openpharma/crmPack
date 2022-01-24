# validate_mcmc_options ----

#' Internal Helper Functions for Validation of [`McmcOptions`] Objects
#'
#' @description `r lifecycle::badge("stable")`
#'
#' These functions are only used internally to validate the format of an input
#' [`McmcOptions`] or inherited classes and therefore not exported.
#'
#' @name validate_mcmcoptions_objects
#' @param object (`McmcOptions`)\cr object to validate.
#' @return A `character` vector with the validation failure messages,
#'   or `TRUE` in case validation passes.
NULL

#' @describeIn validate_mcmcoptions_objects validates that the [`McmcOptions`]
#'   object contains valid integer scalars `iterations`, `burnin` and `step`
#'   as well as proper parameters for Random Number Generator.
validate_mcmc_options <- function(object) {
  o <- Validate()
  allowed_rng_kinds <- c(
    "base::Wichmann-Hill",
    "base::Marsaglia-Multicarry",
    "base::Super-Duper",
    "base::Mersenne-Twister",
    NA_character_
  )

  o$check(
    test_int(object@iterations, lower = 1L),
    "iterations must be integer scalar greater than or equal to 1"
  )
  o$check(
    test_int(object@burnin, lower = 0L),
    "burn-in must be non-negative integer scalar"
  )
  # This below check should not be conducted in above test, using
  # `upper = object@iterations -1` argument, since object@iterations might be
  # not-valid. In such a case `test_int` throws an internal error.
  o$check(
    test_true(object@burnin < object@iterations),
    "burn-in must be lower than iterations"
  )
  o$check(
    test_int(object@step, lower = 1L),
    "step must be integer scalar greater than or equal to 1"
  )

  is_rng_kind_scalar <- test_string(object@rng_kind, na.ok = TRUE)
  o$check(is_rng_kind_scalar, "rng_kind must be a single string")
  o$check(
    test_subset(object@rng_kind, allowed_rng_kinds),
    paste0(
      "rng_kind must one of the following: ",
      paste(allowed_rng_kinds, collapse = ", "),
      ". User specifies the rng_kind without `base::` prefix"
    )
  )

  is_rng_seed_scalar <- test_int(object@rng_seed, na.ok = TRUE)
  o$check(is_rng_seed_scalar, "rng_seed must be an integer scalar")

  # Below `if` condition is not only reasonable but also needed as R CMD check
  # is activating some stricter checks and it fails when arguments to `||` are
  # not scalars, even if `||` works well with vectors of length > 1.
  if (is_rng_kind_scalar && is_rng_seed_scalar) {
    o$check(
      !is.na(object@rng_kind) || is.na(object@rng_seed),
      "rng_seed supplied but rng_kind not set"
    )
  }
  o$result()
}
