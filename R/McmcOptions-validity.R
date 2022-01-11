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
#'   as well as proper parameters for Random Number Generator used in `rJAGS`,
#'   i.e. `.RNG.name` and `.RNG.seed`.
validate_mcmc_options <- function(object) {
  o <- Validate()
  allowed_RNGs <- c(
    "base::Wichmann-Hill",
    "base::Marsaglia-Multicarry",
    "base::Super-Duper",
    "base::Mersenne-Twister"
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
  o$check(
    test_list(object@RNG, len = 2L, names = "named", any.missing = FALSE),
    "RNG must be a named list with two elements"
  )
  o$check(
    test_set_equal(names(object@RNG), c(".RNG.name", ".RNG.seed")),
    "RNG's elements must be named: .RNG.name, .RNG.seed"
  )
  o$check(
    test_subset(object@RNG$.RNG.name, allowed_RNGs),
    paste(
      ".RNG.name in RNG list must one of the following:",
      paste(allowed_RNGs, collapse = ", ")
    )
  )
  o$check(
    test_int(object@RNG$.RNG.seed),
    ".RNG.seed in RNG list must be integer scalar"
  )
  o$result()
}
