#' @include helpers.R
#' @include McmcOptions-validity.R
#' @include CrmPackClass-class.R
NULL

# McmcOptions ----

## class ----

#' `McmcOptions`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`McmcOptions`] is a class for the three canonical MCMC options as well as
#' Random Number Generator settings.
#'
#' @slot iterations (`count`)\cr number of MCMC iterations.
#' @slot burnin (`count`)\cr number of burn-in iterations which are not saved.
#' @slot step (`count`)\cr only every `step`-th iteration is saved after
#'   the `burnin`. In other words, a sample from iteration
#'   `i = 1,...,iterations`, is saved if and only if
#'   `(i - burnin) mod step = 0`.\cr
#'   For example, for `iterations = 6`, `burnin = 0` and `step = 2`, only
#'   samples from iterations `2,4,6` will be saved.
#' @slot rng_kind (`string`)\cr a Random Number Generator (RNG) type used by
#'   [`rjags`]. It must be one out of the following four values:
#'   `base::Wichmann-Hill`, `base::Marsaglia-Multicarry`,
#'   `base::Super-Duper`, `base::Mersenne-Twister`, or `NA_character_`.
#'   If it is `NA_character_` (default), then the RNG kind will be chosen by
#'   [`rjags`].
#' @slot rng_seed (`number`)\cr a Random Number Generator (RNG) seed
#'   used by [`rjags`] for a chosen `rng_kind`. It must be an integer scalar or
#'   `NA_integer_`, which means that the seed will be chosen by [`rjags`].
#'
#' @aliases McmcOptions
#' @export
#'
.McmcOptions <- setClass(
  Class = "McmcOptions",
  slots = c(
    iterations = "integer",
    burnin = "integer",
    step = "integer",
    rng_kind = "character",
    rng_seed = "integer"
  ),
  prototype = prototype(
    iterations = 1000L,
    burnin = 100L,
    step = 2L,
    rng_kind = NA_character_,
    rng_seed = NA_integer_
  ),
  contains = "CrmPackClass",
  validity = v_mcmc_options
)

## constructor ----

#' @rdname McmcOptions-class
#'
#' @param burnin (`count`)\cr number of burn-in iterations which are not saved.
#' @param step (`count`)\cr only every step-th iteration is saved after
#'   the burn-in.
#' @param samples (`count`)\cr number of resulting samples.
#' @param rng_kind (`string`)\cr the name of the RNG type. Possible types are:
#'   `Wichmann-Hill`, `Marsaglia-Multicarry`, `Super-Duper`, `Mersenne-Twister`.
#'   If it is `NA` (default), then the RNG kind will be chosen by `[rjags`].
#' @param rng_seed (`number`)\cr RNG seed corresponding to chosen `rng_kind`.
#'   It must be an integer value or `NA` (default), which means that the seed
#'   will be chosen by `[rjags`].
#'
#' @export
#' @example examples/McmcOptions-class-McmcOptions.R
#'
McmcOptions <- function(burnin = 1e4L,
                        step = 2L,
                        samples = 1e4L,
                        rng_kind = NA_character_,
                        rng_seed = NA_integer_) {
  assert_count(burnin, positive = TRUE)
  assert_count(step, positive = TRUE)
  assert_count(samples, positive = TRUE)
  assert_string(rng_kind, na.ok = TRUE)
  assert_count(rng_seed, na.ok = TRUE)

  if (!is.na(rng_kind)) {
    rng_kind <- paste0("base::", rng_kind)
  } else {
    rng_kind <- NA_character_
  }
  if (!is.na(rng_seed)) {
    rng_seed <- as.integer(rng_seed)
  } else {
    rng_seed <- NA_integer_
  }

  .McmcOptions(
    iterations = as.integer(burnin + (step * samples)),
    burnin = as.integer(burnin),
    step = as.integer(step),
    rng_kind = rng_kind,
    rng_seed = as.integer(rng_seed)
  )
}

## default constructor ----

#' @rdname McmcOptions-class
#' @note Typically, end users will not use the `.DefaultMcmcOptions()` function.
#' @export
.DefaultMcmcOptions <- function() {
  McmcOptions(
    burnin = 500,
    samples = 2000
  )
}
