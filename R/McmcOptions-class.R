#' @include helpers.R McmcOptions-validity.R
NULL

# McmcOptions-class ----

#' `McmcOptions`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`McmcOptions`] is a class for the three canonical MCMC options.
#'
#' @slot iterations (`count`)\cr number of MCMC iterations.
#' @slot burnin (`count`)\cr number of burn-in iterations which are not saved.
#' @slot step (`count`)\cr only every `step`-th iteration is saved after
#'   the `burnin`. In other words, a sample from iteration
#'   `i = 1,...,iterations`, is saved if and only if
#'   `(i - burnin) mod step = 0`.\cr
#'   For example, for `iterations = 6`, `burnin = 0` and `step = 2`, only
#'   samples from iterations `2,4,6` will be saved.
#' @slot rng_kind (`string`)\cr a Random Number Generator (RNG) type used
#'   by [`rJAGS`]. It must be one out of the following four values:
#'   `base::Wichmann-Hill`, `base::Marsaglia-Multicarry`,
#'   `base::Super-Duper`, `base::Mersenne-Twister`.
#' @slot rng_seed (`number`)\cr a Random Number Generator (RNG) seed used
#'   by [`rJAGS`] for a chosen `rng_kind`. It must be an integer scalar.
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
    rng_kind = "base::Mersenne-Twister",
    rng_seed = 1L
  ),
  validity = validate_mcmc_options
)

# McmcOptions-constructor ----

#' @rdname McmcOptions-class
#'
#' @param burnin (`count`)\cr number of burn-in iterations which are not saved.
#' @param step (`count`)\cr only every step-th iteration is saved after
#'   the burn-in.
#' @param samples (`count`)\cr number of resulting samples.
#' @param rng_kind (`string`)\cr the name of the RNG type. Possible types are:
#'   `Wichmann-Hill`, `Marsaglia-Multicarry`, `Super-Duper`, `Mersenne-Twister`.
#' @param rng_seed (`number`)\cr RNG seed corresponding to chosen `rng_kind`.
#'   It must be an integer value.
#'
#' @export
#' @example examples/McmcOptions-class-McmcOptions.R
#'
McmcOptions <- function(burnin = 1e4L,
                        step = 2L,
                        samples = 1e4L,
                        rng_kind = "Mersenne-Twister",
                        rng_seed = 1L) {
  assert_number(burnin)
  assert_number(step)
  assert_number(samples)
  assert_string(rng_kind)
  assert_number(rng_seed)

  .McmcOptions(
    iterations = safeInteger(burnin + (step * samples)),
    burnin = safeInteger(burnin),
    step = safeInteger(step),
    rng_kind = paste0("base::", rng_kind),
    rng_seed = safeInteger(rng_seed)
  )
}
