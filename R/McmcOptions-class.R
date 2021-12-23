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
#'   `(i - burnin) mod step = 0`. For example, for `iterations = 6`, 
#'   `burnin = 0` and `step = 2`, only samples from iterations `2,4,6`
#'   will be saved.
#'
#' @aliases McmcOptions
#' @export
#'
.McmcOptions <- setClass(
  Class = "McmcOptions",
  slots = c(
    iterations = "integer",
    burnin = "integer",
    step = "integer"
  ),
  prototype = prototype(
    iterations = 1000L,
    burnin = 100L,
    step = 2L
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
#'
#' @export
#' @example examples/McmcOptions-class-McmcOptions.R
#'
McmcOptions <- function(burnin = 1e4L,
                        step = 2L,
                        samples = 1e4L) {
  assert_number(burnin)
  assert_number(step)
  assert_number(samples)

  .McmcOptions(
    iterations = safeInteger(burnin + (step * samples)),
    burnin = safeInteger(burnin),
    step = safeInteger(step)
  )
}
