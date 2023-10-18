#' @include McmcOptions-class.R
#' @include Samples-validity.R
#' @include CrmPackClass-class.R
NULL

# Samples ----

## class ----

#' `Samples`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`Samples`] is the class to store the MCMC samples.
#'
#' @slot data (`list`)\cr MCMC samples of the parameter. Each entry in this list
#'   must be a vector (in case of a scalar parameter) or matrix (in case of a
#'   vector-valued parameter) with samples.
#'   In case of matrix, every row is a separate sample, while columns correspond
#'   to the dimension of the parameter.
#' @slot options (`McmcOptions`)\cr MCMC options that were used to generate the
#'   samples.
#'
#' @aliases Samples
#' @export
#'
.Samples <- setClass(
  Class = "Samples",
  slots = c(
    data = "list",
    options = "McmcOptions"
  ),
  prototype = prototype(
    data = list(),
    options = McmcOptions()
  ),
  contains = "CrmPackClass",
  validity = v_samples
)

## constructor ----

#' @rdname Samples-class
#'
#' @param data see slot definition.
#' @param options see slot definition.
#'
#' @export
#' @example examples/Samples-class.R
#'
Samples <- function(data, options) {
  new("Samples", data = data, options = options)
}

## default constructor ----

#' @rdname Samples-class
#' @note Typically, end users will not use the `.DefaultSamples()` function.
#' @export
.DefaultSamples <- function() {
  mcmc(
    data = .DefaultData(),
    model = .DefaultLogisticLogNormal(),
    options = .DefaultMcmcOptions())
}
