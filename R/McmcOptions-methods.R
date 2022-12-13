#' @include McmcOptions-class.R
NULL

# sampleSize ----

#' Computing the Number of Samples
#'
#' @description `r lifecycle::badge("stable")`
#'
#' A method that computes the number of MCMC samples.
#'
#' @param object (`McmcOptions` or `Samples`)\cr object based on which the number
#'   of samples is computed.
#' @param ... not used.
#' @return A number of samples.
#' @export
#'
setGeneric(
  name = "sampleSize",
  def = function(object, ...) {
    standardGeneric("sampleSize")
  },
  valueClass = "integer"
)

# sampleSize-McmcOptions ----

#' @describeIn sampleSize compute the number of MCMC samples based on `McmcOptions`
#'   object.
#' @aliases sampleSize-McmcOptions
#' @example examples/McmcOptions-methods-sampleSize.R
setMethod(
  f = "sampleSize",
  signature = signature(object = "McmcOptions"),
  definition = function(object, ...) {
    iterations_relative <- object@iterations - object@burnin
    if (iterations_relative <= 0) {
      return(0L)
    }
    safeInteger(floor(iterations_relative / object@step))
  }
)

# saveSample ----

#' Determining if this Sample Should be Saved
#'
#' @description `r lifecycle::badge("stable")`
#'
#' A method that determines if a sample from a given `iteration` should be
#' saved. The sample should be saved if and only if:
#' it is not in burn-in period and it matches the `step`.
#'
#' @param object (`McmcOptions`)\cr object based on which the answer is
#'   determined.
#' @param iteration (`count`)\cr the current iteration index.
#' @param ... not used.
#' @return `TRUE` if this sample should be saved.
#' @export
#'
setGeneric(
  name = "saveSample",
  def = function(object, iteration, ...) {
    standardGeneric("saveSample")
  },
  valueClass = "logical"
)

# saveSample-McmcOptions ----

#' @describeIn saveSample determine if a sample should be saved.
#' @aliases saveSample-McmcOptions
#' @example examples/McmcOptions-methods-saveSample.R
setMethod(
  f = "saveSample",
  signature = signature(object = "McmcOptions"),
  definition = function(object, iteration, ...) {
    iteration_relative <- iteration - object@burnin
    iteration_relative > 0 && ((iteration_relative %% object@step) == 0)
  }
)
