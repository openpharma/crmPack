#' @include McmcOptions-class.R
NULL

# size ----

## generic ----

#' Size of an Object
#'
#' @description `r lifecycle::badge("stable")`
#'
#' A method that computes the size of a given object. This can be for instance
#' a size of a MCMC sample, or the size of a cohort. See the help of a specific
#' method for more details.
#'
#' @param object (`McmcOptions` or `Samples` or `CohortSize`)\cr an object
#'   for which the size is computed.
#' @param ... further arguments passed to `size` specific methods.
#'
#' @return A size of a given object.
#' @export
#'
setGeneric(
  name = "size",
  def = function(object, ...) {
    standardGeneric("size")
  },
  valueClass = "integer"
)

## McmcOptions ----

#' @describeIn size compute the number of MCMC samples based on `McmcOptions`
#'   object.
#' @aliases size-McmcOptions
#' @example examples/McmcOptions-methods-size.R
setMethod(
  f = "size",
  signature = signature(object = "McmcOptions"),
  definition = function(object, ...) {
    iterations_relative <- object@iterations - object@burnin
    if (iterations_relative <= 0) {
      return(0L)
    }
    as.integer(floor(iterations_relative / object@step))
  }
)

# saveSample ----

## generic ----

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

## McmcOptions ----

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
