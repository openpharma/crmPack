# sample_size ----

#' Computing the Number of Samples
#'
#' @description `r lifecycle::badge("stable")`
#'
#' A method that computes the number of samples for a given MCMC options triple.
#'
#' @param object (`McmcOptions`)\cr object based on which the number of samples
#'   is computed.
#' @param ... not used.
#' @return A number of samples for a given MCMC options.
#' @export
#'
setGeneric(
  name = "sample_size",
  def = function(object, ...) {
    standardGeneric("sample_size")
  },
  valueClass = "integer"
)

# McmcOptions-sample_size ----

#' @rdname sample_size
#' @aliases sample_size-McmcOptions-method
#' @example examples/McmcOptions-methods-sample_size.R
setMethod(
  f = "sample_size",
  signature = signature(object = "McmcOptions"),
  definition = function(object, ...) {
    # Iteration numbers relative to object@burnin.
    iterations_relative <- object@iterations - object@burnin
    safeInteger(ceiling(iterations_relative / object@step))
  }
)

# save_sample ----

#' Determining if this Sample Should be Saved
#'
#' @description `r lifecycle::badge("stable")`
#'
#' A method that determines if a sample from a given `iteration` should be
#'   saved. The sample should be save if and only if:
#'   (1) it is not in burn-in period and (2) it matches eriod and
#'
#' @param object (`McmcOptions`)\cr object based on which the answer is
#'   determined.
#' @param iteration (`count`)\cr the current iteration index.
#' @param ... not used.
#' @return `TRUE` if this sample should be saved.
#' @export
#'
setGeneric(
  name = "save_sample",
  def = function(object, iteration, ...) {
    standardGeneric("save_sample")
  },
  valueClass = "logical"
)

# McmcOptions-save_sample ----

#' @rdname save_sample
#' @aliases save_sample-McmcOptions-method
#' @example examples/McmcOptions-methods-save_sample.R
setMethod(
  f = "save_sample",
  signature = signature(object = "McmcOptions"),
  definition = function(object, iteration, ...) {
    iteration_relative <- iteration - object@burnin # Relative to object@burnin.
    iteration_relative > 0 && ((iteration_relative %% object@step) == 0)
  }
)
