crmpack_extensions <- function(){

# LogisticNormalTruncPrior ----

## class ----

#' `LogisticNormalTruncPrior`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' [`LogisticNormalTruncPrior`] is the class for the usual logistic regression
#'  model with bivariate normal prior on the intercept and slope.
#'
#' @aliases LogisticNormalTruncPrior
#' @export
#'
#' @slot mean1 the mean of the intercept
#' @slot mean2 the mean of the slope
#' @slot var1 the variance of the intercept
#' @slot var2 the variance of the slope
#'
.LogisticNormalTruncPrior <- setClass(
  Class = "LogisticNormalTruncPrior",
  contains = "GeneralModel",
  slots = c(
    mean1 = "numeric",
    mean2 = "numeric",
    var1 = "numeric",
    var2 = "numeric"
  )
)

## constructor ----

#' @rdname LogisticNormalTruncPrior-class

#' Initialization function for the "TwoParBay" class
#'
#' @param mean1 the mean of the intercept
#' @param mean2 the mean of the slope
#' @param var1 the variance of the intercept
#' @param var2 the variance of the slope
# @return the \code{\linkS4class{TwoParBay}} object
#'
#' @export
#' @keywords methods
LogisticNormalTruncPrior <<- function(mean1, mean2, var1, var2) {
  .LogisticNormalTruncPrior(
    mean1 = mean1,
    mean2 = mean2,
    var1 = var1,
    var2 = var2,
    datamodel = function() {
        for (i in 1:nObs) {
          y[i] ~ dbern(mean[i])
          logit(mean[i]) <- alpha0 + alpha1 * x[i]
        }
      },
    priormodel = function() {
        alpha0 ~ dnorm(mean1, var1)
        alpha1 ~ dnorm(mean2, var2) %_% I(0, )
      },
    datanames = c("nObs", "y", "x"),
    modelspecs = function() {
        list(
          mean1 = mean1,
          mean2 = mean2,
          var1 = var1,
          var2 = var2
        )
    },
    init = function() {
        list(alpha0 = mean1, alpha1 = mean2)
    },
    sample = c("alpha0", "alpha1")
  )
}

## dose ----

#' @describeIn dose compute the dose level reaching a specific target
#'   probability of the occurrence of a DLE (`x`).
#'
#' @aliases dose-LogisticNormalTruncPrior
#' @export
#'
setMethod(
  f = "dose",
  signature = signature(
    x = "numeric",
    model = "LogisticNormalTruncPrior",
    samples = "Samples"
  ),
  definition = function(x, model, samples) {
    assert_probabilities(x)
    assert_subset(c("alpha0", "alpha1"), names(samples))
    assert_length(x, len = size(samples))

    alpha0 <- samples@data$alpha0
    alpha1 <- samples@data$alpha1
    (logit(x) - alpha0) / alpha1
  }
)

## prob ----

#' @describeIn prob
#'
#' @aliases prob-LogisticNormalTruncPrior
#' @export
#'
setMethod(
  f = "prob",
  signature = signature(
    dose = "numeric",
    model = "LogisticNormalTruncPrior",
    samples = "Samples"
  ),
  definition = function(dose, model, samples) {
    assert_numeric(dose, lower = 0L, any.missing = FALSE, min.len = 1)
    assert_subset(c("alpha0", "alpha1"), names(samples))
    assert_length(dose, len = size(samples))

    alpha0 <- samples@data$alpha0
    alpha1 <- samples@data$alpha1
    1/(1+exp(-alpha0 - alpha1 * dose))
  }
)

}


