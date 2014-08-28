#####################################################################################
## Author: Daniel Sabanes Bove [sabanesd *a*t* roche *.* com]
## Project: Object-oriented implementation of CRM designs
##
## Time-stamp: <[Samples-methods.R] by DSB Don 28/08/2014 17:34>
##
## Description:
## Methods for processing the MCMC samples.
##
## History:
## 25/03/2014   file creation
#####################################################################################

##' @include McmcOptions-methods.R
##' @include Model-methods.R
##' @include fromQuantiles.R
{}

## --------------------------------------------------
## Extract certain parameter from "Samples" object to produce
## plots with "ggmcmc" package
## --------------------------------------------------


##' Extract something from an object and produce a data.frame
##'
##' @param object the object
##' @param \dots unused
##' @return the data frame
##'
##' @genericMethods
##' @export
##' @keywords methods
setGeneric("extract",
           def=
           function(object, ...){
               ## there should be no default method,
               ## therefore just forward to next method!
               standardGeneric("extract")},
           valueClass="data.frame")

## --------------------------------------------------
## The method for "Samples"
## --------------------------------------------------

##' Extract certain parameter from Samples object
##'
##' @param object the \code{\linkS4class{Samples}} object
##' @param parameter the name of the parameter
##' @return the data frame suitable for use with \code{\link[ggmcmc]{ggmcmc}}
##'
##' @export
##' @keywords methods
setMethod("extract",
          signature=
          signature(object="Samples"),
          def=
          function(object,
                   parameter,
                   ...){

              ## check the parameter name
              stopifnot(is.character(parameter),
                        is.scalar(parameter),
                        parameter %in% names(object@data))

              ## get the samples for this parameter
              d <- object@data[[parameter]]
              ## this can be either a vector or a matrix

              ## what are the names of the parameter
              ## elements?
              elements <-
                  if(NCOL(d) == 1L)
                      parameter
                  else
                      paste(parameter,
                            "[", seq_len(NCOL(d)), "]",
                            sep="")

              ## now we can build
              ret <- data.frame(Iteration=seq_len(NROW(d)),
                                Chain=1L,
                                Parameter=
                                factor(rep(elements, each=NROW(d)),
                                       levels=elements),
                                value=as.numeric(d))

              ## add the attributes
              ret <- structure(ret,
                               nChains=1L,
                               nParameters=NCOL(d),
                               nIterations=object@options@iterations,
                               nBurnin=object@options@burnin,
                               nThin=object@options@step,
                               description=parameter,
                               parallel=FALSE)
              return(ret)
          })


## --------------------------------------------------
## Get fitted dose-tox curve from Samples
## --------------------------------------------------

##' Fit method for the Samples class
##'
##' @param object the \code{\linkS4class{Samples}} object
##' @param model the \code{\linkS4class{Model}} object
##' @param data the \code{\linkS4class{Data}} object
##' @param points at which dose levels is the fit requested? default is the dose
##' grid
##' @param quantiles the quantiles to be calculated (default: 0.025 and
##' 0.975)
##' @param middle the function for computing the middle point. Default:
##' \code{\link{mean}}
##' @return data frame with dose, middle, lower and upper quantiles
##'
##' @export
##' @keywords methods
setMethod("fitted",
          signature=
          signature(object="Samples"),
          def=
          function(object,
                   model,
                   data,
                   points=data@doseGrid,
                   quantiles=c(0.025, 0.975),
                   middle=mean,
                   ...){
              ## some checks
              stopifnot(is(model, "Model"),
                        is(data, "Data"),
                        is.probRange(quantiles),
                        is.numeric(points))

              ## first we have to get samples from the dose-tox
              ## curve at the dose grid points.
              probSamples <- matrix(nrow=sampleSize(object@options),
                                    ncol=length(points))

              ## evaluate the probs, for all samples.
              for(i in seq_along(points))
              {
                  ## Now we want to evaluate for the
                  ## following dose:
                  probSamples[, i] <- prob(dose=points[i],
                                           model,
                                           object)
              }

              ## extract middle curve
              middleCurve <- apply(probSamples, 2L, FUN=middle)

              ## extract quantiles
              quantCurve <- apply(probSamples, 2L, quantile,
                                  prob=quantiles)

              ## now create the data frame
              ret <- data.frame(dose=points,
                                middle=middleCurve,
                                lower=quantCurve[1, ],
                                upper=quantCurve[2, ])

              ## return it
              return(ret)
          })

## --------------------------------------------------
## Approximate posterior with (log) normal distribution
## --------------------------------------------------

##' Approximate posterior with (log) normal distribution
##'
##' @param object the object
##' @param \dots unused
##' @return the approximation model
##'
##' @genericMethods
##' @export
##' @keywords methods
setGeneric("approximate",
           def=
           function(object, ...){
               ## there should be no default method,
               ## therefore just forward to next method!
               standardGeneric("approximate")},
           valueClass="Model")


##' Approximate posterior with (log) normal distribution
##'
##' It is recommended to use \code{\link{set.seed}} before, in order
##' to be able to reproduce the resulting approximating model exactly.
##'
##' @param object the \code{\linkS4class{Samples}} object
##' @param model the \code{\linkS4class{Model}} object
##' @param data the \code{\linkS4class{Data}} object
##' @param points optional parameter, which gives the dose values at which
##' the approximation should rely on (default: 5 values equally spaced from
##' minimum to maximum of the dose grid)
##' @param refDose the reference dose to be used (default: median of
##' \code{points})
##' @param logNormal use the log-normal prior? (not default) otherwise, the
##' normal prior for the logistic regression coefficients is used
##' @param verbose be verbose (progress statements and plot)? (default)
##' @param \dots additional arguments for
##' \code{\link{Quantiles2LogisticNormal}}, e.g. in order to control the
##' approximation quality, etc.
##' @return the approximation \code{\linkS4class{Model}}
##'
##' @export
##' @keywords methods
setMethod("approximate",
          signature=
          signature(object="Samples"),
          def=
          function(object,
                   model,
                   data,
                   points=
                   seq(from=min(data@doseGrid),
                       to=max(data@doseGrid),
                       length=5L),
                   refDose=median(points),
                   logNormal=FALSE,
                   verbose=TRUE,
                   ...){

              ## get the required quantiles at these dose levels:
              quants <- fitted(object,
                               model,
                               data,
                               points=points,
                               quantiles=c(0.025, 0.975),
                               middle=median)

              ## get better starting values if it is already a logistic normal
              ## model
              if(is(model, "LogisticNormal") && (! logNormal))
              {
                  means <- sapply(object@data,
                                  mean)
                  cov <- cov(as.data.frame(object@data))

                  parstart <- c(means[1], means[2],
                                sqrt(cov[1, 1]), sqrt(cov[2, 2]),
                                cov2cor(cov)[1, 2])
              } else if(is(model, "LogisticLogNormal") && logNormal) {
                  datTrafo <- with(object@data,
                                   cbind(alpha0,
                                         log(alpha1)))

                  means <- colMeans(datTrafo)
                  cov <- cov(datTrafo)

                  parstart <- c(means[1], means[2],
                                sqrt(cov[1, 1]), sqrt(cov[2, 2]),
                                cov2cor(cov)[1, 2])
              } else {
                  parstart <- NULL
              }

              ## run the approx function
              quantRes <- Quantiles2LogisticNormal(dosegrid=quants$dose,
                                                   refDose=refDose,
                                                   lower=quants$lower,
                                                   upper=quants$upper,
                                                   median=quants$middle,
                                                   verbose=verbose,
                                                   parstart=parstart,
                                                   logNormal=logNormal,
                                                   ...)

              if(verbose)
              {
                  matplot(x=points,
                          quantRes$required,
                          type="l", col="blue", lty=1)
                  matlines(x=points,
                           quantRes$quantiles,
                           col="red", lty=1)
                  legend("bottomright",
                         legend=c("original", "approximation"),
                         col=c("blue", "red"),
                         lty=1,
                         bty="n")
              }

              ## return the model
              return(quantRes$model)
          })

## --------------------------------------------------
## Plot dose-tox fit from a model
## --------------------------------------------------

##' Plot method for the "Samples" and "Model" object
##'
##' @param x the \code{\linkS4class{Samples}} object
##' @param y the \code{\linkS4class{Model}} object
##' @param data the \code{\linkS4class{Data}} object
##' @param xlab the x axis label
##' @param ylab the y axis label
##' @return the \code{\link[ggplot2]{ggplot}} object
##'
##' @export
##' @importFrom ggplot2 qplot scale_linetype_manual
##' @keywords methods
setMethod("plot",
          signature=
          signature(x="Samples",
                    y="Model"),
          def=
          function(x, y, data, ...,
                   xlab="Dose level",
                   ylab="Probability of DLT [%]"){

              ## get the fit
              plotData <- fitted(x,
                                 model=y,
                                 data=data,
                                 quantiles=c(0.025, 0.975),
                                 middle=mean)

              ## make the plot
              gdata <-
                  with(plotData,
                       data.frame(x=rep(dose, 3),
                                  y=c(middle, lower, upper) * 100,
                                  group=
                                  rep(c("mean", "lower", "upper"),
                                      each=nrow(plotData)),
                                  Type=
                                  factor(c(rep("Estimate",
                                               nrow(plotData)),
                                           rep("95% Credible Interval",
                                               nrow(plotData) * 2)),
                                         levels=
                                         c("Estimate",
                                           "95% Credible Interval"))))

              ret <- ggplot2::qplot(x=x,
                                    y=y,
                                    data=gdata,
                                    group=group,
                                    linetype=Type,
                                    colour=I("red"),
                                    geom="line",
                                    xlab=xlab,
                                    ylab=ylab,
                                    ylim=c(0, 100))

              ret <- ret +
                  ggplot2::scale_linetype_manual(breaks=
                                                 c("Estimate",
                                                   "95% Credible Interval"),
                                                 values=c(1,2))

              return(ret)
          })


## --------------------------------------------------
## Special method for dual endpoint model
## --------------------------------------------------

##' Plot method for the "Samples" object, when we have
##' the dual endpoint model
##'
##' @param x the \code{\linkS4class{Samples}} object
##' @param y the \code{\linkS4class{DualEndpoint}} object
##' @param data the \code{\linkS4class{DataDual}} object
##' @param extrapolate should the biomarker fit be extrapolated to the whole
##' dose grid? (default)
##' @return the \code{\link[ggplot2]{ggplot}} object
##'
##' @export
##' @importFrom ggplot2 qplot scale_linetype_manual
##' @keywords methods
setMethod("plot",
          signature=
          signature(x="Samples",
                    y="DualEndpoint"),
          def=
          function(x, y, data, extrapolate=TRUE, ...){

              ## call the superclass method, to get the toxicity plot
              plot1 <- callNextMethod(x, y, data, ...)

              ## only look at these dose levels for the plot:
              xLevels <-
                  if(extrapolate)
                      seq_along(data@doseGrid)
                  else
                      1:max(data@xLevel)

              ## get the plot data for the biomarker plot
              functionSamples <- with(samples@data,
                                      betaWintercept + betaW)[, xLevels, drop=FALSE]

              ## extract mean curve
              meanCurve <- colMeans(functionSamples)

              ## extract quantiles
              quantiles <- c(0.025, 0.975)
              quantCurve <- apply(functionSamples, 2L, quantile,
                                  prob=quantiles)

              ## now create the data frame
              plotData <- data.frame(dose=data@doseGrid[xLevels],
                                     mean=meanCurve,
                                     lower=quantCurve[1, ],
                                     upper=quantCurve[2, ])

              ## make the second plot
              gdata <-
                  with(plotData,
                       data.frame(x=rep(dose, 3),
                                  y=c(middle, lower, upper),
                                  group=
                                  rep(c("mean", "lower", "upper"),
                                      each=nrow(plotData)),
                                  Type=
                                  factor(c(rep("Estimate",
                                               nrow(plotData)),
                                           rep("95% Credible Interval",
                                               nrow(plotData) * 2)),
                                         levels=
                                         c("Estimate",
                                           "95% Credible Interval"))))

              plot2 <- ggplot2::qplot(x=x,
                                      y=y,
                                      data=gdata,
                                      group=group,
                                      linetype=Type,
                                      colour=I("blue"),
                                      geom="line",
                                      xlab="Dose level",
                                      ylab="Biomarker level")

              plot2 <- plot2 +
                  ggplot2::scale_linetype_manual(breaks=
                                                 c("Estimate",
                                                   "95% Credible Interval"),
                                                 values=c(1,2))

              ## arrange both plots side by side
              ret <- gridExtra::arrangeGrob(plot1, plot2, ncol=2)
              return(ret)
          })
