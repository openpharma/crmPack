#####################################################################################
## Author: Daniel Sabanes Bove [sabanesd *a*t* roche *.* com]
## Project: Object-oriented implementation of CRM designs
##
## Time-stamp: <[Samples-methods.R] by DSB Mon 11/05/2015 17:46>
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

##' Get specific parameter samples and produce a data.frame
##'
##' Here you have to specify with \code{pos} which
##' parameter you would like to extract from the \code{\linkS4class{Samples}}
##' object
##'
##' @param x the \code{\linkS4class{Samples}} object
##' @param pos the name of the parameter
##' @param envir for vectorial parameters, you can give the indices of the
##' elements you would like to extract. If \code{NULL}, the whole vector samples
##' will be returned
##' @param mode not used
##' @param inherits not used
##'
##' @return the data frame suitable for use with \code{\link[ggmcmc]{ggmcmc}}
##'
##' @export
##' @keywords methods
setMethod("get",
          signature=
              signature(x="Samples",
                        pos="character",
                        envir="ANY",
                        mode="ANY",
                        inherits="ANY"),
          def=
          function(x,
                   pos,
                   envir=NULL,
                   mode=NULL,
                   inherits=NULL){

              ## check the parameter name
              stopifnot(is.scalar(pos),
                        pos %in% names(x@data))

              ## get the samples for this parameter
              d <- x@data[[pos]]
              ## this can be either a vector or a matrix

              ## how many parameters do we have?
              nPars <- NCOL(d)

              ## what are the names of all parameter
              ## elements?
              elements <-
                  if(nPars == 1L)
                      pos
                  else
                      paste(pos,
                            "[", seq_len(nPars), "]",
                            sep="")

              ## in case we have a vector parameter
              if(nPars > 1L)
              {
                  ## what are the indices to be returned?
                  indices <-
                      if(is.null(envir))
                      {
                          seq_along(elements)
                      } else {
                          stopifnot(is.numeric(envir),
                                    all(envir %in% seq_along(elements)))
                          as.integer(envir)
                      }

                  ## subset the data matrix and par names appropriately
                  d <- d[, indices, drop=FALSE]
                  elements <- elements[indices]

                  ## and also reduce the number of parameters
                  nPars <- length(indices)
              }

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
                               nParameters=nPars,
                               nIterations=x@options@iterations,
                               nBurnin=x@options@burnin,
                               nThin=x@options@step,
                               description=elements,
                               parallel=FALSE)
              return(ret)
          })


## --------------------------------------------------
## Get fitted curves from Samples
## --------------------------------------------------

##' Fit method for the Samples class
##'
##' Note this new generic function is necessary because the \code{\link{fitted}}
##' function only allows the first argument \code{object} to appear in the
##' signature. But we need also other arguments in the signature.
##'
##' @param object the \code{\linkS4class{Samples}} object
##' @param model the \code{\linkS4class{Model}} object
##' @param data the \code{\linkS4class{Data}} object
##' @param \dots unused
##' @return the data frame with required information (see method details)
##'
##' @export
##' @keywords methods
setGeneric("fit",
           def=
           function(object,
                    model,
                    data,
                    ...){
               ## there should be no default method,
               ## therefore just forward to next method!
               standardGeneric("fit")},
           valueClass="data.frame")


## --------------------------------------------------
## Get fitted dose-tox curve from Samples
## --------------------------------------------------

##' @param points at which dose levels is the fit requested? default is the dose
##' grid
##' @param quantiles the quantiles to be calculated (default: 0.025 and
##' 0.975)
##' @param middle the function for computing the middle point. Default:
##' \code{\link{mean}}
##'
##' @describeIn fit This method returns a data frame with dose, middle, lower
##' and upper quantiles for the dose-toxicity curve
setMethod("fit",
          signature=
          signature(object="Samples",
                    model="Model",
                    data="Data"),
          def=
          function(object,
                   model,
                   data,
                   points=data@doseGrid,
                   quantiles=c(0.025, 0.975),
                   middle=mean,
                   ...){
              ## some checks
              stopifnot(is.probRange(quantiles),
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
## Get fitted dose-tox and dose-biomarker curves from Samples
## --------------------------------------------------

##' @describeIn fit This method returns a data frame with dose, and middle,
##' lower and upper quantiles, for both the dose-tox and dose-biomarker (suffix
##' "Biomarker") curves, for all grid points (Note that currently only the grid
##' points can be used, because the DualEndpointRW models only allow that)
setMethod("fit",
          signature=
          signature(object="Samples",
                    model="DualEndpoint",
                    data="DataDual"),
          def=
          function(object,
                   model,
                   data,
                   quantiles=c(0.025, 0.975),
                   middle=mean,
                   ...){
              ## some checks
              stopifnot(is.probRange(quantiles))

              ## first obtain the dose-tox curve results from the parent method
              start <- callNextMethod(object=object,
                                      model=model,
                                      data=data,
                                      points=data@doseGrid,
                                      quantiles=quantiles,
                                      middle=middle,
                                      ...)

              ## now obtain the dose-biomarker results

              ## get the biomarker level samples
              ## at the dose grid points.
              biomLevelSamples <- matrix(nrow=sampleSize(object@options),
                                         ncol=data@nGrid)

              ## evaluate the biomLevels, for all samples.
              for(i in seq_len(data@nGrid))
              {
                  ## Now we want to evaluate for the
                  ## following dose:
                  biomLevelSamples[, i] <- biomLevel(dose=data@doseGrid[i],
                                                     xLevel=i,
                                                     model,
                                                     object)
              }

              ## extract middle curve
              middleCurve <- apply(biomLevelSamples, 2L, FUN=middle)

              ## extract quantiles
              quantCurve <- apply(biomLevelSamples, 2L, quantile,
                                  prob=quantiles)

              ## now create the data frame
              biomResults <- data.frame(middleBiomarker=middleCurve,
                                        lowerBiomarker=quantCurve[1, ],
                                        upperBiomarker=quantCurve[2, ])

              ## return both, pasted together
              return(cbind(start, biomResults))
          })


## --------------------------------------------------
## Approximate posterior with (log) normal distribution
## --------------------------------------------------

##' Approximate posterior with (log) normal distribution
##'
##' It is recommended to use \code{\link{set.seed}} before, in order
##' to be able to reproduce the resulting approximating model exactly.
##'
##' @param object the \code{\linkS4class{Samples}} object
##' @param model the \code{\linkS4class{Model}} object
##' @param data the \code{\linkS4class{Data}} object
##' @param \dots additional arguments (see methods)
##' @return the approximation model
##'
##' @export
##' @keywords methods
setGeneric("approximate",
           def=
           function(object, model, data, ...){
               ## there should be no default method,
               ## therefore just forward to next method!
               standardGeneric("approximate")},
           valueClass="Model")



##' @param points optional parameter, which gives the dose values at which
##' the approximation should rely on (default: 5 values equally spaced from
##' minimum to maximum of the dose grid)
##' @param refDose the reference dose to be used (default: median of
##' \code{points})
##' @param logNormal use the log-normal prior? (not default) otherwise, the
##' normal prior for the logistic regression coefficients is used
##' @param verbose be verbose (progress statements and plot)? (default)
##'
##' @describeIn approximate Here the \dots argument can transport additional arguments for
##' \code{\link{Quantiles2LogisticNormal}}, e.g. in order to control the
##' approximation quality, etc.
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
              quants <- fit(object,
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


##' Plotting dose-toxicity model fits
##'
##' @param x the \code{\linkS4class{Samples}} object
##' @param y the \code{\linkS4class{Model}} object
##' @param data the \code{\linkS4class{Data}} object
##' @param xlab the x axis label
##' @param ylab the y axis label
##' @param showLegend should the legend be shown? (default)
##' @param \dots not used
##' @return This returns the \code{\link[ggplot2]{ggplot}}
##' object for the dose-toxicity model fit
##'
##' @export
##' @importFrom ggplot2 qplot scale_linetype_manual
setMethod("plot",
          signature=
          signature(x="Samples",
                    y="Model"),
          def=
          function(x, y, data, ...,
                   xlab="Dose level",
                   ylab="Probability of DLT [%]",
                   showLegend=TRUE){

              ## check args
              stopifnot(is.bool(showLegend))

              ## get the fit
              plotData <- fit(x,
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
                                                 values=c(1,2), guide=ifelse(showLegend,
                                                 "legend", FALSE))

              return(ret)
          })


## --------------------------------------------------
## Special method for dual endpoint model
## --------------------------------------------------


##' Plotting dose-toxicity and dose-biomarker model fits
##'
##' When we have the dual endpoint model,
##' also the dose-biomarker fit is shown in the plot
##'
##' @param x the \code{\linkS4class{Samples}} object
##' @param y the \code{\linkS4class{DualEndpoint}} object
##' @param data the \code{\linkS4class{DataDual}} object
##' @param extrapolate should the biomarker fit be extrapolated to the whole
##' dose grid? (default)
##' @param showLegend should the legend be shown? (not default)
##' @param \dots additional arguments for the parent method
##' \code{\link{plot,Samples,Model-method}}
##' @return This returns the \code{\link[ggplot2]{ggplot}}
##' object with the dose-toxicity and dose-biomarker model fits
##'
##' @export
setMethod("plot",
          signature=
          signature(x="Samples",
                    y="DualEndpoint"),
          def=
          function(x, y, data, extrapolate=TRUE, showLegend=FALSE, ...){

              stopifnot(is.bool(extrapolate))

              ## call the superclass method, to get the toxicity plot
              plot1 <- callNextMethod(x, y, data, showLegend=showLegend, ...)

              ## only look at these dose levels for the plot:
              xLevels <-
                  if(extrapolate)
                      seq_along(data@doseGrid)
                  else
                      1:max(data@xLevel)

              ## get the plot data for the biomarker plot
              functionSamples <- matrix(nrow=sampleSize(x@options),
                                        ncol=length(xLevels))

              ## evaluate the biomLevels, for all samples.
              for(i in seq_along(xLevels))
              {
                  ## Now we want to evaluate for the
                  ## following dose:
                  functionSamples[, i] <-
                      biomLevel(dose=data@doseGrid[xLevels[i]],
                                xLevel=xLevels[i],
                                model=y,
                                samples=x)
              }

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
                                  y=c(mean, lower, upper),
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
                                                 values=c(1,2),
                                                 guide=ifelse(showLegend,
                                                 "legend", FALSE))

              ## arrange both plots side by side
              ret <- gridExtra::arrangeGrob(plot1, plot2, ncol=2)
              return(ret)
          })


