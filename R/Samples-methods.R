#' @include McmcOptions-class.R
#' @include Model-methods.R
#' @include fromQuantiles.R
NULL

# size ----

## Samples ----

#' @describeIn size get the number of MCMC samples from `Samples` object.
#' @aliases size-Samples
#'
#' @export
#' @example examples/Samples-methods-size.R
#'
setMethod(
  f = "size",
  signature = signature(object = "Samples"),
  definition = function(object, ...) {
    size(object@options)
  }
)

# names ----

## Samples ----

#' The Names of the Sampled Parameters
#'
#' @description `r lifecycle::badge("stable")`
#'
#' A method that returns names of the parameters that are sampled.
#'
#' @param x (`Samples`)\cr object with samples.
#'
#' @aliases names-Samples
#' @export
#' @example examples/Samples-methods-names.R
#'
setMethod(
  f = "names",
  signature = signature(x = "Samples"),
  definition = function(x) {
    names(x@data)
  }
)

## --------------------------------------------------
## Extract certain parameter from "Samples" object to produce
## plots with "ggmcmc" package
## --------------------------------------------------

# The next line is required to suppress the message "Creating a generic function
# for ‘get’ from package ‘base’ in package ‘crmPack’" on package load.
# See https://github.com/Roche/crmPack/issues/723
setGeneric("get")

#' Get specific parameter samples and produce a data.frame
#'
#' Here you have to specify with \code{pos} which
#' parameter you would like to extract from the \code{\linkS4class{Samples}}
#' object
#'
#' @param x the \code{\linkS4class{Samples}} object
#' @param pos the name of the parameter
#' @param envir for vectorial parameters, you can give the indices of the
#' elements you would like to extract. If \code{NULL}, the whole vector samples
#' will be returned
#' @param mode not used
#' @param inherits not used
#'
#' @return the data frame suitable for use with \code{\link[ggmcmc]{ggmcmc}}
#'
#' @example examples/Sample-methods-get.R
#' @export
#' @keywords methods
setMethod("get",
  signature =
    signature(
      x = "Samples",
      pos = "character",
      envir = "ANY",
      mode = "ANY",
      inherits = "ANY"
    ),
  def =
    function(x,
             pos,
             envir = NULL,
             mode = NULL,
             inherits = NULL) {
      ## check the parameter name
      assert_scalar(pos)
      assert_choice(pos, names(x))

      ## get the samples for this parameter
      d <- x@data[[pos]]
      ## this can be either a vector or a matrix

      ## how many parameters do we have?
      nPars <- NCOL(d)

      ## what are the names of all parameter
      ## elements?
      elements <-
        if (nPars == 1L) {
          pos
        } else {
          paste(pos,
            "[", seq_len(nPars), "]",
            sep = ""
          )
        }

      ## in case we have a vector parameter
      if (nPars > 1L) {
        ## what are the indices to be returned?
        indices <-
          if (is.null(envir)) {
            seq_along(elements)
          } else {
            assert_integer(envir)
            assert_subset(envir, seq_along(elements))
          }

        ## subset the data matrix and par names appropriately
        d <- d[, indices, drop = FALSE]
        elements <- elements[indices]

        ## and also reduce the number of parameters
        nPars <- length(indices)
      }

      ## now we can build
      ret <- data.frame(
        Iteration = seq_len(NROW(d)),
        Chain = 1L,
        Parameter =
          factor(rep(elements, each = NROW(d)),
            levels = elements
          ),
        value = as.numeric(d)
      )

      ## add the attributes
      ret <- structure(ret,
        nChains = 1L,
        nParameters = nPars,
        nIterations = x@options@iterations,
        nBurnin = x@options@burnin,
        nThin = x@options@step,
        description = elements,
        parallel = FALSE
      )
      return(ret)
    }
)


## --------------------------------------------------
## Get fitted curves from Samples
## --------------------------------------------------

#' Fit method for the Samples class
#'
#' Note this new generic function is necessary because the \code{\link{fitted}}
#' function only allows the first argument \code{object} to appear in the
#' signature. But we need also other arguments in the signature.
#'
#' @param object the \code{\linkS4class{Samples}} object
#' @param model the \code{\linkS4class{GeneralModel}} object
#' @param data the \code{\linkS4class{Data}} object
#' @param \dots passed down to the [prob()] method.
#' @return the data frame with required information (see method details)
#'
#' @export
#' @keywords methods
setGeneric("fit",
  def =
    function(object,
             model,
             data,
             ...) {
      ## there should be no default method,
      ## therefore just forward to next method!
      standardGeneric("fit")
    },
  valueClass = "data.frame"
)


## --------------------------------------------------
## Get fitted dose-tox curve from Samples
## --------------------------------------------------

#' @param points at which dose levels is the fit requested? default is the dose
#' grid
#' @param quantiles the quantiles to be calculated (default: 0.025 and
#' 0.975)
#' @param middle the function for computing the middle point. Default:
#' \code{\link{mean}}
#'
#' @describeIn fit This method returns a data frame with dose, middle, lower
#' and upper quantiles for the dose-toxicity curve
#' @example examples/Sample-methods-fit.R
#'
setMethod("fit",
  signature =
    signature(
      object = "Samples",
      model = "GeneralModel",
      data = "Data"
    ),
  def =
    function(object,
             model,
             data,
             points = data@doseGrid,
             quantiles = c(0.025, 0.975),
             middle = mean,
             ...) {
      ## some checks
      assert_probability_range(quantiles)
      assert_numeric(points)

      ## first we have to get samples from the dose-tox
      ## curve at the dose grid points.
      probSamples <- matrix(
        nrow = size(object),
        ncol = length(points)
      )

      ## evaluate the probs, for all samples.
      for (i in seq_along(points)) {
        ## Now we want to evaluate for the
        ## following dose:
        probSamples[, i] <- prob(
          dose = points[i],
          model,
          object,
          ...
        )
      }

      ## extract middle curve
      middleCurve <- apply(probSamples, 2L, FUN = middle)

      ## extract quantiles
      quantCurve <- apply(probSamples, 2L, quantile,
        prob = quantiles
      )

      ## now create the data frame
      ret <- data.frame(
        dose = points,
        middle = middleCurve,
        lower = quantCurve[1, ],
        upper = quantCurve[2, ]
      )

      ## return it
      return(ret)
    }
)

## --------------------------------------------------
## Get fitted dose-tox and dose-biomarker curves from Samples
## --------------------------------------------------

#' @describeIn fit This method returns a data frame with dose, and middle,
#' lower and upper quantiles, for both the dose-tox and dose-biomarker (suffix
#' "Biomarker") curves, for all grid points (Note that currently only the grid
#' points can be used, because the DualEndpointRW models only allow that)
#'
#' @example examples/Sample-methods-fit-DualEndpoint.R
setMethod("fit",
  signature =
    signature(
      object = "Samples",
      model = "DualEndpoint",
      data = "DataDual"
    ),
  def =
    function(object,
             model,
             data,
             quantiles = c(0.025, 0.975),
             middle = mean,
             ...) {
      ## some checks
      assert_probability_range(quantiles)

      ## first obtain the dose-tox curve results from the parent method
      start <- callNextMethod(
        object = object,
        model = model,
        data = data,
        points = data@doseGrid,
        quantiles = quantiles,
        middle = middle,
        ...
      )

      ## now obtain the dose-biomarker results

      ## get the biomarker level samples
      ## at the dose grid points.
      biomLevelSamples <- biomarker(xLevel = seq_len(data@nGrid), model, samples = object)

      ## extract middle curve
      middleCurve <- apply(biomLevelSamples, 2L, FUN = middle)

      ## extract quantiles
      quantCurve <- apply(biomLevelSamples, 2L, quantile,
        prob = quantiles
      )

      ## now create the data frame
      biomResults <- data.frame(
        middleBiomarker = middleCurve,
        lowerBiomarker = quantCurve[1, ],
        upperBiomarker = quantCurve[2, ]
      )

      ## return both, pasted together
      return(cbind(start, biomResults))
    }
)

## --------------------------------------------------
## Approximate posterior with (log) normal distribution
## --------------------------------------------------

#' Approximate posterior with (log) normal distribution
#'
#' To reproduce the resultant approximate model in the future exactly, include
#' \code{seed = xxxx} in the call to `approximate`.
#'
#' @param object the \code{\linkS4class{Samples}} object
#' @param model the \code{\linkS4class{GeneralModel}} object
#' @param data the \code{\linkS4class{Data}} object
#' @param \dots additional arguments (see methods)
#' @return a `list` containing the approximation model and, if requested, a
#' `ggplot2` object containing a graphical representation of the fitted model
#'
#' @export
#' @keywords methods
setGeneric("approximate",
  def =
    function(object, model, data, ...) {
      ## there should be no default method,
      ## therefore just forward to next method!
      standardGeneric("approximate")
    },
  valueClass = "list"
)


##' @param points optional parameter, which gives the dose values at which
##' the approximation should rely on (default: 5 values equally spaced from
##' minimum to maximum of the dose grid)
##' @param refDose the reference dose to be used (default: median of
##' \code{points})
##' @param logNormal use the log-normal prior? (not default) otherwise, the
##' normal prior for the logistic regression coefficients is used
##' @param verbose be verbose (progress statements)? (default)
##' @param create_plot add a `ggplot2` object to the return value (default)
##'
##' @describeIn approximate Here the \dots argument can transport additional arguments for
##' \code{\link{Quantiles2LogisticNormal}}, e.g. in order to control the
##' approximation quality, etc.
##'
##' @example examples/Sample-methods-approximate.R
setMethod("approximate",
  signature =
    signature(object = "Samples"),
  def =
    function(object,
             model,
             data,
             points =
               seq(
                 from = min(data@doseGrid),
                 to = max(data@doseGrid),
                 length = 5L
               ),
             refDose = median(points),
             logNormal = FALSE,
             verbose = TRUE,
             create_plot = TRUE,
             ...) {
      # Validation
      assert_logical(logNormal)
      assert_logical(verbose)
      assert_logical(create_plot)
      assert_numeric(points)
      assert_numeric(refDose)
      ## get the required quantiles at these dose levels:
      quants <- fit(object,
        model,
        data,
        points = points,
        quantiles = c(0.025, 0.975),
        middle = median
      )

      ## get better starting values if it is already a logistic normal
      ## model
      if (is(model, "LogisticNormal") && (!logNormal)) {
        means <- sapply(
          object@data,
          mean
        )
        cov <- cov(as.data.frame(object@data))

        parstart <- c(
          means[1], means[2],
          sqrt(cov[1, 1]), sqrt(cov[2, 2]),
          cov2cor(cov)[1, 2]
        )
      } else if (is(model, "LogisticLogNormal") && logNormal) {
        datTrafo <- with(
          object@data,
          cbind(
            alpha0,
            log(alpha1)
          )
        )

        means <- colMeans(datTrafo)
        cov <- cov(datTrafo)

        parstart <- c(
          means[1], means[2],
          sqrt(cov[1, 1]), sqrt(cov[2, 2]),
          cov2cor(cov)[1, 2]
        )
      } else {
        parstart <- NULL
      }

      ## run the approx function
      quantRes <- Quantiles2LogisticNormal(
        dosegrid = quants$dose,
        refDose = refDose,
        lower = quants$lower,
        upper = quants$upper,
        median = quants$middle,
        verbose = verbose,
        parstart = parstart,
        logNormal = logNormal,
        ...
      )
      rv <- list()
      rv$model <- quantRes$model
      if (create_plot) {
        rv$plot <- tibble::as_tibble(quantRes$required) %>%
          tibble::add_column(Type = "original") %>%
          tibble::add_column(x = points) %>%
          dplyr::bind_rows(
            tibble::as_tibble(quantRes$quantiles) %>%
              tibble::add_column(Type = "approximation") %>%
              tibble::add_column(x = points)
          ) %>%
          tidyr::pivot_longer(
            c(lower, median, upper),
            names_to = "Line",
            values_to = "y"
          ) %>%
          ggplot(
            aes(
              x = x,
              y = y,
              colour = Type,
              group = interaction(Type, .data$Line),
              linetype = (.data$Line == "median")
            )
          ) +
          geom_line() +
          scale_colour_manual(
            name = " ",
            values = c("red", "blue")
          ) +
          scale_linetype_manual(
            name = " ",
            values = c("dotted", "solid"),
            labels = c("95% CI", "Median"),
            guide = guide_legend(reverse = TRUE)
          ) +
          labs(
            x = "Dose",
            y = "p(Tox)"
          ) +
          theme_light()
      }

      ## return the results
      return(rv)
    }
)

## --------------------------------------------------
## Plot dose-tox fit from a model
## --------------------------------------------------


#' Plotting dose-toxicity model fits
#'
#' @param x the \code{\linkS4class{Samples}} object
#' @param y the \code{\linkS4class{GeneralModel}} object
#' @param data the \code{\linkS4class{Data}} object
#' @param xlab the x axis label
#' @param ylab the y axis label
#' @param showLegend should the legend be shown? (default)
#' @param \dots not used
#' @return This returns the \code{\link[ggplot2]{ggplot}}
#' object for the dose-toxicity model fit
#'
#' @example examples/Sample-methods-plot.R
#' @export
setMethod("plot",
  signature =
    signature(
      x = "Samples",
      y = "GeneralModel"
    ),
  def =
    function(x, y, data, ...,
             xlab = "Dose level",
             ylab = "Probability of DLT [%]",
             showLegend = TRUE) {
      ## check args
      assert_logical(showLegend)

      ## get the fit
      plotData <- fit(x,
        model = y,
        data = data,
        quantiles = c(0.025, 0.975),
        middle = mean,
        ...
      )

      ## make the plot
      gdata <-
        with(
          plotData,
          data.frame(
            x = rep(dose, 3),
            y = c(middle, lower, upper) * 100,
            group =
              rep(c("mean", "lower", "upper"),
                each = nrow(plotData)
              ),
            Type =
              factor(
                c(
                  rep(
                    "Estimate",
                    nrow(plotData)
                  ),
                  rep(
                    "95% Credible Interval",
                    nrow(plotData) * 2
                  )
                ),
                levels =
                  c(
                    "Estimate",
                    "95% Credible Interval"
                  )
              )
          )
        )

      ret <- gdata %>% ggplot() +
        geom_line(
          aes(
            x = x,
            y = y,
            group = group,
            linetype = Type,
          ),
          colour = I("red"),
        ) +
        coord_cartesian(ylim = c(0, 100)) +
        labs(
          x = xlab,
          y = ylab,
        )

      ret <- ret +
        scale_linetype_manual(
          breaks =
            c(
              "Estimate",
              "95% Credible Interval"
            ),
          values = c(1, 2), guide = ifelse(showLegend, "legend", "none")
        )

      return(ret)
    }
)


## --------------------------------------------------
## Special method for dual endpoint model
## --------------------------------------------------


#' Plotting dose-toxicity and dose-biomarker model fits
#'
#' When we have the dual endpoint model,
#' also the dose-biomarker fit is shown in the plot
#'
#' @param x the \code{\linkS4class{Samples}} object
#' @param y the \code{\linkS4class{DualEndpoint}} object
#' @param data the \code{\linkS4class{DataDual}} object
#' @param extrapolate should the biomarker fit be extrapolated to the whole
#' dose grid? (default)
#' @param showLegend should the legend be shown? (not default)
#' @param \dots additional arguments for the parent method
#' \code{\link{plot,Samples,GeneralModel-method}}
#' @return This returns the \code{\link[ggplot2]{ggplot}}
#' object with the dose-toxicity and dose-biomarker model fits
#'
#' @example examples/Sample-methods-plot-DualEndpoint.R
#' @export
setMethod("plot",
  signature =
    signature(
      x = "Samples",
      y = "DualEndpoint"
    ),
  def =
    function(x, y, data, extrapolate = TRUE, showLegend = FALSE, ...) {
      assert_logical(extrapolate)

      ## call the superclass method, to get the toxicity plot
      plot1 <- callNextMethod(x, y, data, showLegend = showLegend, ...)

      ## only look at these dose levels for the plot:
      xLevels <-
        if (extrapolate) {
          seq_along(data@doseGrid)
        } else {
          1:max(data@xLevel)
        }

      ## get the plot data for the biomarker plot
      functionSamples <- biomarker(xLevel = xLevels, model = y, samples = x)

      ## extract mean curve
      meanCurve <- colMeans(functionSamples)

      ## extract quantiles
      quantiles <- c(0.025, 0.975)
      quantCurve <- apply(functionSamples, 2L, quantile,
        prob = quantiles
      )

      ## now create the data frame
      plotData <- data.frame(
        dose = data@doseGrid[xLevels],
        mean = meanCurve,
        lower = quantCurve[1, ],
        upper = quantCurve[2, ]
      )

      ## make the second plot
      gdata <-
        with(
          plotData,
          data.frame(
            x = rep(dose, 3),
            y = c(mean, lower, upper),
            group =
              rep(c("mean", "lower", "upper"),
                each = nrow(plotData)
              ),
            Type =
              factor(
                c(
                  rep(
                    "Estimate",
                    nrow(plotData)
                  ),
                  rep(
                    "95% Credible Interval",
                    nrow(plotData) * 2
                  )
                ),
                levels =
                  c(
                    "Estimate",
                    "95% Credible Interval"
                  )
              )
          )
        )
      plot2 <- gdata %>% ggplot() +
        geom_line(
          aes(
            x = x,
            y = y,
            group = group,
            linetype = Type
          ),
          colour = I("blue")
        ) +
        labs(
          x = "Dose level",
          y = "Biomarker level"
        )

      plot2 <- plot2 +
        scale_linetype_manual(
          breaks =
            c(
              "Estimate",
              "95% Credible Interval"
            ),
          values = c(1, 2),
          guide = ifelse(showLegend, "legend", "none")
        )

      ## arrange both plots side by side
      ret <- gridExtra::arrangeGrob(plot1, plot2, ncol = 2)
      return(ret)
    }
)


## -------------------------------------------------------------------------------------
## Get fitted dose-tox curve from Samples for 'LogisticIndepBeta' model class
## ------------------------------------------------------------------------------------
#' @describeIn fit This method return a data frame with dose, middle lower and upper quantiles
#' for the dose-DLE curve using DLE samples for \dQuote{LogisticIndepBeta} model class
#' @example examples/Samples-method-fitDLE.R
setMethod("fit",
  signature =
    signature(
      object = "Samples",
      model = "LogisticIndepBeta",
      data = "Data"
    ),
  def =
    function(object,
             model,
             data,
             points = data@doseGrid,
             quantiles = c(0.025, 0.975),
             middle = mean,
             ...) {
      ## some checks
      assert_probability_range(quantiles)
      assert_numeric(points)

      ## first we have to get samples from the dose-tox
      ## curve at the dose grid points.
      probSamples <- matrix(
        nrow = size(object),
        ncol = length(points)
      )

      ## evaluate the probs, for all samples.
      for (i in seq_along(points)) {
        ## Now we want to evaluate for the
        ## following dose:
        probSamples[, i] <- prob(
          dose = points[i],
          model,
          object
        )
      }

      ## extract middle curve
      middleCurve <- apply(probSamples, 2L, FUN = middle)

      ## extract quantiles
      quantCurve <- apply(probSamples, 2L, quantile,
        prob = quantiles
      )

      ## now create the data frame
      ret <- data.frame(
        dose = points,
        middle = middleCurve,
        lower = quantCurve[1, ],
        upper = quantCurve[2, ]
      )

      ## return it
      return(ret)
    }
)

## -------------------------------------------------------------------------------------
## Get fitted dose-efficacy curve from Samples for 'Effloglog' model class
## ------------------------------------------------------------------------------------

#' @describeIn fit This method returns a data frame with dose, middle, lower, upper quantiles for
#' the dose-efficacy curve using efficacy samples for \dQuote{Effloglog} model class
#' @example examples/Samples-method-fitEff.R
setMethod("fit",
  signature =
    signature(
      object = "Samples",
      model = "Effloglog",
      data = "DataDual"
    ),
  def =
    function(object,
             model,
             data,
             points = data@doseGrid,
             quantiles = c(0.025, 0.975),
             middle = mean,
             ...) {
      ## some checks
      assert_probability_range(quantiles)
      assert_numeric(points)

      ## first we have to get samples from the dose-tox
      ## curve at the dose grid points.
      ExpEffSamples <- matrix(
        nrow = size(object),
        ncol = length(points)
      )

      ## evaluate the probs, for all samples.
      for (i in seq_along(points)) {
        ## Now we want to evaluate for the
        ## following dose:
        ExpEffSamples[, i] <- efficacy(
          dose = points[i],
          model,
          object
        )
      }

      ## extract middle curve
      middleCurve <- apply(ExpEffSamples, 2L, FUN = middle)

      ## extract quantiles
      quantCurve <- apply(ExpEffSamples, 2L, quantile,
        prob = quantiles
      )

      ## now create the data frame
      ret <- data.frame(
        dose = points,
        middle = middleCurve,
        lower = quantCurve[1, ],
        upper = quantCurve[2, ]
      )

      ## return it
      return(ret)
    }
)
## ==========================================================================================
## --------------------------------------------------------------------
## Get fitted dose-efficacy based on the Efficacy Flexible model
## -------------------------------------------------------------
#' @describeIn fit This method returns a data frame with dose, middle, lower and upper
#' quantiles for the dose-efficacy curve using efficacy samples for \dQuote{EffFlexi}
#' model class
#' @example examples/Samples-method-fitEffFlexi.R
setMethod("fit",
  signature =
    signature(
      object = "Samples",
      model = "EffFlexi",
      data = "DataDual"
    ),
  def =
    function(object,
             model,
             data,
             points = data@doseGrid,
             quantiles = c(0.025, 0.975),
             middle = mean,
             ...) {
      ## some checks
      assert_probability_range(quantiles)
      assert_numeric(points)

      ## first we have to get samples from the dose-tox
      ## curve at the dose grid points.
      ExpEffSamples <- matrix(
        nrow = size(object),
        ncol = length(points)
      )

      ## evaluate the probs, for all samples.
      for (i in seq_along(points)) {
        ## Now we want to evaluate for the
        ## following dose:
        ExpEffSamples[, i] <- efficacy(
          dose = points[i],
          model,
          object
        )
      }

      ## extract middle curve
      middleCurve <- apply(ExpEffSamples, 2L, FUN = middle)

      ## extract quantiles
      quantCurve <- apply(ExpEffSamples, 2L, quantile,
        prob = quantiles
      )

      ## now create the data frame
      ret <- data.frame(
        dose = points,
        middle = middleCurve,
        lower = quantCurve[1, ],
        upper = quantCurve[2, ]
      )

      ## return it
      return(ret)
    }
)

#' @describeIn fit This method returns a data frame with dose, middle, lower
#' and upper quantiles for the dose-efficacy curve using efficacy samples
#' for the \dQuote{LogisticLogNormalOrdinal} model class
#' @example examples/Sample-methods-fit-LogisticLogNormalOrdinal.R
setMethod(
  "fit",
  signature = signature(
    object = "Samples",
    model = "LogisticLogNormalOrdinal",
    data = "DataOrdinal"
  ),
  def = function(object,
                 model,
                 data,
                 points = data@doseGrid,
                 quantiles = c(0.025, 0.975),
                 middle = mean,
                 ...) {
    # Validation
    assert_probability_range(quantiles)
    assert_numeric(points)
    assert_function(middle)

    # Begin
    # Get samples from the dose-tox curve at the dose grid points.
    probSamples <- matrix(
      nrow = size(object),
      ncol = length(points)
    )
    # Evaluate the probs, for all samples.
    for (i in seq_along(points)) {
      # Now we want to evaluate for the following dose:
      probSamples[, i] <- prob(
        dose = points[i],
        model,
        object,
        ...
      )
    }
    # Extract middle curve
    middleCurve <- apply(probSamples, 2L, FUN = middle)
    # Extract quantiles
    quantCurve <- apply(probSamples, 2L, quantile, prob = quantiles)

    # Create the data frame...
    ret <- data.frame(
      dose = points,
      middle = middleCurve,
      lower = quantCurve[1, ],
      upper = quantCurve[2, ]
    )

    # ...and return it
    return(ret)
  }
)
## ==============================================================
## ----------------------------------------------------------------
## Get fitted values at all dose levels from gain samples
## -----------------------------------------------------------------
#' Get the fitted values for the gain values at all dose levels based on
#' a given pseudo DLE model, DLE sample, a pseudo efficacy model, a Efficacy sample
#' and data. This method returns a data frame with dose, middle, lower and upper quantiles
#' of the gain value samples
#'
#' @param DLEmodel the DLE pseudo model of \code{\linkS4class{ModelTox}} class object
#' @param DLEsamples the DLE samples of \code{\linkS4class{Samples}} class object
#' @param Effmodel the efficacy pseudo model of \code{\linkS4class{ModelEff}} class object
#' @param Effsamples the efficacy samples of \code{\linkS4class{Samples}} class object
#' @param data the data input of \code{\linkS4class{DataDual}} class object
#' @param \dots additional arguments for methods
#'
#' @export
#' @keywords methods
#' @example examples/Samples-method-fitGain.R
setGeneric("fitGain",
  def =
    function(DLEmodel,
             DLEsamples,
             Effmodel,
             Effsamples,
             data,
             ...) {
      ## there should be no default method,
      ## therefore just forward to next method!
      standardGeneric("fitGain")
    },
  valueClass = "data.frame"
)

#' @describeIn fitGain This method returns a data frame with dose, middle, lower, upper quantiles for
#' the gain values obtained given the DLE and the efficacy samples
#' @param points at which dose levels is the fit requested? default is the dose
#' grid
#' @param quantiles the quantiles to be calculated (default: 0.025 and
#' 0.975)
#' @param middle the function for computing the middle point. Default:
#' \code{\link{mean}}
#' @example examples/Samples-method-fitGain.R
setMethod("fitGain",
  signature =
    signature(
      DLEmodel = "ModelTox",
      DLEsamples = "Samples",
      Effmodel = "ModelEff",
      Effsamples = "Samples",
      data = "DataDual"
    ),
  def =
    function(DLEmodel,
             DLEsamples,
             Effmodel,
             Effsamples,
             data,
             points = data@doseGrid,
             quantiles = c(0.025, 0.975),
             middle = mean,
             ...) {
      ## some checks
      assert_probability_range(quantiles)
      assert_numeric(points)

      ## first we have to get samples from the gain
      ## at the dose grid points.
      GainSamples <- matrix(
        nrow = size(DLEsamples),
        ncol = length(points)
      )

      ## evaluate the probs, for all gain samples.
      for (i in seq_along(points)) {
        ## Now we want to evaluate for the
        ## following dose:
        GainSamples[, i] <- gain(
          dose = points[i],
          DLEmodel,
          DLEsamples,
          Effmodel,
          Effsamples
        )
      }

      ## extract middle curve
      middleCurve <- apply(GainSamples, 2L, FUN = middle)

      ## extract quantiles
      quantCurve <- apply(GainSamples, 2L, quantile,
        prob = quantiles
      )

      ## now create the data frame
      ret <- data.frame(
        dose = points,
        middle = middleCurve,
        lower = quantCurve[1, ],
        upper = quantCurve[2, ]
      )

      ## return it
      return(ret)
    }
)
## ---------------------------------------------------------------------------------
## Plot the fitted dose-DLE curve with pseudo DLE model with samples
## -------------------------------------------------------------------------------
#' Plot the fitted dose-DLE curve using a \code{\linkS4class{ModelTox}} class model with samples
#'
#' @param x the \code{\linkS4class{Samples}} object
#' @param y the \code{\linkS4class{ModelTox}} model class object
#' @param data the \code{\linkS4class{Data}} object
#' @param xlab the x axis label
#' @param ylab the y axis label
#' @param showLegend should the legend be shown? (default)
#' @param \dots not used
#' @return This returns the \code{\link[ggplot2]{ggplot}}
#' object for the dose-DLE model fit
#'
#' @example examples/Samples-method-plotModelTox.R
#' @export
#' @keywords methods
setMethod("plot",
  signature =
    signature(
      x = "Samples",
      y = "ModelTox"
    ),
  def =
    function(x, y, data, ...,
             xlab = "Dose level",
             ylab = "Probability of DLT [%]",
             showLegend = TRUE) {
      ## check args
      assert_logical(showLegend)


      ## get the fit
      plotData <- fit(x,
        model = y,
        data = data,
        quantiles = c(0.025, 0.975),
        middle = mean
      )

      ## make the plot
      gdata <-
        with(
          plotData,
          data.frame(
            x = rep(dose, 3),
            y = c(middle, lower, upper) * 100,
            group =
              rep(c("mean", "lower", "upper"),
                each = nrow(plotData)
              ),
            Type =
              factor(
                c(
                  rep(
                    "Estimate",
                    nrow(plotData)
                  ),
                  rep(
                    "95% Credible Interval",
                    nrow(plotData) * 2
                  )
                ),
                levels =
                  c(
                    "Estimate",
                    "95% Credible Interval"
                  )
              )
          )
        )

      ret <- gdata %>% ggplot() +
        geom_line(
          aes(
            x = x,
            y = y,
            group = group,
            linetype = Type
          ),
          colour = I("red"),
        ) +
        coord_cartesian(ylim = c(0, 100)) +
        labs(
          x = xlab,
          y = ylab
        )

      ret <- ret +
        scale_linetype_manual(
          breaks =
            c(
              "Estimate",
              "95% Credible Interval"
            ),
          values = c(1, 2), guide = ifelse(showLegend, "legend", "none")
        )

      return(ret)
    }
)


# --------------------------------------------------------------------------------------------
## Plot the fitted dose-efficacy curve using a pseudo efficacy model with samples
## -------------------------------------------------------------------------------------------
#' Plot the fitted dose-efficacy curve using a model from \code{\linkS4class{ModelEff}} class
#' with samples
#'
#' @param x the \code{\linkS4class{Samples}} object
#' @param y the \code{\linkS4class{ModelEff}} model class object
#' @param data the \code{\linkS4class{Data}} object
#' @param xlab the x axis label
#' @param ylab the y axis label
#' @param showLegend should the legend be shown? (default)
#' @param \dots not used
#' @return This returns the \code{\link[ggplot2]{ggplot}}
#' object for the dose-efficacy model fit
#'
#' @example examples/Samples-method-plotModelEff.R
#' @export
#' @keywords methods
setMethod("plot",
  signature =
    signature(
      x = "Samples",
      y = "ModelEff"
    ),
  def =
    function(x, y, data, ...,
             xlab = "Dose level",
             ylab = "Expected Efficacy",
             showLegend = TRUE) {
      ## check args
      assert_logical(showLegend)

      ## get the fit
      plotData <- fit(x,
        model = y,
        data = data,
        quantiles = c(0.025, 0.975),
        middle = mean
      )

      ## make the plot
      gdata <-
        with(
          plotData,
          data.frame(
            x = rep(dose, 3),
            y = c(middle, lower, upper),
            group =
              rep(c("mean", "lower", "upper"),
                each = nrow(plotData)
              ),
            Type =
              factor(
                c(
                  rep(
                    "Estimate",
                    nrow(plotData)
                  ),
                  rep(
                    "95% Credible Interval",
                    nrow(plotData) * 2
                  )
                ),
                levels =
                  c(
                    "Estimate",
                    "95% Credible Interval"
                  )
              )
          )
        )

      ret <- gdata %>% ggplot() +
        geom_line(
          aes(
            x = x,
            y = y,
            group = group,
            linetype = Type
          ),
          colour = I("blue")
        ) +
        labs(
          x = xlab,
          y = ylab
        ) +
        coord_cartesian(xlim = c(0, max(data@doseGrid)))

      ret <- ret +
        scale_linetype_manual(
          breaks =
            c(
              "Estimate",
              "95% Credible Interval"
            ),
          values = c(1, 2), guide = ifelse(showLegend, "legend", "none")
        )

      return(ret)
    }
)

## ----------------------------------------------------------------------------------------
## Plot of fitted dose-DLE curve based on a pseudo DLE model without sample
## -------------------------------------------------------------------------------------
#' Plot of the fitted dose-tox based with a given pseudo DLE model and data without samples
#'
#' @param x the data of \code{\linkS4class{Data}} class object
#' @param y the model of the \code{\linkS4class{ModelTox}} class object
#' @param xlab the x axis label
#' @param ylab the y axis label
#' @param showLegend should the legend be shown? (default)
#' @param \dots not used
#' @return This returns the \code{\link[ggplot2]{ggplot}}
#' object for the dose-DLE model plot
#'
#' @example examples/Samples-method-plotModelToxNoSamples.R
#' @export
#' @keywords methods
setMethod("plot",
  signature =
    signature(
      x = "Data",
      y = "ModelTox"
    ),
  def =
    function(x, y,
             xlab = "Dose level",
             ylab = "Probability of DLE",
             showLegend = TRUE, ...) {
      ## check args
      assert_logical(showLegend)

      ## Make sure the right model estimates are use with the given data
      y <- update(object = y, data = x)


      ## create data frame

      plotData <- data.frame(
        dose = x@doseGrid,
        probDLE = prob(
          dose = x@doseGrid,
          model = y
        )
      )
      ## Look for TD30 and TD35
      TD30 <- dose(
        x = 0.30,
        model = y
      )
      TD35 <- dose(
        x = 0.35,
        model = y
      )

      ## make the plot
      gdata <- with(
        plotData,
        data.frame(
          x = dose,
          y = probDLE,
          group = rep("Estimated DLE", each = nrow(plotData)),
          Type = factor(rep("Estimated DLE", nrow(plotData)), levels = "Estimated DLE")
        )
      )

      plot1 <- gdata %>% ggplot() +
        geom_line(
          aes(
            x = x,
            y = y,
            group = group,
            linetype = Type
          ),
          colour = I("red"),
          linewidth = 1.5
        ) +
        labs(
          x = xlab,
          y = ylab
        ) +
        coord_cartesian(ylim = c(0, 1)) +
        scale_linetype_manual(
          breaks = "Estimated DLE",
          values = c(1, 2),
          guide = ifelse(showLegend, "legend", "none")
        )
      return(plot1)
    }
)


## ---------------------------------------------------------------------------------------------
## Plot the fitted dose-efficacy curve given a pseudo efficacy model without samples
## ----------------------------------------------------------------------------------
#' Plot of the fitted dose-efficacy based with a given pseudo efficacy model and data without samples
#'
#' @param x the data of \code{\linkS4class{DataDual}} class object
#' @param y the model of the \code{\linkS4class{ModelEff}} class object
#' @param xlab the x axis label
#' @param ylab the y axis label
#' @param showLegend should the legend be shown? (default)
#' @param \dots not used
#' @return This returns the \code{\link[ggplot2]{ggplot}}
#' object for the dose-efficacy model plot
#'
#' @example examples/Samples-method-plotModelEffNoSamples.R
#' @export
#' @keywords methods
setMethod("plot",
  signature =
    signature(
      x = "DataDual",
      y = "ModelEff"
    ),
  def =
    function(x, y, ...,
             xlab = "Dose level",
             ylab = "Expected Efficacy",
             showLegend = TRUE) {
      ## check args
      assert_logical(showLegend)
      y <- update(object = y, data = x)

      ## create data frame

      plotEffData <- data.frame(
        dose = x@doseGrid,
        ExpEff = efficacy(
          dose = x@doseGrid,
          model = y
        )
      )

      ## make the second plot
      ggdata <- with(
        plotEffData,
        data.frame(
          x = dose,
          y = ExpEff,
          group = rep("Estimated Expected Efficacy", each = nrow(plotEffData)),
          Type = factor(rep("Estimated Expected Efficacy", nrow(plotEffData)), levels = "Estimated Expected Efficacy")
        )
      )

      ## Get efficacy plot
      plot2 <- ggplot(data = ggdata, aes(x = x, y = y), group = group) +
        xlab("Dose Levels") +
        ylab(paste("Estimated Expected Efficacy")) +
        xlim(c(0, max(x@doseGrid))) +
        geom_line(colour = I("blue"), linewidth = 1.5)

      plot2 <- plot2 +
        geom_line(linewidth = 1.5, colour = "blue")


      return(plot2)
    }
)

## ----------------------------------------------------------------------------------------------------------
## Plot the gain curve using a pseudo DLE and a pseudo Efficacy model with samples
## ----------------------------------------------------------------------------------------------------
#' Plot the gain curve in addition with the dose-DLE and dose-efficacy curve using a given DLE pseudo model,
#' a DLE sample, a given efficacy pseudo model and an efficacy sample
#'
#' @param DLEmodel the dose-DLE model of \code{\linkS4class{ModelTox}} class object
#' @param DLEsamples the DLE sample of \code{\linkS4class{Samples}} class object
#' @param Effmodel the dose-efficacy model of \code{\linkS4class{ModelEff}} class object
#' @param Effsamples the efficacy sample of of \code{\linkS4class{Samples}} class object
#' @param data the data input of \code{\linkS4class{DataDual}} class object
#' @param \dots not used
#' @return This returns the \code{\link[ggplot2]{ggplot}}
#' object for the plot
#'
#' @example examples/Samples-method-plotGain.R
#' @export
#' @keywords methods
setGeneric("plotGain",
  def =
    function(DLEmodel,
             DLEsamples,
             Effmodel,
             Effsamples,
             data, ...) {
      standardGeneric("plotGain")
    }
)
#' @describeIn plotGain Standard method
setMethod("plotGain",
  signature =
    signature(
      DLEmodel = "ModelTox",
      DLEsamples = "Samples",
      Effmodel = "ModelEff",
      Effsamples = "Samples"
    ),
  def =
    function(DLEmodel, DLEsamples, Effmodel, Effsamples, data, ...) {
      ## Get fitted values for probabilities of DLE at all dose levels

      plotDLEData <- fit(DLEsamples,
        model = DLEmodel,
        data = data,
        quantiles = c(0.025, 0.975),
        middle = mean
      )

      ## Get fitted values for mean efficacy values at all dose levels
      plotEffData <- fit(Effsamples,
        model = Effmodel,
        data = data,
        quantiles = c(0.025, 0.975),
        middle = mean
      )

      ## Get fitted values for gain values at all dose levels
      plotGainData <- fitGain(
        DLEmodel = DLEmodel,
        DLEsamples = DLEsamples,
        Effmodel = Effmodel,
        Effsamples = Effsamples,
        data = data
      )

      ## For each of the dose levels, take the mean for the probabilties of DLE, mean efiicacy values
      ## and gain values. Hence combine them into a data frame

      plotData <- data.frame(
        dose = rep(data@doseGrid, 3),
        values = c(
          plotDLEData$middle,
          plotEffData$middle,
          plotGainData$middle
        )
      )
      ## only the line plots for the mean value of the DLE, efficacy and gain samples
      ## at all dose levels
      gdata <- with(
        plotData,
        data.frame(
          x = dose,
          y = values,
          group = c(
            rep("p(DLE)", length(data@doseGrid)),
            rep("Mean Expected Efficacy", length(data@doseGrid)),
            rep("Gain", length(data@doseGrid))
          ),
          Type = factor("Estimate", levels = "Estimate")
        )
      )

      plot1 <- ggplot(data = gdata, aes(x = x, y = y)) +
        geom_line(aes(group = group, color = group), linewidth = 1.5) +
        scale_colour_manual(name = "curves", values = c("green3", "blue", "red")) +
        xlab("Dose Level") +
        xlim(c(0, max(data@doseGrid))) +
        ylab(paste("Values")) +
        ylim(c(min(gdata$y), max(gdata$y)))
      return(plot1)
    }
)

## ----------------------------------------------------------------------------------------------------
## Plot the gain curve using a pseudo DLE and a pseudo Efficacy model without samples
## ----------------------------------------------------------------------------------------------------
#' Plot the gain curve in addition with the dose-DLE and dose-efficacy curve using a given DLE pseudo model,
#' and a given efficacy pseudo model
#'
#' @describeIn plotGain Standard method
#' @param size (`integer`)\cr a vector of length two defining the sizes of
#' the shapes used to identify the doses with, respectively, p(DLE = 0.3) and the
#' maximum gain
#' @param shape (`integer`)\cr a vector of length two defining the shapes
#' used to identify the doses with, respectively, p(DLE = 0.3) and the maximum gain
#'
#' @example examples/Samples-method-plotGainNoSamples.R
#' @export
#' @keywords methods
setMethod("plotGain",
  signature =
    signature(
      DLEmodel = "ModelTox",
      DLEsamples = "missing",
      Effmodel = "ModelEff",
      Effsamples = "missing"
    ),
  def =
    function(DLEmodel, Effmodel, data, size = c(8L, 8L), shape = c(16L, 17L), ...) {
      assert_integer(size, len = 2, any.missing = FALSE, lower = 0, upper = 20)
      assert_integer(shape, len = 2, any.missing = FALSE, unique = TRUE, lower = 0, upper = 25)
      ## Make sure the model estimates are corresponds to the input data
      DLEmodel <- update(object = DLEmodel, data = data)
      Effmodel <- update(object = Effmodel, data = data)

      plotData <- data.frame(
        dose = rep(data@doseGrid, 3),
        values = c(
          prob(
            dose = data@doseGrid,
            model = DLEmodel
          ),
          efficacy(
            dose = data@doseGrid,
            model = Effmodel
          ),
          gain(
            dose = data@doseGrid,
            model_dle = DLEmodel,
            model_eff = Effmodel
          )
        )
      )
      gdata <- with(
        plotData,
        data.frame(
          x = dose,
          y = values,
          group = c(
            rep("p(DLE)", length(data@doseGrid)),
            rep("Expected Efficacy", length(data@doseGrid)),
            rep("Gain", length(data@doseGrid))
          ),
          colour = rep(c("blue", "green3", "red")),
          Type = factor("Estimate", levels = "Estimate")
        )
      )

      # if changing the line type is unacceptable, consider
      # https://stackoverflow.com/questions/25632242/filled-and-hollow-shapes-where-the-fill-color-the-line-color
      plot1 <- ggplot(data = gdata, aes(x = x, y = y)) +
        geom_line(aes(group = group, linetype = group, colour = group), linewidth = 1) +
        scale_colour_manual(
          name = "Curves",
          values = c("blue", "green3", "red")
        ) +
        scale_linetype_manual(
          name = "Curves",
          values = c("solid", "dotted", "dashed")
        ) +
        xlab("Dose Level") +
        ylab(paste("Values"))

      TD30 <- dose(x = 0.3, model = DLEmodel)

      Gainfun <- function(DOSE) {
        -gain(DOSE, model_dle = DLEmodel, model_eff = Effmodel)
      }
      Gstar <- (
        optim(
          min(data@doseGrid),
          Gainfun,
          method = "L-BFGS-B",
          lower = min(data@doseGrid),
          upper = max(data@doseGrid)
        )$par
      )
      MaxGain <- -(
        optim(
          min(data@doseGrid),
          Gainfun,
          method = "L-BFGS-B",
          lower = min(data@doseGrid),
          upper = max(data@doseGrid)
        )$value
      )

      if ((TD30 < min(data@doseGrid)) | (TD30 > max(data@doseGrid))) {
        plot1 <- plot1
        message(paste("TD30", paste(TD30, " not within dose Grid")))
      } else {
        plot1 <- plot1 +
          geom_point(
            data = data.frame(x = TD30, y = 0.3),
            aes(x = x, y = y),
            colour = "violet",
            shape = 16,
            size = 8
          ) +
          annotate(
            "text",
            label = "p(DLE=0.3)",
            x = TD30 + 1,
            y = 0.2,
            size = 5,
            colour = "violet"
          )
      }

      # Add annotated point estimates to graph
      point_data <- tibble::tibble(
        Text = NA_character_,
        X = NA_real_,
        Y = NA_real_,
        Shape = NA_real_,
        Size = NA_real_,
        Colour = NA_character_,
        .rows = 0
      )

      if ((TD30 < min(data@doseGrid)) | (TD30 > max(data@doseGrid))) {
        message(paste("TD30", paste(TD30, " not within dose Grid")))
      } else {
        point_data <- point_data %>%
          tibble::add_row(
            X = TD30,
            Y = 0.3,
            Shape = shape[1],
            Size = size[1],
            Colour = "violet",
            Text = "p(DLE=0.3)"
          )
      }
      if ((Gstar < min(data@doseGrid)) | (Gstar > max(data@doseGrid))) {
        print(paste("Gstar=", paste(Gstar, " not within dose Grid")))
      } else {
        plot1 <- plot1 +
          geom_point(
            data = data.frame(x = Gstar, y = MaxGain),
            aes(x = x, y = y),
            colour = "green3",
            shape = 17,
            size = 8
          ) +
          annotate(
            "text",
            label = "Max Gain",
            x = Gstar,
            y = MaxGain - 0.1,
            size = 5,
            colour = "green3"
          )
      }
      point_data <- point_data %>%
        tibble::add_row(
          X = Gstar,
          Y = MaxGain,
          Shape = shape[2],
          Size = size[2],
          Colour = "green3",
          Text = "Max Gain"
        )


      plot1 <- plot1 +
        geom_point(
          data = point_data,
          inherit.aes = FALSE,
          aes(
            x = .data$X,
            y = .data$Y,
            shape = as.factor(.data$Shape),
            fill = .data$Colour
          ),
          colour = point_data$Colour,
          size = point_data$Size,
        ) +
        scale_fill_manual(
          name = "Estimates",
          labels = c("p(DLE = 0.3)", "Max Gain"),
          values = point_data$Colour
        ) +
        scale_shape_discrete(
          name = "Estimates",
          labels = c("p(DLE = 0.3)", "Max Gain"),
          breaks = point_data$Shape
        ) +
        guides(
          shape = guide_legend(override.aes = list(color = c("violet", "green3")))
        ) +
        coord_cartesian(
          xlim = c(0, max(data@doseGrid)),
          ylim = c(min(gdata$y), max(gdata$y))
        )
      return(plot1)
    }
)
## ==========================================================================================

## -------------------------------------------------------------------------------
## Plot of the DLE and efficacy curve sides by side with samples
## -----------------------------------------------------------------------------
#' Plot of the DLE and efficacy curve side by side given a DLE pseudo model,
#' a DLE sample, an efficacy pseudo model and a given efficacy sample
#'
#' @param DLEmodel the pseudo DLE model of \code{\linkS4class{ModelTox}} class object
#' @param DLEsamples the DLE samples of \code{\linkS4class{Samples}} class object
#' @param Effmodel the pseudo efficacy model of \code{\linkS4class{ModelEff}} class object
#' @param Effsamples the Efficacy samples of \code{\linkS4class{Samples}} class object
#' @param data the data input of \code{\linkS4class{DataDual}} class object
#' @param extrapolate should the biomarker fit be extrapolated to the whole
#' dose grid? (default)
#' @param showLegend should the legend be shown? (not default)
#' @param \dots additional arguments for the parent method
#' \code{\link{plot,Samples,GeneralModel-method}}
#' @return This returns the \code{\link[ggplot2]{ggplot}}
#' object with the dose-toxicity and dose-efficacy model fits
#'
#' @example examples/Samples-method-plotDualResponses.R
#'
#' @export
#' @keywords methods
setGeneric("plotDualResponses",
  def =
    function(DLEmodel,
             DLEsamples,
             Effmodel,
             Effsamples,
             data, ...) {
      standardGeneric("plotDualResponses")
    }
)

#' @describeIn plotDualResponses function still to be documented
setMethod("plotDualResponses",
  signature =
    signature(
      DLEmodel = "ModelTox",
      DLEsamples = "Samples",
      Effmodel = "ModelEff",
      Effsamples = "Samples"
    ),
  def =
    function(DLEmodel, DLEsamples, Effmodel, Effsamples, data, extrapolate = TRUE, showLegend = FALSE, ...) {
      assert_logical(extrapolate)
      assert_logical(showLegend)
      ## Get Toxicity plot
      ## get the fit

      plotDLEData <- fit(DLEsamples,
        model = DLEmodel,
        data = data,
        quantiles = c(0.025, 0.975),
        middle = mean
      )

      ## make the plot
      gdata <-
        with(
          plotDLEData,
          data.frame(
            x = rep(dose, 3),
            y = c(middle, lower, upper) * 100,
            group =
              rep(c("mean", "lower", "upper"),
                each = nrow(plotDLEData)
              ),
            Type =
              factor(
                c(
                  rep(
                    "Estimate",
                    nrow(plotDLEData)
                  ),
                  rep(
                    "95% Credible Interval",
                    nrow(plotDLEData) * 2
                  )
                ),
                levels =
                  c(
                    "Estimate",
                    "95% Credible Interval"
                  )
              )
          )
        )

      ret1 <- gdata %>% ggplot() +
        geom_line(
          aes(
            x = x,
            y = y,
            group = group,
            linetype = Type
          ),
          colour = I("red"),
        ) +
        labs(
          x = "Dose Levels",
          y = "Probability of DLE [%]"
        ) +
        coord_cartesian(ylim = c(0, 100)) +
        scale_linetype_manual(
          breaks = c(
            "Estimate",
            "95% Credible Interval"
          ),
          values = c(1, 2),
          guide = ifelse(showLegend, "legend", "none")
        )
      ## only look at these dose levels for the plot:

      xLevels <- if (extrapolate) {
        seq_along(data@doseGrid)
      } else {
        1:max(data@xLevel)
      }

      ## get the plot data for the efficacy
      functionSamples <- matrix(
        nrow = size(Effsamples),
        ncol = length(xLevels)
      )
      ## evaluate the efficacy for all samples
      for (i in seq_along(xLevels)) {
        ## Now we want to evaluate for the following dose
        functionSamples[, i] <- efficacy(
          dose = data@doseGrid[xLevels[i]],
          model = Effmodel,
          samples = Effsamples
        )
      }
      ## extract mean curve
      meanCurve <- colMeans(functionSamples)

      ## extract quantiles
      quantiles <- c(0.025, 0.975)
      quantCurve <- apply(functionSamples, 2L, quantile, prob = quantiles)

      ## now create the data frame
      plotEffData <- data.frame(
        dose = data@doseGrid[xLevels],
        mean = meanCurve,
        lower = quantCurve[1, ],
        upper = quantCurve[2, ]
      )
      ## make the second plot
      ggdata <- with(plotEffData, data.frame(
        x = rep(dose, 3),
        y = c(mean, lower, upper),
        group =
          rep(c("mean", "lower", "upper"),
            each = nrow(plotEffData)
          ),
        Type =
          factor(
            c(
              rep(
                "Estimate",
                nrow(plotEffData)
              ),
              rep(
                "95% Credible Interval",
                nrow(plotEffData) * 2
              )
            ),
            levels =
              c(
                "Estimate",
                "95% Credible Interval"
              )
          )
      ))

      plot2 <- ggdata %>% ggplot() +
        geom_line(
          aes(
            x = x,
            y = y,
            group = group,
            linetype = Type
          ),
          colour = I("blue"),
        ) +
        labs(
          x = "Dose level",
          y = "Expected Efficacy"
        ) +
        scale_linetype_manual(
          breaks =
            c(
              "Estimate",
              "95% Credible Interval"
            ),
          values = c(1, 2),
          guide = ifelse(showLegend, "legend", "none")
        )

      ## arrange both plots side by side
      ret <- gridExtra::arrangeGrob(ret1, plot2, ncol = 2)
      return(ret)
    }
)

## ------------------------------------------------------------------------------
## Plot of the DLE and efficacy curve sides by side without  samples
## -----------------------------------------------------------------------------
#' Plot of the dose-DLE and dose-efficacy curve side by side given a DLE pseudo model
#' and a given pseudo efficacy model without DLE and efficacy samples
#'
#' @describeIn plotDualResponses Plot the DLE and efficacy curve side by side given a DLE model
#' and an efficacy model without any samples
#'
#' @example examples/Samples-method-plotDualResponsesNoSamples.R
#'
#' @export
#' @keywords methods
setMethod("plotDualResponses",
  signature =
    signature(
      DLEmodel = "ModelTox",
      DLEsamples = "missing",
      Effmodel = "ModelEff",
      Effsamples = "missing"
    ),
  def =
    function(DLEmodel, Effmodel, data, ...) {
      ## Get Toxicity plot
      ## get the fit


      ## Make sure the model estimates are corresponds to the input data
      DLEmodel <- update(object = DLEmodel, data = data)
      Effmodel <- update(object = Effmodel, data = data)


      plotDLEData <- data.frame(
        dose = data@doseGrid,
        probDLE = prob(
          dose = data@doseGrid,
          model = DLEmodel
        )
      )
      ## make the plot
      gdata <- with(
        plotDLEData,
        data.frame(
          x = dose,
          y = probDLE,
          group = rep("Estimated DLE", each = nrow(plotDLEData)),
          Type = factor(rep("Estimated DLE", nrow(plotDLEData)), levels = "Estimated DLE")
        )
      )

      plot1 <- ggplot(data = gdata, aes(x = x, y = y), group = group) +
        xlab("Dose Levels") +
        ylab(paste("Probability of DLE")) +
        ylim(c(0, 1)) +
        xlim(c(0, max(data@doseGrid))) +
        geom_line(colour = I("red"), linewidth = 1.5)


      plot1 <- plot1 +
        geom_line(linewidth = 1.5, colour = "red")

      ## only look at these dose levels for the plot:

      ## get the plot data for the efficacy
      plotEffData <- data.frame(
        dose = data@doseGrid,
        ExpEff = efficacy(
          dose = data@doseGrid,
          model = Effmodel
        )
      )

      ## make the second plot
      ggdata <- with(
        plotEffData,
        data.frame(
          x = dose,
          y = ExpEff,
          group = rep("Estimated Expected Efficacy", each = nrow(plotEffData)),
          Type = factor(rep("Estimated Expected Efficacy", nrow(plotEffData)), levels = "Estimated Expected Efficacy")
        )
      )

      ## Get efficacy plot
      plot2 <- ggplot(data = ggdata, aes(x = x, y = y), group = group) +
        xlab("Dose Levels") +
        ylab(paste("Estimatimated Expected Efficacy")) +
        xlim(c(0, max(data@doseGrid))) +
        geom_line(colour = I("blue"), linewidth = 1.5)

      plot2 <- plot2 +
        geom_line(linewidth = 1.5, colour = "blue")

      ## arrange both plots side by side
      ret <- gridExtra::arrangeGrob(plot1, plot2, ncol = 2)
      return(ret)
    }
)
## =======================================================================================================

## ----------------------------------------------------------------
## Get fitted DLT free survival (piecewise exponential model) based on
## the DA-CRM model
## -----------------------------------------------------------------
#' Get the fitted DLT free survival (piecewise exponential model).
#' This function returns a data frame with dose, middle, lower and upper
#' quantiles for the `PEM` curve. If hazard=TRUE,
#' @param object mcmc samples
#' @param model the mDA-CRM model
#' @param data the data input, a \code{\linkS4class{DataDA}} class object
#' @param quantiles the quantiles to be calculated (default: 0.025 and
#' 0.975)
#' @param middle the function for computing the middle point. Default:
#' \code{\link{mean}}
#' @param hazard should the the hazard over time be plotted based on the `PEM`? (not default)
#'   Otherwise ...
#' @param \dots additional arguments for methods
#'
#' @export
#' @keywords methods
setGeneric("fitPEM",
  def =
    function(object,
             model,
             data,
             quantiles = c(0.025, 0.975),
             middle = mean,
             hazard = FALSE,
             ...) {
      ## there should be no default method,
      ## therefore just forward to next method!
      standardGeneric("fitPEM")
    },
  valueClass = "data.frame"
)


#' Likelihood of DLTs in each interval
#'
#' This is a helper function for the `fitPEM` methods below.
#'
#' @param lambda the vector of piecewise hazards
#' @param Tmax the end of the time interval for DLTs
#' @return vector with the probabilities for DLTs within the intervals.
#'
#' @keywords internal
DLTLikelihood <- function(lambda,
                          Tmax) {
  npiece <- length(lambda)
  h <- seq(from = 0L, to = Tmax, length = npiece + 1)

  # Length of each time interval;
  sT <- rep(0, npiece)


  for (i in 1:npiece) {
    sT[i] <- h[i + 1] - h[i]
  }


  # calculate the exponential part of the distribution:
  s_ij <- function(t, j) {
    if (t > h[j]) {
      min(t - h[j], h[j + 1] - h[j])
    } else {
      0
    }
  }


  # The cumulative hazard function
  expNmu <- function(t) {
    ret <- 1
    for (j in 1:npiece) {
      ret <- ret * exp(-lambda[j] * s_ij(t, j))
    }
    return(ret)
  }

  # CDF of the piecewise exponential
  piece_exp_cdf <- function(x) {
    1 - expNmu(x)
  }

  DLTFreeS <- function(x) {
    (expNmu(x) - expNmu(Tmax)) / piece_exp_cdf(Tmax)
  }

  pDLT <- rep(0, npiece + 1)

  for (i in 1:(npiece)) {
    pDLT[i] <- DLTFreeS(h[i]) - DLTFreeS(h[i + 1])
  }

  return(pDLT)
}

## --------------------------------------------------------------------
## Get fitted DLT free survival (piecewise exponential model) based on
## the DA-CRM model
## -------------------------------------------------------------
#' @describeIn fitPEM This method works for the \code{\linkS4class{DALogisticLogNormal}}
#' model class.
#' @example examples/Samples-method-fitPEM-DALogisticLogNormal.R
setMethod("fitPEM",
  signature =
    signature(
      object = "Samples",
      model = "DALogisticLogNormal",
      data = "DataDA"
    ),
  def =
    function(object,
             model,
             data,
             quantiles = c(0.025, 0.975),
             middle = mean,
             hazard = FALSE,
             ...) {
      ## some checks
      assert_probability_range(quantiles)
      assert_logical(hazard)
      ## Plot points
      points <- seq(0, data@Tmax, length = model@npiece + 1)
      ## first we have to get samples from the PEM
      ## at intercept points and 2 middel points between
      ## intercepts.
      PEMSamples <- matrix(
        nrow = size(object),
        ncol = length(points)
      )

      i_max <- max(seq_along(points))
      ## evaluate the probs, for all samples.

      # The PEM
      if (hazard == FALSE) {
        PEMSamples <- t(apply(object@data$lambda, 1, function(x) {
          fit <- DLTLikelihood(x, data@Tmax)
          return(fit)
        }))
      } else if (hazard == TRUE) {
        for (i in seq_along(points)) {
          if (i == i_max) {
            PEMSamples[, i_max] <- object@data$lambda[, model@npiece]
          } else {
            PEMSamples[, i] <- object@data$lambda[, i]
          }
        }
      }

      ## extract middle curve
      middleCurve <- apply(PEMSamples, 2L, FUN = middle)

      ## extract quantiles
      quantCurve <- apply(PEMSamples, 2L, quantile,
        prob = quantiles
      )

      ## now create the data frame
      ret <- data.frame(
        time = points,
        middle = middleCurve,
        lower = quantCurve[1, ],
        upper = quantCurve[2, ]
      )

      ## return it
      return(ret)
    }
)

## =======================================================================================================


## --------------------------------------------------
## Plot survival curve fit over time
## --------------------------------------------------

## todo: add example file
#' Plotting dose-toxicity model fits
#'
#' @param x the \code{\linkS4class{Samples}} object
#' @param y the \code{\linkS4class{DALogisticLogNormal}} object
#' @param data the \code{\linkS4class{DataDA}} object
#' @param hazard see \code{\link{fitPEM}} for the explanation
#' @param \dots not used
#' @param showLegend should the legend be shown? (default)
#' @return This returns the \code{\link[ggplot2]{ggplot}}
#' object for the dose-toxicity model fit
#'
#' @export
setMethod("plot",
  signature =
    signature(
      x = "Samples",
      y = "DALogisticLogNormal"
    ),
  def =
    function(x, y, data, hazard = FALSE, ...,
             showLegend = TRUE) {
      ## check args
      assert_logical(showLegend)
      assert_logical(hazard)

      ## call the superclass method, to get the toxicity plot
      plot1 <- callNextMethod(x, y, data, showLegend = showLegend, ...)

      ## get the fit
      fitData <- fitPEM(x,
        model = y,
        data = data,
        quantiles = c(0.025, 0.975),
        middle = mean,
        hazard = hazard
      )

      ## make the plot
      Tpoints <- seq(0, data@Tmax, length = y@npiece + 1)
      plotData <-
        with(
          fitData,
          data.frame(
            x = rep(Tpoints, 3),
            y = c(middle, lower, upper) * 100,
            group =
              rep(c("mean", "lower", "upper"),
                each = nrow(fitData)
              ),
            Type =
              factor(
                c(
                  rep(
                    "Estimate",
                    nrow(fitData)
                  ),
                  rep(
                    "95% Credible Interval",
                    nrow(fitData) * 2
                  )
                ),
                levels =
                  c(
                    "Estimate",
                    "95% Credible Interval"
                  )
              )
          )
        )
      plot2 <- plotData %>% ggplot() +
        geom_step(
          aes(
            x = x,
            y = y,
            group = group,
            linetype = Type
          ),
          colour = I("blue")
        ) +
        labs(
          x = "Time",
          y = if (hazard) "Hazard rate*100" else "Probability of DLT [%]"
        ) +
        coord_cartesian(
          ylim = if (hazard) range(plotData$y) else c(0, 100)
        )

      ret <- gridExtra::arrangeGrob(plot1, plot2, ncol = 2)
      return(ret)
    }
)


## =======================================================================================================

# tidy ----

## Samples

## tidy-Samples ----

#' @rdname tidy
#' @aliases tidy-Samples
#' @example examples/Samples-method-tidy.R
#' @export
setMethod(
  f = "tidy",
  signature = signature(x = "Samples"),
  definition = function(x, ...) {
    rv <- lapply(
      slotNames(x),
      function(nm) {
        if (nm == "data") {
          lapply(
            names(x@data),
            function(nm) {
              as_tibble(get(x, nm))
            }
          ) |>
            dplyr::bind_rows() |>
            tidyr::pivot_wider(
              names_from = Parameter,
              values_from = value
            ) |>
            dplyr::bind_cols(h_handle_attributes(get(x, names(x@data)[1])))
        } else {
          slot(x, nm) |>
            tidy() |>
            dplyr::bind_cols()
        }
      }
    )
    names(rv) <- c("data", "options")
    rv <- rv |> h_tidy_class(x)
    rv
  }
)
