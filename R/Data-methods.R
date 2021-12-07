# GeneralData-as.list ----

#' Coerce to List Method for `GeneralData` Objects
#'
#' @description `r lifecycle::badge("stable")`
#'
#' A method that coerces [`GeneralData`] object into list.
#'
#' @param x (`GeneralData`)\cr the object we want to convert into list.
#' @param \dots unused.
#'
#' @return A `list` with all slots in object `x`.
#'
#' @example examples/Data-method-asList.R
#' @export
#'
setMethod(
  f = "as.list",
  signature = signature(x = "GeneralData"),
  definition = function(x, ...) {
    slot_names <- slotNames(x)
    x_list <- lapply(X = slot_names, FUN = slot, object = x)
    names(x_list) <- slot_names
    x_list
  }
)

# Data-plot ----

#' Plot method for the [`Data`] class.
#'
#' @description `r lifecycle::badge("stable")`
#'
#' A method that creates a plot for [`Data`] object.
#'
#' @param x (`Data`)\cr object we want to plot.
#' @param y (`missing`)\cr missing object, for compatibility with the generic function.
#' @param blind (`flag`)\cr indicates whether to blind the data.
#'   If `TRUE`, then placebo subjects are reported at the same level
#'   as the active dose level in the corresponding cohort,
#'   and DLTs are always assigned to the first subjects in a cohort.
#' @param legend (`flag`)\cr whether the legend should be added.
#' @param \dots not used.
#'
#' @return The [`ggplot2`] object.
#'
#' @aliases plot-Data-method
#' @example examples/Data-method-plot-Data.R
#' @export
#'
setMethod(
  f = "plot",
  signature = signature(x = "Data", y = "missing"),
  definition = function(x, y, blind = FALSE, legend = TRUE, ...) {
    assert_flag(blind)
    assert_flag(legend)

    if (x@nObs == 0L) {
      return()
    }

    df <- h_plot_data_df(x, blind)

    p <- h_plot_data(df, placebo = x@placebo, legend = legend) +
      scale_x_continuous(breaks = df$patient, minor_breaks = NULL) +
      scale_y_continuous(
        breaks = sort(unique(c(0, df$dose))),
        minor_breaks = NULL,
        limits = c(0, max(df$dose) * 1.1)
      )
    if (!blind) {
      p <- p +
        geom_text(
          aes(label = ID, size = 2),
          data = df,
          hjust = 0,
          vjust = 0.5,
          angle = 90,
          colour = "black",
          show.legend = FALSE
        )
    }
    p
  }
)

# DataDual-plot ----

#' Plot method for the [`DataDual`] class.
#'
#' @description `r lifecycle::badge("stable")`
#'
#' A method that creates a plot for [`DataDual`] object.
#'
#' @param x (`DataDual`)\cr object we want to plot.
#' @param y (`missing`)\cr missing object, for compatibility with the generic function.
#' @param blind (`flag`)\cr indicates whether to blind the data.
#'   If `TRUE`, then placebo subjects are reported at the same level
#'   as the active dose level in the corresponding cohort,
#'   and DLTs are always assigned to the first subjects in a cohort.
#' @param \dots passed to the first inherited method `plot` after this current method.
#'
#' @return The [`ggplot2`] object.
#'
#' @aliases plot-DataDual-method
#' @example examples/Data-method-plot-DataDual.R
#' @export
#'
setMethod(
  f = "plot",
  signature = signature(x = "DataDual", y = "missing"),
  definition = function(x, y, blind = FALSE, ...) {
    assert_flag(blind)

    # Call the superclass method, to get the first plot.
    plot1 <- callNextMethod(x, blind = blind, legend = FALSE, ...)

    # Create the second, biomarker plot.
    df <- h_plot_data_df(x, blind, biomarker = x@w)

    plot2 <- h_plot_data(
      df = df,
      placebo = x@placebo,
      x = "dose",
      y = "biomarker",
      xlab = "Dose Level",
      ylab = "Biomarker",
      cohort_line = 0
    )
    if (!blind) {
      plot2 <- plot2 +
        geom_text(
          aes(y = biomarker + 0.02 * diff(range(biomarker)), label = patient, size = 2),
          data = df,
          hjust = 0,
          vjust = 0.5,
          angle = 90,
          colour = "black",
          show.legend = FALSE
        )
    }

    # Arrange both plots side by side.
    gridExtra::arrangeGrob(plot1, plot2, ncol = 2)
  }
)

# DataDA-plot ----

#' Plot method for the [`DataDA`] class.
#'
#' @description `r lifecycle::badge("stable")`
#'
#' A method that creates a plot for [`DataDA`] object.
#'
#' @param x (`DataDA`)\cr object we want to plot.
#' @param y (`missing`)\cr missing object, for compatibility with the generic function.
#' @param blind (`flag`)\cr indicates whether to blind the data.
#'   If `TRUE`, then placebo subjects are reported at the same level
#'   as the active dose level in the corresponding cohort,
#'   and DLTs are always assigned to the first subjects in a cohort.
#' @param \dots passed to the first inherited method `plot` after this current method.
#'
#' @return The [`ggplot2`] object.
#'
#' @aliases plot-DataDA-method
#' @example examples/Data-method-plot-DataDA.R
#' @export
#'
setMethod(
  f = "plot",
  signature = signature(x = "DataDA", y = "missing"),
  def = function(x, y, blind = FALSE, ...) {
    assert_flag(blind)

    # Call the superclass method, to get the first plot.
    plot1 <- callNextMethod(x, blind = blind, legend = FALSE, ...)

    # Prepare data set for the second, time plot.
    df <- h_plot_data_df(x, blind, u = x@u, t0 = x@t0)
    df$censored <- ifelse(df$u < x@Tmax & df$toxicity == 0, 1, 0)
    df$tend <- df$t0 + df$u # `tend` stands for `time end`
    df$t0_case <- "Start"
    df$tend_case <- ifelse(
      df$toxicity == "Yes",
      "Yes",
      ifelse(df$censored, "Censored", "No")
    )

    # Build plot object.
    plot2 <- h_plot_data(
      df = df,
      placebo = x@placebo,
      x = "t0",
      y = "patient",
      colour_shape = "t0_case",
      colour_values = c(Yes = "red", No = "black", Start = "black", Censored = "black"),
      shape_values = c(Yes = 17, No = 16, Start = 1, Censored = 4),
      xlab = "Time",
      ylab = "Patient",
      cohort_line = 2,
    )
    plot2 <- plot2 +
      geom_segment(aes(xend = tend, yend = patient)) +
      geom_point(aes(x = tend, shape = tend_case, colour = tend_case), size = 3) +
      scale_y_continuous(breaks = df$patient, minor_breaks = NULL)

    if (!blind) {
      plot2 <- plot2 +
        geom_text(
          aes(label = ID, size = 2),
          size = 3,
          hjust = 1.5,
          vjust = 0,
          angle = 0,
          colour = "black",
          show.legend = FALSE
        )
    }

    # Arrange both plots side by side.
    gridExtra::arrangeGrob(plot1, plot2, ncol = 1)
  }
)

## --------------------------------------------------
## Update a Data object
## --------------------------------------------------


##' Update method for the "Data" class
##'
##' Add new data to the \code{\linkS4class{Data}} object
##'
##' @param object the old \code{\linkS4class{Data}} object
##' @param x the dose level (one level only!)
##' @param y the DLT vector (0/1 vector), for all patients in this cohort
##' @param ID the patient IDs
##' @param newCohort logical: if TRUE (default) the new data are assigned
##' to a new cohort
##' @param \dots not used
##' @return the new \code{\linkS4class{Data}} object
##'
##' @example examples/Data-method-update-Data.R
##' @export
##' @keywords methods
setMethod("update",
          signature=
          signature(object="Data"),
          def=
          function(object,
                   x,
                   y,
                   ID=(if(length(object@ID)) max(object@ID) else 0L) + seq_along(y),
                   newCohort=TRUE,
                   ...){

              ## some checks
              stopifnot(is.scalar(x),
                        all(y %in% c(0, 1)))

              ## which grid level is the dose?
              gridLevel <- matchTolerance(x, object@doseGrid)

              ## add it to the data
              if(is.na(gridLevel))
              {
                  stop("dose is not on grid")
              } else {
                  object@xLevel <- c(object@xLevel,
                                     rep(gridLevel,
                                         length(y)))
              }

              ## increment sample size
              object@nObs <- object@nObs + length(y)

              ## add dose
              object@x <- c(object@x,
                            rep(x,
                                length(y)))

              ## add DLT data
              object@y <- c(object@y, as.integer(y))

              ## add ID
              object@ID <- c(object@ID, ID)

              ## add cohort number
              if(newCohort){
                  object@cohort <- c(object@cohort,
                                     rep(max(tail(object@cohort, 1L), 0L) + 1L,
                                         length(y)))
              }else{
                  object@cohort <- c(object@cohort,
                                     rep(max(tail(object@cohort, 1L), 0L),
                                         length(y)))
              }

              ## return the object
              return(object)
          })


## --------------------------------------------------
## Update a DataParts object
## --------------------------------------------------

##' Update method for the "DataParts" class
##'
##' Add new data to the \code{\linkS4class{DataParts}} object
##'
##' @param object the old \code{\linkS4class{DataParts}} object
##' @param x the dose level (one level only!)
##' @param y the DLT vector (0/1 vector), for all patients in this cohort
##' @param ID the patient IDs
##' @param \dots not used
##' @return the new \code{\linkS4class{DataParts}} object
##'
##' @example examples/Data-method-update-DataParts.R
##' @export
##' @keywords methods
setMethod("update",
          signature=
          signature(object="DataParts"),
          def=
          function(object,
                   x,
                   y,
                   ID=(if(length(object@ID)) max(object@ID) else 0L) + seq_along(y),
                   ...){

              ## first do the usual things as for Data objects
              object <- callNextMethod(object=object, x=x, y=y, ID=ID, ...)

              ## update the part information
              object@part <- c(object@part,
                               rep(object@nextPart,
                                   length(y)))

              ## now decide which part the next cohort will belong to:
              ## only if the nextPart was 1, it can potentially be required to
              ## change it to 2 (once it is 2, it stays)
              if(object@nextPart == 1L)
              {
                  ## if there was a DLT in one of the cohorts,
                  ## or if the current dose was the highest from part 1:
                  if(any(object@y == 1L) || x == max(object@part1Ladder))
                  {
                      ## then this closes part 1 and the next cohort will
                      ## be from part 2:
                      object@nextPart <- 2L
                  }
              }

              ## return the object
              return(object)
          })

## --------------------------------------------------
## Update a DataDual object
## --------------------------------------------------

##' Update method for the "DataDual" class
##'
##' Add new data to the \code{\linkS4class{DataDual}} object
##'
##' @param object the old \code{\linkS4class{DataDual}} object
##' @param x the dose level (one level only!)
##' @param y the DLT vector (0/1 vector), for all patients in this cohort
##' @param w the biomarker vector, for all patients in this cohort
##' @param ID the patient IDs
##' @param newCohort logical: if TRUE (default) the new data are assigned
##' to a new cohort
##' @param \dots not used
##' @return the new \code{\linkS4class{DataDual}} object
##'
##' @example examples/Data-method-update-DataDual.R
##' @export
##' @keywords methods
setMethod("update",
          signature=
          signature(object="DataDual"),
          def=
          function(object,
                   x,
                   y,
                   w,
                   newCohort=TRUE,
                   ID=(if(length(object@ID)) max(object@ID) else 0L) + seq_along(y),
                   ...){

              ## first do the usual things as for Data objects
              object <- callNextMethod(object=object, x=x, y=y, ID=ID,
                                       newCohort=newCohort, ...)

              ## update the biomarker information
              object@w <- c(object@w,
                            w)

              ## return the object
              return(object)
          })

## -----------------------------------------------------------------------------------------
## Extracting efficacy responses for subjects without DLE observed
## ---------------------------------------------------------------------------------

##' Extracting efficacy responses for subjects without or with a DLE. This is a class where we separate
##' efficacy responses with or without a DLE. It outputs the efficacy responses and their corresponding
##' dose levels treated at in two categories (with or without DLE)
##'
##' @param object for data input from \code{\linkS4class{DataDual}} object
##' @param \dots unused
##'
##' @export
##' @keywords methods
setGeneric("getEff",
           def=function(object,...){
             standardGeneric("getEff")},
           valueClass="list")

##' @rdname getEff
##' @param x first
##' @param y second
##' @param w third
##' @example examples/Data-method-getEff.R
setMethod("getEff",
          signature=
            signature(object="DataDual"),
          def=
            function(object,
                     x,
                     y,
                     w,...){
              if (length(which(object@y == 1))==0){
                wNoDLE<-object@w
                wDLE<-NULL
                xNoDLE<- object@x
                xDLE<-NULL
              } else {##with observed efficacy response and DLE observed
                IndexDLE<-which(object@y==1)
                ##Take efficacy responses with no DLE observed
                wNoDLE<-object@w[-IndexDLE]
                wDLE<-object@w[IndexDLE]
                ##Take the corresponding dose levels
                xNoDLE<-object@x[-IndexDLE]
                xDLE<-object@x[IndexDLE]
              }
              ret<-list(wDLE=wDLE,xDLE=xDLE,wNoDLE=wNoDLE,xNoDLE=xNoDLE)
              return(ret)
            })



## --------------------------------------------------
## Update a DataDA object
## --------------------------------------------------

##' Update method for the `DataDA` class
##'
##' Update observations in the \code{\linkS4class{DataDA}} object
##'
##' @param object the old \code{\linkS4class{DataDA}} object
##' @param factDLTs the simulated DLT outcome for all patients
##' @param factSurv the simulated DLT time for all patients
##' @param factT0 the time that patients start DLT observation window
##' @param thisDose the current dose of the cohort
##' @param ID the patient IDs
##' @param newCohort logical: if TRUE (default) the new data are assigned
##' to a new cohort
##' @param trialtime current time in the trial.
##' @param \dots not used
##' @return the new \code{\linkS4class{DataDA}} object
##'
##' @example examples/Data-method-update-DAData.R
##' @export
##' @keywords methods
setMethod("update",
          signature=
            signature(object="DataDA"),
          def=
            function(object,
                     # x,
                     # y,
                     # u,
                     # ID,
                     factDLTs,
                     factSurv,
                     factT0,
                     thisDose,
                     trialtime,
                     ID=NULL,
                     newCohort=TRUE,
                     ...){

              ## some checks
              stopifnot(is.scalar(thisDose),
                        all(factSurv >= 0),
                        all(factDLTs %in% c(0, 1)),
                        length(factDLTs) == length(factSurv),
                        length(factDLTs) == length(factT0)
              )

              ## which grid level is the dose?
              gridLevel <- match(thisDose, object@doseGrid)

              ## add it to the data
              if(is.na(gridLevel))
              {
                stop("dose is not on grid")
              }

              ## increment sample size
              object@nObs <- length(factDLTs)

              ##How many additional patients
              size<-length(factDLTs)-length(object@x)

              ## add dose
              object@x <- c(object@x,
                            rep(thisDose,
                                size))

              #update xLevel
              object@xLevel <- match(object@x,
                                     object@doseGrid)

              ##update DLT free survival time
              object@u<- apply(rbind(factSurv,trialtime-factT0),2,min)

              ## DLT will be observed once the followup time >= the time to DLT
              object@y <- as.integer(factDLTs*(trialtime>=factT0+factSurv))

              ##t0 will be updated;
              object@t0 <- factT0

              ## add ID
              if(size>0)
              {
                if(is.null(ID))
                {
                  object@ID <-c(object@ID,
                                (if(length(object@ID)) max(object@ID) else 0L) +
                                  1:size)
                }
              }

              ## add cohort number
              if(newCohort)
              {
                object@cohort <- c(object@cohort,
                                   rep(max(tail(object@cohort, 1L), 0L) + 1L,
                                       size))
              } else {
                object@cohort <- c(object@cohort,
                                   rep(max(tail(object@cohort, 1L), 0L),
                                       size))
              }

              ## return the object
              return(object)
            })
