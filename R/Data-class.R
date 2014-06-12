#####################################################################################
## Author: Daniel Sabanes Bove [sabanesd *a*t* roche *.* com]
## Project: Object-oriented implementation of CRM designs
##
## Time-stamp: <[Data-class.R] by DSB Die 29/04/2014 14:54>
##
## Description:
## Encapsulate the data input in formal classes.
##
## History:
## 29/01/2014   file creation
## 07/02/2014   add cohort indices
###################################################################################

## --------------------------------------------------
## Class for the data input
## --------------------------------------------------


##' Class for the data input
##'
##' @slot x the doses for the patients
##' @slot y the vector of toxicity events (0 or 1 integers)
##' @slot ID unique patient IDs (integer vector)
##' @slot cohort the cohort indices (sorted values from 0, 1, 2, ...)
##' @slot doseGrid the vector of all possible doses (sorted), i.e. the dose
##' grid
##' @slot nObs number of observations
##' @slot nGrid number of gridpoints
##' @slot xLevel the levels for the doses the patients have been given
##'
##' @export
##' @keywords classes
setClass(Class="Data",
         representation=
         representation(x="numeric",
                        y="integer",
                        ID="integer",
                        cohort="integer",
                        doseGrid="numeric",
                        nObs="integer",
                        nGrid="integer",
                        xLevel="integer"),
         validity=
         function(object){
             stopifnot(all(object@y %in% c(0, 1)),
                       all(! duplicated(object@ID)),
                       all(object@cohort >= 0),
                       ! is.unsorted(object@cohort,
                                     strictly=FALSE),
                       ## be sure that there is only one dose level per cohort
                       all(tapply(X=object@x,
                                  INDEX=object@cohort,
                                  FUN=
                                  function(doses){length(unique(doses))}) == 1),
                       all(object@x %in% object@doseGrid),
                       ! is.unsorted(object@doseGrid,
                                     strictly=TRUE),
                       identical(object@nObs, length(object@y)),
                       identical(object@nObs, length(object@cohort)),
                       identical(object@nObs, length(object@ID)))
         })


##' Initialization method for the "Data" class
##'
##' This is the method for initializing a "Data" class object.
##'
##' Note that \code{ID} and \code{cohort} can be missing, then a warning
##' will be issued and the variables will be filled with default
##' IDs and best guesses, respectively.
##'
##' @param .Object the \code{\linkS4class{Data}} we want to initialize
##' @param x the doses for the patients
##' @param y the vector of toxicity events (0 or 1 integers)
##' @param ID unique patient IDs (integer vector)
##' @param cohort the cohort indices (sorted values from 0, 1, 2, ...)
##' @param doseGrid the vector of all possible doses
##'
##' @export
##' @keywords methods
setMethod("initialize",
          signature=
          signature(.Object = "Data"),
          definition=
          function (.Object,
                    ## Note: default arguments are necessary here
                    ## in order to be able to define the subclass
                    x=numeric(),
                    y=integer(),
                    ID=integer(),
                    cohort=integer(),
                    doseGrid=numeric(),
                    ...){
              ## special initialization for this class
              doseGrid <- sort(unique(doseGrid))
              .Object@nObs <- length(x)
              .Object@nGrid <- length(doseGrid)
              .Object@xLevel <- match(x, doseGrid)

              if((missing(ID) || length(ID) == 0) && length(x))
              {
                  warning("Used default patient IDs!")
                  ID <- seq_along(x)
              }

              if((missing(cohort) || length(cohort) == 0) && length(x))
              {
                  warning("Used best guess cohort indices!")
                  ## This is just assuming that consecutive patients
                  ## in the data set are in the same cohort if they
                  ## have the same dose. Note that this could be wrong,
                  ## if two subsequent cohorts are at the same dose.
                  cohort <- as.integer(c(1, 1 + cumsum(diff(x) != 0)))
              }

              ## then use inherited initialization
              ## (in this case just putting arguments into slots)
              callNextMethod(.Object, x=x, y=y, ID=ID, cohort=cohort,
                             doseGrid=doseGrid, ...)
          })



## --------------------------------------------------
## Subclass with additional biomarker information
## --------------------------------------------------

##' Class for the dual endpoint data input
##'
##' This is a subclass of \code{\linkS4class{Data}}, so contains all
##' slots from \code{\linkS4class{Data}}, and in addition biomarker
##' values.
##'
##' @slot w the continuous vector of biomarker values
##'
##' @export
##' @keywords classes
setClass(Class="DataDual",
         representation=
         representation(w="numeric"),
         contains="Data",
         validity=
         function(object){
             stopifnot(identical(object@nObs, length(object@w)))
         })

