#' @include helpers.R Data-validity.R
NULL

# GeneralData-class ----

#' `GeneralData`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' The [`GeneralData-class`] is a class for general data input.
#'
#' @slot ID (`integer`)\cr unique patient IDs.
#' @slot cohort (`integer`)\cr the cohort indices (sorted values from \{0, 1, 2, ...\}).
#' @slot nObs (`integer`)\cr number of observations, a single value.
#'
#' @export
#' @keywords classes
.GeneralData <- setClass(
  Class = "GeneralData",
  slots = c(
    ID = "integer",
    cohort = "integer",
    nObs = "integer"
  ),
  prototype = prototype(
    ID = integer(),
    cohort = integer(),
    nObs = 0L
  ),
  validity = validate_subjects
)

##' Class for the data input
##'
##' This class inherits from \code{\linkS4class{GeneralData}}.
##'
##' @slot x the doses for the patients
##' @slot y the vector of toxicity events (0 or 1 integers)
##' @slot doseGrid the vector of all possible doses (sorted), i.e. the dose
##' grid
##' @slot nGrid number of gridpoints
##' @slot xLevel the levels for the doses the patients have been given
##' @slot placebo logical value: if TRUE the first dose level in the grid is
##' considered as PLACEBO
##'
##' @example examples/Data-class.R
##' @export
##' @keywords classes
.Data <-
    setClass(Class="Data",
             representation(x="numeric",
                            y="integer",
                            doseGrid="numeric",
                            nGrid="integer",
                            xLevel="integer",
                            placebo="logical"),
             prototype(x=numeric(),
                       y=integer(),
                       doseGrid=numeric(),
                       nGrid=0L,
                       xLevel=integer(),
                       placebo=FALSE),
             contains="GeneralData",
             validity=
                 function(object){
                     o <- Validate()

                     o$check(all(object@y %in% c(0, 1)),
                             "DLT vector y can only have 0 or 1 values")
                     if(!object@placebo){
                        o$check(all(tapply(X=object@x,
                                           INDEX=object@cohort,
                                           FUN=
                                            function(doses){length(unique(doses))}) == 1),
                             "there must be only one dose level per cohort")
                     }else{
                        o$check(all(tapply(X=object@x,
                                          INDEX=object@cohort,
                                          FUN=
                                            function(doses){
                                              doses <- doses[doses != object@doseGrid[1]]
                                              length(unique(doses))}) == 1),
                               "There must be only one dose level, other than placebo, per cohort. In addition a cohort with only placebo is not allowed")
                     }

                     o$check(all(object@x %~% object@doseGrid),
                             "dose values in x must be from doseGrid (tolerance 1e-10)")
                     o$check(! is.unsorted(object@doseGrid,
                                           strictly=TRUE),
                             "doseGrid must be sorted and without duplicate values")
                     for(thisSlot in c("x", "y"))
                         o$check(identical(object@nObs, length(slot(object, thisSlot))),
                                 paste(thisSlot, "must have length nObs"))
                     o$check(identical(object@nGrid, length(object@doseGrid)),
                             "doseGrid must have length nGrid")
                     o$check(all.equal(object@x,
                                       object@doseGrid[object@xLevel],
                                       tolerance=1e-10,
                                       check.names=FALSE,
                                       check.attributes=FALSE),
                             "x must be doseGrid[xLevel] (tolerance 1e-10)")

                     o$result()
                 })
validObject(.Data())

##' Initialization function for the "Data" class
##'
##' This is the function for initializing a "Data" class object.
##'
##' Note that \code{ID} and \code{cohort} can be missing, then a warning
##' will be issued and the variables will be filled with default
##' IDs and best guesses, respectively.
##'
##' @param x the doses for the patients
##' @param y the vector of toxicity events (0 or 1 integers). You can also
##' normal numeric vectors, but these will then be converted to integers.
##' @param ID unique patient IDs (integer vector)
##' @param cohort the cohort indices (sorted values from 0, 1, 2, ...)
##' @param doseGrid the vector of all possible doses
##' @param placebo logical value: if TRUE the first dose level in the grid is
##' considered as PLACEBO
##' @param \dots not used
##' @return the initialized \code{\linkS4class{Data}} object
##'
##' @export
##' @keywords programming
Data <- function(x=numeric(),
                 y=integer(),
                 ID=integer(),
                 cohort=integer(),
                 doseGrid=numeric(),
                 placebo=FALSE,
                 ...){
    ## sort the dose grid
    doseGrid <- as.numeric(sort(unique(doseGrid)))

    ## check IDs
    if((missing(ID) || length(ID) == 0) && length(x))
    {
        warning("Used default patient IDs!")
        ID <- seq_along(x)
    }

    ## check cohort indices
    if((missing(cohort) || length(cohort) == 0) && length(x))
    {
        warning("Used best guess cohort indices!")
        ## This is just assuming that consecutive patients
        ## in the data set are in the same cohort if they
        ## have the same dose. Note that this could be wrong,
        ## if two subsequent cohorts are at the same dose.
        cohort <- as.integer(c(1, 1 + cumsum(diff(x) != 0)))
    }

    ## then initialize the Data object
    ## (in this case just putting arguments into slots)
    ret <- .Data(x=as.numeric(x),
                 y=safeInteger(y),
                 ID=safeInteger(ID),
                 cohort=safeInteger(cohort),
                 doseGrid=doseGrid,
                 nObs=length(x),
                 nGrid=length(doseGrid),
                 xLevel=matchTolerance(x=x, table=doseGrid),
                 placebo=placebo)
    return(ret)
}
validObject(Data())


## ============================================================


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
##' @example examples/Data-class-DataDual.R
##' @export
##' @keywords classes
.DataDual <-
    setClass(Class="DataDual",
             representation=
                 representation(w="numeric"),
             contains="Data",
             validity=
                 function(object){
                     o <- Validate()

                     o$check(identical(object@nObs, length(object@w)),
                             "w must have length nObs")

                     o$result()
                 })
validObject(.DataDual())

##' Initialization function for the "DataDual" class
##'
##' This is the function for initializing a "DataDual" class object.
##'
##' @param w the continuous vector of biomarker values
##' @param \dots additional parameters from \code{\link{Data}}
##' @return the initialized \code{\linkS4class{DataDual}} object
##'
##' @export
##' @keywords programming
DataDual <- function(w=numeric(),
                     ...)
{
    start <- Data(...)
    .DataDual(start,
              w=w)
}
validObject(DataDual())

## ============================================================


## --------------------------------------------------
## Subclass with additional two parts information
## --------------------------------------------------

##' Class for the data with two study parts
##'
##' This is a subclass of \code{\linkS4class{Data}}, so contains all
##' slots from \code{\linkS4class{Data}}, and in addition information on the two
##' study parts.
##'
##' @slot part integer vector; which part does each of the patients belong to?
##' @slot nextPart integer; what is the part for the next cohort?
##' @slot part1Ladder sorted numeric vector; what is the escalation ladder for
##' part 1? This shall be a subset of the \code{doseGrid}.
##'
##' @example examples/Data-class-DataParts.R
##' @export
##' @keywords classes
.DataParts <-
    setClass(Class="DataParts",
             representation(part="integer",
                            nextPart="integer",
                            part1Ladder="numeric"),
             prototype(part=integer(),
                       nextPart=1L,
                       part1Ladder=numeric()),
             contains="Data",
             validity=
                 function(object){
                     o <- Validate()

                     o$check(identical(length(object@part), length(object@x)),
                             "part and x must have same length")
                     o$check(all(object@part %in% as.integer(c(1, 2))),
                             "part must have entries 1 or 2")
                     o$check(is.scalar(object@nextPart) &&
                                 (object@nextPart %in% as.integer(c(1, 2))),
                             "nextPart must be scalar 1 or 2")
                     o$check(all(object@part1Ladder %in% object@doseGrid),
                             "part1Ladder must have entries from doseGrid")
                     o$check(! is.unsorted(object@part1Ladder,
                                           strictly=TRUE),
                             "part1Ladder must be sorted and have unique values")

                     o$result()
                 })
validObject(.DataParts())


##' Initialization function for the "DataParts" class
##'
##' This is the function for initializing a \code{\linkS4class{DataParts}}
##' object.
##'
##' @param part which part does each of the patients belong to?
##' @param nextPart what is the part for the next cohort? (1 or 2)
##' @param part1Ladder what is the escalation ladder for
##' part 1?
##' @param \dots additional parameters from \code{\link{Data}}
##' @return the initialized \code{\linkS4class{DataParts}} object
##'
##' @export
##' @keywords programming
DataParts <- function(part=integer(),
                      nextPart=1L,
                      part1Ladder=numeric(),
                      ...)
{
    start <- Data(...)
    .DataParts(start,
               part=part,
               nextPart=nextPart,
               part1Ladder=part1Ladder)
}
validObject(DataParts())

## ============================================================

## --------------------------------------------------
## Subclass with additional data for mixture
## --------------------------------------------------


##' Class for the data with mixture sharing
##'
##' @slot xshare the doses for the share patients
##' @slot yshare the vector of toxicity events (0 or 1 integers) for the share
##' patients
##' @slot nObsshare number of share patients
##'
##' @seealso \code{\linkS4class{LogisticLogNormalMixture}} for the explanation
##' how to use this data class
##' @export
##' @example examples/Model-class-LogisticLogNormalMixture.R
##' @keywords classes
.DataMixture <-
  setClass(Class="DataMixture",
           representation(xshare="numeric",
                          yshare="integer",
                          nObsshare="integer"),
           prototype(xshare=numeric(),
                     yshare=integer(),
                     nObsshare=0L),
           contains="Data",
           validity=
             function(object){
               o <- Validate()

               o$check(all(object@yshare %in% c(0, 1)),
                       "DLT vector yshare can only have 0 or 1 values")
               o$check(all(object@xshare %in% object@doseGrid),
                       "dose values in xshare must be from doseGrid")
               for(thisSlot in c("xshare", "yshare"))
                 o$check(identical(object@nObsshare, length(slot(object, thisSlot))),
                         paste(thisSlot, "must have length nObs"))

               o$result()
             })
validObject(.DataMixture())

##' Initialization function for the "DataMixture" class
##'
##' This is the function for initializing a "DataMixture" class object.
##'
##' @param xshare see \code{\linkS4class{DataMixture}}
##' @param yshare see \code{\linkS4class{DataMixture}}
##' @param \dots additional arguments for the underlying Data slots
##'
##' @return the initialized \code{\linkS4class{DataMixture}} object
##'
##' @export
##' @keywords programming
DataMixture <- function(xshare=numeric(),
                        yshare=integer(),
                        ...){
  start <- Data(...)
  ret <- .DataMixture(start,
                      xshare=as.numeric(xshare),
                      yshare=safeInteger(yshare),
                      nObsshare=length(xshare))
  return(ret)
}
validObject(DataMixture())

## ============================================================

## --------------------------------------------------
## Subclass with DLT free survival
## --------------------------------------------------

##' Class for the time-to-DLT augmented data input
##'
##' This is a subclass of \code{\linkS4class{Data}}, so contains all
##' slots from \code{\linkS4class{Data}}, and in addition DLT free survival
##' values. (Note that \dQuote{survival} values here refers to the time that
##' the subject did not experience any DLT yet, and is not referring to
##' deaths.)
##'
##' @slot u the continuous vector of DLT free survival values
##' @slot Tmax the DLT observation period
##' @slot t0 time of initial dosing for each patient
##'
##' @example examples/Data-class-DataDA.R
##' @export
##' @keywords classes
.DataDA <-
  setClass(Class="DataDA",
           representation=
             representation(u="numeric",
                            t0="numeric",
                            Tmax="numeric"),
           contains="Data",
           validity=
             function(object){
               o <- Validate()

               o$check(identical(object@nObs, length(object@u)),
                       "u must have length nObs")

               o$check(identical(object@nObs, length(object@t0)),
                       "t0 must have length nObs")

               o$check(all(object@u <= object @Tmax),
                       "u entries must not be larger than Tmax")
               ## (this has to be because the hazard
               ## at Tmax is infinity -> all individuals stop
               ## there)

               o$check(all(object@u >= 0),
                       "u entries must be non-negative")

               ## DSB: why check not required?
               #                                  o$check(all(object@t0 >= 0),
               #                                          "t0 entries must be non-negative")
               #
               o$check(all(object@Tmax > 0),
                       "DLT window needs to be greater than 0")

               o$result()
             })
validObject(.DataDA())

##' Initialization function for the `DataDA` class
##'
##' This is the function for initializing a `DataDA` class object.
##'
##' @param u the continuous vector of DLT free survival values
##' @param Tmax the DLT observation period
##' @param t0 time of initial dosing for each patient (default: 0)
##' @param \dots additional parameters from \code{\link{Data}}
##' @return the initialized \code{\linkS4class{DataDA}} object
##'
##' @export
##' @keywords programming
DataDA <- function(u=numeric(),
                   Tmax=numeric(),
                   t0=numeric(),
                   ...)
{

  start <- Data(...)

  if(missing(t0))
  {
    t0 <- rep(0, length(u))
  }

  .DataDA(start,
          u = as.numeric(u),
          t0 = t0,
          Tmax = as.numeric(Tmax))
}
validObject(DataDA())


## ============================================================


