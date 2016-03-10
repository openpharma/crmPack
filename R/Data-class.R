#####################################################################################
## Author: Daniel Sabanes Bove [sabanesd *a*t* roche *.* com]
## Project: Object-oriented implementation of CRM designs
##
## Time-stamp: <[Data-class.R] by DSB Son 15/02/2015 17:24>
##
## Description:
## Encapsulate the data input in formal classes.
##
## History:
## 29/01/2014   file creation
## 07/02/2014   add cohort indices
###################################################################################

##' @include helpers.R
{}


## --------------------------------------------------
## Class for general data input
## --------------------------------------------------


##' Class for general data input
##'
##' @slot ID unique patient IDs (integer vector)
##' @slot cohort the cohort indices (sorted values from 0, 1, 2, ...)
##' @slot nObs number of observations
##'
##' @export
##' @keywords classes
.GeneralData <-
    setClass(Class="GeneralData",
             representation(ID="integer",
                            cohort="integer",
                            nObs="integer"),
             prototype(ID=integer(),
                       cohort=integer(),
                       nObs=0L),
             validity=
                 function(object){
                     o <- Validate()

                     o$check(all(! duplicated(object@ID)),
                             "IDs must be unique")
                     o$check(all(object@cohort >= 0),
                             "cohort indices must be non-negative")
                     o$check(! is.unsorted(object@cohort,
                                           strictly=FALSE),
                             "cohort indices must be sorted")
                     for(thisSlot in c("cohort", "ID"))
                         o$check(identical(object@nObs, length(slot(object, thisSlot))),
                                 paste(thisSlot, "must have length nObs"))

                     o$result()
                 })
validObject(.GeneralData())


## --------------------------------------------------
## Class for the data input
## --------------------------------------------------


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
##'
##' @export
##' @keywords classes
.Data <-
    setClass(Class="Data",
             representation(x="numeric",
                            y="integer",
                            doseGrid="numeric",
                            nGrid="integer",
                            xLevel="integer"),
             prototype(x=numeric(),
                       y=integer(),
                       doseGrid=numeric(),
                       nGrid=0L,
                       xLevel=integer()),
             contains="GeneralData",
             validity=
                 function(object){
                     o <- Validate()

                     o$check(all(object@y %in% c(0, 1)),
                             "DLT vector y can only have 0 or 1 values")
                     o$check(all(tapply(X=object@x,
                                        INDEX=object@cohort,
                                        FUN=
                                            function(doses){length(unique(doses))}) == 1),
                             "there must be only one dose level per cohort")
                     o$check(all(object@x %in% object@doseGrid),
                             "dose values in x must be from doseGrid")
                     o$check(! is.unsorted(object@doseGrid,
                                           strictly=TRUE),
                             "doseGrid must be sorted and without duplicate values")
                     for(thisSlot in c("x", "y"))
                         o$check(identical(object@nObs, length(slot(object, thisSlot))),
                                 paste(thisSlot, "must have length nObs"))
                     o$check(identical(object@nGrid, length(object@doseGrid)),
                             "doseGrid must have length nGrid")
                     o$check(identical(object@x,
                                       object@doseGrid[object@xLevel]),
                             "x must be doseGrid[xLevel]")

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
                 xLevel=match(x=x, table=doseGrid))
    return(ret)
}
validObject(Data())




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



## --------------------------------------------------
## Class for combo trials
## --------------------------------------------------


##' Class for the data input in combo trials
##'
##' This class inherits from \code{\linkS4class{GeneralData}}.
##'
##' @slot x a matrix with the doses of all \code{nDrugs} drugs (columns) for the
##' \code{nObs} patients (rows). The column names are the \code{drugNames}.
##' @slot y the vector of toxicity events (0 or 1 integers)
##' @slot doseGrid a list containing a vector of all possible doses (sorted) for
##' each of the \code{nDrugs} drugs (named with \code{drugNames}).
##' @slot nDrugs number of drugs
##' @slot drugNames character vector with the drug names
##' @slot nGrid vector with the number of gridpoints for each of the drugs
##' @slot xLevel an integer matrix with the levels for the doses the patients
##' have been given, same dimensions as \code{x}.
##'
##' @export
##' @keywords classes
.DataCombo <-
    setClass(Class="DataCombo",
             representation(x="matrix",
                            y="integer",
                            doseGrid="list",
                            nDrugs="integer",
                            drugNames="character",
                            nGrid="integer",
                            xLevel="matrix"),
             prototype(x=
                           cbind(a=numeric(),
                                 b=numeric()),
                       y=integer(),
                       doseGrid=
                           list(a=numeric(),
                                b=numeric()),
                       nDrugs=2L,
                       drugNames=c("a", "b"),
                       nGrid=
                           c(a=0L,
                             b=0L),
                       xLevel=
                           cbind(a=integer(),
                                 b=integer())),
             contains="GeneralData",
             validity=
                 function(object){
                     o <- Validate()

                     o$check(identical(colnames(object@x),
                                       object@drugNames),
                             "column names of matrix x must be drugNames")
                     o$check(identical(colnames(object@xLevel),
                                       object@drugNames),
                             "column names of matrix xLevel must be drugNames")
                     o$check(identical(names(object@doseGrid),
                                       object@drugNames),
                             "names of list doseGrid must be drugNames")
                     o$check(identical(names(object@nGrid),
                                       object@drugNames),
                             "names of vector nGrid must be drugNames")
                     o$check(identical(dim(object@x),
                                       c(object@nObs, object@nDrugs)),
                             "dimensions of matrix x must be (nObs, nDrugs)")
                     o$check(identical(dim(object@xLevel),
                                       c(object@nObs, object@nDrugs)),
                             "dimensions of matrix xLevel must be (nObs, nDrugs)")
                     o$check(identical(length(object@drugNames),
                                       object@nDrugs),
                             "length of drugNames must be nDrugs")
                     o$check(identical(sapply(object@doseGrid, length),
                                       object@nGrid),
                             "lengths of doseGrid elements must be nGrid")
                     o$check(all(object@y %in% c(0, 1)),
                             "DLT vector y can only have 0 or 1 values")
                     o$check(all(by(data=object@x,
                                    INDICES=object@cohort,
                                    FUN=
                                        function(doseMatrix){nrow(unique(doseMatrix))}) == 1),
                             "there must be only one dose level combination per cohort")
                     for(k in object@drugNames)
                     {
                         o$check(all(object@x[,k] %in% object@doseGrid[[k]]),
                                 paste(k,
                                       "dose values in x must be from doseGrid"))
                         o$check(! is.unsorted(object@doseGrid[[k]],
                                               strictly=TRUE),
                                 paste(k,
                                       "doseGrid must be sorted and without duplicate values"))
                         o$check(identical(object@x[,k],
                                           object@doseGrid[[k]][object@xLevel[,k]]),
                                 paste("xLevel for drug", k, "not matching x"))
                     }
                     for(thisSlot in c("y"))
                         o$check(identical(object@nObs, length(slot(object, thisSlot))),
                                 paste(thisSlot, "must have length nObs"))

                     o$result()
                 })
validObject(.DataCombo())

##' Initialization function for the "DataCombo" class
##'
##' This is the function for initializing a "DataCombo" class object.
##'
##' Note that \code{ID} and \code{cohort} can be missing, then a warning
##' will be issued and the variables will be filled with default
##' IDs and best guesses, respectively.
##'
##' @param x the matrix with the doses for the patients. Recommendation: Use
##' \code{cbind(drugA=..., drugB=...)} to create this matrix.
##' @param y the vector of toxicity events (0 or 1 integers). You can also
##' normal numeric vectors, but these will then be converted to integers.
##' @param ID unique patient IDs (integer vector)
##' @param cohort the cohort indices (sorted values from 0, 1, 2, ...)
##' @param doseGrid the list with vectors of all possible doses for each of the
##' drugs
##' @return the initialized \code{\linkS4class{DataCombo}} object
##'
##' @export
##' @keywords programming
DataCombo <- function(x=
                        matrix(nrow=0, ncol=2,
                               data=NA,
                               dimnames=list(NULL, names(doseGrid))),
                      y=integer(),
                      ID,
                      cohort,
                      doseGrid)
{
    ## checks
    stopifnot(is.matrix(x),
              is.list(doseGrid),
              identical(colnames(x), names(doseGrid)))
    storage.mode(x) <- "double"

    ## extracts
    nObs <- nrow(x)
    nDrugs <- ncol(x)
    drugNames <- colnames(x)

    ## sort the dose grid for each drug
    doseGrid <- lapply(doseGrid,
                       function(x) as.numeric(sort(unique(x))))

    ## check IDs
    if(missing(ID) || length(ID) == 0)
    {
        warning("Used default patient IDs!")
        ID <- seq_len(nObs)
    }

    ## check cohort indices
    if(missing(cohort) || length(cohort) == 0)
    {
        warning("Used best guess cohort indices!")
        ## This is just assuming that consecutive patients
        ## in the data set are in the same cohort if they
        ## have the same dose. Note that this could be wrong,
        ## if two subsequent cohorts are at the same dose.
        cohort <- 
          if(nObs == 0L) integer() else 
            as.integer(c(1, 1 + cumsum(rowSums(abs(diff(x))) != 0)))
    }

    ## get xLevel matrix
    xLevel <- x
    for(k in drugNames)
    {
        xLevel[, k] <- match(x=x[, k],
                             table=doseGrid[[k]])
    }

    ## then initialize the DataCombo object
    ## (in this case just putting arguments into slots)
    ret <- .DataCombo(x=x,
                      y=safeInteger(y),
                      ID=safeInteger(ID),
                      cohort=safeInteger(cohort),
                      doseGrid=doseGrid,
                      nObs=nObs,
                      nDrugs=nDrugs,
                      drugNames=drugNames,
                      nGrid=sapply(doseGrid, length),
                      xLevel=xLevel)
    return(ret)
}

