#####################################################################################
## Author: Daniel Sabanes Bove [sabanesd *a*t* roche *.* com]
## Project: Object-oriented implementation of CRM designs
##
## Time-stamp: <[Design-class.R] by DSB Die 30/12/2014 17:27>
##
## Description:
## This class encapsulates a whole CRM design.
##
## History:
## 12/02/2014   file creation
#####################################################################################

##' @include Model-class.R
##' @include Rules-class.R
##' @include Data-class.R
##' @include helpers.R
{}

## --------------------------------------------------
## Class for the whole CRM design
## --------------------------------------------------

##' Class for the CRM design
##'
##' @slot model the model to be used, an object of class
##' \code{\linkS4class{Model}}
##' @slot nextBest how to find the next best dose, an object of class
##' \code{\linkS4class{NextBest}}
##' @slot stopping stopping rule(s) for the trial, an object of class
##' \code{\linkS4class{Stopping}}
##' @slot increments how to control increments between dose levels,
##' an object of class \code{\linkS4class{Increments}}
##' @slot cohortSize rules for the cohort sizes,
##' an object of class \code{\linkS4class{CohortSize}}
##' @slot data what is the dose grid, any previous data, etc., contained
##' in an object of class \code{\linkS4class{Data}}
##' @slot startingDose what is the starting dose? Must lie on the grid in \code{data}
##'
##' @export
##' @keywords classes
setClass(Class="Design",
         representation=
         representation(model="Model",
                        nextBest="NextBest",
                        stopping="Stopping",
                        increments="Increments",
                        cohortSize="CohortSize",
                        data="Data",
                        startingDose="numeric"),
         validity=
         function(object){
             stopifnot(is.scalar(object@startingDose),
                       object@startingDose %in% object@data@doseGrid)
         })



##' Class for rule-based designs
##'
##' The difference to \code{\linkS4class{Design}} class is that
##' model, stopping and increments slots are missing.
##'
##' @slot nextBest how to find the next best dose, an object of class
##' \code{\linkS4class{NextBest}}
##' @slot cohortSize rules for the cohort sizes,
##' an object of class \code{\linkS4class{CohortSize}}
##' @slot data what is the dose grid, any previous data, etc., contained
##' in an object of class \code{\linkS4class{Data}}
##' @slot startingDose what is the starting dose? Must lie on the grid in \code{data}
##'
##' @export
##' @keywords classes
setClass(Class="RuleDesign",
         representation=
         representation(nextBest="NextBest",
                        cohortSize="CohortSize",
                        data="Data",
                        startingDose="numeric"),
         validity=
         function(object){
             stopifnot(is.scalar(object@startingDose),
                       object@startingDose %in% object@data@doseGrid)
         })


##' Creates a new 3+3 design object from a dose grid
##'
##' @param doseGrid the dose grid to be used
##' @return the object of class \code{\linkS4class{RuleDesign}} with the
##' 3+3 design
##'
##' @export
##' @keywords programming
##' @author Daniel Sabanes Bove \email{sabanesd@@roche.com}
getThreePlusThreeDesign <- function(doseGrid)
{
    emptydata <- new("Data",
                     x=numeric(),
                     y=integer(),
                     doseGrid=doseGrid)
    new("NextBestThreePlusThree")
    design <- new("RuleDesign",
                  nextBest=new("NextBestThreePlusThree"),
                  data=emptydata,
                  cohortSize=
                  new("CohortSizeConst", size=3L),
                  ## using a constant cohort size of 3,
                  ## we obtain exactly the 3+3 design
                  startingDose=head(emptydata@doseGrid, 1))
}
