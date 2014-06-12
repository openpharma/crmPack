#####################################################################################
## Author: Daniel Sabanes Bove [sabanesd *a*t* roche *.* com]
## Project: Object-oriented implementation of CRM designs
##
## Time-stamp: <[Design-class.R] by DSB Mon 02/06/2014 17:10>
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



