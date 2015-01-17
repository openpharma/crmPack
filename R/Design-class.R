#####################################################################################
## Author: Daniel Sabanes Bove [sabanesd *a*t* roche *.* com]
## Project: Object-oriented implementation of CRM designs
##
## Time-stamp: <[Design-class.R] by DSB Fre 16/01/2015 14:33>
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
## Classes for rule-based designs
## --------------------------------------------------


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
##' @slot startingDose what is the starting dose? Must lie on the grid in
##' \code{data}
##'
##' @export
##' @keywords classes
.RuleDesign <-
    setClass(Class="RuleDesign",
             representation(nextBest="NextBest",
                            cohortSize="CohortSize",
                            data="Data",
                            startingDose="numeric"),
             prototype(nextBest=.NextBestThreePlusThree(),
                       cohortSize=CohortSizeConst(3),
                       data=Data(doseGrid=1:3),
                       startingDose=1),
             validity=
                 function(object){
                     o <- Validate()

                     o$check(is.scalar(object@startingDose),
                             "startingDose must be scalar")
                     o$check(object@startingDose %in% object@data@doseGrid,
                             "startingDose must be included in data@doseGrid")

                     o$result()
                 })
validObject(.RuleDesign())

##' Initialization function for "RuleDesign"
##'
##' @param nextBest see \code{\linkS4class{RuleDesign}}
##' @param cohortSize see \code{\linkS4class{RuleDesign}}
##' @param data see \code{\linkS4class{RuleDesign}}
##' @param startingDose see \code{\linkS4class{RuleDesign}}
##' @return the \code{\linkS4class{RuleDesign}} object
##'
##' @export
##' @keywords methods
RuleDesign <- function(nextBest,
                       cohortSize,
                       data,
                       startingDose)
{
    .RuleDesign(nextBest=nextBest,
                cohortSize=cohortSize,
                data=data,
                startingDose=as.numeric(startingDose))
}


## --------------------------------------------------
## Classes for model-based designs
## --------------------------------------------------

##' Class for the CRM design
##'
##' In addition to the slots in the more simple \code{\linkS4class{RuleDesign}},
##' objects of this class contain:
##'
##' @slot model the model to be used, an object of class
##' \code{\linkS4class{Model}}
##' @slot stopping stopping rule(s) for the trial, an object of class
##' \code{\linkS4class{Stopping}}
##' @slot increments how to control increments between dose levels,
##' an object of class \code{\linkS4class{Increments}}
##'
##' @export
##' @keywords classes
.Design <-
    setClass(Class="Design",
             representation(model="Model",
                            stopping="Stopping",
                            increments="Increments"),
             prototype(model=.LogisticNormal(),
                       nextBest=.NextBestNCRM(),
                       stopping=.StoppingMinPatients(),
                       increments=.IncrementsRelative()),
             contains=list("RuleDesign"))
validObject(.Design())


##' Initialization function for "Design"
##'
##' @param model see \code{\linkS4class{Design}}
##' @param stopping see \code{\linkS4class{Design}}
##' @param increments see \code{\linkS4class{Design}}
##' @param \dots additional arguments for \code{\link{RuleDesign}}
##' @return the \code{\linkS4class{Design}} object
##'
##' @export
##' @keywords methods
Design <- function(model,
                   stopping,
                   increments,
                   ...)
{
    start <- RuleDesign(...)
    .Design(start,
            model=model,
            stopping=stopping,
            increments=increments)
}



##' Class for the dual-endpoint CRM design
##'
##' This class has special requirements for the \code{model} and \code{data}
##' slots in comparison to the parent class \code{\linkS4class{Design}}:
##'
##' @slot model the model to be used, an object of class
##' \code{\linkS4class{DualEndpoint}}
##' @slot data what is the dose grid, any previous data, etc., contained
##' in an object of class \code{\linkS4class{DataDual}}
##'
##' Note that the \code{NextBest} slot can be of any class, this allows for easy
##' comparison with recommendation methods that don't use the
##' biomarker information.
##'
##' @export
##' @keywords classes
.DualDesign <-
    setClass(Class="DualDesign",
             representation(model="DualEndpoint",
                            data="DataDual"),
             prototype(model=.DualEndpoint(),
                       nextBest=.NextBestDualEndpoint(),
                       data=DataDual(doseGrid=1:2),
                       startingDose=1),
             contains=list("Design"))
validObject(.DualDesign())


##' Initialization function for "DualDesign"
##'
##' @param model see \code{\linkS4class{DualDesign}}
##' @param data see \code{\linkS4class{DualDesign}}
##' @param \dots additional arguments for \code{\link{Design}}
##' @return the \code{\linkS4class{DualDesign}} object
##'
##' @export
##' @keywords methods
DualDesign <- function(model,
                       data,
                       ...)
{
    start <- Design(...)
    .DualDesign(start,
                model=model,
                data=data)
}




##' Creates a new 3+3 design object from a dose grid
##'
##' @param doseGrid the dose grid to be used
##' @return the object of class \code{\linkS4class{RuleDesign}} with the
##' 3+3 design
##'
##' @export
##' @keywords programming
##' @author Daniel Sabanes Bove \email{sabanesd@@roche.com}
ThreePlusThreeDesign <- function(doseGrid)
{
    emptydata <- Data(doseGrid=doseGrid)

    design <- RuleDesign(nextBest=NextBestThreePlusThree(),
                         data=emptydata,
                         cohortSize=CohortSizeConst(size=3L),
                         ## using a constant cohort size of 3,
                         ## we obtain exactly the 3+3 design
                         startingDose=head(emptydata@doseGrid, 1))

    return(design)
}
