#####################################################################################
## Author: Daniel Sabanes Bove [sabanesd *a*t* roche *.* com]
## Project: Object-oriented implementation of CRM designs
##
## Time-stamp: <[Simulations-class.R] by DSB Die 29/04/2014 15:05>
##
## Description:
## Encapsulate the simulations output in a formal class.
##
## History:
## 12/02/2014   file creation
###################################################################################

##' @include helpers.R
{}


##' Class for the simulations output
##'
##' This class captures the trial simulations.
##'
##' Here also the random generator state before starting the simulation is
##' saved, in order to be able to reproduce the outcome. For this just use
##' \code{\link{set.seed}} with the \code{seed} as argument before running
##' \code{\link{simulate,Design-method}}.
##'
##' @slot data list of produced \code{\linkS4class{Data}} objects
##' @slot doses the vector of final dose recommendations
##' @slot fit list with the final fits
##' @slot stopReasons list of stopping reasons for each simulation run
##' @slot seed random generator state before starting the simulation
##'
##' @export
##' @keywords classes
setClass(Class="Simulations",
         representation=
         representation(data="list",
                        doses="numeric",
                        fit="list",
                        stopReasons="list",
                        seed="integer"),
         validity=
         function(object){
             nSims <- length(object@data)
             stopifnot(all(sapply(object@data, is, "Data")),
                       identical(length(object@doses), nSims),
                       identical(length(object@fit), nSims),
                       identical(length(object@stopReasons), nSims))
         })


##' Class for the summary of simulations output
##'
##' @slot target target toxicity interval
##' @slot targetDoseInterval corresponding target dose interval
##' @slot nsim number of simulations
##' @slot propDLTs proportions of DLTs in the trials
##' @slot meanToxRisk mean toxicity risks for the patients
##' @slot doseSelected doses selected as MTD
##' @slot toxAtDosesSelected true toxicity at doses selected
##' @slot propAtTarget Proportion of trials selecting target MTD
##' @slot doseMostSelected dose most often selected as MTD
##' @slot obsToxRateAtDoseMostSelected observed toxicity rate at dose most often
##' selected
##' @slot fitAtDoseMostSelected fitted toxicity rate at dose most often selected
##' @slot meanFit list with the average, lower (2.5%) and upper (97.5%)
##' quantiles of the mean fitted toxicity at each dose level
##' @slot nObs number of patients overall
##' @slot nAboveTarget number of patients treated above target tox interval
##' @slot doseGrid the dose grid that has been used
##'
##' @export
##' @keywords classes
setClass(Class="Simulations-summary",
         representation=
         representation(target="numeric",
                        targetDoseInterval="numeric",
                        nsim="integer",
                        propDLTs="numeric",
                        meanToxRisk="numeric",
                        doseSelected="numeric",
                        toxAtDosesSelected="numeric",
                        propAtTarget="numeric",
                        doseMostSelected="numeric",
                        obsToxRateAtDoseMostSelected="numeric",
                        fitAtDoseMostSelected="numeric",
                        meanFit="list",
                        nObs="integer",
                        nAboveTarget="integer",
                        doseGrid="numeric"))

