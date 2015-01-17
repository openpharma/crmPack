#####################################################################################
## Author: Daniel Sabanes Bove [sabanesd *a*t* roche *.* com]
## Project: Object-oriented implementation of CRM designs
##
## Time-stamp: <[Simulations-class.R] by DSB Sam 17/01/2015 19:42>
##
## Description:
## Encapsulate the simulations output in a formal class.
##
## History:
## 12/02/2014   file creation
###################################################################################

##' @include helpers.R
##' @include Data-class.R
{}


##' General class for the simulations output
##'
##' This class captures trial simulations.
##'
##' Here also the random generator state before starting the simulation is
##' saved, in order to be able to reproduce the outcome. For this just use
##' \code{\link{set.seed}} with the \code{seed} as argument before running
##' \code{\link{simulate,Design-method}}.
##'
##' @slot data list of produced \code{\linkS4class{Data}} objects
##' @slot doses the vector of final dose recommendations
##' @slot seed random generator state before starting the simulation
##'
##' @export
##' @keywords classes
.GeneralSimulations <-
    setClass(Class="GeneralSimulations",
             representation(data="list",
                            doses="numeric",
                            seed="integer"),
             prototype(data=
                           list(Data(x=1:2,
                                     y=0:1,
                                     doseGrid=1:2),
                                Data(x=3:4,
                                     y=0:1,
                                     doseGrid=3:4)),
                       doses=c(1, 2),
                       seed=1L),
             validity=
                 function(object){
                     o <- Validate()

                     nSims <- length(object@data)

                     o$check(all(sapply(object@data, is, "Data")),
                             "all data elements must be Data objects")
                     o$check(identical(length(object@doses), nSims),
                             "doses must have same length as the data list")

                     o$result()
                 })
validObject(.GeneralSimulations())

##' Initialization function for "GeneralSimulations"
##'
##' @param data see \code{\linkS4class{GeneralSimulations}}
##' @param doses see \code{\linkS4class{GeneralSimulations}}
##' @param seed see \code{\linkS4class{GeneralSimulations}}
##' @return the \code{\linkS4class{GeneralSimulations}} object
##'
##' @export
##' @keywords methods
GeneralSimulations <- function(data,
                               doses,
                               seed)
{
    .GeneralSimulations(data=data,
                        doses=doses,
                        seed=safeInteger(seed))
}



##' Class for the simulations output from model based designs
##'
##' This class captures the trial simulations from model based designs.
##' Additional slots fit and stopReasons compared to the general class
##' \code{\linkS4class{GeneralSimulations}}.
##'
##' @slot fit list with the final fits
##' @slot stopReasons list of stopping reasons for each simulation run
##'
##' @export
##' @keywords classes
.Simulations <-
    setClass(Class="Simulations",
             representation(fit="list",
                            stopReasons="list"),
             ## note: this prototype is put together with the prototype
             ## for GeneralSimulations
             prototype(fit=
                           list(c(0.1, 0.2),
                                c(0.1, 0.2)),
                       stopReasons=
                           list("A", "A")),
             contains="GeneralSimulations",
             validity=
                 function(object){
                     o <- Validate()

                     nSims <- length(object@data)

                     o$check(identical(length(object@fit), nSims),
                             "fit must have same length as data")
                     o$check(identical(length(object@stopReasons), nSims),
                             "stopReasons must have same length as data")

                     o$result()
                 })
validObject(.Simulations())


##' Initialization function for the "Simulations" class
##'
##' @param fit see \code{\linkS4class{Simulations}}
##' @param stopReasons see \code{\linkS4class{Simulations}}
##' @param \dots additional parameters from \code{\link{GeneralSimulations}}
##' @return the \code{\linkS4class{Simulations}} object
##' @export
##'
##' @keywords methods
Simulations <- function(fit,
                        stopReasons,
                        ...)
{
    start <- GeneralSimulations(...)
    .Simulations(start,
                 fit=fit,
                 stopReasons=stopReasons)
}


##' Class for the simulations output from dual-endpoint model based designs
##'
##' This class captures the trial simulations from dual-endpoint model based
##' designs. In comparison to the parent class \code{\linkS4class{Simulations}},
##' it contains additional slots to capture the dose-biomarker fits, and the
##' sigma2W and rho estimates.
##'
##' @slot rhoEst the vector of final posterior median rho estimates
##' @slot sigma2West the vector of final posterior median sigma2W estimates
##' @slot fitBiomarker list with the final dose-biomarker curve fits
##'
##' @export
##' @keywords classes
.DualSimulations <-
    setClass(Class="DualSimulations",
             representation(rhoEst="numeric",
                            sigma2West="numeric",
                            fitBiomarker="list"),
             prototype(rhoEst=c(0.2, 0.3),
                       sigma2West=c(0.2, 0.3),
                       fitBiomarker=
                           list(c(0.1, 0.2),
                                c(0.1, 0.2))),
             contains="Simulations",
             validity=
                 function(object){
                     o <- Validate()

                     nSims <- length(object@data)

                     o$check(identical(length(object@fitBiomarker), nSims),
                             "fitBiomarker list has to have same length as data")
                     o$check(identical(length(object@rhoEst), nSims),
                             "rhoEst vector has to have same length as data")
                     o$check(identical(length(object@sigma2West), nSims),
                             "sigma2West has to have same length as data")

                     o$result()
                 })
validObject(.DualSimulations())


##' Initialization function for "DualSimulations"
##'
##' @param rhoEst see \code{\linkS4class{DualSimulations}}
##' @param sigma2West see \code{\linkS4class{DualSimulations}}
##' @param fitBiomarker see \code{\linkS4class{DualSimulations}}
##' @param \dots additional parameters from \code{\link{Simulations}}
##' @return the \code{\linkS4class{DualSimulations}} object
##'
##' @export
##' @keywords methods
DualSimulations <- function(rhoEst,
                            sigma2West,
                            fitBiomarker,
                            ...)
{
    start <- Simulations(...)
    .DualSimulations(start,
                     rhoEst=rhoEst,
                     sigma2West=sigma2West,
                     fitBiomarker=fitBiomarker)
}


##' Class for the summary of general simulations output
##'
##' Note that objects should not be created by users, therefore no
##' initialization function is provided for this class.
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
##' @slot nObs number of patients overall
##' @slot nAboveTarget number of patients treated above target tox interval
##' @slot doseGrid the dose grid that has been used
##'
##' @export
##' @keywords classes
.GeneralSimulationsSummary <-
    setClass(Class="GeneralSimulationsSummary",
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
                            nObs="integer",
                            nAboveTarget="integer",
                            doseGrid="numeric"))


##' Class for the summary of model-based simulations output
##'
##' In addition to the slots in the parent class
##' \code{\linkS4class{GeneralSimulationsSummary}}, it contains two slots with
##' model fit information.
##'
##' Note that objects should not be created by users, therefore no
##' initialization function is provided for this class.
##'
##' @slot fitAtDoseMostSelected fitted toxicity rate at dose most often selected
##' @slot meanFit list with the average, lower (2.5%) and upper (97.5%)
##' quantiles of the mean fitted toxicity at each dose level
##'
##' @export
##' @keywords classes
.SimulationsSummary <-
    setClass(Class="SimulationsSummary",
             representation(fitAtDoseMostSelected="numeric",
                            meanFit="list"),
             contains="GeneralSimulationsSummary")


##' Class for the summary of dual-endpoint simulations output
##'
##' In addition to the slots in the parent class
##' \code{\linkS4class{SimulationsSummary}}, it contains two slots for the
##' biomarker model fit information.
##'
##' Note that objects should not be created by users, therefore no
##' initialization function is provided for this class.
##'
##' @slot biomarkerFitAtDoseMostSelected fitted biomarker level at dose most often selected
##' @slot meanBiomarkerFit list with the average, lower (2.5%) and upper (97.5%)
##' quantiles of the mean fitted biomarker level at each dose level
##'
##' @export
##' @keywords classes
.DualSimulationsSummary <-
    setClass(Class="DualSimulationsSummary",
             contains="SimulationsSummary",
             representation=
                 representation(biomarkerFitAtDoseMostSelected="numeric",
                                meanBiomarkerFit="list"))

