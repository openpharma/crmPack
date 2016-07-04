#####################################################################################
## Author: Daniel Sabanes Bove [sabanesd *a*t* roche *.* com]
##         Wai Yin Yeung [w *.* yeung1 *a*t* lancaster *.* ac *.* uk]
## Project: Object-oriented implementation of CRM designs
##
## Time-stamp: <[Simulations-class.R] by DSB Sam 17/01/2015 19:42>
##
## Description:
## Encapsulate the simulations output in a formal class.
##
## History:
## 12/02/2014   file creation
## 10/07/2015   added Pseudo simulation class
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
##' @slot placebo set to TRUE (default is FALSE) for a design with placebo
##'
##' @export
##' @keywords classes
.GeneralSimulationsSummary <-
    setClass(Class="GeneralSimulationsSummary",
             representation(target="numeric",
                            targetDoseInterval="numeric",
                            nsim="integer",
                            propDLTs="ANY",
                            meanToxRisk="numeric",
                            doseSelected="numeric",
                            toxAtDosesSelected="numeric",
                            propAtTarget="numeric",
                            doseMostSelected="numeric",
                            obsToxRateAtDoseMostSelected="numeric",
                            nObs="ANY",
                            nAboveTarget="integer",
                            doseGrid="numeric",
                            placebo="logical"))


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
## ==============================================================================

## -------------------------------------------------------------------------------
## class for simulation using pseudo models
## ------------------------------------------------------------------------
##' This is a class which captures the trial simulations from designs using 
##' pseudo model. The design for DLE only responses and model from \code{\linkS4class{ModelTox}}
##' class object. It contains all slots from \code{\linkS4class{GeneralSimulations}} object.
##' Additional slots fit and stopReasons compared to the general class 
##' \code{\linkS4class{GeneralSimulations}}.
##' 
##' @slot fit list of the final values. If samples are involved, these are the final fitted values.
##' If no samples are involved, these are included the final modal estimates of the model parameters
##' and the posterior estimates of the probabilities of the occurrence of a DLE.
##' @slot FinalTDtargetDuringTrialEstimates the vector of all final estimates (the last estimate of) the TDtargetDuringTrial at the end 
##' of each simultaions/when each trial stops
##' @slot FinalTDtargetEndOfTrialEstimates vector of all final estimates or the last estimate of the TDtargetEndOfTrial when each trial 
##' stops
##' @slot FinalTDtargetDuringTrialAtDoseGrid vector of the dose levels at dose grid closest below the final TDtargetDuringTrial estimates
##' @slot FinalTDtargetEndOfTrialAtDoseGrid vector of  the dose levels at dose grid closest below the final TDtargetEndOfTrial estimates
##' @slot FinalTDEOTCIs is the list of all 95\% credibility interval of the final estimates of the TDtargetEndOfTrial 
##' @slot FinalTDEOTRatios is the vector of the ratios of the CI, the raatio of the upper to the lower 95\% credibility intervals 
##' of the final estimates of the TDtargetEndOfTrial
##' @slot FinalCIs list of all the final 95\% credibility intervals of the TDtargetEndofTrial estimates or of the final optimal dose 
##' estimates when DLE and efficacy responses are incorporated after each simulations
##' @slot FinalRatios vector of all the final ratios, the ratios of the upper to the lower 95\% credibility interval of the 
##' final estimates of the TDtargetEndOfTrial or of the final optimal dose estiamtes (when DLE and efficacy responses are
##' incorporated) after each simulations
##' @slot stopReasons todo: add slot description 
##' 
##' @export
##' @keywords class
.PseudoSimulations <-
  setClass(Class="PseudoSimulations",
           representation(fit="list",
                          FinalTDtargetDuringTrialEstimates="numeric",
                          FinalTDtargetEndOfTrialEstimates = "numeric",
                          FinalTDtargetDuringTrialAtDoseGrid="numeric",
                          FinalTDtargetEndOfTrialAtDoseGrid ="numeric",
                          FinalTDEOTCIs="list",
                          FinalTDEOTRatios="numeric",
                          FinalCIs="list",
                          FinalRatios="numeric",
                          stopReasons="list"),
           ## note: this prototype is put together with the prototype
           ## for GeneralSimulations
           prototype(FinalTDtargetDuringTrialEstimates= c(0.1,0.1),
                     FinalTDtargetEndOfTrialEstimates=c(0.1,0.1),
                     FinalTDtargetDuringTrialAtDoseGrid=c(0.1,0.1),
                     FinalTDtargetEndOfTrialAtDoseGrid=c(0.1,0.1),
                     FinalTDEOTCIs=list(c(0.1, 0.2),c(0.1, 0.2)),
                     FinalTDEOTRatios=c(0.1,0.1),
                     FinalCIs=list(c(0.1, 0.2),c(0.1, 0.2)),
                     FinalRatios=c(0.1,0.1),
             stopReasons=
                       list("A", "A")),
           contains="GeneralSimulations",
           validity=
             function(object){
               o <- Validate()
               
               nSims <- length(object@data)
               o$check(identical(length(object@stopReasons), nSims),
                       "stopReasons must have same length as data")
               
               o$result()
             })
validObject(.PseudoSimulations())

##' Initialization function of the 'PseudoSimulations' class
##' @param fit please refer to \code{\linkS4class{PseudoSimulations}} class object
##' @param FinalTDtargetDuringTrialEstimates please refer to \code{\linkS4class{PseudoSimulations}} class object
##' @param FinalTDtargetEndOfTrialEstimates please refer to \code{\linkS4class{PseudoSimulations}} class object
##' @param FinalTDtargetDuringTrialAtDoseGrid please refer to \code{\linkS4class{PseudoSimulations}} class object
##' @param FinalTDtargetEndOfTrialAtDoseGrid please refer to \code{\linkS4class{PseudoSimulations}} class object
##' @param FinalTDEOTCIs please refer to \code{\linkS4class{PseudoSimulations}} class object
##' @param FinalTDEOTRatios please refer to \code{\linkS4class{PseudoSimulations}} class object
##' @param FinalCIs please refer to \code{\linkS4class{PseudoSimulations}} class object
##' @param FinalRatios please refer to \code{\linkS4class{PseudoSimulations}} class object
##' @param stopReasons please refer to \code{\linkS4class{PseudoSimulations}} class object
##' @param \dots additional parameters from \code{\linkS4class{GeneralSimulations}}
##' @return the \code{\linkS4class{PseudoSimulations}} object
##' 
##' @export
##' @keywords methods
PseudoSimulations <- function(fit,
                              FinalTDtargetDuringTrialEstimates,
                              FinalTDtargetEndOfTrialEstimates,
                              FinalTDtargetDuringTrialAtDoseGrid,
                              FinalTDtargetEndOfTrialAtDoseGrid,
                              FinalTDEOTCIs,
                              FinalTDEOTRatios,
                              FinalCIs,
                              FinalRatios,
                              stopReasons,
                              ...)
{
  start <- GeneralSimulations(...)
  .PseudoSimulations(start,
                     fit=fit,
                     FinalTDtargetDuringTrialEstimates=FinalTDtargetDuringTrialEstimates,
                     FinalTDtargetEndOfTrialEstimates=FinalTDtargetEndOfTrialEstimates,
                     FinalTDtargetDuringTrialAtDoseGrid=FinalTDtargetDuringTrialAtDoseGrid,
                     FinalTDtargetEndOfTrialAtDoseGrid= FinalTDtargetEndOfTrialAtDoseGrid,
                     FinalTDEOTCIs=FinalTDEOTCIs,
                     FinalTDEOTRatios=FinalTDEOTRatios,
                     FinalCIs=FinalCIs,
                     FinalRatios=FinalRatios,
                     stopReasons=stopReasons)
}

## ===============================================================================
## -------------------------------------------------------------------------------
## Class for Pseudo simulation using DLE and efficacy responses (Pseudo models except 'EffFlexi' model)
## -----------------------------------------------------------------------------------
##' This is a class which captures the trial simulations design using both the
##' DLE and efficacy responses. The design of model from \code{\linkS4class{ModelTox}}
##' class and the efficacy model from \code{\linkS4class{ModelEff}} class 
##' (except \code{\linkS4class{EffFlexi}} class). It contains all slots from 
##' \code{\linkS4class{GeneralSimulations}} and \code{\linkS4class{PseudoSimulations}} object.
##' In comparison to the parent class \code{\linkS4class{PseudoSimulations}}, 
##' it contains additional slots to 
##' capture the dose-efficacy curve and the sigma2 estimates.
##' 
##' @slot fitEff list of the final values. If DLE and efficacy samples are gerneated, it contains the
##' final fitted values. If no DLE and efficacy samples are used, it contains the modal estimates of the
##' parameters in the two models and the posterior estimates of the probabilities of the occurrence of a
##' DLE and the expected efficacy responses.
##' @slot FinalGstarEstimates a vector of the final estimates of Gstar at the end of each simulations.
##' @slot FinalGstarAtDoseGrid is a vectorof the final estimates of Gstar at dose Grid at the end of each simulations
##' @slot FinalGstarCIs is the list of all 95\% credibility interval of the final estimates of Gstar 
##' @slot FinalGstarRatios is the vector of the ratios of the CI, the ratio of the upper to the lower 95\% credibility interval
##' of the final estimates of Gstar
##' @slot FinalOptimalDose is the vector of the final optimal dose, the minimum of the final TDtargetEndOfTrial estimates and Gstar
##' estimates
##' @slot FinalOptimalDoseAtDoseGrid is the vector of the final optimal dose, the minimum of the final TDtargetEndOfTrial estimates 
##' and Gstar estimates at dose Grid
##' @slot sigma2est the vector of the final posterior mean sigma2 estimates
##' 
##' @export
##' @keywords class
.PseudoDualSimulations <- 
  setClass(Class="PseudoDualSimulations",
           representation(fitEff="list",
                          FinalGstarEstimates="numeric",
                          FinalGstarAtDoseGrid="numeric",
                          FinalGstarCIs="list",
                          FinalGstarRatios="numeric",
                          FinalOptimalDose="numeric",
                          FinalOptimalDoseAtDoseGrid="numeric",
                          sigma2est="numeric"),
           prototype( FinalGstarEstimates=c(0.1,0.1),
                      FinalGstarAtDoseGrid=c(0.1,0.1),
                      FinalGstarCIs=list(c(0.1, 0.2),
                      c(0.1, 0.2)),
                      FinalGstarRatios=c(0.01,0.01),
                      FinalOptimalDose=c(0.01,0.01),
                      FinalOptimalDoseAtDoseGrid=c(0.01,0.01),
             sigma2est= c(0.001,0.002)),
           contains="PseudoSimulations",
           validity=
             function(object){
               o <- Validate()
               nSims <- length(object@data)
               o$check(identical(length(object@sigma2est),nSims),
                       "sigma2est has to have same length as data")
               o$result()
             })

validObject(.PseudoDualSimulations())

##' Initialization function for 'DualPseudoSimulations' class
##' @param fitEff please refer to \code{\linkS4class{PseudoDualSimulations}} class object
##' @param  FinalGstarEstimates please refer to \code{\linkS4class{PseudoDualSimulations}} class object
##' @param FinalGstarAtDoseGrid please refer to \code{\linkS4class{PseudoDualSimulations}} class object
##' @param FinalGstarCIs please refer to \code{\linkS4class{PseudoDualSimulations}} class object
##' @param FinalGstarRatios please refer to \code{\linkS4class{PseudoDualSimulations}} class object
##' @param FinalOptimalDose please refer to \code{\linkS4class{PseudoDualSimulations}} class object
##' @param FinalOptimalDoseAtDoseGrid please refer to \code{\linkS4class{PseudoDualSimulations}} class object
##' @param sigma2est please refer to \code{\linkS4class{PseudoDualSimulations}} class object
##' @param \dots additional parameters from \code{\linkS4class{PseudoSimulations}}
##' @return the \code{\linkS4class{PseudoDualSimulations}} object
PseudoDualSimulations <- function(fitEff,
                                  FinalGstarEstimates,
                                  FinalGstarAtDoseGrid,
                                  FinalGstarCIs,
                                  FinalGstarRatios,
                                  FinalOptimalDose,
                                  FinalOptimalDoseAtDoseGrid,
                                  sigma2est,
                                  ...)
{ 
  start <- PseudoSimulations(...)
  .PseudoDualSimulations(start,
                         fitEff=fitEff,
                         FinalGstarEstimates=FinalGstarEstimates,
                         FinalGstarAtDoseGrid=FinalGstarAtDoseGrid,
                         FinalGstarCIs=FinalGstarCIs,
                         FinalGstarRatios=FinalGstarRatios,
                         FinalOptimalDose=FinalOptimalDose,
                         FinalOptimalDoseAtDoseGrid=FinalOptimalDoseAtDoseGrid,
                         sigma2est=sigma2est)
}


## -------------------------------------------------------------------------------
## Class for Pseudo simulation using DLE and efficacy responses using 'EffFlex' efficacy model
## -----------------------------------------------------------------------------------
##' This is a class which captures the trial simulations design using both the
##' DLE and efficacy responses. The design of model from \code{\linkS4class{ModelTox}}
##' class and the efficacy model from \code{\linkS4class{EffFlexi}} class 
##'  It contains all slots from 
##' \code{\linkS4class{GeneralSimulations}}, \code{\linkS4class{PseudoSimulations}} 
##' and \code{\linkS4class{PseudoDualSimulations}} object.
##' In comparison to the parent class \code{\linkS4class{PseudoDualSimulations}}, 
##' it contains additional slots to 
##' capture the sigma2betaW estimates.
##' 
##' @slot sigma2betaWest the vector of the final posterior mean sigma2betaW estimates
##' 
##' @export
##' @keywords class
.PseudoDualFlexiSimulations <- 
  setClass(Class="PseudoDualFlexiSimulations",
           representation(sigma2betaWest="numeric"),
           prototype(sigma2betaWest= c(0.001,0.002)),
           contains="PseudoDualSimulations",
           validity=
             function(object){
               o <- Validate()
               nSims <- length(object@data)
               o$check(identical(length(object@sigma2betaWest),nSims),
                       "sigma2betaWest has to have same length as data")
               o$result()
             })

validObject(.PseudoDualFlexiSimulations())

##' Initialization function for 'PseudoDualFlexiSimulations' class
##' @param sigma2betaWest please refer to \code{\linkS4class{PseudoDualFlexiSimulations}} class object
##' @param \dots additional parameters from \code{\linkS4class{PseudoDualSimulations}}
##' @return the \code{\linkS4class{PseudoDualFlexiSimulations}} object
PseudoDualFlexiSimulations <- function(sigma2betaWest,
                                       ...)
{ 
  start <- PseudoDualSimulations(...)
  .PseudoDualFlexiSimulations(start,
                              sigma2betaWest=sigma2betaWest)
}

## -------------------------------------------------------------------------------------------------------
## ================================================================================================

##' Class for the summary of pseudo-models simulations output
##'
##' Note that objects should not be created by users, therefore no
##' initialization function is provided for this class.
##'
##' @slot targetEndOfTrial the target probability of DLE wanted at the end of a trial
##' @slot targetDoseEndOfTrial the dose level corresponds to the target probability
##' of DLE wanted at the end of a trial, TDEOT
##' @slot targetDoseEndOfTrialAtDoseGrid the dose level at dose grid corresponds to the target probability
##' of DLE wanted at the end of a trial
##' @slot targetDuringTrial the target probability of DLE wanted during a trial
##' @slot targetDoseDuringTrial the dose level corresponds to the target probability of DLE
##' wanted during the trial. TDDT
##' @slot targetDoseDuringTrialAtDoseGrid the dose level at dose grid corresponds to the target probability
##' of DLE wanted during a trial
##' @slot TDEOTSummary the six-number table summary, include the lowest, the 25th precentile (lower quatile), 
##' the 50th percentile (median), the mean, the 27th percentile and the highest values of the 
##' final dose levels obtained corresponds to the target probability of DLE
##' want at the end of a trial across all simulations
##' @slot TDDTSummary the six-number table summary, include the lowest, the 25th precentile (lower quatile), 
##' the 50th percentile (median), the mean, the 27th percentile and the highest values of the 
##' final dose levels obtained corresponds to the target probability of DLE
##' want during a trial across all simulations
##' @slot FinalDoseRecSummary the six-number table summary, include the lowest, the 25th precentile (lower quatile), 
##' the 50th percentile (median), the mean, the 27th percentile and the highest values of the 
##' final optimal doses, which is either the TDEOT when only DLE response are incorporated into 
##' the escalation procedure or the minimum of the TDEOT and Gstar when DLE and efficacy responses are
##' incorporated, across all simulations
##' @slot ratioTDEOTSummary the six-number summary table of the final ratios of the upper to the lower 95\%
##' credibility intervals of the final TDEOTs across all simulations
##' @slot FinalRatioSummary the six-number summary table of the final ratios of the upper to the lower 95\%
##' credibility intervals of the final optimal doses across all simulations
##' #@slot doseRec the dose level that will be recommend for subsequent study
##' @slot nsim number of simulations
##' @slot propDLE proportions of DLE in the trials
##' @slot meanToxRisk mean toxicity risks for the patients
##' @slot doseSelected doses selected as MTD (targetDoseEndOfTrial)
##' @slot toxAtDosesSelected true toxicity at doses selected
##' @slot propAtTargetEndOfTrial Proportion of trials selecting at the doseGrid closest below the MTD, the 
##' targetDoseEndOfTrial
##' @slot propAtTargetDuringTrial Proportion of trials selecting at the doseGrid closest below the 
##' targetDoseDuringTrial
##' @slot doseMostSelected dose most often selected as MTD
##' @slot obsToxRateAtDoseMostSelected observed toxicity rate at dose most often
##' selected
##' @slot nObs number of patients overall
##' @slot nAboveTargetEndOfTrial number of patients treated above targetDoseEndOfTrial
##' @slot nAboveTargetDuringTrial number of patients treated above targetDoseDuringTrial
##' @slot doseGrid the dose grid that has been used
##' @slot fitAtDoseMostSelected fitted toxicity rate at dose most often selected
##' @slot meanFit list with the average, lower (2.5%) and upper (97.5%)
##' quantiles of the mean fitted toxicity at each dose level
##' 
##' 
##' @export
##' @keywords classes
.PseudoSimulationsSummary <- 
  setClass(Class="PseudoSimulationsSummary",
           representation(targetEndOfTrial="numeric",
                          targetDoseEndOfTrial="numeric",
                          targetDoseEndOfTrialAtDoseGrid="numeric",
                          targetDuringTrial="numeric",
                          targetDoseDuringTrial="numeric",
                          targetDoseDuringTrialAtDoseGrid="numeric",
                          TDEOTSummary="table",
                          TDDTSummary="table",
                          FinalDoseRecSummary="table",
                          ratioTDEOTSummary="table",
                          FinalRatioSummary="table",
                          #doseRec="numeric",
                          nsim="integer",
                          propDLE="numeric",
                          meanToxRisk="numeric",
                          doseSelected="numeric",
                          toxAtDosesSelected="numeric",
                          propAtTargetEndOfTrial="numeric",
                          propAtTargetDuringTrial="numeric",
                          doseMostSelected="numeric",
                          obsToxRateAtDoseMostSelected="numeric",
                          nObs="integer",
                          nAboveTargetEndOfTrial="integer",
                          nAboveTargetDuringTrial="integer",
                          doseGrid="numeric",
                          fitAtDoseMostSelected="numeric",
                          meanFit="list"))
## ---------------------------------------------------------------------------------------------
##' Class for the summary of the dual responses simulations using pseudo models
##' 
##' It contains all slots from \code{\linkS4class{PseudoSimulationsSummary}} object. In addition to 
##' the slots in the parent class \code{\linkS4class{PseudoSimulationsSummary}}, it contains four 
##' more slots for the efficacy model fit information.
##' 
##' Note that objects should not be created by users, therefore no initialization function
##' is provided for this class.
##' 
##' @slot targetGstar the target dose level such that its gain value is at maximum
##' @slot targetGstarAtDoseGrid the dose level at dose Grid closest and below Gstar
##' @slot GstarSummary the six-number table summary (lowest, 25th, 50th (median), 75th percentile, mean 
##' and highest value) of the final Gstar values obtained across all simulations
##' @slot ratioGstarSummary the six-number summary table of the ratios of the upper to the lower 95\%
##' credibility intervals of the final Gstar across all simulations
##' @slot EffFitAtDoseMostSelected fitted expected mean efficacy value at dose most often
##' selected
##' @slot meanEffFit list with mean, lower (2.5%) and upper (97.5%) quantiles of the fitted expected 
##' efficacy value at each dose level.
##' 
##' @export
##' @keywords class
.PseudoDualSimulationsSummary <-
  setClass(Class="PseudoDualSimulationsSummary",
           contains="PseudoSimulationsSummary",
           representation=
             representation(targetGstar="numeric",
                            targetGstarAtDoseGrid="numeric",
                            GstarSummary="table",
                            ratioGstarSummary="table",
                            EffFitAtDoseMostSelected="numeric",
                            meanEffFit="list"))
