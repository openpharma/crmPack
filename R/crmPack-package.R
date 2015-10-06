#####################################################################################
## Author: Daniel Sabanes Bove [sabanesd *a*t* roche *.* com]
## Project: Object-oriented implementation of CRM designs
##
## Time-stamp: <[crmPack-package.R] by DSB Mon 11/05/2015 17:47>
##
## Description:
## Package description.
##
## History:
## 29/01/2014   file creation
#####################################################################################

##' Object-oriented implementation of CRM designs
##'
##' @name crmPack-package
##' @aliases crmPack
##' @docType package
##' @title Object-oriented implementation of CRM designs
##' @author Daniel Sabanes Bove \email{sabanesd@@roche.com}
## @useDynLib crmPack
##  cpp_glmBayesMfp cpp_bfgs cpp_optimize cpp_sampleGlm cpp_evalZdensity
##  cpp_coxfit
##' @importFrom graphics plot hist
##' @importFrom methods setClass setOldClass setGeneric setMethod representation
##' signature prototype initialize new is
##' @keywords package
{}

##' @keywords internal
.onAttach <- function(libname, pkgname)
{
    packageStartupMessage(
        "Type crmPackHelp() to open help browser\n",
        "Type crmPackExample() to open example\n")
}

## need to declare global variable / function
## names in order to avoid R CMD check notes:
globalVariables(c("log.betaZ",
                  "precW",
                  "pow",
                  "nObs",
                  "betaZ",
                  "x",
                  "betaW",
                  "xLevel",
                  "precW",
                  "z",
                  "nGrid",
                  "doseGrid",
                  "betaWintercept",
                  "delta",
                  "deltaStart",
                  "delta2",
                  "Effsamples",
                  "logit<-",
                  "rho0",
                  "alpha0",
                  "alpha1",
                  "inverse",
                  "priorCov",
                  "theta",
                  "comp0",
                  "w",
                  "DLTs",
                  "y",
                  "group",
                  "annotate",
                  "probSamples",
                  "prec",
                  "nu",
                  "samples",
                  "Type",
                  "patient",
                  "toxicity",
                  "ID",
                  "biomarker",
                  "traj",
                  "Statistic",
                  "perc",
                  "..density..",
                  "middle",
                  "lower",
                  "upper",
                  "middleBiomarker",
                  "lowerBiomarker",
                  "upperBiomarker"))
