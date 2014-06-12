#####################################################################################
## Author: Daniel Sabanes Bove [sabanesd *a*t* roche *.* com]
## Project: Object-oriented implementation of CRM designs
##
## Time-stamp: <[McmcOptions-class.R] by DSB Fre 11/04/2014 17:07>
##
## Description:
## Encapsulate the three canonical MCMC options in a formal class.
##
## History:
## 12/12/2013   file creation (copy from glmBfp package)
## 19/12/2013   simplify doc (roxygen2 3.0)
## 29/01/2014   use initialize method rather than constructor function
##              to be better extensible
###################################################################################

##' Class for the three canonical MCMC options
##'
##' @slot iterations number of MCMC iterations
##' @slot burnin number of burn-in iterations which are not saved
##' @slot step only every step-th iteration is saved after the burn-in
##'
##' @export
##' @keywords classes
setClass(Class="McmcOptions",
         representation=
         representation(iterations="integer",
                        burnin="integer",
                        step="integer"),
         validity=function(object){
             if(object@burnin < 0L )
             {
                 return("Burn-in must be non-negative")
             }
             else if(object@burnin >= object@iterations)
             {
                 return("Burn-in must be smaller than iterations")
             }
             else if(object@step < 1)
             {
                 return("Step size must be at least 1")
             }
         })


##' Initialization method for the "McmcOptions" class
##'
##' @param .Object the \code{\linkS4class{McmcOptions}} we want to initialize
##' @param burnin number of burn-in iterations which are not saved (default:
##' \code{10,000})
##' @param step only every step-th iteration is saved after the burn-in
##' (default: \code{2})
##' @param samples number of resulting samples (by default \code{10,000} will
##' result)
##'
##' @export
##' @keywords methods
setMethod("initialize",
          signature(.Object = "McmcOptions"),
          function(.Object,
                   burnin=1e4L,
                   step=2L,
                   samples=1e4L,
                   ...){
              callNextMethod(.Object,
                             iterations=as.integer(burnin + (step * samples)),
                             burnin=as.integer(burnin),
                             step=as.integer(step),
                             ...)
          })
