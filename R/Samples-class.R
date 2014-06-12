#####################################################################################
## Author: Daniel Sabanes Bove [sabanesd *a*t* roche *.* com]
## Project: Object-oriented implementation of CRM designs
##
## Time-stamp: <[Samples-class.R] by DSB Fre 11/04/2014 16:54>
##
## Description:
## A class for the MCMC samples. We need to have something slightly more
## flexible than the "mcmc" class from the "coda" package
##
## History:
## 25/03/2014   file creation
#####################################################################################

##' @include McmcOptions-class.R
##' @include McmcOptions-methods.R
{}

## --------------------------------------------------
## Class for the MCMC output
## --------------------------------------------------

##' Class for the MCMC output
##'
##' @slot data a list where each entry contains the samples of a (vector-valued)
##' parameter in a vector/matrix in the format (number of samples) x (dimension
##' of the parameter).
##' @slot options the \code{\linkS4class{McmcOptions}} which have been used
##'
##' @export
##' @keywords classes
setClass(Class="Samples",
         representation=
         representation(data="list",
                        options="McmcOptions"),
         validity=
         function(object){
             stopifnot(all(sapply(object@data,
                                  NROW) == sampleSize(object@options)))
         })


