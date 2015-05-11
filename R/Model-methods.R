#####################################################################################
## Author: Daniel Sabanes Bove [sabanesd *a*t* roche *.* com]
## Project: Object-oriented implementation of CRM designs
##
## Time-stamp: <[Model-methods.R] by DSB Mon 11/05/2015 17:45>
##
## Description:
## Encapsulate the model input in a formal class.
##
## History:
## 31/03/2014   file creation
###################################################################################

##' @include Model-class.R
##' @include Samples-class.R
{}

## --------------------------------------------------
## Compute the doses for a given probability, given model and samples
## --------------------------------------------------

##' Compute the doses for a given probability, given model and samples
##'
##' @param prob the probability
##' @param model the \code{\linkS4class{Model}}
##' @param samples the \code{\linkS4class{Samples}}
##' @param \dots unused
##'
##' @export
##' @keywords methods
setGeneric("dose",
           def=
           function(prob, model, samples, ...){
               ## there should be no default method,
               ## therefore just forward to next method!
               standardGeneric("dose")
           },
           valueClass="numeric")

##' @describeIn dose
setMethod("dose",
          signature=
          signature(prob="numeric",
                    model="Model",
                    samples="Samples"),
          def=
          function(prob, model, samples, ...){
              ## extract the dose function from the model
              doseFun <- slot(model, "dose")
              ## which arguments, besides the prob, does it need?
              argNames <- setdiff(names(formals(doseFun)),
                                  "prob")
              ## now call the function with prob and with
              ## the arguments taken from the samples
              ret <- do.call(doseFun,
                             c(list(prob=prob),
                               samples@data[argNames]))
              ## return the resulting vector
              return(ret)
          })


## --------------------------------------------------
## Compute the probability for a given dose, given model and samples
## --------------------------------------------------

##' Compute the probability for a given dose, given model and samples
##'
##' @param dose the dose
##' @param model the \code{\linkS4class{Model}} object
##' @param samples the \code{\linkS4class{Samples}}
##' @param \dots unused
##' @return the vector (for \code{\linkS4class{Model}} objects) of probability
##' samples.
##'
##' @export
##' @keywords methods
setGeneric("prob",
           def=
           function(dose, model, samples, ...){
               ## there should be no default method,
               ## therefore just forward to next method!
               standardGeneric("prob")
           })
## todo: simplify this to always vectorize internally over points -> always
## take/return matrix

##' @describeIn prob
setMethod("prob",
          signature=
          signature(dose="numeric",
                    model="Model",
                    samples="Samples"),
          def=
          function(dose, model, samples, ...){
              ## extract the prob function from the model
              probFun <- slot(model, "prob")
              ## which arguments, besides the dose, does it need?
              argNames <- setdiff(names(formals(probFun)),
                                  "dose")
              ## now call the function with dose and with
              ## the arguments taken from the samples
              ret <- do.call(probFun,
                             c(list(dose=dose),
                               samples@data[argNames]))
              ## return the resulting vector
              return(ret)
          })


## --------------------------------------------------
## Compute the biomarker level for a given dose, given model and samples
## --------------------------------------------------

##' Compute the biomarker level for a given dose, given model and samples
##'
##' @param dose the dose
##' @param model the \code{\linkS4class{DualEndpoint}} object
##' @param samples the \code{\linkS4class{Samples}} object
##' @param \dots unused
##'
##' @export
##' @keywords methods
setGeneric("biomLevel",
           def=
           function(dose, model, samples, ...){
               ## there should be no default method,
               ## therefore just forward to next method!
               standardGeneric("biomLevel")
           },
           valueClass="numeric")

##' @param xLevel the grid index of \code{dose}
##' @describeIn biomLevel Here it is very easy, we just return the corresponding
##' column (index \code{xLevel}) of the biomarker samples matrix, since we save
##' that in the samples
setMethod("biomLevel",
          signature=
          signature(dose="numeric",
                    model="DualEndpoint",
                    samples="Samples"),
          def=
          function(dose, model, samples, xLevel, ...){

              return(samples@data$betaW[, xLevel])

          })

