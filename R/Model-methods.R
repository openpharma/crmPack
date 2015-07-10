#####################################################################################
## Author: Daniel Sabanes Bove, Wai Yin Yeung [sabanesd *a*t* roche *.* com, w *.* yeung1 *a*t* lancaster *.* ac *.* uk]
## Project: Object-oriented implementation of CRM designs
##
## Time-stamp: <[Model-methods.R] by DSB Mon 11/05/2015 17:45>
##
## Description:
## Encapsulate the model input in a formal class.
##
## History:
## 31/03/2014   file creation
## 09/07/2015   Adding more methods for Pseudo Models
######################################################################################

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

## =================================================================
## -------------------------------------------------------------------------------------
## Compute the doses for a given probability, given Pseudo DLE model and given samples
## ----------------------------------------------------------------------------- 
##' Compute the doses for a given probability, given Pseudo DLE model with samples 
##' 
##' @param prob the probability
##' @param model the \code{\linkS4class{ModelTox}}
##' @param samples the \code{\linkS4class{Samples}}
##' @param \dots unused
##'
##' @export
##' @keywords methods
setMethod("dose",
          signature=
            signature(prob="numeric",
                      model="ModelTox",
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

## =========================================================================
## ----------------------------------------------------------------------------
## Compute the dose for a given Pseudo DLE model and a given probability
## -----------------------------------------------------------------------
##' Compute the dose for a given probability and a given Pseudo DLE model without samples
##' @param prob the probability
##' @param model the \code{\linkS4class{ModelTox}}
##' @param \dots unused
##'
##' @export
##' @keywords methods
setMethod("dose",
          signature=
            signature(prob="numeric",
                      model="ModelTox"),
          def=
            function(prob, model, ...){
              ## extract the dose function from the model
              doseFun <- slot(model, "dose")
              ## which arguments, besides the prob, does it need?
              argNames <- setdiff(names(formals(doseFun)),
                                  "prob")
              values<-c()
              for (parName in argNames){
                values<-c(values, slot(model,parName))}
              ## now call the function with prob
              ret <- do.call(doseFun,
                             c(list(prob=prob), values))
              ## return the resulting vector
              return(ret)
            })


## =======================================================================================

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

## =============================================================================================
## Compute the probability for a given dose, given Pseudo DLE model and samples
## --------------------------------------------------

##' Compute the probability for a given dose, given Pseudo DLE model and samples
##'
##' @param dose the dose
##' @param model the \code{\linkS4class{ModelTox}} object
##' @param samples the \code{\linkS4class{Samples}}
##' @param \dots unused
##' @return the vector (for \code{\linkS4class{ModelTox}} objects) of probability
##' samples.
##'
##' @export
##' @keywords methods
setMethod("prob",
          signature=
            signature(dose="numeric",
                      model="ModelTox",
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




## ==============================================================================
## ## Compute the probability for a given dose, given Pseudo DLE model
## --------------------------------------------------

##' Compute the probability for a given dose, given Pseudo DLE model without samples
##' @param dose the dose
##' @param model the \code{\linkS4class{ModelTox}} object
##' @param \dots unused
##' @return the vector (for \code{\linkS4class{ModelTox}} objects) of probability
##' samples.
##'
##' @export
##' @keywords methods
setMethod("prob",
          signature=
            signature(dose="numeric",
                      model="ModelTox"),
          def=
            function(dose,model,...){
              ## extract the prob function from the model
              probFun <- slot(model, "prob")
              ## which arguments, besides the dose, does it need?
              argNames <- setdiff(names(formals(probFun)),
                                  "dose")
              ## now call the function with dose
              values<-c()
              for (parName in argNames){
                values<-c(values, slot(model,parName))}
              ret <- do.call(probFun,
                             c(list(dose=dose), values))
              ## return the resulting vector
              return(ret)
            })

## =============================================================================

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
## ==========================================================================================
## -----------------------------------------------------------------------------------------
## Extracting efficacy responses for subjects without DLE observed
## ---------------------------------------------------------------------------------

##' Extracting efficay responses for subjects without an DLE
##' @param object for data input from \code\{\linkS4class{Data}} object 
##' 
##' @export
##' @keywords

setGeneric("getEff",
def=function(object,...){
  standardGeneric("getEff")},
valueClass="list")

##' @describeIn getEff
setMethod("getEff",
          signature=
            signature(object="DataDual"),
          def=
            function(object,
                     x,
                     y,
                     w,...){
              if (length(which(object@y == 1))==0){
                wNoDLE<-object@w
                wDLE<-NULL
                xNoDLE<- object@x
                xDLE<-NULL
              } else {##with observed efficacy response and DLE observed
                IndexDLE<-which(object@y==1)
                ##Take efficacy responses with no DLE observed
                wNoDLE<-object@w[-IndexDLE]
                wDLE<-object@w[IndexDLE]
                ##Take the corresponding dose levels
                xNoDLE<-object@x[-IndexDLE]
                xDLE<-object@x[IndexDLE]
              }
              ret<-list(wDLE=wDLE,xDLE=xDLE,wNoDLE=wNoDLE,xNoDLE=xNoDLE)
              return(ret)
            })
## =============================================================================================
## ---------------------------------------------------------------------------------------------
## Compute the Expected Efficacy based on a given dose, a given pseduo Efficacy model and a given sample
## -----------------------------------------------------------------------------
##' Compute the expected efficacy based on a given dose, a given pseudo Efficacy model and a given sample
##' 
##' @param dose the dose
##' @param model the \code{\linkS4class{ModelEff}} class object
##' @param samples the \code{\linkS4class{Samples}} class object
##' 
##' @export
##' @keywords methods
setGeneric("ExpEff",
           def=
             function(dose,model,samples,...){
               standardGeneric("ExpEff")
             },
           valueClass="numeric")

##' @describeIn ExpEff

setMethod("ExpEff",
          signature=
            signature(dose="numeric",
                      model="ModelEff",
                      samples="Samples"),
          def=
            function(dose, model, samples, ...){
              ## extract the ExpEff function from the model
              EffFun <- slot(model, "ExpEff")
              ## which arguments, besides the dose, does it need?
              argNames <- setdiff(names(formals(EffFun)),
                                  "dose")
              ## now call the function with dose and with
              ## the arguments taken from the samples
              ret <- do.call(EffFun,
                             c(list(dose=dose),
                               samples@data[argNames]))
              ## return the resulting vector
              return(ret)
            })
##======================================================================================

## -------------------------------------------------------------------------------------
## Compute the Expected Efficacy based on a given dose and a given Pseudo Efficacy model
## --------------------------------------------------------------------------------------
##' Compute the Expected Efficacy based a given dose and a given Pseudo Efficacy model without
##' samples
##' @param dose the dose
##' @param model the \code{\linkS4class{ModelEff}} class object
##' 
##' @export
##' @keywords methods
setMethod("ExpEff",
          signature=
            signature(dose="numeric",
                      model="ModelEff"),
          def=
            function(dose, model, ...){
              ## extract the ExpEff function from the model
              EffFun <- slot(model, "ExpEff")
              ## which arguments, besides the dose, does it need?
              argNames <- setdiff(names(formals(EffFun)),
                                  "dose")
              ## now call the function with dose
              values<-c()
              for (parName in argNames){
                values <- c(values, slot(model,parName))}
              
              ret <- do.call(EffFun,
                             c(list(dose=dose),values))
              ## return the resulting vector
              return(ret)
            })
## ======================================================================

## -------------------------------------------------------------------------
## Compute the dose for a given Expected Efficacy and a given Pseudo Efficacy model 
## -------------------------------------------------------------------------------------
##' Compute the dose for a given Expected Efficacy and a given Pseudo Efficacy model 
##' 
##' @param ExpEff the Expected Efficacy value
##' @param model the \code{\linkS4class{ModelEff}} class object

##' 
##' @export
##' @keywords methods

setGeneric("doseforEff",
           def=
             function(ExpEff,model,...){
               standardGeneric("doseforEff")
             },
           valueClass="numeric")
##' @describeIn doseforEff


