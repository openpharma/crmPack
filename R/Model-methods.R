#####################################################################################
## Author: Daniel Sabanes Bove[sabanesd *a*t* roche *.* com],
##         Wai Yin Yeung [w *.* yeung *a*t* lancaster *.* ac *.* uk]
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
## -------------------------------------------------------------------------------------
## Compute the Expected Efficacy based on a given dose, Efficacy Flexible model and the Efficacy samples
## --------------------------------------------------------------------------------------
##' Compute the Expected Efficacy based a given dose, Efficacy Flexible  model with
##' samples
##' @param dose the dose
##' @param model the \code{\linkS4class{ModelEff}} class object
##' @param samples the \code{\linkS4class{Samples}} class object
##' 
##' @export
##' @keywords methods

setMethod("ExpEff",
          signature=
            signature(dose="numeric",
                      model="EffFlexi",
                      samples="Samples"),
          def=
            function(dose, model, samples, ...){
              ## extract the ExpEff function from the model
              EffFun <- slot(model, "ExpEff")
              ret <- EffFun(dose)
              ## return the resulting vector
              return(ret)
            })


## -------------------------------------------------------------------------
## Compute the dose for a given Expected Efficacy and a given Pseudo Efficacy model 
## -------------------------------------------------------------------------------------
##' Compute the dose for a given Expected Efficacy and a given Pseudo Efficacy model 
##' 
##' @param ExpEff the Expected Efficacy value
##' @param model the \code{\linkS4class{ModelEff}} class object
##' @param sample the \code{\linkS4class{Samples}} class object
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
setMethod("doseforEff",
          signature=
            signature(ExpEff="numeric",
                      model="ModelEff"),
          def=
            function(ExpEff, model, ...){
              ## extract the dose function from the model
              doseFun <- slot(model, "dose")
              ## which arguments, besides the ExpEff, does it need?
              argNames <- setdiff(names(formals(doseFun)),
                                  "ExpEff")
              
              ## now call the function with dose 
              values<-c()
              for (parName in argNames){
                values <- c(values,slot(model,parName))}
              
              ret <- do.call(doseFun,
                             c(list(ExpEff=ExpEff),values))
              
              ## return the resulting vector
              return(ret)
            })


## ---------------------------------------------------------------------------------
## Compute gain value using a Pseudo DLE and a Efficacy model
## -------------------------------------------------------------------------------

##' Compute the gain value with a given dose level, given DLE model, given DLE sample, given Efficacy
##' model and a given Efficacy sample
##' 
##' @export
##' @keywords methods
setGeneric("gain",
           def=
             function(dose,DLEmodel,DLEsamples,Effmodel,Effsamples,...){
               standardGeneric("gain")
             },
           valueClass="numeric")

## ===================================
##' Compute the gain value samples given a dose level, a DLE model, a DLE sample, a Efficacy model
##' and a Efficacy sample
##' 
##' @export
##' @keywords methods
##' 
setMethod("gain",
          signature=
            signature(dose="numeric",
                      DLEmodel="ModelTox",
                      DLEsamples="Samples",
                      Effmodel="ModelEff",
                      Effsamples="Samples"),
          def=
            function(dose,DLEmodel,DLEsamples, Effmodel,Effsamples,...){
              
              
              
              ## extract the prob function from the model
              probFun <- slot(DLEmodel, "prob")
              ## which arguments, besides the dose, does it need?
              DLEargNames <- setdiff(names(formals(probFun)),
                                     "dose")
              ## now call the function with dose and with
              ## the arguments taken from the samples
              DLEret <- do.call(probFun,
                                c(list(dose=dose),
                                  DLEsamples@data[DLEargNames]))
              
              ## extract the ExpEff function from the model
              EffFun <- slot(Effmodel, "ExpEff")
              ## which arguments, besides the dose, does it need?
              EffargNames <- setdiff(names(formals(EffFun)),
                                     "dose")
              ## now call the function with dose and with
              ## the arguments taken from the samples
              Effret <- do.call(EffFun,
                                c(list(dose=dose),
                                  Effsamples@data[EffargNames]))
              
              ## return the resulting vector
              Gainret <- Effret/(1+(DLEret/(1-DLEret)))
              return(Gainret)
            })

## ================================================================
##' Compute the gain given a dose level, a given DLE model, a given DLE sample, 
##' the EffFlexi model and a given Efficacy sample
##' 
##' @export
##' @keywords methods
##'
setMethod("gain",
          signature=
            signature(dose="numeric",
                      DLEmodel="ModelTox",
                      DLEsamples="Samples",
                      Effmodel="EffFlexi",
                      Effsamples="Samples"),
          def=
            function(dose,DLEmodel,DLEsamples, Effmodel,Effsamples,...){
              
              
              
              ## extract the prob function from the model
              probFun <- slot(DLEmodel, "prob")
              ## which arguments, besides the dose, does it need?
              DLEargNames <- setdiff(names(formals(probFun)),
                                     "dose")
              ## now call the function with dose and with
              ## the arguments taken from the samples
              DLEret <- do.call(probFun,
                                c(list(dose=dose),
                                  DLEsamples@data[DLEargNames]))
              
              ## extract the ExpEff function from the model
              EffFun <- slot(Effmodel, "ExpEff")
              
              ## now call the function with dose and with
              ## the arguments taken from the samples
              Effret <- EffFun(dose)
              
              ## return the resulting vector
              Gainret <- Effret/(1+(DLEret/(1-DLEret)))
              return(Gainret)
            })
## ===================================================================================
##' Compute the gain given a dose level, a given DLE model and a given Efficacy model
##' 
##' @export
##' @keywords methods
##' 
setMethod("gain",
          signature=
            signature(dose="numeric",
                      DLEmodel="ModelTox",
                      Effmodel="ModelEff"),
          def=
            function(dose,DLEmodel,Effmodel,...){
              ##extract the prob function from the DLE model
              probFun <- slot(DLEmodel,"prob")
              ##which arguments besides the dose dose it need?
              DLEargNames <- setdiff(names(formals(probFun)),"dose")
              ##now call the function with dose
              DLEvalues<-c()
              for (DLEparName in DLEargNames){
                DLEvalues<-c(DLEvalues, slot(DLEmodel,DLEparName))}
              DLEret <- do.call(probFun,
                                c(list(dose=dose), DLEvalues))
              
              ##extract the ExpEff function from the Eff model
              EffFun <- slot(Effmodel,"ExpEff")
              ##which arguments besides the dose dose it need?
              EffargNames <- setdiff(names(formals(EffFun)),"dose")
              ##now call the function with dose 
              Effvalues<-c()
              for (EffparName in EffargNames){
                Effvalues <- c(Effvalues, slot(Effmodel,EffparName))}
              
              Effret <- do.call(EffFun,
                                c(list(dose=dose),Effvalues))
              Gainret <- Effret/(1+(DLEret/(1-DLEret)))
              return(Gainret)
            })


## ==================================================================================

## ---------------------------------------------------------------------------
## Update Pseduo models object to ontain new estimates for model parameters
## -------------------------------------------------------------------------------

##' Update method for the 'LogisticIndepBeta'Model class 
##' @param object is the model which follow \code{\linkS4Class{LogisticIndpeBeta}}} class object
##'
##'@export
##'@keywords method
setMethod("update",
          signature=
            signature(object="LogisticIndepBeta"),
          def=
            function(object,
                     data,
                     ...){
              ##Get Pseudo DLE responses (prior) of the model
              
              PseudoDLE<-object@binDLE
              
              ##Get Pseudo DLE weights of the DLE responses of the model
              PseudoDLEweight<-object@DLEweights
              
              
              ##Get the corresponding dose levels for the Pseudo DLE responses from the model
              PseudoDLEdose<- object@DLEdose
              
              ##update the model estimates with data
              model<- LogisticIndepBeta(binDLE=PseudoDLE,DLEweights=PseudoDLEweight,DLEdose=PseudoDLEdose,data=data)
              
              ##return the updated model
              return(model)
            })

## ================================================================================
##' Update method for the 'Effloglog'Model class 
##' @param object is the model which follow \code{\linkS4Class{Effloglog}}} class object
##' 
##' @export
##' @keywords methods
setMethod("update",
          signature=
            signature(object="Effloglog"),
          def=
            function(object,
                     data,
                     ...){
              ##Get Pseudo Eff responses (prior) of the model
              
              PseudoEff<-object@Eff
              
              ##Get the corresponding dose levels for the Pseudo DLE responses from the model
              PseudoEffdose<- object@Effdose
              
              ## Get the initial values of parameters for nu (if it is not fixed)
              ##OR get the fixed value of nu
              PseudoNu<- object@nu
              
              
              ##update the model estimates with data
              model<- Effloglog(Eff=PseudoEff,Effdose=PseudoEffdose,nu=PseudoNu,data=data)
              
              ##return the updated model
              return(model)
            })
## =================================================================================
