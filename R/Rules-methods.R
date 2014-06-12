#####################################################################################
## Author: Daniel Sabanes Bove [sabanesd *a*t* roche *.* com]
## Project: Object-oriented implementation of CRM designs
##
## Time-stamp: <[Rules-methods.R] by DSB Die 03/06/2014 16:25>
##
## Description:
## Encapsulate the rule functions in formal methods.
##
## History:
## 07/02/2014   file creation
###################################################################################

##' @include Model-methods.R
##' @include Samples-class.R
{}

## ============================================================

## --------------------------------------------------
## Find out what is the next best dose
## --------------------------------------------------


##' Find the next best dose
##'
##' Compute the recommended next best dose.
##'
##' This function outputs the next best dose recommendation based on the
##' corresponding rule \code{nextBest}, the posterior \code{samples} from the
##' \code{model} and the underlying \code{data}.
##'
##' @param nextBest The rule, an object of class \code{\linkS4class{NextBest}}
##' @param doselimit The maximum allowed next dose
##' @param samples the \code{\linkS4class{Samples}} object
##' @param model The model input, an object of class \code{\linkS4class{Model}}
##' @param data The data input, an object of class \code{\linkS4class{Data}}
##' @param \dots possible additional arguments without method dispatch
##' @return a list with the next best dose (element \code{value})
##' on the grid defined in \code{data}, and a plot depicting this recommendation
##' (element \code{plot})
##'
##' @genericMethods
##' @export
##' @keywords methods
setGeneric("nextBest",
           def=
           function(nextBest, doselimit, samples, model, data, ...){
               ## there should be no default method,
               ## therefore just forward to next method!
               standardGeneric("nextBest")
           },
           valueClass="list")

## --------------------------------------------------
## The MTD method
## --------------------------------------------------

##' Find the next best dose based on the MTD rule
##'
##' @importFrom ggplot2 ggplot geom_density xlab ylab xlim aes geom_vline
##' geom_text
setMethod("nextBest",
          signature=
          signature(nextBest="NextBestMTD",
                    doselimit="numeric",
                    samples="Samples",
                    model="Model",
                    data="Data"),
          def=
          function(nextBest, doselimit, samples, model, data, ...){
              ## First, generate the MTD samples.
              mtdSamples <- dose(prob=nextBest@target,
                                 model,
                                 samples)

              ## then derive the next best dose
              mtdEstimate <- nextBest@derive(mtdSamples=mtdSamples)

              ## be sure which doses are ok with respect to maximum
              ## possible dose
              dosesOK <- which(data@doseGrid <= doselimit)

              ## but now, round to the next possible grid point
              index <- which.min(abs(data@doseGrid[dosesOK] - mtdEstimate))
              ret <- data@doseGrid[dosesOK][index]

              ## produce plot
              plot1 <- ggplot() +
                      geom_density(data=
                                   data.frame(x=mtdSamples),
                                   aes(x=x),
                                   fill = "grey50", colour = "grey50") +
                          xlab("MTD") + ylab("Posterior density") +
                              xlim(range(data@doseGrid))

              plot1 <- plot1 +
                  geom_vline(xintercept=mtdEstimate, colour="black", lwd=1.1) +
                      geom_text(data=
                                data.frame(x=mtdEstimate),
                                aes(x, 0,
                                    label = "Est", hjust=+1, vjust = +1),
                                colour="black")

              plot1 <- plot1 +
                  geom_vline(xintercept=doselimit, colour="red", lwd=1.1) +
                      geom_text(data=
                                data.frame(x=doselimit),
                                aes(x, 0,
                                    label = "Max", hjust = +1, vjust = +1),
                                colour="red")

              plot1 <- plot1 +
                  geom_vline(xintercept=ret, colour="blue", lwd=1.1) +
                      geom_text(data=
                                data.frame(x=ret),
                                aes(x, 0,
                                    label = "Next", hjust = 0, vjust = +1),
                                colour="blue")

              ## return next best dose and plot
              return(list(value=ret,
                          plot=plot1))
          })

## --------------------------------------------------
## The NCRM method
## --------------------------------------------------

##' Find the next best dose based on the NCRM method
##'
##' @importFrom ggplot2 ggplot geom_bar xlab ylab ylim aes geom_vline
##' geom_hline geom_point
##' @importFrom gridExtra arrangeGrob
setMethod("nextBest",
          signature=
          signature(nextBest="NextBestNCRM",
                    doselimit="numeric",
                    samples="Samples",
                    model="Model",
                    data="Data"),
          def=
          function(nextBest, doselimit, samples, model, data, ...){
              ## first we have to get samples from the dose-tox
              ## curve at the dose grid points.
              probSamples <- matrix(nrow=sampleSize(samples@options),
                                    ncol=data@nGrid)

              ## evaluate the probs, for all samples.
              for(i in seq_len(data@nGrid))
              {
                  ## Now we want to evaluate for the
                  ## following dose:
                  probSamples[, i] <- prob(dose=data@doseGrid[i],
                                           model,
                                           samples)
              }

              ## Now compute probabilities to be in target and
              ## overdose tox interval
              probTarget <-
                  colMeans((probSamples > nextBest@target[1]) &
                           (probSamples < nextBest@target[2]))

              probOverdose <-
                  colMeans((probSamples > nextBest@overdose[1]) &
                           (probSamples < nextBest@overdose[2]))

              ## which doses are eligible after accounting
              ## for maximum possible dose and  discarding overdoses?
              dosesOK <- which((data@doseGrid <= doselimit) &
                               (probOverdose < nextBest@maxOverdoseProb))

              ## check if there are doses that are OK
              if(length(dosesOK))
              {
                  ## what is the recommended dose level?
                  doseLevel <- which.max(probTarget[dosesOK])
                  ret <- data@doseGrid[dosesOK][doseLevel]
              } else {
                  ## if none of the doses is OK:
                  doseLevel <- NA
                  ret <- NA
              }

              ## produce plot

              ## first for the target probability
              plot1 <- ggplot() +
                  geom_bar(data=
                           data.frame(x=data@doseGrid,
                                      y=probTarget * 100),
                           aes(x=x, y=y),
                           stat="identity",
                           position="identity",
                           width=1,
                           colour="darkgreen",
                           fill="darkgreen") +
                               xlab("Dose") +
                                   ylab(paste("Target probability [%]")) +
                                       ylim(c(0, 100))

              plot1 <- plot1 +
                  geom_vline(xintercept=doselimit,
                             lwd=1.1,
                             lty=2,
                             colour="black")


              if(length(dosesOK))
              {
                  plot1 <- plot1 +
                      geom_vline(xintercept=data@doseGrid[max(dosesOK)],
                                 lwd=1.1,
                                 lty=2,
                                 colour="red")

                  plot1 <- plot1 +
                      geom_point(data=
                                 data.frame(x=ret,
                                            y=probTarget[dosesOK][doseLevel] *
                                            100 + 0.03),
                                 aes(x=x, y=y),
                                 size=3,
                                 pch=25,
                                 col="red",
                                 bg="red")
              }

              ## second for the overdosing probability
              plot2 <- ggplot() +
                  geom_bar(data=
                           data.frame(x=data@doseGrid,
                                      y=probOverdose * 100),
                           aes(x=x, y=y),
                           stat="identity",
                           position="identity",
                           width=1,
                           colour="red",
                           fill="red") +
                               xlab("Dose") +
                                   ylab("Overdose probability [%]") +
                                       ylim(c(0, 100))

              plot2 <- plot2 +
                  geom_hline(yintercept=nextBest@maxOverdoseProb * 100,
                             lwd=1.1,
                             lty=2,
                             colour="black")

              ## now plot them below each other
              plotJoint <- gridExtra::arrangeGrob(plot1, plot2, nrow=2)

              ## return value and plot
              return(list(value=ret,
                          plot=plotJoint))
          })


## --------------------------------------------------
## The method for the dual endpoint model
## --------------------------------------------------

##' Find the next best dose based on the dual endpoint model
##'
##' @importFrom ggplot2 ggplot geom_bar xlab ylab ylim aes geom_vline
##' geom_hline geom_point
##' @importFrom gridExtra arrangeGrob
setMethod("nextBest",
          signature=
          signature(nextBest="NextBestDualEndpoint",
                    doselimit="numeric",
                    samples="Samples",
                    model="DualEndpoint",
                    data="Data"),
          def=
          function(nextBest, doselimit, samples, model, data, ...){

              ## get the biomarker level samples
              biomLevelSamples <- samples@data$betaW

              ## now for each sample, look which was the minimum dose giving
              ## relative target level
              targetIndex <- apply(biomLevelSamples, 1L,
                                   function(x){
                                       min(which(x >= nextBest@target * max(x)))
                                   })

              probTarget <- numeric(ncol(biomLevelSamples))
              tab <- table(targetIndex)
              probTarget[as.numeric(names(tab))] <- tab
              probTarget <- probTarget / nrow(biomLevelSamples)

              ## now get samples from the dose-tox
              ## curve at the dose grid points.
              probSamples <- matrix(nrow=sampleSize(samples@options),
                                    ncol=data@nGrid)

              ## evaluate the probs, for all samples.
              for(i in seq_len(data@nGrid))
              {
                  ## Now we want to evaluate for the
                  ## following dose:
                  probSamples[, i] <- prob(dose=data@doseGrid[i],
                                           model,
                                           samples)
              }

              ## Now compute probabilities to be in
              ## overdose tox interval
              probOverdose <-
                  colMeans((probSamples > nextBest@overdose[1]) &
                           (probSamples < nextBest@overdose[2]))

              ## which doses are eligible after accounting
              ## for maximum possible dose and discarding overdoses?
              dosesOK <- which((data@doseGrid <= doselimit) &
                               (probOverdose < nextBest@maxOverdoseProb))

              ## check if there are doses that are OK
              if(length(dosesOK))
              {
                  ## what is the recommended dose level?
                  doseLevel <- which.max(probTarget[dosesOK])
                  ret <- data@doseGrid[dosesOK][doseLevel]
              } else {
                  ## if none of the doses is OK:
                  doseLevel <- NA
                  ret <- NA
              }

              ## produce plot

              ## first for the target probability
              plot1 <- ggplot() +
                  geom_bar(data=
                           data.frame(x=data@doseGrid,
                                      y=probTarget * 100),
                           aes(x=x, y=y),
                           stat="identity",
                           position="identity",
                           width=1,
                           colour="darkgreen",
                           fill="darkgreen") +
                               xlab("Dose") +
                                   ylab(paste("Target probability [%]")) +
                                       ylim(c(0, 100))

              plot1 <- plot1 +
                  geom_vline(xintercept=doselimit,
                             lwd=1.1,
                             lty=2,
                             colour="black")

              if(length(dosesOK))
              {
                  plot1 <- plot1 +
                      geom_vline(xintercept=data@doseGrid[max(dosesOK)],
                                 lwd=1.1,
                                 lty=2,
                                 colour="red")

                  plot1 <- plot1 +
                      geom_point(data=
                                 data.frame(x=ret,
                                            y=probTarget[dosesOK][doseLevel] *
                                            100 + 0.03),
                                 aes(x=x, y=y),
                                 size=3,
                                 pch=25,
                                 col="red",
                                 bg="red")
              }

              ## second for the overdosing probability
              plot2 <- ggplot() +
                  geom_bar(data=
                           data.frame(x=data@doseGrid,
                                      y=probOverdose * 100),
                           aes(x=x, y=y),
                           stat="identity",
                           position="identity",
                           width=1,
                           colour="red",
                           fill="red") +
                               xlab("Dose") +
                                   ylab("Overdose probability [%]") +
                                       ylim(c(0, 100))

              plot2 <- plot2 +
                  geom_hline(yintercept=nextBest@maxOverdoseProb * 100,
                             lwd=1.1,
                             lty=2,
                             colour="black")

              ## now plot them below each other
              plotJoint <- gridExtra::arrangeGrob(plot1, plot2, nrow=2)

              ## return value and plot
              return(list(value=ret,
                          plot=plotJoint))
          })


## ============================================================


## --------------------------------------------------
## Determine the maximum possible next dose
## --------------------------------------------------

##' Determine the maximum possible next dose
##'
##' Determine the upper limit of the next dose based on the increments rule.
##'
##' This function outputs the maximum possible next dose, based on the
##' corresponding rule \code{increments} and the \code{data}.
##'
##' @param increments The rule, an object of class
##' \code{\linkS4class{Increments}}
##' @param data The data input, an object of class \code{\linkS4class{Data}}
##' @param \dots further arguments
##' @return the maximum possible next dose
##'
##' @genericMethods
##' @export
##' @keywords methods
setGeneric("maxDose",
           def=
           function(increments, data, ...){
               ## there should be no default method,
               ## therefore just forward to next method!
               standardGeneric("maxDose")
           },
           valueClass="numeric")


## --------------------------------------------------
## The maximum allowable relative increments in intervals method
## --------------------------------------------------

##' Determine the maximum possible next dose based on relative increments
setMethod("maxDose",
          signature=
          signature(increments="IncrementsRelative",
                    data="Data"),
          def=
          function(increments, data, ...){
              ## determine what was the last dose
              lastDose <- tail(data@x, 1)

              ## determine in which interval this dose was
              lastInterval <-
                  findInterval(x=lastDose,
                               vec=increments@intervals)

              ## so the maximum next dose is
              ret <-
                  (1 + increments@increments[lastInterval]) *
                      lastDose

              return(ret)
          })


## --------------------------------------------------
## The maximum allowable relative increments in terms of DLTs
## --------------------------------------------------

##' Determine the maximum possible next dose based on relative increments
##' determined by DLTs so far
setMethod("maxDose",
          signature=
          signature(increments="IncrementsRelativeDLT",
                    data="Data"),
          def=
          function(increments, data, ...){
              ## determine what was the last dose
              lastDose <- tail(data@x, 1)

              ## determine how many DLTs have occurred so far
              dltHappened <- sum(data@y)

              ## determine in which interval this is
              interval <-
                  findInterval(x=dltHappened,
                               vec=increments@DLTintervals)

              ## so the maximum next dose is
              ret <-
                  (1 + increments@increments[interval]) *
                      lastDose

              return(ret)
          })


## ============================================================

## --------------------------------------------------
## "AND" combination of stopping rules
## --------------------------------------------------

##' The method combining two atomic stopping rules
##'
##' @param e1 First \code{\linkS4class{Stopping}} object
##' @param e2 Second \code{\linkS4class{Stopping}} object
##' @return The \code{\linkS4class{StoppingAll}} object
##'
##' @keywords methods
setMethod("&",
          signature(e1="Stopping",
                    e2="Stopping"),
          def=
          function(e1, e2){
              new("StoppingAll",
                  stopList=
                  list(e1, e2))
          })

##' The method combining a stopping list and an atomic
##'
##' @param e1 \code{\linkS4class{StoppingAll}} object
##' @param e2 \code{\linkS4class{Stopping}} object
##' @return The modified \code{\linkS4class{StoppingAll}} object
##'
##' @keywords methods
setMethod("&",
          signature(e1="StoppingAll",
                    e2="Stopping"),
          def=
          function(e1, e2){
              e1@stopList <- c(e1@stopList,
                               e2)
              return(e1)
          })

##' The method combining an atomic and a stopping list
##'
##' @param e1 \code{\linkS4class{Stopping}} object
##' @param e2 \code{\linkS4class{StoppingAll}} object
##' @return The modified \code{\linkS4class{StoppingAll}} object
##'
##' @keywords methods
setMethod("&",
          signature(e1="Stopping",
                    e2="StoppingAll"),
          def=
          function(e1, e2){
              e2@stopList <- c(e1,
                               e2@stopList)
              return(e2)
          })

## --------------------------------------------------
## "OR" combination of stopping rules
## --------------------------------------------------

##' The method combining two atomic stopping rules
##'
##' @param e1 First \code{\linkS4class{Stopping}} object
##' @param e2 Second \code{\linkS4class{Stopping}} object
##' @return The \code{\linkS4class{StoppingAny}} object
##'
##' @name or-Stopping-Stopping
##' @keywords methods
setMethod("|",
          signature(e1="Stopping",
                    e2="Stopping"),
          def=
          function(e1, e2){
              new("StoppingAny",
                  stopList=
                  list(e1, e2))
          })

##' The method combining a stopping list and an atomic
##'
##' @param e1 \code{\linkS4class{StoppingAny}} object
##' @param e2 \code{\linkS4class{Stopping}} object
##' @return The modified \code{\linkS4class{StoppingAny}} object
##'
##' @name or-Stopping-StoppingAny
##' @keywords methods
setMethod("|",
          signature(e1="StoppingAny",
                    e2="Stopping"),
          def=
          function(e1, e2){
              e1@stopList <- c(e1@stopList,
                               e2)
              return(e1)
          })

##' The method combining an atomic and a stopping list
##'
##' @param e1 \code{\linkS4class{Stopping}} object
##' @param e2 \code{\linkS4class{StoppingAny}} object
##' @return The modified \code{\linkS4class{StoppingAny}} object
##'
##' @name or-StoppingAny-Stopping
##' @keywords methods
setMethod("|",
          signature(e1="Stopping",
                    e2="StoppingAny"),
          def=
          function(e1, e2){
              e2@stopList <- c(e1,
                               e2@stopList)
              return(e2)
          })



## --------------------------------------------------
## Stop the trial?
## --------------------------------------------------

##' Stop the trial?
##'
##' This function returns whether to stop the trial.
##'
##' @param stopping The rule, an object of class
##' \code{\linkS4class{Stopping}}
##' @param dose the recommended next best dose
##' @param samples the \code{\linkS4class{Samples}} object
##' @param model The model input, an object of class \code{\linkS4class{Model}}
##' @param data The data input, an object of class \code{\linkS4class{Data}}
##' @param \dots additional arguments
##'
##' @return logical value: \code{TRUE} if the trial can be stopped, \code{FALSE}
##' otherwise. It should have an attribute \code{message} which gives the reason
##' for the decision.
##'
##' @genericMethods
##' @export
##' @keywords methods
setGeneric("stopTrial",
           def=
           function(stopping, dose, samples, model, data, ...){
               ## if the recommended next dose is NA,
               ## stop in any case.
               if(is.na(dose))
               {
                   return(structure(TRUE,
                                    message="Recommended next best dose is NA"))
               }

               ## there should be no default method,
               ## therefore just forward to next method!
               standardGeneric("stopTrial")
           },
           valueClass="logical")


## --------------------------------------------------
## Stopping based on multiple stopping rules
## --------------------------------------------------

##' Stop based on multiple stopping rules
setMethod("stopTrial",
          signature=
          signature(stopping="StoppingList",
                    dose="ANY",
                    samples="ANY",
                    model="ANY",
                    data="ANY"),
          def=
          function(stopping, dose, samples, model, data, ...){
              ## evaluate the individual stopping rules
              ## in the list
              individualResults <-
                  lapply(stopping@stopList,
                         stopTrial,
                         dose=dose,
                         samples=samples,
                         model=model,
                         data=data,
                         ...)

              ## summarize to obtain overall result
              overallResult <- stopping@summary(as.logical(individualResults))

              ## retrieve individual text messages,
              ## but let them in the list structure
              overallText <- lapply(individualResults, attr, "message")

              return(structure(overallResult,
                               message=overallText))
          })

## --------------------------------------------------
## Stopping based on fulfillment of all multiple stopping rules
## --------------------------------------------------

##' Stop based on fulfillment of all multiple stopping rules
setMethod("stopTrial",
          signature=
          signature(stopping="StoppingAll",
                    dose="ANY",
                    samples="ANY",
                    model="ANY",
                    data="ANY"),
          def=
          function(stopping, dose, samples, model, data, ...){
              ## evaluate the individual stopping rules
              ## in the list
              individualResults <-
                  lapply(stopping@stopList,
                         stopTrial,
                         dose=dose,
                         samples=samples,
                         model=model,
                         data=data,
                         ...)

              ## summarize to obtain overall result
              overallResult <- all(as.logical(individualResults))

              ## retrieve individual text messages,
              ## but let them in the list structure
              overallText <- lapply(individualResults, attr, "message")

              return(structure(overallResult,
                               message=overallText))
          })


## --------------------------------------------------
## Stopping based on fulfillment of any stopping rule
## --------------------------------------------------

##' Stop based on fulfillment of any stopping rule
setMethod("stopTrial",
          signature=
          signature(stopping="StoppingAny",
                    dose="ANY",
                    samples="ANY",
                    model="ANY",
                    data="ANY"),
          def=
          function(stopping, dose, samples, model, data, ...){
              ## evaluate the individual stopping rules
              ## in the list
              individualResults <-
                  lapply(stopping@stopList,
                         stopTrial,
                         dose=dose,
                         samples=samples,
                         model=model,
                         data=data,
                         ...)

              ## summarize to obtain overall result
              overallResult <- any(as.logical(individualResults))

              ## retrieve individual text messages,
              ## but let them in the list structure
              overallText <- lapply(individualResults, attr, "message")

              return(structure(overallResult,
                               message=overallText))
          })




## --------------------------------------------------
## Stopping based on maximum number of patients
## --------------------------------------------------

##' Stop based on maximum number of patients
setMethod("stopTrial",
          signature=
          signature(stopping="StoppingMaxPatients",
                    dose="ANY",
                    samples="ANY",
                    model="ANY",
                    data="Data"),
          def=
          function(stopping, dose, samples, model, data, ...){
              ## can we stop?
              doStop <- data@nObs >= stopping@nPatients

              ## generate message
              text <-
                  paste("Number of patients is",
                        data@nObs,
                        "and thus",
                        ifelse(doStop, "reached", "below"),
                        "the prespecified maximum number",
                        stopping@nPatients)

              ## return both
              return(structure(doStop,
                               message=text))
          })

## --------------------------------------------------
## Stopping based on number of cohorts near to next best dose
## --------------------------------------------------

##' Stop based on number of cohorts near to next best dose
setMethod("stopTrial",
          signature=
          signature(stopping="StoppingCohortsNearDose",
                    dose="numeric",
                    samples="ANY",
                    model="ANY",
                    data="Data"),
          def=
          function(stopping, dose, samples, model, data, ...){
              ## determine the range where the cohorts must lie in
              lower <- (100 - stopping@percentage) / 100 * dose
              upper <- (100 + stopping@percentage) / 100 * dose

              ## which patients lie there?
              indexPatients <- which((data@x >= lower) & (data@x <= upper))

              ## how many cohorts?
              nCohorts <- length(unique(data@cohort[indexPatients]))

              ## so can we stop?
              doStop <- nCohorts >= stopping@nCohorts

              ## generate message
              text <- paste(nCohorts,
                            " cohorts lie within ",
                            stopping@percentage,
                            "% of the next best dose ",
                            dose,
                            ". This ",
                            ifelse(doStop, "reached", "is below"),
                            " the required ",
                            stopping@nCohorts,
                            " cohorts",
                            sep="")

              ## return both
              return(structure(doStop,
                               message=text))
          })


## --------------------------------------------------
## Stopping based on number of patients near to next best dose
## --------------------------------------------------

##' Stop based on number of patients near to next best dose
setMethod("stopTrial",
          signature=
          signature(stopping="StoppingPatientsNearDose",
                    dose="numeric",
                    samples="ANY",
                    model="ANY",
                    data="Data"),
          def=
          function(stopping, dose, samples, model, data, ...){
              ## determine the range where the cohorts must lie in
              lower <- (100 - stopping@percentage) / 100 * dose
              upper <- (100 + stopping@percentage) / 100 * dose

              ## how many patients lie there?
              nPatients <- sum((data@x >= lower) & (data@x <= upper))

              ## so can we stop?
              doStop <- nPatients >= stopping@nPatients

              ## generate message
              text <- paste(nPatients,
                            " patients lie within ",
                            stopping@percentage,
                            "% of the next best dose ",
                            dose,
                            ". This ",
                            ifelse(doStop, "reached", "is below"),
                            " the required ",
                            stopping@nCohorts,
                            " patients",
                            sep="")

              ## return both
              return(structure(doStop,
                               message=text))
          })

## --------------------------------------------------
## Stopping based on minimum number of cohorts
## --------------------------------------------------

##' Stop based on minimum number of cohorts
setMethod("stopTrial",
          signature=
          signature(stopping="StoppingMinCohorts",
                    dose="ANY",
                    samples="ANY",
                    model="ANY",
                    data="Data"),
          def=
          function(stopping, dose, samples, model, data, ...){
              ## determine number of cohorts
              nCohorts <- length(unique(data@cohort))

              ## so can we stop?
              doStop <- nCohorts >= stopping@nCohorts

              ## generate message
              text <-
                  paste("Number of cohorts is",
                        nCohorts,
                        "and thus",
                        ifelse(doStop, "reached", "below"),
                        "the prespecified minimum number",
                        stopping@nCohorts)

              ## return both
              return(structure(doStop,
                               message=text))
          })

## --------------------------------------------------
## Stopping based on probability of target tox interval
## --------------------------------------------------

##' Stop based on probability of target tox interval
setMethod("stopTrial",
          signature=
          signature(stopping="StoppingTargetProb",
                    dose="numeric",
                    samples="Samples",
                    model="Model",
                    data="ANY"),
          def=
          function(stopping, dose, samples, model, data, ...){
              ## first we have to get samples from the dose-tox
              ## curve at the dose.
              probSamples <- prob(dose=dose,
                                  model,
                                  samples)

              ## Now compute probability to be in target interval
              probTarget <-
                  mean((probSamples > stopping@target[1]) &
                       (probSamples < stopping@target[2]))

              ## so can we stop?
              doStop <- probTarget >= stopping@prob

              ## generate message
              text <-
                  paste("Probability for target toxicity is",
                        round(probTarget * 100),
                        "% for dose",
                        dose,
                        "and thus",
                        ifelse(doStop, "above", "below"),
                        "the required",
                        round(stopping@prob * 100),
                        "%")

              ## return both
              return(structure(doStop,
                               message=text))
          })


## --------------------------------------------------
## Stopping based on MTD distribution
## --------------------------------------------------

##' Stop based on MTD distribution
setMethod("stopTrial",
          signature=
          signature(stopping="StoppingMTDdistribution",
                    dose="numeric",
                    samples="Samples",
                    model="Model",
                    data="ANY"),
          def=
          function(stopping, dose, samples, model, data, ...){
              ## First, generate the MTD samples.

              ## add prior data and samples to the
              ## function environment so that they
              ## can be used.
              mtdSamples <- dose(prob=stopping@target,
                                 model,
                                 samples)

              ## what is the absolute threshold?
              absThresh <- stopping@thresh * dose

              ## what is the probability to be above this dose?
              prob <- mean(mtdSamples > absThresh)

              ## so can we stop?
              doStop <- prob >= stopping@prob

              ## generate message
              text <-
                  paste("Probability of MTD above",
                        round(stopping@thresh * 100),
                        "% of current dose",
                        dose,
                        "is",
                        round(prob * 100),
                        "% and thus",
                        ifelse(doStop, "above", "below"),
                        "the required",
                        round(stopping@prob * 100),
                        "%")

              ## return both
              return(structure(doStop,
                               message=text))
          })


## --------------------------------------------------
## Stopping based on probability of targeting biomarker
## --------------------------------------------------

##' Stop based on probability of targeting biomarker
setMethod("stopTrial",
          signature=
          signature(stopping="StoppingTargetBiomarker",
                    dose="numeric",
                    samples="Samples",
                    model="DualEndpoint",
                    data="ANY"),
          def=
          function(stopping, dose, samples, model, data, ...){
              ## compute the target biomarker prob at this dose

              ## get the biomarker level samples
              biomLevelSamples <- samples@data$betaW

              ## now for each sample, look which was the minimum dose giving
              ## relative target level
              targetIndex <- apply(biomLevelSamples, 1L,
                                   function(x){
                                       min(which(x >= stopping@target * max(x)))
                                   })

              probTarget <- numeric(ncol(biomLevelSamples))
              tab <- table(targetIndex)
              probTarget[as.numeric(names(tab))] <- tab
              probTarget <- probTarget / nrow(biomLevelSamples)

              ## so for this dose we have:
              probTarget <- probTarget[which(data@doseGrid == dose)]

              ## so can we stop?
              doStop <- probTarget >= stopping@prob

              ## generate message
              text <-
                  paste("Probability for target biomarker is",
                        round(probTarget * 100),
                        "% for dose",
                        dose,
                        "and thus",
                        ifelse(doStop, "above", "below"),
                        "the required",
                        round(stopping@prob * 100),
                        "%")

              ## return both
              return(structure(doStop,
                               message=text))
          })

## ============================================================

## --------------------------------------------------
## "MAX" combination of cohort size rules
## --------------------------------------------------

##' "MAX" combination of cohort size rules
##'
##' This function combines cohort size rules by taking
##' the maximum of all sizes.
##'
##' @param \dots Objects of class \code{\linkS4class{CohortSize}}
##' @return the combination as an object of class
##' \code{\linkS4class{CohortSizeMax}}
##'
##' @seealso \code{\link{minSize}}
##' @genericMethods
##' @export
##' @keywords methods
setGeneric("maxSize",
           def=
           function(...){
               ## there should be no default method,
               ## therefore just forward to next method!
               standardGeneric("maxSize")
           },
           valueClass="CohortSizeMax")

##' The method combining cohort size rules by taking maximum
setMethod("maxSize",
          "CohortSize",
          def=
          function(...){
              new("CohortSizeMax",
                  cohortSizeList=
                  list(...))
          })

## --------------------------------------------------
## "MIN" combination of cohort size rules
## --------------------------------------------------

##' "MIN" combination of cohort size rules
##'
##' This function combines cohort size rules by taking
##' the minimum of all sizes.
##'
##' @param \dots Objects of class \code{\linkS4class{CohortSize}}
##' @return the combination as an object of class
##' \code{\linkS4class{CohortSizeMin}}
##'
##' @seealso \code{\link{maxSize}}
##' @genericMethods
##' @export
##' @keywords methods
setGeneric("minSize",
           def=
           function(...){
               ## there should be no default method,
               ## therefore just forward to next method!
               standardGeneric("minSize")
           },
           valueClass="CohortSizeMin")

##' The method combining cohort size rules by taking minimum
setMethod("minSize",
          "CohortSize",
          def=
          function(...){
              new("CohortSizeMin",
                  cohortSizeList=
                  list(...))
          })


## --------------------------------------------------
## Determine the size of the next cohort
## --------------------------------------------------

##' Determine the size of the next cohort
##'
##' This function determines the size of the next cohort.
##'
##' @param cohortSize The rule, an object of class
##' \code{\linkS4class{CohortSize}}
##' @param dose the next dose
##' @param data The data input, an object of class \code{\linkS4class{Data}}
##' @param \dots additional arguments
##'
##' @return the size as integer value
##'
##' @genericMethods
##' @export
##' @keywords methods
setGeneric("size",
           def=
           function(cohortSize, dose, data, ...){
               ## if the recommended next dose is NA,
               ## don't check and return 0
               if(is.na(dose))
               {
                   return(0L)
               }

               ## there should be no default method,
               ## therefore just forward to next method!
               standardGeneric("size")
           },
           valueClass="integer")

## --------------------------------------------------
## The dose range method
## --------------------------------------------------

##' Determine the cohort size based on the range into which the next
##' dose falls into
setMethod("size",
          signature=
          signature(cohortSize="CohortSizeRange",
                    dose="ANY",
                    data="Data"),
          def=
          function(cohortSize, dose, data, ...){

              ## determine in which interval the next dose is
              interval <-
                  findInterval(x=dose,
                               vec=cohortSize@intervals)

              ## so the cohort size is
              ret <- cohortSize@cohortSize[interval]

              return(ret)
          })

## --------------------------------------------------
## The DLT range method
## --------------------------------------------------

##' Determine the cohort size based on the number of DLTs so far
setMethod("size",
          signature=
          signature(cohortSize="CohortSizeDLT",
                    dose="ANY",
                    data="Data"),
          def=
          function(cohortSize, dose, data, ...){

              ## determine how many DLTs have occurred so far
              dltHappened <- sum(data@y)

              ## determine in which interval this is
              interval <-
                  findInterval(x=dltHappened,
                               vec=cohortSize@DLTintervals)

              ## so the cohort size is
              ret <- cohortSize@cohortSize[interval]

              return(ret)
          })

## --------------------------------------------------
## Size based on maximum of multiple cohort size rules
## --------------------------------------------------

##' Size based on maximum of multiple cohort size rules
setMethod("size",
          signature=
          signature(cohortSize="CohortSizeMax",
                    dose="ANY",
                    data="Data"),
          def=
          function(cohortSize, dose, data, ...){
              ## evaluate the individual cohort size rules
              ## in the list
              individualResults <-
                  sapply(cohortSize@cohortSizeList,
                         size,
                         dose=dose,
                         data=data,
                         ...)

              ## summarize to obtain overall result
              overallResult <- max(individualResults)

              return(overallResult)
          })

## --------------------------------------------------
## Size based on minimum of multiple cohort size rules
## --------------------------------------------------

##' Size based on minimum of multiple cohort size rules
setMethod("size",
          signature=
          signature(cohortSize="CohortSizeMin",
                    dose="ANY",
                    data="Data"),
          def=
          function(cohortSize, dose, data, ...){
              ## evaluate the individual cohort size rules
              ## in the list
              individualResults <-
                  sapply(cohortSize@cohortSizeList,
                         size,
                         dose=dose,
                         data=data,
                         ...)

              ## summarize to obtain overall result
              overallResult <- min(individualResults)

              return(overallResult)
          })

## --------------------------------------------------
## Constant cohort size
## --------------------------------------------------

##' Constant cohort size
setMethod("size",
          signature=
          signature(cohortSize="CohortSizeConst",
                    dose="ANY",
                    data="Data"),
          def=
          function(cohortSize, dose, data, ...){
              return(cohortSize@size)
          })


## ============================================================
