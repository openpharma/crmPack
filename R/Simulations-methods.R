#####################################################################################
## Author: Daniel Sabanes Bove [sabanesd *a*t* roche *.* com]
## Project: Object-oriented implementation of CRM designs
##
## Time-stamp: <[Simulations-methods.R] by DSB Die 30/12/2014 19:25>
##
## Description:
## Methods for handling the simulations output.
##
## History:
## 19/02/2014   file creation
###################################################################################


##' @include Simulations-class.R
##' @include helpers.R
{}


##' Plot simulations
##'
##' Summarize the simulations with plots
##'
##' This plot method can be applied to \code{\linkS4class{GeneralSimulations}}
##' objects in order to summarize them graphically. Possible \code{type}s of
##' plots at the moment are: \describe{ \item{trajectory}{Summary of the
##' trajectory of the simulated trials} \item{dosesTried}{Average proportions of
##' the doses tested in patients} } You can specify one or both of these in the
##' \code{type} argument.
##'
##' @param x the \code{\linkS4class{GeneralSimulations}} object we want
##' to plot from
##' @param type the type of plots you want to obtain.
##' @return A single \code{\link[ggplot2]{ggplot2}} object if a single plot is
##' asked for, otherwise a \code{\link{gridExtra}{gTree}} object.
##'
##' @importFrom ggplot2 ggplot geom_step geom_bar aes xlab ylab
##' scale_linetype_manual
##' @importFrom gridExtra arrangeGrob
##' @export
##' @keywords methods
setMethod("plot",
          signature=
          signature(x="GeneralSimulations",
                    y="missing"),
          def=
          function(x,
                   y,
                   type=
                   c("trajectory",
                     "dosesTried"),
                   ...){

              ## which plots should be produced?
              type <- match.arg(type,
                                several.ok=TRUE)
              stopifnot(length(type) > 0L)

              ## start the plot list
              plotList <- list()
              plotIndex <- 0L

              ## summary of the trajectories
              if("trajectory" %in% type)
              {
                  ## get a matrix of the simulated dose trajectories,
                  ## where the rows correspond to the simulations and
                  ## the columns to the patient index:

                  simDoses <- lapply(x@data,
                                     slot,
                                     "x")

                  maxPatients <- max(sapply(simDoses, length))

                  simDosesMat <- matrix(data=NA,
                                        nrow=length(simDoses),
                                        ncol=maxPatients)

                  for(i in seq_along(simDoses))
                  {
                      simDosesMat[i, seq_along(simDoses[[i]])] <-
                          simDoses[[i]]
                  }


                  ## extract statistics
                  stats <- c("Minimum",
                             "Lower Quartile",
                             "Median",
                             "Upper Quartile",
                             "Maximum")
                  traj.df <-
                      data.frame(patient=
                                 rep(seq_len(maxPatients), each=5L),
                                 Statistic=
                                 factor(rep(stats,
                                            maxPatients),
                                        levels=stats),
                                 traj=
                                 c(apply(simDosesMat, 2L, quantile,
                                         na.rm=TRUE)))

                  ## linetypes for the plot
                  lt <- c("Median" = 1,
                          "Lower Quartile" = 2,
                          "Upper Quartile" = 2,
                          "Minimum" = 4,
                          "Maximum"= 4)

                  ## save the plot
                  plotList[[plotIndex <- plotIndex + 1L]] <-
                      ggplot() +
                          geom_step(aes(x=patient,
                                        y=traj,
                                        group=Statistic,
                                        linetype=Statistic),
                                    size=1.2, colour="blue", data=traj.df) +
                                        ## scale_linetype_manual(values=lt) +
                                            xlab("Patient") + ylab("Dose Level")
              }

              ## average distribution of the doses tried
              if("dosesTried" %in% type)
              {
                  ## get the doses tried
                  simDoses <- lapply(x@data,
                                     slot,
                                     "x")

                  ## get the dose distributions by trial
                  doseDistributions <-
                      sapply(simDoses,
                             function(s){
                                 prop.table(table(factor(s,
                                                         levels=
                                                         x@data[[1]]@doseGrid)))
                             })

                  ## derive the average dose distribution across trial
                  ## simulations
                  averageDoseDist <- rowMeans(doseDistributions)

                  ## get in data frame shape
                  dat <- data.frame(dose=as.numeric(names(averageDoseDist)),
                                    perc=averageDoseDist * 100)

                  ## produce and save the plot
                  plotList[[plotIndex <- plotIndex + 1L]] <-
                      ggplot() +
                          geom_bar(data=as.data.frame(dat),
                                   aes(x=dose, y=perc),
                                   stat="identity",
                                   position="identity",
                                   width=1) +
                                       xlab("Dose level") +
                                           ylab("Average proportion [%]")
              }
              ## then finally plot everything

              ## if there is only one plot
              if(identical(length(plotList),
                           1L))
              {
                  ## just return it
                  return(plotList[[1L]])
              } else {
                  ## otherwise arrange them
                  ret <- do.call(gridExtra::arrangeGrob,
                                 plotList)
                  return(ret)
              }
          })


##' Summarize the simulations, relative to a given truth
##'
##' @param object the \code{\linkS4class{GeneralSimulations}} object we want to
##' summarize
##' @param truth a function which takes as input a dose (vector) and returns the
##' true probability (vector) for toxicity. Additional arguments can be supplied
##' via \dots.
##' @param target the target toxicity interval (default: 20-35\%) used for the
##' computations
##' @return an object of class \code{\linkS4class{GeneralSimulations-summary}}
##'
##' @export
##' @keywords methods
setMethod("summary",
          signature=
          signature(object="GeneralSimulations"),
          def=
          function(object,
                   truth,
                   target=c(0.2, 0.35),
                   ...){

              ## extract dose grid
              doseGrid <- object@data[[1]]@doseGrid

              ## evaluate true toxicity at doseGrid
              trueTox <- truth(doseGrid, ...)

              ## find dose interval corresponding to target tox interval
              targetDoseInterval <-
                  sapply(target,
                         function(t){
                             ## we have to be careful because it might be
                             ## that in the range of the dose grid, no
                             ## doses can be found that match the target
                             ## interval boundaries!
                             ## In that case we want to return NA
                             r <- try(uniroot(f=function(x){truth(x, ...) - t},
                                              interval=
                                              range(doseGrid))$root,
                                      silent=TRUE)
                             if(inherits(r, "try-error"))
                             {
                                 return(NA_real_)
                             } else {
                                 return(r)
                             }
                         })

              ## what are the levels above target interval?
              xAboveTarget <- which(trueTox > target[2])

              ## proportion of DLTs in a trial:
              propDLTs <- sapply(object@data,
                                 function(d){
                                     mean(d@y)
                                 })

              ## mean toxicity risk
              meanToxRisk <- sapply(object@data,
                                    function(d){
                                        mean(trueTox[d@xLevel])
                                    })

              ## doses selected for MTD
              doseSelected <- object@doses

              ## replace NA by 0
              doseSelected[is.na(doseSelected)] <- 0

              ## dose most often selected as MTD
              doseMostSelected <-
                  as.numeric(names(which.max(table(doseSelected))))
              xMostSelected <-
                  match(doseMostSelected,
                        table=doseGrid)

              ## observed toxicity rate at dose most often selected
              ## Note: this does not seem very useful!
              ## Reason: In case of a fine grid, few patients if any
              ## will have been treated at this dose.
              tmp <-
                  sapply(object@data,
                         function(d){
                             whichAtThisDose <- which(d@x == doseMostSelected)
                             nAtThisDose <- length(whichAtThisDose)
                             nDLTatThisDose <- sum(d@y[whichAtThisDose])
                             return(c(nAtThisDose=nAtThisDose,
                                      nDLTatThisDose=nDLTatThisDose))
                         })

              obsToxRateAtDoseMostSelected <-
                  mean(tmp["nDLTatThisDose",]) / mean(tmp["nAtThisDose",])

              ## number of patients overall
              nObs <- sapply(object@data,
                             slot,
                             "nObs")

              ## number of patients treated above target tox interval
              nAboveTarget <- sapply(object@data,
                                     function(d){
                                         sum(d@xLevel %in% xAboveTarget)
                                     })

              ## Proportion of trials selecting target MTD
              toxAtDoses <- truth(doseSelected, ...)
              propAtTarget <- mean((toxAtDoses > target[1]) &
                                   (toxAtDoses < target[2]))

              ## give back an object of class GeneralSimulations-summary,
              ## for which we then define a print / plot method
              ret <- new("GeneralSimulations-summary",
                         target=target,
                         targetDoseInterval=targetDoseInterval,
                         nsim=length(object@data),
                         propDLTs=propDLTs,
                         meanToxRisk=meanToxRisk,
                         doseSelected=doseSelected,
                         doseMostSelected=doseMostSelected,
                         obsToxRateAtDoseMostSelected=obsToxRateAtDoseMostSelected,
                         nObs=nObs,
                         nAboveTarget=nAboveTarget,
                         toxAtDosesSelected=toxAtDoses,
                         propAtTarget=propAtTarget,
                         doseGrid=doseGrid)

              return(ret)
          })


##' Summarize the model-based design simulations, relative to a given truth
##'
##' @return an object of class \code{\linkS4class{Simulations-summary}}
##'
##' @export
##' @keywords methods
setMethod("summary",
          signature=
          signature(object="Simulations"),
          def=
          function(object,
                   truth,
                   target=c(0.2, 0.35),
                   ...){

              ## call the parent method
              start <- callNextMethod(object=object,
                                      truth=truth,
                                      target=target,
                                      ...)

              doseGrid <- object@data[[1]]@doseGrid

              ## dose level most often selected as MTD
              xMostSelected <-
                  match(start@doseMostSelected,
                        table=doseGrid)

              ## fitted toxicity rate at dose most often selected
              fitAtDoseMostSelected <-
                  sapply(object@fit,
                         function(f){
                             f$middle[xMostSelected]
                         })

              ## mean fitted toxicity (average, lower and upper quantiles)
              ## at each dose level
              ## (this is required for plotting)
              meanFitMatrix <- sapply(object@fit,
                                      "[[",
                                      "middle")
              meanFit <- list(truth=
                              truth(doseGrid, ...),
                              average=
                              rowMeans(meanFitMatrix),
                              lower=
                              apply(meanFitMatrix,
                                    1L,
                                    quantile,
                                    0.025),
                              upper=
                              apply(meanFitMatrix,
                                          1L,
                                    quantile,
                                    0.975))

              ## give back an object of class Simulations-summary,
              ## for which we then define a print / plot method
              ret <- new("Simulations-summary",
                         start,
                         fitAtDoseMostSelected=fitAtDoseMostSelected,
                         meanFit=meanFit)

              return(ret)
          })


##' A Reference Class to represent sequentially updated reporting objects.
##' @name Report
##' @field object The object from which to report
##' @field df the data frame to which columns are sequentially added
##' @field dfNames the names to which strings are sequentially added
Report <-
    setRefClass("Report",
                fields =
                list(object = "ANY",
                     df = "data.frame",
                     dfNames = "character"),
                methods = list(
                dfSave =
                function(res, name) {
                    df <<- cbind(df, res)
                    dfNames <<- c(dfNames, name)
                    return(res)
                },
                report =
                function(slotName,
                         description,
                         percent=TRUE,
                         digits=0,
                         quantiles=c(0.1, 0.9)) {
                    vals <- slot(object, name=slotName)
                    if(percent)
                    {
                        unit <- " %"
                        vals <- vals * 100
                    } else {
                        unit <- ""
                    }

                    res <- paste(round(mean(vals), digits),
                                 unit,
                                 " (",
                                 paste(round(quantile(vals,
                                                      quantiles,
                                                      na.rm=TRUE),
                                             digits),
                                       unit,
                                       collapse=", ",
                                       sep=""),
                                 ")",
                                 sep="")

                    ## print result to the buffer
                    cat(description, ":",
                        "mean",
                        dfSave(res, slotName),
                        "\n")
                }))


##' Show the summary of the simulations
##'
##' @param object the \code{\linkS4class{GeneralSimulations-summary}} object we want
##' to print
##' @return invisibly returns a data frame of the results with one row and
##' appropriate column names
##'
##' @export
##' @keywords methods
setMethod("show",
          signature=
          signature(object="GeneralSimulations-summary"),
          def=
          function(object){

              r <- Report$new(object=object,
                              df=
                              as.data.frame(matrix(nrow=1,
                                                   ncol=0)),
                              dfNames=character())

              cat("Summary of",
                  r$dfSave(object@nsim, "nsim"),
                  "simulations\n\n")

              cat("Target toxicity interval was",
                  r$dfSave(paste(round(object@target * 100),
                               collapse=", "),
                         "targetToxInterval"),
                  "%\n")
              cat("Target dose interval corresponding to this was",
                  r$dfSave(paste(round(object@targetDoseInterval, 1),
                               collapse=", "),
                         "targetDoseInterval"),
                  "\n")
              cat("Intervals are corresponding to",
                  "10 and 90 % quantiles\n\n")

              r$report("nObs",
                       "Number of patients overall",
                       percent=FALSE)
              r$report("nAboveTarget",
                       "Number of patients treated above target tox interval",
                       percent=FALSE)
              r$report("propDLTs",
                       "Proportions of DLTs in the trials")
              r$report("meanToxRisk",
                       "Mean toxicity risks for the patients")
              r$report("doseSelected",
                       "Doses selected as MTD",
                       percent=FALSE, digits=1)
              r$report("toxAtDosesSelected",
                       "True toxicity at doses selected")
              cat("Proportion of trials selecting target MTD:",
                  r$dfSave(object@propAtTarget * 100,
                         "percentAtTarget"),
                  "%\n")
              cat("Dose most often selected as MTD:",
                  r$dfSave(object@doseMostSelected,
                         "doseMostSelected"),
                  "\n")
              cat("Observed toxicity rate at dose most often selected:",
                  r$dfSave(round(object@obsToxRateAtDoseMostSelected * 100),
                         "obsToxRateAtDoseMostSelected"),
                  "%\n")

              ## finally assign names to the df
              ## and return it invisibly
              names(r$df) <- r$dfNames
              invisible(r$df)
          })


##' Show the summary of the simulations
##'
##' @param object the \code{\linkS4class{Simulations-summary}} object we want
##' to print
##' @return invisibly returns a data frame of the results with one row and
##' appropriate column names
##'
##' @export
##' @keywords methods
setMethod("show",
          signature=
          signature(object="Simulations-summary"),
          def=
          function(object){

              ## call the parent method
              df <- callNextMethod(object)
              dfNames <- names(df)

              ## start report object
              r <- Report$new(object=object,
                              df=df,
                              dfNames=dfNames)

              ## add one reporting line
              r$report("fitAtDoseMostSelected",
                       "Fitted toxicity rate at dose most often selected")

              ## and return the updated information
              names(r$df) <- r$dfNames
              invisible(r$df)
          })


##' Plot summaries of the general simulations
##'
##' Graphical display of the general simulation summary
##'
##' This plot method can be applied to
##' \code{\linkS4class{GeneralSimulations-summary}} objects in order to
##' summarize them graphically. Possible \code{type}s of plots at the moment
##' are:
##'
##' \describe{
##' \item{nObs}{Distribution of the number of patients in the simulated trials}
##' \item{doseSelected}{Distribution of the final selected doses in the trials.
##' Note that this can include zero entries, meaning that the trial was stopped
##' because all doses in the dose grid appeared too toxic.}
##' \item{propDLTs}{Distribution of the proportion of patients with DLTs in the
##' trials}
##' \item{nAboveTarget}{Distribution of the number of patients treated at doses
##' which are above the target toxicity interval (as specified by the
##' \code{truth} and \code{target} arguments to
##' \code{\link{summary,GeneralSimulations-method}})}
##' }
##' You can specify any subset of these in the \code{type} argument.
##'
##' @param x the \code{\linkS4class{GeneralSimulations-summary}} object we want
##' to plot from
##' @param type the types of plots you want to obtain.
##' @return A single \code{\link[ggplot2]{ggplot2}} object if a single plot is
##' asked for, otherwise a \code{\link{gridExtra}{gTree}} object.
##'
##' @importFrom ggplot2 geom_histogram ggplot aes xlab ylab geom_line
##' scale_linetype_manual scale_colour_manual
##' @importFrom gridExtra arrangeGrob
##' @export
##' @keywords methods
setMethod("plot",
          signature=
          signature(x="GeneralSimulations-summary",
                    y="missing"),
          def=
          function(x,
                   y,
                   type=
                   c("nObs",
                     "doseSelected",
                     "propDLTs",
                     "nAboveTarget"),
                   ...){

              ## convenience function to make histograms
              myHist <- function(x, description)
              {
                  dat <- data.frame(x=x)
                  ggplot() +
                      geom_histogram(aes(x=x, y=100*..density..),
                                     data=dat, binwidth=1, origin=-0.5) +
                                         xlab(description)+
                                             ylab("Percent")
              }


              ## which plots should be produced?
              type <- match.arg(type,
                                several.ok=TRUE)
              stopifnot(length(type) > 0L)

              ## start the plot list
              plotList <- list()
              plotIndex <- 0L

              ## distribution of overall sample size
              if("nObs" %in% type)
              {
                  plotList[[plotIndex <- plotIndex + 1L]] <-
                      myHist(x=x@nObs,
                             description="Number of patients in total")
              }

              ## distribution of final MTD estimate
              if("doseSelected" %in% type)
              {
                  plotList[[plotIndex <- plotIndex + 1L]] <-
                      myHist(x=x@doseSelected,
                             description="MTD estimate")
              }

              ## distribution of proportion of DLTs
              if("propDLTs" %in% type)
              {
                  plotList[[plotIndex <- plotIndex + 1L]] <-
                      myHist(x=x@propDLTs * 100,
                             description="Proportion of DLTs [%]")
              }

              ## distribution of number of patients treated at too much tox
              if("nAboveTarget" %in% type)
              {
                  plotList[[plotIndex <- plotIndex + 1L]] <-
                      myHist(x=x@nAboveTarget,
                             description="Number of patients above target")
              }

              ## first combine these small plots
              if(length(plotList))
              {
                  ret <-
                      ## if there is only one plot
                      if(identical(length(plotList),
                                   1L))
                      {
                          ## just use that
                          plotList[[1L]]
                      } else {
                          ## multiple plots in this case
                          do.call(gridExtra::arrangeGrob,
                                  plotList)
                      }
              }

              ## then return
              ret
          })


##' Plot summaries of the model-based design simulations
##'
##' Graphical display of the simulation summary
##'
##' This plot method can be applied to \code{\linkS4class{Simulations-summary}}
##' objects in order to summarize them graphically. Possible \code{type} of
##' plots at the moment are those listed in
##' \code{\link{plot,GeneralSimulations-summary-method}} plus:
##' \describe{
##' \item{meanFit}{Plot showing the average fitted dose-toxicity curve across
##' the trials, together with 95\% credible intervals, and comparison with the
##' assumed truth (as specified by the \code{truth} argument to
##' \code{\link{summary,Simulations-method}})}
##' }
##' You can specify any subset of these in the \code{type} argument.
##'
##' @param x the \code{\linkS4class{Simulations-summary}} object we want
##' to plot from
##' @param type the types of plots you want to obtain.
##' @return A single \code{\link[ggplot2]{ggplot2}} object if a single plot is
##' asked for, otherwise a \code{\link{gridExtra}{gTree}} object.
##'
##' @importFrom ggplot2 geom_histogram ggplot aes xlab ylab geom_line
##' scale_linetype_manual scale_colour_manual
##' @importFrom gridExtra arrangeGrob
##' @export
##' @keywords methods
setMethod("plot",
          signature=
          signature(x="Simulations-summary",
                    y="missing"),
          def=
          function(x,
                   y,
                   type=
                   c("nObs",
                     "doseSelected",
                     "propDLTs",
                     "nAboveTarget",
                     "meanFit"),
                   ...){

              ## which plots should be produced?
              type <- match.arg(type,
                                several.ok=TRUE)
              stopifnot(length(type) > 0L)

              ## substract the specific plot types for model-based
              ## designs
              typeReduced <- setdiff(type,
                                     "meanFit")

              ## are there more plots from general?
              moreFromGeneral <- (length(typeReduced) > 0)

              ## if so, then produce these plots
              if(moreFromGeneral)
              {
                  ret <- callNextMethod(x=x, y=y, type=typeReduced)
              }

              ## is the meanFit plot requested?
              if("meanFit" %in% type)
              {
                  ## which types of lines do we have?
                  type <- c("True toxicity",
                            "Average estimated toxicity",
                            "95% interval for estimated toxicity")

                  ## create the data frame, with
                  ## true tox, average estimated tox, and 95% (lower, upper)
                  ## estimated tox (in percentage) stacked below each other
                  dat <- data.frame(dose=
                                    rep(x@doseGrid, 4L),
                                    group=
                                    rep(1:4, each=length(x@doseGrid)),
                                    type=
                                    factor(rep(type[c(1, 2, 3, 3)],
                                               each=length(x@doseGrid)),
                                           levels=type),
                                    lines=
                                    unlist(x@meanFit) * 100)

                  ## linetypes for the plot
                  lt <- c("True toxicity"=1,
                          "Average estimated toxicity"=1,
                          "95% interval for estimated toxicity"=2)

                  ## colour for the plot
                  col <- c("True toxicity"=1,
                          "Average estimated toxicity"=2,
                          "95% interval for estimated toxicity"=2)

                  ## now create and save the plot
                  thisPlot <- ggplot() +
                      geom_line(aes(x=dose,
                                    y=lines,
                                    group=group,
                                    linetype=type,
                                    col=type),
                                data=dat)

                  thisPlot <- thisPlot +
                       scale_linetype_manual(values=lt) +
                           scale_colour_manual(values=col) +
                               xlab("Dose level") +
                                   ylab("Probability of DLT [%]")

                  ## add this plot to the bottom
                  ret <-
                      if(moreFromGeneral)
                          gridExtra::arrangeGrob(ret, thisPlot)
                      else
                          thisPlot
              }

              ## then finally plot everything
              ret
          })
