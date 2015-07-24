#####################################################################################
## Author: Daniel Sabanes Bove [sabanesd *a*t* roche *.* com],
##         Wai Yin Yeung [ w *.*yeung1 *a*t* lancaster *.* ac *.* uk]
## Project: Object-oriented implementation of CRM designs
##
## Time-stamp: <[Data-methods.R] by DSB Mon 11/05/2015 17:43>
##
## Description:
## Methods for handling the data. Plot ideas taken from bcrm package.
##
## History:
## 30/01/2014   file creation
## 06/02/2014   add method for conversion to list
## 17/02/2014   add update methods
## 21/07/2015   add plots using data and pseudo models
###################################################################################


## ============================================================

## --------------------------------------------------
## Converting Data object to list
## --------------------------------------------------


##' as.list method for the "GeneralData" class
##'
##' @param x the \code{\linkS4class{GeneralData}} object we want to convert
##' @param \dots unused
##' @return a list of all slots in \code{x}
##'
##' @export
##' @keywords methods
setMethod("as.list",
          signature=
          signature(x="GeneralData"),
          def=
          function(x, ...){
              nams <- slotNames(x)
              ret <- lapply(nams,
                            function(n){
                                slot(x, n)
                            })
              names(ret) <- nams
              return(ret)
          })



## ============================================================

## --------------------------------------------------
## Plotting the Data objects
## --------------------------------------------------


##' Plot method for the "Data" class
##'
##' @param x the \code{\linkS4class{Data}} object we want to plot
##' @param y missing
##' @param \dots not used
##' @return the \code{\link[ggplot2]{ggplot}} object
##'
##' @importFrom ggplot2 ggplot geom_point scale_colour_manual xlab ylab aes
##' scale_y_continuous
##'
##' @export
##' @keywords methods
setMethod("plot",
          signature=
          signature(x="Data", y="missing"),
          def=
          function(x, y, ...){
              if(x@nObs == 0)
              {
                  return()
              }

              df <- data.frame(patient=seq_along(x@x),
                               dose=x@x,
                               toxicity=ifelse(x@y==1, "Yes", "No"),
                               ID=paste(" ", x@ID))
              cols <- c("No" = "black","Yes" = "red")

              a <- ggplot(df, aes(x=patient,y=dose)) +
                  scale_y_continuous(breaks=
                                     sort(unique(c(0, df$dose))),
                                     minor_breaks=numeric(),
                                     limits=c(0, max(df$dose) * 1.1))

              a <- a +
                  geom_point(aes(shape=toxicity,colour=toxicity),
                             size=3) +
                                 scale_colour_manual(values=cols) +
                                     xlab("Patient") + ylab("Dose Level")

              a <- a + geom_text(aes(label=ID, size=2),
                                 data=df,
                                 hjust=0, vjust=0.5,
                                 angle=90, colour=I("black"),
                                 show_guide = FALSE)

              a <- a + scale_x_continuous(breaks=df$patient,
                                          minor_breaks=numeric())
              return(a)
          })


## --------------------------------------------------
## Subclass with additional biomarker information
## --------------------------------------------------

##' Plot method for the "DataDual" class
##'
##' @param x the \code{\linkS4class{DataDual}} object we want to plot
##' @param y missing
##' @param \dots not used
##' @return the \code{\link[ggplot2]{ggplot}} object
##'
##' @importFrom ggplot2 ggplot geom_point scale_colour_manual xlab ylab aes
##' @importFrom gridExtra arrangeGrob
##'
##' @export
##' @keywords methods
setMethod("plot",
          signature=
          signature(x="DataDual", y="missing"),
          def=
          function(x, y, ...){
              ## call the superclass method, to get the first plot
              plot1 <- callNextMethod(x, ...)

              ## now to get the second plot
              df <- data.frame(patient=seq_along(x@x),
                               dose=x@x,
                               biomarker=x@w,
                               toxicity=ifelse(x@y==1, "Yes", "No"))
              cols <- c("No" = "black","Yes" = "red")

              plot2 <- ggplot(df, aes(x=dose, y=biomarker))

              plot2 <- plot2 +
                  geom_point(aes(shape=toxicity, colour=toxicity),
                             size=3) +
                      scale_colour_manual(values=cols) +
                          xlab("Dose Level") + ylab("Biomarker")

              plot2 <- plot2 +
                  geom_text(data=df,
                            aes(label=patient, y=biomarker+0.02 * diff(range(biomarker)), size=2), hjust=0,
                            vjust=0.5, angle=90, colour=I("black"),
                            show_guide=FALSE)

              ## arrange both plots side by side
              ret <- gridExtra::arrangeGrob(plot1, plot2, ncol=2)
              return(ret)
          })



## ============================================================

## --------------------------------------------------
## Update a Data object
## --------------------------------------------------


##' Update method for the "Data" class
##'
##' Add new data to the \code{\linkS4class{Data}} object
##'
##' @param object the old \code{\linkS4class{Data}} object
##' @param x the dose level (one level only!)
##' @param y the DLT vector (0/1 vector), for all patients in this cohort
##' @param ID the patient IDs
##' @param \dots not used
##' @return the new \code{\linkS4class{Data}} object
##'
##' @export
##' @keywords methods
setMethod("update",
          signature=
          signature(object="Data"),
          def=
          function(object,
                   x,
                   y,
                   ID=(if(length(object@ID)) max(object@ID) else 0L) + seq_along(y),
                   ...){

              ## some checks
              stopifnot(is.scalar(x),
                        all(y %in% c(0, 1)))

              ## which grid level is the dose?
              gridLevel <- match(x, object@doseGrid)

              ## add it to the data
              if(is.na(gridLevel))
              {
                  stop("dose is not on grid")
              } else {
                  object@xLevel <- c(object@xLevel,
                                     rep(gridLevel,
                                         length(y)))
              }

              ## increment sample size
              object@nObs <- object@nObs + length(y)

              ## add dose
              object@x <- c(object@x,
                            rep(x,
                                length(y)))

              ## add DLT data
              object@y <- c(object@y, as.integer(y))

              ## add ID
              object@ID <- c(object@ID, ID)

              ## add cohort number
              object@cohort <- c(object@cohort,
                                 rep(max(tail(object@cohort, 1L), 0L) + 1L,
                                     length(y)))

              ## return the object
              return(object)
          })


## --------------------------------------------------
## Update a DataParts object
## --------------------------------------------------

##' Update method for the "DataParts" class
##'
##' Add new data to the \code{\linkS4class{DataParts}} object
##'
##' @param object the old \code{\linkS4class{DataParts}} object
##' @param x the dose level (one level only!)
##' @param y the DLT vector (0/1 vector), for all patients in this cohort
##' @param ID the patient IDs
##' @param \dots not used
##' @return the new \code{\linkS4class{DataParts}} object
##'
##' @export
##' @keywords methods
setMethod("update",
          signature=
          signature(object="DataParts"),
          def=
          function(object,
                   x,
                   y,
                   ID=(if(length(object@ID)) max(object@ID) else 0L) + seq_along(y),
                   ...){

              ## first do the usual things as for Data objects
              object <- callNextMethod(object=object, x=x, y=y, ID=ID, ...)

              ## update the part information
              object@part <- c(object@part,
                               rep(object@nextPart,
                                   length(y)))

              ## now decide which part the next cohort will belong to:
              ## only if the nextPart was 1, it can potentially be required to
              ## change it to 2 (once it is 2, it stays)
              if(object@nextPart == 1L)
              {
                  ## if there was a DLT in one of the cohorts,
                  ## or if the current dose was the highest from part 1:
                  if(any(object@y == 1L) || x == max(object@part1Ladder))
                  {
                      ## then this closes part 1 and the next cohort will
                      ## be from part 2:
                      object@nextPart <- 2L
                  }
              }

              ## return the object
              return(object)
          })

## --------------------------------------------------
## Update a DataDual object
## --------------------------------------------------

##' Update method for the "DataDual" class
##'
##' Add new data to the \code{\linkS4class{DataDual}} object
##'
##' @param object the old \code{\linkS4class{DataDual}} object
##' @param x the dose level (one level only!)
##' @param y the DLT vector (0/1 vector), for all patients in this cohort
##' @param w the biomarker vector, for all patients in this cohort
##' @param ID the patient IDs
##' @param \dots not used
##' @return the new \code{\linkS4class{DataDual}} object
##'
##' @export
##' @keywords methods
setMethod("update",
          signature=
          signature(object="DataDual"),
          def=
          function(object,
                   x,
                   y,
                   w,
                   ID=(if(length(object@ID)) max(object@ID) else 0L) + seq_along(y),
                   ...){

              ## first do the usual things as for Data objects
              object <- callNextMethod(object=object, x=x, y=y, ID=ID, ...)

              ## update the biomarker information
              object@w <- c(object@w,
                            w)

              ## return the object
              return(object)
          })


## ==================================================================================================
## ----------------------------------------------------------------------------------------
## Plot of fitted dose-tox curve based without sample based on a given pseudo model
## and data
##-------------------------------------------------------------------------------------
##' Plot of the fitted dose-tox based with a given pseudo DLE model and data witout samples
##' 
##' @param x the data input from the \code{\linkS4class{Data}} class object
##' @param y the model from the \code{\linkS4class{ModelTox}} class object
##' 
##' @export
##' @keywords methods
setMethod("plot",
          signature=
            signature(x="Data",
                      y="ModelTox"),
          def=
            function(x,y,...,
                     xlab="Dose level",
                     ylab="Probability of DLE",
                     showLegend=TRUE){
              ##check args
              
              stopifnot(is.bool(showLegend))
              
              ##Make sure the right model estimates are use with the given data
              y <- update(object=y,data=x)
              
              
              
              ##create data frame
              
              plotData <- data.frame(dose=x@doseGrid,
                                     probDLE=prob(dose=x@doseGrid,
                                                  model=y))
              ##Look for TD30 and TD35
              TD30 <-dose(prob=0.30,
                          model=y)
              TD35 <-dose(prob=0.35,
                          model=y)
              
              ##make the plot
              gdata <- with(plotData,
                            data.frame(x=dose,
                                       y=probDLE,
                                       group=rep("Estimated DLE",each=nrow(plotData)),
                                       Type=factor(rep("Estimated DLE",nrow(plotData)),levels="Estimated DLE")))
              
              plot1 <- ggplot2::qplot(x=x,
                                      y=y,
                                      data=gdata,
                                      group=group,
                                      linetype=Type,
                                      colour=I("red"),
                                      geom="line",
                                      xlab=xlab,
                                      ylab=ylab,
                                      ylim=c(0,1))
              
              plot1 <- plot1 + ggplot2::scale_linetype_manual(breaks="Estimated DLE",
                                                              values=c(1,2),
                                                              guide=ifelse(showLegend,"legend",FALSE))
              
              
              plot1 <- plot1 +
                geom_line(size=1.5,colour="red")
              
              return(plot1)
            })

## ==============================================================================================
## ---------------------------------------------------------------------------------------------
## Plot the fitted dose-efficacy curve given a pseudo data and data
## -----------------------------------------------------------------------------
##' Plot the fitted dose-efficacy curve given a pseudo Efficacy model and data 
##' without samples
##' 
##' @param x the data input from the \code{\linkS4class{DataDual}} class object
##' @param y the model from the \code{\linkS4class{ModelEff}} class object
##' 

setMethod("plot",
          signature=
            signature(x="DataDual",
                      y="ModelEff"),
          def=
            function(x,y,...,
                     xlab="Dose level",
                     ylab="Expected Efficacy",
                     showLegend=TRUE){
              ##check args
              
              stopifnot(is.bool(showLegend))
              y <- update(object=y,data=x)
              
              ##create data frame
              
              plotEffData<- data.frame(dose=x@doseGrid,
                                       ExpEff=ExpEff(dose=x@doseGrid,
                                                     model=y))
              
              ##make the second plot
              ggdata<-with(plotEffData,
                           data.frame(x=dose,
                                      y=ExpEff,
                                      group=rep("Estimated Expected Efficacy",each=nrow(plotEffData)),
                                      Type=factor(rep("Estimated Expected Efficacy",nrow(plotEffData)),levels="Estimated Expected Efficacy")))
              
              ##Get efficacy plot
              plot2 <- ggplot(data=ggdata, aes(x=x,y=y), group=group) +
                xlab("Dose Levels")+
                ylab(paste("Estimated Expected Efficacy")) + xlim(c(0,max(data@doseGrid))) +
                geom_line(colour=I("blue"), size=1.5)
              
              plot2 <- plot2 +
                geom_line(size=1.5,colour="blue")
              
              
              return(plot2)
            })


## ===================================================================================================






