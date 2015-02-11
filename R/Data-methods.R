#####################################################################################
## Author: Daniel Sabanes Bove [sabanesd *a*t* roche *.* com]
## Project: Object-oriented implementation of CRM designs
##
## Time-stamp: <[Data-methods.R] by DSB Son 25/01/2015 21:51>
##
## Description:
## Methods for handling the data. Plot ideas taken from bcrm package.
##
## History:
## 30/01/2014   file creation
## 06/02/2014   add method for conversion to list
## 17/02/2014   add update methods
###################################################################################


## ============================================================

## --------------------------------------------------
## Converting Data object to list
## --------------------------------------------------


##' as.list method for the "Data" class
##'
##' @param x the \code{\linkS4class{Data}} object we want to convert
##' @param \dots unused
##' @return a list of all slots in \code{x}
##'
##' @export
##' @keywords methods
setMethod("as.list",
          signature=
          signature(x="Data"),
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


## --------------------------------------------------
## Plot method for combo data
## --------------------------------------------------

##' Plot method for the "DataCombo" class
##'
##' @param x the \code{\linkS4class{DataDual}} object we want to plot
##' @param y missing
##' @param select two drug names that we want to use for plotting. Defaults
##' to the first two drug names.
##' @param shorten relative amount of shortening the arrows. default: 0.05
##' @param \dots not used
##' @return the \code{\link[ggplot2]{ggplot}} object
##'
##' @importFrom ggplot2 ggplot geom_point scale_colour_manual xlab ylab aes
##' @importFrom grid arrow
##' @importFrom gridExtra arrangeGrob
##'
##' @export
##' @keywords methods
setMethod("plot",
          signature=
          signature(x="DataCombo", y="missing"),
          def=
          function(x, y, select=head(x@drugNames, 2L), shorten=0.03, ...){

              ## make sure select contains drug names
              select <- match.arg(select, choices=x@drugNames,
                                  several.ok=TRUE)

              ## check shorten parameter
              stopifnot(shorten >= 0,
                        shorten < 1)

              ## first plot: (jittered) scatterplot
              amount1 <- min(diff(x@doseGrid[[select[1]]]))/4
              amount2 <- min(diff(x@doseGrid[[select[2]]]))/4

              ## create data frame
              set.seed(12)
              df <- data.frame(dose1=
                                   jitter(x@x[, select[1]],
                                          amount=amount1),
                               dose2=
                                   jitter(x@x[, select[2]],
                                          amount=amount2),
                               toxicity=ifelse(x@y==1, "Yes", "No"),
                               ID=paste(" ", x@ID))
              cols <- c("No" = "black","Yes" = "red")

              ## start the scatterplot
              plot1 <- ggplot(df, aes(x=dose1, y=dose2)) +
                  geom_point(aes(shape=toxicity,colour=toxicity),
                             size=3)

              ## add colour scale and axis labels
              plot1 <- plot1 +
                  scale_colour_manual(values=cols) +
                      xlab(select[1]) + ylab(select[2])

              ## add ID labels
              plot1 <- plot1 +
                  geom_text(aes(label=ID, size=1),
                            data=df,
                            hjust=-amount1, vjust=0,
                            angle=0, colour=I("black"),
                            show_guide = FALSE)

              ## add x axis breaks
              plot1 <- plot1 +
                  scale_x_continuous(breaks=x@x[, select[1]],
                                     minor_breaks=
                                         setdiff(x@doseGrid[[select[1]]],
                                                 x@x[, select[1]]),
                                     limits=c(0, max(df$dose1)))

              ## add y axis breaks
              plot1 <- plot1 +
                  scale_y_continuous(breaks=x@x[, select[2]],
                                     minor_breaks=
                                         setdiff(x@doseGrid[[select[2]]],
                                                 x@x[, select[2]]),
                                     limits=c(0, max(df$dose2)))
              ## todo: later try to show summaries for each tried dose combination,
              ## like small barcharts or similar. will take more work...

              ## second plot: show evolution of cohorts.

              ## first find out which points are we going to plot
              cohortCoords <- as.data.frame(unique(cbind(x@x[, select],
                                                         cohort=x@cohort)))
              names(cohortCoords) <- c("x", "y", "cohort")

              ## Plot the base graph minus the edges
              plot2 <- ggplot() +
                  geom_point(data=cohortCoords,
                             aes(x=x, y=y), size=6, inherit.aes = FALSE) +
                                 geom_text(data=cohortCoords,
                                           aes(x=x, y=y, label=as.character(cohort)),
                                           inherit.aes = FALSE, color="white", size=4)

              ## todo: later revisit this plot, maybe starting both axes at 0
              ## is not optimal.
              ex1 <- max(df$dose1) * 1.1
              ex2 <- max(df$dose2) * 1.1

              ## add x axis breaks
              plot2 <- plot2 +
                  scale_x_continuous(breaks=x@x[, select[1]],
                                     minor_breaks=
                                         setdiff(x@doseGrid[[select[1]]],
                                                 x@x[, select[1]]),
                                     limits=c(0, ex1))

              ## add y axis breaks
              plot2 <- plot2 +
                  scale_y_continuous(breaks=x@x[, select[2]],
                                     minor_breaks=
                                         setdiff(x@doseGrid[[select[2]]],
                                                 x@x[, select[2]]),
                                     limits=c(0, ex2))

              ## calculate the arrows
              dfArrows <- cbind(head(subset(cohortCoords, select=c("x", "y")),
                                     -1L),
                                tail(subset(cohortCoords, select=c("x", "y")),
                                     -1L))
              names(dfArrows) <- c("x", "y", "xend", "yend")

              ## shorten arrows as requested
              dfArrows$x <- dfArrows$x / ex1
              dfArrows$y <- dfArrows$y / ex2
              dfArrows$xend <- dfArrows$xend / ex1
              dfArrows$yend <- dfArrows$yend / ex2

              dfArrows$dx <- dfArrows$xend - dfArrows$x
              dfArrows$dy <- dfArrows$yend - dfArrows$y
              dfArrows$dist <- sqrt(dfArrows$dx^2 + dfArrows$dy^2)
              dfArrows$x <- dfArrows$x +
                  shorten / dfArrows$dist * dfArrows$dx
              dfArrows$y <- dfArrows$y +
                  shorten / dfArrows$dist * dfArrows$dy
              dfArrows$xend <- dfArrows$xend -
                  shorten / dfArrows$dist  * dfArrows$dx
              dfArrows$yend <- dfArrows$yend -
                  shorten / dfArrows$dist  * dfArrows$dy

              dfArrows$x <- dfArrows$x * ex1
              dfArrows$y <- dfArrows$y * ex2
              dfArrows$xend <- dfArrows$xend * ex1
              dfArrows$yend <- dfArrows$yend * ex2

              ## Add the edges
              plot2 <- plot2 +
                  geom_segment(aes(x=x, y=y, xend = xend,
                                   yend = yend),
                               size = 1,
                               data = dfArrows,
                               arrow = grid::arrow(length = grid::unit(1, units="mm")))

              plot2 <- plot2 + xlab(select[1]) + ylab(select[2])

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

## --------------------------------------------------
## Update a DataCombo object
## --------------------------------------------------

##' Update method for the "DataCombo" class
##'
##' Add new data to the \code{\linkS4class{DataCombo}} object
##'
##' @param object the old \code{\linkS4class{DataCombo}} object
##' @param x the dose levels vector (one dose level combination only!)
##' @param y the DLT vector (0/1 vector), for all patients in this cohort
##' @param ID the patient IDs
##' @param \dots not used
##' @return the new \code{\linkS4class{DataCombo}} object
##'
##' @export
##' @keywords methods
setMethod("update",
          signature=
          signature(object="DataCombo"),
          def=
          function(object,
                   x,
                   y,
                   ID=(if(length(object@ID)) max(object@ID) else 0L) + seq_along(y),
                   ...){

              ## checks
              stopifnot(is.numeric(x),
                        identical(length(x), object@nDrugs),
                        identical(names(x), object@drugNames))

              ## which grid levels are the doses?
              gridLevels <- integer(object@nDrugs)
              names(gridLevels) <- object@drugNames
              for(k in object@drugNames)
              {
                  gridLevels[k] <- match(x=x[k],
                                         table=object@doseGrid[[k]])
                  if(is.na(gridLevels[k]))
                  {
                      stop(paste("dose for drug", k, "is not on dose grid"))
                  }
              }

              ## add to the xLevel matrix
              object@xLevel <- rbind(object@xLevel,
                                     matrix(data=gridLevels,
                                            nrow=length(y),
                                            ncol=2L,
                                            byrow=TRUE))

              ## increment sample size
              object@nObs <- object@nObs + length(y)

              ## add dose combo
              object@x <- rbind(object@x,
                                matrix(data=x,
                                       nrow=length(y),
                                       ncol=2L,
                                       byrow=TRUE))

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





## ============================================================

















