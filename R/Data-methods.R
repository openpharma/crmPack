#####################################################################################
## Author: Daniel Sabanes Bove [sabanesd *a*t* roche *.* com]
## Project: Object-oriented implementation of CRM designs
##
## Time-stamp: <[Data-methods.R] by DSB Die 29/04/2014 14:57>
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
                   ID,
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
              if(missing(ID))
              {
                  offsetID <- if(length(object@ID)) max(object@ID) else 0L
                  ID <- offsetID + seq_along(y)
              }
              object@ID <- c(object@ID, ID)

              ## add cohort number
              object@cohort <- c(object@cohort,
                                 rep(max(tail(object@cohort, 1L), 0L) + 1L,
                                     length(y)))

              ## return the object
              return(object)
          })

## ============================================================














