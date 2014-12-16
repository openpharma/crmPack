#####################################################################################
## Author: Daniel Sabanes Bove [sabanesd *a*t* roche *.* com]
## Project: Object-oriented implementation of CRM designs
##
## Time-stamp: <[helpers.R] by DSB Die 16/12/2014 17:45>
##
## Description:
## Some helper functions
##
## History:
## 18/12/2013   file creation
## 19/12/2013   add is.bool from glmBfp
## 31/01/2014   copied from adaptive package
## 11/02/2014   add logit function
## 16/12/2014   add plot method for arrange objects, that we generate by calling
##              gridExtra::arrangeGrob for combining ggplot2 objects
#####################################################################################


##' Helper function to join two function bodies
##'
##' @param body1 first body
##' @param body2 second body
##' @return joined body
##'
##' @keywords internal programming
joinBodies <- function(body1, body2)
{
    lenBody1 <- length(body1)
    lenBody2 <- length(body2)
    for(i in seq_len(lenBody2 - 1L))
    {
        body1[[lenBody1 + i]] <- body2[[1L + i]]
    }
    return(body1)
}

##' Helper function to join two BUGS models
##'
##' @param model1 first model
##' @param model2 second model
##' @return joined model
##'
##' @keywords internal programming
joinModels <- function(model1, model2)
{
    body1 <- body(model1)
    body2 <- body(model2)
    body(model1) <- joinBodies(body1, body2)
    return(model1)
}

##' Checking for scalar
##'
##' @param x the input
##' @return Returns \code{TRUE} if \code{x} is a length one vector
##' (i.e., a scalar)
##'
##' @keywords internal
is.scalar <- function(x)
{
    return(identical(length(x), 1L))
}


##' Predicate checking for a boolean option
##'
##' @param x the object being checked
##' @return Returns \code{TRUE} if \code{x} is a length one logical vector (i.e., a
##' scalar)
##'
##' @keywords internal
is.bool <- function(x)
{
    return(is.scalar(x) &&
           is.logical(x))
}

##' Predicate checking for a probability
##'
##' @param x the object being checked
##' @param bounds whether to include the bounds 0 and 1 (default)
##' @return Returns \code{TRUE} if \code{x} is a probability
##'
##' @keywords internal
is.probability <- function(x,
                           bounds=TRUE)
{
    return(is.scalar(x) &&
           if(bounds){
               0 <= x && 1 >= x
           } else {
               0 < x && 1 > x
           })
}

##' Predicate checking for a probability range
##'
##' @param x the object being checked
##' @param bounds whether to include the bounds 0 and 1 (default)
##' @return Returns \code{TRUE} if \code{x} is a probability range
##'
##' @keywords internal
is.probRange <- function(x,
                         bounds=TRUE)
{
    return(identical(length(x), 2L) &&
           x[1] < x[2] &&
           if(bounds){
               0 <= x[1] && 1 >= x[2]
           } else {
               0 < x[1] && 1 > x[2]
           })
}


##' Shorthand for logit function
##'
##' @param x the function argument
##' @return the logit(x)
##'
##' @export
##' @keywords programming
logit <- function(x)
{
    qlogis(x)
}

##' Open the example pdf for crmPack
##'
##' Calling this helper function should open the example.pdf document,
##' residing in the doc subfolder of the package installation directory.
##'
##' @return nothing
##' @export
##' @keywords documentation
##' @author Daniel Sabanes Bove \email{sabanesd@@roche.com}
crmPackExample <- function()
{
    crmPath <- system.file(package="crmPack")
    utils:::print.vignette(list(PDF="example.pdf", Dir=crmPath))
}

##' Open the browser with help pages for crmPack
##'
##' This convenience function opens your browser with the help pages for
##' crmPack.
##'
##' @return nothing
##' @export
##' @importFrom utils help
##' @keywords documentation
##' @author Daniel Sabanes Bove \email{sabanesd@@roche.com}
crmPackHelp <- function()
{
    utils::help(package="crmPack", help_type="html")
}

##' @importFrom grid grid.draw
##' @export
plot.arrange <- function(x, ...)
{
    grid::grid.draw(x, ...)
}

##' @export
print.arrange <- function(x, ...)
{
    plot.arrange(x, ...)
}
