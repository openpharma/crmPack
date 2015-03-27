#####################################################################################
## Author: Daniel Sabanes Bove [sabanesd *a*t* roche *.* com]
## Project: Object-oriented implementation of CRM designs
##
## Time-stamp: <[helpers.R] by DSB Fre 27/03/2015 14:15>
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

##' Check overlap of two character vectors
##'
##' @param a first character vector
##' @param b second character vector
##' @return returns TRUE if there is no overlap between the two character
##' vectors, otherwise FALSE
##'
##' @keywords internal
noOverlap <- function(a, b)
{
    identical(intersect(a, b),
              character(0))
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


##' checks for whole numbers (integers)
##'
##' @param x the numeric vector
##' @param tol the tolerance
##' @return TRUE or FALSE for each element of x
##'
##' @keywords internal
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)
{
    abs(x - round(x)) < tol
}


##' Safe conversion to integer vector
##'
##' @param x the numeric vector
##' @return the integer vector
##'
##' @keywords internal
safeInteger <- function(x)
{
    testres <- is.wholenumber(x)
    if(! all(testres))
    {
        notInt <- which(! testres)
        stop(paste("elements",
                   paste(notInt, sep=", "),
                   "of vector are not integers!"))
    }
    as.integer(x)
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
    printVignette(list(PDF="example.pdf", Dir=crmPath))
    ## instead of utils:::print.vignette
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


##' Plots arrange objects
##'
##' @method plot arrange
##' @param x the arrange object
##' @param \dots additional parameters for \code{\link[grid]{grid.draw}}
##'
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


## todo: use this function layOut (orig from wq package)?
## function (...)
## {
##     require(grid)
##     x <- list(...)
##     n <- max(sapply(x, function(x) max(x[[2]])))
##     p <- max(sapply(x, function(x) max(x[[3]])))
##     pushViewport(viewport(layout = grid.layout(n, p)))
##     for (i in seq_len(length(x))) {
##         print(x[[i]][[1]], vp = viewport(layout.pos.row = x[[i]][[2]],
##             layout.pos.col = x[[i]][[3]]))
##     }
## }

##' Taken from utils package (print.vignette)
##'
##' @importFrom tools file_ext
##' @importFrom utils browseURL
##' @keywords internal
printVignette <- function (x, ...)
{
    if (nzchar(out <- x$PDF)) {
        ext <- tools::file_ext(out)
        out <- file.path(x$Dir, "doc", out)
        if (tolower(ext) == "pdf") {
            pdfviewer <- getOption("pdfviewer")
            if (identical(pdfviewer, "false")) {
            }
            else if (.Platform$OS.type == "windows" && identical(pdfviewer,
                file.path(R.home("bin"), "open.exe")))
                shell.exec(out)
            else system2(pdfviewer, shQuote(out), wait = FALSE)
        }
        else browseURL(out)
    }
    else {
        warning(gettextf("vignette %s has no PDF/HTML", sQuote(x$Topic)),
            call. = FALSE, domain = NA)
    }
    invisible(x)
}


##' A Reference Class to help programming validation for new S4 classes
##'
##' Starting from an empty \code{msg} vector, with each check that is returning
##' FALSE the vector gets a new element - the string explaining the failure of
##' the validation
##'
##' @name Validate
##' @field msg the message character vector
Validate <-
    setRefClass("Validate",
                fields =
                list(msg = "character"),
                methods = list(
                check =
                    function(test,
                             string){
                        if(test)
                        {} else {
                            msg <<- c(msg,
                                      string)
                        }
                    },
                result =
                    function() {
                        if(length(msg) > 0) msg else TRUE
                    }))

