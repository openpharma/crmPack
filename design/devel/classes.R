#####################################################################################
## Author: Daniel Sabanes Bove [sabanesd *a*t* roche *.* com]
## Project: crmPack
##
## Time-stamp: <[classes.R] by DSB Fre 02/01/2015 16:43>
##
## Description:
## try to reproduce initialization problems
##
## History:
## 02/01/2015   file creation
#####################################################################################

setClass(
  Class = "A",
  representation = representation(x = "numeric"),
  validity = function(object) {
    stopifnot(object@x > 0)
  }
)

setMethod(
  "initialize",
  signature(.Object = "A"),
  function(.Object, ..., z) {
    x <- get("z") + 1
    callNextMethod(.Object, ..., x = x)
  }
)

setClass(
  Class = "B",
  contains = "A",
  representation = representation(y = "numeric"),
  validity = function(object) {
    stopifnot(object@y > 0)
  }
)

setMethod(
  "initialize",
  signature(.Object = "B"),
  function(.Object, ..., bla) {
    .Object <- callNextMethod(
      .Object,
      ...
    )

    .Object@y <- .Object@x + bla
    return(.Object)
  }
)

test <- new("B", z = 4, bla = 5)
test
source("../devel/classes.R")
