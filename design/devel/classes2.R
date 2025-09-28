#####################################################################################
## Author: Daniel Sabanes Bove [sabanesd *a*t* roche *.* com]
## Project: crmPack
##
## Time-stamp: <[classes2.R] by DSB Fre 02/01/2015 18:17>
##
## Description:
## try to reproduce initialization problems
##
## History:
## 02/01/2015   file creation
#####################################################################################

.A <- setClass(
  Class = "A",
  representation(x = "numeric"),
  prototype(x = 1),
  validity = function(object) {
    msg <- NULL
    if (length(object@x) != 1 || object@x <= 0) {
      msg <- c(msg, "'x' must be length 1 and > 0")
    }
    if (is.null(msg)) TRUE else msg
  }
)

validObject(.A())

A <- function(z, ...) {
  x <- z + 1
  .A(x = x, ...)
}

.B <- setClass(
  Class = "B",
  representation(y = "numeric"),
  prototype(y = 2),
  contains = "A",
  validity = function(object) {
    msg <- NULL
    if (length(object@y) != 1 || object@y <= 0) {
      msg <- c(msg, "'y' must be length 1 and > 0")
    }
    if (is.null(msg)) TRUE else msg
  }
)

validObject(.B())

B <- function(bla, z, ...) {
  obj <- A(z, ...)
  y <- obj@x + bla
  .B(obj, y = y, ...)
}

test <- B(
  z = 4,
  bla = 5
)
test
