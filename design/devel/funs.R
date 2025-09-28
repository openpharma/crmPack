#####################################################################################
## Author: Daniel Sabanes Bove [sabanesd *a*t* roche *.* com]
## Project: crmPack
##
## Time-stamp: <[funs.R] by DSB Fre 02/01/2015 10:41>
##
## Description:
## Test environment things with functions
##
## History:
## 02/01/2015   file creation
#####################################################################################

afun <- function(x) {
  print(ls(envir = parent.frame()))
}

bfun <- function(y) {
  afun(5 + y)
}

bfun(3)


f <- function() {
  do.call("on.exit", list(quote(cat("ONEXIT!\n"))), envir = parent.frame())
  42
}

g <- function() {
  x <- f()
  cat("Not yet!\n")
  x
}

g()
