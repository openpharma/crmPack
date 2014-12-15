#####################################################################################
## Author: Daniel Sabanes Bove [sabanesd *a*t* roche *.* com]
## Project: install script for crmPack
##
## Time-stamp: <[install.R] by DSB Mon 15/12/2014 21:10>
##
## Description:
## This is an install script to get crmPack onto your computer.
##
## History:
## 09/12/2014   file creation
#####################################################################################

## install and load required package
options(repos=structure(c(CRAN="http://stat.ethz.ch/CRAN/")))
install.packages("httr")
library("httr")

## specify source and target (temporary file in tmp)
url <-
    paste("https://stash.intranet.roche.com/stash/plugins/servlet/archive/projects/RSTAT",
          "/repos/crmpack?at=refs%2Fheads%2Fmaster",
          sep="")
tmp <- tempdir()
target <- file.path(tmp, "crmpack.zip")

## download from source to target
x <- GET(url, config = list(ssl.verifypeer = FALSE))
bin <- content(x, "raw")
writeBin(bin, target)

## unzip to a temporary directory below tmp
dir <- file.path(tmp, "crmpack")
unzip(zipfile=target, exdir=dir)

## install required packages and crmPack
## descr <- packageDescription(pkg="crmpack", lib.loc = tmp)
install.packages(c("Rcpp", "RcppArmadillo", "rjags", "ggplot2", "gridExtra",
                   "GenSA", "BayesLogit", "mvtnorm"), dependencies=TRUE)
install.packages(dir, repos=NULL, type="source")

test <- system.file(package="crmPack")
utils:::print.vignette(list(PDF="example.pdf", Dir=test))
