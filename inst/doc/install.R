#####################################################################################
## Author: Daniel Sabanes Bove [sabanesd *a*t* roche *.* com]
## Project: install script for crmPack
##
## Time-stamp: <[install.R] by DSB Mon 11/05/2015 17:19>
##
## Description:
## This is an install script to get crmPack onto your computer.
##
## History:
## 09/12/2014   file creation
## 16/01/2015   updates
## 01/05/2015   updates (gran)
## 11/05/2015   update: don't spell out the names of the package crmPack
##              depends on
#####################################################################################

## install and load required package
options(repos=
        c(CRAN="http://stat.ethz.ch/CRAN",
          GRAN="http://gran.roche.com/packages"))

install.packages("crmPack",
                 type="both",
                 dependencies=TRUE)



