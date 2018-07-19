
<!-- README.md is generated from README.Rmd. Please edit that file -->
crmPack
=======

[![Build Status](https://travis-ci.org/Roche/crmPack.svg?branch=master)](https://travis-ci.org/Roche/crmPack/)

The goal of crmPack is to implement a wide range of model-based dose escalation designs, ranging from classical and modern continual reassessment methods (CRMs) based on dose-limiting toxicity endpoints to dual-endpoint designs taking into account a biomarker/efficacy outcome. The focus is on Bayesian inference, making it very easy to setup a new design with your own JAGS code. However, it is also possible to implement 3+3 designs for comparison or models with non-Bayesian estimation. The whole package is written in a modular form in the S4 class system, making it very flexible for adaptation to new models, escalation or stopping rules.

Installation
------------

You can install the development version of crmPack from github with:

``` r
# install.packages("devtools")
devtools::install_github("Roche/crmPack")
```

You can install the stable release version of crmPack from CRAN with:

``` r
install.packages("crmPack")
```

Example
-------

This is a basic example which shows how to run simulations from a CRM with a 2-parameter logistic regression model, using a log normal prior distribution, and custom cohort size, stopping and maximum increments rules:

``` r
library(crmPack)
#> Warning: package 'crmPack' was built under R version 3.4.4
#> Loading required package: ggplot2
#> Warning: package 'ggplot2' was built under R version 3.4.4
#> Type crmPackHelp() to open help browser
#> Type crmPackExample() to open example

# Define the dose-grid
emptydata <- Data(doseGrid = c(1, 3, 5, 10, 15, 20, 25, 40, 50, 80, 100))

# Initialize the CRM model 
model <- LogisticLogNormal(mean=c(-0.85, 1),
                           cov=
                             matrix(c(1, -0.5, -0.5, 1),
                                    nrow=2),
                           refDose=56)

# Choose the rule for selecting the next dose 
myNextBest <- NextBestNCRM(target=c(0.2, 0.35),
                           overdose=c(0.35, 1),
                           maxOverdoseProb=0.25)

# Choose the rule for the cohort-size 
mySize1 <- CohortSizeRange(intervals=c(0, 30),
                           cohortSize=c(1, 3))
mySize2 <- CohortSizeDLT(DLTintervals=c(0, 1),
                         cohortSize=c(1, 3))
mySize <- maxSize(mySize1, mySize2)

# Choose the rule for stopping
myStopping1 <- StoppingMinCohorts(nCohorts=3)
myStopping2 <- StoppingTargetProb(target=c(0.2, 0.35),
                                  prob=0.5)
myStopping3 <- StoppingMinPatients(nPatients=20)
myStopping <- (myStopping1 & myStopping2) | myStopping3

# Choose the rule for dose increments
myIncrements <- IncrementsRelative(intervals=c(0, 20),
                                   increments=c(1, 0.33))

# Initialize the design
design <- Design(model=model,
                 nextBest=myNextBest,
                 stopping=myStopping,
                 increments=myIncrements,
                 cohortSize=mySize,
                 data=emptydata,
                 startingDose=3)

## define the true function
myTruth <- function(dose)
{
  model@prob(dose, alpha0=7, alpha1=8)
}

# Run the simulation on the desired design
# We only generate 1 trial outcomes here for illustration, for the actual study 
# this should be increased of course
options <- McmcOptions(burnin=100,
                       step=1,
                       samples=2000)
time <- system.time(mySims <- simulate(design,
                                       args=NULL,
                                       truth=myTruth,
                                       nsim=1,
                                       seed=819,
                                       mcmcOptions=options,
                                       parallel=FALSE))[3]
```

Presentations
-------------

-   [BBS Seminar 26 June 2017](http://bbs.ceb-institute.org/wp-content/uploads/2017/07/BBS-dose-escalation-Sabanes-26-June-2017-v2.pdf)
-   [BaselR 6 December 2016](https://drive.google.com/open?id=148Ww7LVLBEKEHnWrISWzEchIkWahSjLE)
