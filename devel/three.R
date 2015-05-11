#####################################################################################
## Author: Daniel Sabanes Bove [sabanesd *a*t* roche *.* com]
## Project: Object-oriented implementation of CRM designs
##
## Time-stamp: <[three.R] by DSB Son 03/05/2015 20:30>
##
## Description:
## Test the 3+3 stuff. For development only!!
##
## History:
## 30/12/2014   file creation
## 03/05/2015   update with new syntax
###################################################################################

source("../R/helpers.R")
source("../R/Data-class.R")
source("../R/Data-methods.R")

## create some test data
data <- Data(x=
                 c(0.1, 0.1, 0.1,
                   10, 10, 10,
                   10, 10, 10,
                   20, 20, 20,
                   30, 30, 30,
                   60, 60, 60,
                   60, 60, 60),
             y=
                 as.integer(c(1, 1, 0,
                              0, 0, 1,
                              0, 0, 0,
                              0, 0, 0,
                              0, 0, 0,
                              0, 0, 0,
                              1, 0, 0
                              )),
             doseGrid=
                 c(0.1, 10, 20, 30, 60))
data
data@nGrid

library(ggplot2)
plot(data)

## now on to the rules:
source("../R/Model-class.R")
source("../R/McmcOptions-class.R")
source("../R/McmcOptions-methods.R")
source("../R/mcmc.R")
source("../R/writeModel.R")
source("../R/Samples-class.R")
source("../R/Rules-class.R")
source("../R/Rules-methods.R")

## next best method
myNextBest <- NextBestThreePlusThree()

nextDose <- nextBest(myNextBest, data=data)
nextDose$value
nextDose$stopHere
data


## test design
emptydata <- Data(doseGrid=c(0.1, 10, 20, 30, 60))

source("../R/Design-class.R")
design <- RuleDesign(
              nextBest=myNextBest,
              data=emptydata,
              cohortSize=
              CohortSizeConst(size=3L),
              ## using a constant cohort size of 3,
              ## we obtain exactly the 3+3 design
              startingDose=0.1)

## alternative:
design <- ThreePlusThreeDesign(emptydata@doseGrid)



## ----------------------------------------
## todo: cont here


## for testing the simulate function:
object <- design
truth <- function(dose, alpha0, alpha1)
{
    plogis(alpha0 + alpha1 * dose)
}

## args <- list(rho0=0.1,
##              gamma=20)
args <- list(alpha0=-10,
             alpha1=0.3)

curve(truth(x, args$alpha0, args$alpha1), from=0.1, to=60)
nsim <- 10L
seed <- 23

## iterSim <- 1L


source("../R/Simulations-class.R")
source("../R/Design-methods.R")

mySims <- simulate(design,
                   truth=truth,
                   args=args,
                   nsim=10L)

str(mySims)

## look at simulated trial outcomes:
plot(mySims@data[[9]])
plot(mySims@data[[2]])


## final MTDs
mySims@doses

## what is the true prob at these doses?
truth(mySims@doses,
      args$alpha0, args$alpha1)


## extract operating characteristics
source("../R/Simulations-methods.R")

## the truth we want to compare it with:
myTruth <- function(dose){truth(dose, args$alpha0, args$alpha1)}
curve(myTruth(x), from=0, to=60)

sumOut <- summary(mySims,
                  truth=myTruth)
sumOut

mySims@doses


## make nice plots for simulation output:
## first from the summary object
str(sumOut)

plot(sumOut)
plot(mySims)

plot(sumOut, type=c("nObs"))

## now from the raw simulation output
str(mySims@data)

plot(mySims)

## nice!
