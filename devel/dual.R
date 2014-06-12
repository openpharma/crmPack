#####################################################################################
## Author: Daniel Sabanes Bove [sabanesd *a*t* roche *.* com]
## Project: Object-oriented implementation of CRM designs
##
## Time-stamp: <[dual.R] by DSB Mon 31/03/2014 16:17>
##
## Description:
## Test the dual endpoint stuff. For development only!!
##
## History:
## 24/03/2014   file creation
###################################################################################

source("../R/Model-class.R")
source("../R/helpers.R")
## set up the model

model <- new("DualEndpoint",
             mu=c(0, 1),
             Sigma=matrix(c(1, 0, 0, 1), nrow=2),
             sigma2betaW=
             0.1,
             ## c(a=20, b=50), ## gives very unstable results!!
             sigma2W=
             c(a=0.1, b=0.1),
             rho=
             c(a=1, b=1),
             ## c(a=20, b=10)
             smooth="RW2")

source("../R/Data-class.R")
source("../R/Data-methods.R")
## create some test data
data <- new("DataDual",
            x=
            c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
            y=
            as.integer(c(0, 0, 0, 0, 0, 0, 1, 0)),
            w=
            c(0.3, 0.4, 0.5, 0.4, 0.6, 0.7, 0.5, 0.6),
            doseGrid=
            c(0.1, 0.5, 1.5, 3, 6,
              seq(from=10, to=80, by=2)))
data

library(ggplot2)
plot(data)

source("../R/McmcOptions-class.R")
source("../R/McmcOptions-methods.R")
## and some MCMC options
options <- new("McmcOptions",
               burnin=100,
               step=2,
               samples=1000)


source("../R/mcmc.R")
source("../R/Samples-class.R")

## obtain the samples
samples <- mcmc(data, model, options,
                program="WinBUGS")

str(samples)

## it only works with WinBUGS!! But not with the default "OpenBUGS".
source("../R/Samples-methods.R")


## use the ggmcmc package for convergence checks. we provide the extract function
## for this purpose:
library(ggmcmc)
betaZ <- extract(samples, "betaZ")
str(betaZ)

ggs_traceplot(betaZ)
ggs_density(betaZ)
ggs_autocorrelation(betaZ)
ggs_running(betaZ)

rho <- extract(samples, "rho")
ggs_traceplot(rho)
plot(rho)
ggs_histogram(rho)

## ok now we want to plot the fit:
plot(samples, model, data)
plot(samples, model, data, extrapolate=FALSE)




## now on to the rules:

source("../R/helpers.R")
source("../R/Rules-class.R")
source("../R/Rules-methods.R")

## target level is 90% of maximum biomarker level
## overdose tox interval is 35%+
myNextBest <- new("NextBestDualEndpoint",
                  target=0.9,
                  overdose=c(0.35, 1),
                  maxOverdoseProb=0.25)

nextDose <- nextBest(myNextBest, doselimit=50, samples=samples, model=model, data=data)
nextDose$plot
nextDose$value
data


## stopping rule:
## min 3 cohorts and at least 50% prob in for targeting biomarker,
## or max 20 patients
myStopping1 <- new("StoppingMinCohorts",
                   nCohorts=3L)
myStopping2 <- new("StoppingMaxPatients",
                   nPatients=50L)
myStopping3 <- new("StoppingTargetBiomarker",
                   target=0.9,
                   prob=0.5)

## you can either write this:
myStopping <- new("StoppingAny",
                  stopList=
                  list(new("StoppingAll",
                           stopList=
                           list(myStopping1,
                                myStopping3)),
                       myStopping2))

## or much more intuitively:
myStoppingEasy <- (myStopping1 & myStopping3) | myStopping2
myStoppingEasy
identical(myStopping, myStoppingEasy)


## relative increments:
myIncrements <- new("IncrementsRelative",
                    intervals=c(0, 20, Inf),
                    increments=c(1, 0.33))

## test design
source("../R/Design-class.R")
design <- new("Design",
              model=model,
              nextBest=myNextBest,
              stopping=myStopping,
              increments=myIncrements,
              data=data,
              cohortSize=3L,
              startingDose=12)


## ----------------------------------------
## todo: cont here


## for testing the simulate function:
object <- design
truth <- model@prob
## args <- list(rho0=0.1,
##              gamma=20)
args <- list(alpha0=0,
             alpha1=1)
nsim <- 10L
mcmcOptions <- new("McmcOptions")
seed <- 23

## iterSim <- 1L


source("../R/Simulations-class.R")
source("../R/simulate.R")

mySims <- simulate(design,
                   truth=truth,
                   args=args,
                   nsim=10L,
                   mcmcOptions=mcmcOptions)

str(mySims)

## look at simulated trial outcomes:
plot(mySims@data[[8]])
plot(mySims@data[[5]])
mySims@stopReasons[[5]]

## final MTDs
mySims@doses

## what is the true prob at these doses?
truth(mySims@doses,
      alpha0=0,
      alpha1=1)


## extract operating characteristics
source("../R/Simulations-methods.R")

## the truth we want to compare it with:
myTruth <- function(dose){model@prob(dose, alpha0=0, alpha1=1)}
curve(myTruth(x), from=0, to=30)

sumOut <- summary(mySims,
                  truth=myTruth)
sumOut

mySims@doses


## make nice plots for simulation output:
## first from the summary object
str(sumOut)

plot(sumOut)

plot(sumOut, type="meanFit")

plot(sumOut, type=c("nObs", "meanFit"))

## now from the raw simulation output
str(mySims@data)

plot(mySims)

