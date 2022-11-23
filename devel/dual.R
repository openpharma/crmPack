# nolint start

#####################################################################################
## Author: Daniel Sabanes Bove [sabanesd *a*t* roche *.* com]
## Project: Object-oriented implementation of CRM designs
##
## Time-stamp: <[dual.R] by DSB Son 11/01/2015 14:55>
##
## Description:
## Test the dual endpoint stuff. For development only!!
##
## History:
## 24/03/2014   file creation
## 22/12/2014   test the new JAGS implementation
###################################################################################

source("../R/helpers.R")
source("../R/Model-class.R")

## set up the model
model <- DualEndpointRW(mu=c(0, 1),
                        Sigma=matrix(c(1, 0, 0, 1), nrow=2),
                        sigma2betaW=
                        0.01,
                        ## c(a=20, b=50), ## gives very unstable results!!
                        sigma2W=
                        c(a=0.1, b=0.1),
                        rho=
                        c(a=1, b=1),
                        ## c(a=20, b=10)
                        smooth="RW1")

model <-
    DualEndpointBeta(E0=0.001,
                     Emax=c(0.51, 1.5),
                     delta1=c(0, 100),
                     mode=c(0, 90),
                     refDose=100,
                     mu=c(0, 1),
                     Sigma=matrix(c(1, 0, 0, 1), nrow=2),
                     sigma2W=
                     c(a=0.1, b=0.1),
                     rho=
                     c(a=1, b=1))

library(DoseFinding)
curve(betaMod(x, e0=0.2, eMax=0.6, delta1=5, delta2=5 * 0.9 / 0.1, scal=100),
      from=0, to=50)
curve(betaMod(x, e0=0.2, eMax=0.6, delta1=1, delta2=1 * 0.9 / 0.1, scal=100),
      from=0, to=50)


source("../R/Data-class.R")
source("../R/Data-methods.R")
## create some test data
data <- new("DataDual",
            x=
            c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10,
              20, 20, 20, 40, 40, 40, 50, 50, 50),
            y=
            as.integer(c(0, 0, 0, 0, 0, 0, 1, 0,
                         0, 1, 1, 0, 0, 1, 0, 1, 1)),
            w=
            c(0.3, 0.4, 0.5, 0.4, 0.6, 0.7, 0.5, 0.6,
              0.5, 0.5, 0.55, 0.4, 0.41, 0.39, 0.3, 0.3, 0.2),
            doseGrid=
            c(0.1, 0.5, 1.5, 3, 6,
              seq(from=10, to=80, by=2)))
data
data@nGrid
data@nObs

library(ggplot2)
plot(data)

source("../R/McmcOptions-class.R")
source("../R/McmcOptions-methods.R")
## and some MCMC options
options <- new("McmcOptions",
               burnin=10000,
               step=2,
               samples=50000)


source("../R/mcmc.R")
source("../R/helpers.R")
source("../R/Samples-class.R")

## obtain the samples
library(rjags)
samples <- mcmc(data, model, options, verbose=TRUE)

str(samples)

plot(samples@data$betaW[, 40], type="l")
plot(samples@data$betaW[, 30], type="l")
## ok, so we don't have convergence for RW2 at least with JAGS!
## there is convergence with WinBUGS however.

source("../R/Samples-methods.R")


## use the ggmcmc package for convergence checks. we provide the extract function
## for this purpose:
library(ggmcmc)
betaZ <- get(samples, "betaZ")
str(betaZ)

ggs_traceplot(betaZ)
ggs_density(betaZ)
ggs_autocorrelation(betaZ)
ggs_running(betaZ)

mode <- get(samples, "mode")
ggs_traceplot(mode)

delta1 <- get(samples, "delta1")
ggs_traceplot(delta1)

E0 <- get(samples, "E0")
ggs_traceplot(E0)

Emax <- get(samples, "Emax")
ggs_traceplot(Emax)

ggs_traceplot(get(samples, "precW"))

betaW <- get(samples, "betaW", 6:10)
str(betaW)

ggs_traceplot(betaW)

rho <- get(samples, "rho")
ggs_traceplot(rho)
plot(rho)
ggs_histogram(rho)

## ok now we want to plot the fit:
source("../R/Model-methods.R")

plot(samples, model, data)
x11()
plot(samples, model, data, extrapolate=FALSE)

betaModList <- list(betaMod = rbind(c(1,1), c(1.5,0.75), c(0.8,2.5), c(0.4,0.9)))
plotModels(betaModList, c(0,1), base = 0, maxEff = 1, scal = 1.2)


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
                  stop_list=
                  list(new("StoppingAll",
                           stop_list=
                           list(myStopping1,
                                myStopping3)),
                       myStopping2))

## or much more intuitively:
myStoppingEasy <- (myStopping1 & myStopping3) | myStopping2
myStoppingEasy
identical(myStopping, myStoppingEasy)


stopTrial(myStopping, dose=nextDose$value,
          samples, model, data)

## relative increments:
myIncrements <- new("IncrementsRelative",
                    intervals=c(0, 20, Inf),
                    increments=c(1, 0.33))

## test design
source("../R/Design-class.R")
design <- new("DualDesign",
              model=model,
              nextBest=myNextBest,
              stopping=myStopping,
              increments=myIncrements,
              data=
              new("DataDual",
                  x=numeric(),
                  y=integer(),
                  w=numeric(),
                  doseGrid=
                  c(0.1, 0.5, 1.5, 3, 6,
                    seq(from=10, to=80, by=2))),
              cohortSize=new("CohortSizeConst", size=3L),
              startingDose=6)



## for testing the simulate function:
## object <- design
## truth <- model@prob
## ## args <- list(rho0=0.1,
## ##              gamma=20)
## args <- list(alpha0=0,
##              alpha1=1)
## nsim <- 10L
mcmcOptions <- new("McmcOptions")
seed <- 23

## iterSim <- 1L


source("../R/Simulations-class.R")
source("../R/Design-methods.R")

betaMod <-
    function (dose, e0, eMax, delta1, delta2, scal)
{
    maxDens <- (delta1^delta1) * (delta2^delta2)/((delta1 + delta2)^(delta1 +
                                                                     delta2))
    dose <- dose/scal
    e0 + eMax/maxDens * (dose^delta1) * (1 - dose)^delta2
}
##trace(simulate, browser, signature=c("DualDesign"))
trueTox <- function(dose){
    pnorm((dose-60)/10)
}
trueBiomarker <- function(dose){
    betaMod(dose, e0=0.2, eMax=0.6, delta1=5, delta2=5 * 0.5 / 0.5, scal=100)
}
mySims <- simulate(design,
                   trueTox=trueTox,
                   trueBiomarker=trueBiomarker,
                   sigma2W=0.001,
                   rho=0,
                   nsim=3L,
                   firstSeparate=FALSE,
                   parallel=FALSE,
                   seed=3)
## todo: check parallel=TRUE in package version of this dual.R

source("../R/Simulations-methods.R")

str(mySims, 2)
str(mySims@fitBiomarker, 2)
identical(mySims@fitBiomarker[[1]],
          mySims@fitBiomarker[[2]])
identical(mySims@fitBiomarker[[2]],
          mySims@fitBiomarker[[3]])

plot(mySims@fitBiomarker[[1]]$middleBiomarker, type="l")

mySims@stopReasons
plot(mySims, type=c("dose", "rho"))
plot(mySims)


## look at simulated trial outcomes:
plot(mySims@data[[2]])
mySims@stopReasons[[3]]

## final MTDs
mySims@doses



## extract operating characteristics


## the truth we want to compare it with:
sumOut <- summary(mySims,
                  trueTox=trueTox,
                  trueBiomarker=trueBiomarker)
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

# nolint end
