#####################################################################################
## Author: Daniel Sabanes Bove [sabanesd *a*t* roche *.* com]
## Project: Object-oriented implementation of CRM designs
##
## Time-stamp: <[projectWithPackage.R] by DSB Die 01/04/2014 09:23>
##
## Description:
## Test in the setup of the project. For development only!!
##
## History:
## 06/02/2014   file creation
###################################################################################

library(crmPack)

## model <- new("LogisticNormal",
##              mean=c(-0.85, 1),
##              cov=
##              matrix(c(1, -0.5, -0.5, 1),
##                     nrow=2L),
##              refDose=56)
## model



model <- new("LogisticKadane",
  theta = 0.33,
  xmin = 0.1,
  xmax = 100
)
model



## create some test data
data <- new("Data",
  x =
    c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
  y =
    as.integer(c(0, 0, 0, 0, 0, 0, 1, 0)),
  cohort =
    as.integer(c(0, 1, 2, 3, 4, 5, 5, 5)),
  doseGrid =
    c(
      0.1, 0.5, 1.5, 3, 6,
      seq(from = 10, to = 80, by = 2)
    )
)

## data <- new("Data",
##             x=
##             numeric(),
##             y=
##             as.integer(c()),
##             cohort=
##             as.integer(c()),
##             doseGrid=
##             c(0.1, 0.5, 1.5, 3, 6,
##               seq(from=10, to=80, by=2)))

data
plot(data)

## and some MCMC options
options <- new("McmcOptions",
  burnin = 100,
  step = 2,
  samples = 1000
)


## obtain the samples
samples <- mcmc(data, model, options)
str(samples)

## extract samples for diagnostic plots
rho0 <- extract(samples, "rho0")

## use other package for plotting
library(ggmcmc)
ggs_traceplot(rho0)
ggs_autocorrelation(rho0)
ggs_density(rho0)
## etc.


## ok now we want to plot the fit:
str(samples)

plot(samples, model, data)
data@doseGrid

## now on to the rules:

## relative increments:
myIncrements <- new("IncrementsRelative",
  intervals = c(0, 20, Inf),
  increments = c(1, 0.33)
)

nextMaxDose <- maxDose(myIncrements,
  data = data
)
nextMaxDose

## target tox rate is 33%.
## 25% quantile of posterior distribution is used
## and 90% one-sided CI must be above 50% of current MTD estimate,
## that is the probability that the MTD is above 50% of the current estimate
## must be larger than 90%
myNextBest <- new("NextBestMTD",
  target = 0.33,
  derive =
    function(mtdSamples) {
      quantile(mtdSamples, probs = 0.25)
    }
)

res <- nextBest(myNextBest,
  doselimit = nextMaxDose, samples = samples, model = model,
  data = data
)
res$value
res$plot

## target tox interval is 20-35%.
## overdose tox interval is 35%+
## required prob for target is 25%
myNextBest <- new("NextBestNCRM",
  target = c(0.2, 0.35),
  overdose = c(0.35, 1),
  maxOverdoseProb = 0.25
)

res <- nextBest(myNextBest,
  doselimit = nextMaxDose, samples = samples, model = model,
  data = data
)
res$value
res$plot



## test quantiles function
set.seed(92)
quantTest <-
  Quantiles2LogisticNormal(
    dosegrid = 1:5,
    refDose = 2.5,
    lower = c(0.01, 0.02, 0.05, 0.1, 0.3),
    upper = c(0.5, 0.6, 0.7, 0.8, 0.95),
    median = c(0.2, 0.3, 0.4, 0.45, 0.5),
    control =
      list(
        threshold.stop = 0.01,
        maxit = 50000,
        temperature = 50000,
        max.time = 10,
        verbose = TRUE
      )
  )
str(quantTest)

matplot(quantTest$required,
  type = "l", col = "blue", lty = 1
)
matlines(quantTest$quantiles,
  col = "red", lty = 1
)

## test minimally informative
minTest <- MinimalInformative(
  dosegrid = 1:5,
  refDose = 2.5,
  control =
    list(
      threshold.stop = 0.01,
      maxit = 50000,
      temperature = 50000,
      max.time = 20,
      verbose = TRUE
    )
)

minTest <- MinimalInformative(
  dosegrid = 1:5,
  refDose = 2.5,
  parstart = minTest$parameters,
  control =
    list(
      threshold.stop = 0.01,
      maxit = 50000,
      temperature = 50000,
      max.time = 20,
      verbose = TRUE
    )
)
str(minTest)

matplot(minTest$required,
  type = "l", col = "blue", lty = 1
)
matlines(minTest$quantiles,
  col = "red", lty = 1
)


## stopping rule:
## min 3 cohorts and at least 50% prob in target interval,
## or max 20 patients
myStopping1 <- new("StoppingMinCohorts",
  nCohorts = 3L
)
myStopping2 <- new("StoppingMaxPatients",
  nPatients = 20L
)
myStopping3 <- new("StoppingTargetProb",
  target = c(0.2, 0.35),
  prob = 0.5
)

## easy syntax:
myStopping <- (myStopping1 & myStopping3) | myStopping2


## relative increments:
myIncrements <- new("IncrementsRelative",
  intervals = c(0, 20, Inf),
  increments = c(1, 0.33)
)

## test design
design <- new("Design",
  model = model,
  nextBest = myNextBest,
  stopping = myStopping,
  increments = myIncrements,
  data = data,
  cohort_size = 3L,
  startingDose = 12
)


myTruth <- function(dose) {
  model@prob(dose, rho0 = 0.2, gamma = 10)
}
curve(myTruth(x), from = 0, to = 30)

mySims <- simulate(design,
  truth = myTruth,
  args = list(),
  nsim = 10L,
  mcmcOptions = options
)

str(mySims)

## look at simulated trial outcomes:
plot(mySims@data[[8]])
plot(mySims@data[[5]])
mySims@doses[[8]]
mySims@stopReasons[[5]]

## final MTDs
mySims@doses

## what is the true prob at these doses?
myTruth(mySims@doses)


## extract operating characteristics

## the truth we want to compare it with:

sumOut <- summary(mySims,
  truth = myTruth
)
sumOut
## todo: write output function

mySims@doses


## make nice plots for simulation output
str(sumOut)

plot(sumOut)

## we can also plot the raw simulation output
plot(mySims)
