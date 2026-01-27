# nolint start

#####################################################################################
## Author: Daniel Sabanés Bové [sabanesd *a*t* roche *.* com]
## Project: Object-oriented implementation of CRM designs
##
## Time-stamp: <[project.R] by DSB Son 10/05/2015 22:05>
##
## Description:
## Test in the setup of the project. For development only!!
##
## History:
## 06/02/2014   file creation
## 19/07/2014   update and test mixture prior
## 25/03/2015   update with new syntax and check JAGS/burnin issue fix.
###################################################################################
source("../R/helpers.R")
source("../R/Model-class.R")

model <- LogisticLogNormal(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2L),
  refDose = 56
)

model@prob(30, 0, 1)

## model <- new("LogisticKadane",
##              theta=0.33,
##              xmin=0.1,
##              xmax=100)

source("../R/Data-class.R")
source("../R/Data-methods.R")
## create some test data
data <- Data(
  x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
  y = as.integer(c(0, 0, 0, 0, 0, 0, 1, 0)),
  doseGrid = c(
    0.1,
    0.5,
    1.5,
    3,
    6,
    seq(from = 10, to = 80, by = 2)
  )
)

data

library(ggplot2)
plot(data)


source("../R/McmcOptions-class.R")
source("../R/McmcOptions-methods.R")
source("../R/Samples-class.R")
source("../R/Samples-methods.R")

## and some MCMC options
options <- McmcOptions(
  burnin = 100000,
  step = 4,
  samples = 40829
)


source("../R/mcmc.R")
source("../R/helpers.R")
library(rjags)

## obtain the samples
set.seed(12)
time2 <- system.time(samples2 <- mcmc(data, model, options, verbose = TRUE))
set.seed(12)
time3 <- system.time(samples3 <- mcmc(data, model, options, verbose = TRUE))
all.equal(samples2, samples3)

str(samples2)
str(samples3)
samples1 <- samples2

## with the standard method:
time1 <- system.time(
  samples1 <-
    getMethod(
      "mcmc",
      signature(
        data = "Data",
        model = "Model",
        options = "McmcOptions"
      )
    )(data, model, options, verbose = TRUE)
)

## extract samples for diagnostic plots
alpha0 <- get(samples1, "alpha0")
alpha1 <- get(samples2, "alpha1")

## use other package for plotting
library(ggmcmc)
ggs_traceplot(alpha0)
ggs_autocorrelation(alpha0)
ggs_density(alpha0)

ggs_traceplot(alpha1)
ggs_autocorrelation(alpha1)
ggs_density(alpha1)
## etc.

## ok now we want to plot the fit:
samples <- samples2
str(samples)

source("../R/Model-methods.R")
source("../R/Samples-methods.R")
p1 <- plot(samples, model, data)
p1

nodata <- Data(doseGrid = data@doseGrid)
nosamples <- mcmc(nodata, model, options)
p2 <- plot(nosamples, model, nodata)
p2

f2 <- fit(nosamples, model, nodata)
gdata <-
  with(
    f2,
    data.frame(
      x = rep(dose, 3),
      y = c(middle, lower, upper) * 100,
      group = rep(c("mean", "lower", "upper"), each = nrow(f2)),
      Type = factor(
        c(
          rep(
            "Estimate",
            nrow(f2)
          ),
          rep(
            "95% Credible Interval",
            nrow(f2) * 2
          )
        ),
        levels = c(
          "Estimate",
          "95% Credible Interval"
        )
      )
    )
  )

p1 <- p1 +
  geom_line(
    data = gdata,
    aes(x = x, y = y, group = group, linetype = Type),
    colour = "blue"
  )

p1

## test quantiles function
source("../R/fromQuantiles.R")
set.seed(92)
quantTest <-
  Quantiles2LogisticNormal(
    dosegrid = 1:5,
    refDose = 2.5,
    lower = c(0.01, 0.02, 0.05, 0.1, 0.3),
    upper = c(0.5, 0.6, 0.7, 0.8, 0.95),
    median = c(0.2, 0.3, 0.4, 0.45, 0.5),
    control = list(
      threshold.stop = 0.01,
      maxit = 50000,
      temperature = 50000,
      max.time = 10,
      verbose = TRUE
    )
  )
str(quantTest)

matplot(quantTest$required, type = "l", col = "blue", lty = 1)
matlines(quantTest$quantiles, col = "red", lty = 1)

## test minimally informative
minTest <- MinimalInformative(
  dosegrid = 1:5,
  refDose = 2.5,
  control = list(
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
  control = list(
    threshold.stop = 0.01,
    maxit = 50000,
    temperature = 50000,
    max.time = 20,
    verbose = TRUE
  )
)
str(minTest)

matplot(minTest$required, type = "l", col = "blue", lty = 1)
matlines(minTest$quantiles, col = "red", lty = 1)


## OK, now we can setup the mixture prior:
## Note that the informative part does not match the LogisticLogNormal model
## above, because here we use LogisticNormal...

mixModel <- new(
  "LogisticNormalMixture",
  comp1 = list(
    mean = c(-0.85, exp(1)),
    cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2L)
  ),
  comp2 = list(
    mean = minTest$model@mean,
    cov = minTest$model@cov
  ),
  weightpar = c(a = 1, b = 1),
  refDose = 56
)
mixModel@modelspecs()

## obtain the samples
time <- system.time(mixSamples <- mcmc(data, mixModel, options, verbose = TRUE))

## extract samples for diagnostic plots
alpha0 <- extract(mixSamples, "alpha0")
alpha1 <- extract(mixSamples, "alpha1")
w <- extract(mixSamples, "w")
mean(w$value)

## use other package for plotting
library(ggmcmc)
ggs_traceplot(alpha0)
ggs_autocorrelation(alpha0)
ggs_density(alpha0)

ggs_traceplot(alpha1)
ggs_autocorrelation(alpha1)
ggs_density(alpha1)

ggs_traceplot(w)
ggs_autocorrelation(w)
ggs_density(w)
## etc.

plot(mixSamples, mixModel, data)

## compare with the minimal informative result:
time <- system.time(
  minSamples <- mcmc(data, minTest$model, options, verbose = TRUE)
)
x11()
plot(minSamples, minTest$model, data)


## and with the informative result:
infModel <- new(
  "LogisticNormal",
  mean = c(-0.85, exp(1)),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2L),
  refDose = 56
)
infSamples <- mcmc(data, infModel, options)
x11()
plot(infSamples, infModel, data)

## now on to the rules:

source("../R/helpers.R")
source("../R/Rules-class.R")
source("../R/Rules-methods.R")

## target tox rate is 33%.
## 25% quantile of posterior distribution is used
## and 90% one-sided CI must be above 50% of current MTD estimate,
## that is the probability that the MTD is above 50% of the current estimate
## must be larger than 90%
myNextBest <- new("NextBestMTD", target = 0.5, derive = function(mtdSamples) {
  quantile(mtdSamples, probs = 0.25)
})

mtdRet <- nextBest(
  myNextBest,
  doselimit = 50,
  samples = samples,
  model = model,
  data = data,
  plot = FALSE
)
mtdRet$value

## target tox interval is 20-35%.
## overdose tox interval is 35%+
## required prob for target is 50%
myNextBest <- NextBestNCRM(
  target = c(0.2, 0.35),
  overdose = c(0.35, 1),
  maxOverdoseProb = 0.25
)

ncrmRet <- nextBest(
  myNextBest,
  doselimit = 50,
  samples = samples,
  model = model,
  data = data,
  plot = FALSE
)
ncrmRet$value
ncrmRet$plot


## stopping rule:
## min 3 cohorts and at least 50% prob in target interval,
## or max 20 patients
myStopping1 <- StoppingMinCohorts(nCohorts = 3L)
myStopping2 <- StoppingMinPatients(nPatients = 20L)
myStopping3 <- StoppingTargetProb(
  target = c(0.2, 0.35),
  prob = 0.5
)

## you can either write this:
## myStopping <- new("StoppingAny",
##                   stop_list=
##                       list(new("StoppingAll",
##                                stop_list=
##                                    list(myStopping1,
##                                         myStopping3)),
##                            myStopping2))

## or much more intuitively:
myStoppingEasy <- (myStopping1 & myStopping3) | myStopping2
myStoppingEasy
## identical(myStopping, myStoppingEasy)

## what would be the decision in this case?
stopTrial(
  stopping = myStoppingEasy,
  dose = ncrmRet$value,
  samples = samples,
  model = model,
  data = data
)


## relative increments:
myIncrements <- IncrementsRelative(
  intervals = c(0, 20),
  increments = c(1, 0.33)
)


## cohort size:
## DLT rule says to have size 1 if no DLT has happened,
## and size 3 if at least 1 DLT has happened
mySize <- CohortSizeDLT(
  intervals = c(0, 1),
  cohort_size = c(1, 3)
)

## Range rule says to have size 1 until 30 mg, then size 3.
mySize2 <- CohortSizeRange(
  intervals = c(0, 30),
  cohort_size = c(1, 3)
)

## these two rules are now combined by taking the maximum
mySizeCombined <- maxSize(mySize, mySize2)

## create empty data to start with
emptydata <- Data(doseGrid = seq(from = 0.1, to = 50, by = 0.1))

## test design
source("../R/Design-class.R")
design <- Design(
  model = model,
  nextBest = myNextBest,
  stopping = myStoppingEasy,
  increments = myIncrements,
  cohort_size = mySizeCombined,
  data = emptydata,
  startingDose = 0.1
)

## for testing the simulate function:
object <- design
truth <- model@prob
## args <- list(rho0=0.1,
##              gamma=20)
args <- list(
  alpha0 = 0,
  alpha1 = 1
)
nsim <- 4L
mcmcOptions <- options
seed <- 23
parallel <- TRUE

## iterSim <- 1L

source("../R/Simulations-class.R")
source("../R/Design-methods.R")

set.seed(29)
mySims <- simulate(
  design,
  truth = truth,
  args = args,
  nsim = 2L,
  mcmcOptions = options,
  firstSeparate = FALSE
)

set.seed(29)
mySims2 <- simulate(
  design,
  truth = truth,
  args = args,
  nsim = 2L,
  mcmcOptions = options,
  firstSeparate = FALSE
)

all.equal(mySims, mySims2)

str(mySims)

## look at simulated trial outcomes:
plot(mySims@data[[1]])
plot(mySims@data[[2]])
mySims@stopReasons[[2]]

## final MTDs
mySims@doses

## what is the true prob at these doses?
truth(mySims@doses, alpha0 = 0, alpha1 = 1)

## todo: test hypothetical cohorts

## extract operating characteristics
source("../R/Simulations-methods.R")

## the truth we want to compare it with:
myTruth <- function(dose) {
  model@prob(dose, alpha0 = 0, alpha1 = 1)
}
curve(myTruth(x), from = 0, to = 30)

sumOut <- summary(as(mySims, Class = "Simulations"), truth = myTruth)
sumOut

mySims@doses

sumOut@obsToxRateAtDoseMostSelected
str(sumOut)

## make nice plots for simulation output:
## first from the summary object
str(sumOut)

library(gridExtra)
plot(sumOut)

plot(sumOut, type = "meanFit")

plot(sumOut, type = c("nObs", "meanFit"))

## now from the raw simulation output
str(mySims@data)

plot(mySims)

## todo: include possibility to run simulations on cluster,
## already done:
## - installation of new R etc on cluster
## - using the parallel's cluster...() functions for cores speedup.
##   It should be easy to translate to the cluster from there.

## for testing:
x <- mySims
type <-
  c(
    "trajectory",
    "dosesTried"
  )

## todo: plot function for model alone, to just plot the prior
## without having any data

# nolint end
