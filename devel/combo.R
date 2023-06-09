# nolint start

#####################################################################################
## Author: Daniel Sabanes Bove [sabanesd *a*t* roche *.* com]
## Project: Object-oriented implementation of CRM designs
##
## Time-stamp: <[combo.R] by DSB Sam 07/03/2015 21:58>
##
## Description:
## Test the combo stuff. For development only!!
##
## History:
## 25/01/2015   file creation
###################################################################################

source("../R/helpers.R")
source("../R/Data-class.R")


## create some test data
data <- DataCombo(
  x =
    cbind(
      a = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
      b = c(20, 20, 20, 40, 40, 40, 50, 50)
    ),
  y = c(0, 0, 0, 1, 0, 0, 1, 1),
  doseGrid =
    list(
      a =
        c(
          0.1, 0.5, 1.5, 3, 6,
          seq(from = 10, to = 80, by = 2)
        ),
      b = seq(from = 10, to = 80, by = 10)
    )
)

data
data@nGrid
data@nObs

## now test updating and plotting the data
source("../R/Data-methods.R")

## updating:
data2 <- update(data,
  x = c(a = 0.5, b = 30),
  y = c(0, 1, 0, 0)
)

## plotting:
library(ggplot2)
x11()
plot(data)

## load model code
source("../R/Model-class.R")

## define the model
model <- ComboLogistic(
  singlePriors =
    list(
      a =
        LogisticLogNormal(
          mean = c(0, 1),
          cov = diag(2),
          refDose = 10
        ),
      b =
        LogisticLogNormal(
          mean = c(1, 2),
          cov = diag(2),
          refDose = 20
        )
    ),
  gamma = 0,
  tau = 0.4
)


## try sampling from the model:

source("../R/McmcOptions-class.R")
source("../R/McmcOptions-methods.R")
## and some MCMC options
options <- McmcOptions(
  burnin = 10000,
  step = 2,
  samples = 50000
)


source("../R/mcmc.R")
source("../R/helpers.R")
source("../R/Samples-class.R")

## obtain the samples
library(rjags)

samples <- mcmc(data, model, options, verbose = TRUE)

str(samples)

source("../R/Samples-methods.R")

## use the ggmcmc package for convergence checks. we provide the extract function
## for this purpose:
library(ggmcmc)

alpha0samples <- get(samples, "alpha0")
ggs_traceplot(alpha0samples)

alpha1samples <- get(samples, "alpha1")
ggs_traceplot(alpha1samples)

ggs_traceplot(get(samples, "eta"))

## ok now we want to plot the fit:
source("../R/Model-methods.R")

## test C++ inline. requires MinGW installation and path settings,
## see http://stackoverflow.com/questions/23458841/how-to-get-rcpp-to-work
library(Rcpp)

cppFunction("
int fibonacci(const int x) {
if (x < 2)
return x;
else
return (fibonacci(x - 1)) + fibonacci(x - 2);
}
")
fibonacci(5)


## todo: cont here
system.time(print(plot(samples, model, data, focus = c("a", "b"))))

## old:
##   user  system elapsed
##  73.40    0.25   73.74

## new:

## user  system elapsed
## 5.27    0.20    5.51

## after first run even slightly faster:
## user  system elapsed
## 4.87    0.15    5.02

## ==> ~15 times faster with C++!
## nice!

x11()
plot(samples, model, data, extrapolate = FALSE)

betaModList <- list(betaMod = rbind(c(1, 1), c(1.5, 0.75), c(0.8, 2.5), c(0.4, 0.9)))
plotModels(betaModList, c(0, 1), base = 0, maxEff = 1, scal = 1.2)


## now on to the rules:
source("../R/helpers.R")
source("../R/Rules-class.R")
source("../R/Rules-methods.R")

## target level is 90% of maximum biomarker level
## overdose tox interval is 35%+
myNextBest <- new("NextBestDualEndpoint",
  target = 0.9,
  overdose = c(0.35, 1),
  maxOverdoseProb = 0.25
)

nextDose <- nextBest(myNextBest, doselimit = 50, samples = samples, model = model, data = data)
nextDose$plot
nextDose$value
data


## stopping rule:
## min 3 cohorts and at least 50% prob in for targeting biomarker,
## or max 20 patients
myStopping1 <- new("StoppingMinCohorts",
  nCohorts = 3L
)
myStopping2 <- new("StoppingMaxPatients",
  nPatients = 50L
)
myStopping3 <- new("StoppingTargetBiomarker",
  target = 0.9,
  prob = 0.5
)

## you can either write this:
myStopping <- new("StoppingAny",
  stop_list =
    list(
      new("StoppingAll",
        stop_list =
          list(
            myStopping1,
            myStopping3
          )
      ),
      myStopping2
    )
)

## or much more intuitively:
myStoppingEasy <- (myStopping1 & myStopping3) | myStopping2
myStoppingEasy
identical(myStopping, myStoppingEasy)


stopTrial(myStopping,
  dose = nextDose$value,
  samples, model, data
)

## relative increments:
myIncrements <- new("IncrementsRelative",
  intervals = c(0, 20, Inf),
  increments = c(1, 0.33)
)

## test design
source("../R/Design-class.R")
design <- new("DualDesign",
  model = model,
  nextBest = myNextBest,
  stopping = myStopping,
  increments = myIncrements,
  data =
    new("DataDual",
      x = numeric(),
      y = integer(),
      w = numeric(),
      doseGrid =
        c(
          0.1, 0.5, 1.5, 3, 6,
          seq(from = 10, to = 80, by = 2)
        )
    ),
  cohort_size = new("CohortSizeConst", size = 3L),
  startingDose = 6
)



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
  function(dose, e0, eMax, delta1, delta2, scal) {
    maxDens <- (delta1^delta1) * (delta2^delta2) / ((delta1 + delta2)^(delta1 +
      delta2))
    dose <- dose / scal
    e0 + eMax / maxDens * (dose^delta1) * (1 - dose)^delta2
  }
## trace(simulate, browser, signature=c("DualDesign"))
trueTox <- function(dose) {
  pnorm((dose - 60) / 10)
}
trueBiomarker <- function(dose) {
  betaMod(dose, e0 = 0.2, eMax = 0.6, delta1 = 5, delta2 = 5 * 0.5 / 0.5, scal = 100)
}
mySims <- simulate(design,
  trueTox = trueTox,
  trueBiomarker = trueBiomarker,
  sigma2W = 0.001,
  rho = 0,
  nsim = 3L,
  firstSeparate = FALSE,
  parallel = FALSE,
  seed = 3
)

source("../R/Simulations-methods.R")

str(mySims, 2)
str(mySims@fitBiomarker, 2)
identical(
  mySims@fitBiomarker[[1]],
  mySims@fitBiomarker[[2]]
)
identical(
  mySims@fitBiomarker[[2]],
  mySims@fitBiomarker[[3]]
)

plot(mySims@fitBiomarker[[1]]$middleBiomarker, type = "l")

mySims@stopReasons
plot(mySims, type = c("dose", "rho"))
plot(mySims)


## look at simulated trial outcomes:
plot(mySims@data[[2]])
mySims@stopReasons[[3]]

## final MTDs
mySims@doses



## extract operating characteristics


## the truth we want to compare it with:
sumOut <- summary(mySims,
  trueTox = trueTox,
  trueBiomarker = trueBiomarker
)
sumOut

mySims@doses


## make nice plots for simulation output:
## first from the summary object
str(sumOut)

plot(sumOut)

plot(sumOut, type = "meanFit")

plot(sumOut, type = c("nObs", "meanFit"))

## now from the raw simulation output
str(mySims@data)

plot(mySims)

# nolint end
