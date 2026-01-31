# Simulation study example

## Simulation setting

Here the simulation study setting is defined.

``` r

id <- 1
onset <- 3
a0 <- 2
a1 <- 3
refDose <- 56

# True dose-DLT relationship
myTruth <- function(dose) {
  StandLogDose <- log(dose / refDose)
  plogis(a0 + a1 * StandLogDose)
}

# The conditional CDF of the PEM
if (onset == 30) {
  onset <- 15
  exp_cond_cdf <- function(x) {
    (pexp(42 - x, 1 / onset, lower.tail = FALSE) - pexp(t_max, 1 / onset, lower.tail = FALSE)) / pexp(t_max, 1 / onset)
  }
} else {
  exp_cond_cdf <- function(x) {
    1 - (pexp(x, 1 / onset, lower.tail = FALSE) - pexp(t_max, 1 / onset, lower.tail = FALSE)) / pexp(t_max, 1 / onset)
  }
}
```

## Design definition

Here the the dose escalation designs are defined: in this example the
TITE-CRM is used. Similarly the code can be adapted for the rolling-CRM.

``` r

library(crmPack)
```

    ## Loading required package: ggplot2

    ## Registered S3 method overwritten by 'crmPack':
    ##   method       from  
    ##   print.gtable gtable

    ## Type crmPackHelp() to open help browser
    ## Type crmPackExample() to open example

``` r

t_max <- 42

model <- TITELogisticLogNormal(
  mean = c(1.33, 1.49),
  cov = matrix(c(1.826, 0.0209, 0.0209, 0.0245), nrow = 2),
  ref_dose = refDose
)

myIncrements <- IncrementsRelative(
  intervals = c(0, 20),
  increments = c(10, 3)
)

myNextBest <- NextBestMTD(
  target = 0.3,
  derive =
    function(mtd_samples) {
      mean(mtd_samples)
    }
)

myStopping <- StoppingMinPatients(nPatients = 48)

mySize <- CohortSizeConst(size = 3)

emptydata <- DataDA(doseGrid = seq(from = 2, to = 50, by = 2), Tmax = t_max)

mysafetywindow <- SafetyWindowConst(c(7, 7), 7, 7)

design <- DADesign(
  model = model,
  increments = myIncrements,
  nextBest = myNextBest,
  stopping = myStopping,
  cohort_size = mySize,
  data = emptydata,
  safetyWindow = mysafetywindow,
  startingDose = 8
)
```

## Simulation run

In order to obtain stable results, increase the simulation parameters
appropriately (step, samples, nsim).

``` r

options <- McmcOptions(
  burnin = 20,
  step = 1,
  samples = 50
)
mySims <- simulate(design,
  args = NULL,
  truthTox = myTruth,
  truthSurv = exp_cond_cdf,
  trueTmax = 42,
  nsim = 10,
  seed = 819,
  mcmcOptions = options,
  parallel = FALSE
)
```
