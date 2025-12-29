# Simulate dose escalation procedure using DLE and efficacy responses with samples

**\[stable\]**

This is a method to simulate dose escalation procedure using both DLE
and efficacy responses. This is a method based on the
[`DualResponsesSamplesDesign`](https://openpharma.github.io/crmPack/reference/DualResponsesSamplesDesign-class.md)
where DLE model used are of
[`ModelTox`](https://openpharma.github.io/crmPack/reference/ModelTox-class.md)
class object and efficacy model used are of
[`ModelEff`](https://openpharma.github.io/crmPack/reference/ModelEff-class.md)
class object (special case is
[`EffFlexi`](https://openpharma.github.io/crmPack/reference/EffFlexi-class.md)
class model object). In addition, DLE and efficacy samples are involved
or generated in the simulation process.

## Usage

``` r
# S4 method for class 'DualResponsesSamplesDesign'
simulate(
  object,
  nsim = 1L,
  seed = NULL,
  trueDLE,
  trueEff,
  trueNu = NULL,
  trueSigma2 = NULL,
  trueSigma2betaW = NULL,
  args = NULL,
  firstSeparate = FALSE,
  mcmcOptions = McmcOptions(),
  parallel = FALSE,
  nCores = min(parallel::detectCores(), 5L),
  ...
)
```

## Arguments

- object:

  the
  [`DualResponsesSamplesDesign`](https://openpharma.github.io/crmPack/reference/DualResponsesSamplesDesign-class.md)
  object we want to simulate the data from

- nsim:

  (`count`)  
  the number of simulations (default: 1)

- seed:

  see
  [`set_seed()`](https://openpharma.github.io/crmPack/reference/set_seed.md)

- trueDLE:

  (`function`)  
  a function which takes as input a dose (vector) and returns the true
  probability (vector) of the occurrence of a DLE. Additional arguments
  can be supplied in `args`.

- trueEff:

  (`function`)  
  a function which takes as input a dose (vector) and returns the
  expected efficacy responses (vector). Additional arguments can be
  supplied in `args`.

- trueNu:

  (`number`)  
  (not with
  [`EffFlexi`](https://openpharma.github.io/crmPack/reference/EffFlexi-class.md))
  the precision, the inverse of the variance of the efficacy responses

- trueSigma2:

  (`number`)  
  (only with
  [`EffFlexi`](https://openpharma.github.io/crmPack/reference/EffFlexi-class.md))
  the true variance of the efficacy responses which must be a single
  positive scalar.

- trueSigma2betaW:

  (`number`)  
  (only with
  [`EffFlexi`](https://openpharma.github.io/crmPack/reference/EffFlexi-class.md))
  the true variance for the random walk model used for smoothing. This
  must be a single positive scalar.

- args:

  (`data.frame`)  
  data frame with arguments for the `trueDLE` and `trueEff` function.
  The column names correspond to the argument names, the rows to the
  values of the arguments. The rows are appropriately recycled in the
  `nsim` simulations.

- firstSeparate:

  (`flag`)  
  enroll the first patient separately from the rest of the cohort? (not
  default) If yes, the cohort will be closed if a DLT occurs in this
  patient.

- mcmcOptions:

  ([McmcOptions](https://openpharma.github.io/crmPack/reference/McmcOptions-class.md))  
  object of class
  [`McmcOptions`](https://openpharma.github.io/crmPack/reference/McmcOptions-class.md),
  giving the MCMC options for each evaluation in the trial. By default,
  the standard options are used

- parallel:

  (`flag`)  
  should the simulation runs be parallelized across the clusters of the
  computer? (not default)

- nCores:

  (`count`)  
  how many cores should be used for parallel computing? Defaults to the
  number of cores on the machine, maximum 5.

- ...:

  not used

## Value

an object of class
[`PseudoDualSimulations`](https://openpharma.github.io/crmPack/reference/PseudoDualSimulations-class.md)
or
[`PseudoDualFlexiSimulations`](https://openpharma.github.io/crmPack/reference/PseudoDualFlexiSimulations-class.md)

## Examples

``` r
# nolint start

## Simulate dose-escalation procedure based on DLE and efficacy responses where DLE
## and efficacy samples are used
data <- DataDual(doseGrid = seq(25, 300, 25), placebo = FALSE)
## First for the DLE model
## The DLE model must be of 'ModelTox' (e.g 'LogisticIndepBeta') class
DLEmodel <- LogisticIndepBeta(
  binDLE = c(1.05, 1.8),
  DLEweights = c(3, 3),
  DLEdose = c(25, 300),
  data = data
)

## The efficacy model of 'ModelEff' (e.g 'Effloglog') class
Effmodel <- Effloglog(
  eff = c(1.223, 2.513),
  eff_dose = c(25, 300),
  nu = c(a = 1, b = 0.025),
  data = data
)


## The escalation rule using the 'NextBestMaxGainSamples' class
mynextbest <- NextBestMaxGainSamples(
  prob_target_drt = 0.35,
  prob_target_eot = 0.3,
  derive = function(samples) {
    as.numeric(quantile(samples, prob = 0.3))
  },
  mg_derive = function(mg_samples) {
    as.numeric(quantile(mg_samples, prob = 0.5))
  }
)

## The increments (see Increments class examples)
## 200% allowable increase for dose below 300 and 200% increase for dose above 300
myIncrements <- IncrementsRelative(
  intervals = c(25, 300),
  increments = c(2, 2)
)
## cohort size of 3
mySize <- CohortSizeConst(size = 3)
## Stop only when 10 subjects are treated (only for illustration such a low
## sample size)
myStopping <- StoppingMinPatients(nPatients = 10)
## Now specified the design with all the above information and starting with
## a dose of 25

## Specified the design
design <- DualResponsesSamplesDesign(
  nextBest = mynextbest,
  cohort_size = mySize,
  startingDose = 25,
  model = DLEmodel,
  eff_model = Effmodel,
  data = data,
  stopping = myStopping,
  increments = myIncrements
)
## specified the true DLE and efficacy curve
myTruthDLE <- probFunction(DLEmodel, phi1 = -53.66584, phi2 = 10.50499)
myTruthEff <- efficacyFunction(Effmodel, theta1 = -4.818429, theta2 = 3.653058)

## The true gain curve can also be seen
myTruthGain <- function(dose) {
  return((myTruthEff(dose)) / (1 + (myTruthDLE(dose) / (1 - myTruthDLE(dose)))))
}

## simulate the trial for 10 times involving samples
## for illustration purpose we use 10 burn-ins to generate 50 samples
options <- McmcOptions(burnin = 10, step = 1, samples = 50)
## For illustration purpose only 1 simulations are produced (nsim=1).
mySim <- simulate(
  design,
  args = NULL,
  trueDLE = myTruthDLE,
  trueEff = myTruthEff,
  trueNu = 1 / 0.025,
  nsim = 1,
  mcmcOptions = options,
  seed = 819,
  parallel = FALSE
)


## Simulate dose-escalation procedure based on DLE and efficacy responses where DLE
## and efficacy samples are used
## when the efficacy model is of 'EffFlexi' class
Effmodel <- EffFlexi(
  eff = c(1.223, 2.513),
  eff_dose = c(25, 300),
  sigma2W = c(a = 0.1, b = 0.1),
  sigma2betaW = c(a = 20, b = 50),
  rw1 = FALSE,
  data = data
)


## Specified the design
design <- DualResponsesSamplesDesign(
  nextBest = mynextbest,
  cohort_size = mySize,
  startingDose = 25,
  model = DLEmodel,
  eff_model = Effmodel,
  data = data,
  stopping = myStopping,
  increments = myIncrements
)
## specified the true DLE curve and the true expected efficacy values at all dose levels
myTruthDLE <- probFunction(DLEmodel, phi1 = -53.66584, phi2 = 10.50499)

myTruthEff <- c(
  -0.5478867,
  0.1645417,
  0.5248031,
  0.7604467,
  0.9333009,
  1.0687031,
  1.1793942,
  1.2726408,
  1.3529598,
  1.4233411,
  1.4858613,
  1.5420182
)
## The true gain curve can also be seen
d1 <- data@doseGrid
myTruthGain <- (myTruthEff) / (1 + (myTruthDLE(d1) / (1 - myTruthDLE(d1))))


mySim <- simulate(
  object = design,
  args = NULL,
  trueDLE = myTruthDLE,
  trueEff = myTruthEff,
  trueSigma2 = 0.025,
  trueSigma2betaW = 1,
  mcmcOptions = options,
  nsim = 1,
  seed = 819,
  parallel = FALSE
)

# nolint end
```
