# Simulate dose escalation procedure using both DLE and efficacy responses without samples

**\[stable\]**

This is a method to simulate dose escalation procedure using both DLE
and efficacy responses. This is a method based on the
[`DualResponsesDesign`](https://openpharma.github.io/crmPack/reference/DualResponsesDesign-class.md)
where DLE model used are of
[`ModelTox`](https://openpharma.github.io/crmPack/reference/ModelTox-class.md)
class object and efficacy model used are of
[`ModelEff`](https://openpharma.github.io/crmPack/reference/ModelEff-class.md)
class object. In addition, no DLE and efficacy samples are involved or
generated in the simulation process.

## Usage

``` r
# S4 method for class 'DualResponsesDesign'
simulate(
  object,
  nsim = 1L,
  seed = NULL,
  trueDLE,
  trueEff,
  trueNu,
  args = NULL,
  firstSeparate = FALSE,
  parallel = FALSE,
  nCores = min(parallel::detectCores(), 5L),
  ...
)
```

## Arguments

- object:

  the
  [`DualResponsesDesign`](https://openpharma.github.io/crmPack/reference/DualResponsesDesign-class.md)
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
  the precision, the inverse of the variance of the efficacy responses

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

## Examples

``` r
# nolint start

## Simulate dose-escalation procedure based on DLE and efficacy responses where no DLE
## and efficacy samples are used
## we need a data object with doses >= 1:
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

## The escalation rule using the 'NextBestMaxGain' class
mynextbest <- NextBestMaxGain(
  prob_target_drt = 0.35,
  prob_target_eot = 0.3
)


## The increments (see Increments class examples)
## 200% allowable increase for dose below 300 and 200% increase for dose above 300
myIncrements <- IncrementsRelative(
  intervals = c(25, 300),
  increments = c(2, 2)
)
## cohort size of 3
mySize <- CohortSizeConst(size = 3)
## Stop only when 36 subjects are treated
myStopping <- StoppingMinPatients(nPatients = 36)
## Now specified the design with all the above information and starting with a dose of 25

## Specified the design(for details please refer to the 'DualResponsesDesign' example)
design <- DualResponsesDesign(
  nextBest = mynextbest,
  model = DLEmodel,
  eff_model = Effmodel,
  stopping = myStopping,
  increments = myIncrements,
  cohort_size = mySize,
  data = data,
  startingDose = 25
)
## Specify the true DLE and efficacy curves
myTruthDLE <- probFunction(DLEmodel, phi1 = -53.66584, phi2 = 10.50499)
myTruthEff <- efficacyFunction(Effmodel, theta1 = -4.818429, theta2 = 3.653058)

## The true gain curve can also be seen
myTruthGain <- function(dose) {
  return((myTruthEff(dose)) / (1 + (myTruthDLE(dose) / (1 - myTruthDLE(dose)))))
}


## Then specified the simulations and generate the trial
## For illustration purpose only 1 simulation is produced (nsim=1).
options <- McmcOptions(burnin = 100, step = 2, samples = 200)
mySim <- simulate(
  object = design,
  args = NULL,
  trueDLE = myTruthDLE,
  trueEff = myTruthEff,
  trueNu = 1 / 0.025,
  nsim = 1,
  seed = 819,
  parallel = FALSE
)

# nolint end
```
