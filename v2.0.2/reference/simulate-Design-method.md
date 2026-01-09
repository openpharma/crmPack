# Simulate outcomes from a CRM design

**\[stable\]**

## Usage

``` r
# S4 method for class 'Design'
simulate(
  object,
  nsim = 1L,
  seed = NULL,
  truth,
  args = NULL,
  firstSeparate = FALSE,
  mcmcOptions = McmcOptions(),
  parallel = FALSE,
  nCores = min(parallel::detectCores(), 5),
  derive = list(),
  ...
)
```

## Arguments

- object:

  the
  [`Design`](https://openpharma.github.io/crmPack/reference/Design-class.md)
  object we want to simulate data from

- nsim:

  (`count`)  
  the number of simulations (default: 1)

- seed:

  see
  [`set_seed()`](https://openpharma.github.io/crmPack/reference/set_seed.md)

- truth:

  (`function`)  
  a function which takes as input a dose (vector) and returns the true
  probability (vector) for toxicity. Additional arguments can be
  supplied in `args`.

- args:

  (`data.frame`)  
  data frame with arguments for the `truth` function. The column names
  correspond to the argument names, the rows to the values of the
  arguments. The rows are appropriately recycled in the `nsim`
  simulations. In order to produce outcomes from the posterior
  predictive distribution, e.g, pass an `object` that contains the data
  observed so far, `truth` contains the `prob` function from the model
  in `object`, and `args` contains posterior samples from the model.

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

- derive:

  (`list`)  
  a named list of functions which derives statistics, based on the
  vector of posterior MTD samples. Each list element must therefore
  accept one and only one argument, which is a numeric vector, and
  return a number.

- ...:

  not used

## Value

an object of class
[`Simulations`](https://openpharma.github.io/crmPack/reference/Simulations-class.md)

## Examples

``` r
# nolint start

# Define the dose-grid
emptydata <- Data(doseGrid = c(1, 3, 5, 10, 15, 20, 25, 40, 50, 80, 100))

# Initialize the CRM model
model <- LogisticLogNormal(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
  ref_dose = 56
)

# Choose the rule for selecting the next dose
myNextBest <- NextBestNCRM(
  target = c(0.2, 0.35),
  overdose = c(0.35, 1),
  max_overdose_prob = 0.25
)

# Choose the rule for the cohort-size
mySize1 <- CohortSizeRange(
  intervals = c(0, 30),
  cohort_size = c(1, 3)
)
mySize2 <- CohortSizeDLT(
  intervals = c(0, 1),
  cohort_size = c(1, 3)
)
mySize <- maxSize(mySize1, mySize2)

# Choose the rule for stopping
myStopping1 <- StoppingMinCohorts(nCohorts = 3)
myStopping2 <- StoppingTargetProb(
  target = c(0.2, 0.35),
  prob = 0.5
)
myStopping3 <- StoppingMinPatients(nPatients = 20)
myStopping <- (myStopping1 & myStopping2) | myStopping3 | StoppingMissingDose()

# Choose the rule for dose increments
myIncrements <- IncrementsRelative(
  intervals = c(0, 20),
  increments = c(1, 0.33)
)

# Initialize the design
design <- Design(
  model = model,
  nextBest = myNextBest,
  stopping = myStopping,
  increments = myIncrements,
  cohort_size = mySize,
  data = emptydata,
  startingDose = 3
)

## define the true function
myTruth <- probFunction(model, alpha0 = 7, alpha1 = 8)

# Run the simulation on the desired design
# We only generate 1 trial outcomes here for illustration, for the actual study
# this should be increased of course
options <- McmcOptions(
  burnin = 5,
  step = 1,
  samples = 10
)

time <- system.time(
  mySims <- simulate(
    design,
    args = NULL,
    truth = myTruth,
    nsim = 1,
    seed = 819,
    mcmcOptions = options,
    parallel = FALSE
  )
)[3]

# nolint end
```
