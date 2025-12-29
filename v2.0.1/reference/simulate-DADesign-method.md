# Simulate outcomes from a time-to-DLT augmented CRM design

**\[stable\]**

This method simulates dose escalation trials using time-to-DLT data,
where the timing of dose-limiting toxicities is explicitly modeled.

## Usage

``` r
# S4 method for class 'DADesign'
simulate(
  object,
  nsim = 1L,
  seed = NULL,
  truthTox,
  truthSurv,
  trueTmax = NULL,
  args = NULL,
  firstSeparate = FALSE,
  deescalate = TRUE,
  mcmcOptions = McmcOptions(),
  DA = TRUE,
  parallel = FALSE,
  nCores = min(parallel::detectCores(), 5),
  derive = list(),
  ...
)
```

## Arguments

- object:

  the
  [`DADesign`](https://openpharma.github.io/crmPack/reference/DADesign-class.md)
  object we want to simulate data from

- nsim:

  (`count`)  
  the number of simulations (default: 1)

- seed:

  see
  [`set_seed()`](https://openpharma.github.io/crmPack/reference/set_seed.md)

- truthTox:

  (`function`)  
  a function which takes as input a dose (vector) and returns the true
  probability (vector) for toxicity and the time DLT occurs. Additional
  arguments can be supplied in `args`.

- truthSurv:

  (`function`)  
  a CDF which takes as input a time (vector) and returns the true
  cumulative probability (vector) that the DLT would occur conditioning
  on the patient has DLTs.

- trueTmax:

  (`number` or `NULL`)  
  the true maximum time at which DLTs can occur. Note that this must be
  larger than `Tmax` from the `object`'s base data, which is the length
  of the DLT window, i.e. until which time DLTs are officially declared
  as such and used in the trial.

- args:

  (`data.frame`)  
  data frame with arguments for the `truthTox` function. The column
  names correspond to the argument names, the rows to the values of the
  arguments. The rows are appropriately recycled in the `nsim`
  simulations. In order to produce outcomes from the posterior
  predictive distribution, e.g, pass an `object` that contains the data
  observed so far, `truthTox` contains the `prob` function from the
  model in `object`, and `args` contains posterior samples from the
  model.

- firstSeparate:

  (`flag`)  
  enroll the first patient separately from the rest of the cohort? (not
  default) If yes, the cohort will be closed if a DLT occurs in this
  patient.

- deescalate:

  (`flag`)  
  allow deescalation when a DLT occurs in cohorts with lower dose level?
  (default: TRUE)

- mcmcOptions:

  ([McmcOptions](https://openpharma.github.io/crmPack/reference/McmcOptions-class.md))  
  object of class
  [`McmcOptions`](https://openpharma.github.io/crmPack/reference/McmcOptions-class.md),
  giving the MCMC options for each evaluation in the trial. By default,
  the standard options are used.

- DA:

  (`flag`)  
  use dose-adaptation rules? (default: TRUE)

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

# Define the dose-grid and PEM parameters
emptydata <- DataDA(
  doseGrid = c(0.1, 0.5, 1, 1.5, 3, 6, seq(from = 10, to = 80, by = 2)),
  Tmax = 60
)

# Initialize the mDA-CRM model
npiece_ <- 10
Tmax_ <- 60

lambda_prior <- function(k) {
  npiece_ / (Tmax_ * (npiece_ - k + 0.5))
}

model <- DALogisticLogNormal(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
  ref_dose = 56,
  npiece = npiece_,
  l = as.numeric(t(apply(as.matrix(c(1:npiece_), 1, npiece_), 2, lambda_prior))),
  c_par = 2
)

# Choose the rule for dose increments
myIncrements <- IncrementsRelative(
  intervals = c(0, 20),
  increments = c(1, 0.33)
)

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
myStopping1 <- StoppingTargetProb(
  target = c(0.2, 0.35),
  prob = 0.5
)
myStopping2 <- StoppingMinPatients(nPatients = 50)

myStopping <- (myStopping1 | myStopping2)

# Choose the safety window
mysafetywindow <- SafetyWindowConst(c(6, 2), 7, 7)

# Initialize the design
design <- DADesign(
  model = model,
  increments = myIncrements,
  nextBest = myNextBest,
  stopping = myStopping,
  cohort_size = mySize,
  data = emptydata,
  safetyWindow = mysafetywindow,
  startingDose = 3
)

## set up truth curves
myTruth <- probFunction(model, alpha0 = 2, alpha1 = 3)
curve(myTruth(x), from = 0, to = 100, ylim = c(0, 1))


exp_cond.cdf <- function(x, onset = 15) {
  a <- pexp(28, 1 / onset, lower.tail = FALSE)
  1 - (pexp(x, 1 / onset, lower.tail = FALSE) - a) / (1 - a)
}

# set up simulation settings
options <- McmcOptions(
  burnin = 10,
  step = 1,
  samples = 200
)

# \donttest{
mySims <- simulate(design,
  args = NULL,
  truthTox = myTruth,
  truthSurv = exp_cond.cdf,
  trueTmax = 80,
  nsim = 2,
  seed = 819,
  mcmcOptions = options,
  firstSeparate = TRUE,
  deescalate = FALSE,
  parallel = FALSE
)
# }

# nolint end
```
