# Show the Summary of Model-Based Design Simulations

**\[stable\]**

Display a summary of model-based design simulation results.

## Usage

``` r
# S4 method for class 'SimulationsSummary'
show(object)
```

## Arguments

- object:

  (`SimulationsSummary`)  
  the object we want to print.

## Value

Invisibly returns a data frame of the results with one row and
appropriate column names.

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
# We only generate 1 trial outcome here for illustration, for the actual study
# this should be increased of course
options <- McmcOptions(
  burnin = 100,
  step = 2,
  samples = 1000
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

# Show the Summary of the Simulations
show(summary(mySims, truth = myTruth))
#> Summary of 1 simulations
#> 
#> Target toxicity interval was 20, 35 %
#> Target dose interval corresponding to this was 19.6, 21.6 
#> Intervals are corresponding to 10 and 90 % quantiles
#> 
#> Number of patients overall : mean 17 (17, 17) 
#> Number of patients treated above target tox interval : mean 7 (7, 7) 
#> Proportions of DLTs in the trials : mean 24 % (24 %, 24 %) 
#> Mean toxicity risks for the patients on active : mean 32 % (32 %, 32 %) 
#> Doses selected as MTD : mean 25 (25, 25) 
#> True toxicity at doses selected : mean 63 % (63 %, 63 %) 
#> Proportion of trials selecting target MTD: 0 %
#> Dose most often selected as MTD: 25 
#> Observed toxicity rate at dose most often selected: 57 %
#> Fitted toxicity rate at dose most often selected : mean 26 % (26 %, 26 %) 
#> Stop reason triggered:
#>  ≥ 3 cohorts dosed :  100 %
#>  P(0.2 ≤ prob(DLE | NBD) ≤ 0.35) ≥ 0.5 :  100 %
#>  ≥ 20 patients dosed :  0 %
#>  Stopped because of missing dose :  0 %

# nolint end
```
