# Obtain Hypothetical Trial Course Table for a Design

This generic function takes a design and generates a `data.frame`
showing the beginning of several hypothetical trial courses under the
design. This means, from the generated `data.frame` one can read off:

## Usage

``` r
examine(object, ..., maxNoIncrement = 100L)

# S4 method for class 'Design'
examine(object, mcmcOptions = McmcOptions(), ..., maxNoIncrement)

# S4 method for class 'RuleDesign'
examine(object, ..., maxNoIncrement = 100L)

# S4 method for class 'DADesign'
examine(object, mcmcOptions = McmcOptions(), ..., maxNoIncrement)
```

## Arguments

- object:

  ([`Design`](https://openpharma.github.io/crmPack/reference/Design-class.md)
  or
  [`RuleDesign`](https://openpharma.github.io/crmPack/reference/RuleDesign-class.md))  
  the design we want to examine

- ...:

  additional arguments (see methods)

- maxNoIncrement:

  maximum number of contiguous next doses at 0 DLTs that are the same as
  before, i.e. no increment (default to 100)

- mcmcOptions:

  ([`McmcOptions`](https://openpharma.github.io/crmPack/reference/McmcOptions-class.md))  
  giving the MCMC options for each evaluation in the trial. By default,
  the standard options are used

## Value

The data frame

## Details

- how many cohorts are required in the optimal case (no DLTs observed)
  in order to reach the highest dose of the specified dose grid (or
  until the stopping rule is fulfilled)

- assuming no DLTs are observed until a certain dose level, what the
  next recommended dose is for all possible number of DLTs observed

- the actual relative increments that will be used in these cases

- whether the trial would stop at a certain cohort

Examining the "single trial" behavior of a dose escalation design is the
first important step in evaluating a design, and cannot be replaced by
studying solely the operating characteristics in "many trials". The
cohort sizes are also taken from the design, assuming no DLTs occur
until the dose listed.

## Functions

- `examine(Design)`: Examine a model-based CRM.

- `examine(RuleDesign)`: Examine a rule-based design.

- `examine(DADesign)`: Examine a model-based CRM.

## Examples

``` r
# Define the dose-grid.
emptydata <- Data(doseGrid = c(1, 3, 5, 10, 15, 20, 25))


# Initialize the CRM model.
my_model <- LogisticLogNormal(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
  ref_dose = 56
)

# Choose the rule for selecting the next dose.
my_next_best <- NextBestNCRM(
  target = c(0.2, 0.35),
  overdose = c(0.35, 1),
  max_overdose_prob = 0.25
)


my_size1 <- CohortSizeRange(
  intervals = c(0, 30),
  cohort_size = c(1, 3)
)
my_size2 <- CohortSizeDLT(
  intervals = c(0, 1),
  cohort_size = c(1, 3)
)
my_size <- maxSize(my_size1, my_size2)

# Choose the rule for stopping.
my_stopping1 <- StoppingMinCohorts(nCohorts = 3)
my_stopping2 <- StoppingTargetProb(
  target = c(0.2, 0.35),
  prob = 0.5
)
my_stopping3 <- StoppingMinPatients(nPatients = 20)
my_stopping <- (my_stopping1 & my_stopping2) | my_stopping3 | StoppingMissingDose()

# Choose the rule for dose increments.
my_increments <- IncrementsRelative(
  intervals = c(0, 20),
  increments = c(1, 0.33)
)

# Initialize the design.
my_design <- Design(
  model = my_model,
  nextBest = my_next_best,
  stopping = my_stopping,
  increments = my_increments,
  cohort_size = my_size,
  data = emptydata,
  startingDose = 3
)

my_options <- McmcOptions(
  burnin = 10,
  step = 1,
  samples = 20,
  rng_kind = "Super-Duper",
  rng_seed = 94
)

# \donttest{
examine(my_design, my_options)
#>   dose DLTs nextDose  stop increment
#> 1    3    0        5 FALSE        67
#> 2    3    1        1 FALSE       -67
#> 3    5    0       10 FALSE       100
#> 4    5    1        5 FALSE         0
#> 5   10    0       20 FALSE       100
#> 6   10    1        5 FALSE       -50
#> 7   20    0       25 FALSE        25
#> 8   20    1       20 FALSE         0
# }

# Example where examine stops because stopping rule already fulfilled.
my_stopping4 <- StoppingMinPatients(nPatients = 3)
my_stopping <- (my_stopping1 & my_stopping2) | my_stopping4

my_design <- Design(
  model = my_model,
  nextBest = my_next_best,
  stopping = my_stopping,
  increments = my_increments,
  cohort_size = my_size,
  data = emptydata,
  startingDose = 3
)

# \donttest{
examine(my_design, mcmcOptions = my_options)
#>   dose DLTs nextDose  stop increment
#> 1    3    0        5 FALSE        67
#> 2    3    1        1 FALSE       -67
#> 3    5    0       10 FALSE       100
#> 4    5    1        5 FALSE         0
#> 5   10    0       20  TRUE       100
#> 6   10    1        5  TRUE       -50
# }

# Example where examine stops because infinite looping
# (note that here a very low threshold is used for the parameter
# "maxNoIncrement" in "examine" to keep the execution time short).
my_increments <- IncrementsRelative(
  intervals = c(0, 20),
  increments = c(1, 0.00001)
)

my_stopping <- (my_stopping1 & my_stopping2) | StoppingMissingDose()

design <- Design(
  model = my_model,
  nextBest = my_next_best,
  stopping = my_stopping,
  increments = my_increments,
  cohort_size = my_size,
  data = emptydata,
  startingDose = 3
)

# \donttest{
examine(my_design, mcmcOptions = my_options, maxNoIncrement = 2)
#>   dose DLTs nextDose  stop increment
#> 1    3    0        5 FALSE        67
#> 2    3    1        1 FALSE       -67
#> 3    5    0       10 FALSE       100
#> 4    5    1        5 FALSE         0
#> 5   10    0       20  TRUE       100
#> 6   10    1        5  TRUE       -50
# }
# Define the dose-grid
emptydata <- Data(doseGrid = c(5, 10, 15, 25, 35, 50, 80))

# inizialing a 3+3 design with constant cohort size of 3 and
# starting dose equal 5
myDesign <- RuleDesign(
  nextBest = NextBestThreePlusThree(),
  cohort_size = CohortSizeConst(size = 3L),
  data = emptydata,
  startingDose = 5
)

# Examine the design
set.seed(4235)
# \donttest{
examine(myDesign)
#>    dose DLTs nextDose  stop increment
#> 1     5    0       10 FALSE       100
#> 2     5    1        5 FALSE         0
#> 3     5    2       NA  TRUE        NA
#> 4     5    3       NA  TRUE        NA
#> 5    10    0       15 FALSE        50
#> 6    10    1       10 FALSE         0
#> 7    10    2        5 FALSE       -50
#> 8    10    3        5 FALSE       -50
#> 9    15    0       25 FALSE        67
#> 10   15    1       15 FALSE         0
#> 11   15    2       10 FALSE       -33
#> 12   15    3       10 FALSE       -33
#> 13   25    0       35 FALSE        40
#> 14   25    1       25 FALSE         0
#> 15   25    2       15 FALSE       -40
#> 16   25    3       15 FALSE       -40
#> 17   35    0       50 FALSE        43
#> 18   35    1       35 FALSE         0
#> 19   35    2       25 FALSE       -29
#> 20   35    3       25 FALSE       -29
#> 21   50    0       80 FALSE        60
#> 22   50    1       50 FALSE         0
#> 23   50    2       35 FALSE       -30
#> 24   50    3       35 FALSE       -30
# }
# nolint start

# Define the dose-grid and PEM parameters
emptydata <- DataDA(doseGrid = c(
  0.1, 0.5, 1, 1.5, 3, 6,
  seq(from = 10, to = 80, by = 2)
), Tmax = 60)
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

myStopping <- (myStopping1 | myStopping2) | StoppingMissingDose()

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

set.seed(4235)
# MCMC parameters are set to small values only to show this example. They should be
# increased for a real case.
# This procedure will take a while.
options <- McmcOptions(
  burnin = 10,
  step = 1,
  samples = 100,
  rng_kind = "Mersenne-Twister",
  rng_seed = 12
)
# \donttest{
testthat::expect_warning(
  result <- examine(design, mcmcOptions = options, maxNoIncrement = 2),
  "Stopping because 2 times no increment"
)
# }

# nolint end
```
