# Determine the safety window length of the next cohort

This function determines the safety window length of the next cohort.

## Usage

``` r
windowLength(safetyWindow, size, ...)

# S4 method for class 'SafetyWindowSize'
windowLength(safetyWindow, size, data, ...)

# S4 method for class 'SafetyWindowConst'
windowLength(safetyWindow, size, ...)
```

## Arguments

- safetyWindow:

  The rule, an object of class
  [`SafetyWindow`](https://openpharma.github.io/crmPack/reference/SafetyWindow-class.md)

- size:

  The next cohort size

- ...:

  additional arguments

- data:

  The data input, an object of class
  [`DataDA`](https://openpharma.github.io/crmPack/reference/DataDA-class.md)

## Value

the `windowLength` as a list of safety window parameters (`gap`,
`follow`, `follow_min`)

## Functions

- `windowLength(SafetyWindowSize)`: Determine safety window length based
  on the cohort size

- `windowLength(SafetyWindowConst)`: Constant safety window length

## Examples

``` r
# nolint start

# Create the data
data <- DataDA(
  x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
  y = c(0, 0, 1, 1, 0, 0, 1, 0),
  doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2)),
  u = c(42, 30, 15, 5, 20, 25, 30, 60),
  t0 = c(0, 15, 30, 40, 55, 70, 75, 85),
  Tmax = 60
)
#> Used default patient IDs!
#> Used best guess cohort indices!

# Initialize the CRM model used to model the data
npiece_ <- 10
lambda_prior <- function(k) {
  npiece_ / (data@Tmax * (npiece_ - k + 0.5))
}

model <- DALogisticLogNormal(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
  ref_dose = 56,
  npiece = npiece_,
  l = as.numeric(t(apply(
    as.matrix(c(1:npiece_), 1, npiece_),
    2,
    lambda_prior
  ))),
  c_par = 2
)

# Set-up some MCMC parameters and generate samples from the posterior
options <- McmcOptions(burnin = 100, step = 2, samples = 200)
set.seed(94)
samples <- mcmc(data, model, options)

# Define the rule for dose increments and calculate the maximum dose allowed
myIncrements <- IncrementsRelative(
  intervals = c(0, 20),
  increments = c(1, 0.33)
)
nextMaxDose <- maxDose(myIncrements, data = data)

# Define the rule which will be used to select the next best dose
# based on the class 'NextBestNCRM'
myNextBest <- NextBestNCRM(
  target = c(0.2, 0.35),
  overdose = c(0.35, 1),
  max_overdose_prob = 0.25
)

# Calculate the next best dose
doseRecommendation <- nextBest(
  myNextBest,
  doselimit = nextMaxDose,
  samples = samples,
  model = model,
  data = data
)

# Define the rule which will be used to select the next cohort size
# based on the class 'CohortSizeConst'
mySize <- CohortSizeConst(size = 3)

# Determine the cohort size for the next cohort
sizeRecommendation <- size(mySize, dose = doseRecommendation$value, data = data)

# Rule for the safety window length:
#   -having patientGap as (0,7,3,3,...) for cohort size <4
#   -and having patientGap as (0,9,5,5,...) for cohort size >=4
myWindowLength <- SafetyWindowSize(
  gap = list(c(7, 3), c(9, 5)),
  size = c(1, 4),
  follow = 7,
  follow_min = 14
)

# Determine the safety window parameters for the next cohort
windowLength(myWindowLength, size = sizeRecommendation)
#> $patientGap
#> [1] 0 7 3
#> 
#> $patientFollow
#> [1] 7
#> 
#> $patientFollowMin
#> [1] 14
#> 

# nolint end
# nolint start

# Create the data
data <- DataDA(
  x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
  y = c(0, 0, 1, 1, 0, 0, 1, 0),
  doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2)),
  u = c(42, 30, 15, 5, 20, 25, 30, 60),
  t0 = c(0, 15, 30, 40, 55, 70, 75, 85),
  Tmax = 60
)
#> Used default patient IDs!
#> Used best guess cohort indices!

# Initialize the CRM model used to model the data
npiece_ <- 10
lambda_prior <- function(k) {
  npiece_ / (data@Tmax * (npiece_ - k + 0.5))
}

model <- DALogisticLogNormal(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
  ref_dose = 56,
  npiece = npiece_,
  l = as.numeric(t(apply(
    as.matrix(c(1:npiece_), 1, npiece_),
    2,
    lambda_prior
  ))),
  c_par = 2
)

# Set-up some MCMC parameters and generate samples from the posterior
options <- McmcOptions(burnin = 100, step = 2, samples = 200)
set.seed(94)
samples <- mcmc(data, model, options)

# Define the rule for dose increments and calculate the maximum dose allowed
myIncrements <- IncrementsRelative(
  intervals = c(0, 20),
  increments = c(1, 0.33)
)
nextMaxDose <- maxDose(myIncrements, data = data)

# Define the rule which will be used to select the next best dose
# based on the class 'NextBestNCRM'
myNextBest <- NextBestNCRM(
  target = c(0.2, 0.35),
  overdose = c(0.35, 1),
  max_overdose_prob = 0.25
)

# Calculate the next best dose
doseRecommendation <- nextBest(
  myNextBest,
  doselimit = nextMaxDose,
  samples = samples,
  model = model,
  data = data
)

# Define the rule which will be used to select the next cohort size
# based on the class 'CohortSizeConst'
mySize <- CohortSizeConst(size = 3)

# Determine the cohort size for the next cohort
sizeRecommendation <- size(mySize, dose = doseRecommendation$value, data = data)

# Rule for having safety window length with constant safety window parameters
myWindowLength <- SafetyWindowConst(gap = c(7, 3), follow = 7, follow_min = 14)

# Determine the safety window parameters for the next cohort
windowLength(myWindowLength, size = sizeRecommendation)
#> $patientGap
#> [1] 0 7 3
#> 
#> $patientFollow
#> [1] 7
#> 
#> $patientFollowMin
#> [1] 14
#> 

# nolint end
```
