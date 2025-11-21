# Simulate outcomes from a rule-based design

**\[stable\]**

## Usage

``` r
# S4 method for class 'RuleDesign'
simulate(
  object,
  nsim = 1L,
  seed = NULL,
  truth,
  args = NULL,
  parallel = FALSE,
  nCores = min(parallel::detectCores(), 5L),
  ...
)
```

## Arguments

- object:

  the
  [`RuleDesign`](https://openpharma.github.io/crmPack/reference/RuleDesign-class.md)
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
  simulations.

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
[`GeneralSimulations`](https://openpharma.github.io/crmPack/reference/GeneralSimulations-class.md)

## Examples

``` r
# nolint start

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

model <- LogisticLogNormal(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
  ref_dose = 50
)

## define the true function
myTruth <- probFunction(model, alpha0 = 7, alpha1 = 8)

# Perform the simulation
## For illustration purpose only 10 simulation is produced (nsim=10).
threeSims <- simulate(
  myDesign,
  nsim = 10,
  seed = 35,
  truth = myTruth,
  parallel = FALSE
)

# nolint end
```
