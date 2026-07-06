# Simulate outcomes from a hierarchical CRM design

**\[experimental\]**

## Usage

``` r
# S4 method for class 'HierarchicalDesign'
simulate(
  object,
  nsim = 1L,
  seed = NULL,
  truth,
  truthResponse = plogis,
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
  [`HierarchicalDesign`](https://docs.crmpack.org/reference/HierarchicalDesign-class.md)
  object we want to simulate data from.

- nsim:

  (`count`)\
  the number of simulations.

- seed:

  see [`set_seed()`](https://docs.crmpack.org/reference/set_seed.md).

- truth:

  (`function` or named `list` of `function`)\
  true DLT probability function(s). If a list is supplied, names must
  match the hierarchical arms.

- truthResponse:

  (`function` or named `list` of `function`)\
  true response probability function(s).

- args:

  (`data.frame`)\
  arguments for the truth functions.

- firstSeparate:

  (`flag`)\
  enroll the first patient separately from the rest of the cohort? (not
  default) If yes, the cohort will be closed if a DLT occurs in this
  patient.

- mcmcOptions:

  ([McmcOptions](https://docs.crmpack.org/reference/McmcOptions-class.md))\
  object of class
  [`McmcOptions`](https://docs.crmpack.org/reference/McmcOptions-class.md),
  giving the MCMC options for each evaluation in the trial. By default,
  the standard options are used

- parallel:

  (`flag`)\
  should the simulation runs be parallelized across the clusters of the
  computer? (not default)

- nCores:

  (`count`)\
  how many cores should be used for parallel computing? Defaults to the
  number of cores on the machine, maximum 5.

- derive:

  (`list`)\
  a named list of functions which derives statistics, based on the
  vector of posterior MTD samples. Each list element must therefore
  accept one and only one argument, which is a numeric vector, and
  return a number.

- ...:

  not used

## Value

an object of class
[`HierarchicalSimulations`](https://docs.crmpack.org/reference/HierarchicalSimulations-class.md).
