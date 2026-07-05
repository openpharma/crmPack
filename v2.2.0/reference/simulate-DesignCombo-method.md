# Simulate outcomes from a two-drug combination CRM design

**\[experimental\]**

## Usage

``` r
# S4 method for class 'DesignCombo'
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
  [`DesignCombo`](https://docs.crmpack.org/reference/DesignCombo-class.md)
  object we want to simulate data from

- nsim:

  (`count`) the number of simulations (default: 1).

- seed:

  see [`set_seed()`](https://docs.crmpack.org/reference/set_seed.md)

- truth:

  (`function`) a function that takes as input a dose combination (named
  numeric vector of length 2) and returns the true DLT probability.
  Additional arguments can be supplied in `args`.

- args:

  (`data.frame`) data frame with arguments for the `truth` function. The
  column names correspond to the argument names and rows are recycled
  across simulations.

- firstSeparate:

  (`flag`) enroll the first patient separately from the rest of the
  cohort? If yes, the cohort is closed when a DLT occurs in this first
  patient.

- mcmcOptions:

  ([`McmcOptions`](https://docs.crmpack.org/reference/McmcOptions-class.md))
  MCMC options used for each trial evaluation.

- parallel:

  (`flag`) should simulation runs be parallelized?

- nCores:

  (`count`) number of cores used when `parallel = TRUE`.

- derive:

  (`list`) named list of functions deriving extra statistics from
  posterior toxicity samples at the final recommended combination.

- ...:

  not used.

## Value

an object of class
[`ComboSimulations`](https://docs.crmpack.org/reference/ComboSimulations-class.md)

## Note

Backfill cohorts are not yet implemented for
[`DesignCombo`](https://docs.crmpack.org/reference/DesignCombo-class.md)
simulations and therefore lead to an error if used.

## Examples

``` r
# nolint start

design_combo <- .DefaultDesignCombo()

true_tox_combo <- function(dose) {
  plogis(-6 + 0.08 * dose[1] + 0.06 * dose[2] + 0.001 * dose[1] * dose[2])
}

options <- McmcOptions(
  burnin = 50,
  step = 1,
  samples = 50,
  rng_kind = "Mersenne-Twister",
  rng_seed = 1
)

my_sims_combo <- simulate(
  design_combo,
  truth = true_tox_combo,
  nsim = 1,
  seed = 819,
  mcmcOptions = options,
  parallel = FALSE
)

# nolint end
```
