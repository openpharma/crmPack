# Convert Prior Quantiles to Logistic (Log) Normal Model

**\[stable\]**

This function uses generalized simulated annealing to optimize a
[`LogisticNormal`](https://openpharma.github.io/crmPack/reference/LogisticNormal-class.md)
model to be as close as possible to the given prior quantiles.

## Usage

``` r
Quantiles2LogisticNormal(
  dosegrid,
  refDose,
  lower,
  median,
  upper,
  level = 0.95,
  logNormal = FALSE,
  parstart = NULL,
  parlower = c(-10, -10, 0, 0, -0.95),
  parupper = c(10, 10, 10, 10, 0.95),
  seed = 12345,
  verbose = TRUE,
  control = list(threshold.stop = 0.01, maxit = 50000, temperature = 50000, max.time =
    600)
)
```

## Arguments

- dosegrid:

  (`numeric`)  
  the dose grid.

- refDose:

  (`number`)  
  the reference dose.

- lower:

  (`numeric`)  
  the lower quantiles.

- median:

  (`numeric`)  
  the medians.

- upper:

  (`numeric`)  
  the upper quantiles.

- level:

  (`number`)  
  the credible level of the (lower, upper) intervals. Default is 0.95.

- logNormal:

  (`flag`)  
  use the log-normal prior? If `FALSE` (default), the normal prior for
  the logistic regression coefficients is used.

- parstart:

  (`numeric` or `NULL`)  
  starting values for the parameters. By default, these are determined
  from the medians supplied.

- parlower:

  (`numeric`)  
  lower bounds on the parameters (intercept alpha and the slope beta,
  the corresponding standard deviations and the correlation).

- parupper:

  (`numeric`)  
  upper bounds on the parameters.

- seed:

  (`count`)  
  seed for random number generation.

- verbose:

  (`flag`)  
  should the function be verbose?

- control:

  (`list`)  
  additional options for the optimisation routine, see
  [`GenSA::GenSA()`](https://rdrr.io/pkg/GenSA/man/GenSA.html) for more
  details.

## Value

A list with the best approximating `model`
([`LogisticNormal`](https://openpharma.github.io/crmPack/reference/LogisticNormal-class.md)
or
[`LogisticLogNormal`](https://openpharma.github.io/crmPack/reference/LogisticLogNormal-class.md)),
the resulting `quantiles`, the `required` quantiles and the `distance`
to the required quantiles, as well as the final `parameters` (which
could be used for running the algorithm a second time).
