# Approximate posterior with (log) normal distribution

To reproduce the resultant approximate model in the future exactly,
include `seed = xxxx` in the call to `approximate`.

## Usage

``` r
approximate(object, model, data, ...)

# S4 method for class 'Samples'
approximate(
  object,
  model,
  data,
  points = seq(from = min(data@doseGrid), to = max(data@doseGrid), length = 5L),
  refDose = median(points),
  logNormal = FALSE,
  verbose = TRUE,
  create_plot = TRUE,
  ...
)
```

## Arguments

- object:

  the
  [`Samples`](https://openpharma.github.io/crmPack/reference/Samples-class.md)
  object

- model:

  the
  [`GeneralModel`](https://openpharma.github.io/crmPack/reference/GeneralModel-class.md)
  object

- data:

  the
  [`Data`](https://openpharma.github.io/crmPack/reference/Data-class.md)
  object

- ...:

  additional arguments (see methods)

- points:

  optional parameter, which gives the dose values at which the
  approximation should rely on (default: 5 values equally spaced from
  minimum to maximum of the dose grid)

- refDose:

  the reference dose to be used (default: median of `points`)

- logNormal:

  use the log-normal prior? (not default) otherwise, the normal prior
  for the logistic regression coefficients is used

- verbose:

  be verbose (progress statements)? (default)

- create_plot:

  add a `ggplot2` object to the return value (default)

## Value

a `list` containing the approximation model and, if requested, a
`ggplot2` object containing a graphical representation of the fitted
model

## Functions

- `approximate(Samples)`: Here the ... argument can transport additional
  arguments for
  [`Quantiles2LogisticNormal`](https://openpharma.github.io/crmPack/reference/Quantiles2LogisticNormal.md),
  e.g. in order to control the approximation quality, etc.

## Examples

``` r
# nolint start

# Create some data
data <- Data(
  x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
  y = c(0, 0, 0, 0, 0, 0, 1, 0),
  cohort = c(0, 1, 2, 3, 4, 5, 5, 5),
  doseGrid = c(
    0.1,
    0.5,
    1.5,
    3,
    6,
    seq(from = 10, to = 80, by = 2)
  )
)
#> Used default patient IDs!

# Initialize a model
model <- LogisticLogNormal(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
  ref_dose = 56
)

# Get posterior for all model parameters
options <- McmcOptions(
  burnin = 100,
  step = 2,
  samples = 2000
)
set.seed(94)
samples <- mcmc(data, model, options)

# Approximate the posterior distribution with a bivariate normal
# max.time and maxit are very small only for the purpose of showing the example. They
# should be increased for a real case.
set.seed(94)
approximation <- approximate(
  object = samples,
  model = model,
  data = data,
  logNormal = TRUE,
  control = list(
    threshold.stop = 0.1,
    max.time = 1,
    maxit = 1
  )
)
#> Have got accurate energy 0.08100130116 <= 0.1 in smooth search
#> Emini is: 0.08100130116
#> xmini are:
#> -0.2151250962 0.04243230926 0.8111240864 0.5192893993 0.09173024612 
#> Totally it used 6.4e-05 secs
#> No. of function call is: 1

posterior <- approximation$model

# nolint end
```
