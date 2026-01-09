# Get specific parameter samples and produce a data.frame

Here you have to specify with `pos` which parameter you would like to
extract from the
[`Samples`](https://openpharma.github.io/crmPack/reference/Samples-class.md)
object

## Usage

``` r
# S4 method for class 'Samples,character'
get(x, pos = -1L, envir = NULL, mode = NULL, inherits = NULL)
```

## Arguments

- x:

  the
  [`Samples`](https://openpharma.github.io/crmPack/reference/Samples-class.md)
  object

- pos:

  the name of the parameter

- envir:

  for vectorial parameters, you can give the indices of the elements you
  would like to extract. If `NULL`, the whole vector samples will be
  returned

- mode:

  not used

- inherits:

  not used

## Value

the data frame suitable for use with
[`ggmcmc`](https://rdrr.io/pkg/ggmcmc/man/ggmcmc.html)

## Examples

``` r
# nolint start

# Create some data
data <- Data(
  x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
  y = c(0, 0, 0, 0, 0, 0, 1, 0),
  cohort = c(0, 1, 2, 3, 4, 5, 5, 5),
  doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2))
)
#> Used default patient IDs!

# Initialize a model
model <- LogisticLogNormal(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
  ref_dose = 56
)

# Get posterior for all model parameters
options <- McmcOptions(burnin = 100, step = 2, samples = 2000)
set.seed(94)
samples <- mcmc(data, model, options)

# now extract the alpha0 samples (intercept of the regression model)
alpha0samples <- get(samples, "alpha0")

# nolint end
```
