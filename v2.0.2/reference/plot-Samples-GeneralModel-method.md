# Plotting dose-toxicity model fits

Plotting dose-toxicity model fits

## Usage

``` r
# S4 method for class 'Samples,GeneralModel'
plot(
  x,
  y,
  data,
  ...,
  xlab = "Dose level",
  ylab = "Probability of DLT [%]",
  showLegend = TRUE
)
```

## Arguments

- x:

  the
  [`Samples`](https://openpharma.github.io/crmPack/reference/Samples-class.md)
  object

- y:

  the
  [`GeneralModel`](https://openpharma.github.io/crmPack/reference/GeneralModel-class.md)
  object

- data:

  the
  [`Data`](https://openpharma.github.io/crmPack/reference/Data-class.md)
  object

- ...:

  not used

- xlab:

  the x axis label

- ylab:

  the y axis label

- showLegend:

  should the legend be shown? (default)

## Value

This returns the
[`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html) object
for the dose-toxicity model fit

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

# Plot the posterior mean  (and empirical 2.5 and 97.5 percentile)
# for the prob(DLT) by doses
plot(x = samples, y = model, data = data)


# nolint end
```
