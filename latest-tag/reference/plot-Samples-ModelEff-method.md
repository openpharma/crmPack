# Plot the fitted dose-efficacy curve using a model from [`ModelEff`](https://openpharma.github.io/crmPack/reference/ModelEff-class.md) class with samples

Plot the fitted dose-efficacy curve using a model from
[`ModelEff`](https://openpharma.github.io/crmPack/reference/ModelEff-class.md)
class with samples

## Usage

``` r
# S4 method for class 'Samples,ModelEff'
plot(
  x,
  y,
  data,
  ...,
  xlab = "Dose level",
  ylab = "Expected Efficacy",
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
  [`ModelEff`](https://openpharma.github.io/crmPack/reference/ModelEff-class.md)
  model class object

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
for the dose-efficacy model fit

## Examples

``` r
# nolint start

## we need a data object with doses >= 1:
data <- DataDual(
  x = c(25, 50, 25, 50, 75, 300, 250, 150),
  y = c(0, 0, 0, 0, 0, 1, 1, 0),
  w = c(0.31, 0.42, 0.59, 0.45, 0.6, 0.7, 0.6, 0.52),
  doseGrid = seq(25, 300, 25),
  placebo = FALSE
)
#> Used default patient IDs!
#> Used best guess cohort indices!
##plot the dose-efficacy curve with samples using the model from 'ModelEff'
##class e.g. 'Effloglog' class model
##define the model (see Effloglog example)
Effmodel <- Effloglog(
  eff = c(1.223, 2.513),
  eff_dose = c(25, 300),
  nu = c(a = 1, b = 0.025),
  data = data
)
## define the samples obtained using the 'Effloglog' model (see details in 'Samples' example)
##options for MCMC
options <- McmcOptions(burnin = 100, step = 2, samples = 200)
## samples must be of 'Samples' class
samples <- mcmc(data = data, model = Effmodel, options = options)
## plot the fitted dose-efficacy curve including the 95% credibility interval of the samples
## 'x' should be of 'Samples' class and 'y' of 'ModelEff' class
plot(x = samples, y = Effmodel, data = data)

# nolint end
```
