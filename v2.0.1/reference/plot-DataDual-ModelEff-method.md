# Plot of the fitted dose-efficacy based with a given pseudo efficacy model and data without samples

Plot of the fitted dose-efficacy based with a given pseudo efficacy
model and data without samples

## Usage

``` r
# S4 method for class 'DataDual,ModelEff'
plot(
  x,
  y,
  ...,
  xlab = "Dose level",
  ylab = "Expected Efficacy",
  showLegend = TRUE
)
```

## Arguments

- x:

  the data of
  [`DataDual`](https://openpharma.github.io/crmPack/reference/DataDual-class.md)
  class object

- y:

  the model of the
  [`ModelEff`](https://openpharma.github.io/crmPack/reference/ModelEff-class.md)
  class object

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
for the dose-efficacy model plot

## Examples

``` r
# nolint start

##plot the dose-efficacy curve given a pseudo efficacy model using data without samples
##data must be of 'DataDual' class
##define the data
data <- DataDual(
  x = c(25, 50, 50, 75, 100, 100, 225, 300),
  y = c(0, 0, 0, 0, 1, 1, 1, 1),
  w = c(0.31, 0.42, 0.59, 0.45, 0.6, 0.7, 0.6, 0.52),
  doseGrid = seq(25, 300, 25),
  placebo = FALSE
)
#> Used default patient IDs!
#> Used best guess cohort indices!
##model must be from 'ModelEff' class e.g 'Effloglog' class model
##define the model (see Effloglog example)
Effmodel <- Effloglog(
  eff = c(1.223, 2.513),
  eff_dose = c(25, 300),
  nu = c(a = 1, b = 0.025),
  data = data
)
## plot the dose-efficacy curve
## 'x' is the data and 'y' is the model in plot
plot(x = data, y = Effmodel)


# nolint end
```
