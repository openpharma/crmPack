# Plotting dose-toxicity model fits

Plotting dose-toxicity model fits

## Usage

``` r
# S4 method for class 'Samples,DALogisticLogNormal'
plot(x, y, data, hazard = FALSE, ..., showLegend = TRUE)
```

## Arguments

- x:

  the [`Samples`](https://docs.crmpack.org/reference/Samples-class.md)
  object

- y:

  the
  [`DALogisticLogNormal`](https://docs.crmpack.org/reference/DALogisticLogNormal-class.md)
  object

- data:

  the [`DataDA`](https://docs.crmpack.org/reference/DataDA-class.md)
  object

- hazard:

  see [`fitPEM`](https://docs.crmpack.org/reference/fitPEM.md) for the
  explanation

- ...:

  not used

- showLegend:

  should the legend be shown? (default)

## Value

This returns the
[`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html) object
for the dose-toxicity model fit
