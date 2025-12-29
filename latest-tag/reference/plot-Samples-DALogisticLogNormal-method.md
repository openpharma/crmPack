# Plotting dose-toxicity model fits

Plotting dose-toxicity model fits

## Usage

``` r
# S4 method for class 'Samples,DALogisticLogNormal'
plot(x, y, data, hazard = FALSE, ..., showLegend = TRUE)
```

## Arguments

- x:

  the
  [`Samples`](https://openpharma.github.io/crmPack/reference/Samples-class.md)
  object

- y:

  the
  [`DALogisticLogNormal`](https://openpharma.github.io/crmPack/reference/DALogisticLogNormal-class.md)
  object

- data:

  the
  [`DataDA`](https://openpharma.github.io/crmPack/reference/DataDA-class.md)
  object

- hazard:

  see
  [`fitPEM`](https://openpharma.github.io/crmPack/reference/fitPEM.md)
  for the explanation

- ...:

  not used

- showLegend:

  should the legend be shown? (default)

## Value

This returns the
[`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html) object
for the dose-toxicity model fit
