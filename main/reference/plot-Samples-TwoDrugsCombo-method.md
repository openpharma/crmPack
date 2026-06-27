# Plotting two-drug combination dose-toxicity model fits

Plotting two-drug combination dose-toxicity model fits

## Usage

``` r
# S4 method for class 'Samples,TwoDrugsCombo'
plot(
  x,
  y,
  data,
  ...,
  xlab = NULL,
  ylab = NULL,
  fillLab = "Probability of DLT [%]",
  showLegend = TRUE
)
```

## Arguments

- x:

  the [Samples](https://docs.crmpack.org/reference/Samples-class.md)
  object.

- y:

  the
  [TwoDrugsCombo](https://docs.crmpack.org/reference/TwoDrugsCombo-class.md)
  object.

- data:

  the [DataCombo](https://docs.crmpack.org/reference/DataCombo-class.md)
  object.

- ...:

  passed to [`fit`](https://docs.crmpack.org/reference/fit.md).

- xlab:

  the x axis label. If `NULL`, the first drug name is used.

- ylab:

  the y axis label. If `NULL`, the second drug name is used.

- fillLab:

  the fill legend label.

- showLegend:

  should the legend be shown? (default)

## Value

This returns the
[`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html) object
for the two-drug combination dose-toxicity model fit.
