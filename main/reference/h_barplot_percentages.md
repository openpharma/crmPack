# Convenience function to make barplots of percentages

Convenience function to make barplots of percentages

## Usage

``` r
h_barplot_percentages(
  x,
  description,
  xaxisround = 0,
  x_is_discrete = FALSE,
  axis_text_angle = 0
)
```

## Arguments

- x:

  vector of samples

- description:

  xlab string

- xaxisround:

  rounding for xaxis labels (default: 0, i.e. integers will be used)

- x_is_discrete:

  whether the values on the x-axis should be treated as discrete
  categories

- axis_text_angle:

  (`number`) rotation angle for x-axis tick labels.

## Value

the ggplot2 object
