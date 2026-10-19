# Arrange Plots with Aligned Panels

Aligns the widths of all plot-table columns before arranging plots
vertically. This keeps corresponding x-axes aligned when plots have
legends of different widths.

## Usage

``` r
h_arrange_plots_aligned(..., nrow = length(list(...)))
```

## Arguments

- ...:

  (`ggplot`) plots to arrange.

- nrow:

  (`count`) number of rows in the arrangement.

## Value

A `gtable` object containing the aligned plots.
