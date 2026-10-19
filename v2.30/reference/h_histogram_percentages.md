# Convenience Function to Make Histograms of Percentages

Creates a histogram where the height of each bin is the percentage of
all observations in that bin.

## Usage

``` r
h_histogram_percentages(x, description, bins = 30L)
```

## Arguments

- x:

  (`numeric`) vector of samples.

- description:

  (`string`) x-axis label.

- bins:

  (`count`) number of histogram bins.

## Value

A `ggplot2` object.
