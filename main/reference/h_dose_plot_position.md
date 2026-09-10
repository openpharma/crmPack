# Convert Dose Values to Plot Positions

Convert Dose Values to Plot Positions

## Usage

``` r
h_dose_plot_position(dose, dose_grid, dose_scale)
```

## Arguments

- dose:

  (`numeric`) dose values to position.

- dose_grid:

  (`numeric`) ordered dose grid.

- dose_scale:

  (`string`) dose-axis scale.

## Value

Numeric plot positions. For a factor scale these are equally spaced
dose-level indices; otherwise the original dose values are returned.
