# Helper Function to Calculate Fit Summary

**\[stable\]**

Calculates fit summary statistics for pseudo simulations.

## Usage

``` r
h_pseudo_sim_fit_summary(fit_list, x_most_selected, dose_grid, truth)
```

## Arguments

- fit_list:

  (`list`)  
  list of fit objects from simulations.

- x_most_selected:

  (`integer`)  
  index of dose most often selected.

- dose_grid:

  (`numeric`)  
  dose grid.

- truth:

  (`function`)  
  truth function.

## Value

A list with `fit_at_dose_most_selected` and `mean_fit` components.
