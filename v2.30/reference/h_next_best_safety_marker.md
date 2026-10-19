# Add the Safety-Rule Dose Marker

Add the Safety-Rule Dose Marker

## Usage

``` r
h_next_best_safety_marker(plot, safe_dose, probability, dose_grid, dose_scale)
```

## Arguments

- plot:

  (`ggplot`) plot to decorate.

- safe_dose:

  (`number`) maximum dose passing the overdose criterion.

- probability:

  (`proportion`) probability at each dose-grid value.

- dose_grid:

  (`numeric`) ordered dose grid.

- dose_scale:

  (`string`) dose-axis scale.

## Value

The decorated `ggplot2` object, or the original plot when there is no
finite safe dose.
