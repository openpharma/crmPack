# Add Next-Best Dose References to a Probability Plot

Add Next-Best Dose References to a Probability Plot

## Usage

``` r
h_next_best_reference_lines(
  plot,
  dose_grid,
  dose_scale,
  doselimit = NA_real_,
  safe_dose = NA_real_,
  overdose_threshold = NA_real_
)
```

## Arguments

- plot:

  (`ggplot`) plot to decorate.

- dose_grid:

  (`numeric`) ordered dose grid.

- dose_scale:

  (`string`) dose-axis scale.

- doselimit:

  (`number`) maximum allowed next dose.

- safe_dose:

  (`number`) maximum dose passing the overdose criterion.

- overdose_threshold:

  (`number`) overdose probability threshold in percentage points.

## Value

The decorated `ggplot2` object.
