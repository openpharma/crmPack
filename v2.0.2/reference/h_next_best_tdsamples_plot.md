# Building the Plot for `nextBest-NextBestTDsamples` Method.

**\[experimental\]**

Helper function which creates the plot for
[`nextBest-NextBestTDsamples()`](https://openpharma.github.io/crmPack/reference/nextBest.md)
method.

## Usage

``` r
h_next_best_tdsamples_plot(
  dose_target_drt_samples,
  dose_target_eot_samples,
  dose_target_drt,
  dose_target_eot,
  dose_grid_range,
  nextBest,
  doselimit,
  next_dose
)
```

## Arguments

- dose_target_drt_samples:

  (`numeric`)  
  vector of in-trial samples.

- dose_target_eot_samples:

  (`numeric`)  
  vector of end-of-trial samples.

- dose_target_drt:

  (`number`)  
  target in-trial estimate.

- dose_target_eot:

  (`number`)  
  target end-of-trial estimate.

- dose_grid_range:

  (`numeric`)  
  range of dose grid.

- nextBest:

  (`NextBestTDsamples`)  
  the rule for the next best dose.

- doselimit:

  (`number`)  
  the maximum allowed next dose.

- next_dose:

  (`number`)  
  next best dose.
