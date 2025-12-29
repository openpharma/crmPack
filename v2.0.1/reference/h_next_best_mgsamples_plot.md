# Building the Plot for `nextBest-NextBestMaxGainSamples` Method.

**\[experimental\]**

Helper function which creates the plot for
[`nextBest-NextBestMaxGainSamples()`](https://openpharma.github.io/crmPack/reference/nextBest.md)
method.

## Usage

``` r
h_next_best_mgsamples_plot(
  prob_target_drt,
  dose_target_drt,
  prob_target_eot,
  dose_target_eot,
  dose_mg,
  dose_mg_samples,
  next_dose,
  doselimit,
  dose_grid_range
)
```

## Arguments

- prob_target_drt:

  (`proportion`)  
  target DLT probability during the trial.

- dose_target_drt:

  (`number`)  
  target dose estimate during the trial.

- prob_target_eot:

  (`proportion`)  
  target DLT probability at the end of the trial.

- dose_target_eot:

  (`number`)  
  target dose estimate at the end of the trial.

- dose_mg:

  (`number`)  
  the dose corresponding to the maximum gain.

- dose_mg_samples:

  (`numeric`)  
  for every sample, the dose (from the dose grid) that gives the maximum
  gain value.

- next_dose:

  (`number`)  
  next best dose.

- doselimit:

  (`number`)  
  the maximum allowed next dose.

- dose_grid_range:

  (`numeric`)  
  dose grid range.
