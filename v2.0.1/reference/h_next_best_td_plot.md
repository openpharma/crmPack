# Building the Plot for `nextBest-NextBestTD` Method.

**\[experimental\]**

Helper function which creates the plot for
[`nextBest-NextBestTD()`](https://openpharma.github.io/crmPack/reference/nextBest.md)
method.

## Usage

``` r
h_next_best_td_plot(
  prob_target_drt,
  dose_target_drt,
  prob_target_eot,
  dose_target_eot,
  data,
  prob_dlt,
  doselimit,
  next_dose
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

- data:

  (`Data`)  
  the data object from which the dose grid will be fetched.

- prob_dlt:

  (`numeric`)  
  DLT probabilities for doses in grid.

- doselimit:

  (`number`)  
  the maximum allowed next dose.

- next_dose:

  (`number`)  
  next best dose.
