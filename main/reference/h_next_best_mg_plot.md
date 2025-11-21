# Building the Plot for `nextBest-NextBestMaxGain` Method.

**\[experimental\]**

Helper function which creates the plot for
[`nextBest-NextBestMaxGain()`](https://openpharma.github.io/crmPack/reference/nextBest.md)
method.

## Usage

``` r
h_next_best_mg_plot(
  prob_target_drt,
  dose_target_drt,
  prob_target_eot,
  dose_target_eot,
  dose_mg,
  max_gain,
  next_dose,
  doselimit,
  data,
  model,
  model_eff
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

- max_gain:

  (`number`)  
  the maximum gain estimate.

- next_dose:

  (`number`)  
  next best dose.

- doselimit:

  (`number`)  
  the maximum allowed next dose.

- data:

  (`DataDual`)  
  the data object from which the dose grid will be fetched.

- model:

  (`ModelTox`)  
  the DLT model.

- model_eff:

  (`Effloglog`)  
  the efficacy model.
