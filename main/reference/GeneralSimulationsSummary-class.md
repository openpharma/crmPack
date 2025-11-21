# `GeneralSimulationsSummary`

**\[stable\]**

This class captures the summary of general simulations output. Note that
objects should not be created by users, therefore no initialization
function is provided for this class.

## Usage

``` r
.DefaultGeneralSimulationsSummary()
```

## Slots

- `target`:

  (`numeric`)  
  target toxicity interval

- `target_dose_interval`:

  (`numeric`)  
  corresponding target dose interval

- `nsim`:

  (`integer`)  
  number of simulations

- `prop_dlts`:

  (`ANY`)  
  A numeric array (multi-dimensional) or list representing proportions
  of DLTs in the trials

- `mean_tox_risk`:

  (`numeric`)  
  mean toxicity risks for the patients

- `dose_selected`:

  (`numeric`)  
  doses selected as MTD

- `tox_at_doses_selected`:

  (`numeric`)  
  true toxicity at doses selected

- `prop_at_target`:

  (`numeric`)  
  Proportion of trials selecting target MTD

- `dose_most_selected`:

  (`numeric`)  
  dose most often selected as MTD

- `obs_tox_rate_at_dose_most_selected`:

  (`numeric`)  
  observed toxicity rate at dose most often selected

- `n_obs`:

  (`ANY`)  
  A numeric array (multi-dimensional) or list representing number of
  patients overall.

- `n_above_target`:

  (`integer`)  
  number of patients treated above target tox interval

- `dose_grid`:

  (`numeric`)  
  the dose grid that has been used

- `placebo`:

  (`logical`)  
  set to TRUE (default is FALSE) for a design with placebo

## Note

Typically, end users will not use the
`.DefaultGeneralSimulationsSummary()` function.
