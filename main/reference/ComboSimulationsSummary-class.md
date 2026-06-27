# `ComboSimulationsSummary`

**\[experimental\]**

This class captures summary output from
[`ComboSimulations`](https://docs.crmpack.org/reference/ComboSimulations-class.md)
objects.

## Usage

``` r
.DefaultComboSimulationsSummary()
```

## Slots

- `target`:

  (`numeric`) target toxicity interval. Empty if no truth function was
  supplied.

- `nsim`:

  (`integer`) number of simulations.

- `n_obs`:

  (`integer`) number of patients in each simulation.

- `prop_dlts`:

  (`numeric`) observed proportion of DLTs in each simulation.

- `mean_tox_risk`:

  (`numeric`) average fitted toxicity risk in each simulation.

- `dose_selected`:

  (`matrix`) selected dose combinations; one row per simulation.

- `tox_at_doses_selected`:

  (`numeric`) true toxicity at selected dose combinations (if truth
  supplied).

- `prop_at_target`:

  (`numeric`) proportion of selected combinations within target interval
  (if truth supplied).

- `dose_most_selected`:

  (`numeric`) most frequently selected dose combination.

- `obs_tox_rate_at_dose_most_selected`:

  (`numeric`) observed toxicity rate at the most frequently selected
  combination.

- `dose_grid`:

  (`list`) dose grid for each drug.

- `stop_report`:

  (`matrix`) matrix of stopping rule outcomes.

- `stop_reasons`:

  (`list`) stopping reasons by simulation.

- `additional_stats`:

  (`list`) additional statistics.

## Note

Typically, end users will not use the
`.DefaultComboSimulationsSummary()` function.
