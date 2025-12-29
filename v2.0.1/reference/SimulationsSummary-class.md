# `SimulationsSummary`

**\[stable\]**

In addition to the slots in the parent class
[`GeneralSimulationsSummary`](https://openpharma.github.io/crmPack/reference/GeneralSimulationsSummary-class.md),
it contains two slots with model fit information.

## Usage

``` r
.DefaultSimulationsSummary()
```

## Slots

- `stop_report`:

  (`matrix`)  
  matrix of stopping rule outcomes

- `fit_at_dose_most_selected`:

  (`numeric`)  
  fitted toxicity rate at dose most often selected

- `additional_stats`:

  (`list`)  
  list of additional statistical summary

- `mean_fit`:

  (`list`)  
  list with the average, lower (2.5%) and upper (97.5%) quantiles of the
  mean fitted toxicity at each dose level

## Note

Typically, end users will not use the `.DefaultSimulationsSummary()`
function.
