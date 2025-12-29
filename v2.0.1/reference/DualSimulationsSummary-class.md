# `DualSimulationsSummary`

**\[stable\]** This class captures the summary of dual-endpoint
simulations output. In comparison to its parent class
[`SimulationsSummary`](https://openpharma.github.io/crmPack/reference/SimulationsSummary-class.md),
it has additional slots.

## Usage

``` r
.DefaultDualSimulationsSummary()
```

## Slots

- `biomarker_fit_at_dose_most_selected`:

  (`numeric`)  
  fitted biomarker level at most often selected dose.

- `mean_biomarker_fit`:

  (`list`)  
  list with average, lower (2.5%) and upper (97.5%) quantiles of mean
  fitted biomarker level at each dose

## Note

Typically, end users will not use the `.DefaultDualSimulationsSummary()`
function.
