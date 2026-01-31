# `PseudoDualSimulationsSummary`

**\[stable\]**

This class captures the summary of the dual responses simulations using
pseudo models. It contains all slots from
[`PseudoSimulationsSummary`](https://openpharma.github.io/crmPack/reference/PseudoSimulationsSummary-class.md)
object. In addition to the slots in the parent class
[`PseudoSimulationsSummary`](https://openpharma.github.io/crmPack/reference/PseudoSimulationsSummary-class.md),
it contains additional slots for the efficacy model fit information.

Note that objects should not be created by users, therefore no
initialization function is provided for this class.

## Usage

``` r
.DefaultPseudoDualSimulationsSummary()
```

## Slots

- `target_gstar`:

  (`numeric`)  
  the target dose level such that its gain value is at maximum

- `target_gstar_at_dose_grid`:

  (`numeric`)  
  the dose level at dose Grid closest and below Gstar

- `gstar_summary`:

  (`table`)  
  the six-number table summary (lowest, 25th, 50th (median), 75th
  percentile, mean and highest value) of the final Gstar values obtained
  across all simulations

- `ratio_gstar_summary`:

  (`table`)  
  the six-number summary table of the ratios of the upper to the lower
  95% credibility intervals of the final Gstar across all simulations

- `eff_fit_at_dose_most_selected`:

  (`numeric`)  
  fitted expected mean efficacy value at dose most often selected

- `mean_eff_fit`:

  (`list`)  
  list with mean, lower (2.5%) and upper (97.5%) quantiles of the fitted
  expected efficacy value at each dose level.

## Note

Typically, end users will not use the
`.DefaultPseudoDualSimulationsSummary()` function.
