# Plot `GeneralSimulationsSummary`

**\[stable\]**

Graphical display of the general simulation summary.

This plot method can be applied to
[`GeneralSimulationsSummary`](https://docs.crmpack.org/reference/GeneralSimulationsSummary-class.md)
objects in order to summarize them graphically.

## Usage

``` r
# S4 method for class 'GeneralSimulationsSummary,missing'
plot(
  x,
  y,
  type = c("nObs", "doseSelected", "propDLTs", "nAboveTarget"),
  axis_text_angle = 45,
  true_mtd_legend = TRUE,
  ...
)
```

## Arguments

- x:

  (`GeneralSimulationsSummary`)\
  the object we want to plot from.

- y:

  (`missing`)\
  not used.

- type:

  (`character`)\
  the types of plots you want to obtain, see details.

- axis_text_angle:

  (`number`)\
  rotation angle for the MTD estimate x-axis tick labels. Defaults to 45
  degrees.

- true_mtd_legend:

  (`flag`)\
  whether to show the legend for true MTD triangles. Defaults to `TRUE`.

- ...:

  not used.

## Value

A single `ggplot` object if a single plot is asked for, otherwise a
`gtable` object.

## Details

The following plot types are available:

- `"nObs"`:

  The distribution of the total number of patients in the simulated
  trials. For trials with a placebo, only patients assigned to an active
  dose are included.

- `"doseSelected"`:

  The distribution of the final selected dose (MTD) across trials. A
  selected dose of zero indicates that the trial stopped because all
  doses in the dose grid appeared too toxic. Red triangles mark
  dose-grid levels whose true toxicity is within the target interval.

- `"propDLTs"`:

  The distribution of the percentage of patients with dose-limiting
  toxicities (DLTs). For trials with a placebo, this is the percentage
  among patients assigned to an active dose.

- `"nAboveTarget"`:

  The distribution of the number of patients treated at doses above the
  target toxicity interval, as determined by the `truth` and `target`
  arguments supplied to
  [`summary,GeneralSimulations-method`](https://docs.crmpack.org/reference/summary-GeneralSimulations-method.md).

Any subset of these plot types can be requested with `type`.
