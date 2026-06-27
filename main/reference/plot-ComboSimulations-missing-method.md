# Plot `ComboSimulations`

**\[experimental\]**

Summarize two-drug combination simulations with plots.

This plot method can be applied to
[`ComboSimulations`](https://docs.crmpack.org/reference/ComboSimulations-class.md)
objects. Possible `type`s of plots are:

- trajectory:

  Summary of simulated dose trajectories for each drug

- dosesTried:

  Average proportions of tested doses for each drug

- trajectory2D:

  2D dose-plane evolution with arrows for dose transitions, and labels
  showing DLTs/patients plus a step index hint

## Usage

``` r
# S4 method for class 'ComboSimulations,missing'
plot(x, y, type = c("trajectory", "dosesTried", "trajectory2D"), ...)
```

## Arguments

- x:

  (`ComboSimulations`)\
  the object we want to plot from.

- y:

  (`missing`)\
  not used.

- type:

  (`character`)\
  the type of plots you want to obtain.

- ...:

  not used.

## Value

A single `ggplot` object if a single panel is produced, otherwise a
`gtable` object.
