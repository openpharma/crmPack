# Plot `ComboSimulationsSummary`

**\[experimental\]**

Graphical display of combination simulation summaries.

## Usage

``` r
# S4 method for class 'ComboSimulationsSummary,missing'
plot(
  x,
  y,
  type = c("nObs", "doseSelectedDrug1", "doseSelectedDrug2", "propDLTs"),
  ...
)
```

## Arguments

- x:

  (`ComboSimulationsSummary`) the object we want to plot from.

- y:

  (`missing`) not used.

- type:

  (`character`) the types of plots you want to obtain.

- ...:

  not used.

## Value

A single `ggplot` object if a single plot is asked for, otherwise a
`gtable` object.
