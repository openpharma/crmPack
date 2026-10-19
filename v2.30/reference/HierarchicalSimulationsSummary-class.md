# `HierarchicalSimulationsSummary`

**\[experimental\]**

This class captures arm-specific summaries from
[`HierarchicalSimulations`](https://docs.crmpack.org/reference/HierarchicalSimulations-class.md)
objects.

## Usage

``` r
.DefaultHierarchicalSimulationsSummary()
```

## Slots

- `arms`:

  (`list`)\
  named list of arm-level simulation summary objects.

- `nsim`:

  (`integer`)\
  number of simulations.

## Note

Typically, end users will not use the
`.DefaultHierarchicalSimulationsSummary()` function.
