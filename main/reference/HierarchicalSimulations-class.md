# `HierarchicalSimulations`

**\[experimental\]**

This class captures trial simulations from hierarchical designs
([`HierarchicalDesign`](https://docs.crmpack.org/reference/HierarchicalDesign-class.md)).

## Usage

``` r
HierarchicalSimulations(
  data,
  doses,
  samples,
  fit,
  stop_reasons,
  stop_report,
  additional_stats,
  seed
)

.DefaultHierarchicalSimulations()
```

## Arguments

- data:

  (`list`)\
  see slot definition.

- doses:

  (`list`)\
  see slot definition.

- samples:

  (`list`)\
  see slot definition.

- fit:

  (`list`)\
  see slot definition.

- stop_reasons:

  (`list`)\
  see slot definition.

- stop_report:

  (`list`)\
  see slot definition.

- additional_stats:

  (`list`)\
  see slot definition.

- seed:

  (`integer`)\
  see slot definition.

## Slots

- `data`:

  (`list`)\
  produced
  [`HierarchicalData`](https://docs.crmpack.org/reference/HierarchicalData-class.md)
  objects.

- `doses`:

  (`list`)\
  final dose recommendations, one named list per simulation run with
  entries for each hierarchical arm.

- `samples`:

  (`list`)\
  final
  [`HierarchicalSamples`](https://docs.crmpack.org/reference/HierarchicalSamples-class.md)
  objects.

- `fit`:

  (`list`)\
  final arm-specific fits for each simulation run.

- `stop_reasons`:

  (`list`)\
  arm-specific stopping reasons for each simulation run.

- `stop_report`:

  (`list`)\
  arm-specific stopping rule outcomes for each simulation run.

- `additional_stats`:

  (`list`)\
  additional arm-specific statistics.

- `seed`:

  (`integer`)\
  random generator state before starting the simulation.

## Note

Typically, end users will not use the
`.DefaultHierarchicalSimulations()` function directly.
