# `ComboSimulations`

**\[experimental\]**

This class captures trial simulations from two-drug combination designs
([`DesignCombo`](https://docs.crmpack.org/reference/DesignCombo-class.md)).

It is intentionally separate from
[`Simulations`](https://docs.crmpack.org/reference/Simulations-class.md)
because
[`DataCombo`](https://docs.crmpack.org/reference/DataCombo-class.md) is
not a subclass of
[`Data`](https://docs.crmpack.org/reference/Data-class.md), and final
recommendations are dose combinations.

## Usage

``` r
ComboSimulations(
  data,
  doses,
  fit,
  stop_reasons,
  stop_report,
  additional_stats,
  seed
)

.DefaultComboSimulations()
```

## Arguments

- data:

  (`list`) see slot definition.

- doses:

  (`matrix`) see slot definition.

- fit:

  (`list`) see slot definition.

- stop_reasons:

  (`list`) see slot definition.

- stop_report:

  (`matrix`) see slot definition.

- additional_stats:

  (`list`) see slot definition.

- seed:

  (`integer`) see slot definition.

## Slots

- `data`:

  (`list`) produced
  [`DataCombo`](https://docs.crmpack.org/reference/DataCombo-class.md)
  objects.

- `doses`:

  (`matrix`) final recommended dose combinations; one row per simulation
  run and two columns (one per drug).

- `fit`:

  (`list`) final fitted toxicity surfaces for each simulation run.

- `stop_reasons`:

  (`list`) stopping reasons for each simulation run.

- `stop_report`:

  (`matrix`) matrix of stopping rule outcomes.

- `additional_stats`:

  (`list`) list of additional statistical summary values.

- `seed`:

  (`integer`) random generator state before starting the simulation.

## Note

Typically, end users will not use the `.DefaultComboSimulations()`
function.
