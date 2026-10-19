# `HierarchicalDesign`

**\[experimental\]**

`HierarchicalDesign` stores a collection of named
[`DesignArm`](https://docs.crmpack.org/reference/DesignArm-class.md)
objects and derives the corresponding
[`HierarchicalData`](https://docs.crmpack.org/reference/HierarchicalData-class.md)
and
[`HierarchicalModel`](https://docs.crmpack.org/reference/HierarchicalModel-class.md)
objects from them.

## Usage

``` r
HierarchicalDesign(
  ...,
  exchangeable_parameters = list(),
  pool_correlations = list(),
  pool_priors = list()
)

.DefaultHierarchicalDesign()
```

## Arguments

- ...:

  [`DesignArm`](https://docs.crmpack.org/reference/DesignArm-class.md)
  objects describing the trial arms. These can be created with
  [`DesignArm()`](https://docs.crmpack.org/reference/DesignArm-class.md)
  or
  [`HistoricalArm()`](https://docs.crmpack.org/reference/DesignArm-class.md).

- exchangeable_parameters:

  (`list`)\
  see
  [`HierarchicalModel()`](https://docs.crmpack.org/reference/HierarchicalModel-class.md).

- pool_correlations:

  (`list`)\
  see
  [`HierarchicalModel()`](https://docs.crmpack.org/reference/HierarchicalModel-class.md).

- pool_priors:

  (`list`)\
  see
  [`HierarchicalModel()`](https://docs.crmpack.org/reference/HierarchicalModel-class.md).

## Slots

- `arms`:

  (`list`)\
  a named list of
  [`DesignArm`](https://docs.crmpack.org/reference/DesignArm-class.md)
  objects.

- `data`:

  (`HierarchicalData`)\
  the hierarchical data object assembled from the arm-specific design
  data.

- `model`:

  (`HierarchicalModel`)\
  the hierarchical model object assembled from the arm-specific design
  models.

## Note

Typically, end users will not use the `.DefaultHierarchicalDesign()`
function directly.
