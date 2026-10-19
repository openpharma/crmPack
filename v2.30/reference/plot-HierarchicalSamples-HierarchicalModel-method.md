# Plotting hierarchical dose-toxicity model fits

Plotting hierarchical dose-toxicity model fits

## Usage

``` r
# S4 method for class 'HierarchicalSamples,HierarchicalModel'
plot(x, y, data, ncol = NULL, ...)
```

## Arguments

- x:

  the
  [HierarchicalSamples](https://docs.crmpack.org/reference/HierarchicalSamples-class.md)
  object.

- y:

  the
  [HierarchicalModel](https://docs.crmpack.org/reference/HierarchicalModel-class.md)
  object.

- data:

  the
  [HierarchicalData](https://docs.crmpack.org/reference/HierarchicalData-class.md)
  object.

- ncol:

  (`count` or `NULL`)\
  number of columns in the combined plot. If `NULL`, a compact layout is
  chosen automatically.

- ...:

  passed to the arm-specific `plot` methods.

## Value

This returns a `gtable` object combining the arm-specific fitted plots,
or `NULL` if no arm plot is available.
