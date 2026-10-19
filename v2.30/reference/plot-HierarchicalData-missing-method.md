# Plot Method for the [`HierarchicalData`](https://docs.crmpack.org/reference/HierarchicalData-class.md) Class

**\[experimental\]**

A method that creates a combined plot for a
[`HierarchicalData`](https://docs.crmpack.org/reference/HierarchicalData-class.md)
object by arranging the arm-specific
[`Data`](https://docs.crmpack.org/reference/Data-class.md) and
[`DataCombo`](https://docs.crmpack.org/reference/DataCombo-class.md)
plots.

## Usage

``` r
# S4 method for class 'HierarchicalData,missing'
plot(x, y, ncol = NULL, ...)
```

## Arguments

- x:

  (`HierarchicalData`)\
  object we want to plot.

- y:

  (`missing`)\
  missing object, for compatibility with the generic function.

- ncol:

  (`count` or `NULL`)\
  number of columns in the combined plot. If `NULL`, a compact layout is
  chosen automatically.

- ...:

  passed to the arm-specific `plot` methods.

## Value

A `gtable` object combining the arm-specific plots, or `NULL` if no arm
plot is available.
