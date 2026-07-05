# Plot Method for the [`DataCombo`](https://docs.crmpack.org/reference/DataCombo-class.md) Class

**\[experimental\]**

A method that creates a plot for
[`DataCombo`](https://docs.crmpack.org/reference/DataCombo-class.md)
object.

## Usage

``` r
# S4 method for class 'DataCombo,missing'
plot(x, y, legend = TRUE, ...)
```

## Arguments

- x:

  (`DataCombo`)

- y:

  (`missing`)

- legend:

  (`flag`) whether legends should be displayed.

- ...:

  not used.

## Value

A `gtable` object combining two
[`ggplot2`](https://ggplot2.tidyverse.org/reference/ggplot2-package.html)
plots.

## Examples

``` r
data <- .DefaultDataCombo()

plot(data)
```
