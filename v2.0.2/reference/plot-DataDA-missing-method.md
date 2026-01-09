# Plot Method for the [`DataDA`](https://openpharma.github.io/crmPack/reference/DataDA-class.md) Class

**\[stable\]**

A method that creates a plot for
[`DataDA`](https://openpharma.github.io/crmPack/reference/DataDA-class.md)
object.

## Usage

``` r
# S4 method for class 'DataDA,missing'
plot(x, y, blind = FALSE, ...)
```

## Arguments

- x:

  (`DataDA`)  
  object we want to plot.

- y:

  (`missing`)  
  missing object, for compatibility with the generic function.

- blind:

  (`flag`)  
  indicates whether to blind the data. If `TRUE`, then placebo subjects
  are reported at the same level as the active dose level in the
  corresponding cohort, and DLTs are always assigned to the first
  subjects in a cohort.

- ...:

  passed to the first inherited method `plot` after this current method.

## Value

The
[`ggplot2::ggplot2`](https://ggplot2.tidyverse.org/reference/ggplot2-package.html)
object.

## Examples

``` r
# Create some data of class 'DataDA'.
my_data <- DataDA(
  u = c(42, 30, 15, 5, 20, 25, 30, 60),
  t0 = c(0, 15, 30, 40, 55, 70, 75, 85),
  Tmax = 60,
  x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
  y = c(0, 0, 1, 1, 0, 0, 1, 0),
  doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2))
)
#> Used default patient IDs!
#> Used best guess cohort indices!

# Plot the data.
plot(my_data)
```
