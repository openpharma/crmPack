# Helper Function for the Plot Method of the Data and DataOrdinal Classes

**\[stable\]**

A method that creates a plot for
[`Data`](https://openpharma.github.io/crmPack/reference/Data-class.md)
and
[`DataOrdinal`](https://openpharma.github.io/crmPack/reference/DataOrdinal-class.md)
objects.

**\[stable\]**

A method that creates a plot for
[`Data`](https://openpharma.github.io/crmPack/reference/Data-class.md)
object.

**\[experimental\]**

A method that creates a plot for
[`DataOrdinal`](https://openpharma.github.io/crmPack/reference/DataOrdinal-class.md)
object.

## Usage

``` r
h_plot_data_dataordinal(
  x,
  blind = FALSE,
  legend = TRUE,
  tox_labels = c(Yes = "red", No = "black"),
  tox_shapes = c(Yes = 17L, No = 16L),
  mark_backfill = FALSE,
  ...
)

# S4 method for class 'Data,missing'
plot(
  x,
  y,
  blind = FALSE,
  legend = TRUE,
  include_backfill = TRUE,
  mark_backfill = FALSE,
  mark_response = FALSE,
  ...,
  text_size_markings = 9,
  point_cex_markings = 1.1
)

# S4 method for class 'DataOrdinal,missing'
plot(
  x,
  y,
  blind = FALSE,
  legend = TRUE,
  tox_labels = NULL,
  tox_shapes = NULL,
  ...
)
```

## Arguments

- x:

  (`DataOrdinal`)  
  object we want to plot.

- blind:

  (`flag`)  
  indicates whether to blind the data. If `TRUE`, then placebo subjects
  are reported at the same level as the active dose level in the
  corresponding cohort, and DLTs are always assigned to the first
  subjects in a cohort.

- legend:

  (`flag`)  
  whether the legend should be added.

- tox_labels:

  (`named list of character`)  
  the labels of the toxicity categories.

- tox_shapes:

  (`names list of integers`)  
  the symbols used to identify the toxicity categories.

- mark_backfill:

  (`flag`)  
  whether to mark backfilled patients with empty shape.

- ...:

  not used.

- y:

  (`missing`)  
  missing object, for compatibility with the generic function.

- include_backfill:

  (`flag`)  
  whether to include backfilled patients.

- mark_response:

  (`flag`)  
  whether to mark patients with response with rectangle.

- text_size_markings:

  (`number`)  
  text size for the markings legend.

- point_cex_markings:

  (`number`)  
  point size multiplier for the markings legend.

## Value

The
[`ggplot2::ggplot2`](https://ggplot2.tidyverse.org/reference/ggplot2-package.html)
object.

The
[`ggplot2::ggplot2`](https://ggplot2.tidyverse.org/reference/ggplot2-package.html)
object.

The
[`ggplot2::ggplot2`](https://ggplot2.tidyverse.org/reference/ggplot2-package.html)
object.

## Note

The default values of `tox_shapes` and `tox_labels` result in DLTs being
displayed as red triangles and other responses as black circles.

With more than 9 toxicity categories, toxicity symbols must be specified
manually.  
With more than 5 toxicity categories, toxicity labels must be specified
manually.

## Examples

``` r
# Create some data of class 'Data'.
my_data <- Data(
  x = c(0.001, 0.1, 0.1, 0.5, 0.001, 3, 3, 0.001, 10, 10, 10),
  y = c(0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0),
  cohort = c(1, 1, 1, 2, 3, 3, 3, 4, 4, 4, 4),
  doseGrid = c(0.001, 0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2)),
  placeb = TRUE
)
#> Used default patient IDs!

# Plot the data.
plot(my_data)


# We can also mark backfill patients and response outcomes.
my_data@backfilled <- c(
  FALSE,
  FALSE,
  FALSE,
  FALSE,
  TRUE,
  TRUE,
  TRUE,
  FALSE,
  FALSE,
  FALSE,
  FALSE
)
my_data@response <- as.integer(c(
  NA,
  NA,
  1,
  NA,
  NA,
  0,
  1,
  NA,
  1,
  0,
  0
))
plot(
  my_data,
  mark_backfill = TRUE,
  mark_response = TRUE
)

data <- DataOrdinal(
  x = c(10, 20, 30, 40, 50, 50, 50, 60, 60, 60),
  y = as.integer(c(0, 0, 0, 0, 0, 1, 0, 0, 1, 2)),
  ID = 1L:10L,
  cohort = as.integer(c(1:4, 5, 5, 5, 6, 6, 6)),
  doseGrid = c(seq(from = 10, to = 100, by = 10)),
  yCategories = c("No tox" = 0L, "Sub-tox AE" = 1L, "DLT" = 2L),
  placebo = FALSE
)

plot(data)
```
