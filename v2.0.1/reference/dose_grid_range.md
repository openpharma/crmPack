# Getting the Dose Grid Range

**\[stable\]**

A function that returns a vector of length two with the minimum and
maximum dose in a grid. It returns `c(-Inf, Inf)` if the range cannot be
determined, which happens when the dose grid is empty. User can choose
whether the placebo dose (if any) should be counted or not.

**\[experimental\]**

## Usage

``` r
dose_grid_range(object, ...)

# S4 method for class 'Data'
dose_grid_range(object, ignore_placebo = TRUE)

# S4 method for class 'DataOrdinal'
dose_grid_range(object, ignore_placebo = TRUE)
```

## Arguments

- object:

  (`Data`)  
  object with dose grid.

- ...:

  further arguments passed to class-specific methods.

- ignore_placebo:

  (`flag`)  
  should placebo dose (if any) not be counted?

## Value

A `numeric` vector containing the minimum and maximum of all the doses
in a grid or `c(-Inf, Inf)`.

## Examples

``` r
my_data <- Data(
  x = c(10, 50, 90, 100, 0.001, 20, 30, 30),
  y = c(0, 0, 0, 0, 0, 0, 1, 0),
  ID = 1:8,
  cohort = c(1L, 2L, 3L, 4L, 5L, 5L, 6L, 6L),
  doseGrid = c(0.001, seq(from = 10, to = 100, by = 10)),
  placebo = TRUE
)
dose_grid_range(my_data)
#> [1]  10 100
dose_grid_range(my_data, ignore_placebo = FALSE)
#> [1] 1e-03 1e+02
data <- DataOrdinal(
  x = c(10, 20, 30, 40, 50, 50, 50, 60, 60, 60),
  y = as.integer(c(0, 0, 0, 0, 0, 1, 0, 0, 1, 2)),
  ID = 1L:10L,
  cohort = as.integer(c(1:4, 5, 5, 5, 6, 6, 6)),
  doseGrid = c(seq(from = 10, to = 100, by = 10)),
  yCategories = c("No tox" = 0L, "Sub-tox AE" = 1L, "DLT" = 2L),
  placebo = FALSE
)

dose_grid_range(data)
#> [1]  10 100
```
