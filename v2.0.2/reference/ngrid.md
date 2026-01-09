# Number of Doses in Grid

**\[stable\]**

A function that gets the number of doses in grid. User can choose
whether the placebo dose (if any) should be counted or not.

## Usage

``` r
ngrid(object, ignore_placebo = TRUE, ...)

# S4 method for class 'Data'
ngrid(object, ignore_placebo = TRUE, ...)
```

## Arguments

- object:

  (`Data`)  
  object with dose grid.

- ignore_placebo:

  (`flag`)  
  should placebo dose (if any) not be counted?

- ...:

  further arguments passed to class-specific methods.

## Value

`integer` the number of doses in grid.

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
ngrid(my_data)
#> [1] 10
ngrid(my_data, ignore_placebo = FALSE)
#> [1] 11
```
