# Updating `DataOrdinal` Objects

**\[experimental\]**

A method that updates existing
[`DataOrdinal`](https://openpharma.github.io/crmPack/reference/DataOrdinal-class.md)
object with new data.

## Usage

``` r
# S4 method for class 'DataOrdinal'
update(
  object,
  x,
  y,
  ID = length(object@ID) + seq_along(y),
  new_cohort = TRUE,
  check = TRUE,
  ...
)
```

## Arguments

- object:

  (`DataOrdinal`)  
  object you want to update.

- x:

  (`number`)  
  the dose level (one level only!).

- y:

  (`integer`)  
  the vector of toxicity grades (0, 1, 2, ...) for all patients in this
  cohort. You can also supply `numeric` vectors, but these will then be
  converted to `integer` internally.

- ID:

  (`integer`)  
  the patient IDs. You can also supply `numeric` vectors, but these will
  then be converted to `integer` internally.

- new_cohort:

  (`flag`)  
  if `TRUE` (default) the new data are assigned to a new cohort.

- check:

  (`flag`)  
  whether the validation of the updated object should be conducted. See
  Details below.

- ...:

  not used.

## Value

The new, updated
[`DataOrdinal`](https://openpharma.github.io/crmPack/reference/DataOrdinal-class.md)
object.

## Details

The current implementation of this `update` method allows for updating
the `DataOrdinal` class object by adding a single dose level `x` only.
However, there might be some use cases where the new cohort to be added
contains a placebo and active dose. Hence, such update would need to be
performed iteratively by calling the `update` method twice. For example,
in the first call a user can add a placebo, and then in the second call,
an active dose. Since having a cohort with placebo only is not allowed,
the `update` method would normally throw the error when attempting to
add a placebo in the first call. To allow for such updates, the `check`
parameter should be then set to `FALSE` for that first call.

## Examples

``` r
data <- DataOrdinal(
  x = c(10, 20, 30, 40, 50, 50, 50, 60, 60, 60),
  y = as.integer(c(0, 0, 0, 0, 0, 1, 0, 0, 1, 2)),
  ID = 1L:10L,
  cohort = as.integer(c(1:4, 5, 5, 5, 6, 6, 6)),
  doseGrid = c(seq(from = 10, to = 100, by = 10)),
  yCategories = c("No tox" = 0L, "Sub-tox AE" = 1L, "DLT" = 2L),
  placebo = FALSE
)

update(data, x = 70, y = c(1L, 2L, 1L))
#> An object of class "DataOrdinal"
#> Slot "x":
#>  [1] 10 20 30 40 50 50 50 60 60 60 70 70 70
#> 
#> Slot "y":
#>  [1] 0 0 0 0 0 1 0 0 1 2 1 2 1
#> 
#> Slot "doseGrid":
#>  [1]  10  20  30  40  50  60  70  80  90 100
#> 
#> Slot "nGrid":
#> [1] 10
#> 
#> Slot "xLevel":
#>  [1] 1 2 3 4 5 5 5 6 6 6 7 7 7
#> 
#> Slot "yCategories":
#>     No tox Sub-tox AE        DLT 
#>          0          1          2 
#> 
#> Slot "placebo":
#> [1] FALSE
#> 
#> Slot "ID":
#>  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13
#> 
#> Slot "cohort":
#>  [1] 1 2 3 4 5 5 5 6 6 6 7 7 7
#> 
#> Slot "nObs":
#> [1] 13
#> 
```
