# `DataOrdinal`

**\[experimental\]**

`DataOrdinal` is a class for ordinal toxicity data. It inherits from
[`GeneralData`](https://openpharma.github.io/crmPack/reference/GeneralData-class.md)
and it describes toxicity responses on an ordinal rather than binary
scale.

## Usage

``` r
DataOrdinal(
  x = numeric(),
  y = integer(),
  ID = integer(),
  cohort = integer(),
  doseGrid = numeric(),
  placebo = FALSE,
  yCategories = c(`No DLT` = 0L, DLT = 1L),
  ...
)

.DefaultDataOrdinal()
```

## Arguments

- x:

  (`numeric`)  
  the doses for the patients.

- y:

  (`integer`)  
  the vector of toxicity events (0 or 1). You can also supply `numeric`
  vectors, but these will then be converted to `integer` internally.

- ID:

  (`integer`)  
  unique patient IDs. You can also supply `numeric` vectors, but these
  will then be converted to `integer` internally.

- cohort:

  (`integer`)  
  the cohort (non-negative sorted) indices. You can also supply
  `numeric` vectors, but these will then be converted to `integer`
  internally.

- doseGrid:

  (`numeric`)  
  all possible doses.

- placebo:

  (`flag`)  
  if `TRUE` the first dose level in the `doseGrid` is considered as
  placebo.

- yCategories:

  (named `integer`)  
  the names and codes for the toxicity categories used in the data.
  Category labels are taken from the names of the vector. The names of
  the vector must be unique and its values must be sorted and take the
  values 0, 1, 2, ...

- ...:

  not used.

## Details

The `cohort` can be missing if and only if `placebo` is equal to
`FALSE`.

## Note

This class has been implemented as a sibling of the existing `Data`
class (rather than as a parent or child) to minimise the risk of
unintended side effects on existing classes and methods.

The default setting for the `yCategories` slot replicates the behaviour
of the existing `Data` class.

Typically, end users will not use the `.DefaultDataOrdinal()` function.

## Examples

``` r
DataOrdinal(
  x = c(10, 20, 30, 40, 50, 50, 50, 60, 60, 60),
  y = as.integer(c(0, 0, 0, 0, 0, 1, 0, 0, 1, 2)),
  ID = 1L:10L,
  cohort = as.integer(c(1:4, 5, 5, 5, 6, 6, 6)),
  doseGrid = c(seq(from = 10, to = 100, by = 10)),
  yCategories = c("No tox" = 0L, "Sub-tox AE" = 1L, "DLT" = 2L),
  placebo = FALSE
)
#> An object of class "DataOrdinal"
#> Slot "x":
#>  [1] 10 20 30 40 50 50 50 60 60 60
#> 
#> Slot "y":
#>  [1] 0 0 0 0 0 1 0 0 1 2
#> 
#> Slot "doseGrid":
#>  [1]  10  20  30  40  50  60  70  80  90 100
#> 
#> Slot "nGrid":
#> [1] 10
#> 
#> Slot "xLevel":
#>  [1] 1 2 3 4 5 5 5 6 6 6
#> 
#> Slot "yCategories":
#>     No tox Sub-tox AE        DLT 
#>          0          1          2 
#> 
#> Slot "placebo":
#> [1] FALSE
#> 
#> Slot "ID":
#>  [1]  1  2  3  4  5  6  7  8  9 10
#> 
#> Slot "cohort":
#>  [1] 1 2 3 4 5 5 5 6 6 6
#> 
#> Slot "nObs":
#> [1] 10
#> 
```
