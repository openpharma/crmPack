# `Data`

**\[stable\]**

`Data` is a class for the data input. It inherits from
[`GeneralData`](https://openpharma.github.io/crmPack/reference/GeneralData-class.md).

## Usage

``` r
Data(
  x = numeric(),
  y = integer(),
  ID = integer(),
  cohort = integer(),
  doseGrid = numeric(),
  placebo = FALSE,
  ...
)

.DefaultData()
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

- ...:

  not used.

## Details

The `cohort` can be missing if and only if `placebo` is equal to
`FALSE`.

## Slots

- `x`:

  (`numeric`)  
  the doses for the patients.

- `y`:

  (`integer`)  
  the vector of toxicity events (0 or 1 integers).

- `doseGrid`:

  (`numeric`)  
  the vector of all possible doses (sorted), i.e. the dose grid.

- `nGrid`:

  (`integer`)  
  number of gridpoints.

- `xLevel`:

  (`integer`)  
  the levels for the doses the patients have been given, w.r.t
  `doseGrid`.

- `placebo`:

  (`logical`)  
  if `TRUE` the first dose level in the `doseGrid`is considered as
  PLACEBO.

## Note

`ID` and `cohort` can be missing. Then a message will be issued and the
variables will be filled with default IDs and best guesses cohort, i.e.
a sorted (in ascending order) sequence of values from `{1, 2, ...}`.

Typically, end users will not use the `.DefaultData()` function.

## Examples

``` r
my_data <- Data(
  x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
  y = c(0, 0, 0, 0, 0, 0, 1, 0),
  ID = as.integer(1:8),
  cohort = as.integer(c(1, 2, 3, 4, 5, 6, 6, 6)),
  doseGrid = c(
    0.1,
    0.5,
    1.5,
    3,
    6,
    seq(from = 10, to = 80, by = 2)
  )
)
my_data
#> An object of class "Data"
#> Slot "x":
#> [1]  0.1  0.5  1.5  3.0  6.0 10.0 10.0 10.0
#> 
#> Slot "y":
#> [1] 0 0 0 0 0 0 1 0
#> 
#> Slot "doseGrid":
#>  [1]  0.1  0.5  1.5  3.0  6.0 10.0 12.0 14.0 16.0 18.0 20.0 22.0 24.0 26.0 28.0
#> [16] 30.0 32.0 34.0 36.0 38.0 40.0 42.0 44.0 46.0 48.0 50.0 52.0 54.0 56.0 58.0
#> [31] 60.0 62.0 64.0 66.0 68.0 70.0 72.0 74.0 76.0 78.0 80.0
#> 
#> Slot "nGrid":
#> [1] 41
#> 
#> Slot "xLevel":
#> [1] 1 2 3 4 5 6 6 6
#> 
#> Slot "placebo":
#> [1] FALSE
#> 
#> Slot "ID":
#> [1] 1 2 3 4 5 6 7 8
#> 
#> Slot "cohort":
#> [1] 1 2 3 4 5 6 6 6
#> 
#> Slot "nObs":
#> [1] 8
#> 
```
