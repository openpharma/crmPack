# `DataDA`

**\[stable\]**

`DataDA` is a class for the time-to-DLT augmented data. It inherits from
[`Data`](https://openpharma.github.io/crmPack/reference/Data-class.md)
and it contains additional DLT free survival times.

## Usage

``` r
DataDA(
  u = numeric(),
  t0 = numeric(length(u)),
  Tmax = 0 + .Machine$double.xmin,
  ...
)

.DefaultDataDA()
```

## Arguments

- u:

  (`numeric`)  
  the continuous vector of DLT free survival times.

- t0:

  (`numeric`)  
  time of initial dosing for each patient. Non-negative values sorted in
  ascending order. Default to vector of 0s of length equal to length of
  `u`.

- Tmax:

  (`number`)  
  the DLT observation period.

- ...:

  parameters passed to
  [`Data()`](https://openpharma.github.io/crmPack/reference/Data-class.md).

## Slots

- `u`:

  (`numeric`)  
  the continuous vector of DLT free survival times.

- `t0`:

  (`numeric`)  
  time of initial dosing for each patient. Non-negative values sorted in
  ascending order.

- `Tmax`:

  (`number`)  
  the DLT observation period.

## Note

`survival time` here refers to the time period for which the subject did
not experience any DLT, and is not referring to deaths.

Typically, end users will not use the `.DefaultDataDA()` function.

## Examples

``` r
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

# Set up an empty data set.
empty_data <- DataDA(
  doseGrid = c(0.1, 0.5, 1, 1.5, 3, 6, seq(from = 10, to = 80, by = 2)),
  Tmax = 60
)
empty_data
#> An object of class "DataDA"
#> Slot "u":
#> numeric(0)
#> 
#> Slot "t0":
#> numeric(0)
#> 
#> Slot "Tmax":
#> [1] 60
#> 
#> Slot "x":
#> numeric(0)
#> 
#> Slot "y":
#> integer(0)
#> 
#> Slot "doseGrid":
#>  [1]  0.1  0.5  1.0  1.5  3.0  6.0 10.0 12.0 14.0 16.0 18.0 20.0 22.0 24.0 26.0
#> [16] 28.0 30.0 32.0 34.0 36.0 38.0 40.0 42.0 44.0 46.0 48.0 50.0 52.0 54.0 56.0
#> [31] 58.0 60.0 62.0 64.0 66.0 68.0 70.0 72.0 74.0 76.0 78.0 80.0
#> 
#> Slot "nGrid":
#> [1] 42
#> 
#> Slot "xLevel":
#> integer(0)
#> 
#> Slot "placebo":
#> [1] FALSE
#> 
#> Slot "ID":
#> integer(0)
#> 
#> Slot "cohort":
#> integer(0)
#> 
#> Slot "nObs":
#> [1] 0
#> 
```
