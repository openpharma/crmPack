# `DataDual`

**\[stable\]**

`DataDual` is a class for the dual endpoint data. It inherits from
[`Data`](https://openpharma.github.io/crmPack/reference/Data-class.md)
and it contains additional biomarker information.

## Usage

``` r
DataDual(w = numeric(), ...)

.DefaultDataDual()
```

## Arguments

- w:

  (`numeric`)  
  the continuous vector of biomarker values.

- ...:

  parameters passed to
  [`Data()`](https://openpharma.github.io/crmPack/reference/Data-class.md).

## Slots

- `w`:

  (`numeric`)  
  the continuous vector of biomarker values.

## Note

Typically, end users will not use the `.DefaultDataDual()` function.

## Examples

``` r
my_data <- DataDual(
  w = rnorm(8),
  x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
  y = c(0, 0, 0, 0, 0, 0, 1, 0),
  doseGrid = c(
    0.1,
    0.5,
    1.5,
    3,
    6,
    seq(from = 10, to = 80, by = 2)
  )
)
#> Used default patient IDs!
#> Used best guess cohort indices!
my_data
#> An object of class "DataDual"
#> Slot "w":
#> [1]  2.06502490 -1.63098940  0.51242695 -1.86301149 -0.52201251 -0.05260191
#> [7]  0.54299634 -0.91407483
#> 
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
