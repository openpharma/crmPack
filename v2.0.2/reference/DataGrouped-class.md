# `DataGrouped`

**\[stable\]**

`DataGrouped` is a class for a two groups dose escalation data set,
comprised of a monotherapy (`mono`) and a combination therapy (`combo`)
arm. It inherits from
[`Data`](https://openpharma.github.io/crmPack/reference/Data-class.md)
and it contains the additional group information.

## Usage

``` r
DataGrouped(group = character(), ...)

.DefaultDataGrouped()
```

## Arguments

- group:

  (`factor` or `character`)  
  whether `mono` or `combo` was used. If `character` then will be
  coerced to `factor` with the correct levels internally.

- ...:

  parameters passed to
  [`Data()`](https://openpharma.github.io/crmPack/reference/Data-class.md).

## Slots

- `group`:

  (`factor`)  
  whether `mono` or `combo` was used.

## Note

Typically, end users will not use the `.DefaultDataGrouped()` function.

## Examples

``` r
my_data <- DataGrouped(
  x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
  y = c(0, 0, 1, 1, 0, 0, 1, 0),
  doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2)),
  group = c("mono", "mono", "mono", "mono", "mono", "mono", "combo", "combo")
)
#> Used default patient IDs!
#> Used best guess cohort indices!

# Set up an empty data set.
empty_data <- DataGrouped(
  doseGrid = c(0.1, 0.5, 1, 1.5, 3, 6, seq(from = 10, to = 80, by = 2))
)
empty_data
#> An object of class "DataGrouped"
#> Slot "group":
#> factor()
#> Levels: mono combo
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
