# Updating `DataParts` Objects

**\[stable\]**

A method that updates existing
[`DataParts`](https://openpharma.github.io/crmPack/reference/DataParts-class.md)
object with new data.

## Usage

``` r
# S4 method for class 'DataParts'
update(object, x, y, ..., check = TRUE)
```

## Arguments

- object:

  (`DataParts`)  
  object you want to update.

- x:

  (`number`)  
  the dose level (one level only!).

- y:

  (`integer`)  
  the DLT vector (0/1 vector) for all patients in this cohort. You can
  also supply `numeric` vectors, but these will then be converted to
  `integer` internally.

- ...:

  further arguments passed to `Data` update method
  [`update-Data`](https://openpharma.github.io/crmPack/reference/update-Data-method.md).

- check:

  (`flag`)  
  whether the validation of the updated object should be conducted. See
  help for
  [`update-Data`](https://openpharma.github.io/crmPack/reference/update-Data-method.md)
  for more details on the use case of this parameter.

## Value

The new, updated
[`DataParts`](https://openpharma.github.io/crmPack/reference/DataParts-class.md)
object.

## Examples

``` r
# Create an object of class 'DataParts'.
my_data <- DataParts(
  x = c(0.1, 0.5, 1.5),
  y = c(0, 0, 0),
  doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2)),
  part = c(1L, 1L, 1L),
  nextPart = 1L,
  part1Ladder = c(0.1, 0.5, 1.5, 3, 6, 10)
)
#> Used default patient IDs!
#> Used best guess cohort indices!

# Update the data with a new cohort.
# Note that since we reached the last level from 'part1Ladder'
# then the 'nextPart' is switched from '1' to '2'.
my_data1 <- update(my_data, x = 10, y = 0L)
my_data1
#> An object of class "DataParts"
#> Slot "part":
#> [1] 1 1 1 1
#> 
#> Slot "nextPart":
#> [1] 2
#> 
#> Slot "part1Ladder":
#> [1]  0.1  0.5  1.5  3.0  6.0 10.0
#> 
#> Slot "x":
#> [1]  0.1  0.5  1.5 10.0
#> 
#> Slot "y":
#> [1] 0 0 0 0
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
#> [1] 1 2 3 6
#> 
#> Slot "placebo":
#> [1] FALSE
#> 
#> Slot "ID":
#> [1] 1 2 3 4
#> 
#> Slot "cohort":
#> [1] 1 2 3 4
#> 
#> Slot "nObs":
#> [1] 4
#> 
```
