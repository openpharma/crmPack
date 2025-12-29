# Updating `Data` Objects

**\[stable\]**

A method that updates existing
[`Data`](https://openpharma.github.io/crmPack/reference/Data-class.md)
object with new data.

## Usage

``` r
# S4 method for class 'Data'
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

  (`Data`)  
  object you want to update.

- x:

  (`number`)  
  the dose level (one level only!).

- y:

  (`integer`)  
  the DLT vector (0/1 vector) for all patients in this cohort. You can
  also supply `numeric` vectors, but these will then be converted to
  `integer` internally.

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
  details below.

- ...:

  not used.

## Value

The new, updated
[`Data`](https://openpharma.github.io/crmPack/reference/Data-class.md)
object.

## Details

The current implementation of this `update` method allows for updating
the `Data` class object by adding a single dose level `x` only. However,
there might be some use cases where the new cohort to be added contains
a placebo and active dose. Hence, such update would need to be performed
iteratively by calling the `update` method twice. For example, in the
first call a user can add a placebo, and then in the second call, an
active dose. Since having a cohort with placebo only is not allowed, the
`update` method would normally throw the error when attempting to add a
placebo in the first call. To allow for such updates, the `check`
parameter should be then set to `FALSE` for that first call.

## Examples

``` r
# Create some data of class 'Data'.
my_data <- Data(
  x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
  y = c(0, 0, 0, 0, 0, 0, 1, 0),
  doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2))
)
#> Used default patient IDs!
#> Used best guess cohort indices!

# Update the data with a new cohort.
my_data1 <- update(my_data, x = 20, y = c(0L, 1L, 1L))
my_data1
#> An object of class "Data"
#> Slot "x":
#>  [1]  0.1  0.5  1.5  3.0  6.0 10.0 10.0 10.0 20.0 20.0 20.0
#> 
#> Slot "y":
#>  [1] 0 0 0 0 0 0 1 0 0 1 1
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
#>  [1]  1  2  3  4  5  6  6  6 11 11 11
#> 
#> Slot "placebo":
#> [1] FALSE
#> 
#> Slot "ID":
#>  [1]  1  2  3  4  5  6  7  8  9 10 11
#> 
#> Slot "cohort":
#>  [1] 1 2 3 4 5 6 6 6 7 7 7
#> 
#> Slot "nObs":
#> [1] 11
#> 
```
