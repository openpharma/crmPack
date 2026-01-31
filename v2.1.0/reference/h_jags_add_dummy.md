# Appending a Dummy Number for Selected Slots in Data

**\[experimental\]**

A helper function that appends a dummy value to a given slots in
[`GeneralData`](https://openpharma.github.io/crmPack/reference/GeneralData-class.md)
class object, if and only if the total number of observations (as
indicated by `object@nObs`) equals to `1`. Otherwise, the `object` is
not changed.

## Usage

``` r
h_jags_add_dummy(object, where, dummy = 0)
```

## Arguments

- object:

  (`GeneralData`)  
  object into which dummy values will be added.

- where:

  (`character`)  
  names of slots in `object` to which a `dummy` number will be appended.

- dummy:

  (`number`)  
  a dummy number that will be appended to selected slots in `object`.
  Default to `0`.

## Value

A
[`GeneralData`](https://openpharma.github.io/crmPack/reference/GeneralData-class.md)
object with slots updated with dummy number.

## Note

The main motivation behind this function is related to the `JAGS`. If
there is only one observation, the data is not passed correctly to
`JAGS`, i.e. e.g. `x` and `y` are treated like scalars in the data file.
Therefore it is necessary to add dummy values to the vectors in this
case As we don't change the number of observations (`nObs`), this
addition of zeros doesn't affect the results of `JAGS` computations.

## Examples

``` r
# Create some data of class 'Data'
my_data <- Data(
  x = 0.1,
  y = 0,
  doseGrid = c(0.1, 0.5)
)
#> Used default patient IDs!
#> Used best guess cohort indices!

my_data_2 <- Data(
  x = c(0.1, 0.5),
  y = c(0, 1),
  doseGrid = c(0.1, 0.5)
)
#> Used default patient IDs!
#> Used best guess cohort indices!

# Append dummy to `x` and `y`.
h_jags_add_dummy(my_data, where = c("x", "y"))
#> An object of class "Data"
#> Slot "x":
#> [1] 0.1 0.0
#> 
#> Slot "y":
#> [1] 0 0
#> 
#> Slot "doseGrid":
#> [1] 0.1 0.5
#> 
#> Slot "nGrid":
#> [1] 2
#> 
#> Slot "xLevel":
#> [1] 1
#> 
#> Slot "placebo":
#> [1] FALSE
#> 
#> Slot "backfilled":
#> [1] FALSE
#> 
#> Slot "response":
#> [1] NA
#> 
#> Slot "ID":
#> [1] 1
#> 
#> Slot "cohort":
#> [1] 1
#> 
#> Slot "nObs":
#> [1] 1
#> 

# Append dummy to `x` and `y`. No effect as `my_data_2@nObs != 1`.
h_jags_add_dummy(my_data_2, where = c("x", "y"))
#> An object of class "Data"
#> Slot "x":
#> [1] 0.1 0.5
#> 
#> Slot "y":
#> [1] 0 1
#> 
#> Slot "doseGrid":
#> [1] 0.1 0.5
#> 
#> Slot "nGrid":
#> [1] 2
#> 
#> Slot "xLevel":
#> [1] 1 2
#> 
#> Slot "placebo":
#> [1] FALSE
#> 
#> Slot "backfilled":
#> [1] FALSE FALSE
#> 
#> Slot "response":
#> [1] NA NA
#> 
#> Slot "ID":
#> [1] 1 2
#> 
#> Slot "cohort":
#> [1] 1 2
#> 
#> Slot "nObs":
#> [1] 2
#> 
```
