# Updating `DataDA` Objects

**\[stable\]**

A method that updates existing
[`DataDA`](https://openpharma.github.io/crmPack/reference/DataDA-class.md)
object with new data.

## Usage

``` r
# S4 method for class 'DataDA'
update(object, u, t0, trialtime, y, ..., check = TRUE)
```

## Arguments

- object:

  (`DataDA`)  
  object you want to update.

- u:

  (`numeric`)  
  the new DLT free survival times for all patients, i.e. for existing
  patients in the `object` as well as for new patients.

- t0:

  (`numeric`)  
  the time that each patient starts DLT observation window. This
  parameter covers all patients, i.e. existing patients in the `object`
  as well as for new patients.

- trialtime:

  (`number`)  
  current time in the trial, i.e. a followup time.

- y:

  (`numeric`)  
  the new DLTs for all patients, i.e. for existing patients in the
  `object` as well as for new patients.

- ...:

  further arguments passed to `Data` update method
  [`update-Data`](https://openpharma.github.io/crmPack/reference/update-Data-method.md).
  These are used when there are new patients to be added to the cohort.

- check:

  (`flag`)  
  whether the validation of the updated object should be conducted. See
  help for
  [`update-Data`](https://openpharma.github.io/crmPack/reference/update-Data-method.md)
  for more details on the use case of this parameter.

## Value

The new, updated
[`DataDA`](https://openpharma.github.io/crmPack/reference/DataDA-class.md)
object.

## Note

This function is capable of not only adding new patients but also
updates existing ones with respect to `y`, `t0`, `u` slots.

## Examples

``` r
# Create an object of class 'DataDA'.
my_data <- DataDA(
  x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
  y = c(0, 0, 1, 1, 0, 0, 1, 0),
  doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2)),
  u = c(42, 30, 15, 5, 20, 25, 30, 60),
  t0 = c(0, 15, 30, 40, 55, 70, 75, 85),
  Tmax = 60
)
#> Used default patient IDs!
#> Used best guess cohort indices!

# Update the data.
my_data1 <- update(
  object = my_data,
  y = c(my_data@y, 0), # The 'y' will be updated according to 'u'.
  u = c(my_data@u, 20),
  t0 = c(my_data@t0, 95),
  x = 20,
  trialtime = 120 # This is the global timeline for a trial.
)
my_data1
#> An object of class "DataDA"
#> Slot "u":
#> [1] 42 30 15  5 20 25 30 35 20
#> 
#> Slot "t0":
#> [1]  0 15 30 40 55 70 75 85 95
#> 
#> Slot "Tmax":
#> [1] 60
#> 
#> Slot "x":
#> [1]  0.1  0.5  1.5  3.0  6.0 10.0 10.0 10.0 20.0
#> 
#> Slot "y":
#> [1] 0 0 1 1 0 0 1 0 0
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
#> [1]  1  2  3  4  5  6  6  6 11
#> 
#> Slot "placebo":
#> [1] FALSE
#> 
#> Slot "backfilled":
#> [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#> 
#> Slot "response":
#> [1] NA NA NA NA NA NA NA NA NA
#> 
#> Slot "ID":
#> [1] 1 2 3 4 5 6 7 8 9
#> 
#> Slot "cohort":
#> [1] 1 2 3 4 5 6 6 6 7
#> 
#> Slot "nObs":
#> [1] 9
#> 
```
