# Updating `DataCombo` Objects

**\[experimental\]**

A method that updates existing
[`DataCombo`](https://docs.crmpack.org/reference/DataCombo-class.md)
object with new data.

## Usage

``` r
# S4 method for class 'DataCombo'
update(
  object,
  x,
  y,
  response = rep(NA_integer_, length(y)),
  ID = length(object@ID) + seq_along(y),
  new_cohort = TRUE,
  check = TRUE,
  backfill = FALSE,
  cohort = NULL,
  ...
)
```

## Arguments

- object:

  (`DataCombo`)

- x:

  (`numeric`) named dose combination with one entry for each drug. Names
  must match `object@drugNames`.

- y:

  (`integer`) the DLT vector (0/1 vector) for all patients in this
  cohort.

- response:

  (`integer`) the efficacy response vector (0/1 vector). May contain
  `NA`.

- ID:

  (`integer`) the patient IDs.

- new_cohort:

  (`flag`) if `TRUE` (default) the new data are assigned to a new
  cohort.

- check:

  (`flag`) whether the validation of the updated object should be
  conducted.

- backfill:

  (`flag`) whether the new patients being added are from a backfill
  cohort.

- cohort:

  (`int`) if provided, the new patients will be assigned to this cohort
  index. If `NULL` (default), the cohort index will be determined based
  on the `new_cohort` parameter.

- ...:

  not used.

## Value

The new, updated
[`DataCombo`](https://docs.crmpack.org/reference/DataCombo-class.md)
object.

## Examples

``` r
# Create some data of class 'DataCombo'.
my_data <- DataCombo(
  x = cbind(
    drug1 = c(10, 10, 10, 20, 20, 20),
    drug2 = c(20, 20, 20, 20, 20, 20)
  ),
  y = c(0, 0, 1, 0, 0, 0),
  doseGrid = list(drug1 = c(10, 20, 30), drug2 = c(20, 40))
)
#> Used default patient IDs!
#> Used best guess cohort indices!

# Update the data with a new cohort.
my_data1 <- update(
  my_data,
  x = c(drug1 = 30, drug2 = 40),
  y = c(0L, 1L, 0L)
)
my_data1
#> An object of class "DataCombo"
#> Slot "x":
#>       drug1 drug2
#>  [1,]    10    20
#>  [2,]    10    20
#>  [3,]    10    20
#>  [4,]    20    20
#>  [5,]    20    20
#>  [6,]    20    20
#>  [7,]    30    40
#>  [8,]    30    40
#>  [9,]    30    40
#> 
#> Slot "y":
#> [1] 0 0 1 0 0 0 0 1 0
#> 
#> Slot "doseGrid":
#> $drug1
#> [1] 10 20 30
#> 
#> $drug2
#> [1] 20 40
#> 
#> 
#> Slot "nGrid":
#> [1] 3 2
#> 
#> Slot "xLevel":
#>       drug1 drug2
#>  [1,]     1     1
#>  [2,]     1     1
#>  [3,]     1     1
#>  [4,]     2     1
#>  [5,]     2     1
#>  [6,]     2     1
#>  [7,]     3     2
#>  [8,]     3     2
#>  [9,]     3     2
#> 
#> Slot "drugNames":
#> [1] "drug1" "drug2"
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
#> [1] 1 1 1 2 2 2 3 3 3
#> 
#> Slot "nObs":
#> [1] 9
#> 
```
