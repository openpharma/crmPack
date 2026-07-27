# Extracting Single-Drug Data from Combination Data

**\[experimental\]**

A method that extracts one drug-specific
[`Data`](https://docs.crmpack.org/reference/Data-class.md) object from a
two-drug
[`DataCombo`](https://docs.crmpack.org/reference/DataCombo-class.md)
object.

## Usage

``` r
singleDrugData(object, drug, ...)

# S4 method for class 'DataCombo'
singleDrugData(object, drug)
```

## Arguments

- object:

  (`DataCombo`) object from which single-drug data are extracted.

- drug:

  (`string`) name of the drug to extract. Must be one of
  `object@drugNames`.

- ...:

  not used.

## Value

A [`Data`](https://docs.crmpack.org/reference/Data-class.md) object
containing dose, dose grid and patient-level outcomes corresponding to
the selected drug.

## Examples

``` r
# Example of extracting single-drug data from two-drug combination data.

my_combo_data <- DataCombo(
  x = cbind(
    drug1 = c(10, 10, 20, 20),
    drug2 = c(20, 20, 20, 40)
  ),
  y = c(0L, 1L, 0L, 0L),
  ID = 1L:4L,
  cohort = c(1L, 2L, 3L, 4L),
  doseGrid = list(
    drug1 = c(10, 20, 30),
    drug2 = c(20, 40)
  )
)

# Extract the data for one specific drug.
singleDrugData(my_combo_data, "drug1")
#> An object of class "Data"
#> Slot "x":
#> [1] 10 10 20 20
#> 
#> Slot "y":
#> [1] 0 1 0 0
#> 
#> Slot "doseGrid":
#> [1] 10 20 30
#> 
#> Slot "nGrid":
#> [1] 3
#> 
#> Slot "xLevel":
#> [1] 1 1 2 2
#> 
#> Slot "placebo":
#> [1] FALSE
#> 
#> Slot "backfilled":
#> [1] FALSE FALSE FALSE FALSE
#> 
#> Slot "response":
#> [1] NA NA NA NA
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
