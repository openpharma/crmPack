# `DataCombo`

**\[experimental\]**

`DataCombo` is a class for two-drug combination toxicity data. It
inherits from
[`GeneralData`](https://docs.crmpack.org/reference/GeneralData-class.md)
and stores patient-level dose combinations.

## Usage

``` r
DataCombo(
  x = matrix(numeric(), ncol = 2L),
  y = integer(),
  ID = integer(),
  cohort = integer(),
  doseGrid = list(drug1 = numeric(), drug2 = numeric()),
  drugNames = names(doseGrid),
  backfilled = rep(FALSE, nrow(x)),
  response = rep(NA_integer_, nrow(x)),
  ...
)

.DefaultDataCombo()
```

## Arguments

- x:

  (`matrix`) numeric matrix with 2 columns giving the doses of the two
  drugs.

- y:

  (`integer`) the vector of toxicity events (0 or 1).

- ID:

  (`integer`) unique patient IDs.

- cohort:

  (`integer`) the cohort (non-negative sorted) indices.

- doseGrid:

  (`list`) named list of length 2 with all possible doses for each drug.

- drugNames:

  (`character`) names of the two drugs.

- backfilled:

  (`logical`) whether each patient was in a backfill cohort.

- response:

  (`integer`) whether each patient had a positive efficacy response (1 =
  yes, 0 = no). May contain `NA`.

- ...:

  not used.

## Slots

- `x`:

  (`matrix`) numeric matrix with 2 columns giving the doses of the two
  drugs.

- `y`:

  (`integer`) the vector of toxicity events (0 or 1 integers).

- `doseGrid`:

  (`list`) named list of length 2 with the possible doses for each drug.

- `nGrid`:

  (`integer`) number of grid points for each drug.

- `xLevel`:

  (`matrix`) integer matrix with 2 columns giving the dose levels of `x`
  with respect to `doseGrid`.

- `drugNames`:

  (`character`) names of the two drugs.

- `backfilled`:

  (`logical`) whether this patient was in a backfill cohort.

- `response`:

  (`integer`) whether this patient had a positive efficacy response
  (0/1/`NA`).

## Note

`ID` and `cohort` can be missing. Then a message will be issued and the
variables will be filled with default IDs and best guess cohort indices.

Typically, end users will not use the `.DefaultDataCombo()` function.

## Examples

``` r
## Example for DataCombo ----

DataCombo(
  x = cbind(
    drug1 = c(10, 10, 10, 20, 20, 20),
    drug2 = c(20, 20, 20, 20, 20, 20)
  ),
  y = c(0L, 0L, 1L, 0L, 0L, 0L),
  ID = 1L:6L,
  cohort = c(1L, 1L, 1L, 2L, 2L, 2L),
  doseGrid = list(
    drug1 = c(10, 20, 30),
    drug2 = c(20, 40)
  )
)
#> An object of class "DataCombo"
#> Slot "x":
#>      drug1 drug2
#> [1,]    10    20
#> [2,]    10    20
#> [3,]    10    20
#> [4,]    20    20
#> [5,]    20    20
#> [6,]    20    20
#> 
#> Slot "y":
#> [1] 0 0 1 0 0 0
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
#>      drug1 drug2
#> [1,]     1     1
#> [2,]     1     1
#> [3,]     1     1
#> [4,]     2     1
#> [5,]     2     1
#> [6,]     2     1
#> 
#> Slot "drugNames":
#> [1] "drug1" "drug2"
#> 
#> Slot "backfilled":
#> [1] FALSE FALSE FALSE FALSE FALSE FALSE
#> 
#> Slot "response":
#> [1] NA NA NA NA NA NA
#> 
#> Slot "ID":
#> [1] 1 2 3 4 5 6
#> 
#> Slot "cohort":
#> [1] 1 1 1 2 2 2
#> 
#> Slot "nObs":
#> [1] 6
#> 
```
