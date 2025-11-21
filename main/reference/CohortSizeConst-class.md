# `CohortSizeConst`

**\[stable\]**

`CohortSizeConst` is the class for fixed and constant size of cohort.

## Usage

``` r
CohortSizeConst(size)

.DefaultCohortSizeConst()
```

## Arguments

- size:

  (`number`)  
  see slot definition.

## Slots

- `size`:

  (`integer`)  
  cohort size.

## Note

Typically, end users will not use the `.DefaultCohortSizeConst()`
function.

## Examples

``` r
# Cohort of size 3, constant along the study.
my_size <- CohortSizeConst(size = 3)
```
