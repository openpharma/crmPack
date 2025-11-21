# `CohortSizeRange`

**\[stable\]**

`CohortSizeRange` is the class for cohort size based on dose range.

## Usage

``` r
CohortSizeRange(intervals, cohort_size)

.DefaultCohortSizeRange()
```

## Arguments

- intervals:

  (`numeric`)  
  see slot definition.

- cohort_size:

  (`numeric`)  
  see slot definition.

## Slots

- `intervals`:

  (`numeric`)  
  a vector with the left bounds of the relevant dose intervals.

- `cohort_size`:

  (`integer`)  
  an integer vector with the cohort sizes corresponding to the elements
  of `intervals`.

## Note

Typically, end users will not use the `.DefaultCohortSizeRange()`
function.

## Examples

``` r
# Example for the rule having cohort of size 1 for doses <30
# and having cohort of size 3 for doses >=30.

my_size <- CohortSizeRange(intervals = c(0, 30), cohort_size = c(1, 3))
```
