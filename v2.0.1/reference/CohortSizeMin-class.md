# `CohortSizeMin`

**\[stable\]**

`CohortSizeMin` is the class for cohort size that is based on minimum of
multiple cohort size rules. The `cohort_sizes` slot stores a set of
cohort size rules, which are again the objects of class
[`CohortSize`](https://openpharma.github.io/crmPack/reference/CohortSize-class.md).
The minimum of these individual cohort sizes is taken to give the final
cohort size.

## Usage

``` r
CohortSizeMin(cohort_sizes)

.DefaultCohortSizeMin()
```

## Arguments

- cohort_sizes:

  (`list`)  
  see slot definition.

## Slots

- `cohort_sizes`:

  (`list`)  
  a list of cohort size rules, i.e. objects of class
  [`CohortSize`](https://openpharma.github.io/crmPack/reference/CohortSize-class.md).

## Note

Typically, end users will not use the `.DefaultCohortSizeMin()`
function.

## Examples

``` r
# Rule for cohort of size 1 for doses <30 and cohort of size 3 for doses >=30.
my_size1 <- CohortSizeRange(intervals = c(0, 10), cohort_size = c(1, 3))

# Rule for cohort of size 1 until no DLT were observed and cohort of size 3
# as soon as 1 DLT is observed.
my_size2 <- CohortSizeDLT(intervals = c(0, 1), cohort_size = c(1, 3))

# Cohort size rules of class 'CohortSizeMin' which will then be combined with
# the 'min' operation.
my_size <- CohortSizeMin(cohort_sizes = list(my_size1, my_size2))
```
