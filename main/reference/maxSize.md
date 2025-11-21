# "MAX" combination of cohort size rules

This function combines cohort size rules by taking the maximum of all
sizes.

## Usage

``` r
maxSize(...)

# S4 method for class 'CohortSize'
maxSize(...)
```

## Arguments

- ...:

  Objects of class
  [`CohortSize`](https://openpharma.github.io/crmPack/reference/CohortSize-class.md)

## Value

the combination as an object of class
[`CohortSizeMax`](https://openpharma.github.io/crmPack/reference/CohortSizeMax-class.md)

## Functions

- `maxSize(CohortSize)`: The method combining cohort size rules by
  taking maximum

## See also

[`minSize`](https://openpharma.github.io/crmPack/reference/minSize.md)

## Examples

``` r
# Here is the rule for:
#   having cohort of size 1 for doses <30
#   and having cohort of size 3 for doses >=30
mySize1 <- CohortSizeRange(intervals = c(0, 30), cohort_size = c(1, 3))

# Here is the rule for:
#   having cohort of size 1 until no DLT were observed
#   and having cohort of size 3 as soon as 1 DLT is observed
mySize2 <- CohortSizeDLT(intervals = c(0, 1), cohort_size = c(1, 3))

# This is combining the two rules above by taking the maximum of the sample sizes of
# the single rules
mySize <- maxSize(mySize1, mySize2)
```
