# "MIN" Combination of Cohort Size Rules

**\[stable\]**

This function combines cohort size rules by taking the minimum of all
sizes.

## Usage

``` r
minSize(...)

# S4 method for class 'CohortSize'
minSize(...)
```

## Arguments

- ...:

  Objects of class
  [`CohortSize`](https://openpharma.github.io/crmPack/reference/CohortSize-class.md).

## Value

The combination as an object of class
[`CohortSizeMin`](https://openpharma.github.io/crmPack/reference/CohortSizeMin-class.md).

## Functions

- `minSize(CohortSize)`: The method combining cohort size rules by
  taking minimum.

## See also

[`maxSize()`](https://openpharma.github.io/crmPack/reference/maxSize.md)

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

# This is combining the two rules above by taking the minimum of the sample sizes of
# the single rules
mySize <- minSize(mySize1, mySize2)
```
