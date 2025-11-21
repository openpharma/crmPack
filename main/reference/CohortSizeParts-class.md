# `CohortSizeParts`

**\[stable\]**

`CohortSizeParts` is the class for cohort size that changes for the
second part of the dose escalation. It works only in conjunction with
[`DataParts`](https://openpharma.github.io/crmPack/reference/DataParts-class.md)
objects.

## Usage

``` r
CohortSizeParts(cohort_sizes)

.DefaultCohortSizeParts()
```

## Arguments

- cohort_sizes:

  (`numeric`)  
  see slot definition.

## Slots

- `cohort_sizes`:

  (`integer`)  
  a vector of length two with two sizes, one for part 1, and one for
  part 2 respectively.

## Note

Typically, end users will not use the `.DefaultCohortSizeParts()`
function.

## Examples

``` r
# Part 1 cohort size = 1, Part 2 cohort size = 3.
my_size <- CohortSizeParts(cohort_sizes = c(1, 3))
```
