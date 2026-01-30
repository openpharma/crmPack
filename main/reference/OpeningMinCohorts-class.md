# `OpeningMinCohorts`

**\[experimental\]**

`OpeningMinCohorts` opens backfill cohorts when the overall number of
cohorts treated so far in the trial reaches or exceeds a minimum
threshold. This can be used to implement a "delayed backfill cohort
opening" rule.

## Usage

``` r
OpeningMinCohorts(min_cohorts = 2L)

.DefaultOpeningMinCohorts()
```

## Arguments

- min_cohorts:

  (`integer`)  
  see slot definition.

## Slots

- `min_cohorts`:

  (`integer`)  
  the minimum number of cohorts that must have been treated before
  backfilling can be opened.

## Note

Typically, end users will not use the `.DefaultOpeningMinCohorts()`
function.

## See also

[`Opening`](https://openpharma.github.io/crmPack/reference/Opening-class.md)
and the other subclasses listed in there.
