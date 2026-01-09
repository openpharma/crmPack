# `IncrementsRelativeDLT`

**\[stable\]**

`IncrementsRelativeDLT` is the class for increments control based on
relative differences in terms of DLTs.

## Usage

``` r
IncrementsRelativeDLT(intervals, increments)

.DefaultIncrementsRelativeDLT()
```

## Arguments

- intervals:

  (`numeric`)  
  see slot definition.

- increments:

  (`numeric`)  
  see slot definition.

## Slots

- `intervals`:

  (`integer`)  
  a vector with the left bounds of the relevant DLT intervals. For
  example, `intervals = c(0, 1, 3)` specifies three intervals (sets of
  DLTs: first, 0 DLT; second 1 or 2 DLTs; and the third one, at least 3
  DLTs. That means, the right bound of the intervals are exclusive to
  the interval and the last interval goes from the last value to
  infinity.

- `increments`:

  (`numeric`)  
  a vector of maximum allowable relative increments corresponding to
  `intervals`. IT must be of the same length as the length of
  `intervals`.

## Note

This considers all DLTs across all cohorts observed so far.

Typically, end users will not use the `.DefaultIncrementsRelativeDLT()`
function.

## See also

[IncrementsRelativeDLTCurrent](https://openpharma.github.io/crmPack/reference/IncrementsRelativeDLTCurrent-class.md)
which only considers the DLTs in the current cohort.

## Examples

``` r
# This is the example of a rule for:
# maximum doubling the dose if no DLTs were observed in the whole study so far
# or maximum increasing the dose by 1.33 if 1 or 2 DLTs were observed so far
# or maximum increasing the dose by 1.22 if 3 or more DLTs were observed so far.
my_increments <- IncrementsRelativeDLT(
  intervals = c(0, 1, 3),
  increments = c(1, 0.33, 0.2)
)
```
