# `IncrementsRelativeDLTCurrent`

**\[experimental\]**

`IncrementsRelativeDLTCurrent` is the class for increments control based
on relative differences and current DLTs. The class is based on the
number of DLTs observed in the current cohort, but not cumulatively over
all cohorts so far.

## Usage

``` r
IncrementsRelativeDLTCurrent(intervals = c(0L, 1L), increments = c(2L, 1L))

.DefaultIncrementsRelativeDLTCurrent()
```

## Arguments

- intervals:

  (`numeric`)  
  see slot definition.

- increments:

  (`numeric`)  
  see slot definition.

## Note

Typically, end users will not use the
`.DefaultIncrementsRelativeDLTCurrent()` function.

## See also

[IncrementsRelativeDLT](https://openpharma.github.io/crmPack/reference/IncrementsRelativeDLT-class.md).

## Examples

``` r
# As example, here is the rule for:
# maximum doubling the dose if no DLTs were observed at the current dose
# or maximum increasing the dose by 1.33 if 1 or 2 DLTs were observed at the current dose
# or maximum increasing the dose by 1.22 if 3 or more DLTs were observed.

my_increments <- IncrementsRelativeDLTCurrent(
  intervals = c(0, 1, 3),
  increments = c(1, 0.33, 0.2)
)
```
