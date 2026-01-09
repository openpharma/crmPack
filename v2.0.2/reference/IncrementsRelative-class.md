# `IncrementsRelative`

**\[stable\]**

`IncrementsRelative` is the class for increments control based on
relative differences in intervals.

## Usage

``` r
IncrementsRelative(intervals, increments)

.DefaultIncrementsRelative()
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

  (`numeric`)  
  a vector with the left bounds of the relevant intervals. For example,
  `intervals = c(0, 50, 100)` specifies three intervals: \\(0, 50)\\,
  \\\[50, 100)\\ and \\\[100, +Inf)\\. That means, the right bound of
  the intervals are exclusive to the interval and the last interval goes
  from the last value to infinity.

- `increments`:

  (`numeric`)  
  a vector of the same length with the maximum allowable relative
  increments in the `intervals`.

## Note

Typically, end users will not use the `.DefaultIncrementsRelative()`
function.

## Examples

``` r
# This is the example of a rule for:
# maximum doubling the dose if the current dose is <20
# or only maximum increasing the dose by 1.33 if the current dose is >=20.
my_increments <- IncrementsRelative(
  intervals = c(0, 20),
  increments = c(1, 0.33)
)
```
