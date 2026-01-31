# `IncrementsDoseLevels`

**\[stable\]**

`IncrementsDoseLevels` is the class for increments control based on the
number of dose levels.

## Usage

``` r
IncrementsDoseLevels(levels = 1L, basis_level = "last")

.DefaultIncrementsDoseLevels()
```

## Arguments

- levels:

  (`count`)  
  see slot definition.

- basis_level:

  (`string`)  
  see slot definition.

## Slots

- `levels`:

  (`count`)  
  maximum number of dose levels to increment for the next dose. It
  defaults to 1, which means that no dose skipping is allowed, i.e. the
  next dose can be maximum one level higher than the current base dose.
  The current base dose level is the dose level used to increment from
  (see `basis_level` parameter).

- `basis_level`:

  (`string`)  
  defines the current base dose level. It can take one out of two
  possible values: `last` or `max`. If `last` is specified (default),
  the current base dose level is set to the last dose given. If `max` is
  specified, then the current base dose level is set to the maximum dose
  level given.

## Note

Typically, end users will not use the `.DefaultIncrementsDoseLevels()`
function.

## Examples

``` r
# The rule for dose increments which allows for maximum skip one dose level,
# that is 2 dose levels higher than the last dose given.
my_increments <- IncrementsDoseLevels(levels = 2, basis_level = "last")
```
