# `StoppingDoseStabilized`

**\[experimental\]**

`StoppingDoseStabilized` is the class for stopping when the next best
dose has stabilized. Stabilization occurs when the current next best
dose is the same as the dose administered to each of the most recent
consecutive `nCohorts` cohorts.

For combination trials, both dose components must be the same.

## Usage

``` r
StoppingDoseStabilized(nCohorts = 1L, report_label = NA_character_)

.DefaultStoppingDoseStabilized()
```

## Arguments

- nCohorts:

  (`number`)\
  see slot definition.

- report_label:

  (`string` or `NA`)\
  see slot definition.

## Slots

- `nCohorts`:

  (`number`)\
  number of consecutive cohorts required to have received the current
  next best dose.

## Note

Typically, end users will not use the `.DefaultStoppingDoseStabilized()`
function.

## Examples

``` r
# Stop when the next best dose has remained unchanged for three consecutive
# cohorts.
my_stopping <- StoppingDoseStabilized(nCohorts = 3)
```
