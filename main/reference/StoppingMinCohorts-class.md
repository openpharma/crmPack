# `StoppingMinCohorts`

**\[stable\]**

`StoppingMinCohorts` is the class for stopping based on minimum number
of cohorts.

## Usage

``` r
StoppingMinCohorts(nCohorts = 2L, report_label = NA_character_)

.DefaultStoppingMinCohorts()
```

## Arguments

- nCohorts:

  (`number`)  
  see slot definition.

- report_label:

  (`string` or `NA`)  
  see slot definition.

## Slots

- `nCohorts`:

  (`number`)  
  minimum required number of cohorts.

## Note

Typically, end users will not use the `.DefaultStoppingMinCohorts()`
function.

## Examples

``` r
# As example, here is the rule for stopping the study if at least 6 cohorts
# were already dosed.
my_stopping <- StoppingMinCohorts(nCohorts = 6)
```
