# `StoppingCohortsNearDose`

**\[stable\]**

`StoppingCohortsNearDose` is the class for stopping based on number of
cohorts near to next best dose.

## Usage

``` r
StoppingCohortsNearDose(
  nCohorts = 2L,
  percentage = 50,
  report_label = NA_character_
)

.DefaultStoppingCohortsNearDose()
```

## Arguments

- nCohorts:

  (`number`)  
  see slot definition.

- percentage:

  (`number`)  
  see slot definition.

- report_label:

  (`string` or `NA`)  
  see slot definition.

## Slots

- `nCohorts`:

  (`number`)  
  number of required cohorts.

- `percentage`:

  (`number`)  
  percentage (between and including 0 and 100) within the next best dose
  the cohorts must lie.

## Note

Typically, end users will not use the
`.DefaultStoppingCohortsNearDose()` function.

## Examples

``` r
# Here, is the rule for stopping the study if at least 3 cohorts were dosed
# at a dose within (1 +/- 0.2) of the next best dose.
my_stopping <- StoppingCohortsNearDose(
  nCohorts = 3,
  percentage = 0.2
)
```
