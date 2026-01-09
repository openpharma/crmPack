# `StoppingPatientsNearDose`

**\[stable\]**

`StoppingPatientsNearDose` is the class for stopping based on number of
patients near to next best dose.

## Usage

``` r
StoppingPatientsNearDose(
  nPatients = 10L,
  percentage = 50,
  report_label = NA_character_
)

.DefaultStoppingPatientsNearDose()
```

## Arguments

- nPatients:

  (`number`)  
  see slot definition.

- percentage:

  (`number`)  
  see slot definition.

- report_label:

  (`string` or `NA`)  
  see slot definition.

## Slots

- `nPatients`:

  (`number`)  
  number of required patients.

- `percentage`:

  (`number`)  
  percentage (between and including 0 and 100) within the next best dose
  the patients must lie.

## Note

Typically, end users will not use the
`.DefaultStoppingPatientsNearDose()` function.

## Examples

``` r
# As example, here is the rule for stopping the study if at least 9 patients
# were dosed at a dose within (1 +/- 0.2) of the next best dose.

my_stopping <- StoppingPatientsNearDose(
  nPatients = 9,
  percentage = 20
)
```
