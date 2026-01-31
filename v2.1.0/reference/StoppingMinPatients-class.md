# `StoppingMinPatients`

**\[stable\]**

`StoppingMinPatients` is the class for stopping based on minimum number
of patients

## Usage

``` r
StoppingMinPatients(nPatients = 20L, report_label = NA_character_)

.DefaultStoppingMinPatients()
```

## Arguments

- nPatients:

  (`number`)  
  see slot definition.

- report_label:

  (`string` or `NA`)  
  see slot definition.

## Slots

- `nPatients`:

  (`number`)  
  minimum allowed number of patients.

## Note

Typically, end users will not use the `.DefaultStoppingMinPatients()`
function.

## Examples

``` r
# As example, here is the rule for stopping the study if at least 20 patients
# were already dosed
my_stopping <- StoppingMinPatients(nPatients = 20)
```
