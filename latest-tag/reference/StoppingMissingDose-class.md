# `StoppingMissingDose`

**\[experimental\]**

`StoppingMissingDose` is the class for stopping based on NA returned by
next best dose.

## Usage

``` r
StoppingMissingDose(report_label = NA_character_)

.DefaultStoppingMissingDose()
```

## Arguments

- report_label:

  (`string` or `NA`)  
  see slot definition.

## Note

Typically, end users will not use the `.DefaultStoppingMissingDose()`
function.

## Examples

``` r
# The rule for stopping the study if NA or Placebo is returned as
# next best dose.
my_stopping <- StoppingMissingDose()
```
