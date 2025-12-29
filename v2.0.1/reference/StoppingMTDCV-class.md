# `StoppingMTDCV`

**\[experimental\]**

`StoppingMTDCV` is a class for stopping rule based on precision of MTD
which is calculated as the coefficient of variation (CV) of the MTD.
Here, the MTD is defined as the dose that reaches a specific `target`
probability of the occurrence of a DLT.

## Usage

``` r
StoppingMTDCV(target = 0.3, thresh_cv = 40, report_label = NA_character_)

.DefaultStoppingMTDCV()
```

## Arguments

- target:

  (`proportion`)  
  see slot definition.

- thresh_cv:

  (`number`)  
  see slot definition.

- report_label:

  (`string` or `NA`)  
  see slot definition.

## Slots

- `target`:

  (`proportion`)  
  toxicity target of MTD (except 0 or 1).

- `thresh_cv`:

  (`number`)  
  threshold (percentage \> 0) for CV to be considered accurate enough to
  stop the trial. The stopping occurs when the CV is less than or equal
  to `tresh_cv`.

## Note

Typically, end users will not use the `.DefaultStoppingMTDCV()`
function.

## Examples

``` r
# Stopping the study if the MTD estimation is precise enough, i.e. if robust
# coefficient of variation of the MTD is below 40%.
my_stopping <- StoppingMTDCV(target = 0.3, thresh_cv = 40)
```
