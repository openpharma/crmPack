# `StoppingTDCIRatio`

**\[stable\]**

`StoppingTDCIRatio` is the class for testing a stopping rule that is
based on a target ratio of the 95% credibility interval. Specifically,
this is the ratio of the upper to the lower bound of the 95% credibility
interval's estimate of the target dose (i.e. a dose that corresponds to
a given target probability of the occurrence of a DLT `prob_target`).

## Usage

``` r
StoppingTDCIRatio(
  target_ratio = 5,
  prob_target = 0.3,
  report_label = NA_character_
)

.DefaultStoppingTDCIRatio()
```

## Arguments

- target_ratio:

  (`numeric`)  
  see slot definition.

- prob_target:

  (`proportion`)  
  see slot definition.

- report_label:

  (`string` or `NA`)  
  see slot definition.

## Slots

- `target_ratio`:

  (`numeric`)  
  target for the ratio of the 95% credibility interval's estimate, that
  is required to stop a trial.

- `prob_target`:

  (`proportion`)  
  the target probability of the occurrence of a DLT.

## Note

Typically, end users will not use the `.DefaultStoppingTDCIRatio()`
function.

## Examples

``` r
# Define the target stopping ratio (5) and the target probability of a DLT to
# be used (0.3).
my_stopping <- StoppingTDCIRatio(
  target_ratio = 5,
  prob_target = 0.3
)
```
