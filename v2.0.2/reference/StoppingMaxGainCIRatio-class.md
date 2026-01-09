# `StoppingMaxGainCIRatio`

**\[stable\]**

`StoppingMaxGainCIRatio` is the class for testing a stopping rule that
is based on a target ratio of the 95% credibility interval.
Specifically, this is the ratio of the upper to the lower bound of the
95% credibility interval's estimate of the: (1) target dose (i.e. a dose
that corresponds to a given target probability of the occurrence of a
DLT `prob_target`), or (2) max gain dose (i.e. a dose which gives the
maximum gain), depending on which one out of these two is smaller.

## Usage

``` r
StoppingMaxGainCIRatio(
  target_ratio = 5,
  prob_target = 0.3,
  report_label = NA_character_
)

.DefaultStoppingMaxGainCIRatio()
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

## Examples

``` r
# Define the target stopping ratio (5) and the target probability of a DLT to
# be used (0.3).
my_stopping <- StoppingMaxGainCIRatio(target_ratio = 5, prob_target = 0.3)
.DefaultStoppingMaxGainCIRatio()
#> An object of class "StoppingMaxGainCIRatio"
#> Slot "target_ratio":
#> [1] 5
#> 
#> Slot "prob_target":
#> [1] 0.3
#> 
#> Slot "report_label":
#> [1] "GStar 5 for 0.3 target prob"
#> 
```
