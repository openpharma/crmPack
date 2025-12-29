# `StoppingTargetProb`

**\[stable\]**

`StoppingTargetProb` is the class for stopping based on the probability
of the DLT rate being in the target toxicity interval.

## Usage

``` r
StoppingTargetProb(
  target = c(0.2, 0.35),
  prob = 0.4,
  report_label = NA_character_
)

.DefaultStoppingTargetProb()
```

## Arguments

- target:

  (`number`)  
  see slot definition.

- prob:

  (`proportion`)  
  see slot definition.

- report_label:

  (`string` or `NA`)  
  see slot definition.

## Slots

- `target`:

  (`number`)  
  the target toxicity interval, e.g. `c(0.2, 0.35)`.

- `prob`:

  (`proportion`)  
  required target toxicity probability (except 0 or 1) for reaching
  sufficient precision.

## Note

Typically, end users will not use the `.DefaultStoppingTargetProb()`
function.

## Examples

``` r
# As example, here is the rule for stopping the study if the posterior
# probability that [0.2 =< Prob(DLT | dose) <= 0.35] for the next best dose
# is above 0.5.
my_stopping <- StoppingTargetProb(target = c(0.2, 0.35), prob = 0.5)
```
