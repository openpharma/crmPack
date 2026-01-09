# `StoppingMTDdistribution`

**\[stable\]**

`StoppingMTDdistribution` is the class for stopping based on the
posterior distribution of the MTD. It is used for the cases where the
stopping occurs when the probability of `MTD > thresh * next_dose` is
greater than or equal to `prob`, where the `next_dose` is the
recommended next best dose. Here, the MTD is defined as the dose that
reaches a specific `target` probability of the occurrence of a DLT.

## Usage

``` r
StoppingMTDdistribution(
  target = 0.33,
  thresh = 0.5,
  prob = 0.9,
  report_label = NA_character_
)

.DefaultStoppingMTDdistribution()
```

## Arguments

- target:

  (`proportion`)  
  see slot definition.

- thresh:

  (`proportion`)  
  see slot definition.

- prob:

  (`proportion`)  
  see slot definition.

- report_label:

  (`string` or `NA`)  
  see slot definition.

## Slots

- `target`:

  (`proportion`)  
  the target toxicity probability (except 0 or 1) defining the MTD.

- `thresh`:

  (`proportion`)  
  the threshold (except 0 or 1) relative to the recommended next best
  dose.

- `prob`:

  (`proportion`)  
  required minimum probability, except 0 or 1.

## Note

Typically, end users will not use the
`.DefaultStoppingMTDDistribution()` function.

## Examples

``` r
# As example, here is the rule for stopping the study if there is at least 0.9
# probability that MTD > 0.5 * next_dose. Here MTD is defined as the dose for
# which prob(DLT) = 0.33
my_stopping <- StoppingMTDdistribution(
  target = 0.33,
  thresh = 0.5,
  prob = 0.9
)
```
