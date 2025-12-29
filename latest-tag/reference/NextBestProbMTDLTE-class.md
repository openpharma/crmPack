# `NextBestProbMTDLTE`

**\[experimental\]**

`NextBestProbMTDLTE` is the class of finding a next best dose that
selects the dose with the highest probability of having a toxicity rate
less or equal to the toxicity target. The dose is determined by
calculating the posterior toxicity probability for each dose per
iteration and select the maximum dose that has a toxicity probability
below or equal to the target. The dose with the highest frequency of
being selected as MTD across iterations is the next best dose. Placebo
is not considered in the calculation and removed from the dose grid for
any calculations.

## Usage

``` r
NextBestProbMTDLTE(target)

.DefaultNextBestProbMTDLTE()
```

## Arguments

- target:

  (`numeric`)  
  see slot definition.

## Slots

- `target`:

  (`numeric`)  
  the target toxicity probability.

## Note

Typically, end users will not use the `.DefaultNextBestProbMTDLTE()`
function.

## Examples

``` r
# In the example below, the MTD is defined as the dose with the highest
# probability of having a toxicity rate below 30%.
next_best_prob_mtd_lte <- NextBestProbMTDLTE(target = 0.3)
```
