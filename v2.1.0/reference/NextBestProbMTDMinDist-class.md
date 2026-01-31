# `NextBestProbMTDMinDist`

**\[experimental\]**

`NextBestProbMTDMinDist` is the class of finding a next best dose that
selects the dose with the highest probability of having a toxicity rate
with the smallest distance to the toxicity target. The dose is
determined by calculating the posterior toxicity probability for each
dose per iteration and select the dose that has the smallest toxicity
probability distance to the target. The dose with the highest frequency
of being selected as MTD across iterations is the next best dose.
Placebo is not considered as the next dose and for that reason not used
in calculations. I.e. for placebo the toxicity probability distance to
target is not calculated and taken into account for determination of the
next dose.

## Usage

``` r
NextBestProbMTDMinDist(target)

.DefaultNextBestProbMTDMinDist()
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

Typically, end users will not use the `.DefaultNextBestProbMTDMinDist()`
function.

## Examples

``` r
# In the example below, the MTD is defined as the dose with the highest
# probability of having a toxicity rate with minimal distance
# to the target of 30%.
next_best_prob_mtd_min_dist <- NextBestProbMTDMinDist(target = 0.3)
```
