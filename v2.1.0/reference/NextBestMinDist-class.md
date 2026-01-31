# `NextBestMinDist`

**\[stable\]**

`NextBestMinDist` is the class for next best dose that is based on
minimum distance to target probability.

## Usage

``` r
NextBestMinDist(target)

.DefaultNextBestMinDist()
```

## Arguments

- target:

  (`proportion`)  
  see slot definition.

## Slots

- `target`:

  (`proportion`)  
  single target toxicity probability, except 0 or 1.

## Note

Typically, end users will not use the `.DefaultNextBestMinDist()`
function.

## Examples

``` r
# In the example below, the MTD is defined as the dose with the toxicity rate
# with minimal distance to the target of 30%.
next_best_min_dist <- NextBestMinDist(target = 0.3)
```
