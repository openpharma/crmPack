# `NextBestMTD`

**\[stable\]**

`NextBestMTD` is the class for next best dose based on MTD estimate.

## Usage

``` r
NextBestMTD(target, derive)

.DefaultNextBestMTD()
```

## Arguments

- target:

  (`proportion`)  
  see slot definition.

- derive:

  (`function`)  
  see slot definition.

## Slots

- `target`:

  (`proportion`)  
  target toxicity probability, except 0 or 1.

- `derive`:

  (`function`)  
  a function which derives the final next best MTD estimate, based on
  vector of posterior MTD samples. It must therefore accept one and only
  one argument, which is a numeric vector, and return a number.

## Note

Typically, end users will not use the `.DefaultNextBestMTD()` function.

## Examples

``` r
# In the example below, the MTD is defined as the dose for which prob(DLE) = 0.33
# and we will use the 25th quantile of the posterior of MTD as our next best dose.
next_best_mtd <- NextBestMTD(
  target = 0.33,
  derive = function(mtd_samples) {
    quantile(mtd_samples, probs = 0.25)
  }
)
```
