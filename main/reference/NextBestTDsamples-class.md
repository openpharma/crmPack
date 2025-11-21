# `NextBestTDsamples`

**\[stable\]**

`NextBestTDsamples` is the class to find a next best dose based on
Pseudo DLT model with samples. Namely, it is to find two next best
doses, one for allocation during the trial and the second for final
recommendation at the end of a trial. Hence, there are two target
probabilities of the occurrence of a DLT that must be specified: target
probability to be used during the trial and target probability to be
used at the end of the trial.

## Usage

``` r
NextBestTDsamples(prob_target_drt, prob_target_eot, derive)

.DefaultNextBestTDsamples()
```

## Arguments

- prob_target_drt:

  (`proportion`)  
  see slot definition in
  [`NextBestTD`](https://openpharma.github.io/crmPack/reference/NextBestTD-class.md).

- prob_target_eot:

  (`proportion`)  
  see slot definition in
  [`NextBestTD`](https://openpharma.github.io/crmPack/reference/NextBestTD-class.md).

- derive:

  (`function`)  
  see slot definition.

## Slots

- `derive`:

  (`function`)  
  derives, based on a vector of posterior dose samples, the target dose
  that has the probability of the occurrence of DLT equals to either the
  `prob_target_drt` or `prob_target_eot`. It must therefore accept one
  and only one argument, which is a numeric vector, and return a number.

## Note

Typically, end users will not use the `.DefaultNextBestTDsamples()`
function.

## Examples

``` r
# Target probability of the occurrence of a DLT during the trial is set to 0.35.
# Target probability of the occurrence of a DLT at the end of the trial is set to 0.3.
# We want the use the 30% posterior quantile (the 30th percentile) of the TD35
# (the dose level with probability of the DLT equals 0.35) and TD30 samples.
my_next_best <- NextBestTDsamples(
  prob_target_drt = 0.35,
  prob_target_eot = 0.3,
  derive = function(samples) {
    as.numeric(quantile(samples, probs = 0.3))
  }
)
```
