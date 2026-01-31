# `NextBestMaxGainSamples`

**\[stable\]**

`NextBestMaxGainSamples` is the class to find a next best dose with
maximum gain value based on a pseudo DLT and efficacy models and DLT and
efficacy samples. There are two target probabilities of the occurrence
of a DLT that must be specified: target probability to be used during
the trial and target probability to be used at the end of the trial. It
is suitable to use it only with the
[`ModelTox`](https://openpharma.github.io/crmPack/reference/ModelTox-class.md)
model and
[`ModelEff`](https://openpharma.github.io/crmPack/reference/ModelEff-class.md)
classes.

## Usage

``` r
NextBestMaxGainSamples(prob_target_drt, prob_target_eot, derive, mg_derive)

.DefaultNextBestMaxGainSamples()
```

## Arguments

- prob_target_drt:

  (`proportion`)  
  see slot definition in
  [`NextBestMaxGain`](https://openpharma.github.io/crmPack/reference/NextBestMaxGain-class.md).

- prob_target_eot:

  (`proportion`)  
  see slot definition in
  [`NextBestMaxGain`](https://openpharma.github.io/crmPack/reference/NextBestMaxGain-class.md).

- derive:

  (`function`)  
  see slot definition.

- mg_derive:

  (`function`)  
  see slot definition.

## Slots

- `derive`:

  (`function`)  
  derives, based on a vector of posterior dose samples, the target dose
  that has the probability of the occurrence of DLT equals to either the
  `prob_target_drt` or `prob_target_eot`. It must therefore accept one
  and only one argument, which is a numeric vector, and return a number.

- `mg_derive`:

  (`function`)  
  derives, based on a vector of posterior dose samples that give the
  maximum gain value, the final next best estimate of the dose that
  gives the maximum gain value. It must therefore accept one and only
  one argument, which is a numeric vector, and return a number.

## Note

Typically, end users will not use the `.DefaultNextBestMaxGainSamples()`
function.

## Examples

``` r
# Target probability of the occurrence of a DLT during the trial is set to 0.35.
# Target probability of the occurrence of a DLT at the end of the trial is set to 0.3.
# We want the use the 30% posterior quantile (the 30th percentile) of the TD35
# (the dose level with probability of the DLT equals 0.35) and TD30 samples.
# For `mg_derive` function (which takes the sample of doses which give the maximum
# gain), we will use the 50% posterior quantile (the median or th 50th percentile)
# of the sample.
my_next_best <- NextBestMaxGainSamples(
  prob_target_drt = 0.35,
  prob_target_eot = 0.3,
  derive = function(samples) {
    as.numeric(quantile(samples, prob = 0.3))
  },
  mg_derive = function(mg_samples) {
    as.numeric(quantile(mg_samples, prob = 0.5))
  }
)
```
