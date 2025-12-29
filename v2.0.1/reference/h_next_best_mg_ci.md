# Credibility Intervals for Max Gain and Target Doses at `nextBest-NextBestMaxGain` Method.

**\[experimental\]**

Helper function for
[`nextBest-NextBestMaxGain()`](https://openpharma.github.io/crmPack/reference/nextBest.md)
method. It computes a 95% credibility intervals for given target dose
and max gain dose. It also returns a ratio of upper and lower bounds of
the interval.

## Usage

``` r
h_next_best_mg_ci(dose_target, dose_mg, prob_target, placebo, model, model_eff)
```

## Arguments

- dose_target:

  (`number`)  
  target dose estimate.

- dose_mg:

  (`number`)  
  the dose corresponding to the maximum gain.

- prob_target:

  (`proportion`)  
  target DLT probability.

- placebo:

  (`flag`)  
  if `TRUE` the first dose level in the dose grid used is considered as
  placebo. This is needed to adjust the max gain dose using efficacy
  constant value. If the `placebo` was used, then the `model_eff@const`
  is added to `dose_mg`.

- model:

  (`ModelTox`)  
  the DLT model.

- model_eff:

  (`Effloglog`)  
  the efficacy model.

## References

Yeung WY, Whitehead J, Reigner B, Beyer U, Diack C, Jaki T (2015).
“Bayesian adaptive dose-escalation procedure for binary and continuous
responses utilizing a gain function.” *Pharmaceutical Statistics*.
[doi:10.1002/pst.1706](https://doi.org/10.1002/pst.1706) , Published
online ahead of print.
