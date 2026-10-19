# Calculate the Posterior Overdose Probability at a Selected Dose

Calculate the Posterior Overdose Probability at a Selected Dose

## Usage

``` r
h_overdose_prob(selected_dose, next_best, model, samples, ...)
```

## Arguments

- selected_dose:

  (`numeric`)\
  final recommended dose.

- next_best:

  ([`NextBest`](https://docs.crmpack.org/reference/NextBest-class.md))\
  next-best rule defining the overdose threshold.

- model:

  model used to calculate toxicity probabilities.

- samples:

  ([`Samples`](https://docs.crmpack.org/reference/Samples-class.md))\
  posterior samples from the final model fit.

- ...:

  additional arguments passed to
  [`prob`](https://docs.crmpack.org/reference/prob.md).

## Value

A single posterior overdose probability, or `NA_real_` if no dose was
selected.
