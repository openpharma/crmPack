# Evaluate Single-Agent Toxicity Probabilities in a Combo Model

Computes the monotherapy toxicity contribution for one drug by
delegating to that drug's own
[`prob()`](https://docs.crmpack.org/reference/prob.md) method. This
keeps the combo probability calculation aligned with each single-agent
link function and dose transformation.

## Usage

``` r
h_prob_two_drugs_combo_single_prob(dose, model, samples, drug_index)
```

## Arguments

- dose:

  (`matrix`)\
  combo dose combinations, one row per combination.

- model:

  (`TwoDrugsCombo`)\
  combo model.

- samples:

  (`Samples`)\
  combo model samples.

- drug_index:

  (`integer`)\
  index of the single-agent model to extract.

## Value

Numeric matrix with one row per posterior sample and one column per dose
combination.
