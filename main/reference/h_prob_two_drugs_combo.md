# Calculate Two-Drug Combo Toxicity Probabilities

Combines delegated single-agent toxicity probabilities with the combo
interaction term. Both the monotherapy probabilities and the interaction
covariate follow the corresponding single-agent model definitions.

## Usage

``` r
h_prob_two_drugs_combo(dose, model, samples)
```

## Arguments

- dose:

  (`numeric` or `matrix`)\
  one or more combo dose combinations.

- model:

  (`TwoDrugsCombo`)\
  combo model.

- samples:

  (`Samples`)\
  combo model samples.

## Value

Numeric vector for one dose combination, otherwise numeric matrix with
one row per posterior sample and one column per dose combination.
