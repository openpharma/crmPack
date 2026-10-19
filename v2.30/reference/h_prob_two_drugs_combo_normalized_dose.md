# Evaluate a Single-Agent Dose Normalization

Reuses the dose-normalization expression inferred from a single-agent
JAGS data model, such as `x / ref_dose` or `x - ref_dose`, and evaluates
it for R dose inputs. The result is used for the combo interaction term
so runtime probabilities match the JAGS model used for MCMC.

## Usage

``` r
h_prob_two_drugs_combo_normalized_dose(dose, single_model)
```

## Arguments

- dose:

  (`numeric`)\
  single-agent doses.

- single_model:

  (`GeneralModel`)\
  single-agent model.

## Value

Numeric vector of normalized doses.
