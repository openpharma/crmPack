# Extract Single-Agent Samples from Combo Samples

Converts posterior draws from a
[`TwoDrugsCombo`](https://docs.crmpack.org/reference/TwoDrugsCombo-class.md)
fit back to the sample shape expected by one of its single-agent models.
Shared scalar sample names, such as `alpha0` and `alpha1`, are stored as
matrices in combo samples and sliced by drug. Parameters that are
already model-specific matrix-valued samples, such as mixture-model
parameters, are kept intact.

## Usage

``` r
h_prob_two_drugs_combo_single_samples(samples, model, drug_index)
```

## Arguments

- samples:

  (`Samples`)\
  combo model samples.

- model:

  (`TwoDrugsCombo`)\
  combo model.

- drug_index:

  (`integer`)\
  index of the single-agent model to extract.

## Value

A [`Samples`](https://docs.crmpack.org/reference/Samples-class.md)
object for the requested single-agent model.
