# `HierarchicalModel`

**\[experimental\]**

`HierarchicalModel` is a class for the prototype hierarchical model that
links compatible single-agent binary outcome arms and/or
[`TwoDrugsCombo`](https://docs.crmpack.org/reference/TwoDrugsCombo-class.md)
arms.

## Usage

``` r
HierarchicalModel(
  ...,
  exchangeable_parameters = list(),
  pool_correlations = list(),
  pool_priors = list()
)

.DefaultHierarchicalModel()
```

## Arguments

- ...:

  named model objects describing the trial arms.

- exchangeable_parameters:

  a named list describing which parameters are exchangeable across arms.
  This will be used to define the hierarchical structure of the model.
  Each list entry contains the arms as names and the parameters to be
  shared as a string.

- pool_correlations:

  optional named list pairing exactly two scalar entries from
  `exchangeable_parameters` into a correlated bivariate hierarchy. Each
  pair must refer to indices 1 and 2 of the same latent parameter vector
  in every shared arm. Correlating three or more parameters in one
  multivariate hierarchy is not supported.

- pool_priors:

  optional named list of hyperprior overrides for entries in
  `exchangeable_parameters`. Each entry may contain `mu = c(mean, sd)`
  and/or `tau = c(meanlog, sdlog)`.

## Details

The class currently stores the structural pieces from the design
prototype as a named list of arm-specific models and a named list of
exchangeable parameter pools used to dynamically compile a joint JAGS
model.

## Slots

- `models_to_arms`:

  (`list`)\
  named list of arm-specific models. Each entry must be either a
  compatible single-agent binary outcome
  [`GeneralModel`](https://docs.crmpack.org/reference/GeneralModel-class.md)
  object using `nObs`, `y`, and `x` inputs, or a
  [`TwoDrugsCombo`](https://docs.crmpack.org/reference/TwoDrugsCombo-class.md)
  object.

- `parameter_pools`:

  (`list`)\
  named list describing which parameters are exchangeable across arms.
  Each list entry contains arm names as names and parameter references
  as values.

- `pool_correlations`:

  (`list`)\
  named list pairing exactly two scalar exchangeable parameter pools
  that should use a correlated bivariate normal hierarchy.

- `pool_priors`:

  (`list`)\
  named list of pool-specific hyperprior overrides.

## Note

Typically, end users will not use the `.DefaultHierarchicalModel()`
function directly.

## See also

[`HierarchicalData`](https://docs.crmpack.org/reference/HierarchicalData-class.md),
[`TwoDrugsCombo`](https://docs.crmpack.org/reference/TwoDrugsCombo-class.md).

## Examples

``` r
mono_model <- LogisticLogNormal(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
  ref_dose = 10
)

combo_model <- TwoDrugsCombo(
  single_models = list(
    drug1 = LogisticLogNormal(
      mean = c(-0.85, 1),
      cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
      ref_dose = 10
    ),
    drug2 = LogisticLogNormal(
      mean = c(-0.7, 0.8),
      cov = matrix(c(1.1, -0.3, -0.3, 0.9), nrow = 2),
      ref_dose = 20
    )
  ),
  gamma = 0,
  tau = 1
)

my_model <- HierarchicalModel(
  my_mono = mono_model,
  my_combo = combo_model,
  exchangeable_parameters = list(
    mono_intercept = list(
      my_mono = "alpha0",
      my_combo = "alpha0[1]"
    ),
    mono_slope = list(
      my_mono = "alpha1",
      my_combo = "alpha1[1]"
    )
  )
)

my_model
#> An object of class 'HierarchicalModel'
#> Arms (2): my_mono, my_combo
#> Arm models: my_mono = LogisticLogNormal, my_combo = TwoDrugsCombo
#> Exchangeable parameter pools (2): mono_intercept, mono_slope
```
