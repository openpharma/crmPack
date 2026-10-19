# Compile the Hierarchical Model-Specification Function

**\[experimental\]**

Creates the `modelspecs` function used by
[`mcmc()`](https://docs.crmpack.org/reference/mcmc.md) to prepare the
JAGS data needed by the dynamically compiled hierarchical model.

## Usage

``` r
h_hierarchical_compile_modelspecs(
  models_to_arms,
  parameter_pools,
  pool_correlations = list(),
  pool_priors = list()
)
```

## Arguments

- models_to_arms:

  (`list`)\
  named arm-specific models.

- parameter_pools:

  (`list`)\
  exchangeable parameter specification from
  [`HierarchicalModel()`](https://docs.crmpack.org/reference/HierarchicalModel-class.md).

- pool_correlations:

  (`list`)\
  optional named list pairing exactly two scalar exchangeable pools into
  correlated bivariate normal blocks. Higher dimensional correlation
  blocks, such as correlations across three or more parameters, are not
  supported.

- pool_priors:

  (`list`)\
  optional named list of pool-specific hyperprior overrides.

## Value

A function returning the JAGS data list.
