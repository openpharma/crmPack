# Compile the Hierarchical Prior Model

**\[experimental\]**

Builds the prior part of a hierarchical JAGS model. Unpooled parameters
keep their arm-specific fixed priors, while pooled parameters are linked
through exchangeable normal distributions with simple hyperpriors.

## Usage

``` r
h_hierarchical_compile_priormodel(
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

A list with entries `priormodel` and `sample`.
