# Compile the Hierarchical Initial-Value Function

**\[experimental\]**

Wraps the per-arm `init` functions and renames their outputs so they
match the dynamically generated hierarchical JAGS variable names.

## Usage

``` r
h_hierarchical_compile_init(
  models_to_arms,
  parameter_pools,
  pool_correlations = list()
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

## Value

A function returning a named list of JAGS initial values.
