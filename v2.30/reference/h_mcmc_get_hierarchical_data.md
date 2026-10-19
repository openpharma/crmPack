# Flatten `HierarchicalData` into JAGS Input for a `HierarchicalModel`

**\[experimental\]**

Prepares the data list consumed by the dynamically compiled hierarchical
JAGS model. Arm-specific data are renamed with arm-specific prefixes so
they align with the generated model code.

## Usage

``` r
h_mcmc_get_hierarchical_data(model, data, from_prior)
```

## Arguments

- model:

  (`HierarchicalModel`)\
  hierarchical model object.

- data:

  (`HierarchicalData`)\
  hierarchical data object.

- from_prior:

  (`flag`)\
  should only prior-related inputs be returned?

## Value

Named list suitable for `rjags::jags.model(data = ...)`.
