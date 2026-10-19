# Parse a Hierarchical Parameter Reference

**\[experimental\]**

Converts a user-facing parameter reference into metadata that can be
used by the JAGS code generators.

## Usage

``` r
h_hierarchical_parse_ref(model, arm_name, ref)
```

## Arguments

- model:

  (`GeneralModel`)\
  arm-specific model object.

- arm_name:

  (`string`)\
  the user-facing arm name.

- ref:

  (`string`)\
  parameter reference.

## Value

Named list with entries `kind`, `index`, `latent`, and `sample`.
