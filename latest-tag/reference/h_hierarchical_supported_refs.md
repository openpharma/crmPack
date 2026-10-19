# List Supported Exchangeable Parameter References for an Arm Model

**\[experimental\]**

Returns the parameter reference syntax that may be used for a model in
the `exchangeable_parameters` argument of
[`HierarchicalModel()`](https://docs.crmpack.org/reference/HierarchicalModel-class.md).
Combination models additionally support `"eta"`, which pools the
arm-specific interaction parameters (or their logarithms when
`log_normal_eta = TRUE`).

## Usage

``` r
h_hierarchical_supported_refs(model)
```

## Arguments

- model:

  (`GeneralModel`)\
  arm-specific model object.

## Value

Character vector of supported references.
