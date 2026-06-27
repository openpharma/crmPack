# Find Pooled Nodes for an Arm

Find Pooled Nodes for an Arm

## Usage

``` r
h_hierarchical_pooled_nodes(model, arm_name, refs, pool_names)
```

## Arguments

- model:

  (`GeneralModel`)\
  arm-specific model object.

- arm_name:

  (`string`)\
  hierarchical arm name.

- refs:

  (`character`)\
  hierarchical references.

- pool_names:

  (`character`)\
  pool names matching `refs`.

## Value

Character vector of namespaced nodes with exchangeable priors.
