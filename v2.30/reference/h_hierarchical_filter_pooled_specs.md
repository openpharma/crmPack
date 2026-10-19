# Drop Fixed-Prior Specs for Fully Pooled Generic Parameters

Drop Fixed-Prior Specs for Fully Pooled Generic Parameters

## Usage

``` r
h_hierarchical_filter_pooled_specs(specs, model, arm_name, refs, pool_names)
```

## Arguments

- specs:

  (`list`)\
  namespaced model specifications.

- model:

  (`GeneralModel`)\
  arm-specific model object.

- arm_name:

  (`string`)\
  hierarchical arm name.

- refs:

  (`character`)\
  supported hierarchical references.

- pool_names:

  (`character`)\
  pool names matching `refs`.

## Value

Filtered `specs`.
