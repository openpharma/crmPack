# Find the Pool Name for One or More Parameter References

**\[experimental\]**

Returns the hierarchical pool membership of each parameter reference, or
an empty string if the parameter keeps its arm-specific fixed prior.

## Usage

``` r
h_hierarchical_pool_names(arm_name, refs, pooled_map)
```

## Arguments

- arm_name:

  (`string`)\
  arm name.

- refs:

  (`character`)\
  parameter references belonging to that arm.

- pooled_map:

  (`list`)\
  output of
  [`h_hierarchical_make_pool_map()`](https://docs.crmpack.org/reference/h_hierarchical_make_pool_map.md).

## Value

Character vector of pool names, using `""` for unpooled parameters.
