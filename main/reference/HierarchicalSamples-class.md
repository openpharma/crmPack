# `HierarchicalSamples`

**\[experimental\]**

`HierarchicalSamples` stores posterior draws from a
[`HierarchicalModel`](https://docs.crmpack.org/reference/HierarchicalModel-class.md)
together with metadata that maps each hierarchical arm back to its
arm-level sample names.

## Usage

``` r
HierarchicalSamples(data, options, arm_samples)

.DefaultHierarchicalSamples()
```

## Arguments

- data:

  see slot definition.

- options:

  see slot definition.

- arm_samples:

  see slot definition.

## Slots

- `arm_samples`:

  (`list`)\
  named list with one entry per hierarchical arm. Each entry is a named
  character vector mapping arm-level parameter names such as `"alpha0"`
  to the corresponding sample names stored in `data`.

## Note

Typically, end users will not use the `.DefaultHierarchicalSamples()`
function directly.
