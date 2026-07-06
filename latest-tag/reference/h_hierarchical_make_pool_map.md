# Flatten Hierarchical Pool Definitions into a Lookup Table

**\[experimental\]**

Builds a lookup keyed by `"arm::reference"` so the compiler can quickly
tell whether a parameter is pooled or fixed.

## Usage

``` r
h_hierarchical_make_pool_map(parameter_pools)
```

## Arguments

- parameter_pools:

  (`list`)\
  exchangeable parameter specification from
  [`HierarchicalModel()`](https://docs.crmpack.org/reference/HierarchicalModel-class.md).

## Value

Named list mapping `"arm::reference"` keys to pool names.
