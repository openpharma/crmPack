# Remove Fixed Priors for Pooled Nodes

Remove Fixed Priors for Pooled Nodes

## Usage

``` r
h_hierarchical_remove_pooled_prior_lines(model_fun, pooled_nodes)
```

## Arguments

- model_fun:

  (`function`)\
  namespaced prior model function.

- pooled_nodes:

  (`character`)\
  namespaced nodes with exchangeable priors.

## Value

A function with fixed prior distribution statements removed.
