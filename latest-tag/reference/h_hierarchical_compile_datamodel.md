# Compile the Hierarchical Data Model

**\[experimental\]**

Concatenates the arm-specific likelihood code into a single JAGS model
function. Each arm receives a unique variable-name prefix derived from
its user-facing name.

## Usage

``` r
h_hierarchical_compile_datamodel(models_to_arms)
```

## Arguments

- models_to_arms:

  (`list`)\
  named arm-specific models.

## Value

A function representing the compiled JAGS data model.
