# Sanitize a Hierarchical Name for Generated JAGS Code

**\[experimental\]**

Replaces every non-alphanumeric character with an underscore so that
user supplied arm and pool names can safely be embedded in generated
JAGS variable names.

## Usage

``` r
h_hierarchical_safe_name(name)
```

## Arguments

- name:

  (`string`)\
  the arm or pool name to sanitize.

## Value

A character scalar suitable for generated JAGS identifiers.
