# Summarize Hierarchical Design Simulations

**\[experimental\]**

Summarize hierarchical simulations by delegating to the corresponding
arm-level simulation summary methods.

## Usage

``` r
# S4 method for class 'HierarchicalSimulations'
summary(object, truth, target = c(0.2, 0.35), ...)
```

## Arguments

- object:

  (`HierarchicalSimulations`)\
  the object we want to summarize.

- truth:

  (`function` or named `list` of `function`)\
  true DLT probability function(s). If a list is supplied, names must
  match the hierarchical arms.

- target:

  (`numeric` or named `list` of `numeric`)\
  target toxicity interval(s). If a list is supplied, names must match
  the hierarchical arms.

- ...:

  additional arguments can be supplied here for `truth`.

## Value

An object of class
[`HierarchicalSimulationsSummary`](https://docs.crmpack.org/reference/HierarchicalSimulationsSummary-class.md).
