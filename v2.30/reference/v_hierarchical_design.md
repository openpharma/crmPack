# Internal Helper Functions for Validation of [`HierarchicalDesign`](https://docs.crmpack.org/reference/HierarchicalDesign-class.md) Objects

**\[experimental\]**

These functions are only used internally to validate the format of an
input
[`HierarchicalDesign`](https://docs.crmpack.org/reference/HierarchicalDesign-class.md)
or related classes and therefore not exported.

## Usage

``` r
v_design_arm(object)

v_hierarchical_design(object)
```

## Arguments

- object:

  (`HierarchicalDesign`)\
  object to validate.

## Value

A `character` vector with the validation failure messages, or `TRUE` in
case validation passes.

## Functions

- `v_design_arm()`: validates that the
  [`DesignArm`](https://docs.crmpack.org/reference/DesignArm-class.md)
  object contains valid slots.

- `v_hierarchical_design()`: validates that the
  [`HierarchicalDesign`](https://docs.crmpack.org/reference/HierarchicalDesign-class.md)
  object contains valid hierarchical arm metadata.
