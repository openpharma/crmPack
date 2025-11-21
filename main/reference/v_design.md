# Internal Helper Functions for Validation of [`RuleDesign`](https://openpharma.github.io/crmPack/reference/RuleDesign-class.md) Objects

**\[stable\]**

These functions are only used internally to validate the format of an
input
[`RuleDesign`](https://openpharma.github.io/crmPack/reference/RuleDesign-class.md)
or inherited classes and therefore not exported.

**\[experimental\]**

These functions are only used internally to validate the format of an
input
[`RuleDesignOrdinal`](https://openpharma.github.io/crmPack/reference/RuleDesignOrdinal-class.md)
or inherited classes and therefore not exported.

## Usage

``` r
v_rule_design(object)

v_rule_design_ordinal(object)

v_design_grouped(object)
```

## Arguments

- object:

  (`RuleDesignOrdinal`)  
  object to validate.

## Value

A `character` vector with the validation failure messages, or `TRUE` in
case validation passes.

A `character` vector with the validation failure messages, or `TRUE` in
case validation passes.

## Functions

- `v_rule_design()`: validates that the
  [`RuleDesign`](https://openpharma.github.io/crmPack/reference/RuleDesign-class.md)
  object contains valid `startingDose`.

- `v_rule_design_ordinal()`: validates that the
  [`RuleDesignOrdinal`](https://openpharma.github.io/crmPack/reference/RuleDesignOrdinal-class.md)
  object contains valid `starting_dose`.

- `v_design_grouped()`: validates that the
  [`DesignGrouped`](https://openpharma.github.io/crmPack/reference/DesignGrouped-class.md)
  object contains valid flags.
