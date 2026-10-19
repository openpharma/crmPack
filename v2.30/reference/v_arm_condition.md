# Internal Helper Functions for Validation of [`ArmCondition`](https://docs.crmpack.org/reference/ArmCondition-class.md) Objects

**\[experimental\]**

These functions are only used internally to validate the format of an
input
[`ArmCondition`](https://docs.crmpack.org/reference/ArmCondition-class.md)
or inherited classes and therefore not exported.

## Usage

``` r
v_arm_finished_condition(object)

v_arm_min_dose_condition(object)
```

## Arguments

- object:

  (`ArmCondition`)\
  object to validate.

## Value

A `character` vector with the validation failure messages, or `TRUE` in
case validation passes.

## Functions

- `v_arm_finished_condition()`: validates that the
  [`ArmFinishedCondition`](https://docs.crmpack.org/reference/ArmFinishedCondition-class.md)
  object contains a valid `arm_name` slot.

- `v_arm_min_dose_condition()`: validates that the
  [`ArmMinDoseCondition`](https://docs.crmpack.org/reference/ArmMinDoseCondition-class.md)
  object contains valid `arm_name` and `min_dose` slots.
