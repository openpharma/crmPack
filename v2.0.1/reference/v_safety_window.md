# Internal Helper Functions for Validation of [`SafetyWindow`](https://openpharma.github.io/crmPack/reference/SafetyWindow-class.md) Objects

**\[stable\]**

These functions are only used internally to validate the format of an
input
[`SafetyWindow`](https://openpharma.github.io/crmPack/reference/SafetyWindow-class.md)
or inherited classes and therefore not exported.

## Usage

``` r
v_safety_window_size(object)

v_safety_window_const(object)
```

## Arguments

- object:

  (`SafetyWindow`)  
  object to validate.

## Value

A `character` vector with the validation failure messages, or `TRUE` in
case validation passes.

## Functions

- `v_safety_window_size()`: validates that the
  [`SafetyWindowSize`](https://openpharma.github.io/crmPack/reference/SafetyWindowSize-class.md)
  object contains valid slots.

- `v_safety_window_const()`: validates that the
  [`SafetyWindowConst`](https://openpharma.github.io/crmPack/reference/SafetyWindowConst-class.md)
  object contains valid slots.
