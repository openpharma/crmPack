# Internal Helper Functions for Validation of [`Samples`](https://openpharma.github.io/crmPack/reference/Samples-class.md) Objects

These functions are only used internally to validate the format of an
input
[`Samples`](https://openpharma.github.io/crmPack/reference/Samples-class.md)
or inherited classes and therefore not exported.

## Usage

``` r
v_samples(object)
```

## Arguments

- object:

  (`Samples`)  
  object to validate.

## Value

A `character` vector with the validation failure messages, or `TRUE` in
case validation passes.

## Functions

- `v_samples()`: validates that the
  [`Samples`](https://openpharma.github.io/crmPack/reference/Samples-class.md)
  object contains valid `data` slot.
