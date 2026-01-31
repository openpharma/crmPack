# Internal Helper Functions for Validation of [`Backfill`](https://openpharma.github.io/crmPack/reference/Backfill-class.md) Objects

**\[experimental\]**

These functions are only used internally to validate the format of an
input
[`Backfill`](https://openpharma.github.io/crmPack/reference/Backfill-class.md)
object and therefore not exported.

## Usage

``` r
v_backfill(object)
```

## Arguments

- object:

  (`Backfill`)  
  object to validate.

## Value

A `character` vector with the validation failure messages, or `TRUE` in
case validation passes.

## Functions

- `v_backfill()`: validates that the
  [`Backfill`](https://openpharma.github.io/crmPack/reference/Backfill-class.md)
  object contains valid slots.
