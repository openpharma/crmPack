# Internal Helper Functions for Validation of [`McmcOptions`](https://openpharma.github.io/crmPack/reference/McmcOptions-class.md) Objects

**\[stable\]**

These functions are only used internally to validate the format of an
input
[`McmcOptions`](https://openpharma.github.io/crmPack/reference/McmcOptions-class.md)
or inherited classes and therefore not exported.

## Usage

``` r
v_mcmc_options(object)
```

## Arguments

- object:

  (`McmcOptions`)  
  object to validate.

## Value

A `character` vector with the validation failure messages, or `TRUE` in
case validation passes.

## Functions

- `v_mcmc_options()`: validates that the
  [`McmcOptions`](https://openpharma.github.io/crmPack/reference/McmcOptions-class.md)
  object contains valid integer scalars `iterations`, `burnin` and
  `step` as well as proper parameters for Random Number Generator.
