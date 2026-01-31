# Internal Helper Functions for Validation of Model Parameters Objects

**\[experimental\]**

These functions are only used internally to validate the format of an
object with model parameters or inherited classes and therefore not
exported.

## Usage

``` r
v_model_params_normal(object)
```

## Arguments

- object:

  (`ModelParamsNormal`)  
  multivariate normal parameters object to validate.

## Value

A `character` vector with the validation failure messages, or `TRUE` in
case validation passes.

## Functions

- `v_model_params_normal()`: a helper function that validates
  multivariate normal parameters.
