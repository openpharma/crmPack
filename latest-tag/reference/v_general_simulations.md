# Internal Helper Functions for Validation of [`GeneralSimulations`](https://openpharma.github.io/crmPack/reference/GeneralSimulations-class.md) Objects

**\[stable\]**

These functions are only used internally to validate the format of an
input
[`GeneralSimulations`](https://openpharma.github.io/crmPack/reference/GeneralSimulations-class.md)
or inherited classes and therefore not exported.

## Usage

``` r
v_general_simulations(object)

v_simulations(object)

v_dual_simulations(object)

v_da_simulations(object)
```

## Arguments

- object:

  (`GeneralSimulations`)  
  object to validate.

## Value

A `character` vector with the validation failure messages, or `TRUE` in
case validation passes.

## Functions

- `v_general_simulations()`: validates that the
  [`GeneralSimulations`](https://openpharma.github.io/crmPack/reference/GeneralSimulations-class.md)
  object contains valid `data` object and valid `dose` simulations.

- `v_simulations()`: validates that the
  [`Simulations`](https://openpharma.github.io/crmPack/reference/Simulations-class.md)
  object contains valid object `fit`, `stop_reasons`, `stop_report`, and
  `additional_stats` compared to the general class
  [`GeneralSimulations`](https://openpharma.github.io/crmPack/reference/GeneralSimulations-class.md).

- `v_dual_simulations()`: validates that the
  [`DualSimulations`](https://openpharma.github.io/crmPack/reference/DualSimulations-class.md)
  object and capture the dose-biomarker `fits`, and the `sigma2W` and
  `rho` estimates.

- `v_da_simulations()`: validates that the
  [`DASimulations`](https://openpharma.github.io/crmPack/reference/DASimulations-class.md)
  object contains valid `trial_duration` the vector of trial duration
  values for all simulations.
