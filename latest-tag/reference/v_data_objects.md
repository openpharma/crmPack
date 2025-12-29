# Internal Helper Functions for Validation of [`GeneralData`](https://openpharma.github.io/crmPack/reference/GeneralData-class.md) Objects

**\[stable\]**

These functions are only used internally to validate the format of an
input
[`GeneralData`](https://openpharma.github.io/crmPack/reference/GeneralData-class.md)
or inherited classes and therefore not exported.

## Usage

``` r
v_general_data(object)

h_doses_unique_per_cohort(dose, cohort)

v_data(object)

v_data_dual(object)

v_data_parts(object)

v_data_mixture(object)

v_data_da(object)

v_data_ordinal(object)

v_data_grouped(object)
```

## Arguments

- object:

  (`GeneralData`)  
  object to validate.

- dose:

  (`numeric`)  
  dose values.

- cohort:

  (`integer`)  
  cohort indices parallel to `doses`.

## Value

A `character` vector with the validation failure messages, or `TRUE` in
case validation passes.

`TRUE` if `dose` is unique per `cohort`, otherwise `FALSE`.

## Functions

- `v_general_data()`: validates that the
  [`GeneralData`](https://openpharma.github.io/crmPack/reference/GeneralData-class.md)
  object contains unique `ID`, non-negative `cohort` indices and `ID`
  and `cohort` vectors are of the same length `nObs`.

- `h_doses_unique_per_cohort()`: helper function which verifies whether
  the `dose` values are unique in each and every different `cohort`.

- `v_data()`: validates that the
  [`Data`](https://openpharma.github.io/crmPack/reference/Data-class.md)
  object contains valid elements with respect to their types, dependency
  and length.

- `v_data_dual()`: validates that the
  [`DataDual`](https://openpharma.github.io/crmPack/reference/DataDual-class.md)
  object contains valid biomarker vector with respect to its type and
  the length.

- `v_data_parts()`: validates that the
  [`DataParts`](https://openpharma.github.io/crmPack/reference/DataParts-class.md)
  object contains valid elements with respect to their types, dependency
  and length.

- `v_data_mixture()`: validates that the
  [`DataMixture`](https://openpharma.github.io/crmPack/reference/DataMixture-class.md)
  object contains valid elements with respect to their types, dependency
  and length.

- `v_data_da()`: validates that the
  [`DataDA`](https://openpharma.github.io/crmPack/reference/DataDA-class.md)
  object contains valid elements with respect to their types, dependency
  and length.

- `v_data_ordinal()`: validates that the
  [`DataOrdinal`](https://openpharma.github.io/crmPack/reference/DataOrdinal-class.md)
  object contains valid elements with respect to their types, dependency
  and length.

- `v_data_grouped()`: validates that the
  [`DataGrouped`](https://openpharma.github.io/crmPack/reference/DataGrouped-class.md)
  object contains valid group information.
