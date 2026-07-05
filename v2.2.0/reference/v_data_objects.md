# Internal Helper Functions for Validation of [`GeneralData`](https://docs.crmpack.org/reference/GeneralData-class.md) Objects

**\[stable\]**

These functions are only used internally to validate the format of an
input
[`GeneralData`](https://docs.crmpack.org/reference/GeneralData-class.md)
or inherited classes and therefore not exported.

## Usage

``` r
v_general_data(object)

h_doses_unique_per_cohort(dose, cohort)

h_combo_doses_unique_per_cohort(x, cohort)

v_data(object)

v_data_dual(object)

v_data_parts(object)

v_data_mixture(object)

v_data_da(object)

v_data_ordinal(object)

v_data_grouped(object)

v_data_combo(object)

v_hierarchical_data(object)
```

## Arguments

- object:

  (`GeneralData`)\
  object to validate.

- dose:

  (`numeric`)\
  dose values.

- cohort:

  (`integer`)

- x:

  (`matrix`)

## Value

A `character` vector with the validation failure messages, or `TRUE` in
case validation passes.

`TRUE` if `dose` is unique per `cohort`, otherwise `FALSE`.

`TRUE` if `x` is unique per `cohort`, otherwise `FALSE`.

## Functions

- `v_general_data()`: validates that the
  [`GeneralData`](https://docs.crmpack.org/reference/GeneralData-class.md)
  object contains unique `ID`, non-negative `cohort` indices and `ID`
  and `cohort` vectors are of the same length `nObs`.

- `h_doses_unique_per_cohort()`: helper function which verifies whether
  the `dose` values are unique in each and every different `cohort`.

- `h_combo_doses_unique_per_cohort()`: helper function which verifies
  whether the two-drug dose combination is unique in each cohort.

- `v_data()`: validates that the
  [`Data`](https://docs.crmpack.org/reference/Data-class.md) object
  contains valid elements with respect to their types, dependency and
  length.

- `v_data_dual()`: validates that the
  [`DataDual`](https://docs.crmpack.org/reference/DataDual-class.md)
  object contains valid biomarker vector with respect to its type and
  the length.

- `v_data_parts()`: validates that the
  [`DataParts`](https://docs.crmpack.org/reference/DataParts-class.md)
  object contains valid elements with respect to their types, dependency
  and length.

- `v_data_mixture()`: validates that the
  [`DataMixture`](https://docs.crmpack.org/reference/DataMixture-class.md)
  object contains valid elements with respect to their types, dependency
  and length.

- `v_data_da()`: validates that the
  [`DataDA`](https://docs.crmpack.org/reference/DataDA-class.md) object
  contains valid elements with respect to their types, dependency and
  length.

- `v_data_ordinal()`: validates that the
  [`DataOrdinal`](https://docs.crmpack.org/reference/DataOrdinal-class.md)
  object contains valid elements with respect to their types, dependency
  and length.

- `v_data_grouped()`: validates that the
  [`DataGrouped`](https://docs.crmpack.org/reference/DataGrouped-class.md)
  object contains valid group information.

- `v_data_combo()`: validates that the
  [`DataCombo`](https://docs.crmpack.org/reference/DataCombo-class.md)
  object contains valid two-drug combination data.

- `v_hierarchical_data()`: validates that the
  [`HierarchicalData`](https://docs.crmpack.org/reference/HierarchicalData-class.md)
  object contains valid arms.
