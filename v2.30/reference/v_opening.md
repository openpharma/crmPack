# Internal Helper Functions for Validation of [`Opening`](https://docs.crmpack.org/reference/Opening-class.md) Objects

**\[experimental\]**

These functions are only used internally to validate the format of an
input [`Opening`](https://docs.crmpack.org/reference/Opening-class.md)
or inherited classes and therefore not exported.

## Usage

``` r
v_opening_min_dose(object)

v_opening_min_cohorts(object)

v_opening_min_responses(object)
```

## Arguments

- object:

  (`Opening`)\
  object to validate.

## Value

A `character` vector with the validation failure messages, or `TRUE` in
case validation passes.

## Functions

- `v_opening_min_dose()`: validates that the
  [`OpeningMinDose`](https://docs.crmpack.org/reference/OpeningMinDose-class.md)
  object contains valid `min_dose` slot.

- `v_opening_min_cohorts()`: validates that the
  [`OpeningMinCohorts`](https://docs.crmpack.org/reference/OpeningMinCohorts-class.md)
  object contains valid `min_cohorts` slot.

- `v_opening_min_responses()`: validates that the
  [`OpeningMinResponses`](https://docs.crmpack.org/reference/OpeningMinResponses-class.md)
  object contains valid `min_responses` and `include_lower_doses` slots.
