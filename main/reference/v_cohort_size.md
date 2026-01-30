# Internal Helper Functions for Validation of [`CohortSize`](https://openpharma.github.io/crmPack/reference/CohortSize-class.md) Objects

**\[stable\]**

These functions are only used internally to validate the format of an
input
[`CohortSize`](https://openpharma.github.io/crmPack/reference/CohortSize-class.md)
or inherited classes and therefore not exported.

## Usage

``` r
v_cohort_size_range(object)

v_cohort_size_dlt(object)

v_cohort_size_const(object)

v_cohort_size_random(object)

v_cohort_size_parts(object)

v_cohort_size_max(object)
```

## Arguments

- object:

  (`CohortSize`)  
  object to validate.

## Value

A `character` vector with the validation failure messages, or `TRUE` in
case validation passes.

## Functions

- `v_cohort_size_range()`: validates that the
  [`CohortSizeRange`](https://openpharma.github.io/crmPack/reference/CohortSizeRange-class.md)
  object contains valid `intervals` and `cohort_size` slots.

- `v_cohort_size_dlt()`: validates that the
  [`CohortSizeDLT`](https://openpharma.github.io/crmPack/reference/CohortSizeDLT-class.md)
  object contains valid `intervals` and `cohort_size` slots.

- `v_cohort_size_const()`: validates that the
  [`CohortSizeConst`](https://openpharma.github.io/crmPack/reference/CohortSizeConst-class.md)
  object contains valid `size` slot.

- `v_cohort_size_random()`: validates that the
  [`CohortSizeRandom`](https://openpharma.github.io/crmPack/reference/CohortSizeRandom-class.md)
  object contains valid `min_size` and `max_size` slots.

- `v_cohort_size_parts()`: validates that the
  [`CohortSizeParts`](https://openpharma.github.io/crmPack/reference/CohortSizeParts-class.md)
  object contains valid `sizes` slot.

- `v_cohort_size_max()`: validates that the
  [`CohortSizeMax`](https://openpharma.github.io/crmPack/reference/CohortSizeMax-class.md)
  object contains valid `cohort_sizes` slot.
