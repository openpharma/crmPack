# Internal Helper Functions for Validation of [`Increments`](https://openpharma.github.io/crmPack/reference/Increments-class.md) Objects

**\[stable\]**

These functions are only used internally to validate the format of an
input
[`Increments`](https://openpharma.github.io/crmPack/reference/Increments-class.md)
or inherited classes and therefore not exported.

## Usage

``` r
v_increments_relative(object)

v_increments_relative_parts(object)

v_increments_relative_dlt(object)

v_increments_dose_levels(object)

v_increments_hsr_beta(object)

v_increments_min(object)

v_increments_maxtoxprob(object)

v_increments_ordinal(object)

v_cohort_size_ordinal(object)
```

## Arguments

- object:

  (`Increments`)  
  object to validate.

## Value

A `character` vector with the validation failure messages, or `TRUE` in
case validation passes.

## Functions

- `v_increments_relative()`: validates that the
  [`IncrementsRelative`](https://openpharma.github.io/crmPack/reference/IncrementsRelative-class.md)
  object contains valid `intervals` and `increments` parameters.

- `v_increments_relative_parts()`: validates that the
  [`IncrementsRelativeParts`](https://openpharma.github.io/crmPack/reference/IncrementsRelativeParts-class.md)
  object contains valid `dlt_start` and `clean_start` parameters.

- `v_increments_relative_dlt()`: validates that the
  [`IncrementsRelativeDLT`](https://openpharma.github.io/crmPack/reference/IncrementsRelativeDLT-class.md)
  object contains valid `intervals` and `increments` parameters.

- `v_increments_dose_levels()`: validates that the
  [`IncrementsDoseLevels`](https://openpharma.github.io/crmPack/reference/IncrementsDoseLevels-class.md)
  object contains valid `levels` and `basis_level` option.

- `v_increments_hsr_beta()`: validates that the
  [`IncrementsHSRBeta`](https://openpharma.github.io/crmPack/reference/IncrementsHSRBeta-class.md)
  object contains valid probability target, threshold and shape
  parameters.

- `v_increments_min()`: validates that the
  [`IncrementsMin`](https://openpharma.github.io/crmPack/reference/IncrementsMin-class.md)
  object contains a list with `Increments` objects.

- `v_increments_maxtoxprob()`: validates the
  [`IncrementsMaxToxProb`](https://openpharma.github.io/crmPack/reference/IncrementsMaxToxProb-class.md)

- `v_increments_ordinal()`: validates that the
  [`IncrementsOrdinal`](https://openpharma.github.io/crmPack/reference/IncrementsOrdinal-class.md)
  object contains valid `grade` and standard `Increments` rule.

- `v_cohort_size_ordinal()`: validates that the
  [`CohortSizeOrdinal`](https://openpharma.github.io/crmPack/reference/CohortSizeOrdinal-class.md)
  object contains valid `grade` and standard `CohortSize` rule.
