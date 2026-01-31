# Internal Helper Functions for Validation of [`Recruitment`](https://openpharma.github.io/crmPack/reference/Recruitment-class.md) Objects

**\[experimental\]**

These functions are only used internally to validate the format of an
input
[`Recruitment`](https://openpharma.github.io/crmPack/reference/Recruitment-class.md)
or inherited classes and therefore not exported.

## Usage

``` r
v_recruitment_ratio(object)
```

## Arguments

- object:

  (`Recruitment`) object to validate.

## Value

A `character` vector with the validation failure messages, or `TRUE` in
case validation passes.

## Functions

- `v_recruitment_ratio()`: validates that the
  [`RecruitmentRatio`](https://openpharma.github.io/crmPack/reference/RecruitmentRatio-class.md)
  object contains valid `ratio` slot.
