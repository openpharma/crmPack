# `OpeningMinDose`

**\[experimental\]**

`OpeningMinDose` opens a backfill cohort when the cohort's dose is above
or equal to the minimum dose specified. Note that the next recommended
dose is not taken into account.

## Usage

``` r
OpeningMinDose(min_dose = 0)

.DefaultOpeningMinDose()
```

## Arguments

- min_dose:

  (`number`)  
  see slot definition.

## Slots

- `min_dose`:

  (`number`)  
  the minimum dose at which backfill cohorts can be opened.

## Note

Typically, end users will not use the `.DefaultOpeningMinDose()`
function.

## See also

[`Opening`](https://openpharma.github.io/crmPack/reference/Opening-class.md)
and the other subclasses listed in there.

## Examples

``` r
# Opening backfill cohort when dose is at least 50
my_opening <- OpeningMinDose(min_dose = 50)
```
