# `RecruitmentRatio`

**\[experimental\]**

`RecruitmentRatio` constrains the recruitment of backfill patients based
on a ratio to the main trial cohort size. The maximum number of backfill
patients is calculated as `ceiling(ratio * active_cohort_size)`.

## Usage

``` r
RecruitmentRatio(ratio = 1)

.DefaultRecruitmentRatio()
```

## Arguments

- ratio:

  (`number`) see slot definition.

## Slots

- `ratio`:

  (`number`) the recruitment ratio, specifying the maximum number of
  backfill patients per patient in the main trial cohort (non-negative).

## Note

Typically, end users will not use the `.DefaultRecruitmentRatio()`
function.

## See also

[`Recruitment`](https://openpharma.github.io/crmPack/reference/Recruitment-class.md)
and the other subclasses listed in there.

## Examples

``` r
# Create a RecruitmentRatio object with ratio 0.5
# This means 1 backfill patient for every 2 patients in the active cohort
recruitment <- RecruitmentRatio(ratio = 0.5)
print(recruitment)
#> An object of class "RecruitmentRatio"
#> Slot "ratio":
#> [1] 0.5
#> 

# Create a variant with ratio 1 (1:1)
recruitment_one_to_one <- RecruitmentRatio(ratio = 1)
print(recruitment_one_to_one)
#> An object of class "RecruitmentRatio"
#> Slot "ratio":
#> [1] 1
#> 
```
