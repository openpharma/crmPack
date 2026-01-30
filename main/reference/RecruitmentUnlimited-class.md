# `RecruitmentUnlimited`

**\[experimental\]**

`RecruitmentUnlimited` allows unlimited recruitment of backfill
patients. There is no constraint on the number of backfill patients
relative to the main trial cohort size.

## Usage

``` r
RecruitmentUnlimited()

.DefaultRecruitmentUnlimited()
```

## Note

Typically, end users will not use the `.DefaultRecruitmentUnlimited()`
function.

## See also

[`Recruitment`](https://openpharma.github.io/crmPack/reference/Recruitment-class.md)
and the other subclasses listed in there.

## Examples

``` r
# Create a RecruitmentUnlimited object
recruitment <- RecruitmentUnlimited()
print(recruitment)
#> An object of class "RecruitmentUnlimited"
#> <S4 Type Object>

# Calculate maximum recruits for an active cohort of size 10
max_recruits <- maxRecruits(recruitment, active_cohort_size = 10)
print(max_recruits) # Practically unlimited (1e6)
#> [1] 1000000
```
