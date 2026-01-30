# Calculate Maximum Number of Backfill Patients

**\[experimental\]**

Calculates the maximum number of backfill patients that can be recruited
based on the recruitment rule and the active cohort size.

## Usage

``` r
maxRecruits(object, active_cohort_size, ...)

# S4 method for class 'RecruitmentUnlimited'
maxRecruits(object, active_cohort_size, ...)

# S4 method for class 'RecruitmentRatio'
maxRecruits(object, active_cohort_size, ...)
```

## Arguments

- object:

  (`Recruitment`)  
  the recruitment rule.

- active_cohort_size:

  (`integer`)  
  the current size of the active dose escalation cohort.

- ...:

  further arguments (not used).

## Value

The maximum number of backfill patients as an `integer`.

## Functions

- `maxRecruits(RecruitmentUnlimited)`: method for `RecruitmentUnlimited`
  class. Returns a very large number (practically unlimited).

- `maxRecruits(RecruitmentRatio)`: method for `RecruitmentRatio` class.
  Returns `ceiling(ratio * active_cohort_size)`.

## See also

[`Recruitment`](https://openpharma.github.io/crmPack/reference/Recruitment-class.md),
[`RecruitmentUnlimited`](https://openpharma.github.io/crmPack/reference/RecruitmentUnlimited-class.md),
[`RecruitmentRatio`](https://openpharma.github.io/crmPack/reference/RecruitmentRatio-class.md).

## Examples

``` r
# Create a RecruitmentUnlimited object
recruitment <- RecruitmentUnlimited()

# Calculate maximum recruits for various active cohort sizes
max_recruits_10 <- maxRecruits(recruitment, active_cohort_size = 10)
print(max_recruits_10) # Returns 1e6
#> [1] 1000000

max_recruits_100 <- maxRecruits(recruitment, active_cohort_size = 100)
print(max_recruits_100) # Still returns 1e6 (unlimited)
#> [1] 1000000

# With RecruitmentUnlimited, the active_cohort_size is ignored
# Create a RecruitmentRatio object with ratio 0.5
recruitment <- RecruitmentRatio(ratio = 0.5)

# Calculate maximum recruits based on active cohort size
# For active cohort of 10: ceiling(0.5 * 10) = 5
max_recruits_10 <- maxRecruits(recruitment, active_cohort_size = 10)
print(max_recruits_10) # 5
#> [1] 5

# For active cohort of 7: ceiling(0.5 * 7) = ceiling(3.5) = 4
max_recruits_7 <- maxRecruits(recruitment, active_cohort_size = 7)
print(max_recruits_7) # 4
#> [1] 4

# For active cohort of 15: ceiling(0.5 * 15) = ceiling(7.5) = 8
max_recruits_15 <- maxRecruits(recruitment, active_cohort_size = 15)
print(max_recruits_15) # 8
#> [1] 8
```
