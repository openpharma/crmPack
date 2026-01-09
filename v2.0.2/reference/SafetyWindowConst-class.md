# `SafetyWindowConst`

**\[stable\]**

`SafetyWindowConst` is the class for safety window length and it is used
when the `gap` should be kept constant across cohorts (though it may
vary within a cohort).

## Usage

``` r
SafetyWindowConst(gap, follow, follow_min)

.DefaultSafetyWindowConst()
```

## Arguments

- gap:

  see slot definition.

- follow:

  see slot definition.

- follow_min:

  see slot definition.

## Slots

- `gap`:

  (`integer`)  
  a vector, the constant gap between patients.

- `follow`:

  (`count`)  
  how long to follow each patient. The period of time that each patient
  in the cohort needs to be followed before the next cohort opens.

- `follow_min`:

  (`count`)  
  minimum follow up. At least one patient in the cohort needs to be
  followed at the minimal follow up time.

## Note

Typically, end users will not use the `.DefaultSafetyWindowConst()`
function.

## Examples

``` r
# This is to have along the study constant parameters settings of safety window
# length, regardless of the cohort size.
my_win_len <- SafetyWindowConst(
  gap = c(7, 5, 3),
  follow = 7,
  follow_min = 14
)
```
