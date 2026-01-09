# `CohortSizeOrdinal`

**\[experimental\]**

`CohortSizeOrdinal` is the class for cohort size for an ordinal CRM
trial.

## Usage

``` r
CohortSizeOrdinal(grade, rule)

.DefaultCohortSizeOrdinal()
```

## Arguments

- grade:

  (`integer`)  
  see slot definition.

- rule:

  (`CohortSize`)  
  see slot definition.

## Slots

- `grade`:

  (`integer`)  
  the grade at which the rule should be applied

- `rule`:

  (`CohortSize`)  
  the `CohortSize` rule to apply.

## Note

Typically, end users will not use the `.DefaultCohortSizeOrdinal()`
function.

## Examples

``` r
CohortSizeOrdinal(
  grade = 1L,
  rule = CohortSizeRange(intervals = c(0, 30), cohort_size = c(1L, 3L))
)
#> An object of class "CohortSizeOrdinal"
#> Slot "grade":
#> [1] 1
#> 
#> Slot "rule":
#> An object of class "CohortSizeRange"
#> Slot "intervals":
#> [1]  0 30
#> 
#> Slot "cohort_size":
#> [1] 1 3
#> 
#> 
```
