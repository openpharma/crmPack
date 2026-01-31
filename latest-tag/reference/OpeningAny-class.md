# `OpeningAny`

**\[experimental\]**

`OpeningAny` combines multiple
[`Opening`](https://openpharma.github.io/crmPack/reference/Opening-class.md)
objects using OR logic. A backfill cohort is opened if ANY opening
criteria in the list are satisfied. This can also be created using the
`|` operator.

## Usage

``` r
OpeningAny(...)

.DefaultOpeningAny()
```

## Arguments

- ...:

  (`Opening`) opening objects to combine with OR logic.

## Slots

- `open_list`:

  (`list`)  
  a list of
  [`Opening`](https://openpharma.github.io/crmPack/reference/Opening-class.md)
  objects to be combined with OR logic.

## Note

Typically, end users will not use the `.DefaultOpeningAny()` function.

## See also

[`Opening`](https://openpharma.github.io/crmPack/reference/Opening-class.md),
[`OpeningAll`](https://openpharma.github.io/crmPack/reference/OpeningAll-class.md),
[`OpeningList`](https://openpharma.github.io/crmPack/reference/OpeningList-class.md).

## Examples

``` r
# Create two opening criteria
opening1 <- OpeningMinDose(min_dose = 10)
opening2 <- OpeningMinCohorts(min_cohorts = 3)

# Combine them with OR logic: at least one must be satisfied
opening_any <- OpeningAny(opening1, opening2)
print(opening_any)
#> An object of class "OpeningAny"
#> Slot "open_list":
#> [[1]]
#> An object of class "OpeningMinDose"
#> Slot "min_dose":
#> [1] 10
#> 
#> 
#> [[2]]
#> An object of class "OpeningMinCohorts"
#> Slot "min_cohorts":
#> [1] 3
#> 
#> 
#> 

# Alternative: use the | operator
opening_any_alt <- opening1 | opening2
print(opening_any_alt)
#> An object of class "OpeningAny"
#> Slot "open_list":
#> [[1]]
#> An object of class "OpeningMinDose"
#> Slot "min_dose":
#> [1] 10
#> 
#> 
#> [[2]]
#> An object of class "OpeningMinCohorts"
#> Slot "min_cohorts":
#> [1] 3
#> 
#> 
#> 
```
