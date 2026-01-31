# `OpeningAll`

**\[experimental\]**

`OpeningAll` combines multiple
[`Opening`](https://openpharma.github.io/crmPack/reference/Opening-class.md)
objects using AND logic. A backfill cohort is opened only if ALL opening
criteria in the list are satisfied. This can also be created using the
`&` operator.

## Usage

``` r
OpeningAll(...)

.DefaultOpeningAll()
```

## Arguments

- ...:

  (`Opening`) opening objects to combine with AND logic.

## Slots

- `open_list`:

  (`list`) a list of
  [`Opening`](https://openpharma.github.io/crmPack/reference/Opening-class.md)
  objects to be combined with AND logic.

## Note

Typically, end users will not use the `.DefaultOpeningAll()` function.

## See also

[`Opening`](https://openpharma.github.io/crmPack/reference/Opening-class.md),
[`OpeningAny`](https://openpharma.github.io/crmPack/reference/OpeningAny-class.md),
[`OpeningList`](https://openpharma.github.io/crmPack/reference/OpeningList-class.md).

## Examples

``` r
# Create two opening criteria
opening1 <- OpeningMinDose(min_dose = 10)
opening2 <- OpeningMinCohorts(min_cohorts = 3)

# Combine them with AND logic: both must be satisfied
opening_all <- OpeningAll(opening1, opening2)
print(opening_all)
#> An object of class "OpeningAll"
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

# Alternative: use the & operator
opening_all_alt <- opening1 & opening2
print(opening_all_alt)
#> An object of class "OpeningAll"
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
