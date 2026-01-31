# `OpeningList`

**\[experimental\]**

`OpeningList` is a virtual class for combining multiple
[`Opening`](https://openpharma.github.io/crmPack/reference/Opening-class.md)
objects using logical operators. It is used as a base class for
[`OpeningAll`](https://openpharma.github.io/crmPack/reference/OpeningAll-class.md)
and
[`OpeningAny`](https://openpharma.github.io/crmPack/reference/OpeningAny-class.md).

## Usage

``` r
OpeningList(...)

.DefaultOpeningList()
```

## Arguments

- ...:

  (`Opening`)  
  opening objects to combine.

## Slots

- `open_list`:

  (`list`) a list of
  [`Opening`](https://openpharma.github.io/crmPack/reference/Opening-class.md)
  objects to be combined.

## Note

Typically, end users will not use the `.DefaultOpeningList()` function.

## See also

[`Opening`](https://openpharma.github.io/crmPack/reference/Opening-class.md),
[`OpeningAll`](https://openpharma.github.io/crmPack/reference/OpeningAll-class.md),
[`OpeningAny`](https://openpharma.github.io/crmPack/reference/OpeningAny-class.md).

## Examples

``` r
# Create two simple opening criteria
opening1 <- OpeningMinDose(min_dose = 10)
opening2 <- OpeningMinCohorts(min_cohorts = 3)

# Create an OpeningList that combines them
opening_list <- OpeningList(opening1, opening2)
print(opening_list)
#> An object of class "OpeningList"
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

# You can also create with more than two criteria
opening3 <- OpeningNone()
opening_list_multi <- OpeningList(opening1, opening2, opening3)
print(opening_list_multi)
#> An object of class "OpeningList"
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
#> [[3]]
#> An object of class "OpeningNone"
#> <S4 Type Object>
#> 
#> 
```
