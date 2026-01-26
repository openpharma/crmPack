# `RuleDesignOrdinal`

**\[experimental\]**

`RuleDesignOrdinal` is the class for rule-based designs. The difference
between this class and the
[`DesignOrdinal`](https://openpharma.github.io/crmPack/reference/DesignOrdinal-class.md)
class is that `RuleDesignOrdinal` does not contain `model`, `stopping`
and `increments` slots.

## Usage

``` r
RuleDesignOrdinal(next_best, cohort_size, data, starting_dose)

.DefaultRuleDesignOrdinal()
```

## Arguments

- next_best:

  (`NextBestOrdinal`)  
  see slot definition.

- cohort_size:

  (`CohortSize`)  
  see slot definition.

- data:

  (`DataOrdinal`)  
  see slot definition.

- starting_dose:

  (`number`)  
  see slot definition.

## Details

Please note that the cohort size rules need to be wrapped into the
corresponding
[CohortSizeOrdinal](https://openpharma.github.io/crmPack/reference/CohortSizeOrdinal-class.md)
class, before a successful evaluation of the corresponding methods can
take place. Note also that these wrappers cannot be nested, i.e., you
cannot have a
[CohortSizeOrdinal](https://openpharma.github.io/crmPack/reference/CohortSizeOrdinal-class.md)
inside another
[CohortSizeOrdinal](https://openpharma.github.io/crmPack/reference/CohortSizeOrdinal-class.md)
(which also would not make sense) because it would not be clear which
event grade to use for the methods calculation. However, multiple rules
can be combined using the operators defined, e.g.,
`CohortSizeMin(list(CohortSizeOrdinal(1L, rule1), CohortSizeOrdinal(2L, rule2)))`.

## Slots

- `next_best`:

  (`NextBestOrdinal`)  
  how to find the next best dose.

- `cohort_size`:

  (`CohortSize`)  
  rules for the cohort sizes.

- `data`:

  (`DataOrdinal`)  
  specifies dose grid, any previous data, etc.

- `starting_dose`:

  (`number`)  
  the starting dose, it must lie on the dose grid in `data`.

## Note

Typically, end users will not use the `.DefaultRuleDesignOrdinal()`
function.

## Examples

``` r
RuleDesignOrdinal(
  next_best = NextBestOrdinal(
    1L,
    NextBestMTD(
      target = 0.25,
      derive = function(x) median(x, na.rm = TRUE)
    )
  ),
  cohort_size = CohortSizeOrdinal(1L, CohortSizeConst(size = 3L)),
  data = DataOrdinal(doseGrid = c(5, 10, 15, 25, 35, 50, 80)),
  starting_dose = 5
)
#> An object of class "RuleDesignOrdinal"
#> Slot "next_best":
#> An object of class "NextBestOrdinal"
#> Slot "grade":
#> [1] 1
#> 
#> Slot "rule":
#> An object of class "NextBestMTD"
#> Slot "target":
#> [1] 0.25
#> 
#> Slot "derive":
#> function (x) 
#> median(x, na.rm = TRUE)
#> <environment: 0x55a6783fb7d0>
#> 
#> 
#> 
#> Slot "cohort_size":
#> An object of class "CohortSizeOrdinal"
#> Slot "grade":
#> [1] 1
#> 
#> Slot "rule":
#> An object of class "CohortSizeConst"
#> Slot "size":
#> [1] 3
#> 
#> 
#> 
#> Slot "data":
#> An object of class "DataOrdinal"
#> Slot "x":
#> numeric(0)
#> 
#> Slot "y":
#> integer(0)
#> 
#> Slot "doseGrid":
#> [1]  5 10 15 25 35 50 80
#> 
#> Slot "nGrid":
#> [1] 7
#> 
#> Slot "xLevel":
#> integer(0)
#> 
#> Slot "yCategories":
#> No DLT    DLT 
#>      0      1 
#> 
#> Slot "placebo":
#> [1] FALSE
#> 
#> Slot "ID":
#> integer(0)
#> 
#> Slot "cohort":
#> integer(0)
#> 
#> Slot "nObs":
#> [1] 0
#> 
#> 
#> Slot "starting_dose":
#> [1] 5
#> 
```
