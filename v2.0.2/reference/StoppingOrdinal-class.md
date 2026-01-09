# `StoppingOrdinal`

**\[experimental\]**

`StoppingOrdinal` is the class for stopping based on a Stopping rule
applied to a specific toxicity grade in an ordinal CRM trial

## Usage

``` r
StoppingOrdinal(grade, rule)

.DefaultStoppingOrdinal()
```

## Arguments

- grade:

  (`integer`)  
  see slot definition.

- rule:

  (`Stopping`)  
  see slot definition.

## Slots

- `grade`:

  (`integer`)  
  the grade to which the rule should be applied

- `rule`:

  (`Stopping`)  
  the rule to apply

## Note

Typically, end users will not use the `.DefaultStoppingOrdinal()`
function.

## Examples

``` r
StoppingOrdinal(
  1L,
  StoppingTargetProb(target = c(0.2, 0.35), prob = 0.6)
)
#> An object of class "StoppingOrdinal"
#> Slot "grade":
#> [1] 1
#> 
#> Slot "rule":
#> An object of class "StoppingTargetProb"
#> Slot "target":
#> [1] 0.20 0.35
#> 
#> Slot "prob":
#> [1] 0.6
#> 
#> Slot "report_label":
#> [1] "P(0.2 ≤ prob(DLE | NBD) ≤ 0.35) ≥ 0.6"
#> 
#> 
#> Slot "report_label":
#> character(0)
#> 
```
