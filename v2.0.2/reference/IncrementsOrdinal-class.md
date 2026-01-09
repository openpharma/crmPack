# `IncrementsOrdinal`

**\[experimental\]**

`IncrementsOrdinal` is the class for applying a standard `Increments`
rule to the results of an ordinal CRM trial.

## Usage

``` r
IncrementsOrdinal(grade, rule)

.DefaultIncrementsOrdinal()
```

## Arguments

- grade:

  (`numeric`)  
  see slot definition.

- rule:

  (`Increments`)  
  see slot definition.

## Slots

- `grade`:

  (`integer`)  
  the toxicity grade to which the `rule` should be applied.

- `rule`:

  (`Increments`)  
  the standard `Increments` rule to be applied

## Note

Typically, end users will not use the `.DefaultIncrementsOrdinal()`
function.

## Examples

``` r
IncrementsOrdinal(
  grade = 1L,
  rule = IncrementsRelative(
    intervals = c(0, 20),
    increments = c(1, 0.33)
  )
)
#> An object of class "IncrementsOrdinal"
#> Slot "grade":
#> [1] 1
#> 
#> Slot "rule":
#> An object of class "IncrementsRelative"
#> Slot "intervals":
#> [1]  0 20
#> 
#> Slot "increments":
#> [1] 1.00 0.33
#> 
#> 
```
