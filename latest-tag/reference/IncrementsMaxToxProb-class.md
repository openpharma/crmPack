# `IncrementsMaxToxProb`

**\[experimental\]**

`IncrementsMaxToxProb` is the class for increments control based on
probability of toxicity

## Usage

``` r
IncrementsMaxToxProb(prob)

.DefaultIncrementsMaxToxProb()
```

## Arguments

- prob:

  (`numeric`)  
  see slot definition.

## Slots

- `prob`:

  (`numeric`)  
  See Usage Notes below.

## Note

Typically, end users will not use the `.DefaultIncrementsMaxToxProb()`
function.

## Usage Notes

For binary models, `prob` should be a scalar probability.

For ordinal models, `prob` should be a named vector containing the
maximum permissible probability of toxicity by grade. The names should
match the names of the `yCategories` slot of the associated
`DataOrdinal` object.

## Examples

``` r
# For use with binary models and data
IncrementsMaxToxProb(prob = 0.35)
#> An object of class "IncrementsMaxToxProb"
#> Slot "prob":
#> [1] 0.35
#> 

# For use with ordinal models and data
IncrementsMaxToxProb(prob = c("DLAE" = 0.2, "DLT" = 0.05))
#> An object of class "IncrementsMaxToxProb"
#> Slot "prob":
#> DLAE  DLT 
#> 0.20 0.05 
#> 
```
