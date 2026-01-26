# `NextBestOrdinal`

**\[experimental\]**

`NextBestOrdinal` is the class for applying a standard `NextBest` rule
to the results of an ordinal CRM trial.

## Usage

``` r
NextBestOrdinal(grade, rule)

.DefaultNextBestOrdinal()
```

## Arguments

- grade:

  (`numeric`)  
  see slot definition.

- rule:

  (`NextBest`)  
  see slot definition.

## Slots

- `grade`:

  (`integer`)  
  the toxicity grade to which the `rule` should be applied.

- `rule`:

  (`NextBest`)  
  the standard `NextBest` rule to be applied

## Note

Typically, end users will not use the `.DefaultNextBestOrdinal()`
function.

## Examples

``` r
NextBestOrdinal(
  grade = 1L,
  rule = NextBestMTD(
    0.25,
    function(mtd_samples) {
      quantile(mtd_samples, probs = 0.25)
    }
  )
)
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
#> function (mtd_samples) 
#> {
#>     quantile(mtd_samples, probs = 0.25)
#> }
#> <environment: 0x55a674c52fe8>
#> 
#> 
```
