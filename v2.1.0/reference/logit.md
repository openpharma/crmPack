# Shorthand for Logit Function

**\[stable\]**

Computes the logit transformation.

## Usage

``` r
logit(x)
```

## Arguments

- x:

  (`numeric`)  
  the function argument.

## Value

The logit of `x`, computed as `log(x / (1 - x))`.

## Examples

``` r
logit(0.5)
#> [1] 0
logit(c(0.1, 0.5, 0.9))
#> [1] -2.197225  0.000000  2.197225
```
