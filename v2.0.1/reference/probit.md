# Shorthand for Probit Function

**\[stable\]**

Computes the probit (inverse cumulative distribution function of
standard normal) transformation.

## Usage

``` r
probit(x)
```

## Arguments

- x:

  (`numeric`)  
  the function argument.

## Value

The probit of `x`, the quantile function of the standard normal
distribution.

## Examples

``` r
probit(0.5)
#> [1] 0
probit(c(0.025, 0.5, 0.975))
#> [1] -1.959964  0.000000  1.959964
```
