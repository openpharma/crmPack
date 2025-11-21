# Compute the Distribution Function of Inverse Gamma Distribution

**\[stable\]**

## Usage

``` r
pinvGamma(q, a, b, lower.tail = TRUE, log.p = FALSE)
```

## Arguments

- q:

  (`numeric`)  
  vector of quantiles.

- a:

  (`number`)  
  the shape parameter of the inverse gamma distribution.

- b:

  (`number`)  
  the scale parameter of the inverse gamma distribution.

- lower.tail:

  (`flag`)  
  if `TRUE` (default), probabilities are `P(X <= x)`, otherwise,
  `P(X > x)`.

- log.p:

  (`flag`)  
  if `TRUE`, probabilities/densities `p` are returned as `log(p)`.

## Value

The distribution function value(s).
