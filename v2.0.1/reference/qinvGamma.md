# Compute the Quantile Function of Inverse Gamma Distribution

**\[stable\]**

## Usage

``` r
qinvGamma(p, a, b, lower.tail = TRUE, log.p = FALSE)
```

## Arguments

- p:

  (`numeric`)  
  vector of probabilities.

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
  if `TRUE`, probabilities/densities `p` are given as `log(p)`.

## Value

The quantile value(s).
