# Helper for Minimal Informative Unimodal Beta Distribution

As defined in Neuenschwander et al. (2008) , this function computes the
parameters of the minimal informative unimodal beta distribution, given
the request that the p-quantile should be q, i.e. `X ~ Be(a, b)` with
`Pr(X <= q) = p`.

## Usage

``` r
h_get_min_inf_beta(p, q)
```

## Arguments

- p:

  (`number`)  
  the probability.

- q:

  (`number`)  
  the quantile.

## Value

A list with the two resulting beta parameters `a` and `b`.

## References

Neuenschwander B, Branson M, Gsponer T (2008). “Critical aspects of the
Bayesian approach to phase I cancer trials.” *Statistics in Medicine*,
**27**(13), 2420–2439.
<https://onlinelibrary.wiley.com/doi/10.1002/sim.3230>.
