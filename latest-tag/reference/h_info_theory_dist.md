# Calculating the Information Theoretic Distance

**\[experimental\]**

Helper function which provides the value of the divergence as given by
equation in (7) in the reference at https://doi.org/10.1002/sim.8450.

## Usage

``` r
h_info_theory_dist(prob, target, asymmetry)
```

## Arguments

- prob:

  (`numeric`)  
  vector or matrix with probabilities of a DLT occurring.

- target:

  (`number `)  
  single target probability of a DLT.

- asymmetry:

  (`number`)  
  describes the rate of penalization for overly toxic does, range 0 to
  2.

## Examples

``` r
h_info_theory_dist(c(0.5, 0.2), 0.4, 1.2)
#> [1] 0.040000 0.329877
```
