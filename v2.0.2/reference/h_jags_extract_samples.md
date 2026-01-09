# Extracting Samples from `JAGS` `mcarray` Object

**\[stable\]**

A simple helper function that extracts a sample from
[`rjags::mcarray.object`](https://rdrr.io/pkg/rjags/man/mcarray.object.html)
S3 class object. The
[`rjags::mcarray.object`](https://rdrr.io/pkg/rjags/man/mcarray.object.html)
object is used by the
[`rjags::jags.samples()`](https://rdrr.io/pkg/rjags/man/jags.samples.html)
function to represent MCMC output from a `JAGS` model.

## Usage

``` r
h_jags_extract_samples(x)
```

## Arguments

- x:

  an
  [`rjags::mcarray.object`](https://rdrr.io/pkg/rjags/man/mcarray.object.html)
  object.
