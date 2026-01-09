# `DualEndpointRW`

**\[experimental\]**

`DualEndpointRW` is the class for the dual endpoint model with random
walk prior for biomarker.

## Usage

``` r
DualEndpointRW(sigma2betaW, rw1 = TRUE, ...)

.DefaultDualEndpointRW()
```

## Arguments

- sigma2betaW:

  (`numeric`)  
  the prior variance factor of the random walk prior for the biomarker
  model. Either a fixed value or Inverse-Gamma distribution parameters,
  i.e. vector with two elements named `a` and `b`.

- rw1:

  (`flag`)  
  for specifying the random walk prior on the biomarker level. When
  `TRUE`, random walk of first order is used. Otherwise, the random walk
  of second order is used.

- ...:

  parameters passed to
  [`DualEndpoint()`](https://openpharma.github.io/crmPack/reference/DualEndpoint-class.md).

## Details

This class extends the
[`DualEndpoint`](https://openpharma.github.io/crmPack/reference/DualEndpoint-class.md)
class so that the dose-biomarker relationship \\f(x)\\ is modelled by a
non-parametric random walk of first or second order. That means, for the
first order random walk we assume \$\$betaW_i - betaW_i-1 ~ Normal(0,
(x_i - x_i-1) \* sigma2betaW),\$\$ where \\betaW_i = f(x_i)\\ is the
biomarker mean at the \\i\\-th dose gridpoint \\x_i\\. For the second
order random walk, the second-order differences instead of the
first-order differences of the biomarker means follow the normal
distribution with \\0\\ mean and \\2 \* (x_i - x_i-2) \* sigma2betaW\\
variance.

The variance parameter \\sigma2betaW\\ is important because it steers
the smoothness of the function \\f(x)\\, i.e.: if it is large, then
\\f(x)\\ will be very wiggly; if it is small, then \\f(x)\\ will be
smooth. This parameter can either be a fixed value or assigned an
inverse gamma prior distribution.

## Slots

- `sigma2betaW`:

  (`numeric`)  
  the prior variance factor of the random walk prior for the biomarker
  model. Either a fixed value or Inverse-Gamma distribution parameters,
  i.e. vector with two elements named `a` and `b`.

- `rw1`:

  (`flag`)  
  for specifying the random walk prior on the biomarker level. When
  `TRUE`, random walk of first order is used. Otherwise, the random walk
  of second order is used.

## Note

Non-equidistant dose grids can be used now, because the difference
\\x_i - x_i-1\\ is included in the modelling assumption above. Please
note that due to impropriety of the random walk prior distributions, it
is not possible to produce MCMC samples with empty data objects (i.e.,
sample from the prior). This is not a bug, but a theoretical feature of
this model.

Typically, end users will not use the `.DefaultDualEndpointRW()`
function.

## See also

[`DualEndpoint`](https://openpharma.github.io/crmPack/reference/DualEndpoint-class.md),
[`DualEndpointBeta`](https://openpharma.github.io/crmPack/reference/DualEndpointBeta-class.md),
[`DualEndpointEmax`](https://openpharma.github.io/crmPack/reference/DualEndpointEmax-class.md).

## Examples

``` r
my_model <- DualEndpointRW(
  mean = c(0, 1),
  cov = matrix(c(1, 0, 0, 1), nrow = 2),
  sigma2W = c(a = 0.1, b = 0.1),
  rho = c(a = 1, b = 1),
  sigma2betaW = 0.01,
  rw1 = TRUE
)
```
