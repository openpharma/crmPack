# Update certain components of [`DualEndpoint`](https://openpharma.github.io/crmPack/reference/DualEndpoint-class.md) model with regard to prior variance factor of the random walk.

**\[stable\]**

A simple helper function that takes
[`DualEndpoint`](https://openpharma.github.io/crmPack/reference/DualEndpoint-class.md)
object and updates `priormodel`, `modelspecs`, `init`, `sample` slots
according to the random walk variance.

## Usage

``` r
h_model_dual_endpoint_sigma2betaw(use_fixed, sigma2betaW, de)
```

## Arguments

- use_fixed:

  (`flag`)  
  indicates whether a fixed value for `sigma2betaW` should be used or
  not. If `sigma2betaW` is not supposed to be a fixed value, a prior
  distribution from the Inverse-Gamma distribution will be used. See the
  details below, under `sigma2betaW` argument.

- sigma2betaW:

  (`numeric`)  
  the prior variance factor of the random walk prior for the biomarker
  model. Either a fixed value or Inverse-Gamma distribution parameters,
  i.e. vector with two elements named `a` and `b`.

- de:

  (`DualEnpoint`)  
  dual endpoint model whose slots will be updated.

## Value

A
[`DualEndpoint`](https://openpharma.github.io/crmPack/reference/DualEndpoint-class.md)
model with updated `priormodel`, `modelspecs`, `init`, `sample` slots.

## See also

[`DualEndpointRW`](https://openpharma.github.io/crmPack/reference/DualEndpointRW-class.md).
