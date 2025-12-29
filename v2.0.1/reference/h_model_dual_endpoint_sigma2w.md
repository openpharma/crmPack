# Update [`DualEndpoint`](https://openpharma.github.io/crmPack/reference/DualEndpoint-class.md) class model components with regard to biomarker regression variance.

**\[stable\]**

A simple helper function that takes
[`DualEndpoint`](https://openpharma.github.io/crmPack/reference/DualEndpoint-class.md)
model existing components (`priormodel`, `modelspecs`, `init`,
`sample`), and updates them with regard to to biomarker regression
variance `sigma2W`.

## Usage

``` r
h_model_dual_endpoint_sigma2w(use_fixed, sigma2W, comp)
```

## Arguments

- use_fixed:

  (`flag`)  
  indicates whether a fixed value for the biomarker regression variance
  `sigma2W` should be used or not. If `sigma2W` is not supposed to be a
  fixed value, a prior distribution from the Inverse-Gamma distribution
  will be used. See the details below, under `sigma2W` argument.

- sigma2W:

  (`numeric`)  
  the biomarker variance. Either a fixed value or Inverse-Gamma
  distribution parameters, i.e. vector with two elements named `a` and
  `b`.

- comp:

  (`list`)  
  a named list with model components that will be updated. The names
  should be: `priormodel`, `modelspecs`, `init`, `sample`. For
  definitions of the components, see
  [`GeneralModel`](https://openpharma.github.io/crmPack/reference/GeneralModel-class.md)
  class. The `modelspecs` and `init` components on `comp` list are
  specified up to the body of corresponding `GeneralModel@modelspecs`
  and `GeneralModel@init` functions. These bodies are simply a lists
  itself.

## Value

`list` with updated model components.
