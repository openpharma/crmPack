# Update [`DualEndpoint`](https://openpharma.github.io/crmPack/reference/DualEndpoint-class.md) class model components with regard to DLT and biomarker correlation.

**\[stable\]**

A simple helper function that takes
[`DualEndpoint`](https://openpharma.github.io/crmPack/reference/DualEndpoint-class.md)
model existing components (`priormodel`, `modelspecs`, `init`,
`sample`), and updates them with regard to DLT and biomarker correlation
`rho`.

## Usage

``` r
h_model_dual_endpoint_rho(use_fixed, rho, comp)
```

## Arguments

- use_fixed:

  (`flag`)  
  indicates whether a fixed value for DLT and biomarker correlation
  `rho` should be used or not. If `rho` is not supposed to be a fixed
  value, a prior distribution from the scaled Beta family will be used.
  See the details below, under `rho` argument.

- rho:

  (`numeric`)  
  DLT and biomarker correlation. It must be either a fixed value
  (between `-1` and `1`), or a named vector with two elements, named `a`
  and `b` for the Beta prior on the transformation
  `kappa = (rho + 1) / 2`, which is in `(0, 1)`. For example,
  `a = 1, b = 1` leads to a uniform prior on `rho`.

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

A `list` with updated model components.
