# Update certain components of [`DualEndpoint`](https://openpharma.github.io/crmPack/reference/DualEndpoint-class.md) model with regard to parameters of the function that models dose-biomarker relationship defined in the [`DualEndpointBeta`](https://openpharma.github.io/crmPack/reference/DualEndpointBeta-class.md) class.

**\[stable\]**

A simple helper function that takes
[`DualEndpoint`](https://openpharma.github.io/crmPack/reference/DualEndpoint-class.md)
object and updates `use_fixed`, `priormodel`, `modelspecs`, `init`,
`sample` slots with regard to a given parameter of the dose-biomarker
relationship \\f(x)\\ defined in the
[`DualEndpointBeta`](https://openpharma.github.io/crmPack/reference/DualEndpointBeta-class.md)
class. This update solely depends on whether a given parameter's value
`param` is a fixed-valued scalar or two-elements numeric vector. In the
later case, it is assumed that `param` represents two parameters of a
probability distribution that will be used in `priormodel` function to
generate values for the `param_name` parameter of \\f(x)\\. See the help
page for
[`DualEndpointBeta`](https://openpharma.github.io/crmPack/reference/DualEndpointBeta-class.md)
class for more details.

## Usage

``` r
h_model_dual_endpoint_beta(
  param,
  param_name,
  param_suffix = c("_low", "_high"),
  priormodel = NULL,
  de
)
```

## Arguments

- param:

  (`numeric`)  
  the value of a given `param_name` parameter of the dose-biomarker
  relationship function \\f(x)\\. Either a fixed-valued scalar or vector
  with two elements that are the parameters of a probability
  distribution that will be used in `priormodel` function to generate
  values for the `param_name` parameter of \\f(x)\\.

- param_name:

  (`string`)  
  the name of the parameter of \\f(x)\\, whose value depends on `param`.

- param_suffix:

  (`character`)  
  the two suffixes to be appended to the elements of `param_name` and
  then used when updating `modelspecs`. The value of this argument is
  ignored when `param` is a scalar.

- priormodel:

  (`function` or `NULL`)  
  a function representing the `JAGS` prior specification that will be
  appended to existing `de@priormodel` specification if `param` is not a
  scalar. Otherwise, `de@priormodel` remains unchanged.

- de:

  (`DualEnpoint`)  
  dual endpoint model whose slots will be updated.

## Value

A
[`DualEndpoint`](https://openpharma.github.io/crmPack/reference/DualEndpoint-class.md)
model with updated `use_fixed`, `priormodel`, `modelspecs`, `init`,
`sample` slots.
