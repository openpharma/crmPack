# `DualEndpointBeta`

**\[experimental\]**

`DualEndpointBeta` is the class for the dual endpoint model with beta
function for dose-biomarker relationship.

## Usage

``` r
DualEndpointBeta(E0, Emax, delta1, mode, ref_dose_beta = 1, ...)

.DefaultDualEndpointBeta()
```

## Arguments

- E0:

  (`numeric`)  
  either a fixed number or the two uniform distribution parameters.

- Emax:

  (`numeric`)  
  either a fixed number or the two uniform distribution parameters.

- delta1:

  (`numeric`)  
  either a fixed positive number or the two parameters of the uniform
  distribution, that can take only positive values.

- mode:

  (`numeric`)  
  either a fixed positive number or the two parameters of the uniform
  distribution, that can take only positive values.

- ref_dose_beta:

  (`number`)  
  the reference dose \\x\*\\ (strictly positive number). Note that this
  is different from the `ref_dose` in the inherited
  [`DualEndpoint`](https://openpharma.github.io/crmPack/reference/DualEndpoint-class.md)
  model).

- ...:

  parameters passed to
  [`DualEndpoint()`](https://openpharma.github.io/crmPack/reference/DualEndpoint-class.md).

## Details

This class extends the
[`DualEndpoint`](https://openpharma.github.io/crmPack/reference/DualEndpoint-class.md)
class so that the dose-biomarker relationship \\f(x)\\ is modelled by a
parametric, rescaled beta density function: \$\$f(x) = E0 + (Emax - E0)
\* Beta(delta1, delta2) \* (x/x\*)^{delta1} \* (1 - x/x\*)^{delta2},\$\$
where \\x\*\\ is the maximum dose (end of the dose range to be
considered), \\delta1\\ and \\delta2\\ are the two beta function
parameters, and \\E0\\, \\Emax\\ are the minimum and maximum levels,
respectively. For ease of interpretation, we use the parametrization
based on \\delta1\\ and the mode, where \$\$mode = delta1 / (delta1 +
delta2),\$\$ so that multiplying this by \\x\*\\ gives the mode on the
dose grid.

All parameters can currently be assigned uniform distributions or be
fixed in advance. Note that `E0` and `Emax` can have negative values or
uniform distributions reaching into negative range, while `delta1` and
`mode` must be positive or have uniform distributions in the positive
range.

## Slots

- `E0`:

  (`numeric`)  
  either a fixed number or the two uniform distribution parameters.

- `Emax`:

  (`numeric`)  
  either a fixed number or the two uniform distribution parameters.

- `delta1`:

  (`numeric`)  
  either a fixed positive number or the two parameters of the uniform
  distribution, that can take only positive values.

- `mode`:

  (`numeric`)  
  either a fixed positive number or the two parameters of the uniform
  distribution, that can take only positive values.

- `ref_dose_beta`:

  (`positive_number`)  
  the reference dose \\x\*\\ (note that this is different from the
  `ref_dose` in the inherited
  [`DualEndpoint`](https://openpharma.github.io/crmPack/reference/DualEndpoint-class.md)
  model).

## Note

Typically, end users will not use the `.DefaultDualEndpointBeta()`
function.

## See also

[`DualEndpoint`](https://openpharma.github.io/crmPack/reference/DualEndpoint-class.md),
[`DualEndpointRW`](https://openpharma.github.io/crmPack/reference/DualEndpointRW-class.md),
[`DualEndpointEmax`](https://openpharma.github.io/crmPack/reference/DualEndpointEmax-class.md).

## Examples

``` r
my_model <- DualEndpointBeta(
  mean = c(0, 1),
  cov = matrix(c(1, 0, 0, 1), nrow = 2),
  ref_dose = 10,
  use_log_dose = TRUE,
  sigma2W = c(a = 0.1, b = 0.1),
  rho = c(a = 1, b = 1),
  E0 = c(0, 100),
  Emax = c(0, 500),
  delta1 = c(0, 5),
  mode = c(1, 15),
  ref_dose_beta = 1000
)
```
