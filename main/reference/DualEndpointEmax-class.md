# `DualEndpointEmax`

**\[experimental\]**

`DualEndpointEmax` is the class for the dual endpoint model with `Emax`
function for dose-biomarker relationship.

## Usage

``` r
DualEndpointEmax(E0, Emax, ED50, ref_dose_emax = 1, ...)

.DefaultDualEndpointEmax()
```

## Arguments

- E0:

  (`numeric`)  
  either a fixed number or the two uniform distribution parameters.

- Emax:

  (`numeric`)  
  either a fixed number or the two uniform distribution parameters.

- ED50:

  (`numeric`)  
  either a fixed number or the two uniform distribution parameters.

- ref_dose_emax:

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
parametric `Emax` function: \$\$f(x) = E0 + \[(Emax - E0) \*
(x/x\*)\]/\[ED50 + (x/x\*)\],\$\$ where \\x\*\\ is a reference dose,
\\E0\\ and \\Emax\\ are the minimum and maximum levels for the
biomarker, and \\ED50\\ is the dose achieving half of the maximum effect
\\0.5 \* Emax\\. All parameters can currently be assigned uniform
distributions or be fixed.

## Slots

- `E0`:

  (`numeric`)  
  either a fixed number or the two uniform distribution parameters.

- `Emax`:

  (`numeric`)  
  either a fixed number or the two uniform distribution parameters.

- `ED50`:

  (`numeric`)  
  either a fixed number or the two uniform distribution parameters.

- `ref_dose_emax`:

  (`positive_number`)  
  the reference dose \\x\*\\ (note that this is different from the
  `ref_dose` in the inherited
  [`DualEndpoint`](https://openpharma.github.io/crmPack/reference/DualEndpoint-class.md)
  model).

## Note

Typically, end users will not use the `.DefaultDualEndpointEmax()`
function.

## See also

[`DualEndpoint`](https://openpharma.github.io/crmPack/reference/DualEndpoint-class.md),
[`DualEndpointRW`](https://openpharma.github.io/crmPack/reference/DualEndpointRW-class.md),
[`DualEndpointBeta`](https://openpharma.github.io/crmPack/reference/DualEndpointBeta-class.md).

## Examples

``` r
my_model <- DualEndpointEmax(
  mean = c(0, 1),
  cov = matrix(c(1, 0, 0, 1), nrow = 2),
  sigma2W = c(a = 0.1, b = 0.1),
  rho = c(a = 1, b = 1),
  E0 = c(0, 100),
  Emax = c(0, 500),
  ED50 = c(10, 200),
  ref_dose_emax = 1000
)
```
