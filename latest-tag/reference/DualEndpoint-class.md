# `DualEndpoint`

**\[experimental\]**

`DualEndpoint` is the general class for the dual endpoint model.

## Usage

``` r
DualEndpoint(mean, cov, ref_dose = 1, use_log_dose = FALSE, sigma2W, rho)

.DefaultDualEndpoint()
```

## Arguments

- mean:

  (`numeric`)  
  for the probit toxicity model, the prior mean vector.

- cov:

  (`matrix`)  
  for the probit toxicity model, the prior covariance matrix. The
  precision matrix is internally calculated as an inverse of `cov`.

- ref_dose:

  (`number`)  
  for the probit toxicity model, the reference dose \\x\*\\ (strictly
  positive number).

- use_log_dose:

  (`flag`)  
  for the probit toxicity model, whether a log transformation of the
  (standardized) dose should be used?

- sigma2W:

  (`numeric`)  
  the biomarker variance. Either a fixed value or Inverse-Gamma
  distribution parameters, i.e. vector with two elements named `a` and
  `b`.

- rho:

  (`numeric`)  
  either a fixed value for the correlation (between `-1` and `1`), or a
  named vector with two elements named `a` and `b` for the Beta prior on
  the transformation `kappa = (rho + 1) / 2`, which is in `(0, 1)`. For
  example, `a = 1, b = 1` leads to a uniform prior on `rho`.

## Details

The idea of the dual-endpoint models is to model not only the
dose-toxicity relationship, but also to model, at the same time, the
relationship of a PD biomarker with the dose. The sub-classes of this
class define how the dose-biomarker relationship is parametrized. This
class here shall contain all the common features to reduce duplicate
code. (This class however, must not be virtual as we need to create
objects of it during the construction of subclass objects.)

The dose-toxicity relationship is modeled with probit regression model
\$\$probit\[p(x)\] = betaZ1 + betaZ2 \* x/x\*,\$\$ or \$\$probit\[p(x)\]
= betaZ1 + betaZ2 \* log(x/x\*),\$\$ in case when the option
`use_log_dose` is `TRUE`. Here, \\p(x)\\ is the probability of observing
a DLT for a given dose \\x\\ and \\x\*\\ is the reference dose. The
prior \$\$(betaZ1, log(betaZ2)) ~ Normal(mean, cov).\$\$

For the biomarker response \\w\\ at a dose \\x\\, we assume \$\$w(x) ~
Normal(f(x), sigma2W),\$\$ where \\f(x)\\ is a function of the dose
\\x\\, which is further specified in sub-classes. The biomarker variance
\\sigma2W\\ can be fixed or assigned an Inverse-Gamma prior
distribution; see the details below under slot `sigma2W`.

Finally, the two endpoints \\y\\ (the binary DLT variable) and \\w\\
(the biomarker) can be correlated, by assuming a correlation of level
\\rho\\ between the underlying continuous latent toxicity variable \\z\\
and the biomarker \\w\\. Again, this correlation can be fixed or
assigned a prior distribution from the scaled Beta family; see the
details below under slot `rho`.

Please see the example vignette by typing
[`crmPackExample()`](https://openpharma.github.io/crmPack/reference/crmPackExample.md)
for a full example.

## Slots

- `betaZ_params`:

  (`ModelParamsNormal`)  
  for the probit toxicity model, it contains the prior mean, covariance
  matrix and precision matrix which is internally calculated as an
  inverse of the covariance matrix.

- `ref_dose`:

  (`positive_number`)  
  for the probit toxicity model, the reference dose.

- `use_log_dose`:

  (`flag`)  
  for the probit toxicity model, whether a log transformation of the
  (standardized) dose should be used?

- `sigma2W`:

  (`numeric`)  
  the biomarker variance. Either a fixed value or Inverse-Gamma
  distribution parameters, i.e. vector with two elements named `a` and
  `b`.

- `rho`:

  (`numeric`)  
  either a fixed value for the correlation (between `-1` and `1`), or a
  named vector with two elements named `a` and `b` for the Beta prior on
  the transformation `kappa = (rho + 1) / 2`, which is in `(0, 1)`. For
  example, `a = 1, b = 1` leads to a uniform prior on `rho`.

- `use_fixed`:

  (`logical`)  
  indicates whether a fixed value for `sigma2W` or `rho` (for each
  parameter separately) is used or not. This slot is needed for internal
  purposes and must not be touched by the user.

## Note

Typically, end users will not use the `.DefaultDualEndpoint()` function.

## See also

[`DualEndpointRW`](https://openpharma.github.io/crmPack/reference/DualEndpointRW-class.md),
[`DualEndpointBeta`](https://openpharma.github.io/crmPack/reference/DualEndpointBeta-class.md),
[`DualEndpointEmax`](https://openpharma.github.io/crmPack/reference/DualEndpointEmax-class.md).
