# `ProbitLogNormal`

**\[stable\]**

`ProbitLogNormal` is the class for probit regression model with a
bivariate normal prior on the intercept and log slope.

## Usage

``` r
ProbitLogNormal(mean, cov, ref_dose = 1)

.DefaultProbitLogNormal()
```

## Arguments

- mean:

  (`numeric`)  
  the prior mean vector.

- cov:

  (`matrix`)  
  the prior covariance matrix. The precision matrix `prec` is internally
  calculated as an inverse of `cov`.

- ref_dose:

  (`number`)  
  the reference dose \\x\*\\ (strictly positive number).

## Details

The covariate is the natural logarithm of dose \\x\\ divided by a
reference dose \\x\*\\, i.e.: \$\$probit\[p(x)\] = alpha0 + alpha1 \*
log(x/x\*),\$\$ where \\p(x)\\ is the probability of observing a DLT for
a given dose \\x\\. The prior \$\$(alpha0, log(alpha1)) ~ Normal(mean,
cov).\$\$

## Note

The model used in the
[`DualEndpoint`](https://openpharma.github.io/crmPack/reference/DualEndpoint-class.md)
classes is an extension of this model: `DualEndpoint` supports both
`ProbitNormal` (which is not implemented yet) and `ProbitLogNormal`
models through its `use_log_dose` slot. `ProbitLogNormal` has no such
flag, so always uses `log(x/x*)`as a covariate in its model. Therefore
this class can be used to check the prior assumptions on the
dose-toxicity model, even when sampling from the prior distribution of
the dual endpoint model is not possible, when `use_log_dose = TRUE` is
used.

Typically, end users will not use the `.DefaultProbitLogNormal()`
function.

## See also

[`ModelLogNormal`](https://openpharma.github.io/crmPack/reference/ModelLogNormal-class.md),
[`LogisticNormal`](https://openpharma.github.io/crmPack/reference/LogisticNormal-class.md),
[`LogisticLogNormal`](https://openpharma.github.io/crmPack/reference/LogisticLogNormal-class.md),
[`LogisticLogNormalSub`](https://openpharma.github.io/crmPack/reference/LogisticLogNormalSub-class.md),
[`ProbitLogNormalRel`](https://openpharma.github.io/crmPack/reference/ProbitLogNormalRel-class.md).

## Examples

``` r
my_model <- ProbitLogNormal(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
  ref_dose = 7.2
)
```
