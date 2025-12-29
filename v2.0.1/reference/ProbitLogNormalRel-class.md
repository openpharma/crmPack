# `ProbitLogNormalRel`

**\[stable\]**

`ProbitLogNormalRel` is the class for probit regression model with a
bivariate normal prior on the intercept and log slope.

## Usage

``` r
ProbitLogNormalRel(mean, cov, ref_dose = 1)

.DefaultProbitLogNormalRel()
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

The covariate is the dose \\x\\ divided by a reference dose \\x\*\\,
i.e.: \$\$probit\[p(x)\] = alpha0 + alpha1 \* x/x\*,\$\$ where \\p(x)\\
is the probability of observing a DLT for a given dose \\x\\. The prior
\$\$(alpha0, log(alpha1)) ~ Normal(mean, cov).\$\$

## Note

This model is also used in the
[`DualEndpoint`](https://openpharma.github.io/crmPack/reference/DualEndpoint-class.md)
classes, so this class can be used to check the prior assumptions on the
dose-toxicity model, even when sampling from the prior distribution of
the dual endpoint model is not possible.

Typically, end users will not use the `.DefaultProbitLogNormalRel()`
function.

## See also

[`ModelLogNormal`](https://openpharma.github.io/crmPack/reference/ModelLogNormal-class.md),
[`LogisticNormal`](https://openpharma.github.io/crmPack/reference/LogisticNormal-class.md),
[`LogisticLogNormal`](https://openpharma.github.io/crmPack/reference/LogisticLogNormal-class.md),
[`LogisticLogNormalSub`](https://openpharma.github.io/crmPack/reference/LogisticLogNormalSub-class.md),
[`ProbitLogNormal`](https://openpharma.github.io/crmPack/reference/ProbitLogNormal-class.md).

## Examples

``` r
my_model <- ProbitLogNormalRel(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2)
)
```
