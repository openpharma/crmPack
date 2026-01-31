# `LogisticLogNormalSub`

**\[stable\]**

`LogisticLogNormalSub` is the class for a standard logistic model with
bivariate (log) normal prior with subtractive dose standardization.

## Usage

``` r
LogisticLogNormalSub(mean, cov, ref_dose = 0)

.DefaultLogisticLogNormalSub()
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
  the reference dose \\x\*\\.

## Details

The covariate is the dose \\x\\ minus the reference dose \\x\*\\, i.e.:
\$\$logit\[p(x)\] = alpha0 + alpha1 \* (x - x\*),\$\$ where \\p(x)\\ is
the probability of observing a DLT for a given dose \\x\\. The prior
\$\$(alpha0, log(alpha1)) ~ Normal(mean, cov).\$\$

## Slots

- `params`:

  (`ModelParamsNormal`)  
  bivariate normal prior parameters.

- `ref_dose`:

  (`number`)  
  the reference dose \\x\*\\.

## Note

Typically, end-users will not use the `.DefaultLogisticLogNormalSub()`
function.

## See also

[`LogisticNormal`](https://openpharma.github.io/crmPack/reference/LogisticNormal-class.md),
[`LogisticLogNormal`](https://openpharma.github.io/crmPack/reference/LogisticLogNormal-class.md),
[`ProbitLogNormal`](https://openpharma.github.io/crmPack/reference/ProbitLogNormal-class.md),
[`ProbitLogNormalRel`](https://openpharma.github.io/crmPack/reference/ProbitLogNormalRel-class.md).

## Examples

``` r
my_model <- LogisticLogNormalSub(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
  ref_dose = 50
)
```
