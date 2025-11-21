# `ModelLogNormal`

**\[stable\]**

`ModelLogNormal` is the class for a model with a reference dose and
bivariate normal prior on the model parameters `alpha0` and natural
logarithm of `alpha1`, i.e.: \$\$(alpha0, log(alpha1)) ~ Normal(mean,
cov),\$\$. Transformations other than `log`, e.g. identity, can be
specified too in `priormodel` slot. The parameter `alpha1` has a
log-normal distribution by default to ensure positivity of `alpha1`
which further guarantees `exp(alpha1) > 1`. The slots of this class
contain the mean vector, the covariance and precision matrices of the
bivariate normal distribution, as well as the reference dose. Note that
the precision matrix is an inverse of the covariance matrix in the
`JAGS`. All ("normal") model specific classes inherit from this class.

## Usage

``` r
ModelLogNormal(mean, cov, ref_dose = 1)

.DefaultModelLogNormal()
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

## Slots

- `params`:

  (`ModelParamsNormal`)  
  bivariate normal prior parameters.

- `ref_dose`:

  (`positive_number`)  
  the reference dose.

## Note

Typically, end users will not use the `.DefaultModelLogNormal()`
function.

## See also

[`ModelParamsNormal`](https://openpharma.github.io/crmPack/reference/ModelParamsNormal-class.md),
[`LogisticNormal`](https://openpharma.github.io/crmPack/reference/LogisticNormal-class.md),
[`LogisticLogNormal`](https://openpharma.github.io/crmPack/reference/LogisticLogNormal-class.md),
[`LogisticLogNormalSub`](https://openpharma.github.io/crmPack/reference/LogisticLogNormalSub-class.md),
[`ProbitLogNormal`](https://openpharma.github.io/crmPack/reference/ProbitLogNormal-class.md),
[`ProbitLogNormalRel`](https://openpharma.github.io/crmPack/reference/ProbitLogNormalRel-class.md).
