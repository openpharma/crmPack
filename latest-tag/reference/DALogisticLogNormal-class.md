# `DALogisticLogNormal`

**\[stable\]**

`DALogisticLogNormal` is the class for the logistic model with bivariate
(log) normal prior and data augmentation. This class inherits from the
[`LogisticLogNormal`](https://openpharma.github.io/crmPack/reference/LogisticLogNormal-class.md)
class.

## Usage

``` r
DALogisticLogNormal(npiece = 3, l, c_par = 2, cond_pem = TRUE, ...)

.DefaultDALogisticLogNormal()
```

## Arguments

- npiece:

  (`number`)  
  the number of pieces in the `PEM`.

- l:

  (`numeric`)  
  a vector used in the lambda prior.

- c_par:

  (`numeric`)  
  a parameter used in the lambda prior; according to Liu's paper,
  `c_par = 2` is recommended.

- cond_pem:

  (`flag`)  
  is a conditional piecewise-exponential model used? (default).
  Otherwise an unconditional model is used.

- ...:

  Arguments passed on to
  [`LogisticLogNormal`](https://openpharma.github.io/crmPack/reference/LogisticLogNormal-class.md)

  `mean`

  :   (`numeric`)  
      the prior mean vector.

  `cov`

  :   (`matrix`)  
      the prior covariance matrix. The precision matrix `prec` is
      internally calculated as an inverse of `cov`.

  `ref_dose`

  :   (`number`)  
      the reference dose \\x\*\\ (strictly positive number).

## Slots

- `npiece`:

  (`number`)  
  the number of pieces in the `PEM`.

- `l`:

  (`numeric`)  
  a vector used in the lambda prior.

- `c_par`:

  (`numeric`)  
  a parameter used in the lambda prior; according to Liu's paper,
  `c_par = 2` is recommended.

- `cond_pem`:

  (`flag`)  
  is a conditional piecewise-exponential model used? (default).
  Otherwise an unconditional model is used.

## Note

We still need to include here formula for the lambda prior.

Typically, end users will not use the `.DefaultDALogisticLogNormal()`
function.

## See also

[`ModelLogNormal`](https://openpharma.github.io/crmPack/reference/ModelLogNormal-class.md),
[`LogisticNormal`](https://openpharma.github.io/crmPack/reference/LogisticNormal-class.md),
[`LogisticLogNormal`](https://openpharma.github.io/crmPack/reference/LogisticLogNormal-class.md).

## Examples

``` r
npiece <- 10
Tmax <- 60 # nolintr

lambda_prior <- function(k) {
  npiece / (Tmax * (npiece - k + 0.5))
}

model <- DALogisticLogNormal(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
  ref_dose = 56,
  npiece = npiece,
  l = as.numeric(t(apply(as.matrix(c(1:npiece), 1, npiece), 2, lambda_prior))),
  c_par = 2
)
```
