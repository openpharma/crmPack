# `TITELogisticLogNormal`

**\[stable\]**

`TITELogisticLogNormal` is the class for TITE-CRM based on a logistic
regression model using a bivariate normal prior on the intercept and log
slope parameters.

This class inherits from the
[`LogisticLogNormal`](https://openpharma.github.io/crmPack/reference/LogisticLogNormal-class.md).

## Usage

``` r
TITELogisticLogNormal(weight_method = "linear", ...)

.DefaultTITELogisticLogNormal()
```

## Arguments

- weight_method:

  (`string`)  
  the weight function method: either linear or adaptive. This was used
  in Liu, Yin and Yuan's paper.

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

- `weight_method`:

  (`string`)  
  the weight function method: either linear or adaptive. This was used
  in Liu, Yin and Yuan's paper.

## Note

Typically, end users will not use the `.DefaultTITELogisticLogNormal()`
function.

## See also

[`DALogisticLogNormal`](https://openpharma.github.io/crmPack/reference/DALogisticLogNormal-class.md).

## Examples

``` r
my_model <- TITELogisticLogNormal(
  mean = c(0, 1),
  cov = diag(2),
  ref_dose = 1,
  weight_method = "linear"
)

my_model1 <- TITELogisticLogNormal(
  mean = c(0, 1),
  cov = diag(2),
  ref_dose = 1,
  weight_method = "adaptive"
)
```
