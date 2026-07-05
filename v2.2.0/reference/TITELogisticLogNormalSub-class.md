# `TITELogisticLogNormalSub`

**\[stable\]**

`TITELogisticLogNormalSub` is the class for TITE-CRM based on a logistic
regression model with subtractive dose standardization and a bivariate
normal prior on the intercept and log slope parameters.

This class inherits from
[`LogisticLogNormalSub`](https://docs.crmpack.org/reference/LogisticLogNormalSub-class.md).

## Usage

``` r
TITELogisticLogNormalSub(weight_method = "linear", ...)

.DefaultTITELogisticLogNormalSub()
```

## Arguments

- weight_method:

  (`string`)\
  see the slot description.

- ...:

  Arguments passed on to
  [`LogisticLogNormalSub`](https://docs.crmpack.org/reference/LogisticLogNormalSub-class.md)

  `mean`

  :   (`numeric`)\
      the prior mean vector.

  `cov`

  :   (`matrix`)\
      the prior covariance matrix. The precision matrix `prec` is
      internally calculated as an inverse of `cov`.

  `ref_dose`

  :   (`number`)\
      the reference dose \\x\*\\.

## Details

Basically, the adaptive function allocates more weight to each record
than the linear function when DLTs are observed early and less weight
when DLTs are observed late. When DLT times are evenly distributed both
weights are similar. In addition, with more DLTs, the adaptive weights
become more extreme and different from the linear weights.

## Slots

- `weight_method`:

  (`string`)\
  the weight function method: either linear or adaptive; see Liu et
  al. (2013) .

## Note

Typically, end users will not use the
`.DefaultTITELogisticLogNormalSub()` function.

## References

Liu S, Yin G, Yuan Y (2013). “Bayesian data augmentation dose finding
with continual reassessment method and delayed toxicity.” *The Annals of
Applied Statistics*, **7**(4), 2138–2156.
[doi:10.1214/13-AOAS661](https://doi.org/10.1214/13-AOAS661) .

## See also

[TITELogisticLogNormal](https://docs.crmpack.org/reference/TITELogisticLogNormal-class.md),
[DALogisticLogNormal](https://docs.crmpack.org/reference/DALogisticLogNormal-class.md).

## Examples

``` r
my_model <- TITELogisticLogNormalSub(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
  ref_dose = 50,
  weight_method = "linear"
)

my_model1 <- TITELogisticLogNormalSub(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
  ref_dose = 50,
  weight_method = "adaptive"
)
```
