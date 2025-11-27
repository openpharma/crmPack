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
  see the slot description.

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

## Details

Basically, the adaptive function allocates more weight to each record
than the linear function when DLTs are observed early and less weight
when DLTs are observed late. When DLT times are evenly distributed both
weights are similar. In addition, with more DLTs, the adaptive weights
become more extreme and different from the linear weights.

## Slots

- `weight_method`:

  (`string`)  
  the weight function method: either linear or adaptive; see Liu et
  al. (2013) .

## Note

Typically, end users will not use the `.DefaultTITELogisticLogNormal()`
function.

## References

Liu S, Yin G, Yuan Y (2013). “Bayesian data augmentation dose finding
with continual reassessment method and delayed toxicity.” *The Annals of
Applied Statistics*, **7**(4), 2138–2156.
[doi:10.1214/13-AOAS661](https://doi.org/10.1214/13-AOAS661) .

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
