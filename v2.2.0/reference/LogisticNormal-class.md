# `LogisticNormal`

**\[stable\]**

`LogisticNormal` is the class for the usual logistic regression model
with a bivariate normal prior on the intercept and slope.

## Usage

``` r
LogisticNormal(mean, cov, ref_dose = 1)

.DefaultLogisticNormal()
```

## Arguments

- mean:

  (`numeric`)\
  the prior mean vector.

- cov:

  (`matrix`)\
  the prior covariance matrix. The precision matrix `prec` is internally
  calculated as an inverse of `cov`.

- ref_dose:

  (`number`)\
  the reference dose \\x\*\\ (strictly positive number).

## Details

The covariate is the natural logarithm of the dose \\x\\ divided by the
reference dose \\x\*\\, i.e.: \$\$logit\[p(x)\] = alpha0 + alpha1 \*
log(x/x\*),\$\$ where \\p(x)\\ is the probability of observing a DLT for
a given dose \\x\\. The prior \$\$(alpha0, alpha1) ~ Normal(mean,
cov).\$\$

## Note

Typically, end users will not use the `.DefaultLogisticNormal()`
function.

## See also

[`ModelLogNormal`](https://docs.crmpack.org/reference/ModelLogNormal-class.md),
[`LogisticLogNormal`](https://docs.crmpack.org/reference/LogisticLogNormal-class.md),
[`LogisticLogNormalSub`](https://docs.crmpack.org/reference/LogisticLogNormalSub-class.md),
[`ProbitLogNormal`](https://docs.crmpack.org/reference/ProbitLogNormal-class.md),
[`ProbitLogNormalRel`](https://docs.crmpack.org/reference/ProbitLogNormalRel-class.md),
[`LogisticNormalMixture`](https://docs.crmpack.org/reference/LogisticNormalMixture-class.md).

## Examples

``` r
# Define the dose-grid.
empty_data <- Data(doseGrid = c(1, 3, 5, 10, 15, 20, 25, 40, 50, 80, 100))

my_model <- LogisticNormal(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2)
)

my_options <- McmcOptions(burnin = 10, step = 2, samples = 100)

samples <- mcmc(empty_data, my_model, my_options)
samples
#> An object of class "Samples"
#> Slot "data":
#> $alpha0
#>   [1] -8.981381e-02 -4.980076e-01 -1.409527e+00 -2.578294e+00 -2.351110e+00
#>   [6] -6.183267e-01 -1.672454e+00  5.476478e-01 -4.381458e-01 -3.132636e-01
#>  [11]  6.845051e-01 -2.800253e+00  3.602947e-01 -1.513970e-01 -2.016704e+00
#>  [16] -6.048468e-02 -1.268629e+00 -1.149232e+00 -2.550779e+00 -1.839635e+00
#>  [21] -1.295015e+00 -1.493028e+00 -2.562569e+00  4.500619e-01 -2.342576e+00
#>  [26]  5.223945e-01 -1.501877e+00  2.386207e-01 -2.041239e+00 -1.022720e+00
#>  [31] -8.232285e-01  1.115811e+00 -3.851624e+00 -9.400995e-05 -3.633510e-01
#>  [36]  1.200753e+00  6.380239e-01  3.641610e-01 -1.522947e+00 -9.202687e-01
#>  [41] -1.326283e+00 -3.336404e-01 -4.220710e+00 -2.227417e+00 -1.892591e+00
#>  [46]  4.307372e-01 -1.158765e+00 -1.539250e+00 -1.560239e+00 -1.452555e+00
#>  [51]  7.777713e-01 -1.090625e+00 -2.200735e+00 -2.506771e+00  1.405008e-01
#>  [56] -4.073842e-01 -2.102113e+00 -2.687454e+00 -1.879116e+00 -5.699346e-01
#>  [61] -1.127144e+00 -2.500415e+00 -1.619589e+00 -4.584195e-02  9.766160e-01
#>  [66] -6.330056e-01 -4.307436e-01 -1.467695e+00 -4.048267e-01 -1.522041e+00
#>  [71] -1.922118e+00  7.124175e-01 -4.704217e-01 -2.246491e+00 -2.559189e+00
#>  [76] -6.200446e-01 -8.198456e-01 -2.188716e-01 -1.167597e+00 -1.208344e+00
#>  [81] -9.229780e-01 -2.497374e-01 -1.522768e+00  2.247337e-01  1.679438e+00
#>  [86]  3.436265e-01  9.368353e-02 -2.452906e+00  1.821536e-01 -6.220450e-01
#>  [91] -2.210738e+00  1.282203e-01 -5.053239e-01 -1.356469e+00 -3.092771e-02
#>  [96] -5.839456e-01 -1.944858e+00 -7.267802e-01 -1.795845e+00 -7.032269e-01
#> 
#> $alpha1
#>   [1]  0.2425399  0.8327681  1.1312562  3.1675134  1.8900495  1.1836805
#>   [7]  2.1615909 -1.1543454  1.2611240  0.5692559 -1.0566265  1.4077780
#>  [13]  0.5798423  0.6498462  2.1156964 -0.1937975  1.0892070  0.9530183
#>  [19]  2.9718905  1.9750239  0.8566104  1.7231860  1.6111177  0.6561863
#>  [25]  1.0434993 -1.0996811  2.2940992  0.5290545  1.0015056  1.8501093
#>  [31]  1.3716321 -1.1012398  2.9900390 -0.2886969  1.9355722 -1.9175719
#>  [37]  0.7882932  1.0610569  1.3624898  1.6354919  0.7418671 -0.4891231
#>  [43]  3.7784893  0.4932685  0.6432517 -0.1519720  0.2867294  0.8752883
#>  [49]  1.4243589  0.4992674 -1.8212931  1.8121280  0.2649196  1.6654123
#>  [55]  1.0548402  0.8723112  1.6172721  2.4428387  2.7619623  1.4570338
#>  [61]  1.5677850  1.4893635  0.7061828  0.7130753 -0.3204491  1.6018398
#>  [67]  1.5129865  1.2995436  0.7507325  0.7163406  2.5515228 -0.4172362
#>  [73]  0.3096576  2.1018490  1.2624254  0.5352602  0.4709719  1.1344253
#>  [79]  1.5875529  1.3065206  0.6950016  0.3498329  0.4625664  1.0849135
#>  [85] -0.2446769  1.0155103  0.5066958  2.0875097  0.5240144  1.9660618
#>  [91]  2.4286354 -0.4688659  0.7234600  1.3982826  0.4604734  0.1204930
#>  [97] -0.4587477  0.7156587  2.0941232  1.3338890
#> 
#> 
#> Slot "options":
#> An object of class "McmcOptions"
#> Slot "iterations":
#> [1] 210
#> 
#> Slot "burnin":
#> [1] 10
#> 
#> Slot "step":
#> [1] 2
#> 
#> Slot "rng_kind":
#> [1] NA
#> 
#> Slot "rng_seed":
#> [1] NA
#> 
#> 
```
