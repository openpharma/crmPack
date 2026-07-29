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
#>   [1] -0.59675745 -0.93965252  2.27353329 -0.84911992 -1.52312593 -0.78225849
#>   [7] -3.08672045  0.55004381 -0.06030122  0.64930942 -1.17903177 -0.70195467
#>  [13]  0.18567563 -1.75606507 -0.87449744 -0.97055416  0.58527331 -0.53131401
#>  [19] -0.88098890 -0.15166480  0.48793616  0.09518328  0.02515404 -2.40141312
#>  [25]  1.26500214 -1.83134349  0.93342510  0.25620220 -1.82883466  1.90982472
#>  [31] -1.06535577  0.03798533  0.52773586  0.10753074 -1.44507521 -1.31434212
#>  [37] -1.53005844 -3.95102163 -1.99496721 -0.03071270  0.70822819 -0.36108474
#>  [43] -0.62938392 -1.52872520  0.86900716 -0.43751933 -1.53390034 -0.24655619
#>  [49] -0.96967713 -0.84811774 -0.49292844 -0.77856033 -0.22738001 -0.42055264
#>  [55] -0.73748316 -1.64061680  0.43863995 -0.74550158 -1.44786866  0.70094034
#>  [61] -1.82716525 -1.04645220 -3.02581091  1.57886371  0.03509631 -0.98104506
#>  [67] -2.34922773 -2.37597424 -1.39356369 -2.08644726  0.19127613 -1.11279781
#>  [73] -1.24016403 -0.05167378  0.08364285 -0.38424070  1.03276535 -0.35470009
#>  [79]  1.15262123 -1.33948404 -1.41845831 -0.74474208 -0.53674192 -0.41058545
#>  [85] -0.03775936 -2.06362962 -0.94739276 -0.89335791 -1.30261934 -0.97177098
#>  [91] -1.53710613 -0.18077876 -1.86012274  0.04617794 -1.27095614 -0.03995168
#>  [97] -0.72874692 -0.28793278 -0.70905801 -1.93023607
#> 
#> $alpha1
#>   [1] -1.05457402  1.42925006 -0.01354106  1.52876913  1.89818982  1.58643653
#>   [7]  1.30900105  0.48028282  0.13364754 -2.04367311  0.97105072  0.31417018
#>  [13]  1.01859998  1.54006085  0.98829210  0.63894034  0.15186065  0.10121406
#>  [19]  0.79039885  1.62623373  0.44115196 -0.56456619 -0.13597294  2.41539695
#>  [25]  0.89184119  3.01902693  0.35534636  1.22415988  0.34621338 -0.97377874
#>  [31] -0.69338066  1.44398613  0.61490498  0.99585480  0.85079625  0.71193263
#>  [37]  0.92779067  2.42301062  1.08676015  2.06146561 -0.48327403 -0.14830679
#>  [43]  0.31997501 -0.20018700  1.03451266 -0.17005799  1.74218980  0.85173799
#>  [49]  1.01735873  0.72978632  0.66463940  0.55955858  0.52466278 -0.67270717
#>  [55] -0.29944274  1.27147664  0.87475567  0.29138258  2.33963186 -1.03565232
#>  [61]  4.32471598  0.60031632  1.30570851 -0.88432035  0.39226483  0.30899515
#>  [67]  1.33305928  1.75289754 -0.25003011  3.10161453  0.24290484  0.31075007
#>  [73]  1.85049621  1.11699727  0.29894664  0.79216690 -0.39088335 -0.92574992
#>  [79] -0.65239591  2.37551273  1.45662209  0.76125395  1.13930599 -0.11257804
#>  [85]  0.89360260  2.44874791  0.37622373  1.18744119  0.21335320  0.86573116
#>  [91]  0.53812758 -0.41338796  2.33497229 -0.11074889  1.69398545  1.03089075
#>  [97]  1.57236399  1.81406468  2.16122829  2.16489416
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
