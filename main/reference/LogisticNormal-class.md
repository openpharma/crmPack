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
#>   [1] -2.13639372  0.35115262 -1.76337835  1.68390168 -0.56263551  0.38019024
#>   [7] -1.68507777 -2.05041032 -1.19548422 -0.91822942 -0.41452100 -0.27993271
#>  [13] -0.53408385 -0.83891176 -0.49882361 -0.08837695 -0.93025940 -1.03100298
#>  [19] -2.71945650 -0.02157240 -1.19135552 -2.09653991 -2.52535647 -0.52121909
#>  [25] -1.57299882  0.10813831  0.07944601 -0.04115277 -1.15445895  1.15936134
#>  [31] -0.42058366  0.54098405  0.49283828  0.18765979  0.42848403 -0.38385140
#>  [37] -0.36736730  0.76094615 -0.97656265  0.46179835 -1.68389264 -2.30868734
#>  [43] -1.96567584 -0.27232749 -2.50513049 -0.49702187 -1.82521913 -1.34443092
#>  [49] -1.15102877  0.56161276 -0.40012691 -2.38668785 -1.48164152 -0.02451490
#>  [55] -2.43052023 -0.54326592 -0.47654396 -0.60814426  0.30504468 -0.40133344
#>  [61] -0.77968060 -0.62566892 -0.54819679 -2.09343531 -0.52970162 -0.29497030
#>  [67]  0.57322953 -1.08362231 -1.44380407 -0.33911262 -0.23307620  0.21202267
#>  [73] -3.30344426 -1.19456595 -1.15949851 -1.07337315 -0.93982739 -2.36976065
#>  [79] -0.95513627 -0.73637215 -0.62412273 -1.62793840 -2.73014358 -0.47593144
#>  [85] -1.29996977 -1.53408727 -2.16958276  0.37654635 -0.50181883 -2.88607525
#>  [91] -2.77538421 -2.00567775 -0.48031840 -1.55571548 -0.75943000 -1.43697169
#>  [97] -1.88675865 -1.08336750 -0.45827061 -1.39718185
#> 
#> $alpha1
#>   [1]  2.18485491  0.62210594  2.32778667  0.55598069  0.86417124  0.77912412
#>   [7]  1.74886382  1.25362313  1.55984526  2.45541210  2.63435031  2.78407738
#>  [13]  2.14240569  0.30742480  2.23236845 -0.37683512  0.46206704  1.78361529
#>  [19]  1.82469392 -0.31680122  0.66064540  2.39337583  2.22721906  0.97701276
#>  [25]  1.87305991  1.14179391 -0.23709296  1.20393339  2.71011440 -0.17830489
#>  [31]  0.26508403  0.61974771  1.10989055  0.24345666 -0.98635295  0.64739530
#>  [37]  1.13703836  0.03711421  1.26675793  0.26045101  1.77064749  1.05637918
#>  [43]  0.68913501  0.15875105  1.91492375  0.37093492  1.58720089 -0.04106049
#>  [49] -0.16244843  1.54789650  0.81558100  1.52157563  0.97815955  2.66035397
#>  [55]  0.71758461 -0.81305011 -0.78244899  1.34109085 -0.44312792  0.90739567
#>  [61] -0.28473737 -1.69080154  1.05130850  2.19197794  1.60387573  2.17286209
#>  [67]  0.47044717  3.45595305  0.21215355  1.57470606  0.75373086  1.01216118
#>  [73]  0.48013971  0.38739275  1.34324636  1.38123071  1.26265368  2.09649383
#>  [79]  1.58702138  0.36238863  1.00117967  1.97800133  1.56934344  2.43074295
#>  [85]  0.31357762  0.82939748  3.04514082 -0.55948553  0.93367542  0.67377552
#>  [91]  1.99183950  2.27626043  0.88986353  1.26021123  0.48997954  3.61816609
#>  [97]  2.95481001  0.51227578  0.95910259  1.19117879
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
