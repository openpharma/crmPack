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
#>   [1]  0.83114984 -2.39193020 -1.43862158 -0.56768529  0.12791161 -0.38929961
#>   [7]  0.59319654 -0.29541847 -1.74878970 -0.95124209 -0.93142309 -0.25285855
#>  [13]  0.62346402  0.13187426 -0.64564354 -1.54087116  0.37940256 -2.40833342
#>  [19] -0.33092483  1.75282776  1.36024821 -1.57003291 -2.10348693 -1.59002374
#>  [25] -1.42138363  0.18394835 -1.47937590 -0.07542564 -0.46981504  0.10592122
#>  [31] -0.89704538 -0.86869021 -0.42001103 -2.96585219 -1.58963306 -1.20429162
#>  [37] -0.63860084 -0.88114634 -0.20809013 -2.74363610 -2.25585562 -0.17377528
#>  [43] -0.34928321 -0.78250368 -1.00174068 -2.39319058 -0.07257599 -0.91073210
#>  [49] -0.41189806  0.63418670 -1.80437392 -0.98901638 -0.36419029 -0.92945751
#>  [55] -0.73299996  0.24771667 -0.49514165 -0.45733481 -0.79075362 -0.36524845
#>  [61] -2.14990395 -0.34597179 -0.29843606 -0.58752626 -2.59686254 -0.03165751
#>  [67] -1.80939844 -0.59577467 -0.62666954 -0.56568460 -0.69564637  0.68707668
#>  [73] -0.23670025 -1.29916471 -1.02750103  0.37280427 -1.16985426 -0.63327507
#>  [79] -1.14697003 -0.40262103  0.13025368 -1.37388497 -0.16085129  0.67777436
#>  [85] -3.31044434 -2.00423634 -1.57870934 -0.95044633  1.07681726 -1.81599947
#>  [91] -1.56559194 -0.21172757 -0.65852862 -0.14870920 -0.32884058 -0.23226911
#>  [97] -0.17070160 -0.91278856 -0.74205966 -2.53075437
#> 
#> $alpha1
#>   [1]  0.12679427  1.65126356  0.90992535  0.72732767 -0.31459019  0.53892615
#>   [7] -0.13916768  1.39464828  1.67590183  1.54934864  0.65053429  1.00136763
#>  [13]  0.47976439  0.70216822  2.07030734  2.47009383  0.47184828  2.07471157
#>  [19]  0.83057687 -0.35567163 -0.22659425  2.00505309  0.63487132  2.23303444
#>  [25]  1.01880267 -0.57166659  2.24374729  0.34629352  0.39071249  1.04081652
#>  [31]  1.49154010 -0.29925024  1.46393623  2.30926619  1.42757539  0.22084030
#>  [37]  1.89460368 -0.01850355  1.26742618  2.45296517  0.35775456 -0.15037559
#>  [43]  0.63023733  1.41207155  0.87739152  1.83416901  0.67509205  0.86985004
#>  [49]  0.52885987 -0.50935544  2.16187021  0.27263594  1.35443925  1.47301113
#>  [55]  2.22560312  1.28696157  3.14244888  1.48079589  0.72072026  0.92368241
#>  [61]  1.01854774  1.83946817  1.99526460  1.50870186  1.41748507  1.06904825
#>  [67]  1.12425183  0.76999056  1.98355563  1.78924115 -0.33876633  0.82331493
#>  [73] -0.63647954  2.13382851  1.38592719  0.07876736  0.16738096  1.07110620
#>  [79]  1.71986213  0.48257436  0.59975994  1.62486552  2.04118058  0.53498293
#>  [85]  1.89499278  1.90660501  0.59055142  1.93202033 -1.02757035  1.31436747
#>  [91]  1.45929907 -0.60236825 -0.35961366  1.62630633  1.42172430  0.08260956
#>  [97] -0.61868072  1.59068652  1.13441616  1.38263046
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
