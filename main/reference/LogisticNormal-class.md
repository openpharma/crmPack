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
#>   [1] -0.07206581  0.17998547 -3.19769815 -0.74542357 -0.18237456  1.32703447
#>   [7]  0.47784501  0.47311181  0.01350287 -0.55675648 -0.96430740 -0.72269678
#>  [13] -1.24430235 -0.24155252 -2.89777057 -1.16619846 -2.05218598 -0.46091851
#>  [19] -0.93900994 -2.34228892 -1.00373668 -0.14781731 -0.63598549 -2.49410124
#>  [25] -1.02812457 -1.53438251 -2.23940939 -1.72843383  0.49092492 -1.05144792
#>  [31] -1.14049154 -0.95101167 -0.18621364 -0.83374538 -0.61026423 -1.55113964
#>  [37] -2.72366239 -0.28079538 -1.41936151 -0.77670200 -0.75938342 -1.10304611
#>  [43]  0.03317201  0.68784135 -2.25987697  0.02153044 -1.97893876 -0.47951905
#>  [49] -0.83666811 -0.61349823 -1.42117824 -2.83242397 -1.65640836  0.72406895
#>  [55] -0.32623383  0.68579745 -0.57443207  1.11339342  0.50946429 -0.70006659
#>  [61]  0.10251402 -1.56478341 -0.92039400 -2.19262813 -1.64652131 -1.58592487
#>  [67] -2.10858192 -0.06036024 -0.14038478 -0.99370864 -0.94075054 -1.19349137
#>  [73] -0.61685700 -0.55645810 -1.21584401  0.90347996 -1.44674499 -0.29077879
#>  [79] -2.26828376 -2.12267076 -2.48188791  0.19427076 -0.83278202 -1.82729493
#>  [85] -0.18569969 -1.61666720 -1.28468603 -0.97253317 -1.81707177 -1.19920706
#>  [91] -2.53993708  0.51767951 -1.80862818 -1.17820839 -1.60013370 -1.31247611
#>  [97] -0.30917745 -0.88960816 -1.01353967 -0.49974314
#> 
#> $alpha1
#>   [1]  1.21996854  2.28162762  3.15497445  2.05883678  1.80631777 -0.58702273
#>   [7]  1.94146870  0.63154400  1.14465481 -0.28961792  2.46991925  2.13004004
#>  [13]  0.16885463 -0.14965885  1.35284270  1.46266333  1.30995584  0.21129740
#>  [19]  0.67902021  0.75195966  1.41378193  1.04492804  1.39223827  2.27957354
#>  [25]  0.98105966  0.98310420  0.80820229  1.07762295  0.18135738  0.43374209
#>  [31]  0.90957989  0.34915480  1.10078756  2.07056718 -0.25481286  2.00299180
#>  [37]  1.19417089  1.88221677  0.63690241  1.31452714 -0.15935804  0.85460387
#>  [43]  0.65211027  0.49694593  2.36863923  0.67982128 -0.20211622  1.15601872
#>  [49]  1.88814703  1.01543813  1.49347000  3.60095401  1.91324972  0.74601159
#>  [55]  2.35010294  1.49467808  0.75962549 -0.91077022 -0.21493162  0.96535452
#>  [61]  0.96875807  1.08929971  1.72111325  1.61122874  1.49839396  1.60639269
#>  [67]  1.92678227 -0.76890518  1.84614094  0.71827968  2.54868818  0.92242505
#>  [73]  0.11303322  0.39040887  0.15524828  2.22230634  0.58157300  1.59192103
#>  [79]  0.39283852  0.41527746  3.03984836 -0.61261826 -0.58700332  1.59205823
#>  [85] -0.19579250  0.00234498  1.65456497  1.53762906  1.30200392  0.74773226
#>  [91]  2.11826281  2.37700816  2.35075164  0.01525079  0.71816714  1.89202882
#>  [97]  1.07535631  2.02716741  0.53126830  0.89734134
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
