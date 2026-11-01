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
  the reference dose \\(x\*\\) (strictly positive number).

## Details

The covariate is the natural logarithm of the dose \\(x\\) divided by
the reference dose \\(x\*\\), i.e.: \$\$logit\[p(x)\] = alpha0 + alpha1
\* log(x/x\*),\$\$ where \\(p(x)\\) is the probability of observing a
DLT for a given dose \\(x\\). The prior \$\$(alpha0, alpha1) ~
Normal(mean, cov).\$\$

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
#>   [1] -2.33095298 -0.71496827 -0.66470401 -0.19454531 -1.37231352 -0.26473862
#>   [7] -0.56395352 -0.46734948 -1.43847764 -1.31173717 -1.57138129  0.39109077
#>  [13]  0.60813007 -1.39776821 -0.63117995 -2.31938707 -1.88501369 -2.12853817
#>  [19] -1.11978883 -0.53985212 -0.81337642 -0.62436638 -1.16392792 -0.92780149
#>  [25]  0.80361100 -0.46372091 -0.31026751  0.22878917 -0.83680725 -2.42905002
#>  [31] -1.24419159 -0.01606572 -0.26093043 -0.48978059 -1.34281185 -1.31691820
#>  [37] -0.54679468 -0.12630288 -0.55420498 -0.93717916 -1.61512214 -1.46194791
#>  [43] -2.30434306 -2.07100169  0.56449972  1.21004961 -1.98497656 -1.72084948
#>  [49] -2.39074079 -1.84700565 -1.57852621  0.32799819  1.13121769 -1.29904399
#>  [55] -2.73009177 -0.94655786 -1.81705236 -0.22364388 -1.01614996 -2.11662929
#>  [61] -2.01254974 -0.58890629 -1.23665797  0.19828884 -0.81715848 -1.16163876
#>  [67]  0.82307805 -0.64730769 -2.43991260 -1.19727208 -2.08853464 -0.87356764
#>  [73] -1.71145111 -0.19178455 -0.34111188 -1.05802030 -0.61326362 -0.32369473
#>  [79] -0.21881319 -0.64456545  0.63444901 -0.33129386 -2.49487107 -2.19270085
#>  [85]  0.71325241 -1.00428907 -0.51846383 -0.60151519  0.47838122 -0.96215987
#>  [91] -0.57215896 -1.40275696 -0.02838196 -0.80179575 -0.41990770 -1.63654129
#>  [97] -0.79229244 -0.79941944 -2.02058571  0.29724847
#> 
#> $alpha1
#>   [1]  2.18005545  0.94431066 -0.15823353  1.05487028  1.74328059  0.42908415
#>   [7]  0.36594427  1.70027781  1.94624496 -0.53662769  0.90342873  0.16392544
#>  [13]  0.53698582  0.35583143  0.98870184 -0.99951126  3.11815560  2.09407733
#>  [19]  1.60422025  1.28803704  1.66484146  2.14818295  0.75518884  0.64270508
#>  [25]  0.25091501  1.37452777  0.83906797  1.06932353  0.83435665  0.92836772
#>  [31]  0.78527824  1.21009105  0.58897980  0.22861497  0.94978341  1.16547768
#>  [37] -0.68802942 -0.25265141  2.40932073  1.07526846  0.74723560  2.79129647
#>  [43] -0.56410410  2.05088144  0.94972205 -1.16303113  0.19802976  0.09606923
#>  [49]  2.35009504  0.08551369 -0.67225963  0.50058513  0.71120310  2.69266879
#>  [55]  0.83514856  1.05941118  0.83583148  0.17706286  0.40605128  2.76801882
#>  [61]  0.57664641  2.22259480  2.48345788  0.33101619  0.79887818  1.28963841
#>  [67] -2.02042432  1.78427239  2.03947345  0.93128156  1.24058777 -0.91107596
#>  [73]  1.70383009 -0.12856722  1.15662988  0.36145139  0.49166940  1.56351165
#>  [79]  0.91171617  0.11187500  1.64110280 -0.25935061  2.19209658  3.39858069
#>  [85] -0.38458391  0.95795942  1.81827859  1.98454186  0.62947336 -0.99330057
#>  [91]  2.08437052  1.16773796 -0.49845100  0.85096787  0.81130381  1.33262965
#>  [97] -0.38381568  1.75898795  1.34654397 -1.00577905
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
