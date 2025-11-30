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

[`ModelLogNormal`](https://openpharma.github.io/crmPack/reference/ModelLogNormal-class.md),
[`LogisticLogNormal`](https://openpharma.github.io/crmPack/reference/LogisticLogNormal-class.md),
[`LogisticLogNormalSub`](https://openpharma.github.io/crmPack/reference/LogisticLogNormalSub-class.md),
[`ProbitLogNormal`](https://openpharma.github.io/crmPack/reference/ProbitLogNormal-class.md),
[`ProbitLogNormalRel`](https://openpharma.github.io/crmPack/reference/ProbitLogNormalRel-class.md),
[`LogisticNormalMixture`](https://openpharma.github.io/crmPack/reference/LogisticNormalMixture-class.md).

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
#>   [1] -2.29319595 -1.36176234 -2.27852138 -0.14867948 -1.55790853 -0.04979570
#>   [7] -1.80860710  2.01982670 -1.22804045 -1.12217272 -0.54750611 -0.92629458
#>  [13] -0.79035248  0.87607665  0.47464947 -1.12670206  0.22962661 -1.11127168
#>  [19] -0.38247674  0.17367483 -2.35406952 -0.64967659 -0.48083863  1.15464025
#>  [25] -0.30559214  0.77275623 -0.92994720 -2.66848805 -1.06495015 -0.15174667
#>  [31] -1.67966644 -1.14209386 -2.03718125 -1.59024798 -1.69919437 -1.84185009
#>  [37] -1.65749946 -0.91294292 -1.51096609 -1.22281500 -0.81578335 -2.09987335
#>  [43] -0.37589224 -1.54255706 -1.60448101 -1.68584474 -0.89965796 -2.37301616
#>  [49]  0.37399483  0.03902754 -0.94205038 -1.19314469 -1.83651608 -0.35952264
#>  [55] -1.52793385 -1.90035530  0.90720722 -0.86282694 -0.68206930 -1.11848837
#>  [61] -0.21320747 -0.81366103 -2.25391644 -1.56991310 -2.14162494 -1.47863236
#>  [67] -0.96453394 -3.29405015 -0.16851195  0.56987901  1.27565096 -0.90230884
#>  [73] -1.44124625 -1.34256804 -0.41554769 -2.19193567 -1.78920100  1.90398491
#>  [79] -0.11584133  0.70163119 -2.41517522 -0.65507077 -1.15411863 -3.20672061
#>  [85] -0.66918604 -0.50009738 -2.10038894 -0.83865723 -1.43622286 -2.62543063
#>  [91] -0.44842787 -0.41417086 -0.33665914 -1.35219622 -0.61162577 -0.06814888
#>  [97]  0.13157251 -1.64212753 -0.57713242 -0.03391788
#> 
#> $alpha1
#>   [1]  1.83731663  1.80954313  1.77938575  1.43646906  2.33984173 -0.01449417
#>   [7]  1.59963018  0.18365788  0.33909955  1.39452599  0.74559619 -0.20024502
#>  [13]  0.69371856  0.45020123  0.46946535  1.79388429 -0.49396672  1.52135033
#>  [19]  1.74164894  1.36941101  1.47984466  2.24188663 -0.36053522 -0.74281390
#>  [25]  1.74845173 -1.07853447  1.50822660  1.40768876  0.85654804  0.62451867
#>  [31]  1.39286784  1.19108528  1.26080318  2.32023025  2.22087506  2.85411182
#>  [37]  1.49464961 -0.28046505  0.82856004  0.59672094 -0.87468540  2.47290083
#>  [43]  0.17425669  0.91549183  0.05017705  2.52851052  1.53684962  0.26814239
#>  [49]  1.35522470 -1.05022906  0.05307469  0.61884110  2.61611203  0.16241611
#>  [55]  2.30914988  1.92776460  0.74781689  0.28968032  0.77618098  0.55630657
#>  [61]  2.89215426  0.40939324  1.93575301  0.57885366  1.98537013  0.41649019
#>  [67]  0.93541992  2.93950609  0.59470865  1.11423735  0.21140251  0.37215351
#>  [73]  2.03331254 -0.16618842  0.45607447  0.66364682  1.82015812 -0.66693633
#>  [79] -0.67647534 -0.34176770  2.32334374  0.05761272  1.28269968  3.62234104
#>  [85]  2.57891259  0.60449105  2.54425802  0.19455411  2.01594438  1.42583151
#>  [91]  0.72085397  0.71833202  0.41907746  2.74335009  0.11311167  1.49809556
#>  [97]  0.46320789  2.00156718  0.62023372  0.58953367
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
