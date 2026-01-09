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
#>   [1] -0.462055221 -0.309653268 -0.803543293  0.371682848 -0.529811301
#>   [6] -1.416830279 -0.634107144 -1.953299222  0.010775761 -0.285276489
#>  [11] -2.138613048  0.642277636  1.622743949 -3.022148629 -0.612930397
#>  [16] -0.722004989 -2.382290773 -0.318771770 -1.211046044 -1.843257196
#>  [21] -1.287030193 -0.641439240 -1.114420482 -0.846545110 -0.335114284
#>  [26] -2.339594369 -2.224777010 -1.566816341 -0.791797270  0.040077820
#>  [31] -2.517809803  0.101186815 -0.381848632 -1.485190423  0.139427073
#>  [36]  0.097653669 -1.303770943 -0.324990079  0.228814386  0.158430591
#>  [41] -1.566969062 -2.376140651 -1.949670006 -1.754041005  0.902180455
#>  [46] -2.067402483 -2.987973880 -2.371987603 -0.862506359 -0.924819742
#>  [51]  0.771482004 -0.737266608 -2.924026571 -0.640261118 -1.311804640
#>  [56] -0.497234815 -0.410577740 -0.614903308 -1.794745174 -0.002459648
#>  [61]  1.367650836 -1.698855747 -1.705491329 -0.869892546 -2.247728131
#>  [66] -0.550984053 -2.186038462 -2.206114417 -0.891360969 -0.555518628
#>  [71] -1.888729859 -0.876430375 -2.555075212 -2.732562322 -1.632542482
#>  [76] -1.922442771 -1.233838038  0.630086608 -0.169110034 -0.585939532
#>  [81] -1.089516076 -0.710983617 -1.525675651 -0.667508703 -2.576314053
#>  [86] -0.342575382 -1.820076103 -1.609176857 -0.355811002 -0.282605047
#>  [91] -0.314991708 -0.321869525 -0.757372061  0.783884741 -0.855446270
#>  [96] -1.330680155  0.047174382 -1.231734777 -1.568990025 -0.879043277
#> 
#> $alpha1
#>   [1]  1.04052877 -0.07250274  0.28272235 -0.10407449  1.28845047 -0.58514450
#>   [7]  0.11497982  0.92211741  0.34399300  1.65865775  0.18097073 -0.55025432
#>  [13] -1.08177676  2.52025815  2.11859486  1.36703676  3.74165385  0.22919595
#>  [19]  2.41851238  1.21958723 -0.12654275  0.80189293  0.07660077  0.60592231
#>  [25]  1.20075375  2.30882064  1.38668337  1.05833029  1.84208921  2.61309149
#>  [31]  1.35987289  0.36211980  1.08367769  0.83249539 -0.22208569  0.26352922
#>  [37] -0.51699888  0.31585251  1.44291690  0.27164848  0.79471411  0.91677464
#>  [43]  2.00229641  1.56636686 -0.46590892  0.58935242  1.92207494  2.65563141
#>  [49]  0.17096463 -0.20494427 -1.32000360  0.38623124  0.94982711  1.16812483
#>  [55]  0.42443272  2.44096484  1.26731442  1.69227854  4.02219715  0.37500353
#>  [61] -0.52742556  2.15951520  0.30739126  0.73257601  1.59682704  2.04765715
#>  [67]  2.16069063  2.89531947  1.82155028  0.94263925  2.81833678  0.25691453
#>  [73]  2.05201918  0.52593040  1.82161369  0.50013795  1.58800658 -0.01865203
#>  [79]  1.87740169  1.48432393  1.15651310 -0.26459592  0.65481978  0.77619749
#>  [85]  1.99515055  0.63410493  1.09595040  1.10407758  1.66303525 -0.51722451
#>  [91]  1.68227532  0.68159589 -0.34465365 -0.84564044  0.98089851  3.02859935
#>  [97]  1.41458670  0.61933616  1.52508412  1.76351987
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
