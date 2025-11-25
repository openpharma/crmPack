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
#>   [1] -1.12963516 -1.79181855 -0.69561148 -1.27960485 -2.03056624 -0.11514706
#>   [7]  0.23670270 -0.34954414 -0.29839600 -0.12669784 -0.18537269 -0.51089413
#>  [13]  0.78910542 -1.16905747 -1.75596367 -0.61016087 -0.44763692 -1.66997246
#>  [19] -2.57348655 -3.15377327 -2.90892342 -1.62110367 -0.37455080 -0.35765475
#>  [25] -1.55712476 -0.06370090 -0.68940373 -1.83934042 -0.98474542 -0.18563115
#>  [31] -0.65173019 -1.91512179 -1.04914181 -0.55822073 -1.05702719  0.47648120
#>  [37]  0.75608798 -1.46864924  0.44523629 -2.61352331 -1.02962576 -0.65060782
#>  [43] -0.88237995 -2.19549961 -0.17178415  0.50375026 -0.75568428  1.12513180
#>  [49] -1.27405811 -1.06062724 -1.77981745 -1.16521998  0.77995919 -0.97981539
#>  [55] -1.84928434 -0.33368787 -2.70161297 -1.26482543 -1.90442772  0.90890582
#>  [61] -1.47567893 -1.13815739 -1.74718948 -2.80567647 -0.93491520 -1.00843688
#>  [67] -0.91536349 -1.25904732 -1.50942267 -0.89820318  1.41384445 -0.82393034
#>  [73] -0.12213678 -2.36116451 -1.76734405 -0.86464513  1.19733713 -1.10798461
#>  [79] -0.93863791 -0.86696269 -1.18106949 -0.46245147 -0.74983954  0.22845149
#>  [85] -2.58449524  0.45667690 -0.34282558 -2.28890359 -1.30528583 -1.76521138
#>  [91]  2.12543400  0.60062515 -0.02905021 -0.30181355 -0.44781393 -1.53972882
#>  [97] -1.16635923 -1.35908085 -0.40690400 -1.84890954
#> 
#> $alpha1
#>   [1]  1.51101690  3.08659302  1.55401638  0.53884544  1.52195514  0.28903458
#>   [7]  0.23826552  1.49898015  0.82286718  1.29891323  0.73889219 -0.33813397
#>  [13]  1.56110672  1.60392663  1.57014017  1.37579929 -0.45575141  1.62625384
#>  [19]  2.58457769  2.75745577  0.85923003  1.99081678  0.14767917 -2.29279922
#>  [25]  0.62683844  0.88912992  1.65240101  2.59938251  1.21619771  1.42334124
#>  [31]  2.17216262  2.99360839  0.92175837  1.21653755  0.40278114  0.86689457
#>  [37]  0.40230075 -0.06741944  0.45325119  2.05386284 -0.13883119  1.34094126
#>  [43]  0.43461830  2.94661228  0.98379111  0.59145680  1.53022687 -1.26176778
#>  [49]  1.81284152  1.14977967  0.89960895  1.11548239 -0.86744695  1.59610630
#>  [55] -0.82238385 -0.16199583  0.89801425  1.27055868  2.03994007 -0.79677813
#>  [61]  0.65635977  0.62567976  2.18874714  2.55017975  1.58036843  0.74522975
#>  [67]  2.33634134  0.95090023  2.11603011  0.43417017  0.99172587  0.42128317
#>  [73]  2.35735343  2.74014695  2.09898021 -0.02667874 -1.27784637  0.93096588
#>  [79]  0.47026879  1.56422982  0.03561591  1.01310714  0.81968170  0.03676974
#>  [85]  0.93327289  0.68207024  0.30216475  0.83509900  1.80016660  1.23122176
#>  [91]  1.21597731  1.31578446  1.89926661  1.60067528  0.82435509  2.49367217
#>  [97]  0.82097726  3.35321994  0.68667165  0.68722904
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
