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
#>   [1] -1.572469137 -0.903796104 -0.096341307 -0.056086620  0.346275573
#>   [6]  0.194304147 -0.878643633 -1.568277207 -1.326129377 -0.246600792
#>  [11]  0.021498581 -2.456846937 -0.745686152 -0.340717242 -1.381289207
#>  [16] -0.765713801 -0.375451965 -1.106100280  0.572738950  0.038889831
#>  [21]  0.386644549 -0.473115592 -3.336990272  0.198761685 -1.069479450
#>  [26] -1.839641010 -0.507423100 -0.991583868 -1.471545359 -2.013912519
#>  [31]  1.054656818 -0.228346330  0.180929324 -1.410750073 -1.742882944
#>  [36] -1.299618748 -0.332512293 -1.669442390 -0.057363093 -1.319501222
#>  [41] -1.428882450 -0.648254059 -0.099515891 -1.110341878 -0.068282017
#>  [46]  0.142498739 -0.998442491 -1.371185992 -0.793853249 -1.884613801
#>  [51] -1.413476955 -1.514431287  1.513259464 -0.277454165 -0.201700967
#>  [56] -0.545536329 -0.150547522 -0.593325154 -3.535073658 -0.729079595
#>  [61] -1.344034859 -0.396306655 -0.623568526 -0.082068591 -1.545585496
#>  [66] -1.534431178  0.637350688 -0.716965526 -3.549521016 -0.744335640
#>  [71] -1.480478418  0.002438682 -1.625640460 -1.609602051 -0.336705662
#>  [76] -0.234533434 -0.579557235 -1.813790840 -0.692783247 -1.037738456
#>  [81] -0.248549901  0.033037637 -0.317158290  0.207096559 -0.584969966
#>  [86] -0.832998263 -2.435083033  0.763343185 -0.819555551 -1.363603234
#>  [91]  0.036260855 -0.132496716 -0.142894171 -2.187695087 -0.043415986
#>  [96] -0.747968648 -1.160161766 -1.240530698 -1.082364837  0.455561012
#> 
#> $alpha1
#>   [1]  1.19225027  2.08349532  0.23017284  0.14831522  0.78181228  2.30438581
#>   [7]  0.92254555  1.85590089  0.02532635  0.49199048  1.05097034  2.51456253
#>  [13]  1.01073795  0.78957512  2.63974105  1.90142773  2.88029642  1.54855112
#>  [19] -0.65866099  2.26606739  1.55437154  1.69622361  0.90447884  1.21985079
#>  [25]  0.04716405  1.08031585  0.72152975  2.17158649  0.03790755 -0.48022643
#>  [31] -0.07646654  1.92322362  0.74811439  0.83716533  1.21080841  0.93308273
#>  [37]  2.03724822  1.31865975  2.64152436 -0.03775686  0.84011491  1.23212085
#>  [43] -0.25568393  2.31949566 -0.66966133  0.62246035 -0.04203259  2.67810643
#>  [49]  1.31475933  1.10159568  0.67143481 -0.20045238  0.55394771 -0.12016871
#>  [55]  0.21670203  0.40833505  0.67430619  2.08902953  2.87278983  2.61937882
#>  [61]  1.19280070  0.88927313 -0.02049899 -1.05651924  2.53308380  1.00364772
#>  [67] -0.01249382  0.52298329  2.07361115  2.53058273  0.63756377  2.30429677
#>  [73]  2.09972313  0.83619286 -0.02664361  0.32046580 -1.35770340  1.84687227
#>  [79]  1.41890365  1.57551076 -0.49718604  1.05527860  0.79549914  0.25413976
#>  [85]  1.09506149  2.84828778  2.54794555  1.89350546  1.89341230  1.56744918
#>  [91]  1.42105598  1.04276909 -0.08182417  0.66307345  2.46176307 -0.21481042
#>  [97]  1.24217405  2.24098887  0.18504416  0.32595416
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
