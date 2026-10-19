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
#>   [1] -1.40010397 -2.27681089 -0.09649558  1.14687434 -1.42900689 -1.29407196
#>   [7]  1.03370369 -0.45877816  1.30793436 -2.12592747 -0.27695840 -0.47695283
#>  [13]  0.64682733 -0.50884278 -2.14856940  0.31633522  0.41321368 -1.61295304
#>  [19]  0.41667969 -0.30160876  0.45291103 -1.66271032 -1.95821644 -0.80379255
#>  [25]  1.58264801 -2.83509478  0.37889431 -1.52162501 -1.02149954 -0.34808514
#>  [31] -0.25049024 -0.63139299 -1.91110074 -1.10386120 -1.83823226 -0.25781386
#>  [37] -1.06724979 -1.62625995 -0.48585014 -1.10932977 -0.28925489 -0.89827518
#>  [43] -0.75327224 -0.71152906 -0.79554978 -2.63504732  0.19731285 -1.26577479
#>  [49] -1.38703451  0.19398109  0.29911808 -1.16714804 -0.07651733 -1.09252273
#>  [55] -1.51024805 -2.42451068 -1.42860845 -0.93063063 -0.81012117 -2.44856735
#>  [61] -1.53144728 -1.30601794  0.69266123  0.05112277 -0.21831493 -1.09118580
#>  [67] -0.32388260 -0.83274589 -1.16547149 -0.51603942 -1.15340981 -2.14159739
#>  [73]  0.70184905 -1.72377326  0.78155576  0.75684492 -1.00574374 -2.41236276
#>  [79] -1.24410959  0.67039755  1.07784890 -1.73014845 -1.72797240 -1.22222309
#>  [85] -0.64133947 -0.23635600 -1.96022316 -0.26726462 -0.02800029 -0.72381185
#>  [91] -1.39279274  0.31506536 -1.93687240 -1.35170696 -1.20332102 -1.22463180
#>  [97] -0.20698648 -1.92070188 -1.13128783 -1.15182213
#> 
#> $alpha1
#>   [1]  0.166107378  3.035192001  0.812782718 -0.773129332  1.408784676
#>   [6]  1.567226399 -0.317267461  1.486259672  1.908059639  1.348833303
#>  [11]  2.341052016  0.676565811  0.061446897  0.027806360  0.731687845
#>  [16]  1.980931248 -0.275275524  1.908953095  0.001194458  1.022439376
#>  [21]  0.213698822 -0.197765552  1.724284124  1.001850253 -0.867459708
#>  [26]  1.946980173 -0.311731621  2.153551257  1.517331279  0.762871440
#>  [31]  1.331630204  0.807206310  1.583040511  0.036033227  2.146177021
#>  [36]  1.571646279  0.522884880  1.554596841  0.777537613  1.206608526
#>  [41]  0.960211137  1.662800014  1.020454026  1.194590712  1.506096133
#>  [46]  2.692296559 -0.058203629  1.847005587  2.343613404 -0.318841180
#>  [51]  0.817394330  0.553984019 -0.987083035  0.601205384  1.561612721
#>  [56]  2.314155356  1.576500838  1.347569728  0.814371991  2.066702992
#>  [61]  1.888051958  1.191717576  0.858708273 -0.582823967  1.588967180
#>  [66]  0.453541243  2.154066767  1.061365939  0.172215331 -1.183666987
#>  [71]  1.201360940  2.035204887  0.608773088  1.695258856 -0.803551032
#>  [76] -0.201441809 -0.291081149  1.360414241  1.646028441 -0.334861716
#>  [81]  1.335210939  1.349994730  1.684094898 -0.563044578  2.319001998
#>  [86]  2.269198055  2.697341730  0.175094794  0.415072207  1.174068832
#>  [91]  1.121815924  0.322425201 -0.098740940  0.742277014  2.205583942
#>  [96]  2.055426332  0.639215738  1.704220644 -1.424067245  1.122305216
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
