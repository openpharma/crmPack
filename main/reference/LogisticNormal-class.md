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
#>   [1] -2.50838709 -1.95995335  0.41263755 -3.11671227 -0.46649526 -0.64695853
#>   [7] -2.45433685 -0.37244204 -0.67883960 -0.81134079 -1.71513387 -1.66388175
#>  [13] -2.08202705 -1.42437422 -3.64844624 -0.51484581 -0.98799407 -1.72165793
#>  [19] -1.28700828 -1.26593386 -0.35826577 -3.12255970 -0.18784189 -0.01262584
#>  [25] -0.98261495 -1.63531046 -2.44078155 -1.81845389 -0.78861178 -1.65749997
#>  [31] -2.97727730  0.10390021  0.34070239 -0.67546380 -0.59636561 -2.69809102
#>  [37] -1.25133140 -0.56907708 -1.84060233 -0.30564792  0.45571142  0.29083166
#>  [43] -1.63605461 -0.12457451 -0.97539716 -0.37322938 -0.47257930 -0.41750019
#>  [49] -2.37752426 -0.38757235 -1.36414514 -0.92218572 -0.38671362 -0.13464865
#>  [55] -0.62456601 -1.22835017 -1.61344454 -1.47388844 -0.83948685  0.04578898
#>  [61]  0.23550611  0.28857001  0.31113058 -2.26333644 -0.07017361  1.05479844
#>  [67]  0.64130780 -0.64701695 -0.14440432 -1.12271256 -0.10717438  1.70797392
#>  [73] -0.76622437 -0.73472071 -0.47576128 -1.00832108 -0.79017612 -1.10681059
#>  [79] -1.23572452 -1.36322775  1.18159716 -1.42027914 -0.04527429 -0.37941303
#>  [85] -0.27181021  0.94043124 -1.89401692 -2.85877485 -2.36770145 -0.58803206
#>  [91] -2.12080931 -0.77331908 -2.04465458 -1.08803519 -1.79050725 -0.25806566
#>  [97] -0.06699001 -2.10389868  0.38476998 -0.05275921
#> 
#> $alpha1
#>   [1]  0.048075888  1.345010827  0.685287860  2.045187536  2.245519935
#>   [6]  0.739700472  1.605322571  0.475169537  0.392120369 -0.910088658
#>  [11]  0.107186450  0.790637921  1.740553216  0.197420835  3.237874102
#>  [16]  1.941119646  0.524663764 -0.208931919  1.300720602  2.264266798
#>  [21]  0.847392646  2.918477825  1.419157101 -1.441346202  0.787806740
#>  [26]  1.112394960  1.478833985  0.977859826  1.548905080  0.640110777
#>  [31]  1.483197574  1.730822610  0.083531290  1.688365851  0.394625936
#>  [36]  1.631244946  1.751303101  0.036227058  2.263008416 -0.609536525
#>  [41]  0.265759937 -0.833165746  1.106393457  1.352152495  0.229389171
#>  [46]  1.532095814 -0.787529485  0.259316939  1.490540417  0.635902027
#>  [51]  1.268994162  1.373639243  1.653890595 -0.053503836  1.321788930
#>  [56]  0.383248466  2.454759718  1.813994315  1.455035207 -0.215370790
#>  [61]  1.242779675  2.205750623  0.352558362  2.416439357 -0.283004341
#>  [66]  0.884224595  0.572446638 -0.175537059  1.061536178  0.944590356
#>  [71] -0.140763953 -0.523661453  2.533506880  1.876693599  0.149818298
#>  [76]  2.057493719  1.727423049  1.465708391  0.921819122  1.131334884
#>  [81] -0.794174751 -0.576741738  0.679373379  0.790525877  0.009992514
#>  [86]  0.508074450  0.876903357  1.289590378  1.245086639 -0.876886495
#>  [91]  0.746942267 -0.045208212  3.145055494  0.822441174  2.014543549
#>  [96] -1.406099095 -0.103448056  2.454800725 -0.406440948 -1.386447271
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
