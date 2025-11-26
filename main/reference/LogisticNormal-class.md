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
#>   [1] -0.746299509 -0.727931610 -2.110189259 -0.882277025 -2.532833357
#>   [6] -1.849426042  0.037183011 -0.995598703  0.098160325 -0.825219771
#>  [11] -1.705558420 -1.026347051 -2.134653987  0.804792151 -1.770074206
#>  [16]  0.659078853 -1.424742436  0.372823508 -0.466317085 -0.064145627
#>  [21] -1.820473720 -0.727905675 -0.454709735  1.120924557  0.180526676
#>  [26] -1.020575112 -0.382390899 -1.202062102 -0.279862561  0.228713963
#>  [31] -1.034802842 -0.809281849 -1.250279332  0.061274961 -0.581991995
#>  [36]  0.900316193 -2.929831124 -1.534953479 -0.329176463 -1.489638393
#>  [41] -1.123260686 -1.277983507 -0.299593591 -1.044672979 -0.275687125
#>  [46] -0.059742596 -1.293819622 -0.500207718 -2.062305365 -0.849834503
#>  [51] -1.426081547 -1.121266087 -1.910666837 -1.857433017 -1.725370231
#>  [56]  0.011582187 -1.465137089 -0.798731499 -0.333407281 -0.238771193
#>  [61] -1.992398999 -0.872546820 -0.467675756 -1.416911704 -0.192774977
#>  [66] -1.129815532 -2.546136106 -0.294330350 -0.909117317 -1.379066661
#>  [71] -1.579270043 -2.584030170 -1.050944958 -0.809036369 -0.432047777
#>  [76]  0.189911473  0.005347867 -2.205184063 -0.918443269 -1.503968094
#>  [81] -1.247186748 -1.210166808 -0.546977407 -1.150640599 -3.497392497
#>  [86] -2.113753537  0.435459043 -1.595230398 -1.897136925 -0.994006709
#>  [91] -0.604405773 -1.866311675 -0.846222420 -0.459028638 -1.065930994
#>  [96] -1.795471441 -0.183260235 -0.641150567 -3.042152636 -1.351099922
#> 
#> $alpha1
#>   [1]  1.53311076  0.23117217  0.79467854  0.64464665  2.34747653 -0.12857743
#>   [7]  1.52023060  2.48678891  0.33246662  1.77692168  0.26282943  2.38086313
#>  [13]  1.15963842 -0.84638025  1.12611092 -0.10449004  1.55267300  0.38900542
#>  [19]  1.63473582  0.28126972  1.39366736 -1.52211538  1.36786693 -1.03997464
#>  [25]  1.30541392  2.54072138  0.57424478  1.18100117 -0.14912055  0.60972096
#>  [31]  1.92773390  1.21883894  1.14050110  1.67125297  2.51763169 -1.10434457
#>  [37]  1.55195938  2.35895654  1.03575828  1.36407752  1.60386491  1.37187376
#>  [43]  0.69408127  2.04726255  1.27136216  1.33741785  1.65896174 -0.82972487
#>  [49]  0.16581849  1.27461088  0.79621882 -0.04345578  1.53337086  2.99808753
#>  [55]  1.36710901  1.37536315  2.59140754  0.60042535  0.41708385  1.26969531
#>  [61]  2.14102381  0.77546409  1.14037929  2.16909348  1.99241856  1.62763675
#>  [67]  1.99266753 -0.53055381 -0.57715610  1.55315349  0.53269613 -0.14882113
#>  [73]  1.58001412 -0.20080176  1.27412179  0.46395954  0.66961778  1.19942486
#>  [79]  0.19362176  0.17485237  1.23877081  1.40207575  0.64512187  1.75760940
#>  [85]  1.70135026  1.43983715 -0.16920863  0.07377487  1.44727675  0.93508368
#>  [91]  2.37026326  3.09398554  0.90004171  1.38467740  0.59756555  2.10667352
#>  [97]  2.00052914  1.03578977  1.77850271  2.35984582
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
