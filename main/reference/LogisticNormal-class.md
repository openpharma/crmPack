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
#>   [1] -2.39339380  0.68250155 -0.55499361 -1.88513959  0.12208677  0.86391749
#>   [7] -1.75296845 -0.50993627 -0.75223964  0.35802349 -1.64385824 -0.25755882
#>  [13] -1.48548389 -1.77668349  0.57373338  0.54997500 -0.06401664 -0.20300866
#>  [19]  0.66044504 -0.22409568  0.87966008 -1.12653942 -1.42402412 -1.57729859
#>  [25]  1.21777151 -2.63292850  0.31927118 -0.87859221 -0.29424960 -0.52346314
#>  [31] -1.16089180 -2.08156725 -0.88958158 -1.27964297 -1.29179962  0.44656834
#>  [37] -3.58981121 -1.74772679 -0.82815509 -0.05814511  0.55089779 -2.20311651
#>  [43] -1.76687620 -0.88365671 -1.77593507 -0.75406720 -1.28362814 -2.31168729
#>  [49]  0.24362187 -1.12520234 -1.80364451  0.53039681  0.04845885 -1.72832226
#>  [55] -0.12197969 -0.64134811 -2.15968789 -1.22847087 -0.12326777 -0.75851729
#>  [61] -1.72546939 -1.42431622 -0.91803622 -1.48064818 -2.09133418 -0.43854145
#>  [67] -2.14617958 -1.60943731 -2.29440043 -1.09906079 -1.51574956 -0.92255337
#>  [73] -0.63651593 -1.52690075  0.07661426 -0.93083886  0.34074105 -1.24086280
#>  [79] -2.99923893 -0.29199819 -1.70785399 -3.11818706 -1.45003843 -2.90803810
#>  [85] -0.16549691 -0.50584299 -0.26680241 -1.90682036 -1.75793044  0.36360976
#>  [91] -1.43824752  0.46221317 -0.98161549  1.28916101 -0.55177300  0.61937410
#>  [97] -2.08081745 -1.35618350 -1.96026102 -2.54560015
#> 
#> $alpha1
#>   [1]  1.6169455690 -0.1258273938  1.8324412101  1.7323141663  2.4048029700
#>   [6] -0.2558235156  3.6358158197  0.8877949370  1.3332119571 -0.7426485524
#>  [11] -1.0822885301  0.8453835576 -0.0378461289  1.0143981862 -0.8078472885
#>  [16]  0.0940314784  1.3181363767  1.0747962588 -1.6033110248  1.0503796964
#>  [21] -0.1641849483  2.1177369156  2.5302864875  1.4441946539  0.6125652922
#>  [26]  0.2896552621  0.5038046611  0.3318788741  0.3962574793 -0.4355847117
#>  [31]  1.0207848150  1.5170353017  1.2521550429  2.5547156741  1.5830093417
#>  [36]  1.2485837189  1.3434543526  0.5806120411  1.0168494445  1.6940579363
#>  [41]  0.5460826190  0.0495826451  1.0005709833  1.8106942357  0.0526220146
#>  [46] -0.3795684665  1.3636283809  1.1531583748  1.3570927133  0.7611712624
#>  [51] -1.0128428857  0.6368104645  1.6452876599  1.2534957435 -0.0811320300
#>  [56]  0.9606222452  0.7545074868  1.8740551433  0.8173908883  2.6995817519
#>  [61]  1.2022491015  0.4348114926  2.3803461289  0.8457995169  0.6503228080
#>  [66] -0.1398843479  1.9836788040  1.4152871300  3.1686996905  1.1381990393
#>  [71]  0.5427705873  1.6346898295  0.3599933473  1.4198367958  0.1891334036
#>  [76]  1.9334135509  2.5011270737  2.3467530035  3.2678747776  0.1006098562
#>  [81]  0.9617430723  2.1196271993  0.0351360624  3.0115111311 -0.3416806999
#>  [86]  0.0007968804 -0.8872612435  0.6938037651  2.2097406045  0.1419218501
#>  [91]  1.5012696353  2.0267740183  1.2313355850  0.5066836873  1.8457358311
#>  [96]  0.1531265746  2.1047086424  1.6426880401  2.1171975861  2.5536906865
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
