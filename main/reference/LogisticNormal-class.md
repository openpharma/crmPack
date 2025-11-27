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
#>   [1] -0.54656705 -2.27550528 -2.67378572 -0.94231771 -0.85495375  0.63720765
#>   [7] -0.49951083  0.64235854 -2.38579432 -0.23140951 -1.33448031 -0.75271688
#>  [13] -0.35701259 -1.97564054 -0.35120320 -2.03446676  0.25973392  0.62363291
#>  [19] -1.18770857 -0.21336288 -0.56896106 -1.16759968 -1.46887523 -2.31145735
#>  [25] -1.72188425 -1.02772663  0.96074297  1.03438441 -1.16663602 -2.38189019
#>  [31] -1.57199885 -0.25545620 -1.35520732 -1.31472484 -0.98583036 -1.04912121
#>  [37]  0.43530899 -0.90037568 -3.66969773 -0.09399829  0.42704987 -0.59714277
#>  [43] -0.31460909  0.67068773 -2.56478336 -0.35675041 -0.15226513 -0.68410173
#>  [49] -1.75646650 -2.45227030 -1.50386808 -1.24443027 -0.20536780 -0.21368658
#>  [55] -1.18203916 -1.86025330 -1.73513194 -1.05387314 -1.27379299 -0.29974466
#>  [61]  0.25398482 -2.73045486 -0.77776699  2.73025205  0.74296359 -1.45204901
#>  [67] -0.88478996 -2.59187318  1.02081342 -1.29595361  0.23153700 -1.55579935
#>  [73]  0.16743420 -1.72345184 -1.75885682  0.32433995 -0.62947736 -0.57734093
#>  [79] -1.38060426 -0.31610518  0.36893077 -0.67297785 -1.13075621 -1.74652525
#>  [85]  1.41306232 -0.59240890 -2.33083011 -1.45441524 -0.75839346 -0.08219886
#>  [91] -0.21167023 -0.01709179 -1.75164494 -0.58968282 -1.69274486  1.46506738
#>  [97] -0.88939119 -0.58969215  0.10027920 -1.10999929
#> 
#> $alpha1
#>   [1]  1.11432874  1.66657732  2.77698566  1.23473927 -0.74312118  0.08226345
#>   [7]  1.03414935  0.93182784  1.52663352 -0.96998518  1.90770474  1.83305809
#>  [13]  1.32600587  0.61266684  1.83230253  1.05871325  0.82428777  0.61940511
#>  [19]  1.28906641  0.63100523  1.22466789 -0.48158276  2.30344516  1.50227593
#>  [25]  0.63649557  0.75581001 -0.67082777  0.96324964  1.04566328  1.18597901
#>  [31]  1.71422082  1.26611093  1.17743599  0.27804591  1.19589051  0.39663059
#>  [37]  0.10175675  0.59131108  3.19163658  0.74286324  1.34850418  0.06254716
#>  [43]  1.23298162  1.41914352  0.70487566  0.69196245  0.78306521 -0.13228083
#>  [49]  2.14180637  2.21791873  1.57959588  1.61923523  0.25868660  0.36588899
#>  [55]  0.69395137  2.10994119  0.86791386  0.86002766 -0.20924923  0.06894742
#>  [61]  1.10799145  2.98791366  1.21985187 -0.58936315 -0.13150033  0.61379338
#>  [67]  0.03342256  1.59500178 -0.60056282  0.79865392  0.47333204  0.32079445
#>  [73] -0.14322802  1.26553901  0.85531681 -0.38560080  0.29844650  0.93215822
#>  [79]  2.06282352  0.39653574  0.45144801  3.42073757  0.59222126  2.43062216
#>  [85]  0.29680876  1.57389251  2.66519145  1.07058616  1.10409962  2.24921746
#>  [91]  0.53719233  0.32745936  0.65069249  2.26657993  2.33470088 -0.43202526
#>  [97]  0.97217326 -0.23095301  0.20497435  0.59610420
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
