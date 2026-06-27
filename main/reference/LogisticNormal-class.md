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
#>   [1] -3.18936454 -1.14302559 -1.10414183  0.18195022 -2.31547774 -1.75028424
#>   [7] -1.71103533  0.55924014 -1.23156643 -0.12457626  0.09759892 -1.31792356
#>  [13] -0.13114221 -0.22604922 -0.94867041 -0.15936319 -1.04037306 -2.28359205
#>  [19] -1.12789185  0.29527605 -1.43252934 -2.30871330 -0.37478311 -0.89937840
#>  [25] -0.96566385 -1.37298870 -1.28770941 -0.98205477 -0.33311933  0.22042539
#>  [31] -1.84018607  0.68219897 -0.26680800 -1.23770212 -1.77940836 -0.41848912
#>  [37] -1.68105110  0.03350909  0.13429213 -1.56942458 -0.66809471 -0.41548073
#>  [43] -0.86790907 -1.30803387 -0.27014605 -1.08497836 -1.26775599 -1.12141477
#>  [49]  0.30621611 -1.06826134 -1.18994545 -0.71374000  0.16300451 -0.87281877
#>  [55] -1.68144019 -0.52984594 -0.13590478  0.54588554 -0.27820443 -0.90247149
#>  [61] -2.31792585 -1.28543176 -0.70180512 -1.14463202 -0.33251599  0.21386031
#>  [67] -1.19563503 -0.95231680  0.56751087  1.37354030 -1.79844895 -1.73541485
#>  [73] -1.96483829 -1.78277708 -1.58499426 -2.43695107 -0.11707172 -0.14157374
#>  [79] -0.28302920 -0.39901956 -0.46279780 -0.72811961 -0.95665838 -2.80545745
#>  [85] -0.96401079 -2.19460274 -0.77195745 -0.92104825 -0.44889326 -0.46282940
#>  [91]  0.93522622 -0.34354803 -1.33503397 -1.15888523  0.94606612  0.81629156
#>  [97] -2.66977073 -0.85214032 -1.76488468 -3.34995911
#> 
#> $alpha1
#>   [1]  2.36318430  1.47967133  2.26737451 -0.93563896  1.06297429  1.22713679
#>   [7]  1.80026146  0.59424463  1.76438910  0.80234163  0.51638439  1.69987369
#>  [13] -0.30577367 -0.58848701  0.63888618 -0.32679601  0.93885025  2.71489509
#>  [19] -1.38789073 -0.11007913  1.54958716  2.38527785  1.03907406  1.22161958
#>  [25]  1.12283881  1.19556915  0.92257972  0.19446470  0.61291085  0.55772405
#>  [31]  2.22269143 -0.62969261 -0.32217225  0.42850389  1.99049873  1.25387048
#>  [37]  0.49701757  1.18882658  1.22545149  1.36250588  1.21784666 -0.52624699
#>  [43]  0.41752438 -0.19326798  2.33885933 -1.32548473  1.16788117  1.40645125
#>  [49]  1.40862980  2.03144282  1.86180019  1.93192597  1.25476976  1.35164268
#>  [55]  1.83186191  1.10133233  0.42899084 -0.17100129  0.83174532  0.34353142
#>  [61]  2.53331875  0.77760751  1.33764061  2.26633467  1.31662950  1.20247267
#>  [67]  1.55466804 -0.16513135  1.84291845 -1.81559199  0.68793577  0.05994924
#>  [73]  2.45733022  1.34616268  1.68138533  2.37950051  0.75954662 -0.39029137
#>  [79]  1.18441357  0.53848361  0.51032354  2.65861216  1.67461720  1.45454589
#>  [85]  0.50931466  1.30981329  2.08007761  2.23826468 -0.38985391  0.65941205
#>  [91] -0.35472959  1.64261892  0.24645844  0.32828788 -1.76324788  1.90121220
#>  [97]  3.47941066  0.08417540  2.12850495  2.60507004
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
