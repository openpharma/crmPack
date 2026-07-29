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
#>   [1] -0.12073989 -1.72987654  0.56790050  0.32786991 -3.10946201 -0.78616468
#>   [7]  0.51660516 -0.66285880 -0.01648349 -0.42625091 -1.58286436 -0.02333311
#>  [13]  0.39599835 -0.13652617  0.24728435 -0.79893667 -1.00840601 -1.84378344
#>  [19]  0.18993823  0.10432969  1.30743549 -0.94729934 -0.91035782  0.83945023
#>  [25]  0.24897845 -1.81483936 -1.44190061 -1.66474347  0.26149683 -0.36073816
#>  [31] -0.31892307 -1.34558894 -0.12332694 -0.95093678 -1.64830305 -1.84314322
#>  [37] -0.62860952 -1.83895890 -1.00627580 -1.29486616 -1.76090955 -1.24941051
#>  [43] -0.86064286 -0.57323300 -1.48466383 -1.15622432 -1.82848622  0.18005491
#>  [49] -2.45189423 -1.12912462 -2.11726894 -1.50372171  0.32297527  0.44509039
#>  [55] -0.01620626 -0.42480726  0.57576306 -2.22763288 -0.75691035 -2.33570598
#>  [61]  0.46771419  0.44156362 -0.32235104  0.50223929 -1.20453077  0.46071648
#>  [67] -0.66389573 -2.03766643  0.20798572 -2.80349274 -0.34971378 -0.66172825
#>  [73] -2.34445981 -0.88442375  0.02672964 -1.17302019 -0.82041814 -1.05834801
#>  [79] -0.80373156  1.41459830  0.66555906 -2.49311606 -0.39436175 -0.45125654
#>  [85] -1.02406658  0.43232145 -0.36016388 -1.06111352 -1.38048544 -1.45064655
#>  [91] -1.26103206  0.10551978 -1.32050033  0.33497078 -2.51145392 -1.40098076
#>  [97] -2.06074409 -0.25973220 -0.38380430 -2.43957731
#> 
#> $alpha1
#>   [1] -0.840656198  2.986066308 -0.407530650  1.521955737  1.385227149
#>   [6]  0.628806039  0.899920762  0.417941450 -0.091930761 -0.535940407
#>  [11]  2.686741803  1.421536869  1.366744623  0.335007047  0.457793342
#>  [16] -0.963935629  1.585171970  2.292428459  0.670224707  0.686694607
#>  [21]  0.641415199  2.825946430  1.584927681  0.336163754  0.545081338
#>  [26]  2.416914694  2.142734234 -0.243887083  0.604580410  1.669174297
#>  [31]  1.786930670  1.349810794 -0.004710258  2.064970701  1.834642775
#>  [36]  1.442516441  1.360412328  1.475606100  2.254281398  1.897609955
#>  [41]  1.943046479  1.146442818  1.911926788  2.055213001  2.164338484
#>  [46]  1.834435799  0.502191823  0.501451000  1.716349465  1.266785204
#>  [51]  1.776488848  2.898958773  0.978701026 -0.604558181  0.550015055
#>  [56] -0.373093133  0.179771860  1.359760310  2.025168943  2.260854315
#>  [61]  2.071774122  0.301394825  0.771139525 -0.729097823  1.171505874
#>  [66]  2.001055081  2.333261363  0.266760038  1.737017309  2.028778438
#>  [71]  0.501393170 -0.559361353  2.482187181  0.379406869  1.170451915
#>  [76]  1.338524954  0.348261668  1.174910050  2.237440890  1.431371000
#>  [81]  0.005013167  0.045376186  1.540436939  2.097515129  1.510470807
#>  [86]  0.429293194  0.406012916  2.000616245  1.972182738  2.807700111
#>  [91]  1.746219753  0.430921291  0.602225229 -0.700841039  1.270181994
#>  [96]  2.969261309  0.442313626  0.865556104  2.558157683  0.931785589
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
