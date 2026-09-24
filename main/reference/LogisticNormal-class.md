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
#>   [1]  1.293404985 -0.706650751  1.435640955 -3.314211900 -0.444884496
#>   [6]  0.856444476 -1.532102818 -2.183391360 -0.430478928 -1.387657747
#>  [11] -1.581073063 -3.015070067  0.001116809 -0.199976073 -0.144609262
#>  [16] -1.986475599  0.538266633 -0.125222010 -0.565111328  0.429539188
#>  [21]  0.390017500 -0.762552904 -0.089828277 -0.392052287  0.887440720
#>  [26] -0.994900797 -1.209532715 -0.786361795  0.240156014 -0.411536192
#>  [31] -1.657988845 -2.977173903 -1.468478003 -0.067098620 -2.634200232
#>  [36] -1.198810511  1.866525846 -1.930537338 -1.467697819  0.649739574
#>  [41]  0.606651256 -1.492502599 -0.587716661 -0.739993796 -2.119967381
#>  [46] -1.657461778 -2.561049392 -2.188715467 -1.896974405 -2.209872379
#>  [51] -1.607558921 -0.603936351 -1.006739995 -0.448739030 -1.035965699
#>  [56] -2.003971071 -1.810622152 -1.802300818 -0.687789381  0.401705404
#>  [61] -0.586749652 -0.148764425 -1.735737459 -1.298031203 -0.520228217
#>  [66] -1.434119997 -1.398043255 -0.904494146 -0.831237016 -0.092689445
#>  [71] -1.348673170 -2.125781378 -3.126642227 -0.505454411  0.160882252
#>  [76]  0.218653268 -2.296393737 -1.863799044 -1.960844134 -0.656629485
#>  [81]  0.926437321  0.293370776  0.680587439 -1.451417337  0.564641907
#>  [86] -0.971287208 -0.685966853  0.044030357 -2.223536588 -1.130594616
#>  [91] -1.347428406 -0.180940862 -1.400044113 -1.481984820 -0.707501114
#>  [96] -0.456377567 -1.207804767 -0.142879404 -0.642299361 -0.484628045
#> 
#> $alpha1
#>   [1] -0.42386704  1.08763959 -1.39738330  2.92755811  2.22142301  1.16675204
#>   [7]  1.01776946  1.61430140  0.84236294  1.51260205  2.07101767  0.89220590
#>  [13]  0.27333930  0.34763931  1.87180549  1.89285167 -0.12693602  0.02580546
#>  [19]  1.42439082  0.88093452  2.53796618  0.86282234 -0.13635467  0.88534005
#>  [25]  0.76137135  1.21872487  1.74849762  1.72988138  0.26446001  2.16071795
#>  [31]  0.89949760  2.62092003  0.97788016  2.21509595  2.21820764  3.11918964
#>  [37]  0.12971185  1.88592052  0.61332048  0.29264034  0.98423447  0.76965266
#>  [43]  0.47315879  2.75844371  2.41597852 -0.29270874  0.58395234  0.61187774
#>  [49]  2.47356065  0.31031407  1.03582655  1.65226814  1.46332824  1.93106268
#>  [55]  2.72988565  2.31832248  1.82185287  0.21870881  0.30438548  0.72454538
#>  [61]  0.43825854 -0.47494161  1.58367077  1.19278967  0.22914949  1.86699566
#>  [67] -0.32656349 -0.92595668  0.66616686 -0.11930123  1.04750830  2.63192464
#>  [73]  2.00189452  1.51433225  0.92480168 -0.26836473  0.92406459  1.90651460
#>  [79]  2.44055790  0.85563806  1.32379959 -0.06994042  0.21074702  1.90737861
#>  [85] -1.46003739  0.58572668 -0.33865509  0.40706869  2.22672178 -0.76215557
#>  [91]  1.49433491  0.72372561  1.73252249  1.27850337  0.05335380  1.58571544
#>  [97]  2.15371702  1.05419806  0.52026662  0.96762776
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
