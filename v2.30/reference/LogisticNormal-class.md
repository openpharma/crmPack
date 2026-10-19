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
#>   [1] -1.504515829 -1.240117636 -0.922305305  2.345597016  0.849757732
#>   [6] -1.853759583 -0.175636056 -1.111392152 -2.827078985  0.295324331
#>  [11] -2.119873431 -0.198570934 -0.380429647 -0.114640886  0.237907115
#>  [16] -0.337349944 -1.552462347 -0.275798031 -0.759169805  0.997108456
#>  [21] -0.924009706 -0.669599386 -2.054686866  0.011012049 -1.247346651
#>  [26] -1.179110183 -2.121437033 -0.923972987 -1.145858905 -1.563890897
#>  [31] -1.229759813 -1.464545110  1.317985134 -1.350145404  1.362672066
#>  [36] -0.580728992 -0.572511674 -1.440834602 -1.231733676 -1.119935656
#>  [41] -0.666335899 -0.705352001  1.013134261 -1.532218118 -0.129445894
#>  [46] -0.625382411 -0.266313363 -0.865825601 -1.440150876 -2.077904661
#>  [51] -2.086080017  0.115041154 -0.114994982  1.093500170  0.150079166
#>  [56] -0.514334787 -2.108762033  0.430339282 -0.517221083 -1.034061902
#>  [61] -1.226673676 -0.001541701 -1.067388941 -0.651657646 -1.402990533
#>  [66] -0.225899242  0.042712998  1.079705218 -0.854163673 -1.398299863
#>  [71]  0.464226977 -1.912033978 -0.481251214 -2.075946624 -0.819212971
#>  [76] -0.305572321  0.496684485 -2.069002223 -0.359129020 -2.052027106
#>  [81] -0.779783107 -1.296295543 -0.769150306 -0.163779132 -0.241527825
#>  [86]  0.273167861 -0.518678456  0.371983546 -0.563219307 -0.625173975
#>  [91] -0.013910660 -0.415630076 -1.830272753 -0.837231692 -0.944239681
#>  [96] -1.002709201  0.573991444 -0.314568673  0.776375222 -0.086650829
#> 
#> $alpha1
#>   [1]  0.78567279  0.93456687  0.28392370 -1.45327431  1.60682075  1.52007986
#>   [7] -1.12181091  1.18388887  2.18483698  1.08010327  1.49179759  2.26166211
#>  [13] -0.27276536  0.61258951  1.54471678  0.54056142  1.05392901 -1.04479894
#>  [19]  0.02838910 -0.41871733  1.45150779  1.05760036  1.05142435  0.89566070
#>  [25]  0.98849292  1.42653702  1.75929066  1.13232385  1.71400633  2.09580272
#>  [31] -0.32570360  1.30258015  0.31315281  2.06234754 -0.43389193  1.35755738
#>  [37] -0.31755621  1.95170074  0.52035680  0.12611108  1.89017298 -0.11598377
#>  [43] -0.10861372 -0.21614941  0.52610135  0.78089618  0.78943184  1.27234095
#>  [49]  1.07062428  1.27946756  1.88942740  0.44878809 -0.10490495 -1.16594864
#>  [55] -0.08843268  1.43617081  2.89077167  0.16537986  0.51401006  0.60396013
#>  [61]  1.39735101  1.13245230  1.53326814  1.61451331  0.37708943  0.32796669
#>  [67]  0.30797245 -2.15705229  0.20380581  2.29007699  0.58269619  2.98625913
#>  [73]  1.70953981  0.97760395  1.27731039  0.48506981  0.29798077  0.96652579
#>  [79]  1.27721440  0.98987410  0.91197928  1.33665958 -0.77160163  1.04745416
#>  [85] -0.06476333  0.44262963 -1.18241752  1.65008002  0.71101803  1.82946338
#>  [91]  0.30894468  0.26385527 -0.13591625  0.97276561  1.59180505  2.63960464
#>  [97]  1.11001803  1.15675446  0.51731864  0.33940800
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
