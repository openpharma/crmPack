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
#>   [1] -0.67429202  0.44816607 -1.87688588  0.53365723 -1.10760394  0.23034328
#>   [7] -0.46836205  0.12892867 -0.46010207 -0.20674150 -0.90435068 -1.93960044
#>  [13] -1.90402539 -0.88751181 -0.77897500 -2.14867243 -0.95732434  0.97631730
#>  [19] -0.56079240  0.98586899 -0.27027762  0.40613133 -2.40196473 -0.28269983
#>  [25] -1.87715898 -2.03044512  0.61475051  1.37168738 -1.05295658 -0.32328500
#>  [31] -1.08074985 -1.16444088 -1.88302106 -0.97020612 -0.55026796 -0.54414857
#>  [37] -0.35397842 -1.36692328 -0.33240507  0.01654378 -1.57636317 -1.85685687
#>  [43] -0.56174714 -0.94214216 -1.69473827 -0.60009728 -0.82406381  0.40972636
#>  [49] -1.16514605 -0.59564016  0.86411630 -2.62304268  0.42327812  1.05884811
#>  [55]  0.11660892 -1.25590501 -0.11531111 -0.11029681 -0.09584960 -1.56498420
#>  [61]  0.41400837 -2.74551314 -2.18947751  0.25949790 -0.44610781 -2.00751551
#>  [67] -1.97651529 -1.48984763 -0.79192668 -0.44972240 -0.46285710 -2.03253929
#>  [73]  0.16672170  0.15932802 -0.22208391  0.33161398 -0.48189120 -0.75321929
#>  [79] -1.37517430  0.48096018 -0.88121837 -1.84680028 -1.50577709  0.28425540
#>  [85] -1.60638672 -1.71492168 -1.69937538 -0.10665115 -1.24867212 -1.02549128
#>  [91] -0.08025514  0.93811404 -1.16579210  0.57978598 -0.75874506 -1.24444801
#>  [97] -0.22032262 -0.51090077 -0.25274099 -1.24938305
#> 
#> $alpha1
#>   [1]  0.94904571  0.89475790  0.75242566  0.05918137  1.74200377  1.59556642
#>   [7]  1.43403712 -0.45571877  0.98343337 -0.10839615  0.78342529  1.69109778
#>  [13] -0.37119805  0.61908337  1.91839518  2.52591476  1.96130823 -0.16515170
#>  [19]  1.17752920  0.20905539  0.88295457  0.03395067  1.72194737  0.23013313
#>  [25]  0.27995783  1.87138933 -0.41232167  0.75453028  1.04249185  0.90421774
#>  [31] -2.10004876  1.40224226  0.73612781  0.99974222 -0.56816644  0.27030424
#>  [37]  0.06934331  1.45534198  1.60820258  0.42664796  2.31103867  2.69774526
#>  [43]  0.61323154  1.79483310  2.29372537  0.54287641  1.01421685  0.87463624
#>  [49]  1.90948079  1.16972193 -0.25488029  2.59437756 -0.40219699 -1.92233484
#>  [55] -0.81870246  0.18269909 -0.65191919 -0.70545219  1.73288832  3.71381121
#>  [61]  0.09491452  2.25363311  2.30318428  0.53519992  0.06596655  2.31313656
#>  [67]  2.55218740  2.22351738  0.32226808  1.50476666  1.88961236  1.26290004
#>  [73]  0.74280823 -0.14428765 -0.32833060  0.83079523  0.86570295  1.06196654
#>  [79]  0.17343813  0.75982994  0.15176532  1.55175155  0.94024873  1.48196121
#>  [85]  1.57462851  0.70267291  2.17337919  0.41308997  1.24585442  1.89891370
#>  [91]  0.54225644  0.29890759  2.94380924  1.58278464  0.26930821 -0.47892052
#>  [97]  0.74020144  1.40642681  1.85449988 -0.76707927
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
