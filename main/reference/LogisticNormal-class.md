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
#>   [1] -1.25952416 -1.94240298 -0.61393957 -0.28734305 -1.00268931 -1.85914361
#>   [7] -0.77411302 -0.43626676 -0.50163321 -0.91390432 -0.08867108 -2.73343684
#>  [13] -1.25448580 -0.98746688 -1.54055070 -1.13444509 -1.82734372  0.73094400
#>  [19] -0.32788752 -0.65046800 -1.58269832 -1.12293619 -1.32617481 -1.16505908
#>  [25] -0.75289087 -1.38349318 -0.69975552 -0.66094391  0.68676087 -1.55338027
#>  [31]  0.33763990  0.19795743 -1.33816796 -1.54211378 -1.29574098  0.51432111
#>  [37]  0.72124668 -0.87597010 -0.85063781 -1.44830032 -1.12040133 -3.13513872
#>  [43]  0.53011041  1.56398339 -0.86522479 -0.35232936 -1.89510128 -0.83976040
#>  [49] -0.32280977  2.23230114 -1.34256390 -1.02233592 -0.01652733  0.35501239
#>  [55] -0.42259047 -1.67779442 -0.28058104 -1.72530714 -1.97149547 -1.37414695
#>  [61] -1.23712795  0.55818186  1.14367033 -1.30608181 -0.51521479 -1.22642947
#>  [67] -1.34981302 -0.51690113  0.67859426  0.20262132 -0.46444001 -0.56937805
#>  [73] -0.34578534 -0.63890980 -0.26121646 -1.05978537 -0.42735057 -1.11266686
#>  [79]  0.60183361 -2.84909327 -2.11267633 -2.08794838 -0.60899263 -0.26930325
#>  [85] -0.72360265  0.65014900 -0.74574415 -1.49282041 -0.14188851 -1.43943942
#>  [91] -2.24849316  0.19836340 -0.90789754 -0.29295051  0.53651552 -0.77681193
#>  [97] -3.27121507 -0.65538936 -1.59452392 -0.66545794
#> 
#> $alpha1
#>   [1]  0.70748997  2.53970733  1.37010084  0.13573670 -0.24837779  0.34928103
#>   [7]  0.03864664 -0.63355760  0.62943922  0.84571602 -0.48420524  1.93453493
#>  [13]  3.16697773 -0.29866840  1.47432951  1.11783399  0.99919922  0.51605134
#>  [19]  1.39515511  0.69569034  2.52219303  1.23731042  1.41335744  2.35765917
#>  [25]  0.83469160  1.61534439  1.25133908  0.65098160  0.19930396  0.57941962
#>  [31] -0.40524135  2.57064065  0.64639908  2.54168572  1.68205021  0.52261490
#>  [37]  0.84293969  0.36615749  0.39556622  0.30890317  1.11084076  2.19228647
#>  [43]  0.68726225  0.37971089 -0.48652454  1.15258690  0.94355881  1.04674360
#>  [49]  1.29768491  0.21708211  2.03333239  1.31914490  1.77904944  0.24262459
#>  [55]  2.22011561  0.77665784  0.89764523  2.31739367  1.42443542  1.58019790
#>  [61]  1.15955447  0.65878288  1.38399894  0.64442783  0.57399896  0.94184384
#>  [67]  2.83446275  0.96613024 -0.25330506  0.88058430  0.29373680  2.22134321
#>  [73]  1.46339949  2.14154570  0.73903441  0.22559469  1.38661140  1.89513776
#>  [79] -1.93576233  1.68554814  1.15132701  2.65891642 -0.18889974  1.76122058
#>  [85]  0.63769483  0.27318325  2.50819586  2.35283564  0.42055195  1.96224903
#>  [91]  1.61077447  0.63379171  1.86833873  2.07898531  0.73202451  0.82951097
#>  [97]  4.03662213 -0.08724026  1.24022996  1.27185374
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
