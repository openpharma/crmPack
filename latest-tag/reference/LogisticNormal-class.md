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
#>   [1] -0.17652521 -0.24562339 -1.47302219  0.15012788 -2.58346543 -1.63010782
#>   [7] -0.65594487  0.27393076  0.28543605 -1.01089409 -0.51380869 -0.40650321
#>  [13] -0.60317644  0.16702601 -0.59920261  1.42897012 -0.81659720 -0.51067392
#>  [19] -1.16809361 -0.16781240 -0.80752494 -1.00615639 -0.63127002 -1.47065955
#>  [25] -0.64868891 -2.30839419 -0.69399636  0.56147138  0.17989433  1.23147817
#>  [31] -0.76410442 -1.33790323 -1.79777875 -3.56064015 -0.20624408 -1.30595955
#>  [37] -2.38910852 -1.58081625 -2.38990904 -2.24600424 -0.07094754 -0.71900845
#>  [43] -2.35521964 -2.50861298 -1.43711649 -2.30249814 -0.03292853 -1.64461016
#>  [49] -0.60521670 -0.97577093 -0.47357188 -2.22771078 -1.40989389 -2.27606167
#>  [55] -2.17099856 -1.87390156 -1.99483925 -0.64046998  0.09456755 -1.17303099
#>  [61] -1.62162022 -1.83664216 -0.21766775  1.11902125 -0.16432068 -0.75411620
#>  [67] -1.90700327  0.14518728  1.46631592 -0.08312996 -0.56180757  0.94216611
#>  [73] -0.29485717 -2.48446097 -2.08140390 -0.10875283 -2.05543654 -0.21599150
#>  [79] -1.16181546  1.60770591 -1.86026695 -0.58750379 -2.36166629 -0.68114329
#>  [85] -0.84345815  0.24994182  0.46844582 -1.01285023 -0.73742260 -0.72510387
#>  [91]  1.48537325 -1.17939308  0.76986044 -1.05329425  0.34798814 -2.75336232
#>  [97] -1.41253755 -0.45070089 -1.43450842 -0.93316934
#> 
#> $alpha1
#>   [1]  0.381534961  1.502127045  0.705395912  0.377030448  0.819589410
#>   [6]  0.462292607  1.100208125  0.938376076  0.285290491  2.606301746
#>  [11]  1.524210924  0.198489209  0.608845153  0.758790753  1.347598110
#>  [16]  0.191541922  1.195938841  1.541330349  2.873491515  0.768836213
#>  [21]  1.403722912  1.823631284  2.096234361  0.906126165  0.870126138
#>  [26]  1.778845987 -0.858659968  0.361188364  0.504210885 -1.204626034
#>  [31]  0.719987638  0.608072282  1.342300906  1.577369719 -1.334887244
#>  [36]  1.460430585  0.832815022  1.422363302  1.692397375  2.188910424
#>  [41]  1.179488528  1.064717928  3.020043360  0.598397265  0.278402474
#>  [46]  0.009333376  1.821447493  1.170399669 -0.038863049  2.049275210
#>  [51]  0.804113336  1.549646423  0.466329060  2.875767212  0.577204832
#>  [56]  0.865272267  0.620567733  0.623736073  1.084932306  2.176146822
#>  [61]  2.113216424  2.262305134  0.123532336  1.042653491  0.849789589
#>  [66]  2.161771526  0.444033199  2.159774384  0.299873593  1.670379200
#>  [71]  0.553367286  2.240460786  1.235312667  0.076918939  2.172216243
#>  [76]  0.132948921  0.746745594  1.354220734  0.614703820 -0.180534498
#>  [81]  1.784523559  1.131046088  1.558122983  0.895578023  0.682883376
#>  [86]  0.370745708 -2.263069019  1.629418096  1.729872311  1.424429548
#>  [91] -0.472179393  1.552112988  1.066949926  0.448419631 -0.898060114
#>  [96]  0.448888444  1.749634358  1.027017093  1.236578964  2.413569515
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
