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
#>   [1] -0.58168113 -0.28756027 -0.84607869 -1.70061759 -0.51899288 -1.98726031
#>   [7] -0.61565173 -2.21597470  0.34322988  0.96528872 -0.12994596  0.13950852
#>  [13] -0.66442523 -1.85501482  0.40086128 -0.95699354 -1.69243133 -2.29329656
#>  [19] -0.33372544 -0.59115838 -0.18806767 -0.79684377 -1.62566220 -1.07468910
#>  [25] -1.10042022  0.68348272  0.27851642 -1.31993883  0.72404989 -1.53726545
#>  [31] -2.32384360 -0.61353799 -0.69383330 -0.44124453 -0.80119897 -1.68603563
#>  [37] -0.39959804  0.63902308 -1.45016199  0.03644904 -1.67085420 -0.96800002
#>  [43] -2.14802279 -2.88853043 -0.57372591 -0.75622129 -1.41423290 -0.81511131
#>  [49] -1.16300197 -1.82678871 -1.18968937 -0.51678202  0.47184598 -1.15181947
#>  [55] -0.73001273 -1.52524543  0.08864838 -0.66601846 -1.68359638 -0.52146063
#>  [61] -0.82172762 -2.05965670  0.83276936 -0.94358868  1.32503985 -1.23917364
#>  [67] -1.51013697  0.05250078 -1.75573079 -1.06105362 -1.35236045 -0.57602265
#>  [73] -0.75174099 -0.39741446 -0.25541600 -0.19095150 -1.34560032  0.18173181
#>  [79] -0.19034658 -1.78878369 -0.24701668 -1.19794684  0.02252007 -1.71071785
#>  [85]  0.50556755  0.62007047  0.12576937 -2.39446444 -0.69945129  0.16688949
#>  [91] -1.53151217 -0.65365613 -0.70966830  0.92175914  0.27159589  0.16491384
#>  [97]  0.78034646 -1.31122212  1.28625633 -1.28902661
#> 
#> $alpha1
#>   [1]  1.01386494  0.33655809  1.04302727  2.08174937  1.41079448  1.78987525
#>   [7] -1.35695836  3.13661477  0.69218227  0.37044656 -0.09564575  0.14578923
#>  [13] -1.21046424  2.56543334  0.62193178  0.25554274  2.22047813  1.51712350
#>  [19]  1.23059518  1.50766672  0.68262577  2.04181102  0.59402137  0.39171857
#>  [25]  0.75017098  2.03206337  0.85072182  2.63752192 -0.76607143  0.21772732
#>  [31]  1.64408772  1.38167044  1.28705562  0.60294085  0.84433114  1.77684161
#>  [37]  1.12470500 -1.66774158  0.88399656  1.46573085  1.28969885  1.13366656
#>  [43]  1.21778889  2.93500782  0.68410296  2.64869543  1.13253279  0.67132885
#>  [49]  3.21667675  2.02362236  1.84020008  1.55187262  0.05906507  2.63392321
#>  [55]  1.84508555  0.54947890  0.43666016 -0.45883511  1.10583837  0.59224138
#>  [61]  0.10884447  3.02032573 -0.59564797  0.13684886 -1.10274430  0.78590332
#>  [67]  1.27454917  0.72927242  1.12973116  1.28819245  1.03785846 -0.18416484
#>  [73]  1.51847362  0.40821548  1.34780218  0.09110565  1.95261894  1.43965653
#>  [79] -0.56100959  1.84355457  0.19986082  0.92752469 -0.91684739  1.62746362
#>  [85]  0.20835219  1.02884128  1.02589719  1.69602946  2.95238809  0.82772471
#>  [91]  2.88572114  0.21504909  0.93918567 -0.15155831  1.59797188  0.37919297
#>  [97]  1.55152860  3.20555695  1.22290926  0.71825800
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
