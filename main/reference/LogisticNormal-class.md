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
#>   [1] -1.61539880 -1.83337323  0.34788707 -1.48386852 -0.98308182 -0.68362138
#>   [7]  1.15719309 -1.87632706 -1.94156002 -0.75905850 -0.56366834 -0.48692204
#>  [13] -1.27651475  1.03949883 -0.49150359 -2.48262068 -0.13855160 -1.26927102
#>  [19] -1.03157179  1.66630356 -0.58265950 -0.86321279 -0.99556030 -1.28817246
#>  [25] -0.50941739 -3.55585733  0.89552078 -0.74037534 -0.10173649 -2.02786410
#>  [31] -0.68157742 -1.73869454  1.21101593  0.39586144 -2.40066231 -1.37903061
#>  [37]  0.50615762 -0.81505868 -1.65109275 -0.16651020 -1.34851935 -1.74778189
#>  [43] -0.65299458 -1.81125429  0.44407824 -0.59118037 -0.80960372 -1.99164236
#>  [49] -0.60046698 -0.56764065 -0.27570862 -1.35833015 -2.61717723 -1.82518120
#>  [55] -1.08482608 -0.96360152 -1.82501376  0.64450866 -1.18106870 -1.58370538
#>  [61]  0.13696575 -0.34618910 -1.46666859 -1.98040472 -0.91923370 -0.08138232
#>  [67]  0.23514500 -1.20617000 -1.43597632 -0.14236744 -1.25569537 -0.22675278
#>  [73] -0.35636357 -2.42235508 -0.85477029 -1.42304071  0.11332503 -0.32595282
#>  [79] -0.28167500 -1.08816456 -1.00280691 -2.62330214 -1.46434834 -1.76603251
#>  [85] -0.58968108 -1.28199551  0.54640810 -1.49187116  2.57533580 -0.40830315
#>  [91] -1.83778903  0.06938926 -0.28458947 -3.32883085 -0.80628076 -1.84430689
#>  [97] -2.54129778 -1.56180993 -0.65640160 -1.01174056
#> 
#> $alpha1
#>   [1]  0.55550805  1.70803509  0.71434607  0.61550325  0.36489578  1.77619596
#>   [7]  0.61485305  3.23601168  1.85182646  0.23134109  1.63568001  1.39289866
#>  [13]  0.09311571 -0.78823332 -0.51381260  1.84314483  2.27658484  0.95229681
#>  [19]  0.19329808 -0.62214148  0.89030671  0.98740689 -0.40506069  0.93622640
#>  [25]  1.78323416  2.13142764 -1.86822163  0.86838156  0.72893937  2.76789165
#>  [31]  0.59948469  3.11928782  0.43058441  2.16774408  1.75928354  0.12833633
#>  [37]  0.88286580  0.85240638  0.89068321  0.07144288  0.92735335  0.52995409
#>  [43]  1.34088924  1.76841896 -0.25756194 -0.47260067  1.37359098  0.93031881
#>  [49]  1.34101481  0.38207982  0.34991732  1.65293076  2.16250546  0.93478970
#>  [55] -0.05716090  1.23083520  1.55118922  0.40860203  1.84802824  0.96852078
#>  [61] -0.45951988  1.18485694  0.86029954  2.84697796  0.46788876  0.91823037
#>  [67]  0.11741544  1.90038776  0.76188683  2.43666384  1.88062960  1.30122722
#>  [73]  0.98051727  1.90947932  1.81399587  0.42344258  1.39150145  1.15967318
#>  [79]  0.16864025 -0.01056444  0.71172135  0.68318300  1.39424186  0.41188149
#>  [85]  0.87967264  1.96204725  0.81847628  2.42840313 -1.37110373  1.51065641
#>  [91]  1.71584114  1.80430857 -0.33556830  2.39948768  0.77671288  2.50430747
#>  [97]  1.70877363  0.84435894  1.36590346  2.87074026
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
