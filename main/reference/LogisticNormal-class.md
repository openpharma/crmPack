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
#>   [1] -0.55884108 -1.80132690 -2.67798665 -1.43719595 -0.02719149 -1.04180444
#>   [7]  0.51684159 -3.67531881 -0.28960423 -1.35469879  0.43387106 -1.21398482
#>  [13] -0.68902229 -1.24768804  0.35580570 -0.83388535  0.39069455 -2.39783109
#>  [19] -0.09339838 -0.29643509 -1.27918534 -1.29148589 -1.55595325  0.29765538
#>  [25]  0.77924361 -1.46356167 -1.18304049 -1.22434339 -0.58480331 -0.11381806
#>  [31] -1.48404410 -0.63596611 -1.04354074 -0.32812975 -1.33083285  0.14464050
#>  [37]  1.30639337 -2.14508527  0.08701587 -2.98664882 -4.56895613  0.06539528
#>  [43] -0.09072504 -2.30232543  0.99413168 -0.60858070 -0.27100862 -2.17896664
#>  [49]  0.20419843  1.58227626 -1.08363093 -0.32875535 -1.76526233 -1.39607496
#>  [55] -3.20797306  0.67058121 -2.98384852  0.21659808  1.50159741 -1.41994615
#>  [61] -0.60388236 -1.84632258 -1.09321228 -0.17125916 -0.77678772 -1.58949903
#>  [67] -1.80971671 -1.07686836 -2.16080274 -1.69849293 -0.84898548 -0.02962771
#>  [73] -1.53639442  1.07336723  0.82171816  0.45390057 -2.07918485 -0.86570248
#>  [79] -0.72193004  0.08495484 -4.03661205 -1.16308819 -0.96484347  0.36756935
#>  [85] -0.42343382 -1.32792380 -0.82611068  0.83796367 -0.35933147 -1.28997729
#>  [91]  0.15843723 -0.52519716 -0.07413117  0.65207830 -1.18567175 -3.57149219
#>  [97] -1.15099832 -1.13929465 -2.07607955 -0.37809327
#> 
#> $alpha1
#>   [1]  0.16676022  1.58454718  1.18500512  1.04919006  1.47373162  2.31662156
#>   [7]  0.15619762  3.47117564  1.77350224  0.97086965  1.23469038  1.18457513
#>  [13] -0.04206865  2.52518894 -0.93834938  1.23263667 -0.14395908  2.12237283
#>  [19]  0.37703539  0.51728651  2.09837184  0.92217805  0.98855868  1.01719846
#>  [25]  1.26673054  2.03158291  2.07137177  1.89527621 -0.67918391  1.30428813
#>  [31]  1.93053131  0.65460444  1.02581336  0.46199844  0.88222672 -0.55134452
#>  [37]  0.55203562  2.30276424  0.07514924  2.14255291  3.33199290  1.35380509
#>  [43]  1.01962058  1.04446034  1.49600248  0.91560482  1.06724276  3.14400229
#>  [49]  1.00430255 -0.17820753  0.74816320  0.94092997  0.58426783  1.27812135
#>  [55]  0.85487310 -0.07608416  2.56960630  2.74622573  0.62599089  1.74770617
#>  [61]  1.83097446  0.14558048  1.56601252  1.79038355  0.49681577  1.10325881
#>  [67]  1.83624302  3.27407097  2.10184998  2.14826458  1.25392094  0.56004604
#>  [73]  3.01830036  1.16027063  0.31634025  0.83900305  1.06455283  1.38698577
#>  [79]  1.03716069  2.38555970  3.97080211 -0.32294920  0.59111105  0.81594109
#>  [85]  1.66306515  0.37535371  2.07254107  0.82488903  0.62010199  2.25841323
#>  [91]  1.19762461  1.75140246  1.00934176  0.25514672  1.25916825  1.92511739
#>  [97]  0.47903173  0.99175923  0.45898273  1.84507505
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
