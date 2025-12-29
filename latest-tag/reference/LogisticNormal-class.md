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
#>   [1] -0.735623187  0.783761480 -0.909300394 -0.397288435 -0.469660950
#>   [6] -0.085499525 -1.571314268 -0.218611097 -0.583551218 -0.969386836
#>  [11] -0.193127977  1.497875178 -0.011616638 -0.147940529 -0.673185183
#>  [16] -1.261240079 -2.502411514 -1.907327500 -0.781472625  0.717954402
#>  [21] -2.200824700 -3.380234822 -2.677933842  1.113980527 -1.973549139
#>  [26] -1.466773007 -1.668613570 -1.501954891 -1.207980617 -0.122612958
#>  [31] -0.748140683 -2.896310373  0.029977159 -0.756435595 -1.214591080
#>  [36] -1.616575072 -1.386856902  0.017164538  0.681833726  0.678712925
#>  [41] -2.755701913 -1.153222340 -0.203240100 -0.670306680 -1.205568060
#>  [46] -1.633999297 -1.492755370 -2.718044235 -0.608063634 -0.369644221
#>  [51] -1.116799861 -0.818013910 -0.141131871 -0.490545820  2.081435801
#>  [56]  0.965460774  0.004815959 -1.970927111 -1.491539588 -1.299406027
#>  [61] -1.066019638 -1.509732294 -1.915016852  0.069759805 -1.713913348
#>  [66] -2.584089599 -1.151129756 -0.897080404  0.341218892 -2.265239712
#>  [71]  0.100556647 -0.613567684 -1.788103444 -1.100380785  1.155202042
#>  [76]  0.043635595 -1.615748187 -0.232697319 -1.819662340  0.884341000
#>  [81] -1.998396689 -2.586100447  0.293203538 -1.528283985  0.716788673
#>  [86]  0.434798381 -1.125029913 -0.746454594 -1.386386725 -1.357124488
#>  [91] -0.226578864 -0.138127792  0.064245673 -0.854233058 -0.287527906
#>  [96]  0.582889463 -0.467752770 -1.534618380 -1.963097533  0.249853404
#> 
#> $alpha1
#>   [1]  1.21071138 -1.36055853  2.44406704 -0.51800995 -0.43520940 -0.60912451
#>   [7]  0.86917066  1.19297599  0.41921542  3.12153306  0.48804229 -0.23228884
#>  [13] -0.05469132  1.10570254  0.80147808  1.38676212  1.71172326  2.88093227
#>  [19]  1.75917311  0.69412798  2.91677243  0.89450029  1.87237283 -1.51637135
#>  [25]  0.95484828  1.48703918  1.03005959  1.42083932 -0.65742138  0.76522930
#>  [31]  2.28352129  1.85464428  1.18390009  1.58976460  2.27483423  1.13237110
#>  [37]  0.90938700 -0.30797374 -0.36062897  1.83167603  2.12113770  1.23302038
#>  [43]  0.99912120 -0.45713883  0.62682827  0.90456186  0.07934561  3.85926629
#>  [49]  1.40619098  1.44444085  1.45461164  1.39009000  1.31000876  0.08472004
#>  [55]  0.85807189 -0.24289759  0.94533819  1.33018190 -0.61547230  0.77931994
#>  [61]  1.92497521  1.98878871  1.22454276  0.81814389  1.90118762  2.59378047
#>  [67]  1.41859624 -0.90162770  0.20688676  1.80079582 -0.31235708 -0.19180465
#>  [73]  1.03391446 -0.10860207 -0.18913159  2.41226488  2.21804193  2.60675527
#>  [79]  0.81467780  0.45232670  0.91434862  2.85842328 -0.26453871  1.45927282
#>  [85]  1.59107456 -0.94631693 -0.03362782  2.92264624  0.62687149 -0.30767497
#>  [91]  0.62065438  0.56090165  1.75881014  1.37584551  2.42432238 -0.25806744
#>  [97]  2.03721597  1.58264774  0.07294693 -0.05636140
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
