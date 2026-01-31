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
#>   [1] -0.276158077 -2.248508969 -0.803546405 -0.917853342 -1.303271497
#>   [6] -3.236243518 -1.954313721 -1.194123563  1.737052437  0.982293257
#>  [11]  0.421391690 -1.136327617 -1.696884379  0.181226383 -1.441199868
#>  [16]  0.289752478 -0.581090743 -1.292171326 -0.709224581  0.172762767
#>  [21] -0.707163926  0.417529992 -0.872192515 -0.335649693  1.727501968
#>  [26] -0.938157445 -1.338725240 -2.395959920  0.033478270 -0.086623854
#>  [31] -2.007837967 -0.663066857 -0.933127652  1.653853460 -1.141480352
#>  [36] -0.109977537 -1.072634951 -3.457624198 -1.624622859 -1.671974237
#>  [41] -1.667874186 -1.273300123 -0.103221866  1.190657630  0.482936086
#>  [46]  0.626259012 -0.473490819 -2.190476402  0.873277952 -2.688200940
#>  [51] -0.003025586  0.860397693 -0.335211849 -0.504623477  0.561239502
#>  [56] -0.636896909  0.094923850 -0.146721707 -0.217433745 -1.587305120
#>  [61] -0.619009317 -2.125855239 -0.532016043 -1.727326143 -0.982096753
#>  [66] -1.932115745 -0.223663547 -1.377617537 -2.179497347 -0.972171014
#>  [71] -2.705132160  1.010465388 -0.905532889  0.890960920 -1.069172349
#>  [76]  0.233639468 -2.174795554 -1.791051211 -1.955788691 -0.368321248
#>  [81] -0.057803805 -1.371037427 -0.095722385 -1.698981691 -1.303515843
#>  [86]  0.095668331 -0.926934898 -0.163520525  0.273121028 -2.743318587
#>  [91] -1.816293344  0.917327592 -1.404089324 -0.692985065 -2.136418022
#>  [96]  0.565119874  0.545319729 -1.198402869  0.647888689 -1.165731676
#> 
#> $alpha1
#>   [1]  0.635506789  2.144001402  0.986402715  1.352065726  1.000749148
#>   [6]  2.645070199  2.646525372  1.156651226  0.278151486 -0.141931300
#>  [11]  1.971721419  0.012468361  1.491846697 -0.298173153  0.897726527
#>  [16]  0.571489630  1.728433976  1.906025193  3.086219061  1.499812302
#>  [21]  0.614563595  1.227553796  0.252749393  0.971144607 -0.358806779
#>  [26]  0.261209395  1.088938757  2.318829359  0.149887651  2.094820341
#>  [31]  2.406407489  1.854897651  0.325498575  0.191630303  0.775532433
#>  [36]  0.058924413  0.733654536  2.751551848  2.295180460  0.045382512
#>  [41]  0.242028884 -0.212401237  1.426717082  0.024639543  0.623459498
#>  [46]  1.046977854  1.303150307  1.501094567  0.843362259  1.036768466
#>  [51] -0.004986161 -0.148010676  0.989677035  0.759858882  1.346034350
#>  [56]  0.921702184  1.185039021  0.433717382  0.436700563  0.961862736
#>  [61]  1.982592921  1.626890784  1.632797562  1.230247226  0.531847041
#>  [66]  1.764062149  0.574281765  0.755666851  0.508125306  2.007318939
#>  [71]  2.437617946 -0.614271103  0.036862845  1.720310962  0.789230604
#>  [76]  1.118913583  2.215875156  0.499537440  0.686934656  2.082877333
#>  [81] -0.378877255  3.449433799 -1.069109587  0.961421902  2.120940953
#>  [86]  0.617586226  1.198776048  0.076788817  1.894746422  3.311830134
#>  [91]  1.873078181  0.081762548  1.055105567  1.209089603  1.963408266
#>  [96]  1.712874441 -1.424081186  0.411609674 -0.146441457  0.699675663
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
