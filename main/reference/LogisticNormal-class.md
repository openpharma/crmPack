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
#>   [1]  0.85867393 -0.07510985 -1.08318315 -2.35027547  0.06147621 -1.17744772
#>   [7] -1.45293171 -1.91556711 -3.22083929 -0.72647343 -2.43733414  1.21540775
#>  [13]  0.03541058 -0.79715900 -1.21484870  0.68796845 -0.37234133 -0.81078142
#>  [19] -0.21239486  0.18272294 -2.73192506 -0.76189716 -0.82577202 -2.16908442
#>  [25] -0.37651969 -1.86797074 -1.32293030 -1.45058275 -0.54948005  1.10646495
#>  [31] -0.03641781 -0.87114287 -2.59103516 -1.22837299 -1.45705357 -1.77649374
#>  [37]  0.57741096 -0.75870383 -2.80162320 -1.41758503  0.02612290  0.78277858
#>  [43] -2.45296036 -1.18163826  0.87083780 -1.59286392 -0.44784417 -0.71626351
#>  [49] -1.64944938  0.32437399 -2.30929820  0.49688122 -1.55851180 -1.53506395
#>  [55]  0.30970980 -2.89592056 -2.51616488 -2.55869221 -1.78421960 -4.26757620
#>  [61] -1.10203188 -0.73562092 -0.85657108 -3.11194929 -1.62069940 -1.31573727
#>  [67] -0.50280541 -1.19948338 -0.13737898 -1.94603899 -0.22063799 -0.22115732
#>  [73]  0.12221458 -0.63749681 -1.06766374 -0.97268365 -0.90046661 -0.73627624
#>  [79] -1.77508983 -1.93429994 -1.32083763 -2.81940248 -0.61655971 -0.27284857
#>  [85] -1.27262299 -0.40971662 -2.40979817 -0.84632920 -1.55141288  0.76501052
#>  [91]  0.01994168 -1.83034149 -0.50411280 -2.21619263 -1.96148400 -1.13078350
#>  [97] -2.13092704  0.03518885 -1.47347571  0.04775609
#> 
#> $alpha1
#>   [1] -0.91122008  1.75457513  1.62118022  1.43119739  0.92570730  1.60061976
#>   [7]  2.85692252  2.17411302  2.25294894  1.26540118  1.77738034 -0.89745582
#>  [13]  0.19846453 -1.06012264 -0.23146112 -0.58343748 -0.75629446  1.80532660
#>  [19] -0.27756707 -0.25554229  1.85168131  2.46745436  0.74519827  0.70507388
#>  [25]  0.47195991  1.39406611  2.40144941  0.88616551  0.68383726 -0.26069372
#>  [31]  0.72871320  1.24354489  1.98403619  1.87351568  1.19322860  0.22809271
#>  [37]  0.40044576  2.28731186  2.09415438  0.92790545  0.19413165  0.44531203
#>  [43]  2.52350007  0.43982438 -0.65797264  1.96788266  1.88433737  0.77394538
#>  [49]  1.21877404 -0.24116302  2.30280163  1.38830521  1.95332899  1.11066982
#>  [55] -0.61471529  2.42159075  3.37133579  3.14736171  0.62185200  2.62168415
#>  [61]  1.95688426 -0.76428499 -0.29335331  3.80674074  2.96510636  1.62915241
#>  [67] -0.06593925  0.06621197  0.51707949  2.37060683  0.68408176  0.97682668
#>  [73] -0.05258121  0.51582423  0.30331728  0.52088485  0.92543405  0.41977984
#>  [79]  2.67354214  3.30562478  1.01718614  2.92668175  1.82368659  0.14150977
#>  [85]  1.24262498  0.59468921  0.35977770  1.52154834  3.26645030 -0.38461292
#>  [91]  2.29928481  1.90208620  0.07547264  3.21729901  1.88123591  1.73567833
#>  [97] -0.52325425  0.59967543  0.50019714  0.98867689
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
