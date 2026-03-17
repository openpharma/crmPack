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
#>   [1] -0.45259863  0.78458933 -2.25020606 -2.00854855 -2.12681215 -2.92081818
#>   [7]  0.04766727 -1.57286988 -0.78746123 -2.54447547 -0.17303336 -2.10244874
#>  [13] -1.55492391 -0.36725079  0.89659379 -0.14139626 -1.06758766 -1.06490916
#>  [19]  0.15801843 -1.63082927  0.05635640 -3.70732386  0.74417196  0.75235421
#>  [25] -0.93421832  0.48760361 -1.46012568 -1.43693931 -2.31927276  0.13797908
#>  [31] -0.98883850 -1.25989156 -0.35877968 -1.67151839 -0.10588302 -1.32682490
#>  [37] -1.38667141 -1.76796268 -2.32085805 -0.81885135 -2.37483121 -0.56751701
#>  [43] -0.34913855 -1.45016073 -1.03586209 -2.90809711 -0.10195593 -0.48909511
#>  [49] -1.38312772 -0.43139577 -2.11924962  0.03175558  0.02992716  0.15586514
#>  [55] -2.13120736 -1.44580347 -1.16899554 -0.25381565 -1.93070281 -0.90978440
#>  [61] -1.92634916 -0.71837169  1.04071425 -1.71023808 -0.94540742 -0.81197276
#>  [67]  0.16304686 -0.72000208 -0.08509905 -0.59761939 -2.09157328 -0.37500866
#>  [73]  0.39716028 -0.60896448 -1.53888006  1.39035123  1.21966101 -2.48714090
#>  [79] -1.76099818 -1.52019421 -0.62059812  0.20003207 -1.52474501 -0.81320150
#>  [85]  0.28477761 -1.54422362 -0.47370123 -2.27950850  1.60205254 -1.73053944
#>  [91]  0.13742094 -0.77527417 -0.83884445 -1.04431649 -0.57746206 -0.64644032
#>  [97] -0.74706921 -0.56099759 -1.82311972  1.03028100
#> 
#> $alpha1
#>   [1]  0.249178742  0.012466017  1.658467865  0.603495781  2.200183097
#>   [6]  2.109863737  1.090421049  0.250775682  1.777997683  0.190372642
#>  [11]  0.584432078  0.839217796  1.733692438  1.049634802 -0.154957799
#>  [16]  1.029528294  2.357134793  1.266003078  0.999715051  3.190148070
#>  [21] -0.113400232  1.859574358 -0.820114267  0.341059837  1.366528360
#>  [26]  1.246836779  1.403497302  0.218887504  2.355380677  0.134901171
#>  [31]  1.300013730  2.388088814  2.142780523 -0.780748528  0.014219873
#>  [36]  2.009011640  0.502597474  0.296581160  1.221556012  0.647850597
#>  [41]  1.527146011  0.675149536  1.446621475  1.402441330  0.276211775
#>  [46]  1.402638787  1.111500365  0.215736899  2.251389946  0.850328371
#>  [51]  2.443343681  0.224788878  0.255271999  1.016444069  2.265679245
#>  [56]  1.690055185  0.831068832  1.748913951  1.467404183  1.168143588
#>  [61]  2.534978635  2.023431400 -0.079582236  2.243242087 -0.487813932
#>  [66]  0.837098381  1.638218983 -0.460137545  1.461950353  0.509439650
#>  [71]  1.135079925 -0.342749264  1.064531088 -0.544034329  1.039163989
#>  [76] -0.297837413 -1.136493443  3.444623515  0.593572071  1.207516952
#>  [81]  1.679434461  0.360533970  4.131983164  0.780422167 -0.148655830
#>  [86] -0.007621493  0.614101853  2.437243924 -0.278823907  1.142373577
#>  [91] -0.001800539  0.628452815  0.259206883 -0.619936666  1.938293933
#>  [96]  1.241954556  0.797068067  1.873642381  1.815394048 -2.027044031
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
