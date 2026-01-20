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
#>   [1] -0.240869985 -0.005012262 -3.976563445 -1.281768316 -2.821024737
#>   [6] -2.518102991  0.645552753 -1.358249781 -1.049375305 -0.602771376
#>  [11] -1.768894474 -2.363185525 -1.664391055 -1.315452905 -2.214627008
#>  [16] -1.301390988 -1.084671619 -2.246545037 -0.765179055 -0.263138851
#>  [21] -2.664852882  0.190535337  1.128641061 -2.308475240  0.040032169
#>  [26] -1.431036045 -1.490559241  0.238286444 -1.927666603 -0.839915430
#>  [31] -0.908852185 -1.448707372 -2.517354178 -0.159799402 -1.468569116
#>  [36] -0.999143837 -1.620840733  0.237776582 -2.260353258 -0.907907848
#>  [41] -0.724751988  0.595799338  0.516142631  0.029342464  0.528556860
#>  [46] -1.659542839 -0.342958949 -1.634938681 -1.231524176 -1.237515034
#>  [51] -2.725320730 -1.054390007 -2.100709132 -1.521219807 -0.952086706
#>  [56] -1.724582210 -1.107286068  1.030201498 -2.172990224  1.183742081
#>  [61]  0.449339175 -2.248632671 -0.477908797 -1.020452081 -1.306101561
#>  [66]  0.363392527 -2.475030962 -0.508675916 -0.157483016 -1.326895450
#>  [71] -2.379810679  0.284329125  0.075593084 -0.175736184 -1.213862645
#>  [76] -0.789920275 -1.307085750 -3.521118025 -1.012836995  0.176142984
#>  [81] -1.371959973 -0.239227507 -2.408069144 -0.030240222 -0.972605888
#>  [86] -0.009825209 -2.033469762 -2.234830731 -1.713402139 -0.668789859
#>  [91] -0.309433983 -1.012929963  1.082773656 -0.969707450 -1.715431539
#>  [96] -2.573595216 -1.096154095 -2.591831855 -0.434403887 -1.229699931
#> 
#> $alpha1
#>   [1] -0.80810323  0.12936703  2.99027663  1.67105403  1.39327477  1.81885018
#>   [7]  0.50566833  3.03372060  2.83844295  0.32255056  1.15739847  2.30295623
#>  [13]  0.23910402  0.65480775  2.01497312  0.47112193  2.02436940  2.77221171
#>  [19]  2.23032145  0.69433976  1.28928471 -0.73361754 -1.68235278  1.14197939
#>  [25] -0.42513627  1.56834521  1.36318834  0.98837255  2.67377687 -0.27320185
#>  [31]  1.29573065  0.06660769  2.26088340  0.26598930  0.75433432  1.58683922
#>  [37]  0.58490977 -0.96704150  1.43330445  0.90349315  1.27695547 -0.47907793
#>  [43] -1.55241524  1.05552895  0.47287116  2.75149341  0.02155107  0.87254618
#>  [49] -0.67752538  1.59779966  2.27613745  0.25466321  1.84733416  0.81401858
#>  [55]  1.57731438  1.29676365  1.00105739  0.28071558  2.03990387  0.41096565
#>  [61]  1.12216361  0.51038838 -0.88641319  1.35900277  1.45970356  1.59056940
#>  [67]  0.45057641 -0.12630324  0.99594619  0.82184970  2.56586649  0.44090943
#>  [73]  0.56793505 -0.45959544 -0.28023670  0.01069179  1.19474124  3.03365756
#>  [79]  1.42208171  1.89866105  0.41140088 -0.21335097  2.51182343  1.05999338
#>  [85]  0.08043405  0.13847210  1.41207983  3.58352433  1.11156580  2.10935383
#>  [91] -0.17641103  1.69065876  0.89556781  1.73766229  2.90099657  2.11525288
#>  [97] -0.57215691  2.03336925  0.02049607  1.43998456
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
