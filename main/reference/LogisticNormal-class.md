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
#>   [1]  0.51240626  0.04522624 -1.58634982 -1.08628357 -0.98422252 -0.88787463
#>   [7] -0.55647852 -0.28325887 -2.41442998 -1.60701311 -1.17969030  0.34175941
#>  [13] -2.41356307 -0.81815491 -1.52017735 -2.67502456 -1.63977679 -1.01322783
#>  [19] -0.33154471  0.43638398  0.76894160 -0.92198393 -1.85933844 -1.29210043
#>  [25] -0.57687554 -0.21347387 -0.44512752 -0.62825388 -0.13269731 -0.48409597
#>  [31]  0.89706099  0.60513316 -0.97205235 -0.96389657  0.36128803 -1.05909291
#>  [37] -2.59176392 -0.64107941 -1.85547056  0.20120883  1.89196987 -3.14324346
#>  [43]  0.67835819 -0.36146876 -0.12075227 -0.98450831  0.60250180 -0.33290108
#>  [49]  0.31129479 -0.07802432 -1.72582763 -2.26378096 -0.92627371 -0.94172618
#>  [55] -1.44039896 -0.92241376  1.76637429 -1.10460214 -1.68593012  0.77791840
#>  [61] -1.89673641  0.36235708 -1.09376778  0.10594080 -1.11006917 -1.10322704
#>  [67] -0.03138457 -1.97054974 -0.56309840 -1.04624601 -0.80398716 -1.41132036
#>  [73]  0.03468466 -2.38571188 -1.30721743 -1.40097516 -1.76047944 -0.94238650
#>  [79]  0.87814649  0.05434349 -2.67584217 -0.91335419  0.71823756 -1.32884822
#>  [85]  0.03263311 -0.73955102 -1.72273386  0.20565635  0.02940077 -1.59632295
#>  [91] -2.69625554 -1.31121836 -0.62850195 -1.34799274 -0.33084905 -1.13332674
#>  [97] -0.40379741 -2.14331418 -0.55641970 -0.75020250
#> 
#> $alpha1
#>   [1]  0.91090261 -0.26205494  0.83092116  0.58364903  1.14782308  0.91546881
#>   [7] -0.56030839  1.70206028  2.79846910  1.67295811  1.37184726  0.93359474
#>  [13]  2.12605724 -0.35057496  2.70175211  1.66298217  0.78511892  2.05571344
#>  [19]  1.88733594  0.66271904 -0.65008943  0.69009033 -0.20855518  2.59459143
#>  [25] -0.13594146  1.27962590  0.21005815 -0.45562800  0.14904588  1.50543680
#>  [31]  0.72189128 -0.35509813  0.38291332  1.45034314  0.09201677  0.68198804
#>  [37]  2.34058396 -0.42367763  1.63263320 -0.81245311 -0.58630720  0.66680452
#>  [43]  0.77457202 -0.14880885  0.78316258  1.47253081 -0.52388252 -0.29038593
#>  [49]  0.66486952  0.80292716  1.96344930  1.95947829  1.89776936  1.26143324
#>  [55]  3.60984141  0.32264490 -0.66439556  1.14871361  0.04923490  0.16451394
#>  [61] -0.18854069  0.45230733  0.42940462  1.33984564  0.65273831  0.86521231
#>  [67]  0.30173080  2.20946159  1.11819583  0.19911863  1.39221036  1.34426930
#>  [73]  0.01969738  1.72594245  0.98508706  3.37436489  1.23679970  0.50568648
#>  [79] -0.64929032  1.14945934  1.43555265  2.02249462  0.60094127  2.09481528
#>  [85] -1.24730971  0.89084592  0.14047789 -0.65726229 -0.78267120  0.98237793
#>  [91]  2.35876593  2.10695308  0.24447389  1.72988111  1.56652542  0.08957273
#>  [97] -0.18265489  0.93844653  1.59215715  2.33375757
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
