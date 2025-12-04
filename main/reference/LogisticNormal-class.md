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
#>   [1] -0.98628982  1.31159844 -2.00031700 -0.74907179 -0.37960724 -3.01623453
#>   [7] -2.03187625  0.40614848 -0.84735181 -0.84795984 -1.91962695 -1.94150066
#>  [13] -2.66177740 -0.69162855 -1.05224745 -0.10626818 -0.92227779 -1.56183596
#>  [19] -1.18043433 -0.46058073 -2.21619291  0.76160154 -0.26559128 -0.08340375
#>  [25] -1.00334479 -0.76939944 -0.82573678 -2.82147015 -1.42378814 -2.05772792
#>  [31]  0.41042825 -0.49861421 -0.53433867 -1.40164492 -1.02231988 -1.45082831
#>  [37] -1.78168494 -1.35749518 -0.85135143 -0.80742340 -2.56406959 -0.51610768
#>  [43] -1.31980008 -0.69369421  0.11833316 -0.65073540 -2.23389637 -0.36892466
#>  [49] -0.13393407 -0.07389120  0.03980892  1.04557208 -1.09464774 -0.76034329
#>  [55] -0.43374946 -0.96327643 -1.44163890 -2.05428319 -0.72995441 -2.71387851
#>  [61] -0.29070896 -0.03912142  0.84685646 -0.25465408 -0.66435976  0.26919980
#>  [67] -1.14997047 -1.37134173 -0.28406990  1.11161390 -2.08872109 -1.92703180
#>  [73] -1.29030337  0.40987583  0.48993603  0.45871946 -1.40296966 -3.40132187
#>  [79]  0.02128583 -0.51545785 -1.16228501 -1.59135895 -1.02328640 -1.59944315
#>  [85] -0.35555736  0.06035698 -1.83369469 -1.05884565  0.02753722  0.39607407
#>  [91] -1.48927380 -1.37576533 -1.13823114 -1.14742620 -1.58677485  0.60075494
#>  [97] -0.08465653 -1.48187786 -1.07853654 -0.56820465
#> 
#> $alpha1
#>   [1] -0.82133449 -0.54112967  0.34221318  0.59824616  2.11305294  2.13024710
#>   [7]  0.16662125  1.97447940  0.79848971  0.49555350  1.02095827  0.96352992
#>  [13]  1.76186230  1.52050892  0.85985246 -0.71754449  0.65571913  0.73334731
#>  [19]  1.87106967  0.94046208  1.43165960 -0.80430163  0.01736601 -0.11829274
#>  [25]  0.69133683  0.86274235  1.39891805  2.82902240  2.75015027  1.50914245
#>  [31] -0.15370141  1.56230562  0.06438363  2.19665635  0.70243169  2.53138933
#>  [37]  0.14489325  1.26425802  0.88019017  1.57870037  3.52281189  0.75025828
#>  [43]  0.10417406  2.88360907  0.67064257  0.03852237  2.08449703 -0.80880681
#>  [49]  2.70426181  0.60718300  1.32051779  1.71981751  1.37782567 -0.01203686
#>  [55]  1.86404547  0.74354208  1.10672199  2.28779021  0.42610735  2.63858350
#>  [61]  0.39743386  0.32431900  0.16339115  2.22345556  0.17093147  0.01210716
#>  [67]  1.58836120  0.05623011  0.59750623  0.50448991  1.86536615  1.19410214
#>  [73]  1.06246799  1.67355675 -0.48876119 -0.28558106  0.93138470  3.59708844
#>  [79]  1.84499848  1.48430647  0.82070075  1.29193244 -0.42876471  2.37629160
#>  [85]  0.25107739  1.24642566 -0.37248335  1.03877080 -0.60778037  0.67063028
#>  [91]  0.10495363  0.96838465  2.68808249  0.94235359  3.36587790 -0.02467303
#>  [97] -0.29228916  1.80335334  1.40846286 -0.16950679
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
