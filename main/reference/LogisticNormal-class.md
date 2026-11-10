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
  the reference dose \\(x\*\\) (strictly positive number).

## Details

The covariate is the natural logarithm of the dose \\(x\\) divided by
the reference dose \\(x\*\\), i.e.: \$\$logit\[p(x)\] = alpha0 + alpha1
\* log(x/x\*),\$\$ where \\(p(x)\\) is the probability of observing a
DLT for a given dose \\(x\\). The prior \$\$(alpha0, alpha1) ~
Normal(mean, cov).\$\$

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
#>   [1] -1.107643430 -0.907423364  0.672598826  1.207945123 -1.801474786
#>   [6] -0.656495964 -1.529195722  0.590274631  0.437748822 -0.054022970
#>  [11] -1.097837281  0.058550983 -0.943783370 -1.444298288 -1.938608183
#>  [16]  0.631341463  1.537159691 -2.399436397 -1.621007533 -1.844746172
#>  [21] -0.910630515 -0.348097540 -0.012266908 -1.785455384 -2.457724117
#>  [26] -0.503504690 -2.764292943 -0.682112180  1.332759459 -0.526769587
#>  [31] -1.116096138 -2.853013362 -1.004602397 -2.105364384  1.362046039
#>  [36] -0.332786991 -1.553330905 -0.116714609 -0.745554354 -0.578761324
#>  [41]  0.158917969  0.362465401 -0.243745858  0.175433719 -0.111991894
#>  [46]  0.392691218 -1.087841608  0.279138916 -0.944623738 -0.164381388
#>  [51]  0.088483804 -0.972589337 -0.625275793 -1.135891136  0.139988428
#>  [56]  0.006771755  0.092766504  0.447448818 -2.237772130 -0.737065760
#>  [61] -1.610811666  0.696215992 -1.883308390 -2.462282139 -0.897796093
#>  [66] -1.653392855 -2.075441683 -0.557413302 -1.239550047  0.734527776
#>  [71] -0.394939054 -0.885518322  0.328700017 -1.354647167 -1.092067063
#>  [76] -0.151516599 -0.319441025 -2.782314217 -1.133648388 -1.859216332
#>  [81] -2.313755569 -1.738930393 -0.798690698  0.953853181  1.299926710
#>  [86] -1.197643881 -2.146362580 -0.486534051 -0.160863893 -0.108720656
#>  [91] -1.509456257  0.361592520 -0.713752036 -0.541280721 -1.781893972
#>  [96] -0.664220279 -2.451149507 -2.015978841 -1.605238967  1.099247768
#> 
#> $alpha1
#>   [1]  0.45297860  0.82505101  0.24941707 -0.28960237  2.32134338 -0.26235234
#>   [7]  0.21785807  0.28477955  0.66351343  0.88859319  1.08402210  0.76786962
#>  [13]  2.38790234  1.37439343  0.98774552 -0.62707421 -0.77116143  0.32282490
#>  [19]  0.87477518  1.04731254  1.45079325  2.45989406 -0.25554829 -0.22745096
#>  [25]  0.23076187  0.24068626  2.51369553  0.69890646  0.08485187  1.29770759
#>  [31]  1.72826479  0.20154304  0.83207096  1.17746753 -0.40355738  1.29852985
#>  [37]  2.57075961 -0.46606118 -0.16346682  1.34196900  0.60360623  1.65579149
#>  [43]  0.79547610  1.26998229 -0.17545233 -1.12951220  1.62700308  2.90439120
#>  [49]  0.05605086 -0.21471259 -0.04444067  1.41291539  2.46589188  0.94845413
#>  [55]  1.13755166  0.25597993  0.03331969 -0.30911123  1.08931967  2.02376495
#>  [61]  0.23866844  0.38221436  2.00534357  1.56857232  1.78419204  1.95779377
#>  [67]  2.77838136 -0.98714583  2.14775627 -0.28981098  0.25536808  0.49442671
#>  [73]  1.53151458  0.68117061  2.14111483  0.44233187  0.24327650  0.63119615
#>  [79]  2.32810815  1.76017079  1.62356457  2.97774587  0.23336744 -0.36149624
#>  [85]  0.07436146  2.17121137  1.19271595  1.32876546  1.83392837 -0.21907669
#>  [91] -0.29137146  0.64604978  0.57632432  2.30148598  2.59088544  0.89579464
#>  [97]  2.39666368  2.24434427  2.01241820 -0.25344904
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
