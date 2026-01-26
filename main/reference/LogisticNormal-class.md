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
#>   [1] -0.22553475 -0.23686011 -1.36453728  0.23981698 -1.77504255  2.02767085
#>   [7] -1.08168446  0.64349784 -0.22047452  0.04946599 -1.44298219 -0.17842366
#>  [13] -2.46021484  1.68035594 -0.27551295 -0.16696139 -2.59757156 -0.12164754
#>  [19]  0.69074971 -0.62099120  0.56564394  0.05534594 -1.23116468 -2.43271664
#>  [25] -1.95158253  0.06263254 -1.08057672 -0.02739166 -0.98862842  0.54462761
#>  [31] -1.27592998 -0.10353916 -0.68528296 -1.75405968 -2.82913797 -0.09685068
#>  [37] -1.70284076 -1.94951681 -0.15354898 -0.93977283 -0.98110686  0.19506016
#>  [43] -0.87395121  0.20962415 -0.75712970 -0.71333410 -1.16428557 -0.09664909
#>  [49]  1.06708189 -0.45667237 -0.99480814 -0.80960109  1.56690004 -2.18966611
#>  [55] -0.13209049 -0.48792938 -0.88688600 -2.57652712 -2.14732575 -0.54913862
#>  [61] -1.65464594 -0.43865321 -2.21394447 -0.56146196 -1.19800577 -0.39546400
#>  [67] -0.63688924 -0.44587317 -1.09552382  0.07621762 -2.64391194 -0.80402127
#>  [73] -0.26136464 -0.34937326 -0.52838412 -1.35997043 -2.63436161 -0.73934488
#>  [79] -2.09215078 -0.32533272 -1.00538052  0.22672734 -0.32802409 -1.03769167
#>  [85] -0.38345532 -1.32587684 -2.95773810 -0.29745687 -1.15014950 -0.29945530
#>  [91] -0.25845087 -2.67393105 -0.04073911 -0.76515218 -2.40558513 -0.04213467
#>  [97]  0.30469342 -0.52915618  0.82440444 -0.88294973
#> 
#> $alpha1
#>   [1]  1.08304559  1.38853945  1.14739668 -0.25879592  1.20877826 -1.44951850
#>   [7]  1.78322303 -0.27501776  2.19129414  0.55782196  0.59616785  1.95643176
#>  [13]  2.21628011 -0.89404218  0.83060623 -0.36537692  2.16706693 -0.64798742
#>  [19]  0.79421235  1.47149490  0.70144468 -0.43828296  2.38269920  1.34384693
#>  [25]  1.14545006  0.75331388  1.40914480  0.96354119  0.81348893  0.94287945
#>  [31]  0.58005182  0.78264114  0.70112430  2.78042424  1.38849551  1.99824592
#>  [37]  1.46794528  1.65539833 -0.56830961  1.28634870  1.62701567  2.78027338
#>  [43]  1.74167602  0.75491145  3.05469002  0.86421994 -0.81925002  1.58993805
#>  [49]  1.10550442  1.01518299  1.41420714  1.39510253  0.50591855  1.30798966
#>  [55]  0.12406463  0.52139986  1.13000905  1.58536262  1.58054966  1.48610136
#>  [61]  1.11944737  0.70471211  2.44928095  0.78647887 -1.07273628  2.18117542
#>  [67]  2.05903208  0.40193590  2.19342613 -0.25187719  1.66560605  1.81207286
#>  [73]  1.46461286 -0.08635922  0.89482355  2.24416076  1.61744375 -0.18413674
#>  [79]  2.04330748  0.93368460  0.46473781  0.20460082  1.51318350 -1.30449076
#>  [85]  0.86301406 -1.35681860  2.61674975  0.28375787  1.10199544 -0.50631113
#>  [91]  0.32736677  2.12694152  0.49941963  1.72083353  1.69883124  0.90124944
#>  [97] -0.53598070 -0.28742604  0.88605061  1.18834737
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
