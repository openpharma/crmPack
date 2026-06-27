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
#>   [1] -1.26857924 -1.30975444 -1.51414303 -1.13612566 -0.18409629 -0.39011372
#>   [7] -1.01365516 -0.12200567 -2.35814061 -1.23729525  0.24416561 -0.53576962
#>  [13] -1.02035826 -1.05402217 -0.51647038 -0.78638494 -0.10665164  0.46663022
#>  [19] -2.19594620 -0.17845970 -1.52181171  0.16070498 -2.55606424 -1.06927152
#>  [25] -0.16904220 -1.52460968 -0.26838870 -0.60210339 -1.33818066 -0.50601610
#>  [31] -0.33535861 -0.58768827 -1.55370907 -0.44849335 -0.49995673 -0.64413663
#>  [37] -0.97873076  0.41877718 -0.06778320 -0.18317181 -0.40592904 -1.48847368
#>  [43]  0.70476393  1.05296883 -1.05421623 -2.66030307 -0.12300919 -0.73588067
#>  [49] -2.05339999 -2.41445457 -0.99069373 -1.39906177 -1.74373014 -0.33676466
#>  [55]  0.03756686  0.80613896 -1.84279642 -0.52560056 -0.16123485 -0.30471565
#>  [61]  0.59649190  0.52507403 -2.82963897 -0.50197916 -1.16441603 -0.71983451
#>  [67] -0.02232506 -0.08263370  0.12004001 -0.14149258 -0.24627500  1.86852080
#>  [73]  0.27267991  1.13972590  0.43225513 -0.88477840 -0.75742723 -0.32709255
#>  [79] -0.03267808 -0.19748077 -1.97867969  0.62526558  0.41346186 -2.35818409
#>  [85] -1.35902538 -2.50071120 -0.63202384 -0.55934683  1.08483724  1.22762097
#>  [91] -0.29647060 -0.84702863  0.16945340 -0.57575654 -0.66556643 -2.26696420
#>  [97] -0.66188565 -1.87300294  0.76296387  1.25715138
#> 
#> $alpha1
#>   [1]  0.98127502  1.40240186  1.31009265  2.18452322  1.32444478  2.22675532
#>   [7]  1.63721981  0.96792056  1.24447143  1.86781788 -0.63743253  1.44632404
#>  [13]  0.11743902  0.58897492  1.10827496  0.65804986 -0.22454136  2.20018853
#>  [19]  2.83681532  0.92936558  2.15723012  1.47008431  1.16014508  3.05780779
#>  [25]  2.11197298  1.86765474 -1.45018730  0.95458703  1.67554595  1.93760431
#>  [31]  0.84077338  2.62847625  1.12582215  0.26067410  1.11549484  0.38257983
#>  [37] -0.31455589  0.02452691 -0.43287823  0.40574091  0.58347591  0.19953637
#>  [43] -0.70385681  0.14133106  0.69247963  2.15827225  0.43727660 -0.43146846
#>  [49]  1.38549980  1.59407784  1.73801308 -0.58124026  0.85604516  1.98479633
#>  [55]  0.02214974  1.64170865  1.15530416  0.71244686  0.57944870  1.36505885
#>  [61] -1.38740709  0.38923743  2.89190830  1.37629733  0.36410408 -0.42989294
#>  [67]  0.06018914 -0.78696743 -0.63143761  0.83991189  1.13561184  0.47347726
#>  [73]  0.12435283  1.19386769  1.10258603 -0.52258360  0.39128352  0.91536299
#>  [79]  0.03618429  0.21975383 -0.65210375 -0.52311787 -0.40623981  0.65648586
#>  [85]  1.28794251  1.98779784  0.86527395  1.47515025  0.43790344  2.06144174
#>  [91]  0.29527608  0.48023137  0.78461570  0.13902965  0.88196852  0.76126048
#>  [97]  3.15387915  1.45512651  1.27607819 -0.14553370
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
