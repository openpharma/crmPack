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
#>   [1] -3.04675432 -3.34667224 -0.72750509  0.51215611 -1.43047551 -0.78702951
#>   [7] -1.12581517 -1.63538280 -1.07269335 -2.02108927 -2.32902779 -0.51367626
#>  [13] -1.31016776 -0.80486270 -0.74250845  0.39064394 -0.23985885  0.26386356
#>  [19] -3.32224834  0.68629076 -0.57150985 -0.26350088 -1.06673105  0.78595344
#>  [25] -0.57092442  0.41387840 -2.06676975 -0.49147714  0.63440100 -2.03956290
#>  [31] -0.74996319 -0.84777284 -0.73973307 -2.87237220 -0.74737839 -1.88472391
#>  [37] -2.02815088 -1.68487894 -1.71637331 -0.15375296 -1.85567474 -1.86919179
#>  [43] -1.85170764 -1.82298726  1.22985224 -0.71692987 -3.58452135 -0.45396866
#>  [49] -2.42076548  0.30301070 -0.05297026 -1.92888087 -1.34390778 -1.25640638
#>  [55] -0.25097717 -0.54580711  0.28774882 -2.38308801 -0.43594896  0.69039260
#>  [61]  0.05932011 -1.66690673  1.09521014  0.21264914  0.22644866  1.09772064
#>  [67] -2.46329912  0.66109349 -0.60806517 -0.44632593 -0.45052868 -2.13145529
#>  [73] -2.77115835 -1.42547914 -0.05752040  0.74895826  0.57073565 -0.31465013
#>  [79] -0.31273175 -0.57599079 -2.17646890 -0.91252824 -0.95891831 -0.81566254
#>  [85] -1.44369034 -2.17833467 -1.68486115  1.32079091 -2.88587434 -1.43093350
#>  [91] -1.60261407 -1.06096076 -2.03231565 -0.37661279  0.42197637 -0.27220815
#>  [97] -0.45374792 -0.15960290 -2.02026767 -1.38874941
#> 
#> $alpha1
#>   [1]  2.604493053  2.736946799  1.520788668 -0.631943588  0.407542559
#>   [6]  0.036972381 -1.026957120  1.404094709  1.167778438  0.615165251
#>  [11]  1.635915278  0.496123505  1.972740414  2.807910271 -0.369106149
#>  [16]  0.662417397  1.735724326  0.591065495  3.830956491 -0.531922035
#>  [21]  1.064569236  0.657494103  0.338155964  0.083310621  1.086937100
#>  [26] -1.038434741  2.009095757 -0.118380544  0.262836324  2.266393312
#>  [31]  1.639890975  0.758267363  0.304541836  2.604168611  1.845259179
#>  [36]  0.492815349  2.039979338  1.035538692  2.028189002  1.895175337
#>  [41]  1.653928437  1.144522644  2.340456277  2.213749571 -0.005845527
#>  [46]  0.930997784  2.145441038  0.822511818  2.045210545  1.638381721
#>  [51]  0.426850619  0.614669063  1.662206858  1.036463778  0.552935696
#>  [56]  1.142451610  0.146382377  1.126680863  0.699117436  0.770844343
#>  [61]  0.543595508  1.001329526 -0.383364378 -0.840275175  1.082772466
#>  [66] -1.495495970  1.440619337 -0.280019705 -0.046659681  0.388177312
#>  [71]  0.607683774  1.822843434  2.373910975  1.265581474  0.764860611
#>  [76]  1.014121596 -0.292861440  0.322958354  2.186357832  0.151108962
#>  [81]  1.907294471  0.799571677  1.554542155  1.309634400  1.357796348
#>  [86]  2.685560785  3.107980993 -0.861259514  1.854077187  1.405291646
#>  [91]  0.773039853  1.532688534  1.437518966 -1.101616980 -0.193923806
#>  [96]  2.482332522 -1.401184383  1.854011343  3.448746314  0.760307259
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
