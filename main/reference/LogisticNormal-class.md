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
#>   [1] -1.56173085 -1.84709142  0.21752905 -2.02127954 -1.42208844 -1.03662979
#>   [7] -0.93908959  0.82787896 -1.36026489 -0.96612120 -2.54264832 -0.73845100
#>  [13]  0.12459543  0.94235378 -0.06833635 -0.59969032  0.38724727 -2.98812834
#>  [19]  0.49353191 -1.50829225 -0.83573070 -1.04124861  0.37815855 -1.46387066
#>  [25] -1.22669177  0.15481506 -0.70510320 -1.79474570 -1.08917675 -1.52738992
#>  [31] -2.84331184 -1.69209988 -0.91148050 -1.19015953 -0.27350522 -0.65775752
#>  [37] -0.32446702  1.27421201 -2.29029657 -2.08646556 -1.58376607 -1.63702526
#>  [43]  0.01816608 -0.32366958 -1.59345348 -0.03072588 -1.96885608  0.31677187
#>  [49] -1.49968210 -1.74783879  0.26703979 -0.92504621 -1.46306554 -2.57724959
#>  [55] -1.42786402 -2.37314901  0.54777652 -1.18849761  0.28609384  0.08160158
#>  [61] -2.52609766 -1.51184399 -0.70066146  0.89307095 -2.32419088 -1.21463006
#>  [67] -1.88180649 -1.86855843 -1.03617626 -2.84023651 -1.32967468  0.81408669
#>  [73] -2.05841642 -1.20230227  0.27128496 -0.59109605  0.06862398 -1.06095207
#>  [79] -1.10525185 -0.52134414 -0.48727795  0.92529348 -0.71499502 -1.13482569
#>  [85] -0.62719492 -2.63144966 -0.83688747  0.80066753 -1.63363380  1.27953204
#>  [91] -0.59552543 -1.11947788  0.90684428 -0.84935384 -1.76392661 -1.61703318
#>  [97] -1.44638782 -2.28911979 -0.98178491 -0.54588351
#> 
#> $alpha1
#>   [1]  1.61768728  1.07950695  1.20556903  2.43204750  1.16534758  0.70216241
#>   [7]  1.17827483  0.89202416  2.39035953  1.03185136  2.07639416  1.02097413
#>  [13]  1.01095875  0.91475787  2.15405789  0.95346954  0.16870283  1.58085644
#>  [19]  0.05518624  2.25048318  1.60849881  2.13135799  0.66023063  3.70061640
#>  [25]  0.29440630 -0.14877139  0.82925012  2.64058817  1.69049051  1.40854951
#>  [31]  2.13491957  0.30150287 -0.12267280  0.80396471  0.44784133 -0.31454447
#>  [37]  0.88199432 -0.08705706  2.23039199  2.77575454  0.91651704  2.53838588
#>  [43]  1.12424025 -0.31477267  2.64988390  0.87070662  3.71973731  0.07099166
#>  [49]  3.00326560  1.53634639  0.14146639  2.26149722  0.68727780  1.49386500
#>  [55]  0.85477327  1.11723911 -0.71808131  1.00534106  0.52853210 -0.27480843
#>  [61]  0.92232257  0.12787697  1.89441817  0.29186636  0.47600153  0.71476465
#>  [67]  1.96633726  1.15751031  0.98111669  1.78852777  1.73570757  0.60648482
#>  [73]  0.72480269  0.54473022  0.09658289 -0.57990821  0.36448029  0.76196660
#>  [79]  0.76600146  0.72293590  1.45004687 -0.33442184  1.57149843  1.73645330
#>  [85]  0.11601980  0.48690740  2.31732244 -0.65237404  0.53580260  0.04626691
#>  [91]  0.55129495  1.31844344 -0.34574254  0.42139634  1.43888574  1.68367693
#>  [97]  0.21829460  3.22454767  0.42902990  1.22969495
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
