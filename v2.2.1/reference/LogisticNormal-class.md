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
#>   [1] -0.83536949 -1.23919264 -0.57942219  0.85554516  0.72755224  0.09417698
#>   [7] -1.97565214 -1.08160438 -1.22358522  0.03028239 -0.21683590 -1.04130078
#>  [13] -2.45675322  0.77381916 -1.98165505 -2.61631988 -0.31816934  0.24526347
#>  [19] -0.63444272 -1.34438815 -0.78150966  0.91051501 -1.42491119 -1.06504838
#>  [25]  0.11596213 -2.24925556 -0.96600475 -0.96543017  0.34206134 -1.32079534
#>  [31] -0.12010478 -2.57009856 -1.81641502 -0.90966075 -1.26217369 -0.50648188
#>  [37] -0.60070408  0.58468890  0.69694025 -1.25221472  0.24459334  0.69171753
#>  [43] -1.23571516  0.68825042 -0.31250679 -0.80726612  0.20678684 -0.57127315
#>  [49]  0.89094380 -0.58328442  1.14062355 -2.90505468 -1.85073247  0.98253840
#>  [55] -1.45801435 -0.58368640  0.18908819 -1.04597252 -1.09044863 -0.95804559
#>  [61] -1.80377773 -0.14782410 -1.77869138 -1.46684103  0.23153796  2.04126183
#>  [67]  1.82887083 -0.41568540 -1.05250097 -1.67080595  1.02044352  0.46142009
#>  [73]  0.01412235 -0.68601024 -1.16623707  0.10003499 -0.95677934 -2.81139887
#>  [79] -3.26171555 -1.50150028 -1.24509379 -1.60804594 -1.36538724 -2.58534945
#>  [85] -0.88995768 -1.28669431 -0.71766736  0.46478661 -0.47948668 -3.30881310
#>  [91] -1.77826694 -1.24146349 -1.23022997 -0.99485688 -0.50863238 -1.86964010
#>  [97] -1.64983940 -0.05468570 -0.20765777  0.57995460
#> 
#> $alpha1
#>   [1]  0.209483695  0.415735414  2.730993258  1.298488801  0.598419092
#>   [6]  0.482521432  3.228555776 -0.173000097  1.052359522 -0.665723486
#>  [11]  0.975978102 -0.011675143  2.518848315 -0.333418962  1.896563460
#>  [16]  0.868875036  0.217203005  1.040909777  0.823482680  2.451152488
#>  [21]  1.448330305  0.270841724  0.277042518  0.432375922 -0.967795591
#>  [26] -0.799009330  4.553231873  0.748882180 -0.048003876  2.335115969
#>  [31] -1.260108628  2.111874415  0.958620791  1.979482215  0.961765064
#>  [36]  2.310567918  2.302011337 -0.242832394  1.528430938  2.935965595
#>  [41]  0.478477731 -1.050776283  0.895097728 -0.006769682  0.755797838
#>  [46] -1.011581098  0.374539803  1.674016516  0.385565502  0.486995621
#>  [51]  0.862725663  2.838930664  0.776018002  0.673637736  2.036399033
#>  [56]  2.407770883 -0.252982547  2.471275664  0.879319761  0.538556489
#>  [61]  2.523178451 -1.859045284  1.670337831  1.270184291  1.229605584
#>  [66] -0.716759560  1.243773466  1.211677208 -0.238539396  2.223796341
#>  [71]  0.579060321  0.281731361  0.965633554  0.516678700  2.488023637
#>  [76]  0.369026158  0.895200238  1.021745487  0.716373985  1.446059989
#>  [81]  1.252789612  1.084468798  0.696258468  1.367009095  0.508643539
#>  [86]  0.683795515  1.783253900  0.269158445 -0.429138630  1.564168236
#>  [91]  3.100585000  1.748034206  0.513861460  0.301861333  1.785708562
#>  [96]  2.925375808  2.919615972  1.252892721 -0.048284961  1.213048628
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
