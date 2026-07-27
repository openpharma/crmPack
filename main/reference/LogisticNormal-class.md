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
#>   [1] -1.62822045 -1.91191461 -0.17717476 -0.20493696 -0.81379241  0.04087016
#>   [7] -0.82729494 -2.47696687  0.82249158 -3.05810671 -1.11673102 -1.07224227
#>  [13] -0.28669903 -1.53466223 -1.26574734 -3.02368681  1.01928682 -2.64956450
#>  [19] -2.30889540 -0.89460515 -0.09115474 -0.78772183 -1.34277355 -1.73989895
#>  [25] -1.11945711 -1.60444175 -2.72027889 -1.87113961 -1.34162950  0.57987243
#>  [31] -1.46571142 -0.13444304  1.30464929  0.21725273 -1.40894417  0.84477861
#>  [37] -1.55607555 -0.10041464 -0.66834096  0.07826575 -1.14008550 -2.06893813
#>  [43]  0.72381895 -1.76424214  0.05188930 -0.30926315 -2.68960198 -2.33122722
#>  [49] -0.30493563  0.83836904  0.06463765 -0.21170509 -0.70530932  0.23002357
#>  [55] -0.02590979 -0.33584612  0.84399123 -1.41074785  0.18681573 -0.54088766
#>  [61] -2.58052684 -1.06086910 -1.12209665  1.29988992 -0.71498337  0.66325317
#>  [67] -1.69352899 -2.33764742 -1.95645520 -1.84079061 -1.11160543 -0.07133339
#>  [73]  1.11026792 -2.24207128  0.20066181 -2.42806755 -0.87690734 -0.44958319
#>  [79] -0.15869715 -2.48447066 -0.94004602 -0.81275420 -0.81454410 -2.18522460
#>  [85] -2.62601102 -0.99034594 -0.68790686 -1.94160143  0.07265540 -1.50608277
#>  [91]  0.08649136 -0.59321979  0.73344971 -2.25078484 -1.09713111 -0.76227735
#>  [97] -1.33298169 -1.27821377 -0.23080845  0.96568015
#> 
#> $alpha1
#>   [1]  2.1311988675  0.9015474993 -0.0575886127  0.6004790453  1.1081900742
#>   [6]  0.1255319525  3.2998837162  2.3565877020  0.3749474309  0.9055365683
#>  [11]  0.3314158457  0.0002057025  1.3517768887  1.2922736549  1.0279980646
#>  [16]  2.9281244399 -0.8380418622  2.1939374329  1.2324770497  0.7589467617
#>  [21]  0.2323087225  2.0353946750  0.5194530559  1.7816528716 -0.3095698656
#>  [26]  0.7406249246  3.3169415860  2.5796450781  1.2825957814  2.2736511735
#>  [31]  1.4538194040 -1.3863256933 -0.0743342825  1.1674680188  1.2073652071
#>  [36]  1.6431795476  1.8588767965  2.2390998045  1.7589782997 -0.7064911890
#>  [41]  1.1852379671  0.8189376592  0.2056306463  1.6408468336  1.2519277737
#>  [46]  1.0018465770  3.5063508654  2.4951097801 -0.7010422999  0.3220960181
#>  [51]  1.5150487377  1.3138757401  1.7287326482 -0.2895551745 -0.7887103269
#>  [56]  2.0342290369  0.0056379647  1.3465030387  0.7094676270  0.8900766332
#>  [61]  0.6423694946  0.9907987188  0.5413726150 -1.2638092609  1.0146068718
#>  [66]  1.5937167924  1.9615740060  1.4028163375  2.3871124632  2.1681598961
#>  [71]  0.1209005367  0.2982336427  0.1417544810  2.0855908832  0.3131689735
#>  [76]  2.8615722619  1.2397213540  0.7729816599  0.0721368425  2.4147193663
#>  [81]  0.8226359806  0.2172035487  1.2297067227  1.9176188056  3.8084271979
#>  [86]  1.3906051068  1.5833856516  2.3272326021 -0.3623383422  2.4685182242
#>  [91] -0.2150703403  3.3916359899 -1.9425284422  1.8731627254  0.2395828610
#>  [96]  1.8540538002  0.5527192627  1.3858944132  3.1750828802  0.2682738463
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
