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
#>   [1] -0.82973300 -1.05064034 -1.46211183 -1.53601476 -0.77789274 -1.40588501
#>   [7] -1.37846472 -0.73076073 -1.37296934 -0.63229957 -1.10269420 -1.19026703
#>  [13] -0.22143107 -1.77751382 -0.18498837 -1.11836768  0.62206284  0.73293656
#>  [19]  0.01752377  1.18292994 -1.61715468 -0.63371229 -2.03287139  1.56433922
#>  [25] -1.55964922 -0.70371074 -0.12698542 -1.57496727 -1.75949009 -2.52504585
#>  [31] -0.47804985 -2.40647235 -0.59160933 -0.45032189 -1.15686086 -0.47104056
#>  [37]  1.15349632 -1.03856297 -1.13377998 -1.65780288 -2.56693706 -1.51957135
#>  [43] -1.79964382 -1.33326937 -2.04474400 -0.64099869 -1.84132015  0.06736618
#>  [49] -1.15940632 -1.75960429 -0.82109172 -1.98029093 -0.36084359 -1.60221825
#>  [55] -2.73477184  0.58600214  0.14281794 -2.31430074 -2.52316194  0.10182211
#>  [61] -1.02722276 -0.78955030 -2.48530350 -1.87732420 -2.54975139  0.41271202
#>  [67] -1.07599680 -0.04923724 -0.10459653 -0.93442669 -1.61705912 -2.19696555
#>  [73] -1.27478676 -0.22569219 -1.42379435 -0.76430556  0.40762644 -2.55990868
#>  [79] -0.56453911 -0.54246000 -0.56661064 -0.66990091 -0.17865521 -1.52932082
#>  [85] -1.27555189  1.82358441  0.08325275  0.75789831 -0.79053680 -1.52456489
#>  [91] -0.25077898 -0.35672124 -0.71002274 -1.13596437 -2.56817290 -1.43209869
#>  [97] -1.49597585 -2.13842971 -0.01110264 -0.97595521
#> 
#> $alpha1
#>   [1]  1.444874351  0.642781743 -0.933191808  1.398666609  0.759039639
#>   [6]  0.653274886  0.456371920  0.804091519  0.430304625  1.643004867
#>  [11] -0.003426733  2.366544235 -0.465667673  2.550873788  1.602448214
#>  [16]  1.410764340  0.169553536  0.014964156 -0.638693126 -0.564423973
#>  [21] -0.753216641  1.414923627  0.165584104 -1.201122817  1.577369759
#>  [26]  0.750808806  1.593631560  2.409754992  0.683766356  1.416113422
#>  [31]  0.890093758  1.947712477  2.127120006  0.283429942  0.530466692
#>  [36]  1.622316643 -0.488883133  1.610586882  1.289067942  1.440708590
#>  [41]  2.810730957  1.243580600  1.756415481  0.800804908  3.515128772
#>  [46] -0.472497564  2.029541186 -0.407775441  0.810668091  1.741514468
#>  [51]  0.045807122  3.016545577 -2.133439573  3.040519957  1.932043449
#>  [56]  1.093968805  0.671883251  2.888761177  1.836403433  2.415815837
#>  [61] -0.019855600  0.234859258  1.490236530  1.200368079  0.439458154
#>  [66] -1.055586642  1.295265788  0.443336376 -1.407041679  0.964738127
#>  [71]  0.914398088  1.635206407  2.682417574 -1.356969656  1.427367771
#>  [76]  0.713293316  1.932742560  2.216292523  0.349085746  0.707271939
#>  [81]  0.919302943  0.706463455  1.548541973  1.352042067  2.060579244
#>  [86] -0.477436750 -1.262873320  0.354943356 -0.774139794  1.690545453
#>  [91]  2.118602199  1.481987264 -0.111557155  0.102449881  1.512560981
#>  [96]  1.019753708  0.681604500  3.117487620 -2.036631556  1.257260342
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
