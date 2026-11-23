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
#>   [1] -1.01158483 -0.91144611 -1.00299729 -0.50758496 -1.68020461 -0.70001902
#>   [7]  0.65722473 -1.74289882  0.92751038 -1.96217955 -1.65221251 -0.56144067
#>  [13] -1.02694617  0.23827484 -1.14099163 -0.36436741 -2.01048441 -1.24116715
#>  [19]  0.22341437 -0.92868336 -0.01849211 -1.66992492  0.32030290 -1.09779513
#>  [25]  0.02855183 -0.53955926 -0.38874293 -0.42874509 -0.60009911  0.94133286
#>  [31] -0.97185767  0.41625326 -2.19056811 -1.95489920  1.69752652 -1.29204369
#>  [37]  0.21418539 -1.02075234 -0.36687523  0.42467181 -0.80325990 -2.18427504
#>  [43] -1.93097835 -1.07927983 -2.13572336 -1.92893114 -0.83619687 -1.62476585
#>  [49] -0.23280464 -0.97423646  0.43355553 -0.92329055 -0.21866804 -1.51241848
#>  [55] -1.88217998 -1.83166016 -0.39349576 -1.83054946  0.30120861 -2.25365638
#>  [61] -1.07411473  1.55479531  0.52278212  0.47207260  0.04812945 -1.19780027
#>  [67] -0.28584410 -0.01490815  0.03288793  0.40961567 -0.07245325  0.32745599
#>  [73] -2.78697622 -1.18352280 -1.72206938 -0.03631737 -0.43032558 -2.79903198
#>  [79] -1.29783090 -1.02665792 -1.90452279 -1.96137863 -0.93641665 -1.90134164
#>  [85] -0.85036342 -1.22793613 -1.04221560 -2.46584798 -1.01800466 -1.03557590
#>  [91] -0.39448208 -1.65615479 -0.46513312  0.40657111 -3.29038135 -1.67343771
#>  [97]  0.28909866 -0.99288813 -1.72624459 -1.33098805
#> 
#> $alpha1
#>   [1]  1.93143499  0.47545319  0.64731080  0.43071686  0.20024723  1.52707846
#>   [7]  0.14709807 -0.29225146 -0.39082154  1.07650684  1.60263196  1.73463730
#>  [13] -0.23888982  0.52787264 -0.09188814  0.87326925  1.31817917  1.24724390
#>  [19] -0.68276647  1.75408954  0.73368701  0.91893572 -0.89712626  0.96048715
#>  [25] -0.73980707  2.59361859 -0.48882196  1.44749656  2.08326991 -1.39317283
#>  [31]  2.04095347  0.87048115  1.74000930 -0.03612951 -0.24089571  0.07997189
#>  [37]  0.33200087  1.45179778  2.09360344  1.04718838  1.47217527  0.20312469
#>  [43]  1.99439637  1.06585534 -0.79528128  1.46560600  1.28997629  2.10948258
#>  [49] -0.74105784  0.55923611 -0.40930903  0.57090494  1.28868657  1.30065907
#>  [55]  1.80332441  2.00272140  0.51761166  1.96646264  0.24037678  0.08456333
#>  [61]  0.57261107 -0.99257553  2.25985615  1.18192357  0.04801445  2.73447912
#>  [67]  2.64831561  0.36622570  1.12755352  0.53405514  0.80811000  0.81294241
#>  [73]  1.52651937 -0.47848443  0.87420032  0.43371677  1.07980821  1.22450301
#>  [79]  1.39393298  1.59097535  1.35641703  0.96202796  1.99149555  2.18607351
#>  [85]  0.99442673  0.32292294  0.72868608  0.48749684  1.64034790  0.98581348
#>  [91]  1.01351354  1.31644191  0.11227525 -0.30084957  1.67176640  2.04363318
#>  [97]  1.27863718  2.35156159  0.31265566  1.05855239
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
