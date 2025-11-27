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
#>   [1] -1.061267745 -0.804512671 -1.377338565  0.417766525 -0.730866429
#>   [6] -1.009333581 -1.378495567  0.109220871 -2.184122468  0.213578590
#>  [11] -1.283041194 -1.876728708 -2.923578776 -1.087867714 -2.226518227
#>  [16]  1.867369254 -0.573068495 -1.880553703 -3.749687572 -0.766829133
#>  [21] -1.394300956 -1.769393323 -1.351277576 -0.811865266 -0.765237334
#>  [26] -1.629736533 -0.831007148 -0.395146642  0.412396230 -1.828079827
#>  [31] -1.128186927 -0.426612213 -1.939259864 -0.881868440 -0.141958844
#>  [36] -1.413336594 -0.853287675 -1.331389337 -1.713235401  0.092769640
#>  [41] -0.624771445 -2.538560888 -2.523772869 -1.722777939 -3.785649088
#>  [46] -2.299697106  0.051551289 -2.019717178 -0.462830640 -1.253377170
#>  [51]  0.680577510 -1.709414615 -0.878681316 -0.397998464 -0.579835750
#>  [56]  0.036912369 -1.150229935 -1.901817963 -0.380443724  0.222839967
#>  [61] -1.727896119 -0.790599785 -1.152192879  0.101754112 -2.044016235
#>  [66] -1.041248730 -2.405042630 -2.141757539 -1.285239999 -0.233090722
#>  [71]  0.573232583 -0.485272806 -1.289831901 -0.090848773 -0.265231460
#>  [76] -1.818458161 -2.249170603 -2.202270532 -1.335637927 -1.509742526
#>  [81] -1.266195061  0.231920962 -1.457861122 -0.594811401 -1.057114664
#>  [86] -0.880051859 -1.046134023  0.012999505 -1.172624397 -0.227344734
#>  [91] -0.938276213 -1.361776340 -0.633556789 -0.494766945 -0.002250371
#>  [96] -0.466017136  0.099604568 -0.876671588 -0.302665041 -0.017538989
#> 
#> $alpha1
#>   [1] -0.47790230  0.57636236  1.84295840  1.12217036  0.49739132  1.94383841
#>   [7]  1.08730644  1.55570475  1.81700200  0.03785731  1.85464153  1.38004582
#>  [13]  2.02030971  0.29344180  0.65593335  0.02853866  0.90564774  2.40690910
#>  [19]  2.04410786 -0.51762161  1.65427904  2.26165693  2.11744181  1.08897891
#>  [25]  0.78046823  0.83973366  2.68752680  0.52712856 -0.43461173  1.62359432
#>  [31]  1.05183189 -0.26361671  2.04303182  1.78251280  0.59866667  1.19304642
#>  [37]  1.68522289  0.77026283  1.18572264  0.90381690  0.33107716  0.90616613
#>  [43]  0.44396281  1.09263199  2.23277397  0.99501212  0.73214555  1.03003176
#>  [49]  0.26614013  1.78446070  0.04505618  2.38563151  1.29235746  1.73152316
#>  [55]  0.64563979  0.18376368  1.02585406  0.87024795  0.45081252  0.74387770
#>  [61]  0.26758084  0.05196021  1.83163240  1.06927996  0.53443256  3.40278456
#>  [67]  4.25961815  0.19396882  1.78872302 -0.30500886  1.19019579  1.10553426
#>  [73]  1.59858807  0.93237258  1.17430433  1.86573823  0.46540223  2.15788136
#>  [79]  2.76390217  1.31003218  1.41679380  0.48081209  2.58983445  1.94327259
#>  [85]  0.96487778  1.18070001  0.10122265  0.82334648  0.98112381  0.53960790
#>  [91]  1.65044275  3.01542844  0.88578987  0.47031576  1.09178411  0.20008611
#>  [97]  0.99706066  0.14757386  1.42086485  1.89759057
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
