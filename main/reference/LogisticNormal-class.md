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
#>   [1] -1.768027613 -1.598596774 -1.028791049 -0.576918510  0.617770906
#>   [6] -1.375781623 -1.124895545 -2.863640448 -0.261083922 -0.718475503
#>  [11]  0.177693133 -2.429261088 -0.791515163 -1.968099839 -0.871614169
#>  [16] -2.201253637 -0.797116005 -2.383402915 -0.233526820  0.950124233
#>  [21] -1.228080534 -2.160621297 -1.003769559 -1.360683208 -1.054363155
#>  [26]  0.928292850 -0.234093034 -2.793655832 -1.348849137  1.421433428
#>  [31]  0.687372719 -1.789663667 -0.430744385 -0.644198923 -0.598126699
#>  [36] -0.421518568 -0.251598953 -0.282787712 -0.087737189 -2.179578752
#>  [41] -1.985881658 -1.994007941 -0.207484324  0.747534992 -1.785817307
#>  [46] -1.334748088 -0.021083621 -2.089827527 -2.590356635  0.537522766
#>  [51] -0.493797654 -1.653432157 -0.222377879  0.527445954 -0.844777131
#>  [56]  0.007428819 -0.986915865 -1.400742215 -1.640696485 -0.239728955
#>  [61] -0.626504389 -0.663782625 -0.323391493 -2.525250054 -0.694901831
#>  [66] -1.491967806 -0.363281871 -0.193863757 -3.545260829  0.110185725
#>  [71] -0.843504354  0.449233690  0.427965232 -0.872816123 -0.373640323
#>  [76]  0.127544851 -0.859363688 -0.624645643 -1.203098974 -1.107052538
#>  [81] -1.016295796 -1.714429028  1.221193125 -1.305334314  0.611800919
#>  [86] -0.817681984 -1.751525177  0.339008273 -2.009386437 -0.997308908
#>  [91] -0.235184814  0.119255020 -1.544972529 -0.587635158  1.206412732
#>  [96]  0.149802516 -1.444809593 -1.077288437  1.407453063 -1.169463821
#> 
#> $alpha1
#>   [1]  0.715539792  2.002703496 -0.045029441  0.469476784 -0.114743014
#>   [6]  1.028122128  2.541784303  1.648546590  0.370755989  0.719903303
#>  [11]  1.994914706  2.234765109  2.037695873  3.650938244  0.415394577
#>  [16]  2.484726466  0.563129968  2.250108693  0.157921901 -0.537380399
#>  [21]  0.599418034  0.753514741  1.159858865  2.089121513  0.254421831
#>  [26]  0.767752186  1.348773798  2.213938406  2.075826950 -0.859343385
#>  [31]  0.699153198  1.876541890  2.092494511  2.112052144  0.215857506
#>  [36]  1.668759605 -1.281352028  1.737819630  0.590759934  2.333605100
#>  [41]  2.343527495  0.861717430 -0.255792375  0.919082810  1.942831645
#>  [46] -0.121224368  0.991071538  1.216386930  0.476630310 -0.539002688
#>  [51] -0.560233162  0.874123485  1.144104673 -0.379501067  1.071208945
#>  [56]  1.689172458  0.956129245  2.515283468  1.109319561  1.204213373
#>  [61] -0.578966976  0.437699166  1.216764143 -0.053386067  0.695211958
#>  [66]  1.127496556 -0.177767604 -0.394918512  1.440821264  0.532077634
#>  [71]  1.550647399  0.570644380  0.035340773  0.416505833 -0.356080143
#>  [76]  0.003229525  1.715104907  0.565779628  0.607584118  0.947305501
#>  [81]  1.760190635  2.327285613 -0.017390200  1.997701750  0.422170111
#>  [86]  0.061494482  1.743219081  1.949964386  3.343970108  2.116250307
#>  [91]  0.247033300  1.736375596  1.107972838  0.252078881  0.030555517
#>  [96]  0.080152179  1.290309785  1.301696382  0.332611849  0.946266699
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
