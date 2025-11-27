# `LogisticLogNormal`

**\[stable\]**

`LogisticLogNormal` is the class for the usual logistic regression model
with a bivariate normal prior on the intercept and log slope.

## Usage

``` r
LogisticLogNormal(mean, cov, ref_dose = 1)

.DefaultLogisticLogNormal()
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
a given dose \\x\\. The prior \$\$(alpha0, log(alpha1)) ~ Normal(mean,
cov).\$\$

## Note

Typically, end users will not use the `.DefaultLogisticLogNormal()`
function.

## See also

[`ModelLogNormal`](https://openpharma.github.io/crmPack/reference/ModelLogNormal-class.md),
[`LogisticNormal`](https://openpharma.github.io/crmPack/reference/LogisticNormal-class.md),
[`LogisticLogNormalSub`](https://openpharma.github.io/crmPack/reference/LogisticLogNormalSub-class.md),
[`ProbitLogNormal`](https://openpharma.github.io/crmPack/reference/ProbitLogNormal-class.md),
[`ProbitLogNormalRel`](https://openpharma.github.io/crmPack/reference/ProbitLogNormalRel-class.md),
[`LogisticLogNormalMixture`](https://openpharma.github.io/crmPack/reference/LogisticLogNormalMixture-class.md),
[`DALogisticLogNormal`](https://openpharma.github.io/crmPack/reference/DALogisticLogNormal-class.md).

## Examples

``` r
my_model <- LogisticLogNormal(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
  ref_dose = 50
)
my_model
#> An object of class "LogisticLogNormal"
#> Slot "params":
#> An object of class "ModelParamsNormal"
#> Slot "mean":
#> [1] -0.85  1.00
#> 
#> Slot "cov":
#>      [,1] [,2]
#> [1,]  1.0 -0.5
#> [2,] -0.5  1.0
#> 
#> Slot "prec":
#>           [,1]      [,2]
#> [1,] 1.3333333 0.6666667
#> [2,] 0.6666667 1.3333333
#> 
#> 
#> Slot "ref_dose":
#> An object of class "positive_number"
#> [1] 50
#> 
#> Slot "datamodel":
#> function() {
#>       for (i in 1:nObs) {
#>         logit(p[i]) <- alpha0 + alpha1 * log(x[i] / ref_dose)
#>         y[i] ~ dbern(p[i])
#>       }
#>     }
#> <bytecode: 0x55cf5fd3de70>
#> <environment: 0x55cf66f82e00>
#> 
#> Slot "priormodel":
#> function() {
#>       theta ~ dmnorm(mean, prec)
#>       alpha0 <- theta[1]
#>       alpha1 <- exp(theta[2])
#>     }
#> <bytecode: 0x55cf605f06c8>
#> <environment: 0x55cf66f82bd0>
#> 
#> Slot "modelspecs":
#> function(from_prior) {
#>       ms <- list(mean = params@mean, prec = params@prec)
#>       if (!from_prior) {
#>         ms$ref_dose <- ref_dose
#>       }
#>       ms
#>     }
#> <bytecode: 0x55cf6077ace0>
#> <environment: 0x55cf66f82bd0>
#> 
#> Slot "init":
#> function() {
#>       list(theta = c(0, 1))
#>     }
#> <bytecode: 0x55cf608090a0>
#> <environment: 0x55cf66f82bd0>
#> 
#> Slot "datanames":
#> [1] "nObs" "y"    "x"   
#> 
#> Slot "datanames_prior":
#> character(0)
#> 
#> Slot "sample":
#> [1] "alpha0" "alpha1"
#> 
```
