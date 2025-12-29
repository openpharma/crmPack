# `LogisticLogNormalGrouped`

**\[experimental\]**

`LogisticLogNormalGrouped` is the class for a logistic regression model
for both the mono and the combo arms of the simultaneous dose escalation
design.

## Usage

``` r
LogisticLogNormalGrouped(mean, cov, ref_dose = 1)

.DefaultLogisticLogNormalGrouped()
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

The continuous covariate is the natural logarithm of the dose \\x\\
divided by the reference dose \\x\*\\ as in
[`LogisticLogNormal`](https://openpharma.github.io/crmPack/reference/LogisticLogNormal-class.md).
In addition, \\I_c\\ is a binary indicator covariate which is 1 for the
combo arm and 0 for the mono arm. The model is then defined as:
\$\$logit\[p(x)\] = (alpha0 + I_c \* delta0) + (alpha1 + I_c \* delta1)
\* log(x / x\*),\$\$ where \\p(x)\\ is the probability of observing a
DLT for a given dose \\x\\, and `delta0` and `delta1` are the
differences in the combo arm compared to the mono intercept and slope
parameters `alpha0` and `alpha1`. The prior is defined as \$\$(alpha0,
log(delta0), log(alpha1), log(delta1)) ~ Normal(mean, cov).\$\$

## Note

Typically, end users will not use the
`.DefaultLogisticLogNormalGrouped()` function.

## See also

[`ModelLogNormal`](https://openpharma.github.io/crmPack/reference/ModelLogNormal-class.md),
[`LogisticLogNormal`](https://openpharma.github.io/crmPack/reference/LogisticLogNormal-class.md).

## Examples

``` r
my_model <- LogisticLogNormalGrouped(
  mean = c(-0.85, 0, 1, 0),
  cov = diag(1, 4),
  ref_dose = 50
)
my_model
#> An object of class "LogisticLogNormalGrouped"
#> Slot "params":
#> An object of class "ModelParamsNormal"
#> Slot "mean":
#> [1] -0.85  0.00  1.00  0.00
#> 
#> Slot "cov":
#>      [,1] [,2] [,3] [,4]
#> [1,]    1    0    0    0
#> [2,]    0    1    0    0
#> [3,]    0    0    1    0
#> [4,]    0    0    0    1
#> 
#> Slot "prec":
#>      [,1] [,2] [,3] [,4]
#> [1,]    1    0    0    0
#> [2,]    0    1    0    0
#> [3,]    0    0    1    0
#> [4,]    0    0    0    1
#> 
#> 
#> Slot "ref_dose":
#> An object of class "positive_number"
#> [1] 50
#> 
#> Slot "datamodel":
#> function() {
#>       for (i in 1:nObs) {
#>         logit(p[i]) <- (alpha0 + is_combo[i] * delta0) +
#>           (alpha1 + is_combo[i] * delta1) * log(x[i] / ref_dose)
#>         y[i] ~ dbern(p[i])
#>       }
#>     }
#> <bytecode: 0x56365d46d910>
#> <environment: 0x563668469998>
#> 
#> Slot "priormodel":
#> function() {
#>       theta ~ dmnorm(mean, prec)
#>       alpha0 <- theta[1]
#>       delta0 <- exp(theta[2])
#>       alpha1 <- exp(theta[3])
#>       delta1 <- exp(theta[4])
#>     }
#> <bytecode: 0x56365dcee690>
#> <environment: 0x563668469998>
#> 
#> Slot "modelspecs":
#> function(group, from_prior) {
#>       ms <- list(
#>         mean = params@mean,
#>         prec = params@prec
#>       )
#>       if (!from_prior) {
#>         ms$ref_dose <- ref_dose
#>         ms$is_combo <- as.integer(group == "combo")
#>       }
#>       ms
#>     }
#> <bytecode: 0x56365d265788>
#> <environment: 0x563668469998>
#> 
#> Slot "init":
#> function() {
#>       list(theta = c(0, 1, 1, 1))
#>     }
#> <bytecode: 0x56365d1d0428>
#> <environment: 0x563668469998>
#> 
#> Slot "datanames":
#> [1] "nObs" "y"    "x"   
#> 
#> Slot "datanames_prior":
#> character(0)
#> 
#> Slot "sample":
#> [1] "alpha0" "delta0" "alpha1" "delta1"
#> 
```
