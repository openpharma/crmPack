# `LogisticLogNormalOrdinal`

**\[experimental\]**

`LogisticLogNormalOrdinal` is the class for a logistic lognormal CRM
model using an ordinal toxicity scale.

## Usage

``` r
LogisticLogNormalOrdinal(mean, cov, ref_dose)

.DefaultLogisticLogNormalOrdinal()
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

## Note

Typically, end users will not use the
`.DefaultLogisticLogNormalOrdinal()` function.

## Examples

``` r
LogisticLogNormalOrdinal(
  mean = c(3, 4, 0),
  cov = diag(c(4, 3, 1)),
  ref_dose = 1
)
#> An object of class "LogisticLogNormalOrdinal"
#> Slot "params":
#> An object of class "ModelParamsNormal"
#> Slot "mean":
#> [1] 3 4 0
#> 
#> Slot "cov":
#>      [,1] [,2] [,3]
#> [1,]    4    0    0
#> [2,]    0    3    0
#> [3,]    0    0    1
#> 
#> Slot "prec":
#>      [,1]      [,2] [,3]
#> [1,] 0.25 0.0000000    0
#> [2,] 0.00 0.3333333    0
#> [3,] 0.00 0.0000000    1
#> 
#> 
#> Slot "ref_dose":
#> An object of class "positive_number"
#> [1] 1
#> 
#> Slot "datamodel":
#> function() {
#>       for (i in 1:nObs) {
#>         xhat[i] <- log(x[i] / ref_dose)
#>         for (j in 1:(k - 1)) {
#>           z[i, j] <- alpha[j] + beta * xhat[i]
#>           p[i, j] <- exp(z[i, j]) / (1 + exp(z[i, j]))
#>           tox[i, j] ~ dbern(p[i, j])
#>         }
#>       }
#>     }
#> <bytecode: 0x555f01989fa0>
#> <environment: 0x555f02ac13a8>
#> 
#> Slot "priormodel":
#> function() {
#>       alpha[1] ~ dnorm(mean[1], prec[1, 1])
#>       for (i in 2:(k - 1)) {
#>         alpha[i] ~ dnorm(mean[i], prec[i, i]) %_% T(, alpha[i - 1])
#>       }
#>       gamma ~ dnorm(mean[k], prec[k, k])
#>       beta <- exp(gamma)
#>     }
#> <bytecode: 0x555f0338f650>
#> <environment: 0x555f02ac13a8>
#> 
#> Slot "modelspecs":
#> function(y, from_prior) {
#>       ms <- list(
#>         mean = params@mean,
#>         prec = params@prec,
#>         k = length(mean)
#>       )
#>       if (!from_prior) {
#>         ms$tox <- array(dim = c(length(y), length(mean) - 1))
#>         for (i in seq_along(y)) {
#>           for (j in 1:(ms$k - 1)) {
#>             ms$tox[i, j] <- y[i] >= j
#>           }
#>         }
#>         ms$ref_dose <- ref_dose
#>       }
#>       ms
#>     }
#> <bytecode: 0x555f01cd2f40>
#> <environment: 0x555f02ac13a8>
#> 
#> Slot "init":
#> function() {
#>       list(
#>         alpha = sapply(1:(length(mean) - 1), function(x) -(x + 1)),
#>         gamma = 1
#>       )
#>     }
#> <bytecode: 0x555f06441c38>
#> <environment: 0x555f02ac13a8>
#> 
#> Slot "datanames":
#> [1] "nObs" "x"   
#> 
#> Slot "datanames_prior":
#> character(0)
#> 
#> Slot "sample":
#> [1] "alpha[1]" "alpha[2]" "beta"    
#> 
```
