# `ModelParamsNormal`

**\[experimental\]**

`ModelParamsNormal` is the class for a bivariate normal model
parameters, i.e. the mean vector, covariance matrix and precision
matrix. The precision matrix is an inverse of the covariance matrix in
the `JAGS` and it is computed internally by the object constructor
function.

## Usage

``` r
ModelParamsNormal(mean, cov)

.DefaultModelParamsNormal()
```

## Arguments

- mean:

  (`numeric`)  
  the prior mean vector.

- cov:

  (`matrix`)  
  the prior covariance matrix. The precision matrix `prec` is internally
  calculated as an inverse of `cov`.

## Slots

- `mean`:

  (`numeric`)  
  the mean vector.

- `cov`:

  (`matrix`)  
  the covariance matrix.

- `prec`:

  (`matrix`)  
  the precision matrix, which is an inverse matrix of the `cov`.

## Note

Typically, end users will not use the `.ModelPAramsNormal()` function.

## See also

[`ModelLogNormal`](https://openpharma.github.io/crmPack/reference/ModelLogNormal-class.md),
[`LogisticNormalMixture`](https://openpharma.github.io/crmPack/reference/LogisticNormalMixture-class.md).

## Examples

``` r
ModelParamsNormal(mean = c(1, 6), cov = diag(2))
#> An object of class "ModelParamsNormal"
#> Slot "mean":
#> [1] 1 6
#> 
#> Slot "cov":
#>      [,1] [,2]
#> [1,]    1    0
#> [2,]    0    1
#> 
#> Slot "prec":
#>      [,1] [,2]
#> [1,]    1    0
#> [2,]    0    1
#> 
```
