# Getting Data for `JAGS`

**\[experimental\]**

A simple helper function that prepares an object for `data` argument of
[`rjags::jags.model()`](https://rdrr.io/pkg/rjags/man/jags.model.html),
which is invoked by
[`mcmc()`](https://openpharma.github.io/crmPack/reference/mcmc.md)
method.

## Usage

``` r
h_jags_get_data(model, data, from_prior)
```

## Arguments

- model:

  (`GeneralModel`)  
  an input model.

- data:

  (`GeneralData`)  
  an input data.

- from_prior:

  (`flag`)  
  sample from the prior only? In this case data will not be appended to
  the output, i.e. only the variables required by the `model@priormodel`
  model will be returned in data.

## Examples

``` r
# Create some data from the class `Data`.
my_data <- Data(
  x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
  y = c(0, 0, 0, 0, 0, 0, 1, 0),
  doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2))
)
#> Used default patient IDs!
#> Used best guess cohort indices!

# Initialize the CRM model.
my_model <- LogisticLogNormal(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
  ref_dose = 56
)

jags_data <- h_jags_get_data(my_model, my_data, from_prior = FALSE)
jags_data
#> $nObs
#> [1] 8
#> 
#> $y
#> [1] 0 0 0 0 0 0 1 0
#> 
#> $x
#> [1]  0.1  0.5  1.5  3.0  6.0 10.0 10.0 10.0
#> 
#> $mean
#> [1] -0.85  1.00
#> 
#> $prec
#>           [,1]      [,2]
#> [1,] 1.3333333 0.6666667
#> [2,] 0.6666667 1.3333333
#> 
#> $ref_dose
#> [1] 56
#> 
```
