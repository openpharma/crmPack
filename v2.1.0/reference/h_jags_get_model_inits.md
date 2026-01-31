# Setting Initial Values for `JAGS` Model Parameters

**\[experimental\]**

A simple helper function that prepares an object for `inits` argument of
[`rjags::jags.model()`](https://rdrr.io/pkg/rjags/man/jags.model.html),
which is invoked by
[`mcmc()`](https://openpharma.github.io/crmPack/reference/mcmc.md)
method. The `inits` argument specifies initial values for model
parameters.

## Usage

``` r
h_jags_get_model_inits(model, data)
```

## Arguments

- model:

  (`GeneralModel`)  
  an input model.

- data:

  (`GeneralData`)  
  an input data.

## Value

A `list` of starting values for parameters required to be initialized in
the MCMC `JAGS `sampler.

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

h_jags_get_model_inits(model = my_model, data = my_data)
#> $theta
#> [1] 0 1
#> 
```
