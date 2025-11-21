# The Names of the Sampled Parameters

**\[stable\]**

A method that returns names of the parameters that are sampled.

## Usage

``` r
# S4 method for class 'Samples'
names(x)
```

## Arguments

- x:

  (`Samples`)  
  object with samples.

## Examples

``` r
my_samples <- Samples(
  data = list(alpha = 1:5, beta = 15:19),
  options = McmcOptions(burnin = 2, step = 2, samples = 5)
)

names(my_samples)
#> [1] "alpha" "beta" 
```
