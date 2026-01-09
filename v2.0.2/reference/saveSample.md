# Determining if this Sample Should be Saved

**\[stable\]**

A method that determines if a sample from a given `iteration` should be
saved. The sample should be saved if and only if: it is not in burn-in
period and it matches the `step`.

## Usage

``` r
saveSample(object, iteration, ...)

# S4 method for class 'McmcOptions'
saveSample(object, iteration, ...)
```

## Arguments

- object:

  (`McmcOptions`)  
  object based on which the answer is determined.

- iteration:

  (`count`)  
  the current iteration index.

- ...:

  not used.

## Value

`TRUE` if this sample should be saved.

## Functions

- `saveSample(McmcOptions)`: determine if a sample should be saved.

## Examples

``` r
# Set up the MCMC option in order to have a burn-in of 10000 iterations and
# then take every other iteration up to a collection of 10000 samples.
my_options <- McmcOptions(burnin = 10000, step = 2, samples = 10000)

size(my_options)
#> [1] 10000
saveSample(my_options, iteration = 5)
#> [1] FALSE
```
