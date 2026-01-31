# `Samples`

**\[stable\]**

`Samples` is the class to store the MCMC samples.

## Usage

``` r
Samples(data, options)

.DefaultSamples()
```

## Arguments

- data:

  see slot definition.

- options:

  see slot definition.

## Slots

- `data`:

  (`list`)  
  MCMC samples of the parameter. Each entry in this list must be a
  vector (in case of a scalar parameter) or matrix (in case of a
  vector-valued parameter) with samples. In case of matrix, every row is
  a separate sample, while columns correspond to the dimension of the
  parameter.

- `options`:

  (`McmcOptions`)  
  MCMC options that were used to generate the samples.

## Note

Typically, end users will not use the `.DefaultSamples()` function.

## Examples

``` r
# The MCMC options that were used to generate the samples.
my_options <- McmcOptions(
  burnin = 1000,
  step = 2,
  samples = 1000
)

# Create an object of class 'Samples'
# Here the parameters 'alpha' and 'beta' are randomly generated. Of course, in
# a real example these would be a samples coming from MCMC procedures.
my_samples <- Samples(
  data = list(alpha = rnorm(1000), beta = rnorm(1000)),
  options = my_options
)
```
