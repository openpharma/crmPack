# `McmcOptions`

**\[stable\]**

`McmcOptions` is a class for the three canonical MCMC options as well as
Random Number Generator settings.

## Usage

``` r
McmcOptions(
  burnin = 10000L,
  step = 2L,
  samples = 10000L,
  rng_kind = NA_character_,
  rng_seed = NA_integer_
)

.DefaultMcmcOptions()
```

## Arguments

- burnin:

  (`count`)  
  number of burn-in iterations which are not saved.

- step:

  (`count`)  
  only every step-th iteration is saved after the burn-in.

- samples:

  (`count`)  
  number of resulting samples.

- rng_kind:

  (`string`)  
  the name of the RNG type. Possible types are: `Wichmann-Hill`,
  `Marsaglia-Multicarry`, `Super-Duper`, `Mersenne-Twister`. If it is
  `NA` (default), then the RNG kind will be chosen by `[rjags`\].

- rng_seed:

  (`number`)  
  RNG seed corresponding to chosen `rng_kind`. It must be an integer
  value or `NA` (default), which means that the seed will be chosen by
  `[rjags`\].

## Slots

- `iterations`:

  (`count`)  
  number of MCMC iterations.

- `burnin`:

  (`count`)  
  number of burn-in iterations which are not saved.

- `step`:

  (`count`)  
  only every `step`-th iteration is saved after the `burnin`. In other
  words, a sample from iteration `i = 1,...,iterations`, is saved if and
  only if `(i - burnin) mod step = 0`.  
  For example, for `iterations = 6`, `burnin = 0` and `step = 2`, only
  samples from iterations `2,4,6` will be saved.

- `rng_kind`:

  (`string`)  
  a Random Number Generator (RNG) type used by
  [`rjags::rjags`](https://rdrr.io/pkg/rjags/man/rjags-package.html). It
  must be one out of the following four values: `base::Wichmann-Hill`,
  `base::Marsaglia-Multicarry`, `base::Super-Duper`,
  `base::Mersenne-Twister`, or `NA_character_`. If it is `NA_character_`
  (default), then the RNG kind will be chosen by
  [`rjags::rjags`](https://rdrr.io/pkg/rjags/man/rjags-package.html).

- `rng_seed`:

  (`number`)  
  a Random Number Generator (RNG) seed used by
  [`rjags::rjags`](https://rdrr.io/pkg/rjags/man/rjags-package.html) for
  a chosen `rng_kind`. It must be an integer scalar or `NA_integer_`,
  which means that the seed will be chosen by
  [`rjags::rjags`](https://rdrr.io/pkg/rjags/man/rjags-package.html).

## Note

Typically, end users will not use the `.DefaultMcmcOptions()` function.

## Examples

``` r
# Set up MCMC option in order to have a burn-in of 10000 iterations and
# then take every other iteration up to a collection of 10000 samples.
McmcOptions(burnin = 10000, step = 2, samples = 10000)
#> An object of class "McmcOptions"
#> Slot "iterations":
#> [1] 30000
#> 
#> Slot "burnin":
#> [1] 10000
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
```
