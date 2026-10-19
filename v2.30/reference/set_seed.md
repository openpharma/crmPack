# Helper Function to Set and Save the RNG Seed

**\[stable\]**

This code is basically copied from `stats:::simulate.lm`.

## Usage

``` r
set_seed(seed = NULL)
```

## Arguments

- seed:

  an object specifying if and how the random number generator should be
  initialized ("seeded"). Either `NULL` (default) or an integer that
  will be used in a call to
  [`set.seed()`](https://rdrr.io/r/base/Random.html) before simulating
  the response vectors. If set, the value is saved as the `seed` slot of
  the returned object. The default, `NULL` will not change the random
  generator state.

## Value

The integer vector containing the random number generate state will be
returned, in order to call this function with this input to reproduce
the obtained simulation results.
