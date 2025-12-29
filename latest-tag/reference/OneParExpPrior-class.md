# `OneParExpPrior`

**\[experimental\]**

`OneParExpPrior` is the class for a standard CRM with an exponential
prior on the power parameter for the skeleton prior probabilities. It is
an implementation of a version of the one-parameter CRM (O'Quigley et
al. 1990) .

## Usage

``` r
OneParExpPrior(skel_probs, dose_grid, lambda)

.DefaultOneParExpPrior()
```

## Arguments

- skel_probs:

  see slot definition.

- dose_grid:

  (`numeric`)  
  dose grid. It must be must be a sorted vector of the same length as
  `skel_probs`.

- lambda:

  see slot definition.

## Slots

- `skel_fun`:

  (`function`)  
  function to calculate the prior DLT probabilities.

- `skel_fun_inv`:

  (`function`)  
  inverse function of `skel_fun`.

- `skel_probs`:

  (`numeric`)  
  skeleton prior probabilities. This is a vector of unique and sorted
  probability values between 0 and 1.

- `lambda`:

  (`number`)  
  rate parameter of prior exponential distribution for theta.

## Note

Typically, end users will not use the `.DefaultOneparExpPrior()`
function.

Typically, end users will not use the
[`.DefaultOneParLogNormalPrior()`](https://openpharma.github.io/crmPack/reference/OneParLogNormalPrior-class.md)
function.

## References

O'Quigley J, Pepe M, Fisher L (1990). “Continual reassessment method: a
practical design for phase 1 clinical trials in cancer.” *Biometrics*,
**46**(1). [doi:10.2307/2531628](https://doi.org/10.2307/2531628) .

## Examples

``` r
my_model <- OneParExpPrior(
  skel_probs = c(0.1, 0.3, 0.5, 0.7, 0.9),
  dose_grid = 1:5,
  lambda = 2
)
```
