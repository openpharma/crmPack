# `OneParLogNormalPrior`

**\[stable\]**

`OneParLogNormalPrior` is the class for a standard CRM with a normal
prior on the log power parameter for the skeleton prior probabilities.

## Usage

``` r
OneParLogNormalPrior(skel_probs, dose_grid, sigma2)

.DefaultOneParLogNormalPrior()
```

## Arguments

- skel_probs:

  (`numeric`)  
  skeleton prior probabilities. This is a vector of unique and sorted
  probability values between 0 and 1.

- dose_grid:

  (`numeric`)  
  dose grid. It must be must be a sorted vector of the same length as
  `skel_probs`.

- sigma2:

  (`number`)  
  prior variance of log power parameter alpha.

## Value

an instance of the `OneParLogNormalPrior` class

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

- `sigma2`:

  (`number`)  
  prior variance of log power parameter alpha.

## See also

[`ModelLogNormal`](https://openpharma.github.io/crmPack/reference/ModelLogNormal-class.md).

## Examples

``` r
my_model <- OneParLogNormalPrior(
  skel_probs = seq(from = 0.1, to = 0.9, length = 5),
  dose_grid = 1:5,
  sigma2 = 2
)
```
