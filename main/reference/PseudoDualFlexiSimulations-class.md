# `PseudoDualFlexiSimulations`

**\[stable\]** This class captures the trial simulations design using
both the DLE and efficacy responses using
[`EffFlexi`](https://openpharma.github.io/crmPack/reference/EffFlexi-class.md)
efficacy model. It extends
[`PseudoDualSimulations`](https://openpharma.github.io/crmPack/reference/PseudoDualSimulations-class.md)
by adding the capability to capture the sigma2betaW estimates.

## Usage

``` r
PseudoDualFlexiSimulations(sigma2_beta_w_est, ...)

.DefaultPseudoDualFlexiSimulations()
```

## Arguments

- sigma2_beta_w_est:

  (`numeric`)  
  the vector of the final posterior mean sigma2betaW estimates

- ...:

  additional parameters from
  [`PseudoDualSimulations`](https://openpharma.github.io/crmPack/reference/PseudoDualSimulations-class.md)

## Slots

- `sigma2_beta_w_est`:

  (`numeric`)  
  the vector of the final posterior mean sigma2betaW estimates

## Note

Typically, end users will not use the `.DefaultPseudoFlexiSimulations()`
function.
