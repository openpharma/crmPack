# `DASimulations`

**\[stable\]**

This class captures the trial simulations from DA based designs. In
comparison to the parent class
[`Simulations`](https://openpharma.github.io/crmPack/reference/Simulations-class.md),
it contains additional slots to capture the time to DLT fits, additional
parameters and the trial duration.

## Usage

``` r
DASimulations(trial_duration, ...)

.DefaultDASimulations()
```

## Arguments

- trial_duration:

  (`numeric`)  
  see `DASimulations`

- ...:

  additional parameters from
  [`Simulations`](https://openpharma.github.io/crmPack/reference/Simulations-class.md)

## Slots

- `trial_duration`:

  (`numeric`)  
  the vector of trial duration values for all simulations.

## Note

Typically, end users will not use the `.DefaultDASimulations()`
function.
