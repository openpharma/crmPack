# `StoppingLowestDoseHSRBeta`

**\[experimental\]**

`StoppingLowestDoseHSRBeta` is a class for stopping based on a Hard
Safety Rule using the Beta posterior distribution with Beta(a,b) prior
and a Bin-Beta model based on the observed data at the lowest dose
level. The rule is triggered when the first dose is considered to be
toxic (i.e. above threshold probability) based on the observed data at
the lowest dose level and a Beta(a,b) prior distribution. The default
prior is Beta(1,1). In case that placebo is used, the rule is evaluated
at the second dose of the dose grid, i.e. at the lowest non-placebo
dose.

## Usage

``` r
StoppingLowestDoseHSRBeta(
  target = 0.3,
  prob = 0.95,
  a = 1,
  b = 1,
  report_label = NA_character_
)

.DefaultStoppingLowestDoseHSRBeta()
```

## Arguments

- target:

  (`proportion`)  
  see slot definition.

- prob:

  (`proportion`)  
  see slot definition.

- a:

  (`number`)  
  see slot definition.

- b:

  (`number`)  
  see slot definition.

- report_label:

  (`string` or `NA`)  
  see slot definition.

## Slots

- `target`:

  (`proportion`)  
  the target toxicity.

- `prob`:

  (`proportion`)  
  the threshold probability for the lowest dose being toxic.

- `a`:

  (`number`)  
  shape parameter \\a \> 0\\ of probability distribution Beta (a,b).

- `b`:

  (`number`)  
  shape parameter \\b \> 0\\ of probability distribution Beta (a,b).

## Note

This stopping rule is independent from the underlying model.

Typically, end users will not use the
`.DefaultStoppingLowestDoseHSRBeta()` function.

## Examples

``` r
# Stopping the study if the first dose is toxic with more than 90%
# probability based on a Beta posterior distribution with Beta(1,1) prior.
my_stopping <- StoppingLowestDoseHSRBeta(
  target = 0.3,
  prob = 0.9
)

# Stopping the study if the first dose is toxic with more than 90%
# probability based on a Beta posterior distribution with Beta(0.5,0.5) prior.
my_stopping <- StoppingLowestDoseHSRBeta(
  target = 0.3,
  prob = 0.9,
  a = 0.5,
  b = 0.5
)
```
