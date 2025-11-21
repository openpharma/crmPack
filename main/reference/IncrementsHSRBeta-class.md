# `IncrementsHSRBeta`

**\[experimental\]**

`IncrementsHSRBeta` is a class for limiting further increments using a
Hard Safety Rule based on the Bin-Beta model. Increment control is based
on the number of observed DLTs and number of subjects at each dose
level. The probability of toxicity is calculated using a Bin-Beta model
with prior (a,b). If the probability exceeds the threshold for a given
dose, that dose and all doses above are excluded from further
escalation. This is a hard safety rule that limits further escalation
based on the observed data per dose level, independent from the
underlying model.

## Usage

``` r
IncrementsHSRBeta(target = 0.3, prob = 0.95, a = 1, b = 1)

.DefaultIncrementsHSRBeta()
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

## Slots

- `target`:

  (`proportion`)  
  the target toxicity, except 0 or 1.

- `prob`:

  (`proportion`)  
  the threshold probability (except 0 or 1) for a dose being toxic.

- `a`:

  (`number`)  
  shape parameter \\a \> 0\\ of probability distribution Beta (a,b).

- `b`:

  (`number`)  
  shape parameter \\b \> 0\\ of probability distribution Beta (a,b).

## Note

Typically, end users will not use the `.DefaultIncrementsHSRBeta()`
function.

## Examples

``` r
# Limit the escalation with a hard safety criteria to the doses that are below
# the first dose that is toxic with a probability of 0.95.
my_increments <- IncrementsHSRBeta(target = 0.3, prob = 0.95)
```
